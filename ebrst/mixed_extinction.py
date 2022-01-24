from typing import List

import cv2
from amas.agent import Agent
from comprex import agent as at
from comprex.audio import Speaker, Tone
from comprex.scheduler import blockwise_shuffle, geom_rng
from comprex.util import timestamp
from pino.config import Experimental
from pino.ino import HIGH, LOW, Arduino

FILMTAKER = "FILMTAKER"


async def flush_message_for(agent: Agent, duration: float):
    while duration >= 0.:
        s, _ = timestamp(None)
        await agent.try_recv(duration)
        e, _ = timestamp(None)
        duration -= e - s


def generate_probe_trial(nprobe: int, ntrial: int, warmup: int) -> List[bool]:
    return [
        False if i % ((ntrial - warmup) // nprobe) or i <= warmup else True
        for i in range(1, ntrial + 1)
    ]


async def stimulate(agent: at.Agent, ino: Arduino, expvars: Experimental):
    # read experimental variables from the given config file
    reward_pin = expvars.get("reward-pin", 12)
    reward_duration = expvars.get("reward-duration", 0.006)
    required_response = expvars.get("required-response", 1)
    number_of_rewards = expvars.get("number-of-rewards", 200)
    # TODO: apply `blockwise_shuffle`
    where_probe = generate_probe_trial(expvars.get("number-of-probe", 4),
                                       number_of_rewards, 40)
    where_probe = blockwise_shuffle(where_probe, 40)
    probe_duration = expvars.get("extinction-length", 60.)
    tone = Tone(6000, 30)
    speaker = Speaker(expvars.get("speaker", 0))

    # event ids
    reward_on = reward_pin
    reward_off = -reward_on

    # calculate based on the values read from the config files
    required_responses = geom_rng(required_response, number_of_rewards)

    # experiment control
    try:
        agent.send_to(at.RECORDER, timestamp(at.START))
        while agent.working():
            speaker.play(tone, False, True)
            for req, probe in zip(required_responses, where_probe):
                agent.send_to(FILMTAKER, HIGH)
                await flush_message_for(agent, 0.1)
                agent.send_to(FILMTAKER, LOW)

                if probe:
                    while True:
                        mess = await agent.try_recv(probe_duration)
                        if mess is None:
                            break
                else:
                    agent.send_to(at.RECORDER, timestamp(200))
                    for _ in range(req):
                        await agent.recv()

                agent.send_to(at.RECORDER, timestamp(-tone.freq))

                ino.digital_write(reward_pin, HIGH)
                agent.send_to(at.RECORDER, timestamp(reward_on))
                await agent.sleep(reward_duration)
                ino.digital_write(reward_pin, LOW)
                agent.send_to(at.RECORDER, timestamp(reward_off))

            speaker.stop()
            agent.send_to(at.RECORDER, timestamp(at.NEND))
            agent.send_to(at.OBSERVER, at.NEND)
            agent.finish()

    except at.NotWorkingError:
        agent.send_to(at.RECORDER, timestamp(at.ABEND))
        agent.send_to(at.OBSERVER, at.ABEND)


async def read(agent: at.Agent, ino: Arduino, expvars: Experimental):
    # read experimental variables from the given config file
    lever_pin = expvars.get("lever-pin", 6)

    # cast to `str` to compare with inputs from Arduino
    lever_pin = str(lever_pin)

    # Reading inputs from Arduino
    try:
        while agent.working():
            input_: bytes = await agent.call_async(ino.read_until_eol)
            if input_ is None:
                continue
            parsed_input = input_.rstrip().decode("utf-8")
            agent.send_to(at.RECORDER, timestamp(parsed_input))
            if parsed_input == lever_pin:
                agent.send_to(at.STIMULATOR, parsed_input)

    except at.NotWorkingError:
        ino.cancel_read()


class FilmTaker(at.Agent):
    def __init__(self):
        super().__init__(FILMTAKER)
        self._mark = LOW

    def is_marked(self) -> bool:
        return self._mark is HIGH


async def film(agent: FilmTaker, filename: str, expvars: Experimental):
    # read experimental variables from the given config file
    camid = expvars.get("camera-index", 0)
    video_recording = expvars.get("video-recording", True)

    # initialize and configure `VideoCapture` and `VideoWriter`
    cap = cv2.VideoCapture(camid)
    cap.set(cv2.CAP_PROP_FRAME_WIDTH, 320)
    cap.set(cv2.CAP_PROP_FRAME_HEIGHT, 240)
    cap.set(cv2.CAP_PROP_FPS, 30)
    fourcc = cv2.VideoWriter_fourcc(*"mp4v")
    width = int(cap.get(cv2.CAP_PROP_FRAME_WIDTH))
    height = int(cap.get(cv2.CAP_PROP_FRAME_HEIGHT))
    fps = cap.get(cv2.CAP_PROP_FPS)
    if video_recording:
        video = cv2.VideoWriter(filename, fourcc, fps, (width, height))
    else:
        video = None

    # show and record frames from the camera
    try:
        while agent.working():
            await agent.sleep(0.01)
            ret, frame = cap.read()
            if not ret:
                continue

            if agent.is_marked():
                cv2.circle(frame, (10, 10), 10, (0, 0, 255), thickness=-1)

            cv2.imshow(f"Camera: {camid}", frame)
            if video is not None:
                video.write(frame)
            if cv2.waitKey(1) % 0xFF == ord("q"):
                break
        agent.send_to(at.OBSERVER, at.NEND)
        agent.finish()
    except at.NotWorkingError:
        agent.send_to(at.OBSERVER, at.ABEND)

    cap.release()
    if video is not None:
        video.release()
    cv2.destroyAllWindows()


async def check_markable(agent: FilmTaker):
    try:
        while agent.working():
            _, mess = await agent.recv()
            agent._mark = mess
    except at.NotWorkingError:
        pass
    return None


if __name__ == '__main__':
    from pathlib import Path

    from amas.connection import Register
    from amas.env import Environment
    from comprex.util import get_current_file_abspath, namefile
    from pino.ino import Comport
    from pino.ui.clap import PinoCli

    # read the config file passed from command line
    config = PinoCli().get_config()

    # configure comport and Arduino
    com = Comport() \
        .apply_settings(config.comport) \
        .set_timeout(1.0) \
        .deploy() \
        .connect()
    ino = Arduino(com)
    ino.apply_pinmode_settings(config.pinmode)

    # Set the directory where the data wioll be saved and filename
    data_dir = Path(get_current_file_abspath(__file__)).joinpath("data")
    if not data_dir.exists():
        data_dir.mkdir()
    eventfile = data_dir.joinpath(namefile(config.metadata))
    videofile = data_dir.joinpath(namefile(config.metadata, extension="MP4"))

    # Assign tasks to agents
    stimulator = Agent(at.STIMULATOR) \
        .assign_task(stimulate, ino=ino, expvars=config.experimental) \
        .assign_task(at._self_terminate)

    reader = Agent(at.READER) \
        .assign_task(read, ino=ino, expvars=config.experimental) \
        .assign_task(at._self_terminate)

    filmtaker = FilmTaker() \
        .assign_task(film,
                     filename=str(videofile),
                     expvars=config.experimental) \
        .assign_task(check_markable) \
        .assign_task(at._self_terminate)

    recorder = at.Recorder(str(eventfile))

    observer = at.Observer()

    register = Register([stimulator, reader, recorder, observer, filmtaker])
    env0 = Environment([stimulator, observer])
    env1 = Environment([filmtaker, reader, recorder])

    try:
        env1.parallelize()
        env0.run()
        env1.join()
    except KeyboardInterrupt:
        observer.send_all(at.ABEND)
        observer.finish()
