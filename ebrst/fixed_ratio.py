import cv2
from amas.agent import Agent, NotWorkingError
from comprex.agent import (ABEND, NEND, OBSERVER, READER, RECORDER, START,
                           Recorder, _self_terminate)
from comprex.audio import Speaker, Tone
from comprex.util import timestamp
from pino.config import Experimental
from pino.ino import HIGH, LOW, Arduino, PinState

FILMTAKER = "FILMTAKER"
STIMULATOR = "STIMULATOR"


async def flush_message(agent: Agent):
    while await agent.poll(0.001):
        await agent.recv()
    return None


async def stimulate(agent: Agent, ino: Arduino, expvars: Experimental) -> None:
    us_pin = expvars.get("us", 12)
    us_duration = expvars.get("us-duration", 0.05)
    required_response = expvars.get("required-response", 10)
    trial = expvars.get("trial", 120)
    us_on = us_pin
    us_off = -us_pin
    signal_reward = expvars.get("reward-signal")
    tone = Tone(6000, 0.1)
    speaker = Speaker(expvars.get("speaker", 0))

    try:
        agent.send_to(RECORDER, timestamp(START))
        while agent.working():
            for _ in range(trial):
                # ignore responses during reward presentation
                await flush_message(agent)

                for _ in range(required_response):
                    await agent.recv()

                if signal_reward:
                    speaker.play(tone, blocking=False)

                ino.digital_write(us_pin, HIGH)
                agent.send_to(RECORDER, timestamp(us_on))
                agent.send_to(FILMTAKER, HIGH)
                await agent.sleep(us_duration)
                ino.digital_write(us_pin, LOW)
                agent.send_to(RECORDER, timestamp(us_off))
                agent.send_to(FILMTAKER, LOW)

            agent.send_to(OBSERVER, NEND)
            agent.finish()
            break
    except NotWorkingError:
        agent.send_to(OBSERVER, ABEND)


class Reader(Agent):
    def __init__(self, ino: Arduino):
        super().__init__(READER)
        self.ino = ino


async def read(agent: Reader, expvars: Experimental) -> None:
    lever = str(expvars.get("lever", 6))
    try:
        while agent.working():
            v = await agent.call_async(agent.ino.read_until_eol)
            if v is None:
                continue
            s = v.rstrip().decode("utf-8")
            agent.send_to(RECORDER, timestamp(s))
            if s == lever:
                agent.send_to(STIMULATOR, s)
    except NotWorkingError:
        agent.ino.cancel_read()
        pass
    return None


class FilmTaker(Agent):
    def __init__(self, addr: str):
        super().__init__(addr)
        self._led = LOW

    @property
    def led(self) -> PinState:
        return self._led


async def film(agent: FilmTaker, camid: int, filename: str):
    cap = cv2.VideoCapture(camid)
    fourcc = cv2.VideoWriter_fourcc(*"mp4v")
    width = int(cap.get(cv2.CAP_PROP_FRAME_WIDTH))
    height = int(cap.get(cv2.CAP_PROP_FRAME_HEIGHT))
    fps = cap.get(cv2.CAP_PROP_FPS)
    video = cv2.VideoWriter(filename, fourcc, fps, (width, height))

    try:
        while agent.working():
            await agent.sleep(0.025)

            ret, frame = cap.read()
            if not ret:
                continue

            if agent.led == HIGH:
                cv2.circle(frame, (10, 10), 10, (0, 0, 255), thickness=-1)

            cv2.imshow(f"Camera: {camid}", frame)
            video.write(frame)
            if cv2.waitKey(1) % 0xFF == ord("q"):
                break

        agent.send_to(OBSERVER, NEND)
        agent.finish()
    except NotWorkingError:
        agent.send_to(OBSERVER, ABEND)

    cap.release()
    video.release()
    cv2.destroyAllWindows()
    return None


async def check_pin_state(agent: FilmTaker):
    try:
        while agent.working():
            _, mess = await agent.recv()
            agent._led = mess
    except NotWorkingError:
        pass
    return None


if __name__ == '__main__':
    from os import mkdir
    from os.path import exists, join

    from amas.connection import Register
    from amas.env import Environment
    from comprex.agent import Observer
    from comprex.util import get_current_file_abspath, namefile
    from pino.ino import Comport
    from pino.ui.clap import PinoCli

    config = PinoCli().get_config()

    com = Comport() \
        .apply_settings(config.comport) \
        .set_timeout(1.0) \
        .deploy() \
        .connect()

    ino = Arduino(com)
    ino.apply_pinmode_settings(config.pinmode)
    camid = config.experimental.get("cam-id", 0)

    data_dir = join(get_current_file_abspath(__file__), "data")
    if not exists(data_dir):
        mkdir(data_dir)
    filename = join(data_dir, namefile(config.metadata))
    videoname = join(data_dir, namefile(config.metadata, extension="mp4"))

    stimulator = Agent(STIMULATOR) \
        .assign_task(stimulate, ino=ino, expvars=config.experimental) \
        .assign_task(_self_terminate)

    reader = Reader(ino=ino) \
        .assign_task(read, expvars=config.experimental) \
        .assign_task(_self_terminate)

    recorder = Recorder(filename=filename)

    filmtaker = FilmTaker(FILMTAKER) \
        .assign_task(film, camid=camid, filename=videoname) \
        .assign_task(check_pin_state) \
        .assign_task(_self_terminate)

    observer = Observer()

    agents = [stimulator, reader, recorder, observer, filmtaker]
    register = Register(agents)
    env_exp = Environment(agents[0:-1])
    env_cam = Environment([agents[-1]])

    try:
        rec_video = config.experimental.get("video", False)
        if rec_video:
            env_cam.parallelize()
            env_exp.run()
            env_cam.join()
        else:
            env_exp.run()
    except KeyboardInterrupt:
        observer.send_all(ABEND)
        observer.finish()
