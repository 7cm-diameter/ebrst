from amas.agent import Agent, NotWorkingError
from comprex.agent import (ABEND, NEND, OBSERVER, READER, RECORDER, START,
                           STIMULATOR, Recorder, Stimulator, _self_terminate)
from comprex.util import timestamp
from pino.config import Experimental
from pino.ino import Arduino


async def flush_message(agent: Stimulator):
    while await agent.poll(0.001):
        await agent.recv()
    return None


async def stimulate(agent: Stimulator, expvars: Experimental) -> None:
    us = expvars.get("us", 12)
    us_duration = expvars.get("us-duration", 0.05)
    required_response = expvars.get("required-response", 10)
    trial = expvars.get("trial", 120)
    us_on = us
    us_off = -us

    try:
        agent.send_to(RECORDER, timestamp(START))
        while agent.working():
            for _ in range(trial):
                await flush_message(agent)
                for _ in range(required_response):
                    await agent.recv()
                agent.send_to(RECORDER, timestamp(us_on))
                await agent.high_for(us, us_duration)
                agent.send_to(RECORDER, timestamp(us_off))
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

    data_dir = join(get_current_file_abspath(__file__), "data")
    if not exists(data_dir):
        mkdir(data_dir)
    filename = join(data_dir, namefile(config.metadata))

    stimulator = Stimulator(ino=ino) \
        .assign_task(stimulate, expvars=config.experimental) \
        .assign_task(_self_terminate)
    reader = Reader(ino=ino) \
        .assign_task(read, expvars=config.experimental) \
        .assign_task(_self_terminate)
    recorder = Recorder(filename=filename)
    observer = Observer()

    agents = [stimulator, reader, recorder, observer]
    register = Register(agents)
    env = Environment(agents)

    try:
        env.run()
    except KeyboardInterrupt:
        observer.send_all(ABEND)
        observer.finish()
