import cProfile
import os.path
import pstats

import colorama

from SPPCompiler.Compiler.Compiler import Compiler


PROFILE = True


def main():
    colorama.init()
    file_path = os.path.abspath("notes/src")
    compiler = Compiler(file_path)
    colorama.deinit()


if __name__ == "__main__":
    if not PROFILE:
        main()
    else:
        p = cProfile.Profile()
        p.enable()
        main()
        p.disable()

        s = pstats.Stats(p)
        p.dump_stats("profile.prof")
