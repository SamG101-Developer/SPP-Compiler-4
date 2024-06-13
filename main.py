import os.path
import colorama

from SPPCompiler.Compiler.Compiler import Compiler

import cProfile


def main():
    colorama.init()
    file_path = os.path.abspath("notes/src")
    compiler = Compiler(file_path)


if __name__ == "__main__":
    # p = cProfile.Profile()
    # p.enable()
    main()
    # p.disable()
    # p.print_stats(sort='tottime')
