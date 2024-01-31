import os.path
import colorama

from src.Compiler.Compiler import Compiler


def main():
    colorama.init()
    file_path = os.path.abspath("../notes/src")
    compiler = Compiler(file_path)


if __name__ == "__main__":
    main()
