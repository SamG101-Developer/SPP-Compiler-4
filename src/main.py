from dataclasses import asdict
from pprint import pprint

from src.Compiler.Compiler import Compiler


def main():
    file_path = "../test/main.spp"
    with open(file_path, "r") as file:
        code = file.read()
    compiler = Compiler(code, file_path)
    ast = compiler.compile()

    print("\n\nAST:")
    pprint(asdict(ast))


if __name__ == "__main__":
    main()
