import argparse
import os
import tomllib

__version__ = "0.1.0"


def main():
    # The main parser and the commands subparser
    parser = argparse.ArgumentParser(prog="spp", description="Build tool for S++", add_help=True)
    command_subparsers = parser.add_subparsers(dest="command", required=True, help="commands")

    # spp build
    parser_build = command_subparsers.add_parser("build", help="Build the project")
    parser_build.add_argument("--src", type=str, help="The source directory", default=".")
    parser_build.add_argument("--clean", action="store_true", help="Clean the project before building")
    parser_build_mode_group = parser_build.add_mutually_exclusive_group(required=True)
    parser_build_mode_group.add_argument("--release", action="store_true", help="Build in release mode")
    parser_build_mode_group.add_argument("--debug", action="store_true", help="Build in debug mode")

    # spp run
    parser_run = command_subparsers.add_parser("run", help="Run the project")

    # spp buildrun
    parser_buildrun = command_subparsers.add_parser("buildrun", help="Build and run the project")
    parser_buildrun.add_argument("--src", type=str, help="The source directory", default=".")
    parser_buildrun.add_argument("--clean", action="store_true", help="Clean the project before building")
    parser_buildrun_mode_group = parser_buildrun.add_mutually_exclusive_group(required=True)
    parser_buildrun_mode_group.add_argument("--release", action="store_true", help="Build in release mode")
    parser_buildrun_mode_group.add_argument("--debug", action="store_true", help="Build in debug mode")

    # spp clean
    parser_clean = command_subparsers.add_parser("clean", help="Clean the project")

    # spp help
    parser_help = command_subparsers.add_parser("help", help="Show this help message and exit")

    # spp version
    parser_version = command_subparsers.add_parser("version", help="Show version and exit")

    # Parse the arguments
    args = parser.parse_args()

    src = args.src
    src = os.path.abspath(src)

    # Check for build.toml in the source directory
    toml_build_file = os.path.join(src, "build.toml")
    print(toml_build_file)
    if not os.path.isfile(toml_build_file):
        print("No 'build.toml' file found in the source directory")
        exit(1)
        
    # Parse and validate the build.toml file
    build_toml = tomllib.load(open(toml_build_file, "rb"))
    if "project" not in build_toml:
        print("No 'project' section in the 'build.toml' file")
        exit(1)

    if "name" not in build_toml["project"]:
        print("No 'name' field in the 'project' section of the 'build.toml' file")
        exit(1)

    project_name = build_toml["project"]["name"]
    match args.command:
        case "build":
            if args.clean:
                print(f"Cleaning '{project_name}'")
            print(f"Building '{project_name}' in {'release' if args.release else 'debug'} mode")
        case "run":
            print(f"Running '{project_name}'")
        case "buildrun":
            if args.clean:
                print(f"Cleaning '{project_name}'")
            print(f"Building '{project_name}' in {'release' if args.release else 'debug'} mode")
            print(f"Running '{project_name}'")
        case "clean":
            print(f"Cleaning '{project_name}'")
        case "help":
            parser.print_help()
        case "version":
            print(f"S++ Build Tool {__version__}")
        case _:
            parser.print_help()


if __name__ == "__main__":
    main()
