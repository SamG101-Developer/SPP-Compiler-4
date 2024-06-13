from __future__ import annotations
from typing import List
import os


class ModuleTree:
    _src_path: str
    _modules: List[str]

    def __init__(self, src_path: str):
        self._src_path = src_path
        self._modules = [*walk(src_path)]

    def __iter__(self):
        # Iterate over the modules
        return iter(self._modules)

    # TODO: vcs imports


def walk(path):
    entries = sorted(os.listdir(path))
    dirs = [entry for entry in entries if os.path.isdir(os.path.join(path, entry))]
    files = [entry for entry in entries if os.path.isfile(os.path.join(path, entry))]

    dirs.sort()
    files.sort()

    for dir in dirs:
        subdir_path = os.path.join(path, dir)
        yield from walk(subdir_path)

    for file in files:
        file_path = os.path.join(path, file)
        yield file_path
