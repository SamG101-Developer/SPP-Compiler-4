from __future__ import annotations
from typing import List
from glob import glob


class ModuleTree:
    _src_path: str
    _modules: List[str]

    def __init__(self, src_path: str):
        # Get all the spp module files from the src path
        self._src_path = src_path
        self._modules = glob(self._src_path + "/**/*.spp", recursive=True)

    def __iter__(self):
        # Iterate over the modules
        return iter(self._modules)

    # TODO: vcs imports
