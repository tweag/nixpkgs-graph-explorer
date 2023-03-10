import pytest

from etl import path_to_name, path_to_outputpath, get_output_names
import pandas as pd

dataframe = pd.DataFrame(
    {
        "brokenState": [False],
        "pname": ["qtsvg"],
        "version": ["5.15.8"],
        "name": ["qtsvg-5.15.8"],
        "path": [
            "libsForQt5.qt5.qtsvg, libsForQt5.qtsvg, plasma5Packages.qt5.qtsvg, plasma5Packages.qtsvg, qt5.qtsvg, libsForQt5.qt5.qtsvg, libsForQt5.qtsvg, plasma5Packages.qt5.qtsvg, plasma5Packages.qtsvg, qt5.qtsvg, libsForQt5.qt5.qtsvg, libsForQt5.qtsvg, plasma5Packages.qt5.qtsvg, plasma5Packages.qtsvg, qt5.qtsvg, libsForQt5.qt5.qtsvg, libsForQt5.qtsvg, plasma5Packages.qt5.qtsvg, plasma5Packages.qtsvg, qt5.qtsvg, libsForQt5.qt5.qtsvg, libsForQt5.qtsvg, plasma5Packages.qt5.qtsvg, plasma5Packages.qtsvg, qt5.qtsvg"
        ],
        "license": [""],
        "outputPath": ["/nix/store/m9nfbq0sj9840ddn38yjnys3w7b5hkys-qtsvg-5.15.8"],
        "outputPathAll": [
            [
                {
                    "name": "out",
                    "path": "/nix/store/m9nfbq0sj9840ddn38yjnys3w7b5hkys-qtsvg-5.15.8",
                },
                {
                    "name": "dev",
                    "path": "/nix/store/gv32agv59rgk41wzar1a1kr6ppfll9ch-qtsvg-5.15.8-dev",
                },
                {
                    "name": "bin",
                    "path": "/nix/store/7b2c17spv683hgs9wwz9l77dsbdb3g28-qtsvg-5.15.8-bin",
                },
            ]
        ],
        "buildInputs": [[]],
        "propagatedBuildInputs": [
            ["/nix/store/04p2zzpfvv3mn7may1pad6gfvhkgj321-qtbase-5.15.8-dev"]
        ],
    }
)


def test_get_output_names():
    assert get_output_names(dataframe) == {"out", "bin", "dev"}


def test_path_to_name():
    x = [
        "/nix/store/gv32agv59rgk41wzar1a1kr6ppfll9ch-qtsvg-5.15.8-dev",
        "/nix/store/l907a612vwy2kxsg52kjzip2vcsd4ipz-qttools-5.15.8-dev",
    ]
    output_names = {"out", "bin", "dev"}
    assert path_to_name(x, output_names) == ["qtsvg-5.15.8", "qttools-5.15.8"]


def test_path_to_outputpath():
    path = "/nix/store/gv32agv59rgk41wzar1a1kr6ppfll9ch-qtsvg-5.15.8-dev"
    name = "qtsvg-5.15.8"
    assert path_to_outputpath(dataframe, path, name) == dataframe.outputPath[0]
