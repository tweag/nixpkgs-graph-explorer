from enum import Enum

from pydantic import BaseModel, Field


def snake_case_to_camel_case(name: str):
    parts = name.split("_")
    return "".join([parts[0]] + [part.capitalize() for part in parts[1:]])


class NixpkgsMetadata(BaseModel):
    """Package metadata defined by nixpkgs specifically."""

    pname: str | None = Field(
        default=None, description="The pname attribute of the Nix derivation"
    )
    version: str = Field(description="The package's version")
    broken: bool = Field(description="Flag indicating whether the package is broken")
    license: str = Field(description="The package's license")


class OutputPath(BaseModel):
    """A Nix outputPath"""

    name: str = Field(description="The output path's name (out, doc, dev, ...)")
    path: str = Field(description="The output path")


class ParsedName(BaseModel):
    """The parsed output of the builtins.parseDrvName function."""

    name: str | None = Field(
        default=None, description="The package name of the Nix derivation"
    )
    version: str = Field(description="The version of the Nix derivation")


class BuildInputType(Enum):
    """The type of build input. In Nix there are three different types."""

    BUILD_INPUT = "build_input"
    PROPAGATED_BUILD_INPUT = "propagated_build_input"
    NATIVE_BUILD_INPUT = "native_build_input"


class BuildInput(BaseModel):
    """A build input to a Nix derivation"""

    attribute_path: str = Field(
        description="Attribute path from the flake package set",
    )
    build_input_type: BuildInputType = Field(
        description="The type of build input",
    )
    # None when it can't be built (e.g. wrong platform)
    output_path: str | None = Field(
        description="The output path of the input derivation",
    )

    class Config:
        use_enum_values = True
        alias_generator = snake_case_to_camel_case


class Package(BaseModel):
    """A Nix package, which is an evaluated (not realized) derivation."""

    attribute_path: str = Field(
        description="Attribute path from the flake package set",
    )
    derivation_path: str = Field(
        description="The derivation path of this derivation",
    )
    # None when it can't be built (e.g. wrong platform)
    output_path: str | None = Field(
        description="The output path of this derivation",
    )
    output_paths: list[OutputPath] = Field(
        description="A list of the package's output paths",
    )

    name: str = Field(
        description="The name of the package",
    )
    parsed_name: ParsedName | None = Field(
        default=None,
        description=(
            "The parsed package name and version of the package by Nix builtins"
        ),
    )
    nixpkgs_metadata: NixpkgsMetadata | None = Field(
        default=None,
        description="Optional metadata specific to packages from nixpkgs",
    )
    build_inputs: list[BuildInput] = Field(
        description="The package's build inputs",
    )

    class Config:
        use_enum_values = True
        alias_generator = snake_case_to_camel_case
