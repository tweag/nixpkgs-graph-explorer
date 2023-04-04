from enum import Enum

from pydantic import BaseModel, Field


class NixpkgsMetadata(BaseModel):
    """Package metadata defined by nixpkgs specifically."""

    pname: str | None = Field(
        default=None, description="The pname attribute of the Nix derivation"
    )
    version: str = Field(description="The package's version")
    broken: bool = Field(description="Flag indicating whether the package is broken")
    license: str = Field(description="The package's license")


class OutputPathName(Enum):
    """
    The name of an outputPath.

    In Nix, output paths can have different names which get included in the store path
    """

    OUT = "out"
    LIB = "lib"
    DEV = "dev"


class BuildInputType(Enum):
    """The type of build input. In Nix there are three different types."""

    BUILD_INPUT = "build_input"
    PROPAGATED_BUILD_INPUT = "propagated_build_input"
    NATIVE_BUILD_INPUT = "native_build_input"


class BuildInput(BaseModel):
    """A build input to a Nix derivation"""

    build_input_type: BuildInputType = Field(description="The type of build input.")
    package: "Package" = Field("The input package")


class Package(BaseModel):
    """A Nix package, which is an evaluated (not realized) derivation."""

    name: str = Field(description="The name of the package")
    output_paths: dict[OutputPathName, str] = Field(
        description="A mapping of output path names to corresponding store paths"
    )
    nixpkgs_metadata: NixpkgsMetadata | None = Field(
        default=None, description="Optional metadata specific to packages from nixpkgs"
    )
    build_inputs: list[BuildInput] = Field(description="The package's build inputs")


class NixGraph(BaseModel):
    packages: list[Package]


# Note: We need this due to the circular dependency between BuildInput
# and Package
BuildInput.update_forward_refs()
