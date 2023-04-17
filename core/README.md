# core

Core data model used for extracting Nix dependencies and related utilities.

To generate a JSON schema you can run the following while in the repository Nix shell:

```bash
# Install dependencies
poetry install

# View help menu
poetry run python -m explorer.core --help

# Generate the schema
poetry run python -m explorer.core
```

To extract the data from Nixpkgs you can run the following while in the repository Nix shell:

```bash
# Install dependencies
poetry install

# View help menu
poetry run python -m explorer.extract --help

# Extract the Nix data for nixpkgs on x86_64-linux distributions
poetry run python -m explorer.extract
```

To extract the data from a small flake, for example, `github:edolstra/dwarffs` you can run the following while in the repository Nix shell:


```bash
# Install dependencies
poetry install

# View help menu
poetry run python -m extract_data.core --help

# Extract the Nix data for github:edolstra/dwarffs on x86_64-linux distributions
poetry run python -m extract_data.core --target-flake-ref="github:edolstra/dwarffs"
```
