# core

Core data model used for extracting Nix dependencies and related utilities.
All commands below must be executed within a shell obtained by calling `nix develop`.

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

To develop, install the dependencies, and feed them to [vscode](https://code.visualstudio.com/)
or your favorite editor:

```bash
# Install dependencies
poetry install

# Run vscode within poetry context:
poetry run code .
```
