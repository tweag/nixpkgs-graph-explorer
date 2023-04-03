# core

Core data model used for extracting Nix dependencies and related utilities.

To generate a JSON schema you can run the following while in the repositories
Nix shell:

```bash
# Install dependencies
poetry install

# View help menu
poetry run python -m explorer.core --help

# Generate the schema
poetry run python -m explorer.core
```
