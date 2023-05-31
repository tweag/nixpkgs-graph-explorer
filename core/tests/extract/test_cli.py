import json
from pathlib import Path

from click.testing import CliRunner

from explorer.extract.cli import cli


def test_trivial():
    # need absolute path
    trivial_flake_path = (
        Path(__file__).parent / "fixtures" / "flake-trivial"
    ).absolute()
    # sanity checks
    assert trivial_flake_path.exists()
    assert trivial_flake_path.is_dir()
    assert (trivial_flake_path / "flake.nix").exists()
    assert (trivial_flake_path / "flake.lock").exists()

    # test CLI
    runner = CliRunner()
    result = runner.invoke(
        cli,
        [
            "--target-flake-ref",
            f"path:{trivial_flake_path}",
            "--target-system",
            "x86_64-linux",
            "--verbose",
            # print to stdout
            "-",
        ],
    )

    assert result.stdout != "", "Empty result"
    result_node = json.loads(result.stdout)
    assert result_node.get("output_path") is not None
    assert result_node.get("name") == "trivial-1.0"
    assert result_node.get("parsed_name", {}).get("name") == "trivial"
    assert result_node.get("parsed_name", {}).get("version") == "1.0"
