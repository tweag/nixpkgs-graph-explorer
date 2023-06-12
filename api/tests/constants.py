from explorer.api.graph import Derivation, HasBuildInput, HasPropagatedBuildInput

DERIVATION_A = Derivation(output_path="/nix/store/a", attribute_path="a")
DERIVATION_B = Derivation(output_path="/nix/store/b", attribute_path="b")
DERIVATION_C = Derivation(output_path="/nix/store/c", attribute_path="c")
DERIVATION_AA = Derivation(output_path="/nix/store/aa", attribute_path="a.a")

DUMMY_DERIVATIONS = [DERIVATION_A, DERIVATION_B, DERIVATION_C, DERIVATION_AA]

EDGES = [
    (HasBuildInput(), DERIVATION_A, DERIVATION_B),
    (HasBuildInput(), DERIVATION_B, DERIVATION_C),
    (HasPropagatedBuildInput(), DERIVATION_B, DERIVATION_AA),
]
