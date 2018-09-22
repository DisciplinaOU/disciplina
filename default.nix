{ pkgs ? import ./closure.nix, shell ? false }: with pkgs;

stackToNix {
  # TODO: properly fix this in stack-to-nix
  root = gitIgnore ./. [
    "*.md"
    "*.nix"
    "*.tex"
    ".buildkite"
    "config.yaml"
    "scripts"
    "secrets"
    "specs"
  ];
  inherit shell;
}
