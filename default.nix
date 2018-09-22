{ pkgs ? import ./closure.nix, shell ? false }: with pkgs;

stackToNix {
  root = gitIgnore ./. [
    "*.md"
    "*.nix"
    "*.tex"
    ".buildkite"
    "config.yaml"
  ];
  inherit shell;
}
