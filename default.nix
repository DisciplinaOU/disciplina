{ pkgs ? import ./pkgs.nix, shell ? false }: with pkgs;

stackToNix {
  # TODO: properly fix this in stack-to-nix
  root = gitIgnore ./. [
    "*.nix"
    "/ChangeLog.md"
    "/README.md"
    "/.buildkite"
    "/config.yaml"
    "/scripts"
    "/secrets"
    "/specs"
  ];
  inherit shell;
}
