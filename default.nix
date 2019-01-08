{ pkgs ? import ./pkgs.nix, shell ? false }: with pkgs;
let
  withPostgreSQL = drv: pkgs.haskell.lib.overrideCabal drv (a: {
    testToolDepends = (a.testToolDepends or []) ++
      [ pkgs.ephemeralpg pkgs.postgresql pkgs.getopt ];
    preCheck = (a.preCheck or "") + ''
      export TEST_PG_CONN_STRING=$(pg_tmp -w 600)
    '';
    # we need the temporary directory from pg_tmp
    # so extract it out of $TEST_PG_CONN_STRING
    postCheck = (a.postCheck or "") + ''
      pg_tmp stop -d $(echo ''${TEST_PG_CONN_STRING#*=} | sed 's:%2F:/:g') || :
    '';
  });
in
stackToNix {
  # TODO: properly fix this in stack-to-nix
  root = constGitIgnore "disciplina-src" ./. [
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
  overrides = final: previous: {
    disciplina-educator = withPostgreSQL previous.disciplina-educator;

  overrides = self: super: {
    HList = pkgs.haskell.lib.overrideCabal super.HList (o: {
      preCompileBuildDriver = ''
        rm -f Setup.lhs Setup.hs
        cat << EOF > Setup.hs
        import Distribution.Simple
        main = defaultMain
        EOF
      '';
    });
  };
}
