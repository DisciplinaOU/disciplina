{ pkgs ? import ./pkgs.nix }: with pkgs;
let
  withPostgreSQL = cfg: pkgs: {
    components.tests.disciplina-test.build-tools = # (cfg.components.tests.disciplina-test.depends or []) ++
      [ pkgs.ephemeralpg pkgs.postgresql pkgs.getopt ];
    components.tests.disciplina-test.depends =
      [ pkgs.ephemeralpg pkgs.postgresql pkgs.getopt ];

    preCheck = # (cfg.preCheck or "") +
    ''
      export TEST_PG_CONN_STRING=$(pg_tmp -w 600)
    '';
    # we need the temporary directory from pg_tmp
    # so extract it out of $TEST_PG_CONN_STRING
    postCheck = # (cfg.postCheck or "") +
    ''
      pg_tmp stop -d $(echo ''${TEST_PG_CONN_STRING#*=} | sed 's:%2F:/:g') || :
    '';
  };

  addLatex = cfg: pkgs: {
    # components.library.depends = (cfg.components.library.depends or []) ++ [ pkgs'.pdf-generator-xelatex ];
    components.library.depends = [ pkgs.pdf-generator-xelatex ];
  };

in
haskell-nix.project {
  name = "disciplina";
  src = ./.;

  # modules = [
  #   ({config, pkgs, ...}: {
  #     packages.disciplina-educator =
  #       (withPostgreSQL config.packages.disciplina-educator pkgs) //
  #       (addLatex config.packages.disciplina-educator pkgs);
  #   })
  # ];
}


# project.override {
#   packages.disciplina-educator =
#     (withPostgreSQL
#        (addLatex project.packages.disciplina-educator)
#     );
# }

# haskell-nix.stackProject {
  # TODO: properly fix this in stack-to-nix
  # root = constGitIgnore "disciplina-src" ./. [
  #   "*.nix"
  #   "/ChangeLog.md"
  #   "/README.md"
  #   "/.buildkite"
  #   "/config.yaml"
  #   "/scripts"
  #   "/secrets"
  # ];
  # inherit shell;

  # name = "disciplina";
  # src = ./.;

  # overrides = self: previous: {
  #   disciplina-educator =
  #     (withPostgreSQL
  #       (addLatex previous.disciplina-educator)
  #     );

  #   HList = pkgs.haskell.lib.overrideCabal previous.HList (o: {
  #     preCompileBuildDriver = ''
  #       rm -f Setup.lhs Setup.hs
  #       cat << EOF > Setup.hs
  #       import Distribution.Simple
  #       main = defaultMain
  #       EOF
  #     '';
  #   });
  # };
# }
