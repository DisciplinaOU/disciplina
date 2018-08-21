with import <nixpkgs> {};
{
  educator-docker = dockerTools.buildImage {
    name = "educator";
    tag = "latest";
    contents = (import ../disciplina).disciplina-bin;
    runAsRoot = ''
      mkdir -p /data
    '';
    config = {
      Cmd = [ "/bin/dscp-educator"
        "--config /etc/disciplina/configuration.yaml"
        "--config-key clusterCi"
        "--peer witness-1.disciplina.serokell.team:4010:4011"
        "--peer witness-2.disciplina.serokell.team:4010:4011"
        "--peer witness-3.disciplina.serokell.team:4010:4011"
        "--student-listen 0.0.0.0:4020"
        "--witness-listen 0.0.0.0:4021"
        "--educator-listen 0.0.0.0:4022"
      ];
      WorkingDir = "/data";
      ExposedPorts = {
        "4020/tcp" = {};
        "4021/tcp" = {};
        "4022/tcp" = {};
      };
      Volumes = {
        "/data" = {};
      };
    };
  };
  educator-bundle = {}; # not implemented
  wallet-bundle = {};   # not implemented
}
