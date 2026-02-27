{ pkgs, project, version, clientDist }:

pkgs.dockerTools.buildImage {
  name = "ghcr.io/paolino/kel-circle";
  tag = version;
  config = {
    EntryPoint =
      [ "kel-circle-server" "3001" "/data/kel-circle.db" "bootstrap" "/data/sequencer.key" ];
    ExposedPorts = { "3001/tcp" = { }; };
    Volumes = { "/data" = { }; };
    WorkingDir = "/app";
  };
  copyToRoot = pkgs.buildEnv {
    name = "image-root";
    paths = [
      project.packages."kel-circle:exe:kel-circle-server"
      (pkgs.runCommand "client-files" { } ''
        mkdir -p $out/app/client/kel-circle-trivial/dist
        cp ${clientDist}/index.js \
          $out/app/client/kel-circle-trivial/dist/
        cp ${clientDist}/index.html \
          $out/app/client/kel-circle-trivial/dist/
        cp ${clientDist}/style.css \
          $out/app/client/kel-circle-trivial/dist/
      '')
    ];
  };
}
