{ pkgs }:
let
  lockFile = builtins.fromJSON
    (builtins.readFile ../client/spago.lock);

  # Filter to registry packages (have version + integrity)
  registryPackages = pkgs.lib.filterAttrs
    (_: info: info.type or "" == "registry")
    lockFile.packages;

  # Fetch each registry package using spago.lock integrity
  pursPackages = builtins.mapAttrs
    (name: info:
      pkgs.fetchurl {
        url =
          "https://packages.registry.purescript.org/${name}/${info.version}.tar.gz";
        hash = info.integrity;
      })
    registryPackages;

  # Git dependencies (like keri-purs)
  keriPurs = pkgs.fetchgit {
    url = "https://github.com/paolino/keri-purs.git";
    rev = "25a4a9887d6414e0634c6c4ed61aead8d1f6d852";
    hash =
      "sha256-fAhr8SPHOzfugixGRukdHx1Ah7cqHODZNWZy0KiSq0o=";
  };

  # Assemble all PureScript packages into a single directory
  pursPackagesDir = pkgs.runCommand "purs-packages" { } (
    "mkdir -p $out\n"
    + builtins.concatStringsSep "\n"
      (pkgs.lib.mapAttrsToList
        (name: info:
          let tarball = pursPackages.${name};
          in ''
            mkdir -p $out/${name}-${info.version}
            tar xzf ${tarball} \
              -C $out/${name}-${info.version} \
              --strip-components=1
          '')
        registryPackages)
    + "\n"
    + ''
      mkdir -p $out/keri-purs
      cp -r ${keriPurs}/src $out/keri-purs/src
    '');

  # npm dependencies (FFI runtime deps bundled by esbuild)
  tweetnacl = pkgs.fetchurl {
    url =
      "https://registry.npmjs.org/tweetnacl/-/tweetnacl-1.0.3.tgz";
    hash =
      "sha256-X43EnLRIPiBro+uyKrvM/AIYxZtg/HjM4kGXztW54QI=";
  };
  blakejs = pkgs.fetchurl {
    url =
      "https://registry.npmjs.org/blakejs/-/blakejs-1.2.1.tgz";
    hash =
      "sha256-P1AQJnwaPFI0/G6xGCDqitY6HGX0R8cTXCz687ugHt4=";
  };
in pkgs.stdenvNoCC.mkDerivation {
  name = "kel-circle-client-bundle";
  src = ../client;

  nativeBuildInputs =
    [ pkgs.purs pkgs.esbuild pkgs.nodejs_20 ];

  buildPhase = ''
    shopt -s globstar

    # Set up node_modules for FFI require() resolution
    mkdir -p node_modules/tweetnacl
    tar xzf ${tweetnacl} -C node_modules/tweetnacl \
      --strip-components=1
    mkdir -p node_modules/blakejs
    tar xzf ${blakejs} -C node_modules/blakejs \
      --strip-components=1

    # Compile PureScript (sources + all deps)
    purs compile \
      kel-circle-client/src/**/*.purs \
      kel-circle-trivial/src/**/*.purs \
      ${pursPackagesDir}/*/src/**/*.purs

    # Create entry point that imports and calls main
    echo 'import { main } from "./output/Main/index.js"; main();' \
      > entry.mjs

    # Bundle with esbuild (browser app)
    esbuild --bundle entry.mjs \
      --outfile=dist/index.js \
      --platform=browser
  '';

  installPhase = ''
    mkdir -p $out
    cp dist/index.js $out/
    cp kel-circle-trivial/dist/index.html $out/
    cp kel-circle-trivial/dist/style.css $out/
  '';
}
