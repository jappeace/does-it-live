{
  pkgs ? import ./pkgs.nix { },
  hpkgs ? import ./hpkgs.nix { inherit pkgs; },
}:
let
  exe = hpkgs.does-it-live;
  ghc = pkgs.haskellPackages.ghc;

  # Same native C libraries as shell.nix, needed for building Hackage packages
  nativeDeps = [
    pkgs.pkg-config
    pkgs.zlib
    pkgs.openssl
    pkgs.curl
    pkgs.pcre
    pkgs.pcre2
    pkgs.postgresql
    pkgs.sqlite
    pkgs.libffi
    pkgs.icu
    pkgs.libsodium
    pkgs.lmdb
    pkgs.snappy
    pkgs.zstd
    pkgs.bzip2
    pkgs.xz
    pkgs.libyaml
    pkgs.libxml2
    pkgs.libxslt
    pkgs.gmp
    pkgs.blas
    pkgs.lapack
    pkgs.cairo
    pkgs.pango
    pkgs.glib
    pkgs.gtk3
    pkgs.SDL2
    pkgs.SDL2_image
    pkgs.SDL2_mixer
    pkgs.SDL2_ttf
    pkgs.glew
    pkgs.libGL
    pkgs.libGLU
    pkgs.freeglut
    pkgs.freetype
    pkgs.fontconfig
    pkgs.harfbuzz
    pkgs.libjpeg
    pkgs.libpng
    pkgs.libtiff
    pkgs.giflib
    pkgs.libsndfile
    pkgs.portaudio
    pkgs.alsa-lib
    pkgs.jack2
    pkgs.libusb1
    pkgs.libgit2
    pkgs.zeromq
    pkgs.nettle
    pkgs.gnutls
    pkgs.rdkafka
    pkgs.leveldb
    pkgs.rocksdb
    pkgs.lz4
    pkgs.xxHash
    pkgs.arrow-cpp
    pkgs.secp256k1
  ];

  allContents = [
    exe
    ghc
    pkgs.cabal-install
    pkgs.cacert
    pkgs.coreutils
    pkgs.bash
  ] ++ nativeDeps;

in
pkgs.dockerTools.buildImage {
  name = "does-it-live";
  tag = "latest";

  copyToRoot = pkgs.buildEnv {
    name = "does-it-live-env";
    paths = allContents;
    pathsToLink = [ "/bin" "/lib" "/share" "/etc" "/include" ];
  };

  config = {
    Entrypoint = [ "${exe}/bin/exe" ];
    Env = [
      "HOME=/tmp"
      "SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
      "PATH=${pkgs.lib.makeBinPath allContents}:/usr/bin:/bin"
      "PKG_CONFIG_PATH=${pkgs.lib.makeSearchPath "lib/pkgconfig" nativeDeps}:${pkgs.lib.makeSearchPath "share/pkgconfig" nativeDeps}"
      "LANG=C.UTF-8"
    ];
    WorkingDir = "/tmp";
  };
}
