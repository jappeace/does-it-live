{
  hpkgs ? import ./nix/hpkgs.nix {},
  pkgs ? import ./nix/pkgs.nix {},
}:
hpkgs.shellFor {
  packages = ps: [ ps."does-it-live" ];
  withHoogle = false;

  buildInputs = [
    hpkgs.haskell-language-server
    pkgs.ghcid
    pkgs.cabal-install

    # Fat environment for building Hackage packages with native deps
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
}
