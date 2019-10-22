#!/usr/bin/env bash

CABAL_FLAGS=""

cmd="$1"

shift

case "$cmd" in
  install)
    cabal v2-install \
      --symlink-bindir=$HOME/.local/bin \
      -j8 --overwrite-policy=always --disable-documentation \
      exe:hwa-ci
      $CABAL_FLAGS "$@"
    ;;

  build)
    cabal v2-build all -j8 \
      --disable-tests --disable-benchmarks \
      $CABAL_FLAGS "$@"
    ;;
  
  exec)
    cabal v2-exec "$(echo *.cabal | cut -d . -f 1)" "$@"
    ;;

  test)
    cabal v2-test -j8 --enable-tests --disable-documentation \
      $CABAL_FLAGS "$@"
    ;;

  bench)
    cabal v2-bench -j8 \
      $CABAL_FLAGS "$@"
    ;;

  repl)
    cabal v2-repl \
      $CABAL_FLAGS "$@"
    ;;

  clean)
    cabal v2-clean
    ;;
  
  *)
    echo "Unrecognised command: $cmd"
    exit 1
    ;;
esac

# haskell-ide-engine work-around
for x in $(find dist-newstyle -name setup-config | grep '/opt/setup-config$' | sed 's|/opt/setup-config$||g'); do
  ( cd $x
    ln -fs opt/setup-config setup-config
  )
done

