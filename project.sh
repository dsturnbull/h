#!/usr/bin/env bash

CABAL_FLAGS=""

cmd="$1"

shift

case "$cmd" in
  build)
    cabal new-build --enable-tests all -j8 \
      --disable-benchmarks --disable-documentation\
      $CABAL_FLAGS "$@"
    ;;
  
  sasm)
    cabal new-run sasm -- "$@"
    ;;

  run)
    cabal new-run run -- "$@"
    ;;

  test)
    cabal new-test --enable-tests -j8 --disable-documentation --test-show-details=direct \
      $CABAL_FLAGS "$@"
    ;;

  repl)
    cabal new-repl --enable-tests \
      $CABAL_FLAGS "$@"
    ;;

  clean)
    cabal new-clean
    ;;
  
  *)
    echo "Unrecognised command: $cmd"
    exit 1
    ;;
esac
