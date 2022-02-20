#!/usr/bin/env bash

## This script print the package name based on the
## current build environment
## NOTE: the package name does not include .zip, .rpm or .deb suffix

## Arg 1 is either 'vsn_exact' (default) or 'vsn_matcher'
## when 'vsn_exact' is given, the version number is the output of pkg-vsn.sh
## otherwise '*' is used for 'find' command to find old versions (as upgrade base)

set -euo pipefail

VSN_MATCH="${1:-vsn_exact}"

if [  "${VSN_MATCH}" = 'vsn_exact' ]; then
    PKG_VSN="${PKG_VSN:-$(./pkg-vsn.sh)}"
elif [ "${VSN_MATCH}" = 'vsn_matcher' ]; then
    PKG_VSN='*'
else
    echo "$0 ERROR: badarg"
    exit 1
fi

if [ -z "${PROFILE}" ]; then
    echo "environment variable PROFILE not set"
    exit 2
fi

# ensure dir
cd -P -- "$(dirname -- "${BASH_SOURCE[0]}")/.."

OTP_VSN="${OTP_VSN:-$(./scripts/get-otp-vsn.sh)}"
SYSTEM="$(./scripts/get-distro.sh)"

UNAME="$(uname -m)"
case "$UNAME" in
    x86_64)
        ARCH='amd64'
        ;;
    aarch64)
        ARCH='arm64'
        ;;
    arm*)
        ARCH=arm
        ;;
esac

echo "${PROFILE}-${PKG_VSN}-otp${OTP_VSN}-${SYSTEM}-${ARCH}"
