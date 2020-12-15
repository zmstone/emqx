#!/bin/bash

# This script prints the release version for emqx

# ensure dir
cd -P -- "$(dirname -- "$0")"

PRE_RELEASE_FOR="4.3.0"

echo "$PRE_RELEASE_FOR-pre-$(git rev-parse HEAD | cut -b1-8)"
