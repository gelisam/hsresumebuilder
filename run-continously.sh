#!/bin/bash
set -e

fswatcher --path hsresumebuilder.yaml --path src -- cabal run
