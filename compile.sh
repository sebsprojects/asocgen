#!/bin/bash
version=$(cat "./VERSION.txt")
make BOUNDS_CHECK=1 VERSION=$version
