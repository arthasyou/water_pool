#!/bin/sh

cd ..

docker run -d -p 8080:8080 --init --volume="$PWD/log:/water/log" \
--volume="$PWD/config/sys.config:/water/releases/0.1.0/sys.config" \
--volume="$PWD/config/vm.args:/water/releases/0.1.0/vm.args" \
 water
