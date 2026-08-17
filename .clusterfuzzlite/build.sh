#!/bin/bash -eu
# ClusterFuzzLite entry point. The build itself lives in the fuzz directory
# alongside the harnesses, and is shared with the OSS-Fuzz integration.
exec "$SRC/hobbes/fuzz/oss-fuzz-build.sh"
