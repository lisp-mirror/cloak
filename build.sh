#!/bin/sh -e
NAME=$(basename $0)
CWDREL=$(dirname $0)
CWD=$(cd "$CWDREL" && pwd)

export LD_LIBRARY_PATH="/usr/lib/classpath:$CWD/cloak/c"
export SBCL_HOME="$CWD/sbcl/contrib"

mkdir -p $CWD/core
exec "$CWD/sbcl/src/runtime/sbcl" \
	--core "$CWD/sbcl/output/sbcl.core" \
	--noinform \
	--noprint \
	--userinit /dev/null \
	--eval "(require :asdf)" \
	--eval '(push "'"$CWD/systems/"'" asdf:*central-registry*)' \
	--eval "(asdf:operate 'asdf:load-op :java-cloak-compat)" \
	--eval '(sb-ext:save-lisp-and-die "core/cloak.core")'
