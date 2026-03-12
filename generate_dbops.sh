#!/usr/bin/bash
TARGET_DIR=$(pwd)/lib/db
sqlgg -gen caml -name DbOps -params named $TARGET_DIR/queries.sql > $TARGET_DIR/db_ops.ml
