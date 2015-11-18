#!/bin/bash
if [ "$#" = "0" ]; then
  corebuild main.native
  ./main.native
elif [ "$1" = "token" ]; then
  corebuild main_test_token.native
  ./main_test_token.native
fi


