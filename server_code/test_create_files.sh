#!/bin/bash

for i in {1..5}
do
    touch test_file_$i
    echo "Created test_file_$i"
    sleep 30
done
