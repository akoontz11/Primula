#!/bin/bash
for k in {2..6} ;
do
    for r in {1..50} ;
    do
        $HOME/apps/structure/console/structure  -i SnakeRiverPlainOnly.str -m mainparams.txt -e extraparams.txt -K $k -o output/SnakeRiverPlainOnly_output_$k-$r > output/outfile_$k_$r  &
        sleep 3s
    done
done
echo "All runs started"
