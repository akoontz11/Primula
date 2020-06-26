#!/bin/bash
for k in {2..16} ;
do
    for r in {1..50} ;
    do
        $HOME/apps/structure/console/structure  -i JuneSubset.str -m mainparams.txt -e extraparams.txt -K $k -o output/JuneSubset_output_$k-$r > output/outfile_$k_$r  &
        sleep 3s
    done
done
echo "All runs started"
