#!/bin/bash
for k in {2..10} ;
do
    for r in {1..50} ;
    do
        $HOME/apps/structure/console/structure  -i Primula/MinSamples35.str -m Primula/MinSamples35_mainparams.txt -e Primula/MinSamples35_extraparams.txt -K $k -o Primula/MinSamples35_output_$k-$r > outfile_$k_$r  &
        sleep 3s
    done
done
echo "All runs started"
