for file in $(ls ./src/*.erl); do erlc $file; done
mv *.beam ./ebin
