#!/bin/sh

# makes sure all the required chicken scheme eggs
# are installed
echo "Will run chicken-install for required eggs (libraries)"

eggs=(
    # directory-utils \
	regex \
	simple-loops \
	srfi-1 \
	srfi-13 \
	srfi-19 \
	srfi-69 \
	srfi-113 \
	test
)

for egg in ${eggs[@]}; do
	echo "  * $egg"
done

echo "------------------"
for egg in ${eggs[@]}; do
	chicken-install $egg
done




echo "DONE"
