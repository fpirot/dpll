#!/bin/bash


rm -f comparaison.dat
echo "argument default rand moms dlis" >> comparaison.dat


for directory in `ls Test/ | sort -n`; do

TEMPS1="0"
TEMPS2="0"
TEMPS3="0"
TEMPS4="0"

for file in `ls Test/$directory | sort -n`; do

  echo "je suis en train de traiter l'entree" $directory

  echo $file | /usr/bin/time -f'%U' -o /tmp/toto.txt ./dpll "Test/$directory/$file"
  TMP=`cat /tmp/toto.txt`
  TEMPS1=`echo $TEMPS1 + $TMP | bc`

  echo $file | /usr/bin/time -f'%U' -o /tmp/toto.txt ./dpll "Test/$directory/$file" -rand
  TMP=`cat /tmp/toto.txt`
  TEMPS2=`echo $TEMPS1 + $TMP | bc`
  
   echo $file | /usr/bin/time -f'%U' -o /tmp/toto.txt ./dpll "Test/$directory/$file" -moms
  TMP=`cat /tmp/toto.txt`
  TEMPS3=`echo $TEMPS1 + $TMP | bc`
  
   echo $file | /usr/bin/time -f'%U' -o /tmp/toto.txt ./dpll "Test/$directory/$file" -dlis
  TMP=`cat /tmp/toto.txt`
  TEMPS4=`echo $TEMPS1 + $TMP | bc`

done
echo $directory $TEMPS1 $TEMPS2 $TEMPS3 $TEMPS4 >> ./comparaison.dat
done


# a noter que vous pouvez passer un ou plusieurs arguments a ce
# script, auxquels vous ferez reference par $1, $2, etc. Par exemple,
# vous pourrez taper "bash run-tests.sh toto.dat (pour indiquer le nom
# du fichier rassemblant les resultats des tests), et faire dans ce
# fichier 'echo "argument Fibonacci Fibonacci-memo" >> $1
