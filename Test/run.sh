#!/bin/bash

# preparation du fichier comparaison.dat: on l'enleve s'il existe, 
# on ecrit l'en-tete des colonnes
rm -f comparaison.dat
echo "argument default rand moms dlis" >> comparaison.dat

# une boucle for:
# la variable entree parcourt toutes les valeurs que renvoie la commande
# seq 25 40
# on pourrait aussi faire  "for entree in `ls exemples/`" pour parcourir
# tous les noms de fichier contenus dans le sous-repertoire exemples/.
# au passage, la syntaxe `bla` est utilisee pour designer "la valeur
# renvoyee par l'execution de la commande bla"
for directory in `ls Test/ | sort -n`; do
for file in `ls Test/$directory | sort -n`; do

  # ici $entree designe la valeur de la variable entree
  echo "je suis en train de traiter l'entree" $directory

# on envoie la valeur de entree a la commande /usr/bin/time ....
# cette commande execute dpll, recupere le temps utilisateur (-f'%U'),
# et stocke cette valeur dans le fichier /tmp/toto.txt
  echo $directory | /usr/bin/time -f'%U' -o /tmp/toto.txt ./dpll "Test/$directory/$file"
# on recupere ensuite dans TEMPS1 le contenu de /tmp/toto.txt
# notez a nouveau l'usage des `..`
  TEMPS1=`cat /tmp/toto.txt`

  echo $directory | /usr/bin/time -f'%U' -o /tmp/toto.txt ./dpll "Test/$directory/$file" -rand
  TEMPS2=`cat /tmp/toto.txt`
  
   echo $directory | /usr/bin/time -f'%U' -o /tmp/toto.txt ./dpll "Test/$directory/$file" -moms
  TEMPS3=`cat /tmp/toto.txt`
  
   echo $directory | /usr/bin/time -f'%U' -o /tmp/toto.txt ./dpll "Test/$directory/$file" -dlis
  TEMPS4=`cat /tmp/toto.txt`
  
#  echo $entree | /usr/bin/time -f'%U' -o /tmp/toto.txt ./dpll "Test/$entree" -wlit
#  TEMPS2=`cat /tmp/toto.txt`

  
# echo $entree | /usr/bin/time -f'%U' -o /tmp/toto.txt ./dpll "Test/$entree" -wlit -rand
# TEMPS4=`cat /tmp/toto.txt`

# pour finir, on ajoute une ligne (3 colonnes) au fichier ./comparaison.dat
  echo $directory"."$file $TEMPS1 $TEMPS2 $TEMPS3 $TEMPS4 >> ./comparaison.dat


# fin de la boucle
done
done


# a noter que vous pouvez passer un ou plusieurs arguments a ce
# script, auxquels vous ferez reference par $1, $2, etc. Par exemple,
# vous pourrez taper "bash run-tests.sh toto.dat (pour indiquer le nom
# du fichier rassemblant les resultats des tests), et faire dans ce
# fichier 'echo "argument Fibonacci Fibonacci-memo" >> $1
