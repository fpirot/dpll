#Note you need gnuplot 4.4 for the pdfcairo terminal.
reset
set term pdfcairo font "Gill Sans,9" linewidth 4 rounded

# Line style for axes
set style line 80 lt rgb "#808080"

# Line style for grid
set style line 81 lt 0  # dashed
set style line 81 lt rgb "#808080"  # grey

set grid back linestyle 81
set border 3 back linestyle 80
# Remove border on top and right.  These
# borders are useless and make it harder
# to see plotted lines near the border.
# Also, put it in grey; no need for so much emphasis on a border.

set xtics nomirror
set ytics nomirror

#set log x
#set mxtics 10    # Makes logscale look good.

# Line styles: try to pick pleasing colors, rather
# than strictly primary colors or hard-to-see colors
# like gnuplot s default yellow.  Make the lines thick
# so they re easy to see in small plots in papers.
set style line 1 linetype rgb "#A00000" lw 2 pt 1
set style line 2 linetype rgb "#00A000" lw 2 pt 6
set style line 3 linetype rgb "#5060D0" lw 2 pt 2
set style line 4 linetype rgb "#F25900" lw 2 pt 9

set output "template.pdf"
set xlabel "x axis label"
set ylabel "y axis label"

set key top left

#set xrange [0:1]
#set yrange [0:1]


# on trace deux courbes: avec les colonnes 1 et 2, avec les colonnes 1 et 3
# a chaque fois, le nom de la courbe est lu en tete de colonne
plot "comparaison.dat" using 1:2 title columnheader(2) with lines, \
     "comparaison.dat" using 1:3 title columnheader(3) with lines, \
     "comparaison.dat" using 1:4 title columnheader(4) with lines, \
     "comparaison.dat" using 1:5 title columnheader(5) with lines

