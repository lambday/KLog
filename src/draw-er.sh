#!/bin/bash
#
# Draw the ${MODEL} Entity-Relationship diagram.
#
# fixed for the new I/O behavior of visualize.py
# commented graph visualization, just output dot file
if [ -z "$1" ]
then
        echo "Not enough parameters"
        echo "Usage: $0 model"
        echo "       where model is the name of the intensional description"
        exit
fi
MODEL=$1
echo "Creating gspan file..."
echo "use_module('../../../src/er'),draw_er('${MODEL}')." |yap -l ${MODEL}.pl
echo "Creating dot file..."
python ../../../src/visualize.py er ${MODEL}-er.gspan ${MODEL}-er.dot
#echo "Laying out dot file..."
# Sometimes fdp gives better results, sometimes neato  is better.
#fdp -Tpdf -Gmaxiter=100 ${MODEL}-er.dot >${MODEL}-er.pdf 
# neato -Gepsilon=0.00001 -Tpdf ${MODEL}-er.dot >${MODEL}-er.pdf 
#evince ${MODEL}-er.pdf&
