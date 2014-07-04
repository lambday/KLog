# --------------------------------------------------------------------
# visualize.py
# Paolo Frasconi - p-f@dsi.unifi.it
# Time-stamp: <2010-06-16 14:17:35 paolo>
#
# Extracts a named graph from a gspan file and converts into .dot
# format. Some naive colorization to distinguish nodes by label.
# Special format: V (uppercase) denotes special vertices that are
# highlighted and are supposed to refer to query variables in the
# supervised setting.
#
# --------------------------------------------------------------------

from __future__ import print_function
import string, os, sys
import re

p_colors = {0:'lavender', 1:'gainsboro', 2:'palegreen', 3:'thistle', 4:'paleturquoise'}
r_colors = {0:'cyan',1:'peachpuff',2:'lightcyan', 3:'lightsalmon',4:'magenta'}
c_colors = {0:'cornsilk',1:'lightblue', 2:'orange',3:'pink',4:'powderblue'}

def csv_2_args(csv):
    if len(csv) == 0:
        return "()"
    s = csv[0]
    for item in csv[1:]:
        s = s + "," + item
    s = "(" + s + ")"
    return s

def number(s):
    try:
        float(s)
        return True
    except ValueError:
        return False

class Emit(object):
    def __init__(self, plain, outfile):
        self.global_c = 0
        self.v2color = {}
        self.plain = plain
        self.outf = outfile

    def edge(self,u,v,label):
        if self.plain:
            print('%s -- %s' % (u,v), file=self.outf)
        else:
            print('%s -- %s [label="%s"]' % (u,v,label), file=self.outf)

    def query(self,vertex_no, label):
        if self.plain:
            print(vertex_no, file=self.outf)
        else:
            print('%s [label="%s",shape=polygon,sides=6,peripheries=2,color=red,fillcolor=yellow,style=filled]' % (vertex_no,label), file=self.outf)

    def cls(self,vertex_no, label, csv):
        if self.plain:
            print(vertex_no, file=self.outf)
        else:
            if label in self.v2color:
                c = self.v2color[label]
            else:
                c = self.global_c
                self.global_c = (self.global_c+1) % 5
                self.v2color[label] = c
            print('%s [label="%s%s",shape=box,style=filled,fillcolor="%s"]' % (vertex_no,label,csv_2_args(csv),c_colors[c]), file=self.outf)

    def relationship(self,vertex_no, label, csv):
        if self.plain:
            print(vertex_no, file=self.outf)
        else:
            if label in self.v2color:
                c = self.v2color[label]
            else:
                c = self.global_c
                self.global_c = (self.global_c+1) % 5
                self.v2color[label] = c
            print('%s [label="%s%s",shape=diamond,style=filled,fillcolor="%s"]' % (vertex_no,label,csv_2_args(csv),r_colors[c]), file=self.outf)

    def property(self,vertex_no, label):
        if self.plain:
            print(vertex_no, file=self.outf)
        else:
            if number(label):
                print('%s [label="%s"]' % (vertex_no,label), file=self.outf)
            else:
                if label in self.v2color:
                    c = self.v2color[label]
                else:
                    c = self.global_c
                    self.global_c = (self.global_c+1) % 5
                    self.v2color[label] = c
                print('%s [label="%s",style=filled,color="%s"]' % (vertex_no,label,p_colors[c]), file=self.outf)


def main():
    if len(sys.argv) < 4:
        sys.stderr.write('Usage: %s graph-id gspan-file dot-file [plain]\n' % sys.argv[0])
    else:
        gid = sys.argv[1]
        gspanfile = sys.argv[2]
	
	f = open(gspanfile, 'r')
        if sys.argv[3] == '-':
            outf = sys.stdout
        else:
            outf = open(sys.argv[3], 'w')

        regex = re.compile(r'^t # Example: '+gid+' ?\n(.*?)\nt # Example: ', re.MULTILINE + re.DOTALL)
        match = regex.search(f.read())

        if match == None:
            # Maybe it was the last example in the file, try again
      	    f = open(gspanfile, 'r')
            regex = re.compile(r'^t # Example: '+gid+' ?\n(.*)', re.MULTILINE + re.DOTALL)
            match = regex.search(f.read())

            if match == None:
                raise RuntimeError('Could not find the graph id %s in %s.' % (gid,gspanfile))
            
        lines = match.group(1).splitlines();
	
        if len(sys.argv) == 5 and sys.argv[4] == 'plain':
            emit = Emit(plain=True,outfile=outf)
        else:
            emit = Emit(plain=False,outfile=outf)
        print('graph "G" {', file=outf)
        for line in lines:
            items = line.strip().split()
            if items[0]=='v':
                csv = items[2].split(',')
                meta_pair = csv[0].split(':')
                label_pair = csv[1].split(':')
                if meta_pair[1]=='q':
                    emit.query(items[1],label_pair[1])
                if meta_pair[1]=='i':
                    emit.cls(items[1],label_pair[1],csv[2:])
                if meta_pair[1]=='r':
                    emit.relationship(items[1],label_pair[1],csv[2:])
                if meta_pair[1]=='p':
                    emit.property(items[1],label_pair[1])
            if items[0]=='e':
                label_pair = items[3].split(':')
                emit.edge(items[1],items[2],label_pair[1])
        print('}', file=outf)

if __name__ == '__main__':
    main()



# | awk 'BEGIN{print "graph \"G\" {"} $1=="v"{vid=$2; vlabel=$3; printf("%s [label=\"%s\"]\n",vid,vlabel)}  $1=="e" {srcid=$2; dstid=$3; elabel=$4; printf("%s -- %s [label=\"%s\"]\n",srcid,dstid,elabel)} END{print "}"}' > g.dot

#fdp -Tpdf -Goverlap=prism0 g.dot > g.pdf

#evince g.pdf 
