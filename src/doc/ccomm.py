from __future__ import print_function
from pyparsing import cStyleComment
import sys

def ccomm(filename):
    c_src = open(filename).read()
    cmt = cStyleComment.scanString(c_src)
    for comment,s,e in cmt:
        for line in comment[0].splitlines():
            if line[4:7]!='===':
                print(line)
        print()

if __name__ == '__main__':
    if len(sys.argv) < 2:
        print('Usage: %s file' % sys.argv[0], file=sys.stderr)
    else:
        ccomm(sys.argv[1])
