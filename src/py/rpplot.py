
def readxy(filename):
    import csv
    csvfile = open(filename, 'rb')
    dialect = csv.Sniffer().sniff(csvfile.read(1024))
    csvfile.seek(0)
    reader = csv.reader(csvfile, dialect)
    l = [(float(x),float(y)) for [x,y] in reader]
    xx = [x for (x,y) in l]
    yy = [y for (x,y) in l]
    return (xx,yy)


def rp(methods,fig,subplot,title):
    import pylab
    colors = ['b','g','r','c','m','y','k']
    ax = fig.add_subplot(subplot)
    fig.subplots_adjust(hspace=0.6)
    i=0
    for method,description in methods:
        (rec,prec) = readxy(method)
        # plot = ax.plot(rec, prec, c='blue', label=description)
        plot = ax.plot(rec, prec,c=colors[i], label=description)
        i = (i+1) % len(colors)
    ax.grid(True)
    ax.set_xlabel('Recall')
    ax.set_ylabel('Precision')
    pylab.legend(loc='upper right')
    ax.set_title(title)

