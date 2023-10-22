import csv
import sys
import doctest
#Define function
def is_an_oak(name):
    """ Returns True if name is starts with 'quercus' 
    >>> is_an_oak('q')
    False
    >>> is_an_oak('')
    False
    >>> is_an_oak('Quercuss')
    False
    >>> is_an_oak('Quercus')
    True
    """
    return name.lower() == 'quercus'

def main(argv): 
    f = open('../data/TestOaksData.csv','r')
    g = open('../data/JustOaksData.csv','w', newline="")  # Avoid empty lines
    taxa = csv.reader(f)
    csvwrite = csv.writer(g)
    oaks = set()
    hasTitle = False
    for row in taxa:
        if is_an_oak(row[0]):
            print ("The genus is: ") 
            print(row[0] + '\n')
            print('FOUND AN OAK!\n')
            if not hasTitle:
                csvwrite.writerow(['Genus', 'species'])
                hasTitle = True
            csvwrite.writerow([row[0], row[1]])
        else:
            print(row)
            print ("The genus is: ") 
            print(row[0] + '\n')

         

    return 0
    
if (__name__ == "__main__"):
    status = main(sys.argv)

doctest.testmod()