import csv
import sys
import doctest
## Define function to check if a name is an oak
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
    # Check if the provided name equals 'quercus' after converting to lowercase
    return name.lower() == 'quercus'

# Main function to process the CSV files
def main(argv): 
    f = open('../data/TestOaksData.csv','r')
    g = open('../data/JustOaksData.csv','w', newline="")  # Avoid empty lines
    taxa = csv.reader(f)
    csvwrite = csv.writer(g)
    # Initialize a set to store unique oak names
    oaks = set()
    # Flag to check if the title row has been written
    hasTitle = False
    # Iterate over each row in the source CSV file
    for row in taxa:
        # Check if the first element (genus name) in the row is an oak
        if is_an_oak(row[0]):
            # Debug prints (can be removed in production)
            print ("The genus is: ") 
            print(row[0] + '\n')
            print('FOUND AN OAK!\n')
            # Write the header row only once
            if not hasTitle:
                csvwrite.writerow(['Genus', 'species'])
                hasTitle = True
             # Write the current row to the output file
            csvwrite.writerow([row[0], row[1]])
        else:
            # Debug prints (can be removed in production)
            print(row)
            print ("The genus is: ") 
            print(row[0] + '\n')

         
    # Return 0 to indicate successful completion
    return 0
    
# Standard boilerplate to call the main function when the script is executed
if (__name__ == "__main__"):
    status = main(sys.argv)
# Run the doctest module to validate the tests in the is_an_oak function
doctest.testmod()
