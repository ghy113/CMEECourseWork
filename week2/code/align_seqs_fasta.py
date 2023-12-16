import sys


def calculate_score(s1, s2, l1, l2, startpoint):
        matched = "" # to hold string displaying alignements
        score = 0
        # Iterate over the length of the shorter sequence
        for i in range(l2):
            # Ensure the index is within the range of the longer sequence
            if (i + startpoint) < l1:
                if s1[i + startpoint] == s2[i]: # if the bases match
                    matched = matched + "*"
                    score = score + 1
                else:
                    matched = matched + "-"

        return score


def align(seq1, seq2):
    # Assign the longer sequence s1, and the shorter to s2
    # l1 is length of the longest, l2 that of the shortest

    l1 = len(seq1)
    l2 = len(seq2)
    if l1 >= l2:
        s1 = seq1
        s2 = seq2
    else:
        s1 = seq2
        s2 = seq1
        l1, l2 = l2, l1 # swap the two lengths

    # A function that computes a score by returning the number of matches starting
    # from arbitrary startpoint (chosen by user)
    

    # Test the function with some example starting points:
    # calculate_score(s1, s2, l1, l2, 0)
    # calculate_score(s1, s2, l1, l2, 1)
    # calculate_score(s1, s2, l1, l2, 5)

    # now try to find the best match (highest score) for the two sequences
    my_best_align = None
    my_best_score = -1
    for i in range(l1): # Note that you just take the last alignment with the highest score
        z = calculate_score(s1, s2, l1, l2, i)
        if z > my_best_score:
            my_best_align = "." * i + s2 
            my_best_score = z 

    return my_best_score, my_best_align
# Function to load sequences from fasta files
def load_fasta_seqs(fasta_file1, fasta_file2):
    seq1 = ""
    seq2 = ""
    # Load first sequence
    with open(fasta_file1) as f:
        lines = f.readlines()
        for line in lines[1:]:
            seq1 += line.strip()

    # print(seq1)
    # Load second sequence
    with open(fasta_file2) as f:
        lines = f.readlines()
        for line in lines[1:]:
            seq2 += line.strip()

    return seq1, seq2


# Main function to execute the sequence alignment process
def main(args):
    file1 = "../data/407228326.fasta"
    file2 = "../data/407228412.fasta"
    # Check and update file paths if provided in command line arguments
    if(len(args) == 3):
        file1 = args[1]
        file2 = args[2]
    elif(len(args) != 1):
        print("Usage Error: Please enter 2 fasta file paths as input, or run the file directly (default data/fasta file as input)")
        return 0# Return a non-zero value to indicate error
    # Load sequences from the specified FASTA files
    seq1, seq2 = load_fasta_seqs(file1, file2)
    print("Start...")
    my_best_score, my_best_align = align(seq1, seq2)
    print("Best score:", my_best_score)
    # save the best alignment along with its corresponding score
    with open("../results/best_fasta_alignment.txt", 'w') as f:
        f.write(my_best_align)
    return 0# Return 0 to indicate successful completion


# Run the main function if the script is executed directly
if (__name__ == "__main__"):
    status = main(sys.argv)
    sys.exit(status)
