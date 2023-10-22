import sys
import json
# Two example sequences to match
# seq2 = "ATCGCCGGATTACGGG"
# seq1 = "CAATTCGGAT"


# read sequences from json file.
def read_seqs(filepath):
    with open(filepath, 'r') as f:
        data = json.loads(f.read()) # load json
        seq1 = data['seq1'].strip()
        seq2 = data['seq2'].strip()
    return seq1, seq2

# Assign the longer sequence s1, and the shorter to s2
# l1 is length of the longest, l2 that of the shortest
def format_seq(seq1, seq2):
    l1 = len(seq1)
    l2 = len(seq2)
    if l1 >= l2:
        s1 = seq1
        s2 = seq2
    else:
        s1 = seq2
        s2 = seq1
        l1, l2 = l2, l1 # swap the two lengths
    return l1, l2, s1, s2

# A function that computes a score by returning the number of matches starting
# from arbitrary startpoint (chosen by user)
def calculate_score(s1, s2, l1, l2, startpoint):
    matched = "" # to hold string displaying alignements
    score = 0
    for i in range(l2):
        if (i + startpoint) < l1:
            if s1[i + startpoint] == s2[i]: # if the bases match
                matched = matched + "*"
                score = score + 1
            else:
                matched = matched + "-"

    # some formatted output
    print("." * startpoint + matched)           
    print("." * startpoint + s2)
    print(s1)
    print(score) 
    print(" ")

    return score

# Test the function with some example starting points:
# calculate_score(s1, s2, l1, l2, 0)
# calculate_score(s1, s2, l1, l2, 1)
# calculate_score(s1, s2, l1, l2, 5)

# now try to find the best match (highest score) for the two sequences
def find_best_score(s1, s2, l1, l2):
    my_best_align = None
    my_best_score = -1
    for i in range(l1): # Note that you just take the last alignment with the highest score
        z = calculate_score(s1, s2, l1, l2, i)
        if z > my_best_score:
            my_best_align = "." * i + s2 # think about what this is doing!
            my_best_score = z 
    return my_best_score, my_best_align


def main(args):
    # read sequences from json file.
    seq1, seq2 = read_seqs("/Users/guohongyuan/Desktop/CMEECourseWork/week2/data/seqs.json")
    # Assign the longer sequence s1, and the shorter to s2
    # l1 is length of the longest, l2 that of the shortest
    l1, l2, s1, s2 = format_seq(seq1, seq2)
    # now try to find the best match (highest score) for the two sequences
    my_best_score, my_best_align = find_best_score(s1, s2, l1, l2)

    # save and print best score and best alignment
    print("Best score:", my_best_score)
    # save the best alignment along with its corresponding score
    with open("/Users/guohongyuan/Desktop/CMEECourseWork/week2/results/best_alignment.json", 'w') as f:
        f.write(json.dumps({'best score': my_best_score, 'best_alignment': my_best_align}))
    return 0

if __name__ == "__main__":
    status = main(sys.argv)