taxa = [ ('Myotis lucifugus','Chiroptera'),
         ('Gerbillus henleyi','Rodentia',),
         ('Peromyscus crinitus', 'Rodentia'),
         ('Mus domesticus', 'Rodentia'),
         ('Cleithrionomys rutilus', 'Rodentia'),
         ('Microgale dobsoni', 'Afrosoricida'),
         ('Microgale talazaci', 'Afrosoricida'),
         ('Lyacon pictus', 'Carnivora'),
         ('Arctocephalus gazella', 'Carnivora'),
         ('Canis lupus', 'Carnivora'),
        ]

# Write a python script to populate a dictionary called taxa_dic derived from
# taxa so that it maps order names to sets of taxa and prints it to screen.

# An example output is:
#  
# 'Chiroptera' : set(['Myotis lucifugus']) ... etc. 
# OR, 
# 'Chiroptera': {'Myotis  lucifugus'} ... etc

#### Your solution here #### 

taxa_dic = {} # Initialize as an empty dict
for first, second in taxa: 
    if second in taxa_dic: # set already exists
        taxa_dic[second].add(first)
    else: # not set
        taxa_dic[second] = {first}
for first, second_set in taxa_dic.items(): # format print
    print(f"'{first}': {second_set}")

# Now write a list comprehension that does the same (including the printing after the dictionary has been created)  
 
#### Your solution here #### 
taxa_dic = {second: {first for first, order in taxa if order == second} for _, second in taxa} # comprehension
for first, second_set in taxa_dic.items(): # format print with f'{}'
    print(f"'{first}': {second_set}")
