import csv
import sys
import os
import math

# Function to calculate tree height using trigonometry
def calculate_tree_height(degrees, distance):
    # Convert angle from degrees to radians for trigonometric calculation
    radians = math.radians(degrees)
    # Calculate height using the tangent of the angle and distance
    return distance * math.tan(radians)

# Function to process data from a CSV file
def process(csv_path):    
    # Lists to store processed data and headers
    data = []
    headers = []

    # Open and read the CSV file
    with open(csv_path, 'r') as csv_file:
        csv_reader = csv.reader(csv_file)
        # Extract the header row
        headers = next(csv_reader)

        # Iterate through each row in the CSV file
        for row in csv_reader:
            # Identify column indices for angle and distance
            angle_idx = headers.index("Angle.degrees")
            distance_idx = headers.index("Distance.m")

            # Convert angle and distance values to floats
            angle_degrees = float(row[angle_idx])
            distance = float(row[distance_idx])

            # Calculate tree height and append to the row
            tree_height = calculate_tree_height(angle_degrees, distance)
            row.append(tree_height)
            data.append(row)

    # Append the new header for tree height
    headers.append("Tree.Height.m")
    return headers, data

# Function to write data to a new CSV file
def save_to_csv(savepath, headers, data):    
    # Open the output file for writing
    with open(savepath, 'w', newline='') as csv_file:
        csv_writer = csv.writer(csv_file)
        # Write the header row
        csv_writer.writerow(headers)
        # Write each data row
        for row in data:
            csv_writer.writerow(row)

# Main function to orchestrate the processing of the CSV file
def main(argv):   
    # Check for the presence of command-line argument
    if len(sys.argv) < 2:
        print("Usage: get_TreeHeight.py <filepath of csv>")
        return 1  # Error code for incorrect usage

    # Retrieve the path of the CSV file from command-line argument
    csv_path = sys.argv[1]
    # Process the CSV file
    headers, data = process(csv_path)
    # Construct a file path for saving the results
    base_name = os.path.splitext(os.path.basename(csv_path))[0]
    save_path = os.path.join("../results", f"{base_name}_treeheights.csv")
    # Save the processed data to the new CSV file
    save_to_csv(save_path, headers, data)

    return 0  # Success code

# Script entry point
if (__name__ == "__main__"):
    status = main(sys.argv)
    sys.exit(status)
