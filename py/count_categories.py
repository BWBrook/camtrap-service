import os
import csv

# Set the root directory (change as needed)
ROOT_DIR = r"E:\JC"

# Define target folder names (in lower-case for uniform matching)
ANIMAL_TARGETS = {"animal", "animals"}
BLANK_TARGETS = {"blank-other", "blank-error", "blanks", "blank"}
HUMAN_TARGETS = {"human", "humans", "vehicle", "person"}

def count_files_recursive(folder):
    """
    Count all files in folder (including in all its subdirectories).
    """
    total_files = 0
    for _, _, files in os.walk(folder):
        total_files += len(files)
    return total_files

def find_matching_folders(root, targets):
    """
    Recursively search for directories within 'root' whose basename (in lower-case)
    is one of the 'targets'. To avoid double-counting in case of nested matches,
    once a matching folder is found the function prunes (does not descend further).
    Returns a list of full folder paths.
    """
    matches = []
    for dirpath, dirnames, _ in os.walk(root):
        current_folder = os.path.basename(dirpath).lower()
        if current_folder in targets:
            matches.append(dirpath)
            # Prune the search: do not descend further in this branch.
            dirnames.clear()
    return matches

def process_nam_sub(nam_sub_path):
    """
    For a given top-level folder (nam_sub), count files in matching subfolders.
    Returns a dictionary with counts for each category.
    """
    counts = {"animal": 0, "blank": 0, "human": 0}

    # For each category, find matching folders and count files within them.
    for folder in find_matching_folders(nam_sub_path, ANIMAL_TARGETS):
        counts["animal"] += count_files_recursive(folder)
    for folder in find_matching_folders(nam_sub_path, BLANK_TARGETS):
        counts["blank"] += count_files_recursive(folder)
    for folder in find_matching_folders(nam_sub_path, HUMAN_TARGETS):
        counts["human"] += count_files_recursive(folder)

    return counts

def main():
    results = []  # List of dictionaries, one per top-level folder

    # List items in the ROOT_DIR and process only directories.
    for entry in os.listdir(ROOT_DIR):
        nam_sub_path = os.path.join(ROOT_DIR, entry)
        if os.path.isdir(nam_sub_path):
            cat_counts = process_nam_sub(nam_sub_path)
            results.append({
                "nam_sub": entry,
                "animal": cat_counts["animal"],
                "blank": cat_counts["blank"],
                "human": cat_counts["human"]
            })

    # Print the table to the console with formatted columns.
    header = f"{'nam_sub':<20} {'animal':<10} {'blank':<10} {'human':<10}"
    print(header)
    print("-" * len(header))
    for row in results:
        print(f"{row['nam_sub']:<20} {row['animal']:<10} {row['blank']:<10} {row['human']:<10}")

    # Optionally, write the results to a CSV file.
    csv_filename = "results.csv"
    with open(csv_filename, "w", newline="") as csvfile:
        fieldnames = ["nam_sub", "animal", "blank", "human"]
        writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
        writer.writeheader()
        for row in results:
            writer.writerow(row)
    print(f"\nResults saved to {csv_filename}")

if __name__ == "__main__":
    main()
