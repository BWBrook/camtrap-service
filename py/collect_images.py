import os
import argparse
import shutil

def main():
    parser = argparse.ArgumentParser(description="Collect images from subdirectories based on an image list.")
    parser.add_argument("-f", "--folder", required=True, help=r"Root folder to search (e.g., E:\jc_camera_study)")
    parser.add_argument("-i", "--imagelist", required=True, help="Path to text file containing image filenames (one per line, no file extension)")
    args = parser.parse_args()

    root_folder = os.path.abspath(args.folder)
    image_list_file = os.path.abspath(args.imagelist)

    if not os.path.isdir(root_folder):
        print(f"Error: The folder '{root_folder}' does not exist or is not a directory.")
        return

    if not os.path.isfile(image_list_file):
        print(f"Error: The image list file '{image_list_file}' does not exist.")
        return

    # Load image names from the file, stripping whitespace and using lower-case for comparison.
    with open(image_list_file, "r") as f:
        image_names = {line.strip().lower() for line in f if line.strip()}

    if not image_names:
        print("No image names found in the list. Exiting.")
        return

    # Create 'collected_images' folder inside the root folder if it doesn't exist.
    collected_folder = os.path.join(root_folder, "collected_images")
    if not os.path.exists(collected_folder):
        os.makedirs(collected_folder)
        print(f"Created folder: {collected_folder}")

    collected_count = 0

    # Recursively search for files in root_folder (but skip the collected_images folder)
    for dirpath, dirnames, files in os.walk(root_folder):
        # Skip the 'collected_images' folder if encountered in recursion.
        if os.path.abspath(dirpath) == os.path.abspath(collected_folder):
            continue

        for file in files:
            # Check if the file name (lower-case, ignoring extension) is in our list.
            if os.path.splitext(file)[0].lower() in image_names:
                src_path = os.path.join(dirpath, file)
                dest_path = os.path.join(collected_folder, file)
                try:
                    shutil.copy2(src_path, dest_path)
                    collected_count += 1
                    print(f"Copied '{src_path}' to '{dest_path}'")
                except Exception as e:
                    print(f"Error copying '{src_path}': {e}")

    print(f"\nDone. Total images collected: {collected_count}")

if __name__ == "__main__":
    main()
