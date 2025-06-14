import os
import shutil
import argparse
from datetime import datetime
import exifread

def get_datetime_original(file_path):
    """
    Open an image file, read its EXIF metadata, and return the DateTimeOriginal
    as a datetime object. Returns None if not found or on error.
    """
    try:
        with open(file_path, 'rb') as f:
            tags = exifread.process_file(f, details=False, strict=True)
        dt_tag = tags.get("EXIF DateTimeOriginal")
        if dt_tag:
            # dt_tag is in the format "YYYY:MM:DD HH:MM:SS"
            return datetime.strptime(str(dt_tag), "%Y:%m:%d %H:%M:%S")
        else:
            print(f"WARNING: No DateTimeOriginal tag found in {file_path}")
            return None
    except Exception as e:
        print(f"ERROR: Could not read EXIF from {file_path}: {e}")
        return None

def phase1_rename_files(ROOT_DIR):
    """
    For each nam_sub folder in ROOT_DIR, recurse into all subdirectories,
    find every .JPG file, extract its DateTimeOriginal metadata, and rename
    the file as:
      nam_sub_text_dd-mm-yyyy_hh-mm-ss.JPG
    Note: The hour is not zero-padded so that a time of 4:47:58 becomes '4-47-58'
    """
    # List all immediate subdirectories in ROOT_DIR
    for entry in os.listdir(ROOT_DIR):
        nam_sub_path = os.path.join(ROOT_DIR, entry)
        if not os.path.isdir(nam_sub_path):
            continue

        nam_sub_text = entry  # Record the nam_sub name
        print(f"Phase 1: Processing '{nam_sub_text}'...")

        # Walk through all subdirectories (including nested ones)
        for root, dirs, files in os.walk(nam_sub_path):
            for file in files:
                if not file.lower().endswith('.jpg'):
                    continue
                old_path = os.path.join(root, file)
                dt = get_datetime_original(old_path)
                if dt is None:
                    print(f"Skipping file (no valid datetime): {old_path}")
                    continue

                # Format the datetime as d-mm-yyyy_hh-mm-ss.
                # We use strftime for day, month, and year and then append the hour (non-padded),
                # followed by minute and second (zero-padded).
                # Example: 22-10-2021_4-47-58
                dt_str = f"{dt.day}-{dt.strftime('%m-%Y_')}{dt.hour}{dt.strftime('-%M-%S')}"
                new_filename = f"{nam_sub_text}_{dt_str}.JPG"
                new_path = os.path.join(root, new_filename)

                # Rename the file
                try:
                    os.rename(old_path, new_path)
                    #print(f"Renamed:\n  {old_path}\n  --> {new_path}")
                except Exception as e:
                    print(f"ERROR: Could not rename {old_path} to {new_path}: {e}")

def phase2_move_and_cleanup(ROOT_DIR):
    """
    For each nam_sub folder in ROOT_DIR, move all .JPG files from any subdirectory
    into the root of nam_sub. Then delete all subdirectories within nam_sub.
    """
    for entry in os.listdir(ROOT_DIR):
        nam_sub_path = os.path.join(ROOT_DIR, entry)
        if not os.path.isdir(nam_sub_path):
            continue

        print(f"Phase 2: Cleaning up '{entry}'...")

        # First, find all .JPG files that are NOT already in the nam_sub root.
        files_to_move = []
        for root, dirs, files in os.walk(nam_sub_path):
            # Skip the root of nam_sub
            if os.path.abspath(root) == os.path.abspath(nam_sub_path):
                continue
            for file in files:
                if file.lower().endswith('.jpg'):
                    files_to_move.append(os.path.join(root, file))

        # Move each found file to the root of the nam_sub.
        for src_path in files_to_move:
            dest_path = os.path.join(nam_sub_path, os.path.basename(src_path))
            # If a file with the same name already exists, this should not happen
            # if Phase 1 made them unique. However, we check and print a warning if so.
            if os.path.exists(dest_path):
                print(f"WARNING: Destination already exists, skipping move for {src_path}")
                continue
            try:
                shutil.move(src_path, dest_path)
                #print(f"Moved: {src_path} --> {dest_path}")
            except Exception as e:
                print(f"ERROR: Could not move {src_path} to {dest_path}: {e}")

        # Now delete all subdirectories inside nam_sub.
        # List all items in nam_sub; if it's a directory, remove it.
        for item in os.listdir(nam_sub_path):
            item_path = os.path.join(nam_sub_path, item)
            if os.path.isdir(item_path):
                try:
                    shutil.rmtree(item_path)
                    print(f"Deleted directory: {item_path}")
                except Exception as e:
                    print(f"ERROR: Could not delete directory {item_path}: {e}")

def main():
    parser = argparse.ArgumentParser(description="Rename files to include site name and datetime of image.")
    parser.add_argument("-f", "--folder", required=True, help=r"Root folder to process (e.g., C:\jc_camera_study)")
    args = parser.parse_args()

    root_folder = os.path.abspath(args.folder)

    print("Starting Phase 1: Renaming files based on DateTimeOriginal metadata...")
    phase1_rename_files(root_folder)
    print("\nPhase 1 complete.\n")
    
    print("Starting Phase 2: Moving files to root and deleting subdirectories...")
    phase2_move_and_cleanup(root_folder)
    print("\nPhase 2 complete.")
    
if __name__ == "__main__":
    main()
