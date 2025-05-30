import os
import shutil
import string

def generate_prefix(index):
    """
    Generates a prefix based on an integer index.
    For 0 <= index < 26, returns a single letter (e.g. 'a_').
    For larger indices, returns a two-letter combination (e.g. 'aa_', 'ab_', etc.).
    """
    letters = string.ascii_lowercase
    if index < 26:
        return f"{letters[index]}_"
    else:
        # For indices 26 through 26*26+26-1 (i.e. up to 701), generate a two-letter prefix.
        first = (index // 26) - 1
        second = index % 26
        return f"{letters[first]}{letters[second]}_"

def safe_move(src, dst):
    """
    Moves a file from src to dst. If dst exists, appends a numeric suffix until a free name is found.
    """
    base, ext = os.path.splitext(dst)
    counter = 1
    new_dst = dst
    while os.path.exists(new_dst):
        new_dst = f"{base}_{counter}{ext}"
        counter += 1
    shutil.move(src, new_dst)

def process_root_folder(root_dir):
    """
    Processes each folder in the root directory. For each subfolder within a folder,
    moves its files up to the parent folder, prefixing the filenames with a generated prefix.
    Reports the number of files processed for each parent folder.
    """
    total_files = 0
    for folder_name in os.listdir(root_dir):
        folder_path = os.path.join(root_dir, folder_name)
        if os.path.isdir(folder_path):
            file_count = 0
            # Get a list of subfolders in the current folder
            subfolders = [d for d in os.listdir(folder_path) if os.path.isdir(os.path.join(folder_path, d))]
            for i, subfolder_name in enumerate(subfolders):
                prefix = generate_prefix(i)
                subfolder_path = os.path.join(folder_path, subfolder_name)
                # Process each file in the subfolder
                for file_name in os.listdir(subfolder_path):
                    file_path = os.path.join(subfolder_path, file_name)
                    if os.path.isfile(file_path):
                        new_file_name = prefix + file_name
                        new_file_path = os.path.join(folder_path, new_file_name)
                        safe_move(file_path, new_file_path)
                        file_count += 1
                # Remove the (now empty) subfolder; if it isn’t empty for some reason, report an error.
                try:
                    os.rmdir(subfolder_path)
                except OSError as e:
                    print(f"Could not remove '{subfolder_path}': {e}")
            print(f"Finished processing '{folder_name}'. {file_count} files moved.")
            total_files += file_count
    print(f"All done. Total files moved: {total_files}")

def cli() -> None:
    import argparse
    import logging as log
    from pathlib import Path

    p = argparse.ArgumentParser(
        description="Flatten each service’s sub-folders one level up, "
                    "prefixing filenames to preserve uniqueness."
    )
    p.add_argument(
        "root",
        type=Path,
        help="root directory that contains the service folders"
    )
    p.add_argument(
        "-v", "--verbose",
        action="store_true",
        help="enable INFO-level progress logs"
    )
    args = p.parse_args()

    log.basicConfig(
        level=log.INFO if args.verbose else log.WARNING,
        format="%(levelname)s: %(message)s"
    )

    if not args.root.is_dir():
        p.error(f"'{args.root}' is not a directory")

    # `process_root_folder` expects a str.
    process_root_folder(str(args.root))

if __name__ == "__main__":
    cli()
