import hashlib
import os
import sys


def hash_file_b2sum(filepath):
    hasher = hashlib.blake2b()
    with open(filepath, "rb") as f:
        while chunk := f.read(8192):
            hasher.update(chunk)
    return hasher.hexdigest()


def rename_file_with_hash(filepath):
    if not os.path.isfile(filepath):
        print(f"Error: '{filepath}' is not a valid file.")
        return

    dirname, filename = os.path.split(filepath)
    name, ext = os.path.splitext(filename)
    hash_value = hash_file_b2sum(filepath)
    new_filename = f"{name}-{hash_value}{ext}"
    new_filepath = os.path.join(dirname, new_filename)

    os.rename(filepath, new_filepath)
    print(f"Renamed: {filepath} → {new_filepath}")


if __name__ == "__main__":
    args = sys.argv[1:]
    if not args:
        print(f"Usage: {sys.argv[0]} <filename>")
    else:
        rename_file_with_hash(sys.argv[1])
