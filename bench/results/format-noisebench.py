#!/usr/bin/env python3

import csv
import sys
import os

def process_benchmarks(input_filepath):
    """
    Reads benchmark CSV data, filters for specific libraries, sorts by speed,
    and saves a new CSV with just the name and human-readable items/sec.
    """

    # 1. Determine output filepath
    if not os.path.exists(input_filepath):
        print(f"Error: File not found at '{input_filepath}'", file=sys.stderr)
        sys.exit(1)

    base_name = os.path.splitext(input_filepath)[0]
    output_filepath = f"{base_name}-vps.csv"

    # 2. Read and find header
    lines_to_process = []
    found_header = False
    try:
        with open(input_filepath, 'r') as f:
            for line in f:
                if found_header:
                    lines_to_process.append(line)
                elif line.startswith("name,"):
                    found_header = True
                    lines_to_process.append(line)
                    
    except Exception as e:
        print(f"Error reading file: {e}", file=sys.stderr)
        sys.exit(1)

    if not found_header:
        print("Error: Could not find CSV header 'name,...' in file.", file=sys.stderr)
        sys.exit(1)

    # 3. Parse, Filter, and Format
    reader = csv.DictReader(lines_to_process)
    data_to_sort = []

    for row in reader:
        name = row.get('name', '').strip('"')
        items_str = row.get('items_per_second')

        # Filter to FastNoiseLite and FastNoiseSIMD only
        if ('FastNoiseLite' in name or 'FastNoiseSIMD' in name) and items_str:
            try:
                # Value for sorting
                items_float = float(items_str)
                
                # Human-readable value for output CSV
                items_int = int(items_float)
                items_human = f"{items_int:_}"
                
                # Create the new row dict with only the columns we want
                new_row = {
                    'name': name,
                    'items_per_second_human': items_human
                }
                
                # Add to a list with its sort value
                data_to_sort.append((items_float, new_row))
                
            except (ValueError, TypeError):
                continue # Skip rows with unparseable numbers

    # 4. Sort by speed (fastest first)
    sorted_data = sorted(
        data_to_sort, 
        key=lambda item: item[0],  # Sort by the float value
        reverse=True
    )
    
    # Get just the final dictionaries for writing
    final_rows = [item[1] for item in sorted_data]

    # 5. Write to new CSV
    output_fieldnames = ['name', 'items_per_second_human']
    try:
        with open(output_filepath, 'w', newline='') as f:
            writer = csv.DictWriter(f, fieldnames=output_fieldnames)
            writer.writeheader()
            writer.writerows(final_rows)
        
        print(f"Successfully created: {output_filepath}")

    except Exception as e:
        print(f"Error writing file: {e}", file=sys.stderr)
        sys.exit(1)

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: ./process_bench.py <path_to_results.txt>", file=sys.stderr)
        sys.exit(1)
        
    filepath = sys.argv[1]
    process_benchmarks(filepath)
