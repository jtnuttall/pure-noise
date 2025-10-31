#!/usr/bin/env python3

import csv
import math
import os
import re
import sys

try:
  from tabulate import tabulate
except ImportError:
  def tabulate(vs, headers):
    rs = [headers]
    for row in vs:
      rs.append('| ' + ' | '.join(map(str, row)) + ' |')
    return '\n'.join(rs)

PICO_PER_SEC = 1e12

def main():
  try:
    file_name = sys.argv[1]
    out_file_name = os.path.splitext(file_name)[0] + '-vps.csv'
  except IndexError:
    raise 'file name is required'

  results = []
  print(f'reading {file_name}')
  with open(file_name, 'r') as f, open(out_file_name, 'w') as out:
    reader = csv.reader(f)
    next(reader)
    writer = csv.writer(out)
    writer.writerow(['name', '# values created', 'time (p)', 'noise values per second'])
    for row in reader:
      name = row[0]
      time = row[1]
      time = float(time)
      try:
        ngen = float(re.match(r'.*\sx([0-9]+)$', name).group(1))
      except:
        print(f'could not parse iterations for "{name}"')
        continue
      secs = time / PICO_PER_SEC
      vps = math.floor(ngen / secs)
      result = [name, ngen, secs, vps]
      results.append(result)
      writer.writerow(result)
  
  results.sort(key=lambda r: r[3], reverse=True)
  headers = ['name', 'values / second']
  results = [[n, f'{vps:_}'] for n,ngen,secs,vps in results]
  print(tabulate(results, headers, tablefmt="github"))

if __name__ == '__main__':
  main()
