#!/usr/bin/env python3
import csv
import json
import os
import sys

EXEMPT_FILES = ['.DS_Store']


def process_path(file):
    if file in EXEMPT_FILES:
        return []
    file = file.rstrip('/')
    if os.path.isfile(file):
        basename = os.path.basename(file)
        if basename.startswith('.'):
            return []
        openfile = open(file)
        records = []
        for line in openfile:
            try:
                record = json.loads(line)
                records.append(record)
            except:
                pass
        return records

    elif os.path.isdir(file):
        records = []
        for subfile in os.listdir(file):
            records += process_path('%s/%s' % (file, subfile))
        return records


def convert_records(records):
    keys = set([])
    for record in records:
        for key in record.keys():
            keys.add(key)

    keys = list(keys)
    keys.sort()

    writer = csv.writer(sys.stdout, lineterminator=os.linesep)
    writer.writerow(keys)
    for record in records:
        newrow = []
        for key in keys:
            if key in record:
                newrow.append(record[key])
            else:
                newrow.append('')
        writer.writerow(newrow)


def warn(text):
    print('\x1B[31;40m%s\x1B[0m' % (text), file=sys.stdout)


help = '''
This program converts JSONL files into CSVs. It automatically identifies the
keys to create consistent headers.
This program takes an unlimited number of files as an argument. It can also be
pointed at a directory to convert all files in it.
The new CSV is sent directly to stdout and should be piped to a file:
./jsonl_to_csv.py ./path/to/file.jsonl > newfile.csv
./jsonl_to_csv.py ./path/to/files/ > newfile.csv
'''


if len(sys.argv) <= 1:
    warn('At least one file or directory is required')
    print(help, file=sys.stderr)
    sys.exit(1)

if sys.argv[1] == 'help':
    print(help)
    sys.exit(0)

records = []
for arg in sys.argv[1:]:
    if not os.path.exists(arg):
        warn('Invalid path')
        sys.exit(1)
    records += process_path(arg)

convert_records(records)
