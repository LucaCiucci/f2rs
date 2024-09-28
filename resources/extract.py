import re
import os
import time
import pdfplumber
from tqdm import tqdm

URL = "https://j3-fortran.org/doc/year/18/18-007r1.pdf"
FILE_PATH = 'resources/18-007r1.pdf'
PROCESSED_FILE_PATH = 'resources/18-007r1.rules.yaml'
OUT_FILE_PATH = 'f2rs-parse/src/rules/report/rules-18-007r1.md'
OUT_RULE_LIST_PATH = 'f2rs-parse/f2rs-parse-derive/src/rules-18-007r1.rs'

# a regexp that matches all line numbers, then space, then "R<some_digits>" then the name of the rule (letters, bumbers, -)
PDF_RULE_REGEX = re.compile(r'\n(\d+)\s+R(\d+)\s+([A-Za-z0-9-]+)')

RUST_FILE_REGEX = re.compile(r'F18V007r1 rule "([A-Za-z0-9-]+)" #(\d+)')

def extract_rule_labels(file_path):
    # map from string to string
    rules = {}

    last_write_time = 0
    with pdfplumber.open(file_path) as pdf:
        i = 0
        for page in tqdm(pdf.pages):
            i += 1
            text = page.extract_text()
            new_rules = PDF_RULE_REGEX.findall(text)
            for line_number, rule_number, rule_name in new_rules:
                rule_number = int(rule_number)
                if rule_number in rules:
                    if rules[rule_number] != rule_name:
                        raise Exception("Error: rule {} is already defined".format(rule_number))
                rules[rule_number] = rule_name
    return dict(sorted(rules.items()))

# download the file if it does not exist
if not os.path.exists(FILE_PATH):
    print("Downloading", URL)
    import urllib.request
    urllib.request.urlretrieve(URL, FILE_PATH)

if not os.path.exists(PROCESSED_FILE_PATH):
    print("Extracting rules from", FILE_PATH)
    rules = extract_rule_labels(FILE_PATH)
    with open(PROCESSED_FILE_PATH, 'w', encoding="utf-8") as f:
        for rule_number, rule_name in rules.items():
            f.write(f"- standard: F18V007r1\n  ident: \"{rule_name}\"\n  number: {rule_number}\n")
