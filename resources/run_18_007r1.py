import re
import os
import time
import pdfplumber
from tqdm import tqdm

URL = "https://j3-fortran.org/doc/year/18/18-007r1.pdf"
FILE_PATH = 'resources/18-007r1.pdf'
PROCESSED_FILE_PATH = 'resources/18-007r1.rules.txt'
OUT_FILE_PATH = 'f2rs-parse/src/rules/report/rules-18-007r1.md'

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
            f.write("F18V007r1 rule \"{}\" #{}\n".format(rule_name, rule_number))

def find_all_rules_in_rust_file(file_path):
    # map from string to state
    rules = {}

    with open(file_path, 'r', encoding="utf-8") as f:
        file_content = f.read()
        new_rules = RUST_FILE_REGEX.findall(file_content)
        for rule_name, rule_number in new_rules:
            rule_number = int(rule_number)
            if rule_number in rules:
                raise Exception("Error: rule {} is already defined".format(rule_number))
            rules[rule_number] = rule_name
    return dict(sorted(rules.items()))

def find_all_rules_in_rust_files_recursive(dir_path):
    # set of strings
    rules = {}

    for file_path in os.listdir(dir_path):
        if file_path.endswith(".rs"):
            file_path = os.path.join(dir_path, file_path)
            print("Processing file", file_path)
            new_rules = find_all_rules_in_rust_file(file_path)
            # merge dictionaries
            rules = {**rules, **new_rules}
        else:
            path = os.path.join(dir_path, file_path)
            if os.path.isdir(path):
                new_rules = find_all_rules_in_rust_files_recursive(path)
                # merge dictionaries
                rules = {**rules, **new_rules}
    return dict(sorted(rules.items()))

def run():
    RULES = find_all_rules_in_rust_file(PROCESSED_FILE_PATH)
    IMPLEMENTED_RULES = find_all_rules_in_rust_files_recursive("f2rs-parse/src")

    def write_file():
        number_of_rules = len(RULES)
        number_of_implemented_rules = len(IMPLEMENTED_RULES)
        with open(OUT_FILE_PATH, 'w', encoding="utf-8") as f:
            f.write("# Rules implemented for 18-007r1\n\n")
            if number_of_rules == 0:
                f.write("No rules found!\n\n")
            if number_of_implemented_rules == number_of_rules:
                f.write("All rules implemented!\n\n")
            else:
                f.write("Progress: {}/{} ({:.2f}%)\n\n".format(number_of_implemented_rules, number_of_rules, number_of_implemented_rules / number_of_rules * 100))
                # progressbar, 50 characters wide
                #progressbar_length = 50
                #progressbar = int(number_of_implemented_rules / number_of_rules * progressbar_length)
                #f.write("```\n[{}{}]({:.2f}%)\n```\n\n".format("=" * progressbar, " " * (progressbar_length - progressbar), number_of_implemented_rules / number_of_rules * 100))
                svg = """
<svg width="50%" viewBox="0 0 100 5" preserveAspectRatio="none" xmlns="http://www.w3.org/2000/svg">
    <rect x="0" y="0" width="100" height="20" fill="gray"></rect>
    <rect x="0" y="0" width="{:.2f}" height="20" fill="cyan"></rect>
</svg>
""".format(number_of_implemented_rules / number_of_rules * 100)
                f.write("{}\n\n".format(svg))
            f.write("| R# | Rule name | Implemented |\n")
            f.write("| --- | --- | --- |\n")
            for rule in RULES.items():
                rule_number = rule[0]
                rule_name = rule[1]
                implemented = rule_number in IMPLEMENTED_RULES
                if implemented:
                    implemented_rule_name = IMPLEMENTED_RULES[rule_number]
                    if rule_name != implemented_rule_name:
                        raise Exception("Error: rule {} is implemented as {} but should be {}".format(rule_number, implemented_rule_name, rule_name))
                    f.write("| R{} | _{}_ | ✅ |\n".format(rule_number, rule_name))
                else:
                    f.write("| R{} | _{}_ | ❌ |\n".format(rule_number, rule_name))

    print("Found standard has {} rules".format(len(RULES)))
    print("Found {} implemented rules".format(len(IMPLEMENTED_RULES)))
    write_file()