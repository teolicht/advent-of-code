from collections import defaultdict

def read_input(filename):
    with open(filename, "r") as file:
        return file.readlines()

def parse(input):
    """Take lines from input, strip them and separate into list of lines with page pairs
    and list of lines with the updates."""
    rules = []
    orders = []
    for line in input:
        line = line.strip()
        if "|" in line:
            pair = line.split("|")
            rules.append((pair[0], pair[1]))
        elif "," in line:
            orders.append(line.split(","))
    return (rules, orders)

def generate_rules_dict(rules):
    """
    Take pairs of rules and generate a dict, where each key is a page number 
    and the value is a list containing all the pages that should come after that key.
    """
    rules_dict = defaultdict(list)
    for pair in rules:
        rules_dict[pair[0]].append(pair[1])
    return rules_dict

def generate_indeg_dict(rules_dict, unique_pages):
    """
    Count how many times each number appears in the other numbers' lists.
    """
    indeg_dict = {page: 0 for page in unique_pages}
    for k, v in rules_dict.items():
        for page in v:
            indeg_dict[page] += 1
    return indeg_dict

def extract_relevant_rules(order, rules):
    """
    From the given order, only select the rules that will be relevant for sorting that order.
    """
    relevant_rules = []
    for a, b in rules: 
        if a in order and b in order:
            relevant_rules.append((a, b))
    return relevant_rules

def sort_order(order, rules):
    """
    1. Extract relevant rules
    2. Generate indeg dict out of rules
    3. Sort order according to indegs
    """
    relevant_rules = generate_rules_dict(extract_relevant_rules(order, rules))
    indeg_dict = generate_indeg_dict(relevant_rules, order)
    # Sort indeg_dict according to values (degrees should be in ascending order)
    correct_order = dict(sorted(indeg_dict.items(), key=lambda item: item[1]))
    return list(correct_order.keys())

def check_all_orders(orders, rules):
    correct_sum = 0
    wrong_sum = 0
    for order in orders:
        sorted_order = sort_order(order, rules)
        if order == sorted_order: 
            middle_page = int(order[int(len(order) / 2)])
            correct_sum += middle_page
        else:
            middle_page = int(sorted_order[int(len(sorted_order) / 2)])
            wrong_sum += middle_page 
    return (correct_sum, wrong_sum)

parsed_input = parse(read_input("input.txt"))
rules = parsed_input[0]
orders = parsed_input[1]
result = check_all_orders(orders, rules)
print("Sum of middle page numbers of correct orders: %d" % result[0])
print("Sum of middle page numbers of incorrect orders: %d" % result[1])
