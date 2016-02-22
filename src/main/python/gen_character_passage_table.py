import argparse
import csv
import json

from collections import defaultdict


"""
Generates a .tsv table showing each character tag and the passages in which its
been used.
"""


def main():
	parser_description = ("Generates a .tsv table showing each character tag "
		"and the passages in which its been used.")
	parser = argparse.ArgumentParser(description=parser_description)

	parser.add_argument('frankenbrat_json_filepath',
		help="Path to Frankenbrat .json file with annotation data.")
	parser.add_argument('output_filepath', help="Path to output .tsv file")
	
	args = parser.parse_args()

	table = defaultdict(list)

	with open(args.frankenbrat_json_filepath) as f:
		data = json.load(f)

		entity_types = [e['type'] for e in data['collData']['entity_types']]
		docs = data['docData']['docs']

		for t in entity_types:
			for d in docs:
				for e in d['entities']:
					if t == e[1]:
						table[t].append(d['name'])
						break

	with open(args.output_filepath, 'wb') as f:
		writer = csv.writer(f, delimiter=',', quotechar='"')

		for t, dns in sorted(table.items(), key=lambda i: i[0]):
			writer.writerow([t] + dns)


if __name__ == '__main__':
	main()
