import json
import os
import sys


"""
Converts brat alias annotations in an .ann file and the corresponding annotation
document to the brat .json format expected by Frankenbrat. (The output has
the pre-window and post-window, along with markers, of the annotation document
stripped.)
"""


# Alias tag.
ALIAS_TAG = "ALIAS"


# Desginates the beginning of the text to be annotated in an annotation
# docment.
SHEET_START = "<===== BEGIN ANNOTATION =====>\n" + ("-" * 30)
# Designates the end of the text to be annotated in an annotation document.
SHEET_END = ("-" * 28) + "\n<===== END ANNOTATION =====>"


def get_stripped_text(txt_filepath):
	"""
	Strips the pre-window and post-window for the annotation document referred
	to by the given filepath, including the annotation area markers, returning
	the annotation area text.
	"""

	with open(txt_filepath) as f:
		text = f.read()
		start_ind = text.find(SHEET_START) + len(SHEET_START)
		end_ind = text.find(SHEET_END)

		return text[start_ind: end_ind].strip()


def get_entities(ann_filepath, txt_filepath):
	"""
	Returns the entities specified in the .ann file referred to by the first
	argument in brat .json format. The corresponding annotation document,
	referred to by the second argument, is used to adjust the offsets when
	pre-window and post-window, along with markers, are stripped.
	"""

	with open(txt_filepath) as txt:
		pre_len = txt.read().find(SHEET_START) + len(SHEET_START)

		with open(ann_filepath) as ann:
			entities = []
			
			for line in ann:
				col_entries = line.split("\t")
				col1_subentries = col_entries[1].split()
				
				ann_id = col_entries[0]
				
				# The '-2' accounts for the line seperator following the start
				# annotation marker.
				start_offset = int(col1_subentries[1]) - pre_len - 2
				if ";" in col1_subentries[2]:
					end_offset = int(col1_subentries[3]) - pre_len - 2
				else:
					end_offset = int(col1_subentries[2]) - pre_len - 2

				entities.append([
					ann_id,
					ALIAS_TAG,
					[[start_offset, end_offset]]
				])

			return entities


def main(in_annpath, in_txtpath, out_jsonpath, name):
	doc_data = {
		"name": name,
		"text": get_stripped_text(in_txtpath),
		"entities": get_entities(in_annpath, in_txtpath)
	}

	# Output to a .json file.
	with open(out_jsonpath, "w") as f:
		f.write(json.dumps(doc_data, indent=4, sort_keys=True))


if __name__ == "__main__":
	try:
		in_annpath = sys.argv[1]
	except:
		sys.exit("ERROR: Expected a path to an .ann file or directory with "
			".ann files as the first argument.")
	
	try:
		in_txtpath = sys.argv[2]
	except:
		sys.exit("ERROR: Expected a path to the corresponding annotation "
			"document .txt file as the second argument.")

	try:
		out_jsonpath = sys.argv[3]
	except:
		sys.exit("ERROR: Expected a path to the output Frankenbrat .json file "
			"as the third argument.")

	try:
		name = sys.argv[4]
	except:
		sys.exit("ERROR: Expected the name of the ouput document as the fourth "
			"argument.")

	main(in_annpath, in_txtpath, out_jsonpath, name)
