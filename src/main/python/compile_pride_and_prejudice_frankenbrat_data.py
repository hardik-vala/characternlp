import json
import os

import ann_doc_to_frankenbrat as adtf


# Path to final pride-and-prejudice alias annotations.
ANN_DIRPATH = "../../../data/annotations/alias-ident/pride-and-prejudice/final"
# Path to pride-and-prejudice annotation documents.
DOC_DIRPATH = "../../../data/annotations/alias-ident/pride-and-prejudice/passages"

# Path to output Frankenbrat .json file.
OUTPUT_FILEPATH = "../../../data/annotations/character-resolution/pride-and-prejudice/init.json"


# Annotation file extension.
ANN_EXT = ".ann"
# Annotation document extension.
DOC_EXT = ".txt"


def get_passages(alias_ann_dirpath):
	"""
	Returns the passages which have been alias-annotated in the given directory.
	The passages are returned as a list with each passage represented in the
	following format:

	{
		"chapter": (Chapter number as an int),
		"id": (Id number within the chapter as an int)
	}
	"""

	passages = []

	for root, dirnames, fnames in os.walk(alias_ann_dirpath):
		for fname in fnames:
			if fname.endswith(ANN_EXT):
				passages.append({
					"chapter": int(root.split("-")[-1]),
					"id": int(fname.split(".")[0])
				})

	# Sorts them by chapter #, followed by passage #.
	return sorted(passages, key=lambda p: (p["chapter"], p["id"]))


def main():
	docs_data = []

	for passage in get_passages(ANN_DIRPATH):
		chapter = passage["chapter"]
		pid = passage["id"]

		ann_filepath = os.path.join(os.path.join(ANN_DIRPATH,
			"chapter-" + str(chapter)), str(pid) + ANN_EXT)
		doc_filepath = os.path.join(os.path.join(DOC_DIRPATH,
			"chapter-" + str(chapter)), str(pid) + DOC_EXT)

		docs_data.append({
			"name": "Chapter %d, Passage %d" % (chapter, pid),
			"text": adtf.get_stripped_text(doc_filepath),
			"entities": adtf.get_entities(ann_filepath, doc_filepath)
		})

	output = {
		"collData": {
	        "entity_types": [
	            {
	                "type": "ALIAS",
	                "labels" : ["ALIAS", "AL"],
	                "bgColor": "red",
	                "borderColor": "darken"
	            },
	            {
	                "type": "NON-CHARACTER",
	                "labels": ["NON-CHARACTER", "NON"],
	                "bgColor": "#FFFFFF",
	                "borderColor": "darken"
	            },
	            {
	                "type": "OTHER",
	                "labels": ["OTHER"],
	                "bgColor": "#BBBBBB",
	                "borderColor": "darken"
	            },
	            {
	                "type": "???",
	                "labels": ["???"],
	                "bgColor": "#777777",
	                "borderColor": "darken"
	            },
	            {
	                "type": "UNRESOLVED",
	                "labels" : ["UNRESOLVED"],
	                "bgColor": "#39FF14",
	                "borderColor": "darken"
	            }
	        ]
	    },
	    "docData": { "docs": docs_data },
	    "log": [{ "text": "Hardik: Frankenbrat is awesome!." }]
	}

	# Output to a .json file.
	with open(OUTPUT_FILEPATH, "w") as f:
		f.write(json.dumps(output, indent=4, sort_keys=True))


if __name__ == "__main__":
	main()
