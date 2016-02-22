import csv
import networkx as nx
import matplotlib.pyplot as plt
import sys


# Loads the character graph saved as a .csv file at the given path.
def _load_graph(path):
	# Labels designating different graph entities in the saved character graph .csv file.
	node_label = "NODE"
	edge_label = "EDGE"
	antiedge_label = "ANTIEDGE"

	with open(path, "rb") as f:
		reader = csv.reader(f, delimiter=",", quotechar="\"")

		nodes, edges, antiedges = [], [], []

		for row in reader:
			if (len(row) == 2 and row[1] == node_label):
				nodes.append(row[0])
			elif (len(row) > 2 and row[2] == edge_label):
				edges.append((row[0], row[1]))
			elif (len(row) > 2 and row[2] == antiedge_label):
				antiedges.append((row[0], row[1]))

		return { "nodes": nodes, "edges": edges, "antiedges": antiedges }

# Draws the given graph.
def draw_graph(graph):
	g = nx.Graph()

	for node in graph["nodes"]:
		g.add_node(node)

	for edge in graph["edges"]:
		g.add_edge(edge[0], edge[1])

	pos = nx.spring_layout(g)
	nx.draw(g, pos)

	plt.show()


if __name__ == "__main__":
	try:
		draw_graph(_load_graph(sys.argv[1]))
	except IndexError:
		sys.exit("ERROR: Expected filepath to the character graph .csv file as the first argument.")
