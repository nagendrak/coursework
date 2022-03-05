import random
from math import log
import copy

def read_input(filename):
    input_file = open(filename, 'r')

    nodes = []
    edges = []
    for line in input_file:
        this_list_ints = list(map(int, line.split()))
        nodes.append(this_list_ints[0])

        for i in range(1, len(this_list_ints)):
            if this_list_ints[i] > this_list_ints[0]:
                edges.append([this_list_ints[0], this_list_ints[i]])

    print("Number of nodes (n): ", len(nodes))
    print("Number of edges (m): ", len(edges))
    return nodes, edges

def replace_node_in_edges(edges, node_to_replace, node_to_replace_with):
    for edge in edges:
        if edge[0] == node_to_replace:
            edge[0] = node_to_replace_with
            continue
        elif edge[1] == node_to_replace:
            edge[1] = node_to_replace_with
            continue

def delete_self_loop_edges(edges):
    num_edges_deleted = 0
    for i in range(len(edges)):
        j = i - num_edges_deleted
        if edges[j][0] == edges[j][1]:
            del edges[j]
            num_edges_deleted += 1

def delete_node(nodes, node_to_delete):
    num_nodes_deleted = 0
    for i in range(len(nodes)):
        j = i - num_nodes_deleted
        if nodes[j] == node_to_delete:
            del nodes[j]
            num_nodes_deleted += 1

def contract_graph_at_random_edge(nodes, edges):
    # Generate a random edge index to delete
    edge_index_to_delete = random.randint(0, len(edges) - 1)
    # print("Edge index being deleted: ", edge_index_to_delete, " out of ", len(edges))

    edge_to_delete = edges[edge_index_to_delete]
    head_node = edge_to_delete[0]
    tail_node = edge_to_delete[1]

    # Replace tail_vertex with head_vertex in all edges
    replace_node_in_edges(edges, tail_node, head_node)

    # Delete self-loop edges
    # print("Before: edges ", len(edges), ", nodes ", len(nodes))
    delete_self_loop_edges(edges)

    # Delete tail_vertex
    delete_node(nodes, tail_node)
    # print("After: edges ", len(edges), ", nodes ", len(nodes))

def attempt_min_cut(nodes, edges):
    while len(nodes) > 2:
        contract_graph_at_random_edge(nodes, edges)

    return len(edges)

def compute_karger_min_cuts(original_nodes, original_edges):
    num_min_cuts = 0

    n = len(original_nodes)
    m = len(original_edges)

    num_iterations = int(n * n * log(n))
    print("Number of random iterations: ", num_iterations)

    for iteration in range(num_iterations):
        nodes_copy = copy.deepcopy(original_nodes)
        edges_copy = copy.deepcopy(original_edges)

        current_min_cuts = attempt_min_cut(nodes_copy, edges_copy)

        num_min_cuts = current_min_cuts if iteration == 0 else min(num_min_cuts, current_min_cuts)

        print("Min cuts, this iteration: ", current_min_cuts, " overall: ", num_min_cuts)

    return num_min_cuts

#random.seed(1)

original_nodes, original_edges = read_input("kargerMinCut.txt")
# original_nodes, original_edges = read_input("input_random_10_25.txt")
# original_nodes, original_edges = read_input("input_random_40_200.txt")

print("Number of min cuts: ", compute_karger_min_cuts(original_nodes, original_edges))
