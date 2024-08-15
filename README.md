# SNAfun
FUNctions to enjoy alongside the SNA4DS course

The following API is used:

- add_* : add stuff to network objects
- count_* : counting things in the graph, such as the number of vertices, number of edges, dyad census, triad census
- create_* : create graphs (in `igraph` or `network` format) with specific characteristics
- extract_* : access specific data from (network) objects
- is_* : checks, such as whether a network is bipartite
- has_* : checks, such as whether a network has loops
- list_*: list characteristics, such as `list_edge_attributes`
- make_* : create objects based on graphs, ie. make a matrix from vertex attributes, make a mixing_matrix, etc.
- plot_* : several plot functions
- remove_* : delete parts of the network, such as remove the isolates
- stat_* : help functions for statistical modeling
- to_* : conversion methods, converting from `igraph`, `network`, `matrix`, `data.frame` to `igraph`, `network`, `matrix`, `data.frame`

Then, there are indices/measures at three levels:
- e_* : edge level indices
- g_* : graph level indices, such as betweenness centralization
- v_* : vertex level indices, such as betweenness

Finally, a few some functions don't fit within a clear category and don't follow this API.