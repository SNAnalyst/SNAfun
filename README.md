# SNAfun
FUNctions to enjoy alongside the SNA4DS course

The following API is used (for now):

- add_* : add stuff to network objects
- count_* : counting things in the graph, such as the number of vertices, number of edges, dyad census, triad census
- create_* : create graphs (in `igraph` or `network` format) with specific characteristics
- extract_* : access specific data from (network) objects
- find_* : identify specific parts of the network, such as who the isolates are
- is_* : checks, such as whether a network is bipartite
- has_* : checks, such as whether a network has loops
- make_* : create objects based on graphs, ie. make a matrix from vertex attributes, make a mixing_matrix, etc.
- plot_* : several plot functions
- remove_* : delete parts of the network, such as remove the isolates
- to_* : conversion methods, converting from `igraph`, `network`, `matrix`, `data.frame` to `igraph`, `network`, `matrix`, `data.frame`

Then, there are indices/measures at three levels:
- e_* : edge level indices
- g_* : graph level indices, such as betweenne`ss cent`ralization
- v_* : vertex level indices, such as betweenness

Then, there are some other functions for which there is no specific entry in the API yet. But, hey, naming may just all change again anyway...
