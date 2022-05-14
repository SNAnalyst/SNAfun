# SNAfun
FUNctions to enjoy alongside the SNA4DS course

The following API is used (for now):

- count_* : counting things in the graph, such as the number of vertices, number of edges, dyad census, triad census
- create_* : create graphs (in `igraph` or `network` format) with specific characteristics
- make_* : create objects based on graphs, ie. make a matrix from vertex attributes, make a mixing_matrix, etc.
- to_* : conversion methods, converting from `igraph`, `network`, `matrix`, `data.frame` to `igraph`, `network`, `matrix`, `data.frame`

Then, there are measures at three levels:
- e_* : edge level indices
- g_* : graph level indices, such as betweenne`ss cent`ralization
- v_* : vertex level indices, such as betweenness

Then, there are some other functions for which there is no specific entry in the API yet. But, hey, naming may just all change again anyway...
