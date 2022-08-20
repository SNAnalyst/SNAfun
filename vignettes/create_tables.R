


#### general layout settings ---------------------------------------------------
basic_theme <- function(data) {
  gt::gt(
    data,
    groupname_col = "topic") |> 
    gt::tab_options(
      # data,
      column_labels.hidden = TRUE,
      table_body.hlines.style = "dotted",  # tussen de rijen binnen een groep
      table.border.bottom.color = "white",  # lijn onderaan
      table.border.bottom.width = gt::px(3),
      data_row.padding = 5,   # vertical padding around data cell content
      data_row.padding.horizontal = 20
    ) |>
    gt::fmt_markdown(    # toon code als rmarkdown layout
      columns = c(code)) |> 
    gt::tab_style(         # font and size title
      style = gt::cell_text(
        font = gt::google_font("Titan One"),
        align = "center",
        size = "xx-large"
      ),
      locations = gt::cells_title("title")
    ) |> 
    gt::sub_missing(      # replace missings
      columns = c(code),
      missing_text = "---"
    ) |>
    gt::sub_missing(      # replace missings: NOTE columns
      columns = c(note),
      missing_text = ""
    ) |> 
    gt::tab_style(     # text in note cursief maken
      style = gt::cell_text(
        style = 'italic',
      ),
      locations = gt::cells_body(columns = "note")
    ) |> 
    gt::tab_style(  # make row group labels bold + layout
      gt::cell_text(
        weight = "bold",
        align = "left",
        size = "medium",
        transform = "uppercase"
      ) ,
      locations = gt::cells_row_groups()
    ) |> 
    gt::cols_hide("note") |> 
    gt::tab_style(     # text in note cursief maken
      style = gt::cell_text(
        style = 'italic',
      ),
      locations = gt::cells_body(columns = "note")
    )
}

#### CREATE GRAPHS ######################
df_create <- rbind(
  ###### Create an empty network ----
  c(
    "Create an empty network",
    "snafun",
    '
    create_empty_graph(n_vertices, directed = TRUE, graph = c("igraph", "network"))
    ',
    NA
  ),
  c(
    "Create an empty network",
    "igraph",
    "
    make_empty_graph()
    ",
    NA
  ),
  c(
    "Create an empty network",
    "network",
    "
    network::network::initialize()
    ",
    "The output is a matrix object"
  ),
  ###### Create a ring network ----
  c(
    "Create a ring network",
    "snafun",
    NA,
    NA
  ),
  c(
    "Create a ring network",
    "igraph",
    "
    make_ring()
    ",
    NA
  ),
  c(
    "Create a ring network",
    "network",
    NA,
    NA
  ),
  ####
  c(
    "Create a star network",
    "snafun",
    NA,
    NA
  ),
  c(
    "Create a star network",
    "igraph",
    "
    make_star()
    ",
    NA
  ),
  c(
    "Create a star network",
    "network",
    NA,
    NA
  ),
  ###### given density ----
  c(
    "Create a random graph with given density"
    , "snafun"
    , '
    create_random_graph(
        n_vertices = 10,
        strategy = "gnm",   # to fix the number of edges
        m = 27,             # number of edges required, resulting density = .3
        directed = TRUE,    # or FALSE
        graph = c("igraph", "network"))   # pick one

    create_random_graph(
        n_vertices = 10,
        strategy = "gnp",   # to fix the probability edges
        p = .3,             # probability for each edge, yields approx. density of .3
        directed = TRUE,    # or FALSE
        graph = c("igraph", "network"))
    '   
    , NA
  ),
  c(
    "Create a random graph with given density"
    , "igraph"
    , "
    sample_gnm()
    sample_gnp()
    
    make_directed_graph(n = 10, edges = 27)
    make_undirected_graph(n = 10, edges = 27)
    "
    , NA
  ),
  c(
    "Create a random graph with given density",
    "network",
    "
    # 10 vertices, density on average .3
    sna::rgraph(n = 10, m = 1, tprob = 0.30, mode = 'digraph')

    # 10 vertices, 27 edges (ie. density = .3)
    sna::rgnm(1, 10, m = 27)
    ",
    "output_is_matrix"
  ),
  ###### given dyad census ----
  c(
    "Create a random graph with given dyad census"
    , "snafun"
    , NA  
    , NA
  ),
  c(
    "Create a random graph with given dyad census"
    , "igraph"
    , NA
    , NA
  ),
  c(
    "Create a random graph with given dyad census",
    "network",
    '
    # 5 networks, each with 10 vertices, and 8 M, 25 A, and 12 N dyads
    sna::rguman(n = 5, nv = 10, mut = 8, asym = 25, null = 12, 
      method = "exact")
    ',
    "output_is_matrix"
  ),
  ###### random bipartite ----
  c(
    "Create random bipartite graph"
    , "snafun"
    , '
    create_bipartite(
      n_type1,                        # number of vertices of type 1
      n_type2,                        # number of vertices of type 2
      strategy = c("gnp", "gnm"),
      p,                              # probability of each cross-type edge
      m,                              # number of cross-type edges
      directed = FALSE,
      mode = c("out", "in", "all"),
      graph = c("igraph", "network")
    )
    '   
    , NA
  ),
  c(
    "Create random bipartite graph"
    , "igraph"
    , "
    igraph::sample_bipartite()
    "
    , NA
  ),
  c(
    "Create random bipartite graph",
    "network",
    "
    network::network.bipartite()
    ",
    "network_bip"
  )
  ###### graph from input data ----
  ,
  c(
    "Create graph object from input data"
    , "snafun"
    , '
    #  `x` can be: 
    #   - an edgelist (in data.frame format)
    #   - an incidence matrix (in matrix format)--for a bipartite graph
    #   - an adjacency matrix (in matrix format)
    #   - `vertices`  can be a data.frame containing vertex attributes
    
    to_igraph(x, bipartite = FALSE, vertices = NULL)
    
    to_network(x, bipartite = FALSE, vertices = NULL)
    '   
    , NA
  ),
  c(
    "Create graph object from input data"
    , "igraph"
    , '
    # from an adjacency matrix
    graph_from_adjacency_matrix(adjmatrix, 
      mode = c("directed", "undirected", "max", "min", "upper", "lower", "plus"),
      weighted = NULL, diag = TRUE, add.colnames = NULL, add.rownames = NA)
    
    make_graph(edges, ..., n = max(edges), isolates = NULL, directed = TRUE, 
      dir = directed, simplify = TRUE)
    
    # from an adjacency list
    graph_from_adj_list(adjlist, mode = c("out", "in", "all", "total"), 
      duplicate = TRUE)
    
    # from an edgelist
    graph_from_edgelist(el, directed = TRUE)
    
    # from a data.frame
    graph_from_data_frame(d, directed = TRUE, vertices = NULL)
    
    # from an incidence matrix (in matrix format)
    graph_from_incidence_matrix(incidence, directed = FALSE, 
      mode = c("all", "out", "in", "total"), multiple = FALSE,
      weighted = NULL, add.names = NULL)
    '
    , NA
  ),
  c(
    "Create graph object from input data",
    "network",
    '
    # x can be an adjacency matrix (as a matrix), an incidence matrix (as a matrix), 
    # or an edgelist (as a data.frame)
    network::network(x, vertex.attr = NULL, vertex.attrnames = NULL, directed = TRUE,
      hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = FALSE, ...)
    
    # if x is a data.frame
    network::as.network(x, directed = TRUE, vertices = NULL, hyper = FALSE, 
      loops = FALSE, multiple = FALSE, bipartite = FALSE, 
      bipartite_col = "is_actor", ...)
    
    # if x is a matrix
    as.network(x, matrix.type = NULL, directed = TRUE, hyper = FALSE, 
      loops = FALSE, multiple = FALSE, bipartite = FALSE, ignore.eval = TRUE,
      names.eval = NULL, na.rm = FALSE, edge.check = FALSE, ...)
    ',
    NA
  )
  ) |> 
  as.data.frame() |> 
  magrittr::set_colnames(c("topic", "pkg", "code", "note"))



###### CREATE table ----
table_create <- df_create |> 
  basic_theme() |> 
  gt::tab_header(        # add table title
    title = "Graph creation",
  ) |>
  gt::tab_footnote(
    footnote = "The output is a matrix object, not a 'network' object.",
    locations = gt::cells_body(
      rows = note == "output_is_matrix",
      columns = c(pkg)
    )
  ) |> 
  gt::tab_footnote(
    footnote = "This function is quite unintuitive and cumbersome",
    locations = gt::cells_body(
      rows = note == "network_bip",
      columns = c(pkg)
    )
  )




#### CONVERT ----------------------------------------------------------------
df_convert <- rbind(
  ###### to adjmat ----
  c(
    "Convert to an adjacency matrix",
    "igraph",
    '
    as_adjacency_matrix(g, sparse = FALSE)
    ',
    NA
  ),
  c(
    "Convert to an adjacency matrix",
    "network",
    '
    network::as.sociomatrix(g)
    network::as.matrix.network(flomar_network)
    
    ',
    NA
  ),
  ###### to adjlist ----
  c(
    "Convert to an adjacency list",
    "igraph",
    '
    igraph::as_adj_list(g)
    ',
    NA
  ),
  c(
    "Convert to an adjacency list",
    "network",
    NA,
    NA
  ),
  ###### to edgelist ----
  c(
    "Convert to an edge list",
    "igraph",
    '
    igraph::as_edgelist(g)
    igraph::as_data_frame(g)
    ',
    NA
  ),
  c(
    "Convert to an edge list",
    "network",
    '
    network::as.data.frame.network(g)
    network::as.edgelist(g)
    
    # if you want to include edge weights
    sna::as.edgelist.sna(g)
    
    ',
    NA
  ),
  ###### make directed ----
  c(
    "Make the network directed",
    "snafun",
    NA,
    NA
  ),
  c(
    "Make the network directed",
    "igraph",
    "
    igraph::as.directed(g)
    ",
    NA
  ),
  c(
    "Make the network directed",
    "network",
    NA,
    NA
  ), 
  ###### make undirected ----
  c(
    "Make the network undirected",
    "snafun",
    '
    # from an adjacency matrix as input
    to_symmetric_matrix(g,
      rule = c("weak", "mutual", "out", "in", "average", "max", "min"),
      na.rm = TRUE)
    ',
    NA
  ),
  c(
    "Make the network undirected",
    "igraph",
    "
    igraph::as.undirected(g)

    # for example
    igraph::as.undirected(g, mode = 'collapse', edge.attrib.comb = list(weight = 'sum'))
    ",
    NA
  ),
  c(
    "Make the network undirected",
    "network",
    '
    # from an adjacency matrix as input
    sna::symmetrize(g)

    # for example
    sna::symmetrize(g, rule = "strong")
    ',
    NA
  ) , 
  ###### project ----
  c(
    "Project a bipartite graph",
    "snafun",
    NA,
    NA
  ),
  c(
    "Project a bipartite graph",
    "igraph",
    "
    igraph::bipartite.projection(g)
    ",
    NA
  ),
  c(
    "Project a bipartite graph",
    "network",
    NA,
    NA
  ) , 
  ###### line graph ----
  c(
    "Convert to a line graph",
    "snafun",
    NA,
    NA
  ),
  c(
    "Convert to a line graph",
    "igraph",
    "
    igraph::make_line_graph(g)
    ",
    NA
  ),
  c(
    "Convert to a line graph",
    "network",
    NA,
    NA
  )
) |> 
  as.data.frame() |> 
  magrittr::set_colnames(c("topic", "pkg", "code", "note"))

###### CONVERT table ----
table_convert <- df_convert |> 
  basic_theme() |> 
  gt::tab_header(        # add table title
    title = "Graph object conversion",
  ) |> 
  gt::tab_footnote(
    footnote = "These functions have many additional arguments",
    locations = gt::cells_body(
      rows = note == "more_args",
      columns = c(pkg)
    )
  )


#### TO ------------------------------------------------------------------------

el <- 
  "
  `to_edgelist()`
  "

ma <- 
  "
  `to_matrix()`
  "

ig <- 
  "
  `to_igraph()`
  "

nw <- 
  "
  `to_network()`
  "

d_to <- data.frame(
  INPUT = c("edgelist", "matrix", "igraph", "network"),
  edgelist = rep(el, 4),
  matrix = rep(ma, 4),
  igraph = rep(ig, 4),
  network = rep(nw, 4)
)

###### to table ----------------------------------------------------------------
table_to <- d_to |> 
  gt::gt() |> 
  gt::tab_spanner(
    label = "OUTPUT",
    columns = c("edgelist", "matrix", "igraph", "network")
  ) |> 
  gt::fmt_markdown(    # toon code als rmarkdown layout
    columns = c(edgelist, matrix, igraph, network)
  ) |> 
  gt::tab_style(  # make row group labels bold + layout
    gt::cell_text(
      font = "helvetica",
      weight = "bold",
      size = "medium"
    ),
    locations = list(gt::cells_column_labels(),
                     gt::cells_column_spanners(),
                     gt::cells_body(columns = INPUT))
  ) |>
  gt::tab_style(
    style = gt::cell_borders(
      sides = "right", 
      color = "lightgray", 
      style = "solid", 
      weight = gt::px(2)
    ),
    locations = gt::cells_body(
      columns = INPUT
    )
  ) |> 
  gt::tab_header(
    title = "Convert between various formats",
    subtitle = gt::md("Using the consistent functions of **snafun**")
  ) |> 
  gt::tab_source_note(
    source_note = gt::md(
  "
  *Conversion includes bipartite graphs.*
  
  *Check the arguments for the various options.*
  "
    )
  )
  
  
  
  
  
  
  


#### MANIPULATE ----------------------------------------------------------------
df_manipulate <- rbind(
  ###### print the graph ----
  c(
    "Print the graph object",
    "snafun",
    '
    print(x)
    ',
    NA
  ),
  c(
    "Print the graph object",
    "igraph",
    '
    igraph::print.igraph(x)
    ',
    "often_works"
  ),
  c(
    "Print the graph object",
    "network",
    '
    network::print.network(x)
    ',
    "often_works"
  ),
  ###### check the graph object ----
  c(
    "Check characteristics of the graph object",
    "snafun",
    '
    has_edge_attributes(x)

    has_vertex_attributes(x)

    has_vertex_attribute(x, attrname)

    has_edge_attribute(x, attrname)

    has_vertexnames(x)

    has_loops(x)
    
    is_bipartite(x)
    
    is_connected(x, rule = c("weak", "strong"))
    
    is_directed(x)
    
    is_network(x)

    is_igraph(x)
    
    is_signed(x)
    
    is_weighted(x)
    ',
    NA
  ),
  c(
    "Check characteristics of the graph object",
    "igraph",
    '
    # various functions, including
    any_loop(g)
    any_multiple(g)
    is_bipartite(graph)
    is_connected(graph, mode = c("weak", "strong"))
    is_directed(graph)
    is_igraph(graph)
    is_named(graph)
    is_simple(graph)
    is_weighted(graph)
    ',
    NA
  ),
  c(
    "Check characteristics of the graph object",
    "network",
    '
    # various functions, including:
    network::has.loops(x)
    network::is.bipartite(x)
    network::is.directed(x)
    network::is.multiplex(x)
    network::is.network(x)
    sna::is.connected(g, connected = "strong")
    ',
    NA
  ),
  ###### access the vertices ----
  c(
    "Access the vertices and edges",
    "snafun",
    NA,
    NA
  ),
  c(
    "Access the vertices and edges",
    "igraph",
    '
    V(x)              # vertices
    E(x)              # edges
    ',
    NA
  ),
  c(
    "Access the vertices and edges",
    "network",
    NA,
    NA
  ),
  ###### access attrs ----
  c(
    "Extract vertex / edge / graph attributes",
    "snafun",
    '
    extract_vertex_attribute(x, name)
    extract_vertex_names(x)             # specific to access the vertex names attribute
    extract_edge_attribute(x, name)
    extract_graph_attribute(x, name)
    
    ',
    NA
  ),
  c(
    "Extract vertex / edge / graph attributes",
    "igraph",
    '
    vertex_attr(graph, name, index = V(graph))
    V(graph)$attributename
    
    edge_attr(graph, name, index = E(graph))
    E(graph)$attributename
    
    graph_attr(graph, name)
    
    ',
    NA
  ),
  c(
    "Extract vertex / edge / graph attributes",
    "network",
    '
    network::get.vertex.attribute(x, attrname)
    network::get.edge.attribute(x, attrname)
    network::get.network.attribute(x, attrname)
    ',
    "more_args"
  ),
  ###### extract edgeID ----
  c(
    "Extract the ID of an edge",
    "snafun",
    '
    extract_edge_id(object, ego, alter, edgelist)
    ',
    NA
  ),
  c(
    "Extract vertex / edge / graph attributes",
    "igraph",
    NA,
    NA
  ),
  c(
    "Extract vertex / edge / graph attributes",
    "network",
    NA,
    NA
  ),
  ###### extract vertex names ----
  c(
    "Extract vertex names",
    "snafun",
    '
    extract_vertex_names(x)
    ',
    NA
  ),
  c(
    "Extract vertex names",
    "igraph",
    '
    V(x)$name
    vertex_attr(x, name = "names")
    ',
    NA
  ),
  c(
    "Extract vertex names",
    "network",
    '
    network::get.vertex.attribute(x, "vertex.names")
    network::network.vertex.names(x)
    ',
    NA
  ),
  ###### set vertex names ----
  c(
    "set vertex names",
    "snafun",
    '
    add_vertex_names(x, value)
    ',
    NA
  ),
  c(
    "Set vertex names",
    "igraph",
    '
    igraph::V(x)$name <- value
    igraph::set_vertex_attr(x, name = "names")
    ',
    NA
  ),
  c(
    "set vertex names",
    "network",
    '
    network::set.vertex.attr(x, "vertex.names", value)
    network::network.vertex.names(x) <- value
    ',
    NA
  ),
  ###### list attrs ----
  c(
    "List vertex / edge / graph attributes",
    "snafun",
    '
    list_vertex_attribute(x, name)
    list_edge_attribute(x, name)
    list_graph_attribute(x, name)
    
    ',
    NA
  ),
  c(
    "List vertex / edge / graph attributes",
    "igraph",
    '
    vertex_attr_names(graph)
    edge_attr_names(graph)
    graph_attr_names(graph)
    
    ',
    NA
  ),
  c(
    "List vertex / edge / graph attributes",
    "network",
    '
    network::list.vertex.attributes(x)
    network::list.edge.attributes(x
    network::list.network.attributes(x)
    ',
    NA
  ),
  ###### extract all vertex attributes ----
  c(
    "Extract all vertex attributes into a data.frame",
    "snafun",
    '
    extract_all_vertex_attributes(g)
    ',
    NA
  ),
  c(
    "Extract all vertex attributes into a data.frame",
    "igraph",
    NA,
    NA
  ),
  c(
    "Extract all vertex attributes into a data.frame",
    "network",
    NA,
    NA
  ),
  ###### add attrs ----
  c(
    "Add vertex / edge / graph attributes",
    "snafun",
    '
    # very flexible and powerful functions to add attributes 
    # in several ways, with the same function
    add_edge_attributes(object, attr_name, value, edgelist, overwrite = FALSE)
    
    add_vertex_attributes(x, attr_name = NULL, value)
    add_graph_attribute(x, attr_name = NULL, value)
    ',
    NA
  ),
  c(
    "Add vertex / edge / graph attributes",
    "igraph",
    '
    igraph::set.edge.attribute(g, "name", value)
    igraph::E(g)$name <- value
    
    igraph::set.vertex.attribute(g, "name", value)
    igraph::V(g)$name <- value
    
    igraph::set.graph.attribute(g, "name", value)
    g$name <- value
    ',
    NA
  ),
  c(
    "Add vertex / edge / graph attributes",
    "network",
    '
    network::set.edge.attribute(g, "new_name", value, e = seq_along(g$mel))
    network::set.edge.value(g, "new_name", value, e = seq_along(g$mel))
    
    network::set.network.attribute(g, "new_name", value)

    network::set.vertex.attribute(g, "new_name", value, v = seq_len(network::network.size(g)))
    ',
    NA
  ),
  ###### remove attrs ----
  c(
    "Remove vertex / edge / graph attributes",
    "snafun",
    '
    remove_edge_attribute(x, attr_name)

    remove_vertex_attribute(x, attr_name)
    remove_vertex_names(x)   # specific for the vertex name attribute

    remove_graph_attribute(x, attr_name)
    ',
    NA
  ),
  c(
    "Remove vertex / edge / graph attributes",
    "igraph",
    '
    delete_edge_attr(graph, name)
    delete_vertex_attr(graph, name)
    delete_graph_attr(graph, name)
    ',
    NA
  ),
  c(
    "Remove vertex / edge / graph attributes",
    "network",
    '
    network::delete.edge.attribute(x, attrname, ...)
    network::delete.vertex.attribute(x, attrname, ...)
    network::delete.network.attribute(x, attrname, ...)
    ',
    NA
  ),
  ###### add edges/vertices ----
  c(
    "Add vertices or edges",
    "snafun",
    NA,
    NA
  ),
  c(
    "Add vertices or edges",
    "igraph",
    '
    add_vertices(graph, nv, ..., attr = list())
    add_edges(graph, edges, ..., attr = list())
    ',
    NA
  ),
  c(
    "Add vertices or edges",
    "network",
    '
    add.vertices(x, nv, vattr = NULL, last.mode = TRUE, ...)
    add.edge(x, tail, head, names.eval = NULL, vals.eval = NULL, edge.check = FALSE, ...)
    add.edges(x, tail, head, names.eval = NULL, vals.eval = NULL, ...)
    ',
    NA
  ),
  ###### remove edges/vertices ----
  c(
    "Remove vertices or edges",
    "snafun",
    '
    remove_vertices(x, vertices)
    ',
    NA
  ),
  c(
    "Remove vertices or edges",
    "igraph",
    '
    delete_vertices(graph, v)
    delete_edges(graph, edges)
    ',
    NA
  ),
  c(
    "Remove vertices or edges",
    "network",
    '
    network::delete.edges(x, eid)
    network::delete.vertices(x, vid)
    ',
    NA
  ),
  ###### contract vertices ----
  c(
    "Contract vertices into one",
    "snafun",
    '
    contract_vertices(g, vertices, method = c("min", "max", "union", "add"),
      attr = NULL, new_name = "set", out_as_igraph = TRUE)
    ',
    NA
  ),
  c(
    "Contract vertices into one",
    "igraph",
    NA,
    NA
  ),
  c(
    "Contract vertices into one",
    "network",
    NA,
    NA
  ),
  ###### subset ----
  c(
    "Extract a subset from the graph",
    "snafun",
    '
    # single function to do it all
    extract_subgraph(x, v_to_keep, e_to_keep)
    ',
    NA
  ),
  c(
    "Extract a subset from the graph",
    "igraph",
    "
    # subset based on vertices
    igraph::induced_subgraph(g, vids = theVerticesYouWantToKeep)

    # subset based on edges
    igraph::subgraph.edges(g, eids = theEdgesYouWantToKeep)
      ",
    NA
  ),
  c(
    "Extract a subset from the graph",
    "network",
    '
    # subset based on vertices
    network::get.inducedSubgraph(g, v = theVerticesYouWantToKeep)

    # subset based on edges
    network::get.inducedSubgraph(g, eid = theEdgesYouWantToKeep)
    ',
    NA
  ),
  ###### egonet ----
  c(
    "Extract egonets from the graph",
    "snafun",
    '
    extract_egonet(x, vertices = NULL, order = 1, type = c("all", "out", "in"))
    ',
    NA
  ),
  c(
    "Extract egonets from the graph",
    "igraph",
    '
    make_ego_graph(g, order = 1, nodes = V(graph), mode = c("all", "out", "in"), mindist = 0)
    ',
    NA
  ),
  c(
    "Extract egonets from the graph",
    "network",
    '
    ego.extract(dat, ego = NULL, neighborhood = c("combined", "in", "out"))
    ',
    NA
  ),  
  ###### clean up ----
  c(
    "Clean up the graph",
    "snafun",
    "
    find_isolates(x, names = TRUE, loops = FALSE)
    remove_isolates(x, loops = FALSE)
    remove_loops(x)
    ",
    NA
  ),
  c(
    "Clean up the graph",
    "igraph",
    "
    igraph::simplify(g)

    # for example
    igraph::simplify(g, remove.multiple = TRUE,
        remove.loops = TRUE, edge.attrib.comb = list(weight = 'max'))
    ",
    NA
  ),
  c(
    "Clean up the graph",
    "network",
    '
    sna::isolates(g, diag=FALSE)
    ',
    NA
  )
) |> 
  as.data.frame() |> 
  magrittr::set_colnames(c("topic", "pkg", "code", "note"))

###### MANIPULATE table ----
table_manipulate <- df_manipulate |> 
  basic_theme() |> 
  gt::tab_header(        # add table title
    title = "Manipulate the graph object",
  ) |> 
  gt::tab_footnote(
    footnote = "These functions have many additional arguments",
    locations = gt::cells_body(
      rows = note == "more_args",
      columns = c(pkg)
    )
  ) |> 
  gt::tab_footnote(
    footnote = "Often, but certainly not always, it will also work if you just use `print(x)`",
    locations = gt::cells_body(
      rows = note == "often_works",
      columns = c(pkg)
    )
  )







#### GRAPH ----------------------------------------------------------------
df_graph <- rbind(
  ###### summary ----
  c(
    "Summarize the network",
    "snafun",
    '
    g_summary(x, directed = TRUE)
    ',
    NA
  ),
  c(
    "Summarize the network",
    "igraph",
    NA,
    NA
  ),
  c(
    "Summarize the network",
    "network",
    NA,
    NA
  ),
  ###### number vertices ----
  c(
    "Count the number of vertices",
    "snafun",
    '
    count_vertices(x)
    ',
    NA
  ),
  c(
    "Count the number of vertices",
    "igraph",
    '
    vcount()
    ',
    NA
  ),
  c(
    "Count the number of vertices",
    "network",
    '
    network::network.size()
    
    ',
    NA
  ),
  ###### number edges ----
  c(
    "Count the number of edges",
    "snafun",
    '
    count_edges(x)
    ',
    NA
  ),
  c(
    "Count the number of edges",
    "igraph",
    '
    ecount(x)
    gsize(x)
    ',
    NA
  ),
  c(
    "Count the number of edges",
    "network",
    '
    network::network.edgecount(x)
    
    ',
    NA
  ),
  ###### density ----
  c(
    "Density",
    "snafun",
    '
    g_density(x, loops = FALSE)
    ',
    NA
  ),
  c(
    "Density",
    "igraph",
    "
    igraph::edge_density(g)
    ",
    NA
  ),
  c(
    "Density",
    "network",
    "
    # preferable for biprartite graphs
    network::network_density(g)

    # preferable for valued graphs
    sna::gden(g)
    ",
    NA
  ),
  ###### reciprocity ----
  c(
    "Reciprocity",
    "snafun",
    '
    g_reciprocity(x)
    ',
    NA
  ),
  c(
    "Reciprocity",
    "igraph",
    '
    igraph::reciprocity(g)
    ',
    NA
  ),
  c(
    "Reciprocity",
    "network",
    "
    sna::grecip(g,'measure = 'edgewise')
    ",
    NA
  ),
  ###### transitivity ----
  c(
    "Transitivity",
    "snafun",
    '
    g_transitivity(x)
    ',
    NA
  ),
  c(
    "Transitivity",
    "igraph",
    "
    igraph::transitivity(g, type = 'global')
    ",
    NA
  ),
  c(
    "Transitivity",
    "network",
    "
    sna::gtrans(g, mode = 'digraph', measure = 'weak', use.adjacency = TRUE)
    ",
    NA
  ),
  ###### mean distance ----
  c(
    "Mean distance between vertices",
    "snafun",
    "
    g_mean_distance(x)
    ",
    NA
  ),
  c(
    "Mean distance between vertices",
    "igraph",
    
    "
    igraph::mean_distance(g, directed = TRUE, unconnected = TRUE)
    ",
    NA
  ),
  c(
    "Mean distance between vertices",
    "network",
    NA,
    NA
  ),
  ###### degree distribution ----
  c(
    "Degree distribution",
    "snafun",
    '
    g_degree_distribution(x, mode = c("out", "in", "all"),
                                  type = c("density", "count"),
                                  cumulative = FALSE, 
                                  loops = FALSE,
                                  digits = 3)
    ',
    NA
  ),
  c(
    "Degree distribution",
    "igraph",
    "
    igraph::degree_distribution(graph, cumulative = FALSE, mode = 'out')
    ",
    NA
  ),
  c(
    "Degree distribution",
    "network",
    NA,
    NA
  ),
  ###### dyad census ----
  c(
    "Dyad census",
    "snafun",
    "
    count_dyads(x, echo = TRUE)
    ",
    NA
  ),
  c(
    "Dyad census",
    "igraph",
    
    "
    igraph::dyad_census(g)
    ",
    NA
  ),
  c(
    "Dyad census",
    "network",
    "
    sna::dyad.census(g)
    ",
    NA
  ),
  ###### triad census ----
  c(
    "Triad census",
    "snafun",
    "
    count_triads(x, echo = TRUE)
    ",
    NA
  ),
  c(
    "Triad census",
    "igraph",
    
    "
    igraph::triad_census(g)
    ",
    NA
  ),
  c(
    "Triad census",
    "network",
    "
    sna::triad.census(g, mode = 'digraph')
    ",
    NA
  ),
  ###### assortativity ----
  c(
    "Degree assortativity",
    "snafun",
    NA,
    NA
  ),
  c(
    "Degree assortativity",
    "igraph",
    "
    igraph::assortativity_degree(g, directed = TRUE)
    ",
    NA
  ),
  c(
    "Degree assortativity",
    "network",
    NA,
    NA
  ),
  ###### diameter ----
  c(
    "Diameter",
    "snafun",
    "
    g_diameter(x, directed = is_directed(x), unconnected = TRUE)
    ",
    NA
  ),
  c(
    "Diameter",
    "igraph",
    "
    igraph::diameter(g, directed = TRUE, unconnected = TRUE)

    # what is the vertex pair with the longest geodesic
    igraph::farthest_vertices(g)
    ",
    NA
  ),
  c(
    "Diameter",
    "network",
    NA,
    NA
  ),
  ###### radius ----
  c(
    "Radius",
    "snafun",
    '
    g_radius(x, mode = c("all", "out", "in"))
    ',
    NA
  ),
  c(
    "Radius",
    "igraph",
    '
    igraph::radius(graph, mode = c("all", "out", "in", "total"))
    ',
    NA
  ),
  c(
    "Radius",
    "network",
    NA,
    NA
  ),
  ###### compactness ----
  c(
    "Compactness",
    "snafun",
    '
    g_compactness(x, mode = c("out", "in", "all"))
    ',
    NA
  ),
  c(
    "Compactness",
    "igraph",
    NA,
    NA
  ),
  c(
    "Compactness",
    "network",
    NA,
    NA
  ),
  ###### centralization ----
  c(
    "Centralization",
    "snafun",
    '
    # a single function to calculate centralization (either `Freeman` or `sd`) 
    # on a wide range of centrality indices
    
    g_centralize(x, measure = "betweenness",
      directed = TRUE, mode = c("all", "out", "in"), 
      k = 3, damping = 0.85, normalized = TRUE, 
      method = c("freeman", "sd"))
    ',
    NA
  ),
  c(
    "Centralization",
    "igraph",
    "
    # general function for Freeman centralization
    igraph::centralize(scores, theoretical.max = 0, normalized = TRUE)
    
    # betweenness centralization
    igraph::centr_betw(g, directed = TRUE)$centralization

    # closeness centralization
    igraph::centr_clo(g, mode = 'out', normalized = FALSE)$centralization

    # degree centralization
    igraph::centr_degree(g, mode = 'all')

    # eigenvector centralization
    igraph::centr_eigen(g, directed = TRUE)$centralization
    ",
    "freeman"
  ),
  c(
    "Centralization",
    "network",
    "
    # general function for Freeman centralization, 
    # include any function that calculates vertex centrality
    sna::centralization(g, FUN, mode = 'digraph', normalize=TRUE, ...)
    ",
    "freeman"
  ),
  ###### mixing matrix ----
  c(
    "Mixing matrix",
    "snafun",
    '
    make_mixingmatrix(x, attrname, by_edge = FALSE, loops = has_loops(x))
    ',
    NA
  ),
  c(
    "Mixing matrix",
    "igraph",
    NA,
    NA
  ),
  c(
    "Mixing matrix",
    "network",
    '
    network::mixingmatrix(object, attrname, useNA = "ifany", expand.bipartite = FALSE)
    ',
    NA
  ),
  ###### correlation ----
  c(
    "Correlation between two graphs",
    "snafun",
    '
    g_correlation(g1, g2, diag = FALSE)
    ',
    NA
  ),
  c(
    "Correlation between two graphs",
    "igraph",
    NA,
    NA
  ),
  c(
    "Correlation between two graphs",
    "network",
    "
    sna::gcor(g1, g2, mode = 'graph')
    ",
    NA
  ),
  ###### Fast-greedy community detection ----
  c(
    "Fast-greedy community detection",
    "snafun",
    '
    extract_comm_fastgreedy(x, weights = NA, modularity = TRUE, 
      merges = TRUE, membership = TRUE)
    ',
    NA
  ),
  c(
    "Fast-greedy community detection",
    "igraph",
    '
    igraph::cluster_fast_greedy(g, merges = TRUE, modularity = TRUE,
      membership = TRUE, weights = NULL)
    ',
    NA
  ),
  c(
    "Fast-greedy community detection",
    "network",
    NA,
    NA
  ),
  ###### Girvan-Newman community detection ----
  c(
    "Girvan-Newman community detection",
    "snafun",
    '
    extract_comm_girvan(x, weights = NA, directed = TRUE, modularity = TRUE,
      edge.betweenness = FALSE, bridges = FALSE, merges = TRUE, membership = TRUE)
    ',
    NA
  ),
  c(
    "Girvan-Newman community detection",
    "igraph",
    '
    igraph::cluster_edge_betweenness(g, weights = NULL, directed = TRUE, edge.betweenness = TRUE,
      merges = TRUE, bridges = TRUE, modularity = TRUE, membership = TRUE)
    ',
    NA
  ),
  c(
    "Girvan-Newman community detection",
    "network",
    NA,
    NA
  ),
  ###### Louvain community detection ----
  c(
    "Louvain community detection",
    "snafun",
    '
    extract_comm_louvain(x, weights = NA, resolution = 1)
    ',
    NA
  ),
  c(
    "Louvain community detection",
    "igraph",
    '
    cluster_louvain(graph, weights = NULL, resolution = 1)
    ',
    NA
  ),
  c(
    "Louvain community detection",
    "network",
    NA,
    NA
  ),
  ###### Walktrap community detection ----
  c(
    "Walktrap community detection",
    "snafun",
    '
    extract_comm_walktrap(x, weights = NA, steps = 4, modularity = TRUE,
      merges = TRUE, membership = TRUE)
    ',
    NA
  ),
  c(
    "Walktrap community detection",
    "igraph",
    '
    cluster_walktrap(g, weights = NULL, steps = 4, merges = TRUE, modularity = TRUE, 
      membership = TRUE)
    ',
    NA
  ),
  c(
    "Walktrap community detection",
    "network",
    NA,
    NA
  ),
  ###### Merge community membership ----
  c(
    "Merge community membership",
    "snafun",
    '
    merge_membership(coms, merges)
    ',
    NA
  ),
  c(
    "Merge community membership",
    "igraph",
    NA,
    NA
  ),
  c(
    "Merge community membership",
    "network",
    NA,
    NA
  )
) |> 
  as.data.frame() |> 
  magrittr::set_colnames(c("topic", "pkg", "code", "note"))

###### GRAPH table ----
table_graph <- df_graph |> 
  basic_theme() |> 
  gt::tab_header(        # add table title
    title = "Explore the graph",
  ) |> 
  gt::tab_footnote(
    footnote = "igraph and sna only calculate `Freeman` centralization (snafun 
    does both `Freeman` and `sd`)",
    locations = gt::cells_body(
      rows = note == "freeman",
      columns = c(pkg)
    )
  ) |> 
  gt::tab_footnote(
    footnote = "`$res` or `$vector` return the centrality scores",
    locations = gt::cells_body(
      rows = note == "freeman" & pkg == "igraph",
      columns = c(pkg)
    )
  )










#### VERTICES ----------------------------------------------------------------
df_vertices <- rbind(
  ###### degree ----
  c(
    "Degree",
    "snafun",
    '
    v_degree(x, vids = NULL, mode = c("all", "out", "in"),
      loops = FALSE, rescaled = FALSE)
    ',
    NA
  ),
  c(
    "Degree",
    "igraph",
    "
    igraph::degree(g, mode = 'out')
    ",
    NA
  ),
  c(
    "Degree",
    "network",
    "
    sna::degree(g, gmode = 'digraph', cmode = 'outdegree')
      ",
    NA
  ),
  ###### betweenness ----
  c(
    "Betweenness",
    "snafun",
    '
    v_betweenness(x, vids = NULL, directed = TRUE, rescaled = FALSE)
    ',
    NA
  ),
  c(
    "Betweenness",
    "igraph",
    "
    igraph::betweenness(g, directed = TRUE)
    ",
    NA
  ),
  c(
    "Betweenness",
    "network",
    "
    sna::betweenness(g, gmode = 'digraph', cmode = 'directed')
    ",
    NA
  ),
  ###### flow betweenness ----
  c(
    "Flow betweenness",
    "snafun",
    NA,
    NA
  ),
  c(
    "Flow betweenness",
    "igraph",
    NA,
    NA
  ),
  c(
    "Flow betweenness",
    "network",
    "
    sna::flowbet(g, gmode = 'digraph', cmode = 'rawflow')
    ",
    NA
  ),
  ###### bonacich ----
  c(
    "Bonacich power centrality",
    "snafun",
    NA,
    NA
  ),
  c(
    "Bonacich power centrality",
    "igraph",
    "
    igraph::power_centrality(g)
    ",
    NA
  ),
  c(
    "Bonacich power centrality",
    "network",
    "
    sna::bonpow(g, gmode = 'digraph')
    ",
    NA
  ),
  ###### closeness ----
  c(
    "Closeness centrality",
    "snafun",
    '
    v_closeness(x, vids = NULL, mode = c("all", "out", "in"), rescaled = FALSE)
    ',
    NA
  ),
  c(
    "Closeness centrality",
    "igraph",
    "
    igraph::closeness(g, mode = 'all')
    ",
    NA
  ),
  c(
    "Closeness centrality",
    "network",
    "
    sna::closeness(g, gmode = 'digraph', cmode = 'directed')
    ",
    NA
  ),
  ###### harmonic ----
  c(
    "Harmonic centrality",
    "snafun",
    '
    v_harmonic(x, vids = NULL, mode = c("all", "out", "in"), rescaled = FALSE)
    ',
    NA
  ),
  c(
    "Harmonic centrality",
    "igraph",
    '
    harmonic_centrality(g, vids = V(graph), 
      mode = c("out", "in", "all"), weights = NULL)
    ',
    NA
  ),
  c(
    "Harmonic centrality",
    "network",
    NA,
    NA
  ),
  ###### stress ----
  c(
    "Stress centrality",
    "snafun",
    '
    v_stress(x, vids = NULL, directed = TRUE, rescaled = FALSE)
    ',
    NA
  ),
  c(
    "Stress centrality",
    "igraph",
    NA,
    NA
  ),
  c(
    "Stress centrality",
    "network",
    "
    sna::stresscent(g, gmode = 'digraph', cmode = 'directed')
    ",
    NA
  ),
  ###### eccentricity ----
  c(
    "Eccentricity",
    "snafun",
    '
    v_eccentricity(x, vids = NULL, mode = c("all", "out", "in"), rescaled = FALSE)
    ',
    NA
  ),
  c(
    "Eccentricity",
    "igraph",
    "
    igraph::eccentricity(g, mode = 'all')
    ",
    NA
  ),
  c(
    "Eccentricity",
    "network",
    NA,
    NA
  ),
  ###### eigenvector ----
  c(
    "Eigenvector",
    "snafun",
    '
    v_eigenvector(x, directed = TRUE, rescaled = FALSE)
    ',
    NA
  ),
  c(
    "Eigenvector",
    "igraph",
    "
    igraph::eigen_centrality(g, directed = TRUE, scale = FALSE)$vector
    ",
    NA
  ),
  c(
    "Eigenvector",
    "network",
    "
    sna::evcent(g, gmode = 'digraph', rescale=FALSE)
    ",
    NA
  ),
  ###### pagerank ----
  c(
    "Page rank",
    "snafun",
    '
    v_pagerank(x, vids = NULL, damping = 0.85, directed = TRUE, rescaled = FALSE)
    ',
    NA
  ),
  c(
    "Page rank",
    "igraph",
    '
    page_rank(g, vids = V(graph), directed = TRUE, damping = 0.85, weights = NULL)
    ',
    NA
  ),
  c(
    "Page rank",
    "network",
    NA,
    NA
  ),
  ###### geokpath ----
  c(
    "Geo k-path",
    "snafun",
    '
    # without using weights
    v_geokpath(x, vids = NULL, mode = c("all", "out", "in"), 
      k = 3, rescaled = FALSE)
    
    # if weights are to be used
    v_geokpath_w(x, vids = NULL, mode = c("all", "out", "in"),
      weights = NULL, k = 3)
    ',
    NA
  ),
  c(
    "Geo k-path",
    "igraph",
    NA,
    NA
  ),
  c(
    "Geo k-path",
    "network",
    NA,
    NA
  ),
  ###### shapley ----
  c(
    "Shapley centrality",
    "snafun",
    '
    v_shapley(x, add.vertex.names = FALSE, vids = NULL, rescaled = FALSE)
    ',
    NA
  ),
  c(
    "Shapley centrality",
    "igraph",
    NA,
    NA
  ),
  c(
    "Shapley centrality",
    "network",
    NA,
    NA
  ),
  
  ###### neighbors ----
  c(
    "Who are the neighbors of a vertex",
    "snafun",
    '
    extract_neighbors(x, vertex, type = c("out", "in", "all"))
    ',
    NA
  ),
  c(
    "Who are the neighbors of a vertex",
    "igraph",
    "
    igraph::neighbors(g, 'Jane', mode = 'out')

    # all options
    igraph::neighbors(graph, v, mode = c('out', 'in', 'all', 'total'))
    ",
    NA
  ),
  c(
    "Who are the neighbors of a vertex",
    "network",
    "
    network::get.neighborhood(g, 1, 'out')

    # all options
    network::get.neighborhood(x, v, type = c('out', 'in', 'combined'), na.omit = TRUE)
      ",
    NA
  ),
  ###### neighborhood ----
  c(
    "Neighborhood of a vertex",
    "snafun",
    NA,
    NA
  ),
  c(
    "Neighborhood of a vertex",
    "igraph",
    
    '
    igraph::make_ego_graph(g, order = 1, nodes = "Jane", mode = "all")

    # all options
    igraph::make_ego_graph(graph, order = 1, nodes = V(graph),
      mode = c("all", "out", "in"), mindist = 0)
      ',
    NA
  ),
  c(
    "Neighborhood of a vertex",
    "network",
    '
    sna::ego.extract(dat, ego = NULL, neighborhood = c("combined", "in", "out"))

    sna::neighborhood(dat, order, neighborhood.type = c("in", "out", "total"),
      mode = "digraph", diag = FALSE, thresh = 0, return.all = FALSE, partial = TRUE)
      ',
    NA
  )
) |> 
  as.data.frame() |> 
  magrittr::set_colnames(c("topic", "pkg", "code", "note"))

###### VERTICES table ----
table_vertices <- df_vertices |> 
  basic_theme() |> 
  gt::tab_header(        # add table title
    title = "Explore the vertices",
  # ) |> 
  # gt::tab_footnote(
  #   footnote = 'These functions serve equivalent purposes, but yield quite different kinds of outputs',
  #   locations = gt::cells_row_groups(
  #     rows = topic == "The neighborhood of a vertex",
  #     columns = c(pkg)
  #   )
  ) |> 
  gt::tab_footnote(
    footnote = 'These functions serve equivalent purposes, but yield quite different kinds of outputs',
    locations = gt::cells_row_groups(groups = "Neighborhood of a vertex")
  )







#### DYADS ----------------------------------------------------------------
df_dyads <- rbind(
  ###### shortest ----
  c(
    "shortest path for a given set of vertics",
    "snafun",
    NA,
    NA
  ),
  c(
    "shortest path for a given set of vertics",
    "igraph",
    "
    igraph::all_shortest_paths(g, from = IDofVertex, to = igraph::V(g), mode = 'out')
    ",
    NA
  ),
  c(
    "shortest path for a given set of vertics",
    "network",
    NA,
    NA
  ),
  ###### geodesic lengths ----
  c(
    "Geodesic lengths",
    "snafun",
    '
    d_distance(x, mode = c("all", "out", "in"))
    ',
    NA
  ),
  c(
    "Geodesic lengths",
    "igraph",
    "
    igraph::igraph::distances(g, mode = 'out')
    ",
    NA
  ),
  c(
    "Geodesic lengths",
    "network",
    "
    sna::geodist(g, count.paths = TRUE)$counts
    ",
    NA
  ),
  ###### structural equivalence ----
  c(
    "Structural equivalence",
    "snafun",
    '
    d_structural_equivalence(x, weights = NA, digits = 3, suppressWarnings = TRUE)
    ',
    NA
  ),
  c(
    "Structural equivalence",
    "igraph",
    NA,
    NA
  ),
  c(
    "Structural equivalence",
    "network",
    '
    sna::sedist(g, method = "correlation", mode ="digraph", diag=FALSE)
    ',
    NA
  ),
  ###### edge betweenness ----
  c(
    "Edge betweenness",
    "snafun",
    NA,
    NA
  ),
  c(
    "Edge betweenness",
    "igraph",
    "
    igraph::edge.betweenness(g, directed = FALSE)
    ",
    NA
  ),
  c(
    "Edge betweenness",
    "network",
    NA,
    NA
  )
) |> 
  as.data.frame() |> 
  magrittr::set_colnames(c("topic", "pkg", "code", "note"))

###### DYADS table ----
table_dyads <- df_dyads |> 
  basic_theme() |> 
  gt::tab_header(        # add table title
    title = "Explore the dyads",
    # ) |> 
    # gt::tab_footnote(
    #   footnote = 'These functions serve equivalent purposes, but yield quite different kinds of outputs',
    #   locations = gt::cells_row_groups(
    #     rows = topic == "The neighborhood of a vertex",
    #     columns = c(pkg)
    #   )
  ) |> 
  gt::tab_footnote(
    footnote = 'The output is a table with an entry per vertex pair',
    locations = gt::cells_row_groups(groups = "Geodesic lengths")
  )









#### STATS ---------------------------------------------------------------------
df_stats <- rbind(
c(
  "Randomly permute the order of the vertices",
  "snafun",
  NA,
  NA
),
c(
  "Randomly permute the order of the vertices",
  "igraph",
  "
  permute(graph = x, permutation = sample(igraph::vcount(x)))
  ",
  NA
),
c(
  "Randomly permute the order of the vertices",
  "network",
  "
  sna::rmperm(x)
  ",
  "The output is a matrix object"
)
) |> 
  as.data.frame() |> 
  magrittr::set_colnames(c("topic", "pkg", "code", "note"))

###### STATS table ----
table_stats <- df_stats |> 
  basic_theme() |> 
  gt::tab_header(        # add table title
    title = "Statistical analysis",
  )
  
  

#### STATISTICAL MODELS --------------------------------------------------------
df_models <- rbind(
  c("Dependent vertex attribute explained by a network weight matrix and a matrix of covariates",
    "Network autocorrelation model",
    "
    sna::lnam
    "),
  
  c("A statistic on a single network",
    "Conditional Uniform Graph test",
    "
    sna::cug.test
    "),
  
  c("Association between two networks",
    "QAP",
    "
    sna::qaptest
    "),
  c("A valued dependent network explained by one or more explanatory networks",
    "QAP linear model",
    "
    sna::netlm
    "),
  c("A binary dependent network explained by one or more explanatory networks",
    
    "QAP logistic model",
    "
    sna::netlogit
    "),
  c("A binary or valued dependent network explained by a set of endogenous and exogenous variables",
    "Exponential random graph models",
    "
    ergm::ergm
    "
  )
) |>
  as.data.frame() |>
  magrittr::set_colnames(c("When", "Which approach", "Function"))


table_models <-
  df_models |>
  gt::gt() |>
  gt::tab_options(
    column_labels.border.top.color = "white", # lijn boven kolomnamen
    column_labels.border.top.width = gt::px(3),
    column_labels.border.bottom.color = "grey",
    column_labels.font.weight = "bold",
    # table.border.bottom.color = "white",  # lijn onderaan
    table.border.bottom.width = gt::px(3),
    data_row.padding = gt::px(3)
  ) |>
  gt::tab_header(        # add table title
    title = "Statistical network models",
  ) |>
  gt::tab_style(         # font and size title
    style = gt::cell_text(
      font = gt::google_font("Titan One"),
      align = "center",
      size = "xx-large"
    ),
    locations = gt::cells_title("title")
  ) |>
  gt::fmt_markdown(    # toon code als rmarkdown layout
    columns = "Function"
  )




#### BTERGM TERMS --------------------------------------------------------------

df_btergm_terms <- rbind(
  c("memory",   ########### memory
    "Positive autoregression",
    
    "Previous existing edges persist in a next network",
    
    '
     btergm::memory(type = "autoregression", lag = 1)
     '
  ),
  c("memory",
    "Dyadic stability",
    
    "Both previous existing and non-existing ties are carried over to the current network",
    
    '
     btergm::memory(type = "stability", lag = 1)
     '
  ),
  c("memory",
    "Edge innovation",
    
    "A non-existing previous tie becomes existent in the current network",
    
    '
     btergm::memory(type = "innovation", lag = 1)
     '
  ),
  c("memory",
    "Edge loss",
    
    "An existing previous tie is dissolved in the current network",
    
    '
     btergm::memory(type = "loss", lag = 1)
     '
  ),
  c("delayed reciprocity",
    "reciprocity",
    
    "if node j is tied to node i at t = 1, does this lead to a reciprocation of that tie back from i to j at t = 2?",
    
    '
     btergm::delrecip(mutuality = FALSE, lag = 1)
     '
  ),
  c("delayed reciprocity",
    "mutuality",
    
    "if node j is tied to node i at t = 1, does this lead to a reciprocation of that tie back from i to j at t = 2 AND if i is not tied to j at t = 1, will this lead to j not being tied to i at t = 2? This captures a trend away from asymmetry.",
    
    '
     btergm::delrecip(mutuality = TRUE, lag = 1)
     '
  ),
  c("time covariates",
    "time effect per se",
    
    "Test for a specific trend (linear or non-linear) for edge formation",
    
    '
     btergm::timecov(transform = function(t) t)
     '
  ),
  c("time covariates",
    "Time effect of a covariate",
    
    "Interaction effect to test whether the importance of a covariate increases or decreases over time",
    
    '
     btergm::timecov(x, transform = function(t) t)
     '
  )
)  |>
  as.data.frame() |>
  magrittr::set_colnames(c("type", "what", "meaning", "btergm"))


table_btergm_terms <- df_btergm_terms |>
  gt::gt(
    groupname_col = "type",   # row groups
    rowname_col = "what"      # stub
  ) |>
  gt::tab_header(        # add table title
    title = "Temporal effects for the ERGM",
  ) |>
  gt::tab_options(
    column_labels.border.top.color = "white", # lijn boven kolomnamen
    column_labels.border.top.width = gt::px(3),
    column_labels.border.bottom.color = "white",
    column_labels.font.weight = "bold",
    table_body.hlines.style = "dotted",  # tussen de rijen binnen een groep
    # table.border.bottom.color = "white",  # lijn onderaan
    table.border.bottom.width = gt::px(3),
    data_row.padding = gt::px(3),
    table.width = gt::px(1200),
    table_body.border.top.color = "red"
  ) |>
  gt::cols_width(
    what ~ px(250),
    meaning ~ px(400)
  )  |>
  gt::tab_style(         # remove right border from stub
    gt::cell_borders(
      sides = "right",
      color = NULL),
    locations = gt::cells_stub()
  ) |>
  gt::tab_style(         # font and size title
    style = gt::cell_text(
      font = gt::google_font("Titan One"),
      align = "center",
      size = "xx-large"
    ),
    locations = gt::cells_title("title")
  ) |>
  gt::tab_style(     # font voor rijlabels
    style = gt::cell_text(
      font = gt::google_font("IBM Plex Sans"),
      align = "left",
      size = "large",
      weight = 'bold',
    ),
    locations = gt::cells_column_labels(gt::everything())
  ) |>
  gt::fmt_markdown(    # toon code als rmarkdown layout
    columns = btergm
  ) |>
  gt::tab_style(
    gt::cell_text(
      weight = "bold"
    ) ,
    locations = gt::cells_row_groups()
  )













