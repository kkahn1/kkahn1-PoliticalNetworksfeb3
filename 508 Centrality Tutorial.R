# These data are from the paper "Networks of Cooperation: Rebel Alliances in Fragmented Civil Wars" (Gade et al. 2019)
# The authors examine factional cooperation in the Syrian civil war. The data come from primary insurgent sources. 
# These include over 9,000 claims of attacks. The data were taken from Arabic and English newspapers, US government
# informational briefs, and thinktank reports. The authors then used text processing to find claims of attacks that included
# the words "joint", “collaboration,” “cooperation,” or “support.” The claims were all coded by hand to make sure that it really
# should be included.

# The authors end up with 220 active insurgent groups. They could not collect data on every rebel group involved in 
# Syria so they limited the analysis to medium-N. Including less prominent groups could have risked credibility of 
# the data; they only wanted to include sufficiently credbile information. This also meant that they assume prominent
# actors matter the most. They thus had 44 major groups and after tracking relationships they were left with 31 that 
# had collaborative relationships.

set.seed(12345)

# Reading in adjacency matrix
loc <- read.csv("https://raw.githubusercontent.com/kkahn1/kkahn1-PoliticalNetworksfeb3/master/LocationDiff_CollabPaper_Oct2018_31Groups.csv")

## Dealing with the data the same way that the authors did before creating the network
## This is to make sure the row names and column names stay as row names and column names
# Creating a vector of column X, which should be row names
x <- loc$X
# Getting rid of the first column (Column X which should be row names)
loc <- loc[,-1]
# Making column and row names the vector x
colnames(loc) <- x
rownames(loc) <- x
# Check dimensions
dim(loc)
# Convert to matrix
loc <- as.matrix(loc)
dimnames(loc)[[1]] <- x
dimnames(loc)[[2]] <- x

# Packages we will need
# install.packages("network")
# install.packages("igraph")
# install.packages("centiserve")
# install.packages("intergraph")
# install.packages("sna")
# install.packages("CINNA")
library(network)
library(igraph)
library(centiserve)
library(intergraph)
library(sna)
library(CINNA)

# Note: sna and igraph share some function names that we will be using here

######################

# There are many measures of centrality and these can be done using functions from different packages. 
# We will first create a network object the same way that we learned how to in last week's lecture.

# Create a network object from location dataset
locnet <- network(loc, matrix.type = "adjacency", directed = FALSE)
locnet

# Create a visual of the network.
plotlocnet <- plot(locnet, displaylabels = TRUE, vertex.cex= 2, label.cex = .7, edge.col= "gray", label.pos = 5, vertex.col = "purple")

#####################################

# The three best-known measures of centrality are degree, closeness and betweenness. We will start with
# degree-like measures. 

################### Degree-like measures ##########################
# These can be thought of as volume measures. How many paths stem from (or go to) a node?

# Degree centrality is the number of edges of length one that come from a given node. This appeared in the 
# Gray and Potter (2012) reading for today. 
centr_degree(locnet)
# Oh no! An error. We created a graph object with "network" but this "igraph" function does not recognize
# it. We learned last week that we can use the package "intergraph" to fix this problem.
# First, we create a network object that igraph will be able read.
igr_locnet <- asIgraph(locnet)
# Now we can look at degree centrality.
cent <- centr_degree(igr_locnet)

# A geodesic path is the shortest path between two nodes. A geodesic k-path is the shortest path between two
# nodes less than k away. You can set k to anything. The reason I chose 1, which is the same as degree centrality
# is because this network is so dense that when k > 1, every node has 30 neighbors that are k or less paths away.
geokpath(igr_locnet, k = 1)
geokpath(igr_locnet, k = 2)
# Note that you can use ? to see arguments in a function
?geokpath
# For example, setting the mode will let you choose all paths or only paths stemming from a node or only paths
# going to a node. This is only for directed networks.

# The next three are degree-like measures that are similar to each other.
# Katz's measure is a weighted measure of path centrality where longer 
# and more indirect paths count for little.
katzcent(igr_locnet, alpha = .03)
# Bonacich's measure is similar to Katz's. Bonacich realized part of Katz's equation could be negative. This has an
# option to rescale so that the centrality scores sum to 1.
power_centrality(igr_locnet, rescale = TRUE)
igraph::bonpow(igr_locnet, rescale = TRUE)
# Eigenvector centrality is kind of like Katz's measure of centrality but updated. This appeared in the Gray and
# Potter reading for today. The importance of a node depends on the nodes around it. High eigenvector centrality
# means more ties to the core of the network.  
eigen_centrality(igr_locnet, scale = FALSE, weights = NULL)

################### Closeness measures ##########################
# These can be thought of as length measures, whereas we thought of degree-measures as volume measures.

# Closeness centrality is the total geodesic distance from one node to all other nodes. Again, a geodesic path is 
# the shortest path between two nodes, so this measures how many steps required to get from one node to every other 
# node. Larger values mean there is less centrality, so you could think of "closeness" as measuring farness. 
igraph::closeness(igr_locnet)
closeness.freeman(igr_locnet)

# Information centrality builds on information theory which says that that information diffusion will not always 
# take the shortest paths, so Freeman's closeness is not best in these situations. Information centrality looks for
# linear combinations of paths with minimum variance. The variance of the best linear combination is the length, or
# the distance between two nodes.
infocent(locnet, diag = FALSE, rescale = TRUE, gmode = "graph")
# gmode here is whether your path is directed or undirected. This defaults to "diagraph" which would calculate
# centrality as if it was a directed network.

# Centroid centrality involves finding the node (or nodes) that is (are) the centroid and then calculating the distance
# from a node to the centroid as centrality. 
centroid(igr_locnet)


################### Betweenness measures ##########################
# These are based on the number of walks that pass through a node, or medial measures. 

# Betweenness centrality is the amount of times a node needs another node to reach a third node.
# Let's imagine node i uses node k to get to node j. The output shows k's (the middleman) share of the shortest
# path traffic from i to j.
igraph::betweenness(igr_locnet)
igraph::betweenness(igr_locnet, directed = FALSE)
sna::betweenness(locnet, gmode = "graph")
centralization.betweenness(igr_locnet)

# Flow betweenness only considers edge-disjoint paths. Many paths may share edges, so this avoids
# counting them numerous times.
flowbet(locnet, gmode = "graph")
flowbet(locnet, gmode = "graph", rescale = TRUE)


##############################

# Finally, the package CINNA has a function where you can see what centrality measures are appropriate for your
# network.
proper_centralities(igr_locnet)
# It also has a function to calculate many centrality measures, but it takes a long time so I won't do it in class.
# calculate_centralities(igr_locnet)
# However, you could create a vector of proper_centralities and instruct calculate_centralities to include specific ones
pr_cent <- proper_centralities(igr_locnet)
calculate_centralities(igr_locnet, include = pr_cent[15:17])
