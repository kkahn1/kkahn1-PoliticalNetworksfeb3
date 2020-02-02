# These data are from the paper "Networks of Cooperation: Rebel Alliances in Fragmented Civil Wars" (Gade et al. 2019)
# The authors examine factional cooperation in the Syrian civil war. The data come from primary insurgent sources. 
# These include over 9,000 claims of attacks. The data were taken from Arabic and English newspapers, US government
# informational briefs, and thinktank reports. The authors then used text processing to find claims of attacks that included
# the words "joint," “collaboration,” “cooperation,” or “support.” The claims were all coded by hand to make sure that they really
# should be included.

# The authors end up with 220 active insurgent groups. They could not collect data on every rebel group involved in 
# Syria so they limited the analysis to medium-N. Including less prominent groups could have risked credibility of 
# the data; they only wanted to include sufficiently credbile information. This also meant that they assume prominent
# actors matter the most. They thus had 44 major groups and after tracking relationships they were left with 31 that 
# had collaborative relationships. 
set.seed(12345)

# Reading in adjacency matrix. This is a matrix of groups and whether they operate in the same area
loc <- read.csv("https://raw.githubusercontent.com/kkahn1/kkahn1-PoliticalNetworksfeb3/master/LocationDiff_CollabPaper_Oct2018_31Groups.csv")

# Reading in vertex level data. We will add some measures of centrality to it
vld <- read.csv("https://raw.githubusercontent.com/kkahn1/kkahn1-PoliticalNetworksfeb3/master/IdeologyVars_JPR%20copy.csv")
# The authors included more groups in their vertex level data than in their network, so we need to remove the extra groups
# to be able to save new variables to this dataframe
vld <- vld[-c(5, 14, 17, 19, 20, 21, 25, 32, 34, 35, 40, 42, 44),]

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

# Add vertex attributes to the network
network::set.vertex.attribute(locnet, attrname = names(vld), value = vld)

# Create a visual of the network.
plotlocnet <- plot(locnet, displaylabels = TRUE, vertex.cex= 2, label.cex = .7, edge.col= "gray", label.pos = 5, vertex.col = "purple")

#####################################

# The three best-known measures of centrality are degree, closeness and betweenness. We will start with
# degree-like measures. 

################### Degree-like measures ##########################
# These can be thought of as volume measures. How many paths stem from (or go to) a node?
# The more edges a node has, the more central the node is

# Degree centrality is the number of edges of length one that come from a given node.
centr_degree(locnet)
# We get an error because we created a graph object with "network" but this "igraph" function does not recognize
# it. We learned last week that we can use the package "intergraph" to fix this problem.
# First, we create a network object that igraph will be able read.
igr_locnet <- asIgraph(locnet)
# Now we can look at degree centrality.
centr_degree(igr_locnet)
# This function gives us both the node centrality scores and the graph level centralization. Centralization is 
# the difference between the largest centrality value and all other values. You can think of 1 as a star graph, 
# where one actor reaches all other actors by one path, and a score of 0 would be a circle graph

# Another function for degree centrality. Note that this time we are saving it as an object. This is because we 
# are going to use this object again to see a visual representation of degree centrality.
DC <- igraph::degree(igr_locnet)
DC
# This tells R that we want the size of our nodes to depend on degree centrality. You can change the
# denominator to make nodes larger or smaller.
V(igr_locnet)$size = DC/1.5
# We also can control the color of the vertices and edges (V for vertex, E for edges)
V(igr_locnet)$color <- "cyan"
E(igr_locnet)$color <- "thistle1"
# Now we can plot this
plot(igr_locnet) 
# Let's add labels so we know what belongs to what
plot(igr_locnet, vertex.label = get.vertex.attribute(igr_locnet, "GroupCode"))
# Let's make those labels a little smaller
plot(igr_locnet, vertex.label = get.vertex.attribute(igr_locnet,"GroupCode"), vertex.label.cex = .4)

# We can also add this to our vertex level dataset
vld <- data.frame(vld, DC)

# A geodesic path is the shortest path between two nodes. A geodesic k-path is the shortest path between two
# nodes less than k away. You can set k to anything. The reason I chose 1, which is the same as degree centrality
# is because this network is so dense that when k > 1, every node has 30 neighbors that are k or less paths away.
geokpath(igr_locnet, k = 1)
geokpath(igr_locnet, k = 2)
# Let's look at some of the arguments in a centrality function
?geokpath
# For example, setting the mode will let you choose all paths or only paths stemming from a node or only paths
# going to a node. This is only for directed networks.


# Eigenvector centrality: a measure of degree centrality where the importance of a node depends on the importance 
# of the nodes around it. High eigenvector centrality means more ties to the core of the network.  
eigen_centrality(igr_locnet, scale = FALSE, weights = NULL)
igraph::evcent(igr_locnet)
# We can also tell R we only want the vector of centrality scores
igraph:: evcent(igr_locnet)$vector
EC <- igraph:: evcent(igr_locnet)$vector
# Let's see this visually. Because the centrality scores for the nodes are so small this time, we will multiply 
# the size of the nodes by 15, but this is arbitrary and you can choose any amount
V(igr_locnet)$size <- EC*15 
# Let's change some colors in a different way than we did before and add some labels
plot(igr_locnet, vertex.color = "mediumturquoise", edge.color = "slategray2", 
     vertex.label = get.vertex.attribute(igr_locnet,"GroupCode"), vertex.label.cex = .7)

# Let's add this to our vertex level data as well
vld <- data.frame(vld, EC)


################### Closeness measures ##########################
# Closeness centrality is the total geodesic distance from one node to all other nodes. How many steps are required to get from one 
# node to every other node? Larger values mean there is less centrality, so you could think of "closeness" as measuring farness. 
# However, with the function you use, pay attention to whether the inverse is being taken.

# These two functions of closeness take the inverse, so higher values mean closer.
igraph::closeness(igr_locnet)
closeness.freeman(igr_locnet)
# We'll save this so that we can make a graph
CC <- closeness.freeman(igr_locnet)
# Let's see this visually
V(igr_locnet)$size <- CC*500
# Let's change some colors in a different way than we did before and add some labels
plot(igr_locnet, vertex.color = "seagreen2", edge.color = "gray", 
     vertex.label = get.vertex.attribute(igr_locnet,"GroupCode"), vertex.label.cex = .7)

# Add to vertex level data
vld <- data.frame(vld, CC)


# Information centrality builds on information theory which says that that information diffusion will not always 
# take the shortest paths, so Freeman's closeness is not best in these situations. Information centrality looks for
# linear combinations of paths with minimum variance. The variance of the best linear combination is the length, or
# the distance between two nodes. These are converted to nearness (as opposed to farness) and the closeness is 
# the harmonic mean of each row of the nearness matrix.
infocent(locnet, diag = FALSE, rescale = TRUE, gmode = "graph")
# gmode here is whether your path is directed or undirected. This defaults to "diagraph" in the sna package which 
# would calculate centrality as if it was a directed network.

# Centroid centrality involves finding the node (or nodes) that is (are) the centroid and then calculating the distance
# from a node to the centroid as centrality. 
centroid(igr_locnet)

################### Betweenness measures ##########################
# These are based on the number of walks that pass through a node, or medial measures. 

# Betweenness centrality is the amount of times a node needs another node to reach a third node.
# It finds the shortest paths and counts how many times a node is on those paths.
igraph::betweenness(igr_locnet, directed = FALSE)
sna::betweenness(locnet, gmode = "graph")
BC <- igraph::betweenness(igr_locnet, directed = FALSE)

# Create a visual
V(igr_locnet)$size <- BC*2
# Let's change some colors in a different way than we did before and add some labels
plot(igr_locnet, vertex.color = "mediumpurple", edge.color = "gray", 
     vertex.label = get.vertex.attribute(igr_locnet,"GroupCode"), vertex.label.cex = .7)

# Add to vertex level dataset
vld <- data.frame(vld, BC)

# If you wanted you could also get the centralization score
centralization.betweenness(igr_locnet)

# Flow betweenness only considers edge-disjoint paths. Many paths may share edges, so this avoids
# counting them numerous times.
flowbet(locnet, gmode = "graph")
# We can look at how this compares to the between centrality graph we just made
FBC <- flowbet(locnet, gmode = "graph")
V(igr_locnet)$size <- FBC/20
plot(igr_locnet, vertex.color = "mediumpurple", edge.color = "gray", 
     vertex.label = get.vertex.attribute(igr_locnet,"GroupCode"), vertex.label.cex = .7)

################ Comparing centrality #################
# Create a data frame of the centrality scores you want. For this we will look at degree, eigenvector, closeness, and betweenness
df <- data.frame(DC, EC, CC, BC)
# At the start we made a vector of group abbreviations. This puts them into the new matrix as row names
rownames(df) <- x
# Correlation matrix
cor_matrix <- cor(df)
cor(df)
# Scatterplot
pairs(~DC + EC + CC +BC, data = df)

# We could also compare a centrality measure to some vertex level data.
# Let's see if degree centrality correlates to group size
plot(x = vld$DC, y = vld$GroupSize)

##############################

# Finally, the package CINNA has a function where you can see what centrality measures are appropriate for your
# network.
proper_centralities(igr_locnet)
# It also has a function to calculate many centrality measures, but it takes a long time so I won't do it in class.
# calculate_centralities(igr_locnet)
# But you could create a vector of proper_centralities and instruct calculate_centralities to include specific ones
pr_cent <- proper_centralities(igr_locnet)
calculate_centralities(igr_locnet, include = pr_cent[13:16])
