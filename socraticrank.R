require('igraph')
require('dplyr')

mydata = read.csv("~/Work/socraticrank/mdl_forum_posts_scrubbed_snadata_question_info.csv")
mydata$isQuestion = mydata$message_q_mark == 1;

graphData = mydata[,c("userid_from", "userid_to", "isQuestion")]
colnames(graphData) = c("from", "to", "question")

# filter the data to only include replies that have questions
graphData = graphData[graphData$question, ]

# find all the unique interaction counts
graphData = graphData %>% group_by(from, to) %>% summarise(count=n())

# remove all replies to oneself
graphData[graphData$from == graphData$to,]$count = 0
graphData = graphData[graphData$count != 0,]

# building igraph 
mygraph = graph.data.frame(graphData)

# use page rank
pranks = page.rank(mygraph, weights=graphData$count)$vector

# normalize page rank as z-score
V(mygraph)$rank = (pranks - mean(pranks)) / sd(pranks) 

# get a subgraph of the top thought leaders
highRankGraph = induced.subgraph(mygraph, which(V(mygraph)$rank > 5))

# plot the graph
layout = layout.circle(highRankGraph)
plot(highRankGraph, 
     main="Top Question Elictors on Moodle at a University (z-scores of SocraticRank > 5)", 
     sub="Node Size = SocraticRank (PageRank); Edge Value = # of Reply Posts", 
     edge.label = E(highRankGraph)$count, 
     edge.arrow.size = .25, 
     edge.label.cex = .5, 
     vertex.size = 5 * (V(highRankGraph)$rank / max(V(highRankGraph)$rank)), 
     vertex.label = V(highRankGraph)$name, 
     vertex.label.cex = .5, 
     vertex.label.dist = .25, 
     layout=layout)



