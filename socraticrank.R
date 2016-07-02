require('igraph')
require('dplyr')

getPosts = function(filePath) {
  rawPosts = read.csv(filePath)
  rawPosts$isquestion = rawPosts$message_q_mark == 1;
  posts = rawPosts[,c("userid_from", "userid_to", "isquestion", "id")]
  colnames(posts) = c("fromUser", "toUser", "isQuestion", "postId")
  return(posts)
}

posts = getPosts("/Users/will.riley/socraticrank/mdl_forum_posts_scrubbed_snadata_question_info.csv");

# filter the data to only include replies that have questions
questionReplies = posts[posts$isQuestion, ]

# find all the unique question reply counts
questionReplyCounts = questionReplies %>% group_by(fromUser, toUser) %>% summarise(count=n())

# remove all replies to oneself
questionReplyCountsToOthers = questionReplyCounts[,];
questionReplyCountsToOthers[questionReplyCountsToOthers$fromUser == questionReplyCountsToOthers$toUser,]$count = 0
questionReplyCountsToOthers = questionReplyCountsToOthers[questionReplyCountsToOthers$count != 0,]

# build igraph of question replies
questionRepliesToOthersGraph = graph.data.frame(questionReplyCountsToOthers)

# use page rank to compute socratic ranks
socraticRanks = page.rank(questionRepliesToOthersGraph, weights=questionReplyCountsToOthers$count)$vector

# normalize socratic rank as z-score
getZScore = function(x) {
  return((x - mean(x)) / sd(x))
}
V(questionRepliesToOthersGraph)$socraticRank = getZScore(socraticRanks) 

# get a subgraph of the top elictors of questions based on the z-score of socraticRanks
# and plot the subgraph

getTopQuestionRepliesToOthersGraph = function(graph, minZScoreSocraticRank) {
  return(induced.subgraph(graph, which(V(graph)$socraticRank > minZScoreSocraticRank)))
}
  
plotSocraticRank = function(topRankQuestionRepliesToOthersGraph, minZScoreSocraticRank) {
  layout = layout.circle(topRankQuestionRepliesToOthersGraph)
  plot(topRankQuestionRepliesToOthersGraph, 
       main= paste("Top Question Elictors on Moodle at a University (z-scores of SocraticRank > ", minZScoreSocraticRank, ")"), 
       sub="Node Size = SocraticRank (PageRank); Edge Value = # of Reply Posts", 
       edge.label = E(topRankQuestionRepliesToOthersGraph)$count, 
       edge.arrow.size = .25, 
       edge.label.cex = .5, 
       vertex.size = 5 * (V(topRankQuestionRepliesToOthersGraph)$socraticRank / max(V(topRankQuestionRepliesToOthersGraph)$socraticRank)), 
       vertex.label = V(topRankQuestionRepliesToOthersGraph)$name, 
       vertex.label.cex = .5, 
       vertex.label.dist = .25, 
       layout=layout)
}

minZScoreSocraticRank = 10
topRankQuestionRepliesToOthersGraph = getTopQuestionRepliesToOthersGraph(questionRepliesToOthersGraph, minZScoreSocraticRank);
plotSocraticRank(topRankQuestionRepliesToOthersGraph, minZScoreSocraticRank)

# get the number of posts
postCount = length(unique(posts$postId))

# get the number of questions posted
questionPostCount = length(unique(posts[posts$isQuestion, ]$postId))

# get the number of people who posted anything
postersCount = length(unique(c(posts$fromUser, posts$toUser)))

# get the number of people who have posted replies that are questions to someone else
questionPostersToOthersCount = length(V(questionRepliesToOthersGraph))

# get the number of replies that are questions posted to someone else
questionRepliesToOthersCount = sum(V(questionRepliesToOthersGraph)$count)

# a user's socraticRank is moderately correlated with the number of people that they have replied to with a question
# est. cor = 0.6417773 conf int. 0.6341354 0.6492938 alpha 0.95
# receiversCountSocraticRankCor = cor.test(degree(questionRepliesToOthersGraph, mode="out"), V(questionRepliesToOthersGraph)$socraticRank)
# plot(degree(questionRepliesToOthersGraph, mode="out"), V(questionRepliesToOthersGraph)$socraticRank)

# a user's socraticRank is highly correlated with the number of people that have replied to him/her with a question
# est. cor = 0.9095568 conf int. 0.9073048 0.9117565 alpha 0.95
# repliersCountSocraticRankCor = cor.test(degree(questionRepliesToOthersGraph, mode="in"), V(questionRepliesToOthersGraph)$socraticRank)
# plot(degree(questionRepliesToOthersGraph, mode="in"), V(questionRepliesToOthersGraph)$socraticRank)

# a user's socraticRank is moderately correlated with the number of people he/she has replied to
# est. cor = 0.6246993 conf int. 0.6167782 0.6324940 alpha 0.95
# betweenessReceiversSocraticRankCor = cor.test(betweenness(questionRepliesToOthersGraph), V(questionRepliesToOthersGraph)$socraticRank)
# plot(betweenness(questionRepliesToOthersGraph), V(questionRepliesToOthersGraph)$socraticRank)

# a user's socroaticRank is moderately correlated with their betweenness in terms of how many questions they have received as replies
# est. cor = 0.6176655 conf int. 0.6096316 0.6255726 alpha 0.95
# betweenessRepliesReceivedSocraticRankCor = cor.test(betweenness(questionRepliesToOthersGraph, weights=E(questionRepliesToOthersGraph)$count), V(questionRepliesToOthersGraph)$socraticRank)
# plot(betweenness(questionRepliesToOthersGraph, weights=E(questionRepliesToOthersGraph)$count), V(questionRepliesToOthersGraph)$socraticRank)
