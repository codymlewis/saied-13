library(ggplot2)

# Plot the QRs of nodes over time
plot.nodes <- function(nodes) {
    ids = c()
    recommendations.number = c()
    QRs = c()
    malicious.state = c()
    for(node in nodes) {
        node.recommendations = length(node$QR)
        ids = c(ids, rep(node$id, node.recommendations))
        recommendations.number = c(recommendations.number, 1:node.recommendations)
        QRs = c(QRs, rev(node$QR))
        malicious.state = c(
            malicious.state, rep(`if`(is(node, "Node.BadMouther"), "Malicious", "Non-Malicious"), node.recommendations)
        )
    }
    data <- data.frame(ids=ids, recommendations=recommendations.number, QRs=QRs, malicious.state=malicious.state)
    ggplot(data=data, aes(x=recommendations, y=QRs, group=ids)) +
        geom_line(aes(colour=malicious.state)) +
        labs(
            title="Quality of Recommendations of Nodes",
            x="Number of Recommendations",
            y="Quality of Recommendations",
            colour=NULL
        ) +
        malicious_indicator() +
        y_limit() +
        theme(legend.position = "bottom")
}

# Plot all of the trust values
plot.trust <- function(nodes) {
    ids = c()
    transactions = c()
    trust = c()
    malicious.state = c()
    for(node in nodes) {
        number.transactions = length(node$trust)
        ids = c(ids, rep(node$id, number.transactions))
        transactions = c(transactions, 1:number.transactions)
        trust = c(trust, node$trust)
        malicious.state = c(
            malicious.state, rep(`if`(is(node, "Node.BadMouther"), "Malicious", "Non-Malicious"), number.transactions)
        )
    }
    data = data.frame(ids=ids, transactions=transactions, trust=trust, malicious.state=malicious.state)
    ggplot(data=data, aes(x=transactions, y=trust, group=ids)) +
        geom_point(aes(colour=malicious.state)) +
        malicious_indicator() +
        labs(
            title="Trust Values of the Node Services",
            x="Number of Transactions",
            y="Trust Value",
            colour = NULL
        ) +
        y_limit() +
        theme(legend.position = "bottom")
}

# Plot the final Quality of recommendations
plot.QRs.final <- function(nodes) {
    ids = c()
    QRs.final = c()
    malicious.state = c()
    for(node in nodes) {
        ids = c(ids, node$id)
        QRs.final = c(QRs.final, head(node$QR, 1))
        malicious.state = c(malicious.state, `if`(is(node, "Node.BadMouther"), "Malicious", "Non-Malicious"))
    }
    data = data.frame(ids=ids, QRs.final=QRs.final, malicious.state=malicious.state)
    ggplot(data=data, aes(x=ids, y=QRs.final)) +
        geom_point(aes(colour=malicious.state)) +
        malicious_indicator() +
        labs(
            title="Final Quality of Recommendations of the Nodes",
            x="Node ID",
            y="Final Quality of Recommendation",
            colour=NULL
        ) +
        y_limit() +
        theme(legend.position = "bottom")
}

# Colourise Malicious and non Malicious nodes for ggplot
malicious_indicator <- function() {
    return(scale_color_manual(breaks=c("Malicious", "Non-Malicious"), values=c("red", "blue")))
}

# Provide a limit on the y-axis for ggplot
y_limit <- function() {
    return(ylim(c(-1.1, 1.1)))
}

# Save a graph
graph.save <- function(filename) {
    dir.create("./graphs", showWarnings=FALSE)
    ggsave(file = sprintf("./graphs/%s", filename))
}
