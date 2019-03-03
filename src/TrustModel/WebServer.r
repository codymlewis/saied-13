#!/usr/bin/env Rscript
# Author: Cody Lewis
# Date: 2019-01-25
# The web server based GUI for the Trust model Simulator

library(shiny)

source("Attacks.r")
source("TrustManager.r")
source("TrustModel.r")

HEIGHT = "450px"
WIDTH = "250%"

network <- list()
nodemon_data <- matrix()

# The frontend user interface
ui <- fluidPage(
    titlePanel("Trust Model Simulator"),
    sidebarLayout(
        sidebarPanel(
            h3("Parameters", id="params"),
            sliderInput(
                inputId="theta",
                label="θ:",
                min=0,
                max=1,
                value=0.7
            ),
            sliderInput(
                inputId="lambda",
                label="λ:",
                min=0,
                max=1,
                value=0.7
            ),
            sliderInput(
                inputId="eta",
                label="η:",
                min=0,
                max=20,
                value=1
            ),
            sliderInput(
                inputId="total_nodes",
                label="Total Nodes:",
                min=10,
                max=500,
                value=200
            ),
            sliderInput(
                inputId="transactions",
                label="Transactions:",
                min=1,
                max=1000,
                value=300
            ),
            sliderInput(
                inputId="poor_witnesses",
                label="Poor Witnesses (%):",
                min=0,
                max=100,
                value=20
            ),
            sliderInput(
                inputId="constrained_nodes",
                label="Constrained Nodes (%):",
                min=0,
                max=100,
                value=50
            ),
            sliderInput(
                inputId="reputation_threshold",
                label="Reputation Threshold:",
                min=-2,
                max=1,
                value=-1,
                step=0.1
            ),
            sliderInput(
                inputId="malicious",
                label="Malicious Nodes (%):",
                min=0,
                max=100,
                value=10
            ),
            radioButtons(
                inputId="mouthing_type",
                label="Mouthing Type of Malicious Nodes:",
                choices=list(
                    "Bad Mouth"=BAD_MOUTH_FLAG,
                    "Good Mouth"=GOOD_MOUTH_FLAG,
                    "On-Off"=ON_OFF_FLAG
                ),
                selected=BAD_MOUTH_FLAG
            ),
            radioButtons(
                inputId="context_type",
                label="Context Attack Type of Malicious Nodes:",
                choices=list(
                    "None"=1,
                    "Service Set"=SERVICE_SET_FLAG,
                    "Capability Set"=CAPABILITY_SET_FLAG,
                    "Service Set + Capability Set"=SERVICE_SET_FLAG * CAPABILITY_SET_FLAG,
                    "Time Decay"=TIME_DECAY_FLAG,
                    "Service Set + Time Decay"=SERVICE_SET_FLAG * TIME_DECAY_FLAG,
                    "Capability Set + Time Decay"=CAPABILITY_SET_FLAG * TIME_DECAY_FLAG,
                    "Service Set + Capability Set + Time Decay"=SERVICE_SET_FLAG * CAPABILITY_SET_FLAG * TIME_DECAY_FLAG
                ),
                selected=1
            ),
            actionButton("submit", "Run Transactions",
                            class="btn btn-primary")
        ),
        mainPanel(
            id="main"
        )
    )
)

# The backend server functionality
server <- function(input, output) {
    observeEvent(input$submit, {
        time = 1
        REPUTATION_THRESHOLD <<- as.numeric(input$reputation_threshold)
        attack_type = as.numeric(input$mouthing_type) *
            as.numeric(input$context_type)
        network <<- create_network(
            input$total_nodes, input$malicious / 100,
            time, S_MAX, C_MAX, input$poor_witnesses / 100,
            input$constrained_nodes / 100
        )
        nodemon_data <<- create_nodemon_matrix(input$transactions)
        network$attack_type <<- assign_attack_types(
            network$attack_type, input$malicious / 100,
            input$total_nodes, attack_type
        )
        R = create_report_set(input$total_nodes)
        withProgress(message=sprintf("Performing %d transactions",
                                     input$transactions),
                     detail="This may take a while...", value=0, {
            for(i in 1:input$transactions) {
                R = initialize(
                    network, R, time, input$lambda,
                    input$theta, input$eta
                )
                if((i %% 30) == 0) {
                    time = time + 1
                }
                cs_targets = c(floor(runif(1, min=1, max=C_MAX)),
                               get_random_service())
                result = post_init(
                    network, input$lambda, input$theta,
                    input$eta, R, time, input$total_nodes, cs_targets
                )
                R = result[[1]]
                network <<- result[[2]]
                nodemon_data[i, ] <<- result[[3]]
                incProgress(1 / input$transactions)
            }
        })
        # Insert the UI for selecting an ploting nodes on first pass
        if(input$submit == 1) {
            insertUI(
                selector="#main",
                where="afterBegin",
                ui=h2("Plots of Node data", id="plot-heading")
            )
            insertUI(
                selector="#plot-heading",
                where="afterEnd",
                ui=numericInput(
                    "view_node_id", "View Node:", 1,
                    min=1, max=input$total_nodes
                )
            )
            insertUI(
                selector="#view_node_id",
                where="afterEnd",
                ui=plotOutput("node_data", width=WIDTH, height=HEIGHT)
            )
            insertUI(
                selector="#node_data",
                where="afterEnd",
                ui=plotOutput("reputations", width=WIDTH, height=HEIGHT)
            )
            insertUI(
                selector="#reputations",
                where="afterEnd",
                ui=plotOutput("final_qrs", width=WIDTH, height=HEIGHT)
            )
            insertUI(
                selector="#final_qrs",
                where="afterEnd",
                ui=plotOutput("final_trust", width=WIDTH, height=HEIGHT)
            )
            insertUI(
                selector="#final_trust",
                where="afterEnd",
                ui=plotOutput("nodemon", width=WIDTH, height=HEIGHT)
            )
        }
        output$node_data <- renderPlot({
            graph_single_node(network, input$view_node_id)
        })
        output$reputations <- renderPlot({
            graph_reputations(network)
        })
        output$final_qrs <- renderPlot({
            graph_final_qrs(network)
        })
        output$final_trust <- renderPlot({
            graph_final_trust(network)
        })
        output$nodemon <- renderPlot({
            graph_nodemon_data(nodemon_data, NODE_MON_ID,
                               network$malicious[[NODE_MON_ID]])
        })
    })
    output$node_data <- renderPlot({
        graph_single_node(network, input$view_node_id)
    })
}

options(shiny.port=8100)
shinyApp(ui=ui, server=server)
