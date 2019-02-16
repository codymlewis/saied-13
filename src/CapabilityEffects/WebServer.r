#!/usr/bin/env Rscript

library(shiny)

source("ReputationQR.r")

NORMAL_FLAG = 0
BAD_MOUTH_FLAG = 1
GOOD_MOUTH_FLAG = 2

ui <- fluidPage(
    titlePanel("Reputation Impacts in a Trust Model"),
    sidebarLayout(
        sidebarPanel(
            h3("Parameters", id="params"),
            sliderInput(
                inputId="s_target",
                label="Service Target:",
                min=1,
                max=100,
                value=50
            ),
            sliderInput(
                inputId="c_target",
                label="Capability Target:",
                min=1,
                max=100,
                value=50
            ),
            sliderInput(
                inputId="s_j",
                label="Reported Service of the Node:",
                min=1,
                max=100,
                value=50
            ),
            sliderInput(
                inputId="c_j",
                label="Reported Capability of the Node:",
                min=1,
                max=100,
                value=50
            ),
            radioButtons(
                inputId="reporter_attack_type",
                label="Note Taking Type of Reporting Nodes:",
                choices=list(
                    "Normal"=NORMAL_FLAG,
                    "Bad Mouth"=BAD_MOUTH_FLAG,
                    "Good Mouth"=GOOD_MOUTH_FLAG
                ),
                selected=NORMAL_FLAG
            ),
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
                inputId="transactions",
                label="Transactions:",
                min=1,
                max=1000,
                value=300
            )
        ),
        mainPanel(
            id="main",
            plotOutput("reputation")
        )
    )
)

server <- function(input, output) {
    output$reputation <- renderPlot({
        reputations = rep(1, each=(input$transactions + 1))
        node_qrs = c(1)
        node_qr_times = c(1)
        time = 1
        client_qrs = runif(input$transactions)
        if(input$reporter_attack_type == NORMAL_FLAG) {
            client_notes = take_notes(input$c_j, input$c_target, client_qrs)
        } else if(input$reporter_attack_type == BAD_MOUTH_FLAG) {
            client_notes = rep(-1, each=length(client_qrs))
        } else {
            client_notes = rep(1, each=length(client_qrs))
        }
        node_note = 1
        for(transaction in 1:input$transactions) {
            if((transaction %% 30) == 0) {
                time = time + 1
            }
            d = report_dist(
                input$c_j, input$s_j, input$c_target,
                input$s_target, input$eta, node_note,
                find_dist(S_MAX, input$s_target)**2,
                find_dist(C_MAX, input$c_target)**2,
                S_MAX, C_MAX
            )
            w = find_weight(input$lambda, input$theta, node_note,
                            time, d, time)
            qr = find_qr(
                w, client_notes[[transaction]], input$theta, time,
                node_note, client_qrs[[transaction]], node_qrs, node_qr_times
            )
            node_qrs = c(qr, node_qrs)
            node_qr_times = c(time, node_qr_times)
            reputations[[transaction + 1]] = calculate_reputation(
                input$theta, client_notes, client_qrs, transaction,
                time, node_qr_times
            )
        }
        plot_reputation(reputations)
    })
}

options(shiny.port=8100)
shinyApp(ui=ui, server=server)
