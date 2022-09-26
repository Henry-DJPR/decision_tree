
# Packages
library(tidyverse)
library(shiny)
library(htmltools)
library(bslib)


# Generate decision tree
questions <- read_csv("data-raw/questions.csv") %>%
  as.data.frame()


# Progress bar fun
progress_bar <- function(pcent){
  div(
    class="progress",
    div(
      class = "progress-bar",
      role="progressbar",
      `aria-valuenow` = as.character(pcent),
      `aria-valuemin` = "0",
      `aria-valuemax` = "100",
      style = paste0("width: ", round(pcent), "%;")
    )
  )
}


# Turn questions into tab panels
panels <- tagList()

for(i in 1:nrow(questions)){
  category <- questions[i, "category"]
  question <- questions[i, "question"]
  progress <- i / nrow(questions) * 100
  category_short <- str_remove_all(category, "[:blank:]")

  panels <- panels %>%
    tagAppendChild(
      tabPanel(
        title = paste0("p", i),
        fluidRow(
          div(
            class = "col text-center p-3",
            div(tags$b(category)),
            p(
              style = "min-height: 120px;",
              question
            )
          )
        ),
        fluidRow(
          div(
            class = "col text-center",
            actionButton(
              "yes_btn",
              "Yes",
              width = "80%",
              class = "btn-secondary",
              onclick = paste0(
                'javascript:
                $("a[data-value=p', i + 1, ']").tab("show");',
                category_short, "_yes++;",
                "updateTotals();"
              )
            )
          ),
          div(
            class = "col text-center",
            actionButton(
              "no_btn",
              "No",
              width = "80%",
              class = "btn-secondary",
              onclick = paste0(
                'javascript:
                $("a[data-value=p', i + 1, ']").tab("show");',
                category_short, "_no++;",
                "updateTotals();"
              )
            )
          )
        ),
        tags$hr(),
        tags$small("Question", i, "of", nrow(questions)),
        progress_bar(progress)
      )
    )
}

# Final tab
panels <- panels %>%
  tagAppendChild(
    tabPanel(
      title = paste0("p", nrow(questions) + 1),
      div(
        class = "text-center p-3",
        tags$b("Results")
      ),
      div(
        class = "p-3",
        tags$table(
          class = "table",
          tags$thead(
            tags$th("Category"),
            tags$th("Yes"),
            tags$th("No")
          ),
          tags$tbody(
            map(
              unique(questions$category),
              function(category){
                category_short <- str_remove_all(category, "[:blank:]")
                tags$tr(
                  tags$th(category),
                  tags$td(span(id = paste0(category_short, "_yes"))),
                  tags$td(span(id = paste0(category_short, "_no")))
                )
              }
            )
          )
        )
      )
    )
  )



# Seat tabs in panel and tweak look
out <- tabsetPanel(!!!panels$children, type = "hidden") %>%
  tagAppendAttributes(
    class = "decisionTree"
  ) %>%
  tagAppendAttributes(
    style = "display: none;",
    .cssSelector = ".nav"
  )


# Put questions in UI containers

ui <- fluidPage(
  theme = bs_theme(version = 5),
  tags$head(
    tags$script(
      "
      var Biosecurity_yes = 0;
      var Biosecurity_no = 0;
      var Certificationsandproductattributes_yes = 0;
      var Certificationsandproductattributes_no = 0;
      var FoodSafety_yes = 0;
      var FoodSafety_no = 0;
      var MarketAccess_yes = 0;
      var MarketAccess_no = 0;
      var Provenance_yes = 0;
      var Provenance_no = 0;
      var Supplychainefficiencyandimprovement_yes = 0;
      var Supplychainefficiencyandimprovement_no = 0;
      function updateTotals(){
        $('#Biosecurity_yes').text(Math.round(Biosecurity_yes * 100 / (Biosecurity_yes + Biosecurity_no)) + '%');
        $('#Biosecurity_no').text(Math.round(Biosecurity_no * 100 / (Biosecurity_yes + Biosecurity_no)) + '%');
        $('#Certificationsandproductattributes_yes').text(Math.round(Certificationsandproductattributes_yes * 100 / (Certificationsandproductattributes_yes + Certificationsandproductattributes_no)) + '%');
        $('#Certificationsandproductattributes_no').text(Math.round(Certificationsandproductattributes_no * 100 / (Certificationsandproductattributes_yes + Certificationsandproductattributes_no)) + '%');
        $('#FoodSafety_yes').text(Math.round(FoodSafety_yes * 100 / (FoodSafety_yes + FoodSafety_no)) + '%');
        $('#FoodSafety_no').text(Math.round(FoodSafety_no * 100 / (FoodSafety_yes + FoodSafety_no)) + '%');
        $('#MarketAccess_yes').text(Math.round(MarketAccess_yes * 100 / (MarketAccess_yes + MarketAccess_no)) + '%');
        $('#MarketAccess_no').text(Math.round(MarketAccess_no * 100 / (MarketAccess_yes + MarketAccess_no)) + '%');
        $('#Provenance_yes').text(Math.round(Provenance_yes * 100 / (Provenance_yes + Provenance_no)) + '%');
        $('#Provenance_no').text(Math.round(Provenance_no * 100 / (Provenance_yes + Provenance_no)) + '%');
        $('#Supplychainefficiencyandimprovement_yes').text(Math.round(Supplychainefficiencyandimprovement_yes * 100 / (Supplychainefficiencyandimprovement_yes + Supplychainefficiencyandimprovement_no)) + '%');
        $('#Supplychainefficiencyandimprovement_no').text(Math.round(Supplychainefficiencyandimprovement_no * 100 / (Supplychainefficiencyandimprovement_yes + Supplychainefficiencyandimprovement_no)) + '%');
      }
      function clearTotals(){
        Biosecurity_yes = 0;
        Biosecurity_no = 0;
        Certificationsandproductattributes_yes = 0;
        Certificationsandproductattributes_no = 0;
        FoodSafety_yes = 0;
        FoodSafety_no = 0;
        MarketAccess_yes = 0;
        MarketAccess_no = 0;
        Provenance_yes = 0;
        Provenance_no = 0;
        Supplychainefficiencyandimprovement_yes = 0;
        Supplychainefficiencyandimprovement_no = 0;
      }
      "
    )
  ),
  fluidRow(
    div(
      class = "col m-4",
      style = "max-width: 600px;",
      div(
        class = "card",
        div(
          class = "card-header",
          span(class = "font-weight-bold", "Traceability Form Demo"),
          actionButton(
            inputId = "refreshTree",
            label = "",
            class = "float-end",
            icon = icon("rotate"),
            onclick = paste0(
              'javascript: $(".decisionTree .nav a").first().tab("show");',
              "clearTotals();"
            )
          )
        ),
        div(
          class = "card-body",
          out
        )
      )
    )
  )
)


shinyApp(ui = ui, server = function(...){})


