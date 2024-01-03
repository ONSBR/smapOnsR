#' Visualizador de calibracao
#' 
#' Ferramenta de visualizacao de calibracao do modelo smap/ons
#'
#' @importFrom shiny fluidPage titlePanel sidebarPanel fluidRow column h2 h3 numericInput 
#' mainPanel plotOutput textOutput actionButton dateRangeInput checkboxGroupInput brushOpts shinyApp
#' @importFrom shinyjs useShinyjs
#' @importFrom shiny observe renderPlot reactiveVal renderText updateActionButton updateNumericInput
#' @importFrom shinyjs toggleState enable disable
#' @importFrom future future value resolved
#' @importFrom DT renderDataTable dataTableOutput
#' @importFrom plotly renderPlotly plotlyOutput plot_ly
#' @importFrom moments skewness kurtosis
#' @export

executa_visualizador_calibracao <- function(){
    `%>%` <- magrittr::`%>%`
    
    ui_calibracao <- shiny::fluidPage(
        shinyjs::useShinyjs(),
        shiny::titlePanel("Calibracao SMAP/ONS"),
        shiny::tabsetPanel(
            shiny::tabPanel("Dados",
                shiny::fileInput(inputId = "arquivo_parametros", label = shiny::h3("Selecione o arquivo de parametros")),
                shiny::fileInput(inputId = "arquivo_vazao", label = shiny::h3("Selecione o arquivo de vazao")),
                shiny::fileInput(inputId = "arquivo_precipitacao", label = shiny::h3("Selecione o arquivo de precipitacao")),
                shiny::fluidRow(
                    shiny::column(3, 
                        shiny::fileInput(inputId = "arquivo_evapotranspiracao", label = shiny::h3("Selecione o arquivo de evapotranspiracao")),
                        shiny::fileInput(inputId = "arquivo_evapotranspiracao_nc", label = shiny::h3("Selecione o arquivo de NC de evapotranspiracao"))
                    )
                ),
                shiny::fileInput(inputId = "arquivo_postos_plu", label = shiny::h3("Selecione o arquivo de relacao postos plu x sub-bacias")),
                shiny::selectInput(inputId ="sub_bacia", label = shiny::h3("Selecione a sub-bacia a ser calibrada"), choices = NULL)
            ),
            shiny::tabPanel("Calibracao",
                shiny::sidebarLayout(
                    shiny::sidebarPanel(
                        shiny::fluidRow(
                            shiny::column(4, shiny::numericInput(inputId = "Ebin", label = "Ebin",value = NULL)),
                            shiny::column(3, shiny::numericInput(inputId = "Supin", label = "Supin",value = NULL)),
                            shiny::column(4, shiny::numericInput(inputId = "Tuin", label = "Tuin",value = NULL))
                        ),
                        shiny::hr(),
                        shiny::fluidRow(
                            shiny::column(4, shiny::h3("Limite Inferior"), shiny::uiOutput("li_psat")),
                            shiny::column(3, shiny::h3("Variaveis"), shiny::uiOutput("psats")),
                            shiny::column(4, shiny::h3("Limite Superior"), shiny::uiOutput("ls_psat"))
                        ),
                        shiny::hr(),
                        shiny::fluidRow(
                            shiny::column(4, 
                                shiny::numericInput(inputId = "limite_inferior_str", label = "LI str",value = NULL),
                                shiny::numericInput(inputId = "limite_inferior_k2t", label = "LI k2t",value = NULL),
                                shiny::numericInput(inputId = "limite_inferior_crec", label = "LI crec",value = NULL),
                                shiny::numericInput(inputId = "limite_inferior_capc", label = "LI capc",value = NULL),
                                shiny::numericInput(inputId = "limite_inferior_k_kt", label = "LI k_kt",value = NULL),
                                shiny::numericInput(inputId = "limite_inferior_h1", label = "LI h1",value = NULL),
                                shiny::numericInput(inputId = "limite_inferior_k2t2", label = "LI k2t2",value = NULL),
                                shiny::numericInput(inputId = "limite_inferior_ai", label = "LI ai",value = NULL),
                                shiny::numericInput(inputId = "limite_inferior_h", label = "LI h",value = NULL),
                                shiny::numericInput(inputId = "limite_inferior_k1t", label = "LI k1t",value = NULL),
                                shiny::numericInput(inputId = "limite_inferior_k3t", label = "LI k3t",value = NULL),
                                shiny::numericInput(inputId = "limite_inferior_pcof", label = "LI pcof",value = NULL),
                                shiny::numericInput(inputId = "limite_inferior_ecof", label = "LI ecof",value = NULL),
                                shiny::numericInput(inputId = "limite_inferior_ecof2", label = "LI ecof2",value = NULL),
                                shiny::numericInput(inputId = "limite_inferior_alfa", label = "LI alfa",value = NULL),
                                shiny::numericInput(inputId = "limite_inferior_beta", label = "LI beta",value = NULL),
                                shiny::numericInput(inputId = "kt_max", label = "kt max",value = NULL)
                            ),
                            shiny::column(3,
                                        shiny::numericInput(inputId = "str", label = "str",value = NULL),
                                        shiny::numericInput(inputId = "k2t", label = "k2t",value = NULL),
                                        shiny::numericInput(inputId = "crec", label = "crec",value = NULL),
                                        shiny::numericInput(inputId = "capc", label = "capc",value = NULL),
                                        shiny::numericInput(inputId = "k_kt", label = "k_kt",value = NULL),
                                        shiny::numericInput(inputId = "h1", label = "h1",value = NULL),
                                        shiny::numericInput(inputId = "k2t2", label = "k2t2",value = NULL),
                                        shiny::numericInput(inputId = "ai", label = "ai",value = NULL),
                                        shiny::numericInput(inputId = "h", label = "h",value = NULL),
                                        shiny::numericInput(inputId = "k1t", label = "k1t",value = NULL),
                                        shiny::numericInput(inputId = "k3t", label = "k3t",value = NULL),
                                        shiny::numericInput(inputId = "pcof", label = "pcof",value = NULL),
                                        shiny::numericInput(inputId = "ecof", label = "ecof",value = NULL),
                                        shiny::numericInput(inputId = "ecof2", label = "ecof2",value = NULL),
                                        shiny::numericInput(inputId = "alfa", label = "alfa",value = NULL),
                                        shiny::numericInput(inputId = "beta", label = "beta",value = NULL),
                                        shiny::numericInput(inputId = "kt_min", label = "kt min",value = NULL)
                            ),
                            shiny::column(4, 
                                        shiny::numericInput(inputId = "limite_superior_str", label = "LS str",value = NULL),
                                        shiny::numericInput(inputId = "limite_superior_k2t", label = "LS k2t",value = NULL),
                                        shiny::numericInput(inputId = "limite_superior_crec", label = "LS crec",value = NULL),
                                        shiny::numericInput(inputId = "limite_superior_capc", label = "LS capc",value = NULL),
                                        shiny::numericInput(inputId = "limite_superior_k_kt", label = "LS k_kt",value = NULL),
                                        shiny::numericInput(inputId = "limite_superior_h1", label = "LS h1",value = NULL),
                                        shiny::numericInput(inputId = "limite_superior_k2t2", label = "LS k2t2",value = NULL),
                                        shiny::numericInput(inputId = "limite_superior_ai", label = "LS ai",value = NULL),
                                        shiny::numericInput(inputId = "limite_superior_h", label = "LS h",value = NULL),
                                        shiny::numericInput(inputId = "limite_superior_k1t", label = "LS k1t",value = NULL),
                                        shiny::numericInput(inputId = "limite_superior_k3t", label = "LS k3t",value = NULL),
                                        shiny::numericInput(inputId = "limite_superior_pcof", label = "LS pcof",value = NULL),
                                        shiny::numericInput(inputId = "limite_superior_ecof", label = "LS ecof",value = NULL),
                                        shiny::numericInput(inputId = "limite_superior_ecof2", label = "LS ecof2",value = NULL),
                                        shiny::numericInput(inputId = "limite_superior_alfa", label = "LS alfa",value = NULL),
                                        shiny::numericInput(inputId = "limite_superior_beta", label = "LS beta",value = NULL)
                            ),
                        ),
                        shiny::hr(),
                        shiny::fluidRow(
                            shiny::dateRangeInput(inputId = "periodo_simulacao", label = "Periodo de simulacao", start = NULL, end = NULL, min = NULL, max = NULL),
                            shiny::dateRangeInput(inputId = "periodo_calibracao", label = "Periodo de calibracao", start = NULL, end = NULL, min = NULL, max = NULL),
                            shiny::numericInput(inputId = "numero_periodo_desconsiderado", label = "Periodos desconsiderados", value = 0, min = 0),
                            shiny::uiOutput("periodos_desconsiderados")
                        ),
                        shiny::hr(),
                        shiny::fluidRow(
                            shiny::downloadButton("download_parametros", "Download parametros_sub_bacia.csv"),
                            shiny::downloadButton("download_postos_plu", "Download postos_plu_sub_bacia.csv")
                        )
                    ),

                    shiny::mainPanel(
                        shiny::fluidRow(
                            shiny::dateRangeInput("zoom_calibracao", "Zoom calibracao", start = NULL, end = NULL, min = NULL, max = NULL),
                            dygraphs::dygraphOutput("dygraph_zoom", heigh = "600px"),
                            shiny::selectInput(inputId ="funcao_objetivo", label = shiny::h3("Selecione a funcao objetivo"), choices = c("dm", "nse", "mape", "kge"), selected = "dm"),
                            shiny::column(1, shiny::actionButton(inputId = "botao_calibracao", label = "Calibrar", class = "btn-lg btn-success")),
                            shiny::column(1, shiny::checkboxGroupInput("variaveis", "variaveis", choices = c("Qsup1", "Qsup2", "Qplan"))),
                            shiny::column(1, shiny::textOutput("funcao_objetivo")),
                            shiny::column(5, shiny::tableOutput("tabela_metrica1")),
                            shiny::column(3, shiny::tableOutput("tabela_metrica2")),
                            shiny::selectInput(inputId = "estatistica", label = shiny::h3("Estatisticas mensais"), choices = c("media", "dp", "assimetria", "curtose")),
                            plotly::plotlyOutput("metrica_mensal"),
                            plotly::plotlyOutput("grafico_kts")
                        )
                    )
                )
            ),
            shiny::tabPanel("Tabela Dados",
                DT::dataTableOutput("tabela")
            )
        )
    )

    servidor_calibracao <- function(input, output, session) {

        disable_button <- shiny::reactiveVal(FALSE)

        shiny::observeEvent(input$sub_bacia, {
            arquivo_parametros <- input$arquivo_parametros
            arquivo_postos_plu <- input$arquivo_postos_plu
            vazao <- vazao_posto()
            shiny::updateNumericInput(session, "Ebin", value = vazao$valor[1] * 0.3)
            shiny::updateNumericInput(session, "Supin", value = vazao$valor[1] * 0.7)
            shiny::updateNumericInput(session, "Tuin", value = 0.3)
            if (!is.null(arquivo_parametros) & !is.null(arquivo_postos_plu)) {
                vetor_modelo <- vetor_modelo()
                parametros_posto <- parametros_posto()
                postos_plu <- postos_plu()
                modelo <- new_modelo_smap_ons(parametros_posto, postos_plu[postos_plu$nome == input$sub_bacia])
                kt_max <- sum(vetor_modelo[1:2] > 0)
                kt_min <- sum(modelo$kt[4:63] > 0)
                shiny::updateNumericInput(session, "str", value = vetor_modelo[1])
                shiny::updateNumericInput(session, "k2t", value = vetor_modelo[2])
                shiny::updateNumericInput(session, "crec", value = vetor_modelo[3])
                shiny::updateNumericInput(session, "capc", value = vetor_modelo[4])
                shiny::updateNumericInput(session, "k_kt", value = vetor_modelo[5])
                shiny::updateNumericInput(session, "h1", value = vetor_modelo[6])
                shiny::updateNumericInput(session, "k2t2", value = vetor_modelo[7])
                shiny::updateNumericInput(session, "ai", value = vetor_modelo[8])
                shiny::updateNumericInput(session, "h", value = vetor_modelo[9])
                shiny::updateNumericInput(session, "k1t", value = vetor_modelo[10])
                shiny::updateNumericInput(session, "k3t", value = vetor_modelo[11])
                shiny::updateNumericInput(session, "pcof", value = vetor_modelo[12])
                shiny::updateNumericInput(session, "ecof", value = vetor_modelo[13])
                shiny::updateNumericInput(session, "ecof2", value = vetor_modelo[14])
                shiny::updateNumericInput(session, "alfa", value = vetor_modelo[15])
                shiny::updateNumericInput(session, "beta", value = vetor_modelo[16])
                shiny::updateNumericInput(session, "kt_max", value = kt_max)
                shiny::updateNumericInput(session, "kt_min", value = kt_min)

                shiny::updateNumericInput(session, "limite_inferior_str", value = vetor_modelo[1] * 0.5)
                shiny::updateNumericInput(session, "limite_inferior_k2t", value = vetor_modelo[2] * 0.5)
                shiny::updateNumericInput(session, "limite_inferior_crec", value = vetor_modelo[3] * 0.5)
                shiny::updateNumericInput(session, "limite_inferior_capc", value = vetor_modelo[4] * 0.5)
                shiny::updateNumericInput(session, "limite_inferior_k_kt", value = vetor_modelo[5] * 0.5)
                shiny::updateNumericInput(session, "limite_inferior_h1", value = vetor_modelo[6] * 0.5)
                shiny::updateNumericInput(session, "limite_inferior_k2t2", value = vetor_modelo[7] * 0.5)
                shiny::updateNumericInput(session, "limite_inferior_ai", value = vetor_modelo[8] * 0.5)
                shiny::updateNumericInput(session, "limite_inferior_h", value = vetor_modelo[9] * 0.5)
                shiny::updateNumericInput(session, "limite_inferior_k1t", value = vetor_modelo[10] * 0.5)
                shiny::updateNumericInput(session, "limite_inferior_k3t", value = vetor_modelo[11] * 0.5)
                shiny::updateNumericInput(session, "limite_inferior_pcof", value = 0.8)
                shiny::updateNumericInput(session, "limite_inferior_ecof", value = 0.8)
                shiny::updateNumericInput(session, "limite_inferior_ecof2", value = 0.8)
                shiny::updateNumericInput(session, "limite_inferior_alfa", value = 0.0000001)
                shiny::updateNumericInput(session, "limite_inferior_beta", value = 0.0000001)

                shiny::updateNumericInput(session, "limite_superior_str", value = vetor_modelo[1] * 2)
                shiny::updateNumericInput(session, "limite_superior_k2t", value = vetor_modelo[2] * 2)
                shiny::updateNumericInput(session, "limite_superior_crec", value = vetor_modelo[3] * 2)
                shiny::updateNumericInput(session, "limite_superior_capc", value = vetor_modelo[4] * 2)
                shiny::updateNumericInput(session, "limite_superior_k_kt", value = vetor_modelo[5] * 2)
                shiny::updateNumericInput(session, "limite_superior_h1", value = vetor_modelo[6] * 2)
                shiny::updateNumericInput(session, "limite_superior_k2t2", value = vetor_modelo[7] * 2)
                shiny::updateNumericInput(session, "limite_superior_ai", value = vetor_modelo[8] * 2)
                shiny::updateNumericInput(session, "limite_superior_h", value = vetor_modelo[9] * 2)
                shiny::updateNumericInput(session, "limite_superior_k1t", value = vetor_modelo[10] * 2)
                shiny::updateNumericInput(session, "limite_superior_k3t", value = vetor_modelo[11] * 2)
                shiny::updateNumericInput(session, "limite_superior_pcof", value = 1.2)
                shiny::updateNumericInput(session, "limite_superior_ecof", value = 1.2)
                shiny::updateNumericInput(session, "limite_superior_ecof2", value = 1.2)
                shiny::updateNumericInput(session, "limite_superior_alfa", value = 100)
                shiny::updateNumericInput(session, "limite_superior_beta", value = 100)
                precipitacao <- precipitacao_posto()
                data_minimo <- (min(precipitacao$data) + kt_min)
                data_maximo <- (max(precipitacao$data) - kt_max)
                shiny::updateDateRangeInput(session, "periodo_simulacao", start = data_minimo, end = data_maximo, min = data_minimo, max = data_maximo)
                shiny::updateDateRangeInput(session, "periodo_calibracao", start = data_minimo, end = data_maximo, min = data_minimo, max = data_maximo)
                shiny::updateDateRangeInput(session, "zoom_calibracao", start = data_minimo, end = data_maximo, min = data_minimo, max = data_maximo)
            }
        })

        shiny::observe({
            if (input$numero_periodo_desconsiderado >= 1) {
                input_periodos_desconsiderados <- lapply(1:input$numero_periodo_desconsiderado, function(i) {
                dateRangeInput(
                    inputId = paste0("periodo_desconsiderado_", i),
                    label = paste("Periodo desconsiderado ", i),
                    start = NULL, end = NULL
                )
                })
                output$periodos_desconsiderados <- shiny::renderUI(input_periodos_desconsiderados)
            } else {
                output$periodos_desconsiderados <- NULL
            }
        })

        shiny::observeEvent(input$periodo_simulacao, {
            data_minimo <- input$periodo_simulacao[1]
            data_maximo <- input$periodo_simulacao[2]
            shiny::updateDateRangeInput(session, "periodo_calibracao", start = data_minimo, end = data_maximo, min = data_minimo, max = data_maximo)
            shiny::updateDateRangeInput(session, "zoom_calibracao", start = data_minimo, end = data_maximo, min = data_minimo, max = data_maximo)
        })

        shiny::observeEvent(input$kt_min, {
            arquivo_parametros <- input$arquivo_parametros
            arquivo_precipitacao <- input$arquivo_precipitacao
            if (!is.null(arquivo_parametros) & !is.null(arquivo_precipitacao)) {
                precipitacao <- precipitacao_posto()
                kt_min <- input$kt_min
                kt_max <- input$kt_max
                data_minimo <- (min(precipitacao$data) + kt_min)
                data_maximo <- (max(precipitacao$data) - kt_max)
                shiny::updateDateRangeInput(session, "periodo_simulacao", start = data_minimo, end = data_maximo, min = data_minimo, max = data_maximo)
                shiny::updateDateRangeInput(session, "zoom_calibracao", start = data_minimo, end = data_maximo, min = data_minimo, max = data_maximo)
            }
        })

        shiny::observeEvent(input$kt_max, {
            arquivo_parametros <- input$arquivo_parametros
            arquivo_precipitacao <- input$arquivo_precipitacao
            if (!is.null(arquivo_parametros) & !is.null(arquivo_precipitacao)) {
                precipitacao <- precipitacao_posto()
                kt_min <- input$kt_min
                kt_max <- input$kt_max
                data_minimo <- (min(precipitacao$data) + kt_min)
                data_maximo <- (max(precipitacao$data) - kt_max)
                shiny::updateDateRangeInput(session, "periodo_simulacao", start = data_minimo, end = data_maximo, min = data_minimo, max = data_maximo)
                shiny::updateDateRangeInput(session, "zoom_calibracao", start = data_minimo, end = data_maximo, min = data_minimo, max = data_maximo)
            }
        })
        
        parametros <- shiny::reactive({
            arquivo_parametros <- input$arquivo_parametros$datapath
            if (!is.null(arquivo_parametros)) {
                parametros <- le_parametros(arquivo_parametros)
                return(parametros)
            }
        })

        area <-  shiny::reactive({
            arquivo_parametros <- input$arquivo_parametros$datapath
            arquivo_postos_plu <- input$arquivo_postos_plu$datapath
            if (!is.null(arquivo_parametros) & !is.null(arquivo_postos_plu)) {
                parametros_posto <- parametros_posto()
                area <- parametros_posto$valor[parametros_posto$parametro == "Area"]
            }
        })

        parametros_posto <- shiny::reactive({
            arquivo_parametros <- input$arquivo_parametros$datapath
            if (!is.null(arquivo_parametros)) {
                parametros <- parametros()
                return(parametros[parametros$nome == input$sub_bacia])
            }
        })

        vetor_modelo <- shiny::reactive({
            arquivo_parametros <- input$arquivo_parametros$datapath
            arquivo_postos_plu <- input$arquivo_postos_plu$datapath
            if (!is.null(arquivo_parametros) & !is.null(arquivo_postos_plu)) {
                postos_plu <- postos_plu()
                parametros_posto <- parametros_posto()
                modelo <- new_modelo_smap_ons(parametros_posto, postos_plu[postos_plu$nome == input$sub_bacia])
                vetor_modelo <- unlist(modelo)
                if (any(parametros_posto$parametro == "Alfa")){
                    alfa <- parametros_posto[parametros_posto$parametro == "Alfa"]$valor
                } else{
                    alfa <- 5
                }
                if (any(parametros_posto$parametro == "Beta")){
                    beta <- parametros_posto[parametros_posto$parametro == "Beta"]$valor
                } else{
                    beta <- 5
                }
                vetor_modelo <- as.numeric(c(vetor_modelo[1:11], vetor_modelo[75:77], alfa, beta))
                return(vetor_modelo)
            }
        })

        shiny::observeEvent(input$arquivo_vazao, {
            arquivo_vazao <- input$arquivo_vazao$datapath
            if (!is.null(arquivo_vazao)) {
                historico_vazao <- le_historico_verificado(arquivo_vazao)
                sub_bacias <- as.vector(unique(historico_vazao$posto))
                aux <- sub_bacias
                sub_bacias[1] <- "    "
                sub_bacias[2:(length(sub_bacias) + 1)] <- aux
                shiny::updateSelectInput(session, "sub_bacia", choices = sub_bacias)
            }
        })

        vazao_observada <- shiny::reactive({
            arquivo_vazao <- input$arquivo_vazao$datapath
            if (!is.null(arquivo_vazao)) {
                vazao <- le_historico_verificado(arquivo_vazao)
                return(vazao)
            }
        })

        vazao_posto <- shiny::reactive({
            vazao <- vazao_observada()
            return(vazao[vazao$posto == input$sub_bacia])
        })

        postos_plu <- shiny::reactive({
            arquivo_postos_plu <- input$arquivo_postos_plu$datapath
            if (!is.null(arquivo_postos_plu)) {
                postos_plu <- le_postos_plu(arquivo_postos_plu)
                return(postos_plu)
            }
        })

        gera_entrada_psat <- function(num_inputs) {
            inputs <- vector("list", num_inputs)
            postos_plu <- postos_plu()
            postos_plu <- postos_plu[postos_plu$nome %in% input$sub_bacia]
            if (num_inputs > 1){
                for (i in seq_len(num_inputs)) {
                inputs[[i]] <- shiny::numericInput(inputId = paste0("posto_plu_", i),
                                            label = postos_plu$posto[i],
                                            value = postos_plu$valor[i],
                                            min = 0,
                                            max = 1)
                }
            } else {
                inputs[[1]] <- shiny::numericInput(inputId = paste0("posto_plu_", 1),
                                            label = postos_plu$posto[1],
                                            value = postos_plu$valor[1],
                                            min = 1,
                                            max = 1)
            }
            return(inputs)
        }

        gera_limite_inferior_psat <- function(num_inputs) {
            inputs <- vector("list", num_inputs)
            postos_plu <- postos_plu()
            postos_plu <- postos_plu[postos_plu$nome %in% input$sub_bacia]
            if (num_inputs > 1) {
            for (i in seq_len(num_inputs)) {
                inputs[[i]] <- shiny::numericInput(inputId = paste0("limite_inferior_posto_plu_", i),
                                            label = paste0("LI ",postos_plu$posto[i]),
                                            value = 0,
                                            min = 0,
                                            max = 1)
                }
            } else {
                inputs[[1]] <- shiny::numericInput(inputId = paste0("limite_inferior_posto_plu_", 1),
                                            label = paste0("LI ",postos_plu$posto[1]),
                                            value = 1,
                                            min = 1,
                                            max = 1)
            }
            return(inputs)
        }

        gera_limite_superior_psat <- function(num_inputs) {
            inputs <- vector("list", num_inputs)
            postos_plu <- postos_plu()
            postos_plu <- postos_plu[postos_plu$nome %in% input$sub_bacia]
            if (num_inputs > 1) {
                for (i in seq_len(num_inputs)) {
                    inputs[[i]] <- shiny::numericInput(inputId = paste0("limite_superior_posto_plu_", i),
                                            label = paste0("LS ",postos_plu$posto[i]),
                                            value = 1,
                                            min = 0,
                                            max = 1)
                }
            } else {
                inputs[[1]] <- shiny::numericInput(inputId = paste0("limite_inferior_posto_plu_", 1),
                                            label = paste0("LS ",postos_plu$posto[1]),
                                            value = 1,
                                            min = 1,
                                            max = 1)
            }
            return(inputs)
        }

        shiny::observeEvent(input$sub_bacia, {
            arquivo_postos_plu <- input$arquivo_postos_plu$datapath
            arquivo_vazao <- input$arquivo_vazao$datapath
            if ((!is.null(arquivo_postos_plu)) & (!is.null(arquivo_vazao))){
                postos_plu <- postos_plu()
                numero_postos_plu <- nrow(postos_plu[postos_plu$nome %in% input$sub_bacia])
                psats <- gera_entrada_psat(numero_postos_plu)
                li_psat <- gera_limite_inferior_psat(numero_postos_plu)
                ls_psat <- gera_limite_superior_psat(numero_postos_plu)
                output$psats <- shiny::renderUI({
                    psats
                })
                output$li_psat <- shiny::renderUI({
                    li_psat
                })
                output$ls_psat <- shiny::renderUI({
                    ls_psat
                })
            }
        })

        precipitacao_observada <- shiny::reactive({
            arquivo_precipitacao <- input$arquivo_precipitacao$datapath
            if (!is.null(arquivo_precipitacao)) {
                precipitacao <- le_historico_verificado(arquivo_precipitacao)
                return(precipitacao)
            }
        })

        precipitacao_posto <- shiny::reactive({
            arquivo_precipitacao <- input$arquivo_precipitacao$datapath
            if (!is.null(arquivo_precipitacao)) {
                precipitacao <- precipitacao_observada()
                postos_plu <- postos_plu()
                return(precipitacao[precipitacao$posto %in% postos_plu$posto[postos_plu$nome == input$sub_bacia]])
            }
        })

        shinyjs::enable("arquivo_evapotranspiracao")
        shinyjs::enable("arquivo_evapotranspiracao_nc")

        shiny::observeEvent(input$arquivo_evapotranspiracao, {
            shinyjs::toggleState("arquivo_evapotranspiracao_nc", condition = is.null(input$arquivo_evapotranspiracao))
        })

        shiny::observeEvent(input$arquivo_evapotranspiracao_nc, {
            shinyjs::toggleState("arquivo_evapotranspiracao", condition = is.null(input$arquivo_evapotranspiracao_nc))
        })

        evapotranspiracao_observada <- shiny::reactive({
            arquivo_evapotranspiracao <- input$arquivo_evapotranspiracao$datapath
            arquivo_evapotranspiracao_nc <- input$arquivo_evapotranspiracao_nc$datapath
            if (!is.null(arquivo_evapotranspiracao)) {
                evapotranspiracao <- le_historico_verificado(arquivo_evapotranspiracao)
                shinyjs::disable("arquivo_evapotranspiracao_nc")
                return(evapotranspiracao)
            }
            if (!is.null(arquivo_evapotranspiracao_nc)) {
                evapotranspiracao <- le_evapotranspiracao_nc(arquivo_evapotranspiracao_nc)
                shinyjs::disable("arquivo_evapotranspiracao")
                
                return(evapotranspiracao)
            }
        })

        evapotranspiracao_posto <- shiny::reactive({
            evapotranspiracao <- evapotranspiracao_observada()
            colnames(evapotranspiracao)[2] <- "nome"
            evapotranspiracao <- evapotranspiracao[evapotranspiracao$nome == input$sub_bacia]
            arquivo_evapotranspiracao_nc <- input$arquivo_evapotranspiracao_nc$datapath
            if (!is.null(arquivo_evapotranspiracao_nc)) {
                precipitacao <- precipitacao_posto()
                postos_plu <- postos_plu()
                precipitacao <- ponderacao_espacial(precipitacao, postos_plu[postos_plu$nome == input$sub_bacia])
                evapotranspiracao <- transforma_NC_serie(precipitacao, evapotranspiracao)
            }
            return(evapotranspiracao)
        })


        # Enable/disable the run button
        shiny::observe({
            shinyjs::toggleState(id = "botao_calibracao", condition = !disable_button())
        })

        output$grafico_kts <- plotly::renderPlotly({
            vetor_modelo <- vetor_modelo()
            vetor_modelo[15] <- input$alfa
            vetor_modelo[16] <- input$beta
            kt_max <- input$kt_max
            kt_min <- input$kt_min
            
            kt <- cria_kt(kt_max, kt_min, vetor_modelo[15], vetor_modelo[16])
            kt <- data.table::data.table(kt)
            kt$lag <- 2:-60

            plot <- plotly::plot_ly(data = kt[which(lag %in% kt_max:-kt_min)], x = ~lag, y = ~kt, name = 'Distribuicao dos Kts', type = 'scatter', mode = 'lines', height = 4, width = 4) 
        })

        saida <-  shiny::reactive({
            if (!is.null(input[[paste0("posto_plu_1")]])) {
                vetor_modelo <- vetor_modelo()
                vetor_modelo[1] <- input$str
                vetor_modelo[2] <- input$k2t
                vetor_modelo[3] <- input$crec
                vetor_modelo[4] <- input$capc
                vetor_modelo[5] <- input$k_kt
                vetor_modelo[6] <- input$h1
                vetor_modelo[7] <- input$k2t2
                vetor_modelo[8] <- input$ai
                vetor_modelo[9] <- input$h
                vetor_modelo[10] <- input$k1t
                vetor_modelo[11] <- input$k3t
                vetor_modelo[12] <- input$pcof
                vetor_modelo[13] <- input$ecof
                vetor_modelo[14] <- input$ecof2
                vetor_modelo[15] <- input$alfa
                vetor_modelo[16] <- input$beta
                kt_max <- input$kt_max
                kt_min <- input$kt_min
                vazao <- vazao_posto()
                evapotranspiracao <- evapotranspiracao_posto()
                precipitacao <- precipitacao_posto()
                Ebin <- input$Ebin
                Tuin <- input$Tuin
                Supin <- input$Supin
                area <- area()
                postos_plu <- postos_plu()
                data_inicio_simulacao <- input$periodo_simulacao[1]
                data_fim_simulacao <- input$periodo_simulacao[2]
                data_inicio_simulacao <- data_inicio_simulacao - kt_min
                data_fim_simulacao <- data_fim_simulacao + kt_max

                precipitacao <- precipitacao[data >= data_inicio_simulacao & data <= data_fim_simulacao]
                evapotranspiracao <- evapotranspiracao[data >= data_inicio_simulacao + kt_min & data <= data_fim_simulacao - kt_max]
                
                kt <- cria_kt(kt_max, kt_min, vetor_modelo[15], vetor_modelo[16])
                
                inicializacao <- inicializacao_smap(vetor_modelo, area, Ebin, Tuin, Supin)
                
                numero_postos_plu <- nrow(postos_plu[postos_plu$nome == input$sub_bacia])
                if (numero_postos_plu > 1) {
                    for (iposto in 1: numero_postos_plu){
                        vetor_modelo[(16 + iposto)] <- input[[paste0("posto_plu_", iposto)]]
                        postos_plu[postos_plu$nome == input$sub_bacia]$valor[iposto] <- input[[paste0("posto_plu_", iposto)]]
                    }
                }

                precipitacao_ponderada <- ponderacao_espacial(precipitacao, postos_plu[postos_plu$nome == input$sub_bacia])

                precipitacao_ponderada <- precipitacao_ponderada$valor * vetor_modelo[12]
                precipitacao_ponderada <- ponderacao_temporal(precipitacao_ponderada, kt, kt_max, kt_min)
                
                data_inicio_simulacao <- data_inicio_simulacao + kt_min
                data_fim_simulacao <- data_fim_simulacao - kt_max
                evapotranspiracao_ponderada <- data.table::data.table(evapotranspiracao[which((evapotranspiracao$data >= data_inicio_simulacao)
                                            & (evapotranspiracao$data <= data_fim_simulacao))])
                evapotranspiracao_ponderada <- evapotranspiracao_ponderada$valor * vetor_modelo[13]
                evapotranspiracao_planicie_ponderada <- data.table::data.table(evapotranspiracao[which((evapotranspiracao$data >= data_inicio_simulacao)
                                            & (evapotranspiracao$data <= data_fim_simulacao))])
                evapotranspiracao_planicie_ponderada <- evapotranspiracao_planicie_ponderada$valor * vetor_modelo[14]
                
                
                vetor_inicializacao <- unlist(inicializacao)
                numero_dias <- length(evapotranspiracao_planicie_ponderada)
                
                saida <- funcaoSmapCpp::rodada_varios_dias_cpp2(vetor_modelo, vetor_inicializacao, area, precipitacao_ponderada,
                                                            evapotranspiracao_ponderada, evapotranspiracao_planicie_ponderada, numero_dias)
                saida <- data.table::data.table(saida)
                saida$data <- seq.Date(data_inicio_simulacao, data_fim_simulacao, by = 1)
                saida$precipitacao_ponderada <- precipitacao_ponderada
                saida$evapotranspiracao_ponderada <- evapotranspiracao_ponderada
                saida <- merge(saida, vazao, by = "data")
                saida$posto <- NULL
                colnames(saida)[22] <- "vazao_observada"
                saida$Ed <- NULL
                saida$Ed2 <- NULL
                saida$Ed3 <- NULL
                saida$Eb <- NULL
                data.table::setcolorder(saida, c("data", "precipitacao_ponderada", "evapotranspiracao_ponderada", "vazao_observada", "Qcalc",
                                                "Qbase", "Qsup1", "Qsup2", "Qplan", "Rsolo", "Rsup", "Rsup2", "Rsub",
                                                "Es", "Er", "Rec", "Marg", "Tu"))
                return(saida)
            }
        })

        output$dygraph <- dygraphs::renderDygraph({
            if (!is.null(saida())) {
                variaveis_grafico <- c("Qcalc", "Qbase", input$variaveis)
                vazao <- vazao_posto()
                precipitacao <- precipitacao_posto()
                postos_plu <- postos_plu()
                data_inicio_simulacao <- input$periodo_simulacao[1]
                data_fim_simulacao <- input$periodo_simulacao[2]

                precipitacao <- precipitacao[data >= data_inicio_simulacao & data <= data_fim_simulacao]

                precipitacao <- ponderacao_espacial(precipitacao, postos_plu[postos_plu$nome == input$sub_bacia])
                saida <- saida()
                saida <- data.table::melt(saida, id.vars = c("data"), variable.name = "variavel",
                                            value.name = "valor")
                simulacao <- xts::xts()
                for (variaveis in variaveis_grafico){
                    simulacao <- cbind(simulacao, xts::xts(saida$valor[which(saida$variavel == variaveis)], order.by = saida$data[which(saida$variavel == variaveis)]))
                }
                colnames(simulacao) <- variaveis_grafico
                observacao <- xts::xts(x = vazao$valor[which((vazao$data >= data_inicio_simulacao) & (vazao$data <= data_fim_simulacao))], order.by =  vazao$data[which((vazao$data >= data_inicio_simulacao) & (vazao$data <= data_fim_simulacao))])
                colnames(observacao) <- "vazao observada"

                prec_aux <- xts::xts(x = precipitacao$valor, order.by =  precipitacao$data)
                colnames(prec_aux) <- "Precipitacao"

                dygraphs::dygraph(cbind(simulacao, observacao, prec_aux),
                                main = input$sub_bacia) %>%
                dygraphs::dyHighlight(highlightCircleSize = 5,
                                    highlightSeriesBackgroundAlpha = 0.2,
                                    hideOnMouseOut = FALSE,
                                    highlightSeriesOpts = list(strokeWidth = 2)) %>% 
                dygraphs::dyRangeSelector() %>%
                dygraphs::dyAxis("y", label = "Vazao (m3/s)", independentTicks = TRUE) %>%
                dygraphs::dySeries("vazao.observada", color = "#0000FF") %>%
                dygraphs::dyBarSeries("Precipitacao", axis = 'y2', color = "#008000") %>%
                dygraphs::dyAxis("y2", label = "Precipitacao (mm)", valueRange = c(200, 0)) %>%
                dygraphs::dySeries("Qcalc", color = "#FF0000") %>%
                dygraphs::dySeries("Qbase", color = "#FFCC00") %>%
                dygraphs::dyLegend(show = "follow")
            }
        })

        output$dygraph_zoom <- dygraphs::renderDygraph({
            if (!is.null(saida())) {
                variaveis_grafico <- c("Qcalc", "Qbase", input$variaveis)
                vazao <- vazao_posto()
                evapotranspiracao <- evapotranspiracao_posto()
                precipitacao <- precipitacao_posto()
                postos_plu <- postos_plu()
                data_inicio_simulacao <- input$periodo_simulacao[1]
                data_fim_simulacao <- input$periodo_simulacao[2]

                precipitacao <- precipitacao[data >= data_inicio_simulacao & data <= data_fim_simulacao]
                evapotranspiracao <- evapotranspiracao[data >= data_inicio_simulacao & data <= data_fim_simulacao]

                precipitacao <- ponderacao_espacial(precipitacao, postos_plu[postos_plu$nome == input$sub_bacia])
                saida <- saida()
                saida <- data.table::melt(saida, id.vars = c("data"), variable.name = "variavel",
                                            value.name = "valor")
                simulacao <- xts::xts()
                for (variaveis in variaveis_grafico){
                    simulacao <- cbind(simulacao, xts::xts(saida$valor[which((saida$variavel == variaveis) & (saida$data >= input$zoom_calibracao[1]) & (saida$data <= input$zoom_calibracao[2]))], order.by = saida$data[which((saida$variavel == variaveis) & (saida$data >= input$zoom_calibracao[1]) & (saida$data <= input$zoom_calibracao[2]))]))
                }
                colnames(simulacao) <- variaveis_grafico
                observacao <- xts::xts(x = vazao$valor[which((vazao$data >= input$zoom_calibracao[1]) & (vazao$data <= input$zoom_calibracao[2]))], order.by =  vazao$data[which((vazao$data >= input$zoom_calibracao[1]) & (vazao$data <= input$zoom_calibracao[2]))])
                colnames(observacao) <- "vazao observada"

                prec_aux <- xts::xts(x = precipitacao$valor[which((precipitacao$data >= input$zoom_calibracao[1]) & (precipitacao$data <= input$zoom_calibracao[2]))], order.by =  precipitacao$data[which((precipitacao$data >= input$zoom_calibracao[1]) & (precipitacao$data <= input$zoom_calibracao[2]))])
                colnames(prec_aux) <- "Precipitacao"

                dygraphs::dygraph(cbind(simulacao, observacao, prec_aux),
                                main = input$sub_bacia, ) %>%
                dygraphs::dyHighlight(highlightCircleSize = 5,
                                    highlightSeriesBackgroundAlpha = 0.2,
                                    hideOnMouseOut = FALSE,
                                    highlightSeriesOpts = list(strokeWidth = 2)) %>% 
                dygraphs::dyRangeSelector() %>%
                dygraphs::dyAxis("y", label = "Vazao (m3/s)", independentTicks = TRUE) %>%
                dygraphs::dySeries("vazao.observada", color = "#0000FF") %>%
                dygraphs::dyBarSeries("Precipitacao", axis = 'y2', color = "#008000") %>%
                dygraphs::dyAxis("y2", label = "Precipitacao (mm)", valueRange = c(200, 0)) %>%
                dygraphs::dySeries("Qcalc", color = "#FF0000") %>%
                dygraphs::dySeries("Qbase", color = "#FFCC00") %>%
                dygraphs::dyLegend(show = "follow")
            }
        })

        output$tabela <- DT::renderDataTable(saida())

        output$funcao_objetivo <- shiny::renderText({
            if (!is.null(input[[paste0("posto_plu_1")]])) {
                data_inicio_objetivo <- input$periodo_calibracao[1]
                data_fim_objetivo <- input$periodo_calibracao[2]
                vazao <- vazao_posto()

                vazao_fo <- vazao[which((vazao$data >= data_inicio_objetivo) & (vazao$data <= data_fim_objetivo))]

                vazao_fo[, peso := 1 / .N]

                if (input$numero_periodo_desconsiderado >= 1) {
                    for (iperiodo in 1:input$numero_periodo_desconsiderado){
                        vazao_fo[data >= input[[paste0("periodo_desconsiderado_", iperiodo)]][1] & data <= input[[paste0("periodo_desconsiderado_", iperiodo)]][2], peso := 0]
                    }
                    vazao_fo[peso != 0, peso := 1 / .N]
                }

                if (input$funcao_objetivo == "dm") {
                    calcula_funcao_objetivo <- calcula_dm
                } else if (input$funcao_objetivo == "nse") {
                    calcula_funcao_objetivo <- calcula_nse
                } else if (input$funcao_objetivo == "mape") {
                    calcula_funcao_objetivo <- calcula_mape
                } else if (input$funcao_objetivo == "kge") {
                    calcula_funcao_objetivo <- calcula_kge
                }

                funcao_objetivo <- calcula_funcao_objetivo(saida()[data >= data_inicio_objetivo & data <= data_fim_objetivo, Qcalc], vazao_fo[, valor], vazao_fo[, peso])
                paste0("funcao objetivo = ", round(funcao_objetivo, 2))
            }
        })

        output$tabela_metrica1 <- shiny::renderTable({
            if (!is.null(input[[paste0("posto_plu_1")]])) {
                data_inicio_objetivo <- input$periodo_calibracao[1]
                data_fim_objetivo <- input$periodo_calibracao[2]
                vazao <- vazao_posto()

                vazao_fo <- vazao[which((vazao$data >= data_inicio_objetivo) & (vazao$data <= data_fim_objetivo))]

                vazao_fo[, peso := 1 / .N]

                if (input$numero_periodo_desconsiderado >= 1) {
                    for (iperiodo in 1:input$numero_periodo_desconsiderado){
                        vazao_fo[data >= input[[paste0("periodo_desconsiderado_", iperiodo)]][1] & data <= input[[paste0("periodo_desconsiderado_", iperiodo)]][2], peso := 0]
                    }
                    vazao_fo[peso != 0, peso := 1 / .N]
                }

                saida_objetivo <- saida()[data >= data_inicio_objetivo & data <= data_fim_objetivo]

                metricas <- data.table::data.table(tipo = c("historico", "simulado"))
                metricas[tipo == "simulado", media := saida_objetivo[, mean(Qcalc)]]
                metricas[tipo == "simulado", dp := saida_objetivo[, sd(Qcalc)]]
                metricas[tipo == "simulado", assimetria := saida_objetivo[, moments::skewness(Qcalc)]]
                metricas[tipo == "simulado", curtose := saida_objetivo[, moments::kurtosis(Qcalc)]]
                metricas[tipo == "simulado", p95 := saida_objetivo[, quantile(Qcalc, 0.95)]]
                metricas[tipo == "simulado", max := saida_objetivo[, max(Qcalc)]]
                metricas[tipo == "historico", media := vazao_fo[, mean(valor)]]
                metricas[tipo == "historico", dp := vazao_fo[, sd(valor)]]
                metricas[tipo == "historico", assimetria := vazao_fo[, moments::skewness(valor)]]
                metricas[tipo == "historico", curtose := vazao_fo[, moments::kurtosis(valor)]]
                metricas[tipo == "historico", p95 := vazao_fo[, quantile(valor, 0.95)]]
                metricas[tipo == "historico", max := vazao_fo[, max(valor)]]
                metricas        
            }
        })

        output$tabela_metrica2 <- shiny::renderTable({
            if (!is.null(input[[paste0("posto_plu_1")]])) {
                data_inicio_objetivo <- input$periodo_calibracao[1]
                data_fim_objetivo <- input$periodo_calibracao[2]
                vazao <- vazao_posto()

                vazao_fo <- vazao[which((vazao$data >= data_inicio_objetivo) & (vazao$data <= data_fim_objetivo))]

                vazao_fo[, peso := 1 / .N]

                if (input$numero_periodo_desconsiderado >= 1) {
                    for (iperiodo in 1:input$numero_periodo_desconsiderado){
                        vazao_fo[data >= input[[paste0("periodo_desconsiderado_", iperiodo)]][1] & data <= input[[paste0("periodo_desconsiderado_", iperiodo)]][2], peso := 0]
                    }
                    vazao_fo[peso != 0, peso := 1 / .N]
                }

                saida_objetivo <- saida()[data >= data_inicio_objetivo & data <= data_fim_objetivo]

                metricas <- data.table::data.table(metrica = as.character(), valor = as.numeric())

                metricas <- rbindlist(list(metricas, data.table::data.table(metrica = "dm", valor = calcula_dm(saida_objetivo[, Qcalc], vazao_fo[, valor], vazao_fo[, peso]))))
                metricas <- rbindlist(list(metricas, data.table::data.table(metrica = "mape", valor = calcula_mape(saida_objetivo[, Qcalc], vazao_fo[, valor], vazao_fo[, peso]))))
                metricas <- rbindlist(list(metricas, data.table::data.table(metrica = "nse", valor = calcula_nse(saida_objetivo[, Qcalc], vazao_fo[, valor], vazao_fo[, peso]))))
                metricas <- rbindlist(list(metricas, data.table::data.table(metrica = "pbias", valor = calcula_pbias(saida_objetivo[, Qcalc], vazao_fo[, valor], vazao_fo[, peso]))))
                metricas <- rbindlist(list(metricas, data.table::data.table(metrica = "correl", valor = calcula_correlacao(saida_objetivo[, Qcalc], vazao_fo[, valor], vazao_fo[, peso]))))
                metricas <- rbindlist(list(metricas, data.table::data.table(metrica = "kge", valor = calcula_kge(saida_objetivo[, Qcalc], vazao_fo[, valor], vazao_fo[, peso]))))
                metricas        
            }
        })

        output$metrica_mensal <- plotly::renderPlotly({
            if (!is.null(input[[paste0("posto_plu_1")]])) {
                data_inicio_objetivo <- input$periodo_calibracao[1]
                data_fim_objetivo <- input$periodo_calibracao[2]
                vazao <- vazao_posto()

                vazao_fo <- vazao[which((vazao$data >= data_inicio_objetivo) & (vazao$data <= data_fim_objetivo))]

                vazao_fo[, peso := 1 / .N]

                if (input$numero_periodo_desconsiderado >= 1) {
                    for (iperiodo in 1:input$numero_periodo_desconsiderado){
                        vazao_fo[data >= input[[paste0("periodo_desconsiderado_", iperiodo)]][1] & data <= input[[paste0("periodo_desconsiderado_", iperiodo)]][2], peso := 0]
                    }
                    vazao_fo[peso != 0, peso := 1 / .N]
                }
                saida_objetivo <- saida()[data >= data_inicio_objetivo & data <= data_fim_objetivo]

                metricas <- data.table::data.table(estatistica = as.character(), lubridate = as.integer(), V1 = as.numeric())
                simulacao <- saida_objetivo[, mean(Qcalc), by = lubridate::month(data)]
                simulacao[, estatistica := "media"]
                metricas <- rbindlist(list(metricas, simulacao), use.names=TRUE)

                simulacao <- saida_objetivo[, sd(Qcalc), by = lubridate::month(data)]
                simulacao[, estatistica := "dp"]
                metricas <- rbindlist(list(metricas, simulacao), use.names=TRUE)

                simulacao <- saida_objetivo[, moments::skewness(Qcalc), by = lubridate::month(data)]
                simulacao[, estatistica := "assimetria"]
                metricas <- rbindlist(list(metricas, simulacao), use.names=TRUE)

                simulacao <- saida_objetivo[, moments::kurtosis(Qcalc), by = lubridate::month(data)]
                simulacao[, estatistica := "curtose"]
                metricas <- rbindlist(list(metricas, simulacao), use.names=TRUE)
                metricas[, tipo := "simulado"]

                metricas2 <- data.table::data.table(estatistica = as.character(), lubridate = as.integer(), V1 = as.numeric())
                observado <- vazao_fo[, mean(valor), by = lubridate::month(data)]
                observado[, estatistica := "media"]
                metricas2 <- rbindlist(list(metricas2, observado), use.names=TRUE)

                observado <- vazao_fo[, sd(valor), by = lubridate::month(data)]
                observado[, estatistica := "dp"]
                metricas2 <- rbindlist(list(metricas2, observado), use.names=TRUE)

                observado <- vazao_fo[, moments::skewness(valor), by = lubridate::month(data)]
                observado[, estatistica := "assimetria"]
                metricas2 <- rbindlist(list(metricas2, observado), use.names=TRUE)

                observado <- vazao_fo[, moments::kurtosis(valor), by = lubridate::month(data)]
                observado[, estatistica := "curtose"]
                metricas2 <- rbindlist(list(metricas2, observado), use.names=TRUE)

                metricas2[, tipo := "observado"]

                metricas <- rbindlist(list(metricas, metricas2), use.names=TRUE)
                data.table::setnames(metricas, c("lubridate", "V1"), c("mes", "valor"))
                
                grafico_mensal <- ggplot2::ggplot(data = metricas[estatistica == input$estatistica], 
                                    ggplot2::aes(x = mes, y = valor, color = tipo)) + ggplot2::geom_line() + ggplot2::theme_light()
                grafico_mensal <- plotly::ggplotly(grafico_mensal)
                grafico_mensal
            }
        })


        shiny::observeEvent(input$botao_calibracao,{
            limite_superior <- as.numeric(rep(0, 16), c(16))
            limite_superior[1] <- input$limite_superior_str
            limite_superior[2] <- input$limite_superior_k2t
            limite_superior[3] <- input$limite_superior_crec
            limite_superior[4] <- input$limite_superior_capc
            limite_superior[5] <- input$limite_superior_k_kt
            limite_superior[6] <- input$limite_superior_h1
            limite_superior[7] <- input$limite_superior_k2t2
            limite_superior[8] <- input$limite_superior_ai
            limite_superior[9] <- input$limite_superior_h
            limite_superior[10] <- input$limite_superior_k1t
            limite_superior[11] <- input$limite_superior_k3t
            limite_superior[12] <- input$limite_superior_pcof
            limite_superior[13] <- input$limite_superior_ecof
            limite_superior[14] <- input$limite_superior_ecof2
            limite_superior[15] <- input$limite_superior_alfa
            limite_superior[16] <- input$limite_superior_beta
            
            limite_inferior <- as.numeric(rep(0, 16), c(16))
            limite_inferior[1] <- input$limite_inferior_str
            limite_inferior[2] <- input$limite_inferior_k2t
            limite_inferior[3] <- input$limite_inferior_crec
            limite_inferior[4] <- input$limite_inferior_capc
            limite_inferior[5] <- input$limite_inferior_k_kt
            limite_inferior[6] <- input$limite_inferior_h1
            limite_inferior[7] <- input$limite_inferior_k2t2
            limite_inferior[8] <- input$limite_inferior_ai
            limite_inferior[9] <- input$limite_inferior_h
            limite_inferior[10] <- input$limite_inferior_k1t
            limite_inferior[11] <- input$limite_inferior_k3t
            limite_inferior[12] <- input$limite_inferior_pcof
            limite_inferior[13] <- input$limite_inferior_ecof
            limite_inferior[14] <- input$limite_inferior_ecof2
            limite_inferior[15] <- input$limite_inferior_alfa
            limite_inferior[16] <- input$limite_inferior_beta
            
            vetor_modelo <- vetor_modelo()
            vetor_modelo[1] <- input$str
            vetor_modelo[2] <- input$k2t
            vetor_modelo[3] <- input$crec
            vetor_modelo[4] <- input$capc
            vetor_modelo[5] <- input$k_kt
            vetor_modelo[6] <- input$h1
            vetor_modelo[7] <- input$k2t2
            vetor_modelo[8] <- input$ai
            vetor_modelo[9] <- input$h
            vetor_modelo[10] <- input$k1t
            vetor_modelo[11] <- input$k3t
            vetor_modelo[12] <- input$pcof
            vetor_modelo[13] <- input$ecof
            vetor_modelo[14] <- input$ecof2
            vetor_modelo[15] <- input$alfa
            vetor_modelo[16] <- input$beta

            if (input$funcao_objetivo == "dm") {
                funcao_objetivo <- calcula_dm
                fnscale = 1
            } else if (input$funcao_objetivo == "nse") {
                funcao_objetivo <- calcula_nse
                fnscale = -1
            } else if (input$funcao_objetivo == "mape") {
                funcao_objetivo <- calcula_mape
                fnscale = 1
            } else if (input$funcao_objetivo == "kge") {
                funcao_objetivo <- calcula_kge
                fnscale = -1
            }

            area <- area()
            Ebin <- input$Ebin
            Tuin <- input$Tuin
            Supin <- input$Supin

            data_inicio_objetivo <- input$periodo_calibracao[1]
            data_fim_objetivo <- input$periodo_calibracao[2]
            kt_max <- input$kt_max
            kt_min <- input$kt_min
            data_inicio_simulacao <- input$periodo_simulacao[1]
            data_fim_simulacao <- input$periodo_simulacao[2]
            data_inicio_simulacao <- data_inicio_simulacao - kt_min
            data_fim_simulacao <- data_fim_simulacao + kt_max

            vazao <- vazao_posto()
            evapotranspiracao <- evapotranspiracao_posto()
            precipitacao <- precipitacao_posto()
            postos_plu <- postos_plu()
            numero_postos_plu <- nrow(postos_plu[postos_plu$nome == input$sub_bacia])

            if (numero_postos_plu > 1) {
                for (iposto in 1:numero_postos_plu){
                    vetor_modelo[(16 + iposto)] <- input[[paste0("posto_plu_", iposto)]]
                    limite_superior[(16 + iposto)] <- input[[paste0("limite_superior_posto_plu_", iposto)]]
                    limite_inferior[(16 + iposto)] <- input[[paste0("limite_inferior_posto_plu_", iposto)]]
                }
            }
            
            precipitacao <- precipitacao[data >= data_inicio_simulacao & data <= data_fim_simulacao]

            evapotranspiracao_fo <- data.table::data.table(evapotranspiracao[data >= data_inicio_simulacao + kt_min & data <= data_fim_simulacao - kt_max])

            vazao_fo <- vazao[which((vazao$data >= data_inicio_objetivo) & (vazao$data <= data_fim_objetivo))]
            
            vazao_fo[, peso := 1 / .N]

            if (input$numero_periodo_desconsiderado >= 1) {
                for (iperiodo in 1:input$numero_periodo_desconsiderado){
                    vazao_fo[data >= input[[paste0("periodo_desconsiderado_", iperiodo)]][1] & data <= input[[paste0("periodo_desconsiderado_", iperiodo)]][2], peso := 0]
                }
                vazao_fo[peso != 0, peso := 1 / .N]
            }

            # Disable the run button
            shiny::updateActionButton(session, "botao_calibracao", label = "Calibrando...aguarde")
            disable_button(TRUE)
            shinyjs::disable("botao_calibracao")
            
            # Execute the long-running function asynchronously
            par <- future::future({
                calibracao(vetor_modelo, kt_min, kt_max, area, Ebin, Tuin, Supin, precipitacao,
                                    evapotranspiracao_fo, vazao_fo, data_inicio_objetivo, data_fim_objetivo,
                                    limite_inferior, limite_superior, postos_plu[postos_plu$nome == input$sub_bacia],
                                    funcao_objetivo, fnscale, vazao_fo[, peso])
            })
            
            shiny::observe({
                if (future::resolved(par)) {
                    shiny::updateNumericInput(session, "str", value = as.numeric(future::value(par)$par[1]))
                    shiny::updateNumericInput(session, "k2t", value = as.numeric(future::value(par)$par[2]))
                    shiny::updateNumericInput(session, "crec", value = as.numeric(future::value(par)$par[3]))
                    shiny::updateNumericInput(session, "capc", value = as.numeric(future::value(par)$par[4]))
                    shiny::updateNumericInput(session, "k_kt", value = as.numeric(future::value(par)$par[5]))
                    shiny::updateNumericInput(session, "h1", value = as.numeric(future::value(par)$par[6]))
                    shiny::updateNumericInput(session, "k2t2", value = as.numeric(future::value(par)$par[7]))
                    shiny::updateNumericInput(session, "ai", value = as.numeric(future::value(par)$par[8]))
                    shiny::updateNumericInput(session, "h", value = as.numeric(future::value(par)$par[9]))
                    shiny::updateNumericInput(session, "k1t", value = as.numeric(future::value(par)$par[10]))
                    shiny::updateNumericInput(session, "k3t", value = as.numeric(future::value(par)$par[11]))
                    shiny::updateNumericInput(session, "pcof", value = as.numeric(future::value(par)$par[12]))
                    shiny::updateNumericInput(session, "ecof", value = as.numeric(future::value(par)$par[13]))
                    shiny::updateNumericInput(session, "ecof2", value = as.numeric(future::value(par)$par[14]))
                    shiny::updateNumericInput(session, "alfa", value = as.numeric(future::value(par)$par[15]))
                    shiny::updateNumericInput(session, "beta", value = as.numeric(future::value(par)$par[16]))
                    if (numero_postos_plu > 1) {
                        for (iposto in 1:numero_postos_plu){
                            postos_plu[postos_plu$nome == input$sub_bacia]$valor[iposto] <- as.numeric(future::value(par)$par[(16 + iposto)])
                            shiny::updateNumericInput(session, paste0("posto_plu_", iposto), value = as.numeric(future::value(par)$par[(16 + iposto)]))
                        }
                    }

                    disable_button(FALSE)
                    shinyjs::enable("botao_calibracao")
                    shiny::updateActionButton(session, "botao_calibracao", label = "Calibrar")
                }
            })
        })

        parametros_exportacao <- shiny::reactive({
            parametros <- parametros_posto()

            parametros$valor[parametros$parametro == "Str"] <- input$str
            parametros$valor[parametros$parametro == "K2t"] <- input$k2t
            parametros$valor[parametros$parametro == "Crec"] <- input$crec
            parametros$valor[parametros$parametro == "Capc"] <- input$capc
            parametros$valor[parametros$parametro == "K_kt"] <- input$k_kt
            parametros$valor[parametros$parametro == "H1"] <- input$h1
            parametros$valor[parametros$parametro == "K2t2"] <- input$k2t2
            parametros$valor[parametros$parametro == "Ai"] <- input$ai
            parametros$valor[parametros$parametro == "H"] <- input$h
            parametros$valor[parametros$parametro == "K1t"] <- input$k1t
            parametros$valor[parametros$parametro == "K3t"] <- input$k3t
            parametros$valor[parametros$parametro == "Pcof"] <- input$pcof
            parametros$valor[parametros$parametro == "Ecof"] <- input$ecof
            parametros$valor[parametros$parametro == "Ecof2"] <- input$ecof2
            parametros$valor[parametros$parametro == "Area"] <- area()
            parametros$valor[parametros$parametro == "ktMin"] <- input$kt_min
            parametros$valor[parametros$parametro == "ktMax"] <- input$kt_max
            if (nrow(parametros[parametros$parametro == "Alfa"]) == 0){
                parametros <- data.table::rbindlist(list(parametros, data.table::data.table(nome = parametros[, unique(nome)], parametro = "Alfa", valor = input$alfa)))
            } else {
                parametros$valor[parametros$parametro == "Alfa"] <- input$alfa
            }
            if (nrow(parametros[parametros$parametro == "Beta"]) == 0){
                parametros <- data.table::rbindlist(list(parametros, data.table::data.table(nome = parametros[, unique(nome)], parametro = "Beta", valor = input$beta)))
            } else {
                parametros$valor[parametros$parametro == "Beta"] <- input$beta
            }
            

            kt <- cria_kt(input$kt_max, input$kt_min, input$alfa, input$beta)
            parametros$valor[parametros$parametro %in% paste0("Kt", 2:-60)] <- kt

            return(parametros)
        })

        postos_plu_exportacao <- shiny::reactive({
            postos_plu <- postos_plu()
            postos_plu <- postos_plu[postos_plu$nome == input$sub_bacia]
            numero_postos_plu <- nrow(postos_plu)
            if (numero_postos_plu > 1) {
                for (iposto in 1:numero_postos_plu){
                    postos_plu[postos_plu$nome == input$sub_bacia]$valor[iposto] <- input[[paste0("posto_plu_", iposto)]]
                }
            }
            return(postos_plu)
        })

        output$download_parametros <- shiny::downloadHandler(
            filename = function() {
                paste0("parametros_", input$sub_bacia, ".csv")
            },
            content = function(file) {
                utils::write.table(parametros_exportacao(), file, quote = FALSE, row.names = FALSE, sep = ";")
            }
        )

        output$download_postos_plu <- shiny::downloadHandler(
            filename = function() {
                paste0("postos_plu_", input$sub_bacia, ".csv")
            },
            content = function(file) {
                utils::write.table(postos_plu_exportacao(), file, quote = FALSE, row.names = FALSE, sep = ";")
            }
        )
    }

    shiny::shinyApp(ui = ui_calibracao, server = servidor_calibracao)
}


##### VISUALIZADOR DE CASOS EXECUTADOS ############


#' Visualizador SMAP/ONS
#' 
#' Visualiza dos cenarios gerados pelo modelo SMAP/ONS
#'
#' @param previsoes contendo as seguintes colunas:
#'     \itemize{
#'     \item{data_caso - data da rodada}
#'     \item{data_previsao - data da previsao}
#'     \item{cenario - nome do cenario}
#'     \item{nome - nome da sub-bacia}
#'     \item{variavel - nome da variavel}
#'     \item{valor - valor da variavel}
#'     }
#' @param assimilacao data table com as colunas:
#' \itemize{
#'     \item{data_caso - data da rodada}
#'     \item{data_assimilacao - data da assimilacao}
#'     \item{cenario - nome do cenario}
#'     \item{nome - nome da sub-bacia}
#'     \item{variavel - nome da variavel}
#'     \item{valor - valor da variavel}
#'     }
#' @param precipitacao data table contendo precipitacao observada e previsata com as colunas:
#' \itemize{
#'     \item{data_previsao - data da precipitacao}
#'     \item{data_rodada - data da rodada}
#'     \item{cenario - nome do cenario}
#'     \item{nome - nome da sub-bacia}
#'     \item{variavel - nome da variavel}
#'     \item{valor - valor da variavel}
#'     }
#' @param funcao_objetivo data table com as colunas:
#' \itemize{
#'     \item{funcao_objetivo - valor obtido da funcao objetivo durante a assimilacao}
#'     \item{nome - nome da sub-bacia}
#'     \item{data_caso - data do caso executado}
#'     }
#' @export

executa_visualizador_previsao <- function(previsoes, assimilacao, precipitacao, funcao_objetivo){
    `%>%` <- magrittr::`%>%`
    
    ui_previsao <- shiny::fluidPage(
        shinyjs::useShinyjs(),
        shiny::titlePanel("Visualizador de cenarios do SMAP/ONS"),
        shiny::tabsetPanel(
            shiny::tabPanel("Cenarios SMAP/ONS",
                shiny::sidebarLayout(
                    shiny::sidebarPanel(
                        shiny::fluidRow(
                            shiny::selectInput("data_caso", shiny::h3("Data do Caso"), 
                            choices = unique(previsoes[, data_caso]), selected = NULL),
                            shiny::selectInput("sub_bacia", shiny::h3("Sub-bacia"), 
                            choices = unique(previsoes[, nome]), selected = NULL),
                        )
                    ),
                    shiny::mainPanel(
                        shiny::fluidRow(
                            plotly::plotlyOutput("plotly", heigh = "600px"),
                            shiny::textOutput("funcao_objetivo")
                        )
                    )
                )
            ),
            shiny::tabPanel("Tabela Previsao",
                DT::dataTableOutput("tabela_previsao")
            ),
            shiny::tabPanel("Tabela Assimilacao",
                DT::dataTableOutput("tabela_assimilacao")
            )
        )
    )

    servidor_previsao <- function(input, output, session) {

        previsoes_sub_bacia <- shiny::reactive({
            previsoes[nome == input$sub_bacia & data_caso == input$data_caso]
            previsoes
        })

        precipitacao_sub_bacia <- shiny::reactive({
            precipitacao[nome == input$sub_bacia & data_rodada == input$data_caso]
            precipitacao
        })

        output$tabela_previsao <- DT::renderDataTable(previsoes)

        output$tabela_assimilacao <- DT::renderDataTable(assimilacao)

        output$plotly <- plotly::renderPlotly({
            formata_label <- function(breaks) formatC(breaks, format = "d", width = 6, flag = " ")

            previsoes_sub_bacia <- previsoes_sub_bacia()
            precipitacao_sub_bacia <- precipitacao_sub_bacia()

            mediana <- previsoes_sub_bacia[variavel == "Qcalc", median(valor), by = c("data_previsao", "nome")]
            data.table::setnames(mediana, "V1", "mediana")
            minimo <- previsoes_sub_bacia[variavel == "Qcalc", min(valor), by = c("data_previsao", "nome")]
            data.table::setnames(minimo, "V1", "minimo")
            per_0_05 <- previsoes_sub_bacia[variavel == "Qcalc", quantile(valor, 0.05), by = c("data_previsao", "nome")]
            data.table::setnames(per_0_05, "V1", "per_0_05")
            per_0_25 <- previsoes_sub_bacia[variavel == "Qcalc", quantile(valor, 0.25), by = c("data_previsao", "nome")]
            data.table::setnames(per_0_25, "V1", "per_0_25")
            per_0_75 <- previsoes_sub_bacia[variavel == "Qcalc", quantile(valor, 0.75), by = c("data_previsao", "nome")]
            data.table::setnames(per_0_75, "V1", "per_0_75")
            per_0_95 <- previsoes_sub_bacia[variavel == "Qcalc", quantile(valor, 0.95), by = c("data_previsao", "nome")]
            data.table::setnames(per_0_95, "V1", "per_0_95")
            maximo <- previsoes_sub_bacia[variavel == "Qcalc", max(valor), by = c("data_previsao", "nome")]
            data.table::setnames(maximo, "V1", "maximo")

            quantis <- merge(mediana, per_0_25, by = c("data_previsao", "nome"))
            quantis <- merge(quantis, per_0_75, by = c("data_previsao", "nome"))
            quantis <- merge(quantis, per_0_95, by = c("data_previsao", "nome"))
            quantis <- merge(quantis, per_0_05, by = c("data_previsao", "nome"))
            quantis <- merge(quantis, minimo, by = c("data_previsao", "nome"))
            quantis <- merge(quantis, maximo, by = c("data_previsao", "nome"))


            grafico_vazao <- ggplot2::ggplot(data = quantis[nome == input$sub_bacia], ggplot2::aes(x = data_previsao)) +
                ggplot2::geom_ribbon(ggplot2::aes(x = data_previsao, ymin = minimo, ymax = per_0_05, fill = "0%-5%"), alpha = 0.7) +
                ggplot2::geom_ribbon(ggplot2::aes(x = data_previsao, ymin = per_0_05, ymax = per_0_25, fill = "5%-25%"), alpha = 0.8) +
                ggplot2::geom_ribbon(ggplot2::aes(x = data_previsao, ymin = per_0_25, ymax = per_0_75, fill = "25%-75%"), alpha = 1) +
                ggplot2::geom_ribbon(ggplot2::aes(x = data_previsao, ymin = per_0_75, ymax = per_0_95, fill = "75%-95%"), alpha = 0.8) +
                ggplot2::geom_ribbon(ggplot2::aes(x = data_previsao, ymin = per_0_95, ymax = maximo, fill = "95%-100%"), alpha = 1) +
                ggplot2::geom_line(ggplot2::aes(y = mediana, color = "mediana"), linetype = "dotted",size=1.3) + 
                ggplot2::ylab("Vazao Incremental (m3/s)") +
                ggplot2::theme_light() +
                ggplot2::scale_fill_manual(values=c("0%-5%" = '#FF6D6D', "5%-25%" = '#F4B183',
                                        "25%-75%" = '#C5E0B4', "75%-95%" = '#BDD7EE',
                                        '95%-100%' = '#5B9BD5'), name = "")+
                
                ggplot2::scale_color_manual(values=c('Observado'='black','mediana'='red'),name="")+
                
                ggplot2::theme(legend.position="bottom",legend.text = ggplot2::element_text(size = 8),axis.title.x = ggplot2::element_blank(),axis.text.y = ggplot2::element_text(family = "mono"))+
                
                ggplot2::scale_x_date(date_breaks = "3 day", date_labels = "%d/%b") +
                ggplot2::scale_y_continuous(labels = formata_label)

            mediana <- precipitacao_sub_bacia[data_previsao >= previsoes_sub_bacia[, min(data_previsao)] & data_previsao <= previsoes_sub_bacia[, max(data_previsao)], median(valor), by = c("data_previsao", "nome")]
            data.table::setnames(mediana, "V1", "mediana")
            minimo <- precipitacao_sub_bacia[data_previsao >= previsoes_sub_bacia[, min(data_previsao)] & data_previsao <= previsoes_sub_bacia[, max(data_previsao)], min(valor), by = c("data_previsao", "nome")]
            data.table::setnames(minimo, "V1", "minimo")
            p5 <- precipitacao_sub_bacia[data_previsao >= previsoes_sub_bacia[, min(data_previsao)] & data_previsao <= previsoes_sub_bacia[, max(data_previsao)], quantile(valor, 0.05), by = c("data_previsao", "nome")]
            data.table::setnames(p5, "V1", "5%")
            p25 <- precipitacao_sub_bacia[data_previsao >= previsoes_sub_bacia[, min(data_previsao)] & data_previsao <= previsoes_sub_bacia[, max(data_previsao)], quantile(valor, 0.25), by = c("data_previsao", "nome")]
            data.table::setnames(p25, "V1", "25%")
            p75 <- precipitacao_sub_bacia[data_previsao >= previsoes_sub_bacia[, min(data_previsao)] & data_previsao <= previsoes_sub_bacia[, max(data_previsao)], quantile(valor, 0.75), by = c("data_previsao", "nome")]
            data.table::setnames(p75, "V1", "75%")
            p95 <- precipitacao_sub_bacia[data_previsao >= previsoes_sub_bacia[, min(data_previsao)] & data_previsao <= previsoes_sub_bacia[, max(data_previsao)], quantile(valor, 0.95), by = c("data_previsao", "nome")]
            data.table::setnames(p95, "V1", "95%")
            p100 <- precipitacao_sub_bacia[data_previsao >= previsoes_sub_bacia[, min(data_previsao)] & data_previsao <= previsoes_sub_bacia[, max(data_previsao)], max(valor), by = c("data_previsao", "nome")]
            data.table::setnames(p100, "V1", "100%")

            quantis_prec <- merge(p75, p5, by = c("data_previsao", "nome"))
            quantis_prec <- merge(quantis_prec, p95, by = c("data_previsao", "nome"))
            quantis_prec <- merge(quantis_prec, p25, by = c("data_previsao", "nome"))
            quantis_prec <- merge(quantis_prec, p100, by = c("data_previsao", "nome"))

            quantis_prec <- data.table::melt(quantis_prec, id.vars = c("data_previsao", "nome"), variable.name = "variavel", value.name = "valor")
            quantis_prec$variavel <- factor(quantis_prec$variavel, , levels = c("100%", "95%", "75%", "25%","5%"))


            grafico_prec <- ggplot2::ggplot(data = quantis_prec[nome == input$sub_bacia], ggplot2::aes(x = data_previsao, y = valor, fill = variavel)) +
                ggplot2::geom_bar(data = quantis_prec[nome == input$sub_bacia], ggplot2::aes(x = data_previsao, y = valor, fill = variavel), stat = "identity") +
                ggplot2::scale_fill_manual(values = c( "5%" = '#FF6D6D', "25%" = '#F4B183', "75%" = '#C5E0B4', 
                                            "95%" = '#BDD7EE', "100%" = '#5B9BD5'))  +
            ggplot2::theme_light() +
            ggplot2::ylab("Prec (mm/dia)")+
            ggplot2::ggtitle(input$sub_bacia)+
                ggplot2::scale_x_date(date_breaks= "3 day", date_labels = "%d/%b", position = "top") +
                ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 24, face = 'bold'), axis.title.x = ggplot2::element_blank(), axis.text.y = ggplot2::element_text(family = "mono"),
                    legend.position = "top")+ ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1)) + ggplot2::scale_y_reverse(labels = formata_label, limits = c(100, 0)) 

            grafico_vazao_plotly <- plotly::ggplotly(grafico_vazao)
            grafico_prec_plotly <- plotly::ggplotly(grafico_prec)

            layout <- plotly::subplot(grafico_prec_plotly, grafico_vazao_plotly, nrows = 2, heights = c(0.2, 0.8))
            layout
        })

        output$tabela <- DT::renderDataTable(previsoes)

        output$funcao_objetivo <- shiny::renderText({
            paste0("funcao objetivo = ", funcao_objetivo$funcao_objetivo[(funcao_objetivo$nome == input$sub_bacia) & (funcao_objetivo$data_caso == input$data_caso)])
        })

    }

    shiny::shinyApp(ui = ui_previsao, server = servidor_previsao)
}
