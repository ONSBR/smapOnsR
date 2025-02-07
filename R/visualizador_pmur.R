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

executa_visualizador_calibracao_pmur <- function(){
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
                                shiny::numericInput(inputId = "limite_inferior_lambda", label = "LI lambda",value = NULL),
                                shiny::numericInput(inputId = "limite_inferior_h", label = "LI h",value = NULL),
                                shiny::numericInput(inputId = "limite_inferior_k1t", label = "LI k1t",value = NULL),
                                shiny::numericInput(inputId = "limite_inferior_k3t", label = "LI k3t",value = NULL),
                                shiny::numericInput(inputId = "limite_inferior_pcof", label = "LI pcof",value = NULL),
                                shiny::numericInput(inputId = "limite_inferior_ecof", label = "LI ecof",value = NULL),
                                shiny::numericInput(inputId = "limite_inferior_ecof2", label = "LI ecof2",value = NULL),
                                shiny::numericInput(inputId = "limite_inferior_pmur", label = "LI pmur",value = NULL),
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
                                        shiny::numericInput(inputId = "lambda", label = "lambda",value = NULL),
                                        shiny::numericInput(inputId = "h", label = "h",value = NULL),
                                        shiny::numericInput(inputId = "k1t", label = "k1t",value = NULL),
                                        shiny::numericInput(inputId = "k3t", label = "k3t",value = NULL),
                                        shiny::numericInput(inputId = "pcof", label = "pcof",value = NULL),
                                        shiny::numericInput(inputId = "ecof", label = "ecof",value = NULL),
                                        shiny::numericInput(inputId = "ecof2", label = "ecof2",value = NULL),
                                        shiny::numericInput(inputId = "pmur", label = "pmur",value = NULL),
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
                                        shiny::numericInput(inputId = "limite_superior_lambda", label = "LS lambda",value = NULL),
                                        shiny::numericInput(inputId = "limite_superior_h", label = "LS h",value = NULL),
                                        shiny::numericInput(inputId = "limite_superior_k1t", label = "LS k1t",value = NULL),
                                        shiny::numericInput(inputId = "limite_superior_k3t", label = "LS k3t",value = NULL),
                                        shiny::numericInput(inputId = "limite_superior_pcof", label = "LS pcof",value = NULL),
                                        shiny::numericInput(inputId = "limite_superior_ecof", label = "LS ecof",value = NULL),
                                        shiny::numericInput(inputId = "limite_superior_ecof2", label = "LS ecof2",value = NULL),
                                        shiny::numericInput(inputId = "limite_superior_pmur", label = "LS pmur",value = NULL),
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
                            shiny::column(3, shiny::selectInput(inputId ="funcao_objetivo", label = shiny::h3("Selecione a funcao objetivo"), choices = c("dm", "nse", "mape", "kge"), selected = "dm")),
                            shiny::column(3, shiny::selectInput(inputId = "tipo_escala", label = shiny::h3("Selecione a escala das variaveis"), choices = c(0, 1), selected = 0)),
                            shiny::column(3, shiny::selectInput(inputId ="ndeps", label = shiny::h3("Passo de otimizacao"), choices = c(1, 0.1, 0.001, 0.0001, 0.00001, 0.000001, 0.0000001), selected = 0.001))
                        ), shiny::fluidRow(
                            shiny::column(1)
                        ), shiny::fluidRow(
                            shiny::dateRangeInput("zoom_calibracao", "Zoom calibracao", start = NULL, end = NULL, min = NULL, max = NULL),
                            dygraphs::dygraphOutput("dygraph_zoom", heigh = "600px")
                        ), shiny::fluidRow(
                            shiny::column(1, shiny::actionButton(inputId = "botao_calibracao", label = "Calibrar", class = "btn-lg btn-success")),
                            shiny::column(1, shiny::checkboxGroupInput("variaveis", "variaveis", choices = c("Qsup1", "Qsup2", "Qplan"))),
                            shiny::column(1, shiny::textOutput("funcao_objetivo")),
                            shiny::column(6, shiny::tableOutput("tabela_metrica1")),
                            shiny::column(3, shiny::tableOutput("tabela_metrica2")),
                            shiny::selectInput(inputId = "estatistica", label = shiny::h3("Estatisticas mensais"), choices = c("media", "dp", "assimetria", "curtose")),
                            plotly::plotlyOutput("metrica_mensal"),
                            plotly::plotlyOutput("grafico_kts")
                        )
                    )
                )
            ),
            shiny::tabPanel("Tabela Dados",
                DT::dataTableOutput("tabela"),
                shiny::downloadButton("download_simulacao", "Download simulacao_sub_bacia.csv")
            ),
            shiny::tabPanel("Tabela info calibracao",
                DT::dataTableOutput("info_calibracao")
            )
        )
    )

    servidor_calibracao <- function(input, output, session) {

        disable_button <- shiny::reactiveVal(FALSE)
        
        get_param_value <- function(param_name, default_value, parametros, limite) {
            if (limite %in% colnames(parametros)) {
                return(parametros[parametros$parametro == param_name, ..limite])
            }
            return(default_value)
        }


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
                modelo <- new_modelo_smap_ons_pmur(parametros_posto, postos_plu[postos_plu$nome == input$sub_bacia])
                kt_max <- attributes(modelo)$kt_max
                kt_min <- attributes(modelo)$kt_min
                shiny::updateNumericInput(session, "str", value = vetor_modelo[1])
                shiny::updateNumericInput(session, "k2t", value = vetor_modelo[2])
                shiny::updateNumericInput(session, "crec", value = vetor_modelo[3])
                shiny::updateNumericInput(session, "capc", value = vetor_modelo[4])
                shiny::updateNumericInput(session, "k_kt", value = vetor_modelo[5])
                shiny::updateNumericInput(session, "h1", value = vetor_modelo[6])
                shiny::updateNumericInput(session, "k2t2", value = vetor_modelo[7])
                shiny::updateNumericInput(session, "lambda", value = vetor_modelo[8])
                shiny::updateNumericInput(session, "h", value = vetor_modelo[9])
                shiny::updateNumericInput(session, "k1t", value = vetor_modelo[10])
                shiny::updateNumericInput(session, "k3t", value = vetor_modelo[11])
                shiny::updateNumericInput(session, "pcof", value = vetor_modelo[12])
                shiny::updateNumericInput(session, "ecof", value = vetor_modelo[13])
                shiny::updateNumericInput(session, "ecof2", value = vetor_modelo[14])
                shiny::updateNumericInput(session, "pmur", value = vetor_modelo[15])
                shiny::updateNumericInput(session, "alfa", value = vetor_modelo[16])
                shiny::updateNumericInput(session, "beta", value = vetor_modelo[17])
                shiny::updateNumericInput(session, "kt_max", value = kt_max)
                shiny::updateNumericInput(session, "kt_min", value = kt_min)

                shiny::updateNumericInput(session, "limite_inferior_str",   value = get_param_value("Str", vetor_modelo[1] * 0.5, parametros_posto, "limite_inferior"))
                shiny::updateNumericInput(session, "limite_inferior_k2t",   value = get_param_value("K2t", vetor_modelo[2] * 0.5, parametros_posto, "limite_inferior"))
                shiny::updateNumericInput(session, "limite_inferior_crec",  value = get_param_value("Crec", vetor_modelo[3] * 0.5, parametros_posto, "limite_inferior"))
                shiny::updateNumericInput(session, "limite_inferior_capc",  value = get_param_value("Capc", vetor_modelo[4] * 0.5, parametros_posto, "limite_inferior"))
                shiny::updateNumericInput(session, "limite_inferior_k_kt",  value = get_param_value("K_kt", vetor_modelo[5] * 0.5, parametros_posto, "limite_inferior"))
                shiny::updateNumericInput(session, "limite_inferior_h1",    value = get_param_value("H1", vetor_modelo[6] * 0.5, parametros_posto, "limite_inferior"))
                shiny::updateNumericInput(session, "limite_inferior_k2t2",  value = get_param_value("K2t2", vetor_modelo[7] * 0.5, parametros_posto, "limite_inferior"))
                shiny::updateNumericInput(session, "limite_inferior_lambda",value = get_param_value("Lambda", vetor_modelo[8] * 0.5, parametros_posto, "limite_inferior"))
                shiny::updateNumericInput(session, "limite_inferior_h",     value = get_param_value("H", vetor_modelo[9] * 0.5, parametros_posto, "limite_inferior"))
                shiny::updateNumericInput(session, "limite_inferior_k1t",   value = get_param_value("K1t", vetor_modelo[10] * 0.5, parametros_posto, "limite_inferior"))
                shiny::updateNumericInput(session, "limite_inferior_k3t",   value = get_param_value("K3t", vetor_modelo[11] * 0.5, parametros_posto, "limite_inferior"))
                shiny::updateNumericInput(session, "limite_inferior_pcof",  value = get_param_value("Pcof", 0.8, parametros_posto, "limite_inferior"))
                shiny::updateNumericInput(session, "limite_inferior_ecof",  value = get_param_value("Ecof", 0.8, parametros_posto, "limite_inferior"))
                shiny::updateNumericInput(session, "limite_inferior_ecof2", value = get_param_value("Ecof2", 0.8, parametros_posto, "limite_inferior"))
                shiny::updateNumericInput(session, "limite_inferior_pmur",  value = get_param_value("Pmur", vetor_modelo[15] * 0.5, parametros_posto, "limite_inferior"))
                shiny::updateNumericInput(session, "limite_inferior_alfa",  value = get_param_value("Alfa", 0.001, parametros_posto, "limite_inferior"))
                shiny::updateNumericInput(session, "limite_inferior_beta",  value = get_param_value("Beta", 0.001, parametros_posto, "limite_inferior"))

                shiny::updateNumericInput(session, "limite_superior_str", value = get_param_value("Str", vetor_modelo[1] * 2, parametros_posto, "limite_superior"))
                shiny::updateNumericInput(session, "limite_superior_k2t", value = get_param_value("K2t", vetor_modelo[2] * 2, parametros_posto, "limite_superior"))
                shiny::updateNumericInput(session, "limite_superior_crec",value = get_param_value("Crec", vetor_modelo[3] * 2, parametros_posto, "limite_superior"))
                shiny::updateNumericInput(session, "limite_superior_capc",value = get_param_value("Capc", vetor_modelo[4] * 2, parametros_posto, "limite_superior"))
                shiny::updateNumericInput(session, "limite_superior_k_kt",value = get_param_value("K_kt", vetor_modelo[5] * 2, parametros_posto, "limite_superior"))
                shiny::updateNumericInput(session, "limite_superior_h1", value = get_param_value("H1", vetor_modelo[6] * 2, parametros_posto, "limite_superior"))
                shiny::updateNumericInput(session, "limite_superior_k2t2",value = get_param_value("K2t2", vetor_modelo[7] * 2, parametros_posto, "limite_superior"))
                shiny::updateNumericInput(session, "limite_superior_lambda", value = get_param_value("Lambda", vetor_modelo[8] * 2, parametros_posto, "limite_superior"))
                shiny::updateNumericInput(session, "limite_superior_h", value = get_param_value("H", vetor_modelo[9] * 2, parametros_posto, "limite_superior"))
                shiny::updateNumericInput(session, "limite_superior_k1t", value = get_param_value("K1t", vetor_modelo[10] * 2, parametros_posto, "limite_superior"))
                shiny::updateNumericInput(session, "limite_superior_k3t", value = get_param_value("K3t", vetor_modelo[11] * 2, parametros_posto, "limite_superior"))
                shiny::updateNumericInput(session, "limite_superior_pcof",value = get_param_value("Pcof", 0.8, parametros_posto, "limite_superior"))
                shiny::updateNumericInput(session, "limite_superior_ecof",value = get_param_value("Ecof", 0.8, parametros_posto, "limite_superior"))
                shiny::updateNumericInput(session, "limite_superior_ecof2", value = get_param_value("Ecof2", 0.8, parametros_posto, "limite_superior"))
                shiny::updateNumericInput(session, "limite_superior_pmur",value = get_param_value("Pmur", vetor_modelo[15] * 2, parametros_posto, "limite_superior"))
                shiny::updateNumericInput(session, "limite_superior_alfa",value = get_param_value("Alfa", 0.001, parametros_posto, "limite_superior"))
                shiny::updateNumericInput(session, "limite_superior_beta",value = get_param_value("Beta", 0.001, parametros_posto, "limite_superior"))
                precipitacao <- precipitacao_posto()
                data_minimo <- (min(precipitacao$data) + kt_min)
                data_maximo <- (max(precipitacao$data) - kt_max)
                data_inicio_simulacao <- data_minimo
                data_final_simulacao <- data_maximo
                data_inicio_objetivo <- data_minimo
                data_final_objetivo <- data_maximo

                periodos <- periodos()
                if (nrow(periodos) > 0) {
                    if (nrow(periodos[parametro == "data_inicio_simulacao"]) > 0) {
                        if (periodos[parametro == "data_inicio_simulacao", valor] >= data_minimo) {
                            data_inicio_simulacao <- periodos[parametro == "data_inicio_simulacao", valor]
                        } else {
                            data_inicio_simulacao <- data_minimo
                        }
                    }
                    if (nrow(periodos[parametro == "data_final_simulacao"]) > 0) {
                        if (periodos[parametro == "data_final_simulacao", valor] <= data_maximo) {
                            data_final_simulacao <- periodos[parametro == "data_final_simulacao", valor]
                        } else {
                            data_final_simulacao <- data_maximo
                        }
                    }
                    if (nrow(periodos[parametro == "data_inicio_objetivo"]) > 0) {
                        if (periodos[parametro == "data_inicio_objetivo", valor] >= data_minimo) {
                            data_inicio_objetivo <- periodos[parametro == "data_inicio_objetivo", valor]
                        } else {
                            data_inicio_objetivo <- data_minimo
                        }
                    }
                    if (nrow(periodos[parametro == "data_final_objetivo"]) > 0) {
                        if (periodos[parametro == "data_final_objetivo", valor] <= data_maximo) {
                            data_final_objetivo <- periodos[parametro == "data_final_objetivo", valor]
                        } else {
                            data_final_objetivo <- data_maximo
                        }
                    }
                }
                
                shiny::updateDateRangeInput(session, "periodo_simulacao", start = data_inicio_simulacao, end = data_final_simulacao, min = data_minimo, max = data_maximo)
                shiny::updateDateRangeInput(session, "periodo_calibracao", start = data_inicio_objetivo, end = data_final_objetivo, min = data_minimo, max = data_maximo)
                shiny::updateDateRangeInput(session, "zoom_calibracao", start = data_inicio_simulacao, end = data_final_simulacao, min = data_minimo, max = data_maximo)
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
            arquivo_parametros <- input$arquivo_parametros
            if (!is.null(arquivo_parametros)) {
                vetor_modelo <- vetor_modelo()
                parametros_posto <- parametros_posto()
                postos_plu <- postos_plu()
                modelo <- new_modelo_smap_ons_pmur(parametros_posto, postos_plu[postos_plu$nome == input$sub_bacia])
                kt_max <- attributes(modelo)$kt_max
                kt_min <- attributes(modelo)$kt_min
                precipitacao <- precipitacao_posto()
                data_minimo <- (min(precipitacao$data) + kt_min)
                data_maximo <- (max(precipitacao$data) - kt_max)
                data_inicio_simulacao <- input$periodo_simulacao[1]
                data_final_simulacao <- input$periodo_simulacao[2]
                data_inicio_calibracao <- data_minimo
                data_final_calibracao <- data_maximo
                periodos <- periodos()
                if (nrow(periodos) > 0) {
                    if (nrow(periodos[parametro == "data_inicio_simulacao"]) > 0) {
                        if (periodos[parametro == "data_inicio_simulacao", valor] >= data_minimo) {
                            data_inicio_simulacao <- input$periodo_simulacao[1]
                    } else {
                            data_inicio_simulacao <- data_minimo
                        }
                    }
                    if (nrow(periodos[parametro == "data_final_simulacao"]) > 0) {
                        if (periodos[parametro == "data_final_simulacao", valor] >= data_minimo) {
                            data_final_simulacao <- input$periodo_simulacao[2]
                        } else {
                            data_final_simulacao <- data_maximo
                        }
                    }
                }

                if (data_inicio_simulacao > input$periodo_calibracao[1]) {
                    data_inicio_calibracao <- data_inicio_simulacao
                } else {
                    data_inicio_calibracao <- input$periodo_calibracao[1]
                }

                if (data_final_simulacao < input$periodo_calibracao[2]) {
                    data_final_calibracao <- data_final_simulacao
                } else {
                    data_final_calibracao <- input$periodo_calibracao[2]
                }

                shiny::updateDateRangeInput(session, "periodo_simulacao", start = data_inicio_simulacao, end = data_final_simulacao, min = data_minimo, max = data_maximo)
                shiny::updateDateRangeInput(session, "periodo_calibracao", start = data_inicio_calibracao, end = data_final_calibracao, min = data_minimo, max = data_maximo)
                shiny::updateDateRangeInput(session, "zoom_calibracao", start = data_inicio_simulacao, end = data_final_simulacao, min = data_minimo, max = data_maximo)
            }
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
                data_inicio_simulacao <- input$periodo_simulacao[1]
                data_final_simulacao <- input$periodo_simulacao[2]
                if (data_inicio_simulacao >= data_minimo) {
                    data_inicio_simulacao <- data_inicio_simulacao
                } else {
                    data_inicio_simulacao <- data_minimo
                }
                if (data_final_simulacao <= data_maximo) {
                    data_final_simulacao <- data_final_simulacao
                } else {
                    data_final_simulacao <- data_maximo
                }
                
                data_inicio_objetivo <- input$periodo_calibracao[1]
                data_final_objetivo <- input$periodo_calibracao[2]
                if (data_inicio_objetivo >= data_minimo) {
                    data_inicio_objetivo <- data_inicio_objetivo
                } else {
                    data_inicio_objetivo <- data_minimo
                }

                if (data_final_objetivo <= data_maximo) {
                    data_final_objetivo <- data_final_objetivo
                } else {
                    data_final_objetivo <- data_maximo
                }

                shiny::updateDateRangeInput(session, "periodo_simulacao", start = data_inicio_simulacao, end = data_final_simulacao, min = data_minimo, max = data_maximo)
                shiny::updateDateRangeInput(session, "periodo_calibracao", start = data_inicio_objetivo, end = data_final_objetivo, min = data_minimo, max = data_maximo)
                shiny::updateDateRangeInput(session, "zoom_calibracao", start = data_inicio_simulacao, end = data_final_simulacao, min = data_minimo, max = data_maximo)
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
                data_inicio_simulacao <- input$periodo_simulacao[1]
                data_final_simulacao <- input$periodo_simulacao[2]
                if (data_inicio_simulacao >= data_minimo) {
                    data_inicio_simulacao <- data_inicio_simulacao
                } else {
                    data_inicio_simulacao <- data_minimo
                }
                if (data_final_simulacao <= data_maximo) {
                    data_final_simulacao <- data_final_simulacao
                } else {
                    data_final_simulacao <- data_maximo
                }

                data_inicio_objetivo <- input$periodo_calibracao[1]
                data_final_objetivo <- input$periodo_calibracao[2]
                if (data_inicio_objetivo >= data_minimo) {
                    data_inicio_objetivo <- data_inicio_objetivo
                } else {
                    data_inicio_objetivo <- data_minimo
                }

                if (data_final_objetivo <= data_maximo) {
                    data_final_objetivo <- data_final_objetivo
                } else {
                    data_final_objetivo <- data_maximo
                }

                shiny::updateDateRangeInput(session, "periodo_simulacao", start = data_inicio_simulacao, end = data_final_simulacao, min = data_minimo, max = data_maximo)
                shiny::updateDateRangeInput(session, "periodo_calibracao", start = data_inicio_objetivo, end = data_final_objetivo, min = data_minimo, max = data_maximo)
                shiny::updateDateRangeInput(session, "zoom_calibracao", start = data_inicio_simulacao, end = data_final_simulacao, min = data_minimo, max = data_maximo)
            }
        })
        
        parametros <- shiny::reactive({
            arquivo_parametros <- input$arquivo_parametros$datapath
            if (!is.null(arquivo_parametros)) {
                parametros <- le_parametros(arquivo_parametros)
                return(parametros)
            }
        })

        periodos <- shiny::reactive({
            arquivo_parametros <- input$arquivo_parametros$datapath
            if (!is.null(arquivo_parametros)) {
                periodos <- le_datas_calibracao(arquivo_parametros)
                if (nrow(periodos > 0)) {
                    return(periodos)
                } else {
                    return(data.table::data.table())
                }
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
                parametros <- parametros[parametros$nome == input$sub_bacia]
                if (any(parametros$parametro == "Pmur")){
                    
                } else {
                    parametros <- rbind(parametros, data.table::data.table(nome = input$sub_bacia, 
                    parametro = "Pmur", valor = parametros[parametros$parametro == "Str"]$valor * 
                            parametros[parametros$parametro == "Capc"]$valor / 200,
                            limite_inferior = parametros[parametros$parametro == "Capc"]$valor * 0.8 / 200,
                            limite_superior = parametros[parametros$parametro == "Capc"]$valor * 1.2 / 200))
                }
                if (any(parametros$parametro == "Lambda")){
                    
                } else {
                    parametros <- rbind(parametros, data.table::data.table(nome = input$sub_bacia, 
                    parametro = "Lambda", valor = parametros[parametros$parametro == "Ai"]$valor ,
                            limite_inferior = parametros[parametros$parametro == "Ai"]$valor * 0.8,
                            limite_superior = parametros[parametros$parametro == "Ai"]$valor * 1.2))
                }
                return(parametros)
            }
        })

        vetor_modelo <- shiny::reactive({
            arquivo_parametros <- input$arquivo_parametros$datapath
            arquivo_postos_plu <- input$arquivo_postos_plu$datapath
            if (!is.null(arquivo_parametros) & !is.null(arquivo_postos_plu)) {
                postos_plu <- postos_plu()
                parametros_posto <- parametros_posto()
                modelo <- new_modelo_smap_ons_pmur(parametros_posto, postos_plu[postos_plu$nome == input$sub_bacia])
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
                vetor_modelo <- as.numeric(c(vetor_modelo[1:11], vetor_modelo[75:78], alfa, beta))
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
            vetor_modelo[16] <- input$alfa
            vetor_modelo[17] <- input$beta
            kt_max <- input$kt_max
            kt_min <- input$kt_min
            
            kt <- cria_kt(kt_max, kt_min, vetor_modelo[16], vetor_modelo[17])
            kt <- data.table::data.table(kt)
            kt$lag <- 2:-60

            plot <- plotly::plot_ly(data = kt[which(lag %in% kt_max:-kt_min)], x = ~lag, y = ~kt, name = 'Distribuicao dos Kts', type = 'scatter', mode = 'lines', height = 4, width = 4) 
        })

        saida <-  shiny::reactive({
            if (!is.null(input[[paste0("posto_plu_1")]])) {
                if (!is.null(input$periodo_simulacao)) {
                    vetor_modelo <- vetor_modelo()
                    vetor_modelo[1] <- input$str
                    vetor_modelo[2] <- input$k2t
                    vetor_modelo[3] <- input$crec
                    vetor_modelo[4] <- input$capc
                    vetor_modelo[5] <- input$k_kt
                    vetor_modelo[6] <- input$h1
                    vetor_modelo[7] <- input$k2t2
                    vetor_modelo[8] <- input$lambda
                    vetor_modelo[9] <- input$h
                    vetor_modelo[10] <- input$k1t
                    vetor_modelo[11] <- input$k3t
                    vetor_modelo[12] <- input$pcof
                    vetor_modelo[13] <- input$ecof
                    vetor_modelo[14] <- input$ecof2
                    vetor_modelo[15] <- input$pmur
                    vetor_modelo[16] <- input$alfa
                    vetor_modelo[17] <- input$beta
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
                    
                    kt <- cria_kt(kt_max, kt_min, vetor_modelo[16], vetor_modelo[17])
                    
                    inicializacao <- inicializacao_smap(vetor_modelo, area, Ebin, Tuin, Supin)
                    
                    numero_postos_plu <- nrow(postos_plu[postos_plu$nome == input$sub_bacia])
                    if (numero_postos_plu > 1) {
                        for (iposto in 1: numero_postos_plu){
                            vetor_modelo[(17 + iposto)] <- input[[paste0("posto_plu_", iposto)]]
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
                    
                    saida <- funcaoSmapCpp::rodada_pmur_cpp(vetor_modelo, vetor_inicializacao, area, precipitacao_ponderada,
                                                                evapotranspiracao_ponderada, evapotranspiracao_planicie_ponderada, numero_dias)
                    saida <- data.table::data.table(saida)
                    saida$data <- seq.Date(data_inicio_simulacao, data_fim_simulacao, by = 1)
                    saida$precipitacao_ponderada <- precipitacao_ponderada
                    saida$evapotranspiracao_ponderada <- evapotranspiracao_ponderada
                    saida <- merge(saida, vazao, by = "data")
                    saida$posto <- NULL
                    
                    colnames(saida)[23] <- "vazao_observada"
                    saida$Ed <- NULL
                    saida$Ed2 <- NULL
                    saida$Ed3 <- NULL
                    saida$Eb <- NULL
                    data.table::setcolorder(saida, c("data", "precipitacao_ponderada", "evapotranspiracao_ponderada", "vazao_observada", "Qcalc",
                                                    "Qbase", "Qsup1", "Qsup2", "Qplan", "Rsolo", "Rsup", "Rsup2", "Rsub",
                                                    "Es", "Er", "Rec", "Marg", "Tu", "Ai"))
                    return(saida)
                }
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

        info_calibracao <- reactiveVal(data.table::data.table())

        output$info_calibracao <- DT::renderDataTable(info_calibracao())

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
                paste0("funcao objetivo = ", round(funcao_objetivo, 4))
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
            limite_superior <- as.numeric(rep(0, 17), c(17))
            limite_superior[1] <- input$limite_superior_str
            limite_superior[2] <- input$limite_superior_k2t
            limite_superior[3] <- input$limite_superior_crec
            limite_superior[4] <- input$limite_superior_capc
            limite_superior[5] <- input$limite_superior_k_kt
            limite_superior[6] <- input$limite_superior_h1
            limite_superior[7] <- input$limite_superior_k2t2
            limite_superior[8] <- input$limite_superior_lambda
            limite_superior[9] <- input$limite_superior_h
            limite_superior[10] <- input$limite_superior_k1t
            limite_superior[11] <- input$limite_superior_k3t
            limite_superior[12] <- input$limite_superior_pcof
            limite_superior[13] <- input$limite_superior_ecof
            limite_superior[14] <- input$limite_superior_ecof2
            limite_superior[15] <- input$limite_superior_pmur
            limite_superior[16] <- input$limite_superior_alfa
            limite_superior[17] <- input$limite_superior_beta
            
            limite_inferior <- as.numeric(rep(0, 17), c(17))
            limite_inferior[1] <- input$limite_inferior_str
            limite_inferior[2] <- input$limite_inferior_k2t
            limite_inferior[3] <- input$limite_inferior_crec
            limite_inferior[4] <- input$limite_inferior_capc
            limite_inferior[5] <- input$limite_inferior_k_kt
            limite_inferior[6] <- input$limite_inferior_h1
            limite_inferior[7] <- input$limite_inferior_k2t2
            limite_inferior[8] <- input$limite_inferior_lambda
            limite_inferior[9] <- input$limite_inferior_h
            limite_inferior[10] <- input$limite_inferior_k1t
            limite_inferior[11] <- input$limite_inferior_k3t
            limite_inferior[12] <- input$limite_inferior_pcof
            limite_inferior[13] <- input$limite_inferior_ecof
            limite_inferior[14] <- input$limite_inferior_ecof2
            limite_inferior[15] <- input$limite_inferior_pmur
            limite_inferior[16] <- input$limite_inferior_alfa
            limite_inferior[17] <- input$limite_inferior_beta
            
            vetor_modelo <- vetor_modelo()
            vetor_modelo[1] <- input$str
            vetor_modelo[2] <- input$k2t
            vetor_modelo[3] <- input$crec
            vetor_modelo[4] <- input$capc
            vetor_modelo[5] <- input$k_kt
            vetor_modelo[6] <- input$h1
            vetor_modelo[7] <- input$k2t2
            vetor_modelo[8] <- input$lambda
            vetor_modelo[9] <- input$h
            vetor_modelo[10] <- input$k1t
            vetor_modelo[11] <- input$k3t
            vetor_modelo[12] <- input$pcof
            vetor_modelo[13] <- input$ecof
            vetor_modelo[14] <- input$ecof2
            vetor_modelo[15] <- input$pmur
            vetor_modelo[16] <- input$alfa
            vetor_modelo[17] <- input$beta

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
            tipo_escala <- input$tipo_escala
            ndeps <- as.numeric(input$ndeps)

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
                    vetor_modelo[(17 + iposto)] <- input[[paste0("posto_plu_", iposto)]]
                    limite_superior[(17 + iposto)] <- input[[paste0("limite_superior_posto_plu_", iposto)]]
                    limite_inferior[(17 + iposto)] <- input[[paste0("limite_inferior_posto_plu_", iposto)]]
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
                calibracao_pmur_paralela(vetor_modelo, kt_min, kt_max, area, Ebin, Tuin, Supin, precipitacao,
                                    evapotranspiracao_fo, vazao_fo, data_inicio_objetivo, data_fim_objetivo,
                                    limite_inferior, limite_superior, postos_plu[postos_plu$nome == input$sub_bacia],
                                    funcao_objetivo, fnscale, vazao_fo[, peso], tipo_escala, ndeps)
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
                    shiny::updateNumericInput(session, "lambda", value = as.numeric(future::value(par)$par[8]))
                    shiny::updateNumericInput(session, "h", value = as.numeric(future::value(par)$par[9]))
                    shiny::updateNumericInput(session, "k1t", value = as.numeric(future::value(par)$par[10]))
                    shiny::updateNumericInput(session, "k3t", value = as.numeric(future::value(par)$par[11]))
                    shiny::updateNumericInput(session, "pcof", value = as.numeric(future::value(par)$par[12]))
                    shiny::updateNumericInput(session, "ecof", value = as.numeric(future::value(par)$par[13]))
                    shiny::updateNumericInput(session, "ecof2", value = as.numeric(future::value(par)$par[14]))
                    shiny::updateNumericInput(session, "pmur", value = as.numeric(future::value(par)$par[15]))
                    shiny::updateNumericInput(session, "alfa", value = as.numeric(future::value(par)$par[16]))
                    shiny::updateNumericInput(session, "beta", value = as.numeric(future::value(par)$par[17]))

                    if (numero_postos_plu > 1) {
                        for (iposto in 1:numero_postos_plu){
                            postos_plu[postos_plu$nome == input$sub_bacia]$valor[iposto] <- as.numeric(future::value(par)$par[(17 + iposto)])
                            shiny::updateNumericInput(session, paste0("posto_plu_", iposto), value = as.numeric(future::value(par)$par[(17 + iposto)]))
                        }
                    }

                    disable_button(FALSE)
                    shinyjs::enable("botao_calibracao")
                    shiny::updateActionButton(session, "botao_calibracao", label = "Calibrar")

                    tabela <- data.table::as.data.table(future::value(par)$loginfo)
                    if (numero_postos_plu > 1) {
                        colnames(tabela) <- c("step", "str", "k2t", "crec", "capc", "k_kt", "h1", "k2t2", 
                                          "lambda", "h", "k1t", "k3t", "pcof", "ecof", "ecof2", "pmur",
                                          "alfa", "beta", paste0("ke", 1:numero_postos_plu),
                                          "fn", "grad_str", "grad_k2t", "grad_crec", 
                                          "grad_capc", "grad_k_kt", "grad_h1", "grad_k2t2", 
                                          "grad_lambda", "grad_h", "grad_k1t", "grad_k3t", 
                                          "grad_pcof", "grad_ecof", "grad_ecof2", "grad_ecof2",
                                          "grad_pmur", 
                                          "grad_beta", paste0("grad_ke", 1:numero_postos_plu))
                    } else {
                        colnames(tabela) <- c("step", "str", "k2t", "crec", "capc", "k_kt", "h1", "k2t2", 
                                            "ai", "h", "k1t", "k3t", "pcof", "ecof", "ecof2", "pmur",
                                            "alfa", "beta", "fn", "grad_str", "grad_k2t", "grad_crec", 
                                            "grad_capc", "grad_k_kt", "grad_h1", "grad_k2t2", 
                                            "grad_ai", "grad_h", "grad_k1t", "grad_k3t", 
                                            "grad_pcof", "grad_ecof", "grad_ecof2", 
                                            "grad_pmur", "grad_alfa", "grad_beta")
                    }
                    info_calibracao(tabela)
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
            parametros$valor[parametros$parametro == "Lambda"] <- input$lambda
            parametros$valor[parametros$parametro == "H"] <- input$h
            parametros$valor[parametros$parametro == "K1t"] <- input$k1t
            parametros$valor[parametros$parametro == "K3t"] <- input$k3t
            parametros$valor[parametros$parametro == "Pcof"] <- input$pcof
            parametros$valor[parametros$parametro == "Ecof"] <- input$ecof
            parametros$valor[parametros$parametro == "Ecof2"] <- input$ecof2

            parametros$limite_inferior[parametros$parametro == "Str"] <- input$limite_inferior_str
            parametros$limite_inferior[parametros$parametro == "K2t"] <- input$limite_inferior_k2t
            parametros$limite_inferior[parametros$parametro == "Crec"] <- input$limite_inferior_crec
            parametros$limite_inferior[parametros$parametro == "Capc"] <- input$limite_inferior_capc
            parametros$limite_inferior[parametros$parametro == "K_kt"] <- input$limite_inferior_k_kt
            parametros$limite_inferior[parametros$parametro == "H1"] <- input$limite_inferior_h1
            parametros$limite_inferior[parametros$parametro == "K2t2"] <- input$limite_inferior_k2t2
            parametros$limite_inferior[parametros$parametro == "Lambda"] <- input$limite_inferior_lambda
            parametros$limite_inferior[parametros$parametro == "H"] <- input$limite_inferior_h
            parametros$limite_inferior[parametros$parametro == "K1t"] <- input$limite_inferior_k1t
            parametros$limite_inferior[parametros$parametro == "K3t"] <- input$limite_inferior_k3t
            parametros$limite_inferior[parametros$parametro == "Pcof"] <- input$limite_inferior_pcof
            parametros$limite_inferior[parametros$parametro == "Ecof"] <- input$limite_inferior_ecof
            parametros$limite_inferior[parametros$parametro == "Ecof2"] <- input$limite_inferior_ecof2

            parametros$limite_superior[parametros$parametro == "Str"] <- input$limite_superior_str
            parametros$limite_superior[parametros$parametro == "K2t"] <- input$limite_superior_k2t
            parametros$limite_superior[parametros$parametro == "Crec"] <- input$limite_superior_crec
            parametros$limite_superior[parametros$parametro == "Capc"] <- input$limite_superior_capc
            parametros$limite_superior[parametros$parametro == "K_kt"] <- input$limite_superior_k_kt
            parametros$limite_superior[parametros$parametro == "H1"] <- input$limite_superior_h1
            parametros$limite_superior[parametros$parametro == "K2t2"] <- input$limite_superior_k2t2
            parametros$limite_superior[parametros$parametro == "Lambda"] <- input$limite_superior_lambda
            parametros$limite_superior[parametros$parametro == "H"] <- input$limite_superior_h
            parametros$limite_superior[parametros$parametro == "K1t"] <- input$limite_superior_k1t
            parametros$limite_superior[parametros$parametro == "K3t"] <- input$limite_superior_k3t
            parametros$limite_superior[parametros$parametro == "Pcof"] <- input$limite_superior_pcof
            parametros$limite_superior[parametros$parametro == "Ecof"] <- input$limite_superior_ecof
            parametros$limite_superior[parametros$parametro == "Ecof2"] <- input$limite_superior_ecof2

            parametros$valor[parametros$parametro == "Area"] <- area()
            parametros$valor[parametros$parametro == "ktMin"] <- input$kt_min
            parametros$valor[parametros$parametro == "ktMax"] <- input$kt_max
            if (nrow(parametros[parametros$parametro == "Pmur"]) == 0){
                parametros <- data.table::rbindlist(list(parametros, data.table::data.table(nome = parametros[, unique(nome)], 
                            parametro = "Pmur", valor = input$pmur, limite_inferior = input$pmur * 0.8 , limite_superior = input$pmur * 1.2)))
            } else {
                parametros$valor[parametros$parametro == "Pmur"] <- input$pmur
            }
            if (nrow(parametros[parametros$parametro == "Alfa"]) == 0){
                parametros <- data.table::rbindlist(list(parametros, data.table::data.table(nome = parametros[, unique(nome)], 
                parametro = "Alfa", valor = input$alfa, limite_inferior = 0.001, limite_superior = 100)))
            } else {
                parametros$valor[parametros$parametro == "Alfa"] <- input$alfa
            }
            if (nrow(parametros[parametros$parametro == "Beta"]) == 0){
                parametros <- data.table::rbindlist(list(parametros, data.table::data.table(nome = parametros[, unique(nome)], 
                parametro = "Beta", valor = input$beta, limite_inferior = 0.001, limite_superior = 100)))
            } else {
                parametros$valor[parametros$parametro == "Beta"] <- input$beta
            }

            kt <- cria_kt(input$kt_max, input$kt_min, input$alfa, input$beta)
            parametros$valor[parametros$parametro %in% paste0("Kt", 2:-60)] <- kt
            
            parametros[, valor := as.character(valor)]

            parametros <- data.table::rbindlist(list(parametros, 
                            data.table::data.table(nome = parametros[, unique(nome)], 
                            parametro = "data_inicio_simulacao", valor = as.character(input$periodo_simulacao[1]),
                            limite_inferior = 0, limite_superior = 0)))
            parametros <- data.table::rbindlist(list(parametros, 
                            data.table::data.table(nome = parametros[, unique(nome)], 
                            parametro = "data_final_simulacao", valor = as.character(input$periodo_simulacao[2]),
                            limite_inferior = 0, limite_superior = 0)))
            parametros <- data.table::rbindlist(list(parametros, 
                            data.table::data.table(nome = parametros[, unique(nome)], 
                            parametro = "data_inicio_objetivo", valor = as.character(input$periodo_calibracao[1]),
                            limite_inferior = 0, limite_superior = 0)))
            parametros <- data.table::rbindlist(list(parametros, 
                            data.table::data.table(nome = parametros[, unique(nome)], 
                            parametro = "data_final_objetivo", valor = as.character(input$periodo_calibracao[2]),
                            limite_inferior = 0, limite_superior = 0)))
            parametros <- data.table::rbindlist(list(parametros, 
                            data.table::data.table(nome = parametros[, unique(nome)], 
                            parametro = "Ebin", valor = as.character(input$Ebin), 
                            limite_inferior = 0, limite_superior = 0)))
            parametros <- data.table::rbindlist(list(parametros, 
                            data.table::data.table(nome = parametros[, unique(nome)], 
                            parametro = "Supin", valor = as.character(input$Supin),
                            limite_inferior = 0, limite_superior = 0)))
            parametros <- data.table::rbindlist(list(parametros, 
                            data.table::data.table(nome = parametros[, unique(nome)], 
                            parametro = "Tuin", valor = as.character(input$Tuin),
                            limite_inferior = 0, limite_superior = 0)))

            if (input$numero_periodo_desconsiderado >= 1) {
                for (iperiodo in 1:input$numero_periodo_desconsiderado){
                        parametros <- data.table::rbindlist(list(parametros, 
                            data.table::data.table(nome = parametros[, unique(nome)], 
                            parametro = paste0("inicio_periodo_desconsiderado_", iperiodo), 
                            valor = as.character(input[[paste0("periodo_desconsiderado_", iperiodo)]][1]),
                            limite_inferior = 0, limite_superior = 0)))
                        parametros <- data.table::rbindlist(list(parametros, 
                            data.table::data.table(nome = parametros[, unique(nome)], 
                            parametro = paste0("final_periodo_desconsiderado_", iperiodo),
                            valor = as.character(input[[paste0("periodo_desconsiderado_", iperiodo)]][2]),
                            limite_inferior = 0, limite_superior = 0)))
                    }
            }

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

        output$download_simulacao <- shiny::downloadHandler(
            filename = function() {
                paste0("simulacao_", input$sub_bacia, ".csv")
            },
            content = function(file) {
                utils::write.table(saida(), file, quote = FALSE, row.names = FALSE, sep = ";")
            }
        )
    }

    shiny::shinyApp(ui = ui_calibracao, server = servidor_calibracao)
}