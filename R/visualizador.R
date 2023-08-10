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
#' @export

executa_visualizador_calibracao <- function(){
    `%>%` <- magrittr::`%>%`
    
    ui_calibracao <- shiny::fluidPage(
        shinyjs::useShinyjs(),
        shiny::titlePanel("Calibraçao SMAP/ONS"),
        shiny::tabsetPanel(
            shiny::tabPanel("Dados",
                shiny::fileInput(inputId = "arquivo_parametros", label = shiny::h3("Selecione o arquivo de parâmetros")),
                shiny::fileInput(inputId = "arquivo_vazao", label = shiny::h3("Selecione o arquivo de vazao")),
                shiny::fileInput(inputId = "arquivo_precipitacao", label = shiny::h3("Selecione o arquivo de precipitaçao")),
                shiny::fluidRow(
                    shiny::column(3, 
                        shiny::fileInput(inputId = "arquivo_evapotranspiracao", label = shiny::h3("Selecione o arquivo de evapotranspiraçao")),
                        shiny::fileInput(inputId = "arquivo_evapotranspiracao_nc", label = shiny::h3("Selecione o arquivo de NC de evapotranspiraçao"))
                    )
                ),
                shiny::fileInput(inputId = "arquivo_postos_plu", label = shiny::h3("Selecione o arquivo de relaçao postos plu x sub-bacias")),
                shiny::selectInput(inputId ="sub_bacia", label = shiny::h3("Selecione a sub-bacia a ser calibrada"), choices = NULL)
            ),
            shiny::tabPanel("Calibraçao",
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
                            shiny::dateRangeInput(inputId = "periodo_calibracao", label = "Periodo de calibracao", start = NULL, end = NULL, min = NULL, max = NULL)
                        ),
                    ),

                    shiny::mainPanel(
                        shiny::fluidRow(
                            dygraphs::dygraphOutput("dygraph", heigh = "600px"),
                            shiny::textOutput("funcao_objetivo"),
                            shiny::column(3,
                                shiny::actionButton(inputId = "botao_calibracao", label = "Calibrar", class = "btn-lg btn-success"),
                                shiny::checkboxGroupInput("variaveis", "variaveis", choices = c("Qsup1", "Qsup2", "Qplan", "Qbase")),
                                shiny::plotOutput("grafico_kts", width = "50%")
                            ),
                            shiny::downloadButton("download_parametros", "Download parametros_sub_bacia.csv"),
                            shiny::downloadButton("download_postos_plu", "Download postos_plu_sub_bacia.csv")
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
            vazao <- vazao_posto()
            shiny::updateNumericInput(session, "Ebin", value = vazao$valor[1] * 0.3)
            shiny::updateNumericInput(session, "Supin", value = vazao$valor[1] * 0.7)
            shiny::updateNumericInput(session, "Tuin", value = 0.3)
            if (!is.null(arquivo_parametros)) {
                vetor_modelo <- vetor_modelo()
                parametros_posto <- parametros_posto()
                postos_plu <- postos_plu()
                modelo <- new_modelo_smap_ons(parametros_posto, postos_plu[postos_plu$nome == input$sub_bacia])
                kt_max <- max(which(modelo$kt[3:1] > 0)) - 1
                kt_min <- max(which(modelo$kt[3:63] > 0)) - 1
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
                shiny::updateDateRangeInput(session, "periodo_calibracao", start = data_minimo, end = data_maximo, min = data_minimo, max = data_maximo)
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
                shiny::updateDateRangeInput(session, "periodo_calibracao", start = data_minimo, end = data_maximo, min = data_minimo, max = data_maximo)
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
                shiny::updateDateRangeInput(session, "periodo_calibracao", start = data_minimo, end = data_maximo, min = data_minimo, max = data_maximo)
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
                return(parametros[parametros$Nome == input$sub_bacia])
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
                vetor_modelo <- as.numeric(c(vetor_modelo[1:11], vetor_modelo[75:77], 5, 5))
                return(vetor_modelo)
            }
        })

        shiny::observeEvent(input$arquivo_vazao, {
            arquivo_vazao <- input$arquivo_vazao$datapath
            if (!is.null(arquivo_vazao)) {
                historico_vazao <- le_historico_verificado(arquivo_vazao)
                sub_bacias <- unique(historico_vazao$posto)
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

        output$grafico_kts <- shiny::renderPlot({
            vetor_modelo <- vetor_modelo()
            vetor_modelo[15] <- input$alfa
            vetor_modelo[16] <- input$beta
            kt_max <- input$kt_max
            kt_min <- input$kt_min
            
            kt <- cria_kt(kt_max, kt_min, vetor_modelo[15], vetor_modelo[16])
            kt <- data.table::data.table(kt)
            kt$lag <- 2:-60

            plot <- ggplot2::ggplot() +
                    ggplot2::geom_line(data = kt[which(lag %in% kt_max:-kt_min)], ggplot2::aes(y = kt, x = lag), show.legend = TRUE) + 
                    ggplot2::labs(title = "Distribuiçao dos Kts",
                                    y = "",
                                    x = "lag") +
                    ggplot2::theme_bw() +
                    ggplot2::theme(title = ggplot2::element_text(size = 20, face = "bold"),
                   axis.text = ggplot2::element_text(size = 14),
                   axis.text.x = ggplot2::element_text(size = 12, angle = 90, vjust = -0.1),
                   axis.title = ggplot2::element_text(size = 14, face = "bold"),
                   strip.text = ggplot2::element_text(size = 20),
                   legend.text = ggplot2::element_text(size = 20),
                   legend.position = 'bottom')
            print(plot)
        })

        saida <-  shiny::reactive({
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
            
            kt <- cria_kt(kt_max, kt_min, vetor_modelo[15], vetor_modelo[16])
            
            numero_dias <- nrow(evapotranspiracao)
            
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
            
            data_inicio_simulacao <- (min(precipitacao$data) + kt_min)
            data_fim_simulacao <- (max(precipitacao$data) - kt_max)
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
        })

        output$dygraph <- dygraphs::renderDygraph({
            variaveis_grafico <- c("Qcalc", input$variaveis)
            vazao <- vazao_posto()
            evapotranspiracao <- evapotranspiracao_posto()
            precipitacao <- precipitacao_posto()
            postos_plu <- postos_plu()

            precipitacao <- ponderacao_espacial(precipitacao, postos_plu[postos_plu$nome == input$sub_bacia])
            saida <- saida()
            saida <- data.table::melt(saida, id.vars = c("data"), variable.name = "variavel",
                                        value.name = "valor")
            simulacao <- xts::xts()
            for (variaveis in variaveis_grafico){
                simulacao <- cbind(simulacao, xts::xts(saida$valor[which(saida$variavel == variaveis)], order.by = saida$data[which(saida$variavel == variaveis)]))
            }
            colnames(simulacao) <- variaveis_grafico
            observacao <- xts::xts(x = vazao$valor, order.by =  vazao$data)
            colnames(observacao) <- "vazao observada"

            prec_aux <- xts::xts(x = precipitacao$valor, order.by =  precipitacao$data)
            colnames(prec_aux) <- "Precipitacao"

            dygraphs::dygraph(cbind(simulacao, observacao, prec_aux),
                            main = input$sub_bacia, ) %>%
            dygraphs::dyHighlight(highlightCircleSize = 5,
                                highlightSeriesBackgroundAlpha = 0.2,
                                hideOnMouseOut = FALSE,
                                highlightSeriesOpts = list(strokeWidth = 2)) %>% 
            dygraphs::dyRangeSelector() %>%
            dygraphs::dyAxis("y", label = "Vazao (m³/s)", independentTicks = TRUE) %>%
            dygraphs::dySeries("vazao.observada", color = "#0c2ad3") %>%
            dygraphs::dySeries("Precipitacao", stepPlot = TRUE, fillGraph = TRUE, axis = 'y2', color = "#0f610f") %>%
            dygraphs::dyAxis("y2", label = "Precipitaçao (mm)", valueRange = c(200, 0)) %>%
            dygraphs::dySeries("Qcalc", color = "red") %>%
            dygraphs::dyLegend(show = "follow")
        })

        output$tabela <- DT::renderDataTable(saida())

        output$funcao_objetivo <- shiny::renderText({
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
            data_inicio_objetivo <- input$periodo_calibracao[1]
            data_fim_objetivo <- input$periodo_calibracao[2]
            kt_max <- input$kt_max
            kt_min <- input$kt_min
            Ebin <- input$Ebin
            Tuin <- input$Tuin
            Supin <- input$Supin
            area <- area()
            vazao <- vazao_posto()
            evapotranspiracao <- evapotranspiracao_posto()
            precipitacao <- precipitacao_posto()
            postos_plu <- postos_plu()

            numero_postos_plu <- nrow(postos_plu[postos_plu$nome == input$sub_bacia])
            if (numero_postos_plu > 1) {
                for (iposto in 1: numero_postos_plu){
                    vetor_modelo[16 + iposto] <- input[[paste0("posto_plu_", iposto)]]
                    postos_plu[postos_plu$nome == input$sub_bacia]$valor[iposto] <- input[[paste0("posto_plu_", iposto)]]
                }
            }

            vetor_modelo[17:(16 + numero_postos_plu)] <- postos_plu$valor[postos_plu$nome == input$sub_bacia]

            kt <- cria_kt(kt_max, kt_min, vetor_modelo[15], vetor_modelo[16])
            
            numero_dias <- nrow(evapotranspiracao)
            
            inicializacao <- inicializacao_smap(vetor_modelo, area, Ebin, Tuin, Supin)
            
            evapotranspiracao_fo <- data.table::data.table(evapotranspiracao[which((evapotranspiracao$data >= (min(precipitacao$data) + kt_min))
                                         & (evapotranspiracao$data <= (max(precipitacao$data) - kt_max)))])
            
            vazao_fo <- vazao[which((vazao$data >= data_inicio_objetivo) & (vazao$data <= data_fim_objetivo))]

            vetor_inicializacao <- unlist(inicializacao)
            funcao_objetivo <- funcao_objetivo_calibracao(vetor_modelo, kt_min, kt_max, area, Ebin, Tuin, Supin, precipitacao,
                                                                evapotranspiracao_fo, vazao_fo, data_inicio_objetivo, data_fim_objetivo,
                                                                postos_plu[postos_plu$nome == input$sub_bacia])
            paste0("funcao objetivo = ", funcao_objetivo)
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

            area <- area()
            Ebin <- input$Ebin
            Tuin <- input$Tuin
            Supin <- input$Supin

            data_inicio_objetivo <- input$periodo_calibracao[1]
            data_fim_objetivo <- input$periodo_calibracao[2]
            kt_max <- input$kt_max
            kt_min <- input$kt_min

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

            evapotranspiracao_fo <- data.table::data.table(evapotranspiracao[which((evapotranspiracao$data >= (min(precipitacao$data) + kt_min))
                                         & (evapotranspiracao$data <= (max(precipitacao$data) - kt_max)))])

            vazao_fo <- vazao[which((vazao$data >= data_inicio_objetivo) & (vazao$data <= data_fim_objetivo))]

            # Disable the run button
            shiny::updateActionButton(session, "botao_calibracao", label = "Calibrando...aguarde")
            disable_button(TRUE)
            shinyjs::disable("botao_calibracao")
            
            # Execute the long-running function asynchronously
            par <- future::future({
                calibracao(vetor_modelo, kt_min, kt_max, area, Ebin, Tuin, Supin, precipitacao,
                                    evapotranspiracao_fo, vazao_fo, data_inicio_objetivo, data_fim_objetivo,
                                    limite_inferior, limite_superior, postos_plu[postos_plu$nome == input$sub_bacia])
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
                utils::write.csv(parametros_exportacao(), file, quote = FALSE, row.names = FALSE, sep = ";")
            }
        )

        output$download_postos_plu <- shiny::downloadHandler(
            filename = function() {
                paste0("postos_plu_", input$sub_bacia, ".csv")
            },
            content = function(file) {
                utils::write.csv(postos_plu_exportacao(), file, quote = FALSE, row.names = FALSE, sep = ";")
            }
        )
    }

    shiny::shinyApp(ui = ui_calibracao, server = servidor_calibracao)
}


##### VISUALIZADOR DE CASOS EXECUTADOS ############


executa_visualizador_previsao <- function(previsoes, assimilacao, precipitacao, funcao_objetivo, historico_vazao){
    `%>%` <- magrittr::`%>%`
    
    ui_previsao <- shiny::fluidPage(
        shinyjs::useShinyjs(),
        shiny::titlePanel("Visualizador de previsões do SMAP/ONS"),
        shiny::tabsetPanel(
            shiny::tabPanel("Previsao SMAP/ONS",
                shiny::sidebarLayout(
                    shiny::sidebarPanel(
                        shiny::fluidRow(
                            shiny::selectInput("data_caso", h3("Data do Caso"), 
                            choices = unique(previsoes[, data_caso]), selected = NULL),
                            shiny::selectInput("sub_bacia", h3("Sub-bacia"), 
                            choices = unique(previsoes[, nome]), selected = NULL)
                        )
                    ),
                    shiny::mainPanel(
                        shiny::fluidRow(
                            dygraphs::dygraphOutput("dygraph", heigh = "600px"),
                            shiny::textOutput("funcao_objetivo")
                        )
                    )
                )
            ),
            shiny::tabPanel("Tabela Previsao",
                DT::dataTableOutput("tabela_previsao")
            ),
            shiny::tabPanel("Tabela Assimilaçao",
                DT::dataTableOutput("tabela_assimilacao")
            )
        )
    )

    servidor_previsao <- function(input, output, session) {

        output$tabela_previsao <- DT::renderDataTable(previsoes)

        output$tabela_assimilacao <- DT::renderDataTable(assimilacao)

        output$dygraph <- dygraphs::renderDygraph({
            variaveis_grafico <- c("Qcalc", "Qbase")
            datas_previsao <- previsoes$data_previsao[which((previsoes$variavel == "Qcalc") & (previsoes$nome == input$sub_bacia) & (previsoes$data_caso == input$data_caso))]

            simulacao <- xts::xts()
            for (variaveis in variaveis_grafico){
                simulacao <- cbind(simulacao, xts::xts(previsoes$valor[which((previsoes$variavel == variaveis) & (previsoes$nome == input$sub_bacia) & (previsoes$data_caso == input$data_caso))], order.by = datas_previsao))
            }
            colnames(simulacao) <- variaveis_grafico
            
            datas_assimilacao <- assimilacao$data_assimilacao[which((assimilacao$variavel == variaveis) & (assimilacao$nome == input$sub_bacia) & (assimilacao$data_caso == input$data_caso))]
            
            assimilacao_aux <- xts::xts()
            for (variaveis in variaveis_grafico){
                assimilacao_aux <- cbind(assimilacao_aux, xts::xts(assimilacao$valor[which((assimilacao$variavel == variaveis) & (assimilacao$nome == input$sub_bacia) & (assimilacao$data_caso == input$data_caso))], order.by = datas_assimilacao))
            }
            colnames(assimilacao_aux) <- variaveis_grafico

            simulacao <- rbind(simulacao, assimilacao_aux)
            datas_precipitacao <- precipitacao$data_previsao[which((precipitacao$data_rodada == input$data_caso) & (precipitacao$nome == input$sub_bacia) & (precipitacao$data_previsao >= min(datas_assimilacao)) & (precipitacao$data_previsao <= max(datas_previsao)))]
            prec_aux <- xts::xts(x = precipitacao$valor[which((precipitacao$data_rodada == input$data_caso) & (precipitacao$nome == input$sub_bacia) & (precipitacao$data_previsao %in% datas_precipitacao))], order.by = datas_precipitacao)
            colnames(prec_aux) <- "Precipitacao"

            datas_vazao <- historico_vazao$data[(historico_vazao$posto == input$sub_bacia) & (historico_vazao$data <= max(datas_precipitacao)) & (historico_vazao$data >= min(datas_precipitacao))]
            vazao_observada <- xts::xts(x = historico_vazao$valor[which((historico_vazao$posto == input$sub_bacia) & (historico_vazao$data %in% datas_vazao))], order.by = datas_vazao)
            colnames(vazao_observada) <- "vazao_observada"

            dygraphs::dygraph(cbind(simulacao, prec_aux, vazao_observada),
                            main = input$sub_bacia) %>%
            dygraphs::dyHighlight(highlightCircleSize = 5,
                                highlightSeriesBackgroundAlpha = 0.2,
                                hideOnMouseOut = FALSE,
                                highlightSeriesOpts = list(strokeWidth = 2)) %>% 
            dygraphs::dyRangeSelector() %>%
            dygraphs::dyAxis("y", label = "Vazao (m³/s)", independentTicks = TRUE) %>%
            dygraphs::dySeries("vazao_observada", color = "#0c2ad3") %>%
            dygraphs::dySeries("Precipitacao", stepPlot = TRUE, fillGraph = TRUE, axis = 'y2', color = "#0f610f") %>%
            dygraphs::dyAxis("y2", label = "Precipitaçao (mm)", valueRange = c(200, 0)) %>%
            dygraphs::dySeries("Qcalc", color = "red") %>%
            dygraphs::dyEvent(min(datas_previsao), "Previsao", labelLoc = "bottom") %>%
            dygraphs::dySeries("Qbase", color = "#e4c356") %>%
            dygraphs::dyLegend(show = "follow")
        })

        output$tabela <- DT::renderDataTable(previsoes)

        output$funcao_objetivo <- shiny::renderText({
            paste0("funcao objetivo = ", funcao_objetivo$funcao_objetivo[(funcao_objetivo$nome == input$sub_bacia) & (funcao_objetivo$data_caso == input$data_caso)])
        })

    }

    shiny::shinyApp(ui = ui_previsao, server = servidor_previsao)
}
