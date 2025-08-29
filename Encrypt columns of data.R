# Options ####
# Version After CRAN release 0.1
options(scipen=9999)
# Expand memory and improve options
options(shiny.maxRequestSize = 30*1024^2)
# Create directories and load packages ####
server_address <- tempdir()
# server_address <- dirname(rstudioapi::getActiveDocumentContext()$path)
if (server_address != tempdir()) {
  private_keys_folder <- paste0(server_address, "/private_keys_folder")
  public_keys_folder <- paste0(server_address, "/public_keys_folder")
  data_storage_folder <- paste0(server_address, "/data_storage_folder")
  encryptions_keys_path <- paste0(server_address, "/encryption_keys.RDS")
  if (dir.exists(private_keys_folder) == FALSE) {dir.create(private_keys_folder)}
  if (dir.exists(public_keys_folder) == FALSE) {dir.create(public_keys_folder)}
  if (dir.exists(data_storage_folder) == FALSE) {dir.create(data_storage_folder)}
  if (file.exists(encryptions_keys_path) == FALSE) {
    encryptions_keys <- cbind.data.frame(
      unique_file_name = character(0),
      columns = character(0),
      data_type = character(0),
      order = numeric(0),
      level = character(0)
    )
    saveRDS(encryptions_keys, encryptions_keys_path)
  }
}
library("shiny")
library("shinyjs")
library("shinybusy")
library("EQUALencrypt")
# Interface ####
# Some functions and list to create the user interface
{
  ui_short_forms <- {cbind.data.frame(short_name = c("text", "numeric", "slider", "select", "checkbox", "radio", "file", "action", "date", "html", "plot", "image", "download"),
                                      long_name = c("textInput", "numericInput", "sliderInput", "selectInput", "checkboxGroupInput",
                                                    "radioButtons", "fileInput", "actionButton", "dateInput", "HTML", "plotOutput", "plotOutput", "downloadButton"),
                                      render_name = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "renderPlot", "renderImage", "downloadHandler")
  )}
  ui_short_forms$update_name <- paste0("update", str_to_title(ui_short_forms$short_name), str_remove(ui_short_forms$long_name, ui_short_forms$short_name))
  create_UI <- function(ui_text, ui_short_forms) {
    ui_text$order <- 1:nrow(ui_text)
    ui_text$additional_options <- NA
    ui_text$submit_text <- NA
    ui_text$rv_text <- paste0(ui_text$variables, " = NA")
    ui_text$additional_options[! is.na(ui_text$additional_parameters)] <- paste0(
      ", ", paste0(str_replace_all(ui_text$additional_parameters[! is.na(ui_text$additional_parameters)], ";", ",")))
    ui_text$additional_options[is.na(ui_text$additional_parameters)] <- ''
    # Input
    ui_text_input <- ui_text[ui_text$input_type %in% c("text", "numeric", "slider", "select", "checkbox", "radio", "file", "action", "date"),]
    ui_text_input$submit_text[ui_text_input$input_type == "text"] <-
      paste0("(input$", ui_text_input$variables[ui_text_input$input_type == "text"], " != '')")
    ui_text_input$submit_text[ui_text_input$input_type %in% c("numeric", "slider")] <-
      paste0("(! is.na(input$", ui_text_input$variables[ui_text_input$input_type  %in% c("numeric", "slider")], "))")
    ui_text_input$submit_text[ui_text_input$input_type %in% c("select", "checkbox", "radio")] <-
      paste0("(! TRUE %in% (input$", ui_text_input$variables[ui_text_input$input_type  %in% c("select", "checkbox", "radio")], " == ''))")
    ui_text_input$submit_text[ui_text_input$input_type %in% c("file", "action")] <-
      paste0("(! is.null(input$", ui_text_input$variables[ui_text_input$input_type  %in% c("file", "action")], "))")
    ui_text_input$submit_text[ui_text_input$input_type == "action"] <-
      paste0("(input$", ui_text_input$variables[ui_text_input$input_type  == "action"], " > 0)")
    ui_text_input$ui <- paste0("output$", ui_text_input$variables, "_UI <- renderUI(",
                               ui_short_forms$long_name[match(ui_text_input$input_type, ui_short_forms$short_name)], "('", ui_text_input$variables, "', ",
                               "'", ui_text_input$description, "'", ui_text_input$additional_options,"))")
    submit_conditions <- ui_text_input$submit_text[ui_text_input$mandatory == "yes"]
    capture_values <- paste0("if (length(input$",ui_text_input$variables,") > 0) {rv$", ui_text_input$variables, " <- input$", ui_text_input$variables, "}")
    # Output
    ui_text_output <- ui_text[! (ui_text$input_type %in% c("text", "numeric", "slider", "select", "checkbox", "radio", "file", "action", "date")),]
    if (nrow(ui_text_output) > 0) {
      ui_text_output$ui <- paste0("output$", ui_text_output$variables, "_UI <- renderUI(",
                                  ui_short_forms$long_name[match(ui_text_output$input_type, ui_short_forms$short_name)],
                                  unlist(lapply(1:nrow(ui_text_output), function(x) {
                                    if (ui_text_output$input_type[x] == "html") {
                                      paste0("(rv$", ui_text_output$variables[x],"))")
                                    } else if (ui_text_output$input_type[x] %in% c("plot", "image")) {
                                      paste0("('", ui_text_output$variables[x], "'", ui_text_output$additional_options[x],"))")
                                    } else if (ui_text_output$input_type[x] == "download"){
                                      paste0("('", ui_text_output$variables[x], "','", ui_text_output$description[x],"'))")
                                    }
                                  }))
      )
      ui_text_output$rv_text <- paste0(ui_text_output$variables, " = NULL")
      ui_text_output$rv_text[((ui_text_output$input_type == "html") & (! is.na(ui_text_output$description)))] <-
        paste0(ui_text_output$variables[((ui_text_output$input_type == "html") & (! is.na(ui_text_output$description)))], " = '", ui_text_output$description[((ui_text_output$input_type == "html") & (is.na(ui_text_output$description)))], "'")
      ui_text_extra_output <- ui_text_output[ui_text_output$input_type %in% c("plot", "image", "download"),]
      if (nrow(ui_text_extra_output) > 0) {
        ui_text_extra_output$order <- ui_text_extra_output$order + 0.5
        ui_text_extra_output$ui <- paste0(
          "if (! is.null(", paste0("rv$", ui_text_extra_output$variables), ")) {",
          "output$", ui_text_extra_output$variables, " <- ",
          ui_short_forms$render_name[
            match(ui_text_extra_output$input_type, ui_short_forms$short_name)],
          "(",
          unlist(lapply(1:nrow(ui_text_extra_output), function(x) {
            if (ui_text_extra_output$input_type[x] == "html") {
              output <- NA
            } else if (ui_text_extra_output$input_type[x] == "plot") {
              output <- paste0("rv$", ui_text_extra_output$variables[x])
              split_information <- unlist(str_split(ui_text_extra_output$additional_parameters_2[x], "; " ))
              if (length(split_information) > 0) {
                output <- paste0(output, ", ", paste0(split_information[2:length(split_information)], collapse = ", "))
              }
            } else if (ui_text_extra_output$input_type[x] == "image") {
              output <- paste0("list(src = rv$", ui_text_extra_output$variables[x])
              split_information <- unlist(str_split(ui_text_extra_output$additional_parameters_2[x], "; " ))
              if (length(split_information) > 0) {
                output <- paste0(output, ", ", paste0(split_information[2:length(split_information)], collapse = ", "))
              }
              output <- paste0(output, "), deleteFile = FALSE")
            } else if (ui_text_extra_output$input_type[x] == "download") {
              output <- paste0(
                "filename = '",
                paste0(str_remove(ui_text_extra_output$variables[x], "download_"), ".",
                       ui_text_extra_output$additional_parameters_2[x]),
                "', content = function(file) {file.copy(rv$",ui_text_extra_output$variables[x],
                ", file)}"
              )
            }
            return(output)
          })),
          ")}"
        )
        ui_text_output <- rbind.data.frame(ui_text_output, ui_text_extra_output)
      }
    }
    ui_text_merged <- rbind.data.frame(ui_text_input, ui_text_output)
    ui_text_merged <- ui_text_merged[order(ui_text_merged$order),]
    output <- list(ui = ui_text_merged$ui,
                   submit_conditions = submit_conditions,
                   capture_values = capture_values,
                   rv_text = unique(ui_text_merged$rv_text)
    )
  }
}
# UI text and create UI
{
  ui_text <- {cbind.data.frame(
    variables = c(
      "restart", "html_message",
      "encryption_decryption", "submit_encryption_decryption", "reset_encryption_decryption",
      "file_upload_encrypt", "level_1", "level_2", "level_3", "level_4", "level_5",
      "level_6", "level_7", "submit_encryption", "reset_encryption", "download_encrypted_file",
      "file_upload_decrypt", "public_keys_upload", "private_keys_upload",  "submit_decryption", "reset_decryption", "download_decrypted_file"
    ),
    description = c("Encrypt or decrypt one more file",
                    "Message",
                    "Select whether you want to encrypt a new file or decrypt columns in a database created using this program",
                    "Submit your choice",
                    "Reset your choice",
                    "Upload the file to be encrypted",
                    "Select the columns that should be available for people with level 1 access",
                    "Select the columns that should be available for people with level 2 access",
                    "Select the columns that should be available for people with level 3 access",
                    "Select the columns that should be available for people with level 4 access",
                    "Select the columns that should be available for people with level 5 access",
                    "Select the columns that should be available for people with level 6 access",
                    "Select the columns that should be available for people with level 7 access",
                    "Submit your choices",
                    "Reset your choices",
                    "Download the encrypted file",
                    "Upload the zipped file that contains the main content",
                    "Upload the zipped file that contains the public keys",
                    "Upload the zipped file that contains the private keys",
                    "Submit your choices",
                    "Reset your choices",
                    "Download the decrypted file"
    ),
    mandatory = c("no", "no",
                  "yes", "no", "no",
                  "no", "no", "no", "no", "no", "no",
                  "no", "no", "no", "no", "no",
                  "no", "no", "no", "no", "no", "no"
    ),
    input_type = c("action", "html",
                   "select", "action", "action",
                   "file", "checkbox", "checkbox", "checkbox", "checkbox", "checkbox",
                   "checkbox", "checkbox", "action", "action", "download",
                   "file", "file", "file", "action", "action", "download"

    ),
    additional_parameters = c(NA, NA,
                              "choices = c('', 'encryption', 'decryption'); width = '90%'",
                              NA, NA,
                              "accept = '.csv'; width = '90%'",
                              "choices = colnames(uploaded_file); inline = TRUE; width = '90%'",
                              "choices = setdiff(colnames(uploaded_file), c(input$level_1)); inline = TRUE; width = '90%'",
                              "choices = setdiff(colnames(uploaded_file), c(input$level_1, input$level_2)); inline = TRUE; width = '90%'",
                              "choices = setdiff(colnames(uploaded_file), c(input$level_1, input$level_2, input$level_3)); inline = TRUE; width = '90%'",
                              "choices = setdiff(colnames(uploaded_file), c(input$level_1, input$level_2, input$level_3, input$level_4)); inline = TRUE; width = '90%'",
                              "choices = setdiff(colnames(uploaded_file), c(input$level_1, input$level_2, input$level_3, input$level_4, input$level_5)); inline = TRUE; width = '90%'",
                              "choices = setdiff(colnames(uploaded_file), c(input$level_1, input$level_2, input$level_3, input$level_4, input$level_5, input$level_6)); inline = TRUE; width = '90%'",
                              NA, NA, NA,
                              "accept = '.zip'; width = '90%'",
                              "accept = '.zip'; width = '90%'",
                              "accept = '.zip'; width = '90%'",
                              NA, NA, NA),
    additional_parameters_2 = c(NA, NA,
                                NA, NA, NA,
                                NA, NA, NA, NA, NA, NA,
                                NA, NA, NA, NA, "zip",
                                NA, NA, NA, NA, NA, "csv")
  )}
  main_panel_display_fields <- paste0("uiOutput(outputId = '", ui_text$variables, "_UI')")
  main_panel_display <- paste0("fluidRow(", paste0("eval(parse(text = main_panel_display_fields[",1:length(main_panel_display_fields),"]))", collapse = ",\n"), ")")
  start_submit_text <- create_UI(ui_text, ui_short_forms)
  start_text <- start_submit_text[[1]]
  check_text <- paste0("(", paste0(start_submit_text[[2]], collapse = " & "), ")")
  capture_text <- paste0(start_submit_text[[3]], collapse = "\n")
  required_reactive_values <- paste0("rv <- reactiveValues(",paste0(start_submit_text[[4]], collapse = ", "), ")")
  all_ui_null <- paste0("output$", ui_text$variables, "_UI <- NULL")
  all_rv_reset <- paste0("rv$", start_submit_text[[4]])
}
# Instructions
Instructions <- {paste0(
  "<h2>Instructions</h2>",
  "<h3>General comments</h3>",
  "<ol start = 1>",
  "<li>This program has been created for sharing research data which are in tabular format and which requires different access levels for different people.</li>",
  "<li>This program is undergoing testing. Therefore, the user must use this software at their own risk.</li>",
  "<li>This program must not be used for any unlawful purposes.</li>",
  "<li>This program encrypts ASCII printable characters only.</li>",
  "</ol>",
  "<h3>Initial screen</h3>",
  "<p>In the initial screen, choose whether you want to encrypt a new file or decrypt a file that you encrypted using this program and click on submit.</p>",
  "<h3>Encrypt a new file</h3>",
  "<ol start = 1>",
  "<li>The only accepted format for encrypting a file is 'csv' file format.</li>",
  "<li>Select the columns that people with each access level can view.</li>",
  "<li>The columns which have not been selected at any access level will be unecrypted and will be available to people with any level of access.</li>",
  "<li>People with higher access level will also be able to view the columns that people with lower access level can view. ",
  "For example, a person with access level 4 will be able to view the unencrypted columns and the columns that people with access levels 1 to 3 can view in addition to access level 4 that they belong to.</li>",
  "</ol>",
  "<h3>Sharing encrypted data</h3>",
  "<ol start = 1>",
  "<li>Once the file is encrypted, a message that the encryption was successful will appear.</li>",
  "<li>Download the encrypted file by clicking on 'Download the encrypted file'.</li>",
  "<li>The downloaded file will be a 'zip' file and contains two 'zip' files ('not_publicly_shareable.zip' and 'publicly_shareable,zip').</li>",
  "<li>The access levels are from 0 to 7. Level 0 refers to the content that anyone can view, level 1 refers to the content that people with access level 1 can view",
  "level 2 refers to the content that people with access level 2 can view, and so on.</li>",
  "<li>The 'not_publicly_shareable.zip' contains multiple 'zip' files containing the private keys for different levels of access. This file should never be shared publicly. ",
  "However, this should be retained securely as without these private keys, one cannot view the content. ",
  "You must share the private keys corresponding to the person's access level confidentially to allow them to view the encrypted content.</li>",
  "<li>The 'publicly_shareable.zip' contains multiple 'zip' files containing the main content and public keys for different levels of access. ",
  "This file can be shared publicly as without the private keys no one can view the content. ",
  "We recommend that you share the public keys for all levels of access publicly to prevent and detect data corruption. ",
  "However, you can choose to share the main content specific to a person's access level along with the private keys confidentially in separate emails or using different methods, for example, share a link of the encrypted file by one drive or google drive and share the private key by email.</li>",
  "</ol>",
  "<h3>Decryption of data</h3>",
  "<ol start = 1>",
  "<li>You must upload the main content, the public key, and the private key specific to the access level.</li>",
  "<li>If the private key matches the main content, the content corresponding to the access level will be decrypted.</li>",
  "<li>If there is no content corresponding (up)to the level of access, you an error message will be displayed.</li>",
  "<li>If there was content corresponding (up)to the level of access and the correct content was provided, a message about successful encryption will be displayed.</li>",
  "<li>In such a case, download the decrypted file by clicking on 'Download the decrypted file'.</li>",
  "<li>The downloaded file will be a 'csv' file, which can be opened with Excel or Google sheet.</li>",
  "</ol>"
)}
# User interface
ui <- {fluidPage(
  # Some parameters for web page
  shinyjs::useShinyjs(),
  add_busy_spinner(spin = "fading-circle"),
  {tags$head(
    tags$style(HTML('
        body {background-color: aliceblue;color: black;}
        p {text-align: left; margin-top: 0px; margin-bottom: 0px;line-height: 1.6;font-family:Sans-Serif}
        h1 {text-align: center;font-family:arial bold;}
        h2 {text-align: left; margin-top: 6px;font-family:arial bold; color: maroon; font-weight: bold; font-size: 28px}
        h3 {text-align: left; margin-top: 6px;font-family:arial bold; color: black; font-weight: bold; font-size: 20px}
        h4 {text-align: left; margin-top: 6px;font-family:arial bold; color: green; background-color: yellow; font-weight: bold; font-size: 28px}
        h5 {text-align: left; margin-top: 6px;font-family:arial bold; color: maroon; background-color: yellow; font-weight: bold; font-size: 28px}
#submit_encryption_decryption{background-color:darkgreen; text-align:center; font-size: 26px; font-family:arial bold; color: white;}
#submit_encryption{background-color:darkgreen; text-align:center; font-size: 26px; font-family:arial bold; color: white;}
#submit_decryption{background-color:darkgreen; text-align:center; font-size: 26px; font-family:arial bold; color: white;}
#reset_encryption_decryption{background-color:maroon; text-align:center; font-size: 26px; font-family:arial bold; color: white;}
#reset_encryption{background-color:maroon; text-align:center; font-size: 26px; font-family:arial bold; color: white;}
#reset_decryption{background-color:maroon; text-align:center; font-size: 26px; font-family:arial bold; color: white;}
#restart{background-color:purple; text-align:center; font-size: 26px; font-family:arial bold; color: white;}
#download_encrypted_file{background-color:darkblue; text-align:center; font-size: 26px; font-family:arial bold; color: white;}
#download_decrypted_file{background-color:darkblue; text-align:center; font-size: 26px; font-family:arial bold; color: white;}
')))
  },
  # Title panel
  {fluidRow(headerPanel(div(
    column(width = 12, HTML('<h1 style = "color:white; background-color:darkblue; font-family:arial bold;"><b>Encrypt and decrypt data columns</b></h1>')),
  )))},
  # Side bar
  sidebarLayout(
    # Side panel for instructions ####
    sidebarPanel(HTML(Instructions)),
    # Main panel for input and output ####
    mainPanel(
      # Name ####
      fluidRow(
        column(width = 12, HTML('<h2 style = "color:#254636; background-color: aliceblue; text-align: left; font-family:arial bold;"><b>Evidence-Based Healthcare: Best Information for Best Practice</b></h2>')),
        column(width = 12, HTML('<h3 style = "color:#254636; background-color: aliceblue; text-align: left; font-family:arial bold;"><b>Developed by: </b><a href="https://profiles.ucl.ac.uk/11524-kurinchi-gurusamy" target="_blank"><i>Professor Kurinchi Gurusamy, University College London</i></a></h3>')),
        column(width = 12, HTML('<h2 style = "color:#254636; background-color: aliceblue; text-align: left; font-family:arial bold;"><b>EQUity through biomedicAL research (EQUAL) group</b></h2>')),
      ),
      # Main information ####
      eval(parse(text = main_panel_display)),
    ),
  ),
)
}
# Server
server <- function(input, output, session) {
  eval(parse(text = required_reactive_values))
  eval(parse(text = start_text[2:5]))
  observeEvent(input$restart, {
    if (! is.null(input$restart)) {
      eval(parse(text = all_rv_reset))
      eval(parse(text = all_ui_null))
      unlink(list.files(tempdir(), full.names = TRUE))
      eval(parse(text = start_text[2:5]))
    }
  })
  observeEvent(input$submit_encryption_decryption, {
    if (! is.null(input$submit_encryption_decryption)) {
      rv$html_message = ''
      rv$submit_encryption_decryption <- ((! TRUE %in% (input$encryption_decryption == '')))
      if (rv$submit_encryption_decryption == TRUE) {
        eval(parse(text = capture_text))
        eval(parse(text = all_ui_null))
        if (rv$encryption_decryption == "encryption") {
          rv$html_message = ''
          eval(parse(text = start_text[c(1,2,6)]))
        } else {
          rv$html_message = ''
          eval(parse(text = start_text[c(1,2, 18:22)]))
        }
      } else {
        rv$html_message = "<h5>Choosing whether encryption or decryption is required is mandatory. Please enter this information before submission.</h5>"
      }
    }
  })
  observeEvent(input$reset_encryption_decryption, {
    if (! is.null(input$reset_encryption_decryption)) {
      eval(parse(text = all_rv_reset))
      eval(parse(text = all_ui_null))
      eval(parse(text = start_text[2:5]))
    }
  })
  observeEvent(input$file_upload_encrypt, {
    if (length(input$file_upload_encrypt) > 0) {
      if (! is.null(input$file_upload_encrypt)) {
        if (! FALSE %in% (input$file_upload_encrypt != "")) {
          rv$html_message = ''
          rv$file_upload_encrypt <- input$file_upload_encrypt
          uploaded_file <- read.csv(rv$file_upload_encrypt$datapath, check.names = FALSE,
                                    na.strings = c("", " ", "  "))
          colnames(uploaded_file) <- iconv(colnames(uploaded_file), 'latin1', 'ASCII', sub = '')
          colnames(uploaded_file)[colnames(uploaded_file) == ""] <- "Missing column names"
          eval(parse(text = all_ui_null))
          eval(parse(text = start_text[c(1,2,7:15)]))
        } else {
          rv$html_message = "<h5>A file must be uploaded for encryption.</h5>"
        }
      }
    }
  })
  observeEvent(input$submit_encryption, {
    if (! is.null(input$submit_encryption)) {
      eval(parse(text = paste0('disable("', ui_text$variables[1:15],'")')))
      eval(parse(text = capture_text))
      results <- EQUAL_perform_data_encryption(rv = rv, server_address = server_address)
      rv$html_message = results[[1]]
      rv$download_encrypted_file <- results[[2]]
      eval(parse(text = all_ui_null))
      eval(parse(text = start_text[c(1,2,16:17)]))
    }
  })
  observeEvent(input$reset_encryption, {
    if (! is.null(input$reset_encryption)) {
      uploaded_file <- read.csv(rv$file_upload_encrypt$datapath, check.names = FALSE,
                                na.strings = c("", " ", "  "))
      colnames(uploaded_file) <- iconv(colnames(uploaded_file), 'latin1', 'ASCII', sub = '')
      colnames(uploaded_file)[colnames(uploaded_file) == ""] <- "Missing column names"
      eval(parse(text = all_rv_reset))
      eval(parse(text = all_ui_null))
      eval(parse(text = start_text[c(1,2,7:15)]))
    }
  })
  observeEvent(input$submit_decryption, {
    if (! is.null(input$submit_decryption)) {
      eval(parse(text = capture_text))
      eval(parse(text = paste0('disable("', ui_text$variables[c(1,2, 18:22)],'")')))
      results <- try(EQUAL_perform_data_decryption(rv), silent = TRUE)
      if (TRUE %in% (class(results) == "try-error")) {
        eval(parse(text = all_rv_reset))
        eval(parse(text = all_ui_null))
        eval(parse(text = start_text[c(1,2,18:22)]))
        rv$html_message = paste0("<h5>The main content and private keys of the encrypted file are mandatory and must be uploaded correctly. ", 
                                 "Please check whether there is any content in any of the zipped files and upload these files correctly before submission.</h5>")
      } else {
        rv$html_message = results[[1]]
        rv$download_decrypted_file <- results[[2]]
        eval(parse(text = all_ui_null))
        if (! is.null(rv$download_decrypted_file)) {
          eval(parse(text = start_text[c(1,2,23,24)]))
        } else {
          eval(parse(text = start_text[c(1,2)]))
        }
      }
    }
  })
  observeEvent(input$reset_decryption, {
    if (! is.null(input$reset_decryption)) {
      eval(parse(text = all_rv_reset))
      eval(parse(text = all_ui_null))
      eval(parse(text = start_text[c(1,2,18:22)]))
    }
  })
}
# Run the application ####
shinyApp(ui = ui, server = server)
