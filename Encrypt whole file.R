# Options ####
# Version 0.4 Pre-release version
options(scipen=9999)
# Expand memory and improve options
options(shiny.maxRequestSize = 30*1024^2)
# Functions ####
EQUAL_encrypt_generate_keys <- function(public_key_folder, private_key_folder, key_name) {
  public_key_name <- paste0(public_key_folder, "/", key_name)
  private_key_name <- paste0(private_key_folder, "/", key_name)
  key <- openssl::rsa_keygen(bits = 4096)
  pubkey <- as.list(key)$pubkey
  openssl::write_pem(key, private_key_name)
  openssl::write_pem(pubkey, public_key_name)
  output <- list(private_key = key, public_key = pubkey)
}
EQUAL_encrypt_file <- function(file_name, public_key_folder, key_name) {
  temp_public_key <- paste0(public_key_folder, "/", key_name)
  aes_key <- rand_bytes(32)
  iv <- rand_bytes(16)
  aes_encrypted_file <- aes_cbc_encrypt(file_name, key = aes_key, iv = iv)
  rsa_encrypted_aes_key <- rsa_encrypt(aes_key, pubkey = temp_public_key, oaep = TRUE)
  encrypted_file <- list(iv = iv, session = rsa_encrypted_aes_key, data = aes_encrypted_file)
}
EQUAL_decrypt_file <- function(encrypted_data, private_key_folder, key_name, data_storage_folder) {
  temp_private_key <- paste0(private_key_folder, "/", key_name)
  decrypted_aes_key <- rsa_decrypt(data = encrypted_data$session, key = read_key(temp_private_key), oaep = TRUE)
  decrypted_data <- aes_cbc_decrypt(data = encrypted_data$data, key = decrypted_aes_key, iv = encrypted_data$iv)
  binary_file <- file(paste0(data_storage_folder, "/decrypted_file.zip"), "wb")
  writeBin(decrypted_data, con = binary_file)
  close(binary_file)
}
EQUAL_insert_signature_file <- function(file_name, private_key_folder, key_name) {
  private_key_name <- paste0(private_key_folder, "/", key_name)
  openssl::signature_create(file_name, hash = sha384, key = read_key(private_key_name))
}
EQUAL_verify_signature <- function(file_name, signature, key_name, public_key_folder) {
  public_key_name <- paste0(public_key_folder, "/", key_name)
  verification <- suppressWarnings(try(
    openssl::signature_verify(file_name, sig = signature, hash = sha384, pubkey = read_pubkey(public_key_name))
    , silent = TRUE))
  ! (TRUE %in% (class(verification) == "try-error"))
}
EQUAL_perform_file_encryption <- function(rv, server_address = tempdir()) {
  temp_private_keys_folder <- paste0(tempdir(), "/private_keys_folder")
  temp_public_keys_folder <- paste0(tempdir(), "/public_keys_folder")
  temp_data_storage_folder <- paste0(tempdir(), "/data_storage_folder")
  if (dir.exists(temp_private_keys_folder) == TRUE) {unlink(temp_private_keys_folder, recursive = TRUE)}
  if (dir.exists(temp_public_keys_folder) == TRUE) {unlink(temp_public_keys_folder, recursive = TRUE)}
  if (dir.exists(temp_data_storage_folder) == TRUE) {unlink(temp_data_storage_folder, recursive = TRUE)}
  dir.create(temp_private_keys_folder)
  dir.create(temp_public_keys_folder)
  dir.create(temp_data_storage_folder)
  keys <- EQUAL_encrypt_generate_keys(
    public_key_folder = temp_public_keys_folder, private_key_folder = temp_private_keys_folder,
    key_name = "encryption_key.txt"
  )
  file_name <- paste0(temp_data_storage_folder, "/uploaded_file.zip")
  zip::zip(file_name, files = rv$file_upload_encrypt$datapath, mode = "cherry-pick")
  saveRDS(EQUAL_encrypt_file(file_name = file_name, public_key_folder = temp_public_keys_folder, 
                                       key_name = "encryption_key.txt"),
          paste0(temp_data_storage_folder, "/encrypted_file.RDS"))
  
  saveRDS(EQUAL_insert_signature_file(file_name = paste0(temp_data_storage_folder, "/encrypted_file.RDS"), private_key_folder = temp_private_keys_folder,
                                      key_name = "encryption_key.txt"), paste0(temp_data_storage_folder, "/signature.RDS"))
  file.rename(paste0(temp_public_keys_folder, "/encryption_key.txt"),
              paste0(temp_public_keys_folder, "/public_encryption_key.txt"))
  zipped_content_publicly_shareable <- zip::zip(
    paste0(temp_data_storage_folder, "/publicly_shareable.zip"),
    files = c(paste0(temp_data_storage_folder, "/encrypted_file.RDS"),
              paste0(temp_public_keys_folder, "/public_encryption_key.txt"),
              paste0(temp_data_storage_folder, "/signature.RDS")
              ), 
    mode = "cherry-pick")
  file.rename(paste0(temp_private_keys_folder, "/encryption_key.txt"),
              paste0(temp_private_keys_folder, "/private_encryption_key.txt"))
  zipped_content_not_publicly_shareable <- zip::zip(
    paste0(temp_data_storage_folder, "/not_publicly_shareable.zip"),
    files = paste0(temp_private_keys_folder, "/private_encryption_key.txt"), 
    mode = "cherry-pick")
  zip_all_encrypted_content <- zip::zip(
    paste0(temp_data_storage_folder, "/all_encrypted_content.zip"),
    files = c(paste0(temp_data_storage_folder,"/publicly_shareable.zip"),
              paste0(temp_data_storage_folder, "/not_publicly_shareable.zip")
    ), 
    mode = "cherry-pick")
  if (server_address != tempdir()) {
    silencer <- file.copy(list.files(path = temp_private_keys_folder, full.names = TRUE), private_keys_folder)
    silencer <- file.copy(list.files(path = temp_public_keys_folder, full.names = TRUE), public_keys_folder)
    silencer <- file.copy(paste0(temp_data_storage_folder,"/all_encrypted_content.zip"), 
                          paste0(data_storage_folder, "/encrypted_content", str_remove_all(Sys.time(), "[^a-zA-Z0-9]"), ".zip"))
  }
  silencer <- file.copy(paste0(temp_data_storage_folder, "/all_encrypted_content.zip"), 
                        paste0(tempdir(), "/encrypted_content.zip"), overwrite = TRUE)
  unlink(temp_private_keys_folder, recursive = TRUE)
  unlink(temp_public_keys_folder, recursive = TRUE)
  unlink(temp_data_storage_folder, recursive = TRUE)
  if (file.exists(paste0(tempdir(), "/data.RDS"))) {file.remove(paste0(tempdir(), "/data.RDS"))}
  output <- list(html_message = "<h4>Encryption successfully completed. The encrypted files can be downloaded by clicking on the 'Download the encrypted file' button. Please see the instructions on what should be shared and what should not be shared.</h4>",
                 encrypted_file_path = paste0(tempdir(), "/encrypted_content.zip")) 
  return(output)
}
EQUAL_perform_file_decryption <- function(rv) {
  temp_data_storage_folder <- paste0(tempdir(), "/data_storage_folder")
  if (dir.exists(temp_data_storage_folder) == TRUE) {unlink(temp_data_storage_folder, recursive = TRUE)}
  dir.create(temp_data_storage_folder)
  signature_present <- EQUAL_verify_signature(file_name = rv$file_upload_decrypt$datapath, 
                                              signature = readRDS(rv$signature_upload$datapath),
                                              public_key_folder = dirname(rv$public_keys_upload$datapath),
                                              key_name = basename(rv$public_keys_upload$datapath))  
  if (signature_present == FALSE) {
    output <- list(html_message = "<h5>The signature could not be verified. This may be due to data corruption. Please download from the original source.</h5>",
                   decrypted_file_path = NULL)
    unlink(temp_data_storage_folder, recursive = TRUE)
    return(output)
  } else {
    decrypted_file <- try(
      EQUAL_decrypt_file(encrypted_data = readRDS(rv$file_upload_decrypt$datapath),
                         private_key_folder = dirname(rv$private_keys_upload$datapath),
                         key_name = basename(rv$private_keys_upload$datapath),
                         data_storage_folder = temp_data_storage_folder),
      silent = TRUE)
    if (TRUE %in% (class(decrypted_file) == "try-error")) {
      output <- list(html_message = "<h5>The private key or signature were incorrect or not provided. Please contact the data provider for the correct private key and signature.</h5>",
                     decrypted_file_path = NULL)
    } else {
      file.copy(paste0(temp_data_storage_folder, "/decrypted_file.zip"), tempdir(), overwrite = TRUE)
      zip::unzip(zipfile = paste0(temp_data_storage_folder, "/decrypted_file.zip"),
            exdir = paste0(temp_data_storage_folder, "/decrypted_file_folder"))
      output <- list(html_message = "<h4>The file has been successfully decrypted. Please download the decrypted file by clicking on 'Download the decrypted file' option.</h4>",
                     decrypted_file_path = paste0(tempdir(), "/decrypted_file.zip"))
    }
    # unlink(temp_data_storage_folder, recursive = TRUE)
    return(output)
  }
}
# Create directories and load packages ####
server_address <- tempdir()
# server_address <- dirname(rstudioapi::getActiveDocumentContext()$path)
if (server_address != tempdir()) {
  private_keys_folder <- paste0(server_address, "/private_keys_folder")
  public_keys_folder <- paste0(server_address, "/public_keys_folder")
  data_storage_folder <- paste0(server_address, "/data_storage_folder")
  if (dir.exists(private_keys_folder) == FALSE) {dir.create(private_keys_folder)}
  if (dir.exists(public_keys_folder) == FALSE) {dir.create(public_keys_folder)}
  if (dir.exists(data_storage_folder) == FALSE) {dir.create(data_storage_folder)}
}
library("shiny")
library("openssl")
library("stringr")
library("shinyjs")
library("shinybusy")
library("zip")
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
      "file_upload_encrypt", "submit_encryption", "reset_encryption", "download_encrypted_file",
      "file_upload_decrypt", "signature_upload", "public_keys_upload", "private_keys_upload",  "submit_decryption", "reset_decryption", "download_decrypted_file"
    ),
    description = c("Encrypt or decrypt one more file",
                    "Message", 
                    "Select whether you want to encrypt a new file or decrypt columns in a database created using this program", 
                    "Submit your choice", 
                    "Reset your choice", 
                    "Upload the file to be encrypted", 
                    "Submit your choices", 
                    "Reset your choices", 
                    "Download the encrypted file", 
                    "Upload the encrypted file (RDS format)", 
                    "Upload the signature (RDS format)", 
                    "Upload the text file that contains the public keys", 
                    "Upload the text file that contains the private keys", 
                    "Submit your choices", 
                    "Reset your choices", 
                    "Download the decrypted file"
    ),
    mandatory = c("no", "no", 
                  "yes", "no", "no", 
                  "no", "no", "no", "no", 
                  "no", "no", "no", "no", "no", "no", "no"
    ),
    input_type = c("action", "html", 
                   "select", "action", "action", 
                   "file", "action", "action", "download",
                   "file", "file", "file", "file", "action", "action", "download"
                   
    ),
    additional_parameters = c(NA, NA,
                              "choices = c('', 'encryption', 'decryption'); width = '90%'", 
                              NA, NA,
                              "width = '90%'",
                              NA, NA, NA,
                              "accept = '.RDS'; width = '90%'",
                              "accept = '.RDS'; width = '90%'",
                              "accept = '.txt'; width = '90%'",
                              "accept = '.txt'; width = '90%'",
                              NA, NA, NA),
    additional_parameters_2 = c(NA, NA, 
                                NA, NA, NA, 
                                NA, NA, NA, "zip",
                                NA, NA, NA, NA, NA, NA, "zip")
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
  "<li>This program encrypts only whole files.</li>",
  "</ol>",
  "<h3>Initial screen</h3>",
  "<p>In the initial screen, choose whether you want to encrypt a new file or decrypt a file that you encrypted using this program and click on submit.</p>",
  "<h3>Encrypt a new file</h3>",
  "<p>You can encrypt only one file at a time.</p>",
  "<h3>Sharing encrypted data</h3>",
  "<ol start = 1>",
  "<li>Once the file is encrypted, a message that the encryption was successful will appear.</li>",
  "<li>Download the encrypted file by clicking on 'Download the encrypted file'.</li>",
  "<li>The downloaded file will be a 'zip' file and contains two 'zip' files ('not_publicly_shareable.zip' and 'publicly_shareable,zip').</li>",
  "<li>The 'not_publicly_shareable.zip' contains a single 'txt' file which is the private key specific for the encrypted file. This file should never be shared publicly. ", 
  "However, this should be retained securely as without these private keys, one cannot decrypt the file. ", 
  "You must share the private keys confidentially with people to allow them to view the encrypted content.</li>",
  "<li>The 'publicly_shareable.zip' contains two 'RDS' files (the 'encrypted file', the 'signature' which verifies that the 'encrypted file' has not been corrupted), and one 'txt' file which represents the public key, also required for verifying whether the 'encrypted file' has been corrupted. ",
  "These files can be shared publicly as without the private key no one can view the content. ",
  "We recommend that you share the public key and the signature publicly to prevent and detect data corruption. ",
  "However, you can choose to share the 'encrypted file' to a person specifically along with the private keys confidentially in separate emails or using different methods, for example, share a link of the encrypted file by one drive or google drive and share the private key by email.</li>",
  "</ol>",
  "<h3>Decryption of data</h3>",
  "<ol start = 1>",
  "<li>You must upload the encrypted file, the signature, the public key, and the private key specific to the file.</li>",
  "<li>If the all the components match, the content will be decrypted.</li>",
  "<li>Download the decrypted file by clicking on 'Download the decrypted file'.</li>",
  "<li>The downloaded file will be a 'zip' file. The decrypted content can be viewed when you unzip the downloaded file.</li>",
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
    column(width = 12, HTML('<h1 style = "color:white; background-color:darkblue; font-family:arial bold;"><b>Encrypt and decrypt files</b></h1>')),
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
          eval(parse(text = start_text[c(1,2,6:8)]))
        } else {
          rv$html_message = ''
          eval(parse(text = start_text[c(1,2, 11:16)]))
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
  observeEvent(input$submit_encryption, {
    if (! is.null(input$submit_encryption)) {
      eval(parse(text = paste0('disable("', ui_text$variables[1:8],'")')))
      eval(parse(text = capture_text))
      results <- try(EQUAL_perform_file_encryption(rv = rv, server_address = server_address), silent = TRUE)
      if (TRUE %in% (class(results) == "try-error")) {
        eval(parse(text = all_rv_reset))
        eval(parse(text = all_ui_null))
        eval(parse(text = start_text[c(1,2,6:8)]))
        rv$html_message = "<h5>A file to be encrypted is a mandatory requirement. Please upload a file correctly before submission.</h5>"
      } else {
        rv$html_message = results[[1]]
        rv$download_encrypted_file <- results[[2]]
        eval(parse(text = all_ui_null))
        eval(parse(text = start_text[c(1,2,9:10)]))
      }
    }
  })
  observeEvent(input$reset_encryption, {
    eval(parse(text = all_rv_reset))
    eval(parse(text = all_ui_null))
    eval(parse(text = start_text[c(1,2,6:8)]))
  })
  observeEvent(input$submit_decryption, {
    if (! is.null(input$submit_decryption)) {
      eval(parse(text = capture_text))
      eval(parse(text = paste0('disable("', ui_text$variables[c(1,2, 11:16)],'")')))
      results <- try(EQUAL_perform_file_decryption(rv), silent = TRUE)
      if (TRUE %in% (class(results) == "try-error")) {
        eval(parse(text = all_rv_reset))
        eval(parse(text = all_ui_null))
        eval(parse(text = start_text[c(1,2,11:16)]))
        rv$html_message = "<h5>The encrypted file, signature, public, and private keys are mandatory and must be uploaded correctly. Please upload these files correctly before submission.</h5>"
      } else {
        rv$html_message = results[[1]]
        rv$download_decrypted_file <- results[[2]]
        eval(parse(text = all_ui_null))
        if (! is.null(rv$download_decrypted_file)) {
          eval(parse(text = start_text[c(1,2,17,18)]))
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
      eval(parse(text = start_text[c(1,2,11:16)]))
    }
  })
}
# Run the application ####
shinyApp(ui = ui, server = server)