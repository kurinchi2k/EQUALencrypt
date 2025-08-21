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
