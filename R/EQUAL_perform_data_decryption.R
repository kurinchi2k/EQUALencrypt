EQUAL_perform_data_decryption <- function(rv) {
  temp_private_keys_folder <- paste0(tempdir(), "/private_keys_folder")
  temp_public_keys_folder <- paste0(tempdir(), "/public_keys_folder")
  temp_data_storage_folder <- paste0(tempdir(), "/data_storage_folder")
  if (dir.exists(temp_private_keys_folder) == TRUE) {unlink(temp_private_keys_folder, recursive = TRUE)}
  if (dir.exists(temp_public_keys_folder) == TRUE) {unlink(temp_public_keys_folder, recursive = TRUE)}
  if (dir.exists(temp_data_storage_folder) == TRUE) {unlink(temp_data_storage_folder, recursive = TRUE)}
  dir.create(temp_private_keys_folder)
  dir.create(temp_public_keys_folder)
  dir.create(temp_data_storage_folder)
  uploaded_file <- zip::unzip(rv$file_upload_decrypt$datapath, exdir = temp_data_storage_folder)
  uploaded_file <- list.files(temp_data_storage_folder, full.names = TRUE)
  private_keys_file <- zip::unzip(rv$private_keys_upload$datapath, exdir = temp_private_keys_folder)
  if (TRUE %in% (rv$public_keys_upload != "")) {
    public_keys_file <- zip::unzip(rv$public_keys_upload$datapath, exdir = temp_public_keys_folder)
  }
  decrypted_data <- lapply(1:length(uploaded_file), function(x) {
    data <- readRDS(uploaded_file[x])
    level <- data$metadata$level[1]
    if (! TRUE %in% (rv$public_keys_upload != "")) {
      write_pem(data$public_key, path = paste0(temp_public_keys_folder, "/", data$metadata$unique_file_name[1], "%_%", level))
    }
    saveRDS(data$encrypted_signed_data, paste0(temp_data_storage_folder, "/encrypted_signed_data.RDS"))
    signature_present <- EQUAL_verify_signature(file_name = paste0(temp_data_storage_folder, "/encrypted_signed_data.RDS"),
                                                signature = data$signature,
                                                public_key_folder = temp_public_keys_folder,
                                                key_name = paste0(data$metadata$unique_file_name[1], "%_%", level))
    if (signature_present == FALSE) {
      output <- list(decrypted_data = NULL, metadata = NULL,
                     html_message = "<h5>The signature could not be verified. This may be due to data corruption. Please download from the original source.</h5>")
      return(output)
    } else {
      if (level != "level_0") {
        decrypted_data <- try(EQUAL_decrypt_data(encrypted_data = data$encrypted_signed_data,
                                                 private_key_folder = temp_private_keys_folder,
                                                 key_name = paste0(data$metadata$unique_file_name[1], "%_%", level)),silent = TRUE)
        if (TRUE %in% (class(decrypted_data) == "try-error")) {
          output <- list(decrypted_data = NULL, metadata = NULL,
                         html_message = "<h5>The private keys for one or more access levels were incorrect or not provided. Please contact the data provider for the correct private keys and access levels.</h5>")
          return(output)
        }
      } else {
        decrypted_data <- data$encrypted_signed_data
      }
      output <- list(decrypted_data = decrypted_data, metadata = data$metadata,
                     html_message = NULL)
    }
  })
  valid_decryption <- ! (TRUE %in% lapply(decrypted_data, function(x) {is.null(x[[1]])}))
  if (valid_decryption == FALSE) {
    output <- list(html_message = paste0(unique(unlist(lapply(decrypted_data, function(x) {x[[3]]}))), collapse = "\n"),
                   decrypted_file_name = NULL)
  } else {
    metadata <- do.call(rbind.data.frame, lapply(decrypted_data, function(x) {x[[2]]}))
    metadata <- metadata[order(metadata$order),]
    decrypted_data <- do.call(cbind.data.frame, lapply(decrypted_data, function(x) {x[[1]]}))
    decrypted_data <- data.frame(decrypted_data[,metadata$columns])
    colnames(decrypted_data) <- metadata$columns
    write.csv(decrypted_data, paste0(tempdir(), "/decrypted_data.csv"),
              row.names = FALSE, na ="")
    output <- list(html_message = "<h4>The data has been successfully decrypted. Please download the decrypted data by clicking on 'Download the decrypted file' option.</h4>",
                   decrypted_file_name = paste0(tempdir(), "/decrypted_data.csv"))
  }
  unlink(temp_private_keys_folder, recursive = TRUE)
  unlink(temp_public_keys_folder, recursive = TRUE)
  unlink(temp_data_storage_folder, recursive = TRUE)
  return(output)
}
