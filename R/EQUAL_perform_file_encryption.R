globalVariables(c("private_keys_folder", "public_keys_folder", "data_storage_folder"))
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
