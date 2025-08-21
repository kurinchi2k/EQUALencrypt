EQUAL_insert_signature_data <- function(data, private_key_folder, key_name) {
  saveRDS(data, paste0(tempdir(), "/data.RDS"))
  data_file <- paste0(tempdir(), "/data.RDS")
  private_key_name <- paste0(private_key_folder, "/", key_name)
  signature <- openssl::signature_create(data_file, hash = sha384, key = read_key(private_key_name))
  output <- list(path_to_signed_file = data_file, signature = signature)
}
