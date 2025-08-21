EQUAL_insert_signature_file <- function(file_name, private_key_folder, key_name) {
  private_key_name <- paste0(private_key_folder, "/", key_name)
  openssl::signature_create(file_name, hash = sha384, key = read_key(private_key_name))
}
