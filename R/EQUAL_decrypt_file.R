EQUAL_decrypt_file <- function(encrypted_data, private_key_folder, key_name, data_storage_folder) {
  temp_private_key <- paste0(private_key_folder, "/", key_name)
  decrypted_aes_key <- rsa_decrypt(data = encrypted_data$session, key = read_key(temp_private_key), oaep = TRUE)
  decrypted_data <- aes_cbc_decrypt(data = encrypted_data$data, key = decrypted_aes_key, iv = encrypted_data$iv)
  binary_file <- file(paste0(data_storage_folder, "/decrypted_file.zip"), "wb")
  writeBin(decrypted_data, con = binary_file)
  close(binary_file)
}
