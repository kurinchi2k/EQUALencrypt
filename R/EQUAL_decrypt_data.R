EQUAL_decrypt_data <- function(encrypted_data, private_key_folder, key_name) {
  temp_private_key <- paste0(private_key_folder, "/", key_name)
  decrypted_aes_key <- rsa_decrypt(data = encrypted_data$session, key = read_key(temp_private_key), oaep = TRUE)
  decrypted_data <- aes_cbc_decrypt(data = encrypted_data$data, key = decrypted_aes_key, iv = encrypted_data$iv)
  output <- unserialize(decrypted_data)
}
