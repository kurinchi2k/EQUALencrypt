EQUAL_encrypt_file <- function(file_name, public_key_folder, key_name) {
  temp_public_key <- paste0(public_key_folder, "/", key_name)
  aes_key <- rand_bytes(32)
  iv <- rand_bytes(16)
  aes_encrypted_file <- aes_cbc_encrypt(file_name, key = aes_key, iv = iv)
  rsa_encrypted_aes_key <- rsa_encrypt(aes_key, pubkey = temp_public_key, oaep = TRUE)
  encrypted_file <- list(iv = iv, session = rsa_encrypted_aes_key, data = aes_encrypted_file)
}
