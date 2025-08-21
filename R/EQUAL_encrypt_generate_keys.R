EQUAL_encrypt_generate_keys <- function(public_key_folder, private_key_folder, key_name) {
  public_key_name <- paste0(public_key_folder, "/", key_name)
  private_key_name <- paste0(private_key_folder, "/", key_name)
  key <- openssl::rsa_keygen(bits = 4096)
  pubkey <- as.list(key)$pubkey
  openssl::write_pem(key, private_key_name)
  openssl::write_pem(pubkey, public_key_name)
  output <- list(private_key = key, public_key = pubkey)
}
