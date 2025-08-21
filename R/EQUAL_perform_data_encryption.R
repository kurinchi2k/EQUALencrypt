globalVariables(c("encryptions_keys_path", "private_keys_folder", "public_keys_folder", "data_storage_folder"))
EQUAL_perform_data_encryption <- function(rv, server_address = tempdir()) {
  temp_private_keys_folder <- paste0(tempdir(), "/private_keys_folder")
  temp_public_keys_folder <- paste0(tempdir(), "/public_keys_folder")
  temp_data_storage_folder <- paste0(tempdir(), "/data_storage_folder")
  if (dir.exists(temp_private_keys_folder) == TRUE) {unlink(temp_private_keys_folder, recursive = TRUE)}
  if (dir.exists(temp_public_keys_folder) == TRUE) {unlink(temp_public_keys_folder, recursive = TRUE)}
  if (dir.exists(temp_data_storage_folder) == TRUE) {unlink(temp_data_storage_folder, recursive = TRUE)}
  dir.create(temp_private_keys_folder)
  dir.create(temp_public_keys_folder)
  dir.create(temp_data_storage_folder)
  uploaded_file <- read.csv(rv$file_upload_encrypt$datapath, check.names = FALSE, na.strings = c("", " ", "  "))
  colnames(uploaded_file) <- iconv(colnames(uploaded_file), 'latin1', 'ASCII', sub = '')
  colnames(uploaded_file)[colnames(uploaded_file) == ""] <- "Missing column names"
  new_encryption_rows <- {cbind.data.frame(
    unique_file_name = UUIDgenerate(),
    columns = colnames(uploaded_file),
    data_type = unlist(lapply(uploaded_file, typeof)),
    order = 1:ncol(uploaded_file),
    level = NA
  )}
  new_encryption_rows$level <- unlist(lapply(new_encryption_rows$columns, function(x) {
    if (x %in% rv$level_1) {
      "level_1"
    } else if (x %in% rv$level_2) {
      "level_2"
    } else if (x %in% rv$level_3) {
      "level_3"
    } else if (x %in% rv$level_4) {
      "level_4"
    } else if (x %in% rv$level_5) {
      "level_5"
    } else if (x %in% rv$level_6) {
      "level_6"
    } else if (x %in% rv$level_7) {
      "level_7"
    } else {
      "level_0"
    }
  }))
  if (dir.exists(paste0(temp_data_storage_folder, "/", new_encryption_rows$unique_file_name[1]))) {
    unlink(paste0(temp_data_storage_folder, "/", new_encryption_rows$unique_file_name[1]), recursive = TRUE)
  }
  dir.create(paste0(temp_data_storage_folder, "/", new_encryption_rows$unique_file_name[1]))
  # Create each column as list
  uploaded_file_columns <- as.list(uploaded_file)
  unencrypted_columns <- uploaded_file_columns[new_encryption_rows$columns[new_encryption_rows$level == "level_0"]]
  if (length(unencrypted_columns) > 0) {
    keys <- EQUAL_encrypt_generate_keys(
      public_key_folder = temp_public_keys_folder, private_key_folder = temp_private_keys_folder,
      key_name = paste0(new_encryption_rows$unique_file_name[1], "%_%level_0")
    )
    signed_data_path_signatures <- EQUAL_insert_signature_data(data = unencrypted_columns, private_key_folder = temp_private_keys_folder,
                                                               key_name = paste0(new_encryption_rows$unique_file_name[1], "%_%level_0")
    )
    saveRDS(list(
      metadata = new_encryption_rows[new_encryption_rows$level == "level_0",],
      public_key = read_pubkey(paste0(temp_public_keys_folder, "/", paste0(new_encryption_rows$unique_file_name[1], "%_%level_0"))),
      encrypted_signed_data = readRDS(signed_data_path_signatures[[1]]),
      signature = signed_data_path_signatures[[2]]
    ), paste0(temp_data_storage_folder, "/", new_encryption_rows$unique_file_name[1], "/level_0.RDS"))
  }
  # Encrypt
  if (nrow(new_encryption_rows[new_encryption_rows$level != "level_0",]) > 0) {
    levels_of_access <- setdiff(sort(unique(new_encryption_rows$level)), "level_0")
    for (i in 1:length(levels_of_access)) {
      columns_to_encrypt <- new_encryption_rows$columns[new_encryption_rows$level == levels_of_access[i]]
      keys <- EQUAL_encrypt_generate_keys(
        public_key_folder = temp_public_keys_folder, private_key_folder = temp_private_keys_folder,
        key_name = paste0(new_encryption_rows$unique_file_name[1], "%_%", levels_of_access[i])
      )
      encrypted_data <- EQUAL_encrypt_data(data = uploaded_file_columns[columns_to_encrypt],
                                           public_key_folder = temp_public_keys_folder,
                                           key_name = paste0(new_encryption_rows$unique_file_name[1], "%_%", levels_of_access[i]))
      signed_data_path_signatures <- EQUAL_insert_signature_data(data = encrypted_data, private_key_folder = temp_private_keys_folder,
                                                                 key_name = paste0(new_encryption_rows$unique_file_name[1], "%_%", levels_of_access[i])
      )
      signed_data_path <- signed_data_path_signatures[[1]]
      signatures <- signed_data_path_signatures[[2]]
      saveRDS(list(
        metadata = new_encryption_rows[new_encryption_rows$level == levels_of_access[i],],
        public_key = read_pubkey(paste0(temp_public_keys_folder, "/", new_encryption_rows$unique_file_name[1], "%_%", levels_of_access[i])),
        encrypted_signed_data = readRDS(signed_data_path_signatures[[1]]),
        signature = signed_data_path_signatures[[2]]
      ), paste0(temp_data_storage_folder, "/", new_encryption_rows$unique_file_name[1], "/", levels_of_access[i], ".RDS"))
    }
  }
  # Package for different levels of access
  zipped_each_level <- lapply(0:7, function(x) {
    file_names <- paste0(temp_data_storage_folder, "/", new_encryption_rows$unique_file_name[1],"/level_", 0:x, ".RDS")
    file_names <- file_names[file.exists(file_names)]
    zip::zip(paste0(temp_data_storage_folder, "/", new_encryption_rows$unique_file_name[1],"/level_", x, "_main_content.zip"),
             files = file_names, mode = "cherry-pick")
  })
  zipped_each_public_keys <- lapply(0:7, function(x) {
    file_names <- paste0(temp_public_keys_folder, "/", new_encryption_rows$unique_file_name[1],"%_%level_", 0:x)
    file_names <- file_names[file.exists(file_names)]
    zip::zip(paste0(temp_data_storage_folder, "/", new_encryption_rows$unique_file_name[1],"/level_", x, "_public_keys.zip"),
             files = file_names, mode = "cherry-pick")
  })
  zipped_each_private_keys <- lapply(0:7, function(x) {
    file_names <- paste0(temp_private_keys_folder, "/", new_encryption_rows$unique_file_name[1],"%_%level_", 0:x)
    file_names <- file_names[file.exists(file_names)]
    zip::zip(paste0(temp_data_storage_folder, "/", new_encryption_rows$unique_file_name[1],"/level_", x, "_private_keys.zip"),
             files = file_names, mode = "cherry-pick")
  })
  zipped_content_publicly_shareable <- zip::zip(
    paste0(temp_data_storage_folder, "/", new_encryption_rows$unique_file_name[1],"/publicly_shareable.zip"),
    files = c(paste0(temp_data_storage_folder, "/", new_encryption_rows$unique_file_name[1],"/level_", 0:7, "_main_content.zip"),
              paste0(temp_data_storage_folder, "/", new_encryption_rows$unique_file_name[1],"/level_", 0:7, "_public_keys.zip")), mode = "cherry-pick")
  zipped_content_not_publicly_shareable <- zip::zip(
    paste0(temp_data_storage_folder, "/", new_encryption_rows$unique_file_name[1],"/not_publicly_shareable.zip"),
    files = paste0(temp_data_storage_folder, "/", new_encryption_rows$unique_file_name[1],"/level_", 0:7, "_private_keys.zip"), mode = "cherry-pick")
  zip_all_encrypted_content <- zip::zip(
    paste0(temp_data_storage_folder, "/", new_encryption_rows$unique_file_name[1],"/all_encrypted_content.zip"),
    files = c(paste0(temp_data_storage_folder, "/", new_encryption_rows$unique_file_name[1],"/publicly_shareable.zip"),
              paste0(temp_data_storage_folder, "/", new_encryption_rows$unique_file_name[1],"/not_publicly_shareable.zip")), mode = "cherry-pick")
  if (server_address != tempdir()) {
    encryption_keys <- readRDS(encryptions_keys_path)
    encryption_keys <- rbind.data.frame(encryption_keys, new_encryption_rows)
    saveRDS(encryption_keys, encryptions_keys_path)
    silencer <- file.copy(list.files(path = temp_private_keys_folder, full.names = TRUE), private_keys_folder, overwrite = TRUE)
    silencer <- file.copy(list.files(path = temp_public_keys_folder, full.names = TRUE), public_keys_folder, overwrite = TRUE)
    silencer <- file.copy(paste0(temp_data_storage_folder, "/", new_encryption_rows$unique_file_name[1],"/all_encrypted_content.zip"),
                          paste0(data_storage_folder, "/", new_encryption_rows$unique_file_name[1],"_all_encrypted_content.zip"),
                          overwrite = TRUE)
  }
  silencer <- file.copy(paste0(temp_data_storage_folder, "/", new_encryption_rows$unique_file_name[1],"/all_encrypted_content.zip"),
                        paste0(tempdir(), "/", new_encryption_rows$unique_file_name[1],"_all_encrypted_content.zip"),
                        overwrite = TRUE)
  unlink(temp_private_keys_folder, recursive = TRUE)
  unlink(temp_public_keys_folder, recursive = TRUE)
  unlink(temp_data_storage_folder, recursive = TRUE)
  if (file.exists(paste0(tempdir(), "/data.RDS"))) {file.remove(paste0(tempdir(), "/data.RDS"))}
  output <- list(html_message = "<h4>Encryption successfully completed. The encrypted files can be downloaded by clicking on the 'Download the encrypted file' button. Please see the instructions on what should be shared and what should not be shared.</h4>",
                 encrypted_file_name = paste0(tempdir(), "/", new_encryption_rows$unique_file_name[1],"_all_encrypted_content.zip"))
  return(output)
}
