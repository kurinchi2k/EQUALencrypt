---
title: 'EQUALencrypt: An R package for encrypting and decrypting research data'
tags:
  - R
  - encrypt
  - decrypt
  - data sharing
  - secondary analysis
  - reproducibility
authors:
  - name: Kurinchi Gurusamy
    affiliation: 1
affiliations:
  - index: 1
    name: University College London
date: 21 August 2025
bibliography: paper.bib
---

# Summary
Two shiny applications *EQUALencrypt - Encrypt and decrypt columns of data* and *EQUALencrypt - Encrypt and decrypt whole files* allow people with no coding skills to encrypt and share research data. 

# Statement of need
Sharing research data will enable testing the data for reproducibility of analysis and secondary analyses [@Lvovs:2025; @Kelly:2024]. These will increase the quality of research and decrease the costs for performing research [@Lvovs:2025; @Kelly:2024]. However, when the research involves human participants, it is important that personal identifiable data are not shared publicly to meet the legal requirements [@Lvovs:2025].

R is a free software with advanced statistical algorithms, data encryption and decryption, and digital signature insertion verification. However, considerable coding skills are necessary. 

Two shiny applications *EQUALencrypt - Encrypt and decrypt columns of data* [@Gurusamy:2025a] and *EQUALencrypt - Encrypt and decrypt whole files* [@Gurusamy:2025b] were created, which allow people with no coding skills to encrypt data. 

# Approach for data encryption and insertion of digital signature
*EQUALencrypt - Encrypt and decrypt whole files* [@Gurusamy:2025b] accepts whole files of any extension as input and uses openssl package to encrypt and digitally sign the file. To encrypt and digitally sign the file, it generates an unique pair of private and public RSA keys (4096 bits), encrypts the file using symmetric AES256 algorithm (32 bits for the key and 16 bits for initialization vector), encrypts the AES key using the asymmetric RSA keys, and includes padding according to PKCS #1 v2.0 specifications. It then inserts a digital signature using the SHA384 algorithm for the hash function.

*EQUALencrypt - Encrypt and decrypt columns of data* [@Gurusamy:2025a] accepts only csv files (with only ASCII characters) and up to 7 levels of access of columns. The columns which have not been selected at any access level will be unencrypted and will be available to people with any level of access. People with higher access level will also be able to view the columns that people with lower access level can view. For example, a person with access level 4 will be able to view the unencrypted columns and the columns that people with access levels 1 to 3 can view in addition to access level 4 that they belong to.

The columns in each level of access are encrypted using the same approach as for Encrypt and decrypt whole files [@Gurusamy:2025a], except that a set of unique pair of private and public RSA keys (4096 bits) are generated for each level of access which contain at least one column.

# Sharing the information publicly and privately
The publicly shareable information include the encrypted files, digital signature, and the public key, while the private keys must be shared only with people who have permission to decrypt the data. It is recommended that sharing the publicly shareable information and the private keys are performed in two different to decrease the risk of undesirable decryption.

# Verification of signature and decryption
The user uploads the encrypted data, digital signature, the public and private keys to decrypt data or columns of data (into the corresponding applications). If the encrypted data, digital signature, and the keys match, the data is decrypted. In this way, data integrity and security are both achieved.

# Testing
Testing of EQUALencrypt - Encrypt and decrypt whole files [@Gurusamy:2025a] was performed on simulated data, graphs generated from simulated data, and a set of CC0 images available from The Cleveland Museum of Art [@The:Cleveland:Museum:of:Art:2025].

Testing of EQUALencrypt - Encrypt and decrypt columns of data [@Gurusamy:2025b] was performed on simulated data.

The tests revealed that the decrypted data was identical to the encrypted data when the encrypted data, digital signature, and the keys matched. Decryption was not performed in any instance in which there was a mismatch in any of encrypted data, digital signature, and the keys or if there was an alteration to the data.

# Acknowledgements
No external source of funding

# Conflicts of Interest
My salary and promotions are linked to performing and reporting high-quality research.

# References
