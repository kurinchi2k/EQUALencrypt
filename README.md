This repository relates to two shiny applications *EQUALencrypt - Encrypt and decrypt whole files* and *EQUALencrypt - Encrypt and decrypt columns of data*, 
which allow people with no coding skills to encrypt and share research data. Without the correct digital signatures and private keys, data encrypted using 
the applications cannot be decrypted, ensuring data integrity and security during data transfer between researchers.

For users who plan to install the applications on a server, we have an image built using Docker Desktop application. These images can be pulled from 
https://hub.docker.com/r/kurinchi2k/equalencrypt_whole_file and https://hub.docker.com/r/kurinchi2k/equalencrypt_columns. The user interfaces for the two 
applications are available from the port mapped to port 3838, the port used by both the applications.

For individual researchers who want to run local versions of the applications, we recommend that they use the Zenodo links ( 
https://doi.org/10.5281/zenodo.16743676 and https://doi.org/10.5281/zenodo.16744058) to obtain more information about installations required.
