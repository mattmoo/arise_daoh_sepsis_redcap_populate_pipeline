#' Encrypt a vector using a character key.
#'
#' .. content for \details{} ..
#'
#' @title

aes_encrypt_vector <- function(data_in, initialisation_vector_raw, key = '') {

  if (key == '') {
    stop('Encryption password not set')
  }
  if (nchar(key) > 40) {
    stop('Encryption password seems too long, may be malformed')
  }
  
  key = openssl::sha256(charToRaw(key))
  # key = as.raw(charToRaw(key))
  
  encrypt = function(data_in) {
    data = openssl::aes_cbc_encrypt(data = charToRaw(data_in),
                                    key = key,
                                    iv = initialisation_vector_raw)
    data_char = base64enc::base64encode(data)
    
    return(data_char)
  }
  
  data_out = unlist(lapply(X = data_in, FUN = encrypt))
  
  return(data_out)

}

aes_decrypt_vector <- function(data_in, initialisation_vector_raw, key = '') {
  
  if (key == '') {
    stop('Encryption password not set')
  }
  if (nchar(key) > 40) {
    stop('Encryption password seems too long, may be malformed')
  }
  
  key = openssl::sha256(charToRaw(key))
  # key = as.raw(charToRaw(key))
  
  decrypt = function(data_in) {
    data = openssl::aes_cbc_decrypt(data = base64enc::base64decode(data_in),
                                    key = key,
                                    iv = initialisation_vector_raw)
    data_char = rawToChar(data)
    
    return(data_char)
  }
  
  data_out = unlist(lapply(X = data_in, FUN = decrypt))
  
  return(data_out)
  
}
