# Grab app name, API key and API secret from the credentials saved in 1Password:

authenticate_twitter <- function() {
  #whatever name you assigned to your created app
  appname <- "voc-collector"
  
  # api key (example below is not a real key)
  key <- "xsBRv4HDRFw3YRe1jg3lTWxz8"
  
  # api secret (example below is not a real key)
  secret <- "1Qf0flgv8R8Qia96SNFtJ7JjEoVsHAoNJ4gD0tDcoZDHh3ZpPi"
  
  # access_token
  access_token <- "3302721691-53elyQLaCZwWpAiCImnxBfm48mqmeLt6QqjDz2f"
  
  # access_secret
  access_secret <- "W4H4xpBLw3g6VkYrUtJEuIYaM2JQuSZZ58FKWm2m3Nq7U"
  
  # create token named "twitter_token"
  twitter_token <- create_token(
    app = appname,
    consumer_key = key,
    consumer_secret = secret,
    access_token = access_token,
    access_secret = access_secret)
  
  test_token <- get_token()
  
  # uncomment to test that the token is stable
  
  # test_token
}
