library("translateR")

df <- read.csv("deerestatM.csv", stringsAsFactors = FALSE)

ncol()

df_vi <- subset(df, df$lang == "vi")

df_vi$translated_data <- translate(content.vec = df_vi$stripped_text,
                                  google.api.key = "AIzaSyBTXz8tyyiuTUJVAjyy4A6nrw8-EQhCqiE",
                                  source.lang = "vi", target.lang = "en")

df_de <- subset(df, df$lang == "de")

df_de$translated_data <- translate(content.vec = df_de$stripped_text,
                                   google.api.key = "AIzaSyBTXz8tyyiuTUJVAjyy4A6nrw8-EQhCqiE",
                                   source.lang = "de", target.lang = "en")


df_de$translated_data_hashtags <- translate(content.vec = df_de$hashtags,
                                   google.api.key = "AIzaSyBTXz8tyyiuTUJVAjyy4A6nrw8-EQhCqiE",
                                   source.lang = "de", target.lang = "en")
