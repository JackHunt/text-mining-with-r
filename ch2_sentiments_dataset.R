library(tidytext)

print("sentiments")
print(sentiments)

print("afinn")
print(get_sentiments("afinn"))

print("bing")
print(get_sentiments("bing"))

print("nrc")
print(get_sentiments("nrc"))
