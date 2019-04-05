# Runing RJava
if (Sys.info()['sysname'] == 'Darwin') {
  libjvm <- paste0(system2('/usr/libexec/java_home',stdout = TRUE)[1],'/jre/lib/server/libjvm.dylib')
  message (paste0('Load libjvm.dylib from: ',libjvm))
  dyn.load(libjvm)
}


library(RYandexTranslate)
ls("package:RYandexTranslate")

api_key = "trnsl.1.1.20180803T171456Z.490652cf407663f6.c39c8fefeb5c5d78301b1a1bded6b48772a2ea74"
directions = get_translation_direction(api_key)

head(directions)

data = translate(api_key, text="how are you?", lang="en-id"  )

data = detect_language(api_key,text="how are you?")
data

