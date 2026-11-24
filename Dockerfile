# Build the jar first:  clojure -M:uberjar
# Then:                 docker build -t loregate .
#                       docker run -p 8080:8080 -v $PWD/deploy:/config loregate /config/settings.edn
FROM eclipse-temurin:21-jre
WORKDIR /app
COPY target/loregate.jar /app/loregate.jar
EXPOSE 8080
ENTRYPOINT ["java", "--enable-native-access=ALL-UNNAMED", "-jar", "/app/loregate.jar"]
CMD ["/config/settings.edn"]
