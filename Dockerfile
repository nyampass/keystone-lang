FROM clojure:temurin-19-tools-deps-bullseye-slim AS builder

WORKDIR /build
COPY . /build
RUN clj -T:build uber

FROM eclipse-temurin:19-jre-alpine
WORKDIR /app
COPY --from=builder /build/target/*-standalone.jar ./app.jar
CMD ["java", "-jar", "app.jar"]
