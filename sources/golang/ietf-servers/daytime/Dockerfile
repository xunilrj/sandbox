FROM golang:alpine AS builder
RUN apk update && apk add --no-cache git
WORKDIR /build
COPY . /build
RUN go get -v ...
RUN CGO_ENABLED=0 GOOS=linux GOARCH=amd64 go build -a -installsuffix cgo -ldflags="-w -s" -o /opt/daytime/main

FROM scratch
COPY --from=builder /opt/daytime /opt/daytime
EXPOSE 13
ENTRYPOINT  ["/opt/daytime/main"]