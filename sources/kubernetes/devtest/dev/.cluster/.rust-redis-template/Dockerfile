FROM ekidd/rust-musl-builder:stable
VOLUME /usr/src
WORKDIR /usr/src
ADD start.sh /usr/start.sh
RUN sudo chmod +x /usr/start.sh
ENTRYPOINT ["/usr/start.sh"]