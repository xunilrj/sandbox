use log::trace;
use rdkafka::admin::{AdminClient, AdminOptions, NewTopic, TopicReplication};
use rdkafka::client::DefaultClientContext;
use rdkafka::config::ClientConfig;
use rdkafka::consumer::base_consumer::BaseConsumer;
use rdkafka::consumer::Consumer;
use rdkafka::consumer::DefaultConsumerContext;
use rdkafka::message::OwnedHeaders;
use rdkafka::producer::{FutureProducer, FutureRecord};
use std::sync::mpsc::{channel, Receiver, Sender};
use std::time::Duration;

pub enum KafkaMessage {
    Stop,
    FireAndForget(String, String),
}

impl KafkaMessage {
    pub(in crate::actors::kafka) async fn process(
        self,
        producer: &FutureProducer,
        topic: &str,
    ) -> bool {
        match self {
            KafkaMessage::FireAndForget(key, payload) => {
                let never_block = 0;
                let _ = producer
                    .send(
                        FutureRecord::to(topic)
                            .key(&key)
                            .payload(&payload)
                            .headers(OwnedHeaders::new().add("header_key", "header_value")),
                        never_block,
                    )
                    .await
                    .unwrap();
                false
            }
            KafkaMessage::Stop => true,
        }
    }
}

pub fn start_kafka_producer(
    brokers: String,
    topic: String,
) -> (Sender<KafkaMessage>, tokio::task::JoinHandle<()>) {
    let (tx, rx) = channel::<KafkaMessage>();
    let admin: AdminClient<DefaultClientContext> = ClientConfig::new()
        .set("bootstrap.servers", &brokers)
        .set("message.timeout.ms", "500")
        .create()
        .expect("admin creation error");

    let producer: FutureProducer = ClientConfig::new()
        .set("bootstrap.servers", &brokers)
        .set("message.timeout.ms", "5000")
        .create()
        .expect("Producer creation error");

    let topic = topic.to_owned();
    let t = tokio::spawn(async move {
        let opts = AdminOptions::new().operation_timeout(Some(Duration::from_secs(1)));
        let topic1 =
            NewTopic::new(&topic, 1, TopicReplication::Fixed(1)).set("max.message.bytes", "1234");
        admin
            .create_topics(&[topic1], &opts)
            .await
            .expect("topic creation failed");

        loop {
            let msg = rx.recv().unwrap();
            let stop = msg.process(&producer, &topic).await;
            if stop {
                break;
            }
        }
    });

    (tx, t)
}

#[derive(Debug)]
pub enum KafkaConsumerMessage {
    Message(String),
}

pub fn start_kafka_consumer(brokers: String, topic: String) -> Receiver<KafkaConsumerMessage> {
    // let admin: AdminClient<DefaultClientContext> = ClientConfig::new()
    //     .set("bootstrap.servers", &brokers)
    //     .set("message.timeout.ms", "500")
    //     .create()
    //     .expect("admin creation error");

    let ctx = DefaultConsumerContext {};

    let consumer: BaseConsumer = ClientConfig::new()
        .set("group.id", "chat")
        .set("bootstrap.servers", &brokers)
        .set("enable.auto.commit", "true")
        .create_with_context(ctx)
        .expect("Consumer creation failed");
    consumer.subscribe(&[&topic]).unwrap();

    let (tx, rx) = channel::<KafkaConsumerMessage>();

    let _t = tokio::spawn(async move {
        trace!("start_kafka_consumer.loop started.");
        loop {
            let message = consumer.poll(None);
            let msg = message.unwrap().unwrap().ptr();

            let slice =
                unsafe { std::slice::from_raw_parts((*msg).payload as *const u8, (*msg).len) };
            let payload: String = String::from_utf8_lossy(slice).to_string();
            tx.send(KafkaConsumerMessage::Message(payload)).unwrap();
        }
    });

    rx
}
