# Backend Architecture

Philosophy:

- We going to use the "Onion Architecture";
- Components communicate as they are remote;
  - A Note on Distributed Computing https://www.cc.gatech.edu/classes/AY2010/cs4210_fall/papers/smli_tr-94-29.pdf
  - "Localization as optimization" strategy of Actor Systems;
    - The key for enabling this is to go from remote to local by way of optimization instead of trying to go from local to remote by way of generalization. See this classic paper for a detailed discussion on why the second approach is bound to fail.
    - https://doc.akka.io/docs/akka/2.1/general/remoting.html
- Semantic logging


# Onion Architecture - Ports and Adapters

Our system is going to be partitioned in "Components". Each "Component" is going to have a "Facade", "Services", "Repositories", a "Transactional Queue" and a "Non-Transaction Queue".

Possible adapters are:

- ORM adapter
- Search adapter
- Notifications adapter
  - E-mail
  - Mobile push
- Generic bus adapter

Possible ports are:

- REST API
- Admin UI
- User UI
- Command line
  - Internal cmdline
  - External cmdline (using REST API)
- Websocket

# Remote First

"Remote First" philosophy comes from the famous paper "A Note on Distributed Computing". This paper is the philosophical foundation for Actor System. Its conclusion that matter to us here is that going from local to remote as a mean of generalization is bounded to fail because local and remote calls and essentially different. That is why in "Actor Systems" you model all, 100%, of the interaction as remote calls and make some calls local as optimization. "Locality as optimization" and not as standard.

A Note on Distributed Computing  
https://www.cc.gatech.edu/classes/AY2010/cs4210_fall/papers/smli_tr-94-29.pdf  


In practice this means:

1 - All methods are async/await;
2 - You can be calling other Components through a Proxy class.
3 - We will probably have more Coarse Grained calls.
3.1 - Instead of calling a method "n" times inside a loop, we will call just once with an array, for example.

Isn't this over-engineering? No, for two reasons.

1 - We are not making the code more complex than necessary. We are just making all calls async/waitable;
2 - We are no making the code more generic/abstract.

# 

## Services

1 - Subcategorized as xxxQueries and xxxCommands

## Repositories

1 - Always use generic repository where applicable
2 - getById return null when does not find
3 - return queryBuilder for specific queries instead of running the query
4 - save/delete/update existe on the Session, never on the repository

## Logging

We use semantic logging in two senses. Log events are objects with data and not just text and each log level exist for a reason.

  - Trace = understand what is happening inside a method. Ideally turn on for just one method.
  - Verbose =  understand what is happening system-wise. Ideally method calls with arguments
  - Information = "aggregable" objects for KPIs (Key Performance Indicators)
  - Warning = "aggregable" objects for KRIs (Key Risk Indicators)
  - Error = Need human/admin interaction. Notification must be sent.
  - Fatal = application in killing itself

### Aggregating log events for KPIs

"Information" events are aggregated using CEP in KPIs

### Aggregating log events for KRIs

"Warning" events are aggregated using CEP in KRIs
 
## Vault
## Config Service ()

