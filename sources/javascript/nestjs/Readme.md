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

## COnfig/Toggle

## Services

1 - Subcategorized as xxxQueries and xxxCommands

## Repositories

1 - Always use generic repository where applicable
2 - getById return null when does not find
3 - return queryBuilder for specific queries instead of running the query
4 - save/delete/update existe on the Session, never on the repository

## Logging

We use semantic logging in two senses. Log events are objects with data and not just text and each log level exist for a reason.

This is not how https://tools.ietf.org/html/rfc5424 address these levels.
https://github.com/winstonjs/winston

  - Trace = understand what is happening inside a method. Ideally turn on for just one method.
    - https://stackoverflow.com/questions/11853256/how-to-get-javascript-function-calls-trace-at-runtime
    - https://javascriptweblog.wordpress.com/2010/06/01/a-tracer-utility-in-2kb/
  - Verbose =  understand what is happening system-wise. Ideally method calls with arguments
    - https://stackoverflow.com/questions/11853256/how-to-get-javascript-function-calls-trace-at-runtime
  - Information = "aggregable" objects for KPIs (Key Performance Indicators)
  - Warning = "aggregable" objects for KRIs (Key Risk Indicators)
  - Error = Need human/admin interaction. Notification must be sent.
  - Fatal = application in killing itself

### Verbose log

Verbose log must come from frameworks listenters, plugins etc... you should not contaminate your code with log call all over the place.

### Aggregating log events for KPIs

"Information" events are aggregated using CEP in KPIs

### Aggregating log events for KRIs

"Warning" events are aggregated using CEP in KRIs
 
## Vault
## Config Service ()

# Testing

80% of tests using api test with stub adapters (db, queue etc...)

Minimize mocking. Why? It generally test your implementation and not the method intentions.
System must be testable using configuration. Minimize hacks to test. Why? test adaptability of architecture.

https://www.npmjs.com/package/fast-check
https://www.npmjs.com/package/jsverify

# Devops

# API Contracts

All api endpoints have a type, that defines the contract. 

List Entities
  GET /api/<ENTITY>
	Pagination	?page=1&page_size=1000
	Filter	?filter=Name%20eq%’NAME’
	Order	?orderby=Name%20desc
	Expand	(_privileges)
	Returns	200 […] ← array
  Property Testing
    for all properties Order must return sorted list
    for all properties filter must return property matching filter
    for all items can appear only one page
  GET /api/<ENTITY>/$metadata
    properties info

Get ID		
  GET /api/<ENTITY>/<KEY>
	invalid id	400 {“code”:”invalidId”,message:”...”}
	not found	404 {“code”:”notFound”,message:”...”}
	deleted	404 {“code”:”notFound”,message:”...”}
	Expand	(_privileges, _allowedItems/<PROPERTY>)
	Returns	200 {…} <- bag or properties
  Property Testing
    for all items, listed on "List Entity" must be returned by id

Create Entity	
  POST /api/<ENTITY>
	Missing Required Property	400 {“code”:”InvalidInput”,message:”...”, "errorDetails": [ "propertyName": "AuditFunctionId", "message": "(MUST BE GLOBALIZED!!"}]} }
	Invalid Property Value	400 {“code”:”InvalidInput”,message:”...”, "errorDetails": [ "propertyName": "AuditFunctionId", "message": "(MUST BE GLOBALIZED!!"}]} }
	Relationship: 
        {…, Responsible: { Id: “...” } } OR
        {…, Responsible: { Property: “...” } } <- only with return just one item OR
        {..., Responsible: "KEY", ...} <- must be key value
	Returns	201 {…} ← same as GET (!IMPORTANT)
  Property Testing
    after creation list qtd must increase by 1
    after creation must be possible to get by id (exactly equal obj)
    after creation must be possible to see item in list    

Update Entity
    when PUT <- completely update entity
        PUT /api/<ENTITY>/<KEY>
        Invalid Property Value	400 {“code”:”InvalidInput”,message:”...”, "errorDetails": [ "propertyName": "AuditFunctionId", "message": "(MUST BE GLOBALIZED!!"}]} }
	    returns 200 ← same as GET (!IMPORTANT)
        PUT /api/<ENTITY>/<KEY>?force=true
            CAN BE USED WITH FORCE TO BYPASS RULES (needs permission)
  Property Testing
    get after put must return exactly equal put (on editable properties)
	when PATCH <- just the properties passed
        PATCH /api/<ENTITY>/<KEY>
        return 200 <- same as GET (!IMPORTANT)
        used with state machines: {state: "newState", transitionField: ""}
  Property Testing
    get after patch must return exactly equal patch (for sent properties)

Delete Entity	
  DELETE /api/<ENTITY>/<KEY>
	invalid id	400 {“code”:”invalidId”,message:”...”}
	not found	404 {“code”:”notFound”,message:”...”}
	already deleted	404 {“code”:”notFound”,message:”...”}
  Property Testing
    after delete should return 404 on get
    after delete should not appear on list

“Methods” (eg: Approve)	POST api/<ENTITY>/<KEY>/$approve {“Description:”...”}
	200 {…} ← OkInfo


Datasources	<- different from list entities, more "grid" like
    GET /api/datasources/<NAME>
	Pagination	?page=1&page_size=1000
	Filter	?filter=Name%20eq%’NAME’
	Order	?orderby=Name%20desc
	Expand	???
	Returns	200 { columns: [], rows:[], aggregations: {<KV>} }
  Property Testing
    for all properties Order must return sorted list
    for all properties filter must return property matching filter
    for all items can appear only one page

Datasets <- multiple datasources
    GET /api/datasets/<NAME>
    GET /api/datasets?datasources=<NAME>,<NAME>
	Pagination	?page=1&page_size=1000
	Filter	?filter=Name%20eq%’NAME’ <- cross filter and/or per datasource
	Order	?orderby=Name%20desc <- cross order and/or per datasource
	Expand	???
	Returns	200 { <NAME1>: {datasource}, <NAME2>: {datasource} }

Allowed Items		
	Pagination	?page=1&page_size=1000
	Filter	?filter=Name%20eq%’NAME’
	Order	?orderby=Name%20desc
	Expand	???
	Returns	200 […] ← same as list entities
	URIs	
        GET /api/<ENTITY>/$allowedItems/Responsible ← 200 […]
		GET /api/<ENTITY>/<KEY>/$allowedItems/Responsible ← 200 […]
		Pagination, Filter, Order, Expand

Error Schema
    { errors: [{code:"", message:"globalized user friendly text", data: {}}]}

Asking Progress
    https://www.w3.org/TR/eventsource/
    https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events
    standard only supports GET, but it works with any method.
    POST /api/<ENTITY>/<KEY>/$publish
    Accept-Type: text/event-stream

    200 Accepted
    Content-Type: text/event-stream
    events
        event: start
        data: {type: "start", id: "ASYNCID"}

        event: progress
        data: {type: "progress", id: "ASYNCID", progress: 0.1, message: "user friendly globalized text", step: {usefuldata}}

        event: end
        data: {type: "end", id: "ASYNCID"}

        event: return
        data: {} <- normal return

API Batch
    https://github.com/Huachao/vscode-restclient    
    /api/batch

# Task Scheduler
# Task Job (Run Background)
# Health CHeck

https://github.com/GenFirst/nest-status-monitor

## Globalization

https://github.com/ToonvanStrijp/nestjs-i18n


# NestJS

## IOC Instropectors

To allow interception in AOP style we configure out module using our little IOC helper class. It is a fluent API that allows us insert interceptors in any class using a POJO config class, the first argument of the "ioc" method. The second argument is all possible interceptors.

```ts
@Module({
  imports: [],
  controllers: [AppController],
  providers: ioc({}, {log})
    .transient(AppService2)
    .transient(AppService, IAppService)
    .build() 
})
export class AppModule {}
```

We can very easily configure with, and all methods, of all dependencies will be "logged". This functionality has some overhead, off course, but it is very useful.

```ts
@Module({
  imports: [],
  controllers: [AppController],
  providers: ioc({"*":["log"]}, {log})
    .transient(AppService2)
    .transient(AppService, IAppService)
    .build() 
})
export class AppModule {}
```

You can also append this configuration from the Request with:

```
GET http://localhost:3000 HTTP/1.1
X-App-Config: {"*":["log"]}
```

This is a "debug", "dev" tool, so we allow you to control when this is available, dev only, "admin" only, from certain IPs etc... You should never use this in your normal workflow, but enabling the correct logger and using the Server Events functionality gives you a very strong debugging tool as we will see in the future.

### Server Events
### Server Events Logger
### What-if
### Trace Instropector

# Access Control using RBAC on Sql Server graph




