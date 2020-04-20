# Backend Architecture

![alt text](./BasicCSharpBackend.png "Basic C# Backend Architecture")  


Controllers/ASP.NET Core:  
- Call Façades and transform results in HTTP Responses.

Façades:

To avoid the façade-must-call-service-that-must-call-store rule, I like to put
non-repetitive logic directly on the façade. If something repeat we can promote it to a "Service".

- Validate authentication/authorization
- Validate input
- Orchestrate Services or Do the logic
- MUST Commit the transaction (only façades commit)
  - As o side-effect is generated before the commit is called, we can easily generate what-if. Useful for tests.

Services

- Do the logic
- NEVER validate authentication/authorization
- NEVER commit
- Must be composable/orchestratable (not a real word) by Façades

UoW (implement three interfaces)

- Implement generic store/repository: 
  - IQueryable<T> get<T>()  
  - T getById<T>(id)  
  - Pagination<T> paginate<T>(pagination)  
  - void insert<T>(T item)  
  - Task<T> insertReturn<T>(T item)  <- task only finished after commit (dangerous but usefull)
  - void update<T>(T item)  
  - Task<T> updateReturn<T>(T item)  <- task only finished after commit (dangerous but usefull)
  - void delete(T item)

- Implement domain event dispatcher
  - Task Raise(EventArgs)

- Implement ISaveChanges
  - Task Commit()

Most important method:  
 - Generate an EntityCreate, EntityUpdated, EntityDeleted for each operation (same order they happened)  
 - Dispatch all events (same order they happened) to "Sync Handlers"
   - "Sync Handlers" typically increase the semantic of events (when "Due Date" is changed send email to responsible". This handler NEVER send the email, just tell that an email must be sent.)
 - Open Transaction  
   - Send entities commands to the DB  
   - Save persisted events to the DB  
   - Commit Transaction  
   - Send messages to the queue
     - "Async Handlers" do the actual work. That email is sent here.

I understand that the most polemic part here is: "why saving the messages in the DB if we have a queue?"

Normally people do:

```
uow.Commit();
Queue.Dispatch(events);
```

Of course that in 99.999% of the cases, everything works, but when the "Dispatch" fails, the messages are lost. If this is OK, so this solution is fine.

If this is not OK, we need something better:
 - or we put DB and Queue in "distributed transaction";
   - in this case, how you backup/restore the system? Not easy.
 - or we persist everything in the DB.

The second is easier and scale much better (we can have multiple tables, partitioned tables to guarantee O(1) etc...)

The second point would be: "If we persisting messages in the DB, why even use a queue?"

The answer is: First because it is better. In the 99.99% of the cases, we never touch the "message table" so it is not onerous. Second, we may have non-persisted/it-doesnt-matter-if-we-lose-the-message messages (alert the user through Websocket that something happened, for example).

More:
"Life beyond Distributed Transactions: an Apostate’s Opinion"  
http://www-db.cs.wisc.edu/cidr/cidr2007/papers/cidr07p15.pdf

# Stackoverflow Answers

## C# and .NET

### UnitOfWork, Repository Database Connection Issue  
about IoC container Unity  
https://stackoverflow.com/questions/39954586/unitofwork-repository-database-connection-issue/40418462#40418462  

### Why does 'unbox.any' not provide a helpful exception text the way 'castclass' does?
about how the .NET JIT works  
https://stackoverflow.com/questions/39914845/why-does-unbox-any-not-provide-a-helpful-exception-text-the-way-castclass-do/40073409#40073409

### Cancel Specific Task using CancellationToken
How TPL works  
https://stackoverflow.com/questions/35816146/cancel-specific-task-using-cancellationtoken/35817808#35817808