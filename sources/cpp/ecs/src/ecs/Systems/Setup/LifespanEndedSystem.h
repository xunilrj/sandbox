
//sys_lifespan removes entities past their expiration time. No need to run any more logic on them in other systems.
struct LifespanEndedSystem : System</*TODO*/>
{
    virtual void update() override
    {
        foreach([](LifespanComponent& x) {
			if(x.age <= 0)
                remove(x);
		});
    }
};