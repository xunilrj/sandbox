//sys_collide recalculates AABBs based on updated transforms, 
//and detects intersections between them. 
//Our collision detection is only used for triggers and 
//shooting, and it doesn't require any collision response.
// If it was the case, i.e. if entities had rigid bodies and
// needed to be moved back in response to collisions, 
//we'd need to run sys_transform again after sys_collide.
struct CollisionSystem : System</*TODO*/>
{
    virtual void update() override
    {
        //run("tranformSystem");
    }
};