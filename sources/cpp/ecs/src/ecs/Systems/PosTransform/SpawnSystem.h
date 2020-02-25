//sys_shoot spawns projectiles if Shoot.Target is set.
// It runs after sys_transform to ensure that the
// spawn point for projectiles, which is attached 
//to the character in front of their chest, has an up-to-date
struct SpawnSystem : System</*TODO*/>
{
    virtual void update() override
    {
    }
};