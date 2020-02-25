// sys_select translates the current mouse position 
// into a world position and uses ray casting to 
// pick a collider under the cursor. 
// This information is then used in sys_control_player 
// to set navigation and shooting targets.
struct MousePickSystem : System</*TODO*/>
{
    virtual void update() override
    {
        auto ray = GetRay();
        foreach([](ColliderComponent& x) {
			...
		});

        //or

        fold([](ColliderComponent& x, Current& c){
            auto distance = distance(ray, x);
            if(distance < c.distance)
            {
                c = x;
            }
            return c;
        });
    }
};