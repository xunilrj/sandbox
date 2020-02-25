class EntityManager
{
public:
	EntityManager() : last_id{0}
	{
	}

    EntityRef new_entity() 
    {
		entities.emplace_back();
		auto& e = entities.back();
		e.id = last_id++;
		return { e.id };
    }
private:
	uint64_t last_id;
	std::vector<Entity> entities;
};