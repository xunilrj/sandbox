#include "../deltaTime/deltaTime.h"
#include "../TaskSystem/TaskSystem.h"
#include "../ecs/ecs.h"

// EXAMPLE 1

struct PositionComponent
{
	COMPONENTID(1, DenseStore<PositionComponent>);
	float x, y, z;
};

struct RigidBodyComponent
{
	COMPONENTID(2, DenseStore<RigidBodyComponent>);
	float vx, vy, vz;
	float ax, ay, az;
};

struct EulerIntegratorSystem : System<DeltaTimeComponent, PositionComponent, RigidBodyComponent>
{
    virtual void update() override
    {
		foreach([](DeltaTimeComponent& time, PositionComponent& pos, RigidBodyComponent& rb) {
			pos.x += rb.vx * time.dt;
			pos.y += rb.vy;
			pos.z += rb.vz;

			rb.vx += rb.ax;
			rb.vy += rb.ay;
			rb.vz += rb.az;
		});
    }
};

#include <iostream>
#include <iomanip>

bool input_enabled = true;
void enable_input(Scene& s) { input_enabled = true; }
void disable_input(Scene& s) { input_enabled = false; }

int main()
{
    auto scene = Scene{};
	scene.add(0, disable_input);
	scene.add(1, enable_input);

	//TODO InputSystem
	//TODO System Order (Serial, Parallel)
	//TODO Rendering
	//TODO Animation
	scene
		.add<EulerIntegratorSystem>();
	//auto anim = scene.add<AnimationSystem>(); //TODO

	//TODO entity serialization
	auto e1 = scene.build_entity()
		.set(PositionComponent{ 0, 0, 0 })
		.set(RigidBodyComponent{ 1, 0, 0, 0, 0, 0})
		.build();

	auto e2 = scene.build_entity()
		.set(PositionComponent{ 0, 0, 0 })
		.build();

	auto anim1 = scene.build_anim()
		.then(AnimManager::lerp(1, 0, 1))
		.then(AnimManager::lerp(2, 1, 0.5));
	auto anim2 = scene.build_anim()
		.then(AnimManager::lerp(1, 0, 1));
	
	
	/*scene.components.build_copier(
		e2, &InterpolationComponent::last,
		e2, &PositionComponent::x
	);*/

	auto hStdout = GetStdHandle(STD_OUTPUT_HANDLE);
	//deltaTime return uint64_t milliseconds
	deltaTime<double> dt{ 16, true };
	while (true)
	{
		auto elapsed = dt.step();
		if (elapsed > 0) {
			
			auto& int2 = scene.get<PositionComponent>(e2);

			if (input_enabled && (GetKeyState('A') & 0x8000))
			{
				scene.call(0);
				auto& anim_i1 = scene.start(anim1, e2, &PositionComponent::x);
				auto& anim_i2 = scene.start_after(anim2, e2, &PositionComponent::y, &anim_i1);
				scene.call(1, &anim_i2);
			}

			scene.set<DeltaTimeComponent>({ (float)(elapsed / 1000.0) });
			scene.update();

			std::cout.precision(4);
			std::cout << std::setw(4) << int2.x << " " << std::setw(4) << int2.y << std::endl;
		}
	}
    return 0;
}