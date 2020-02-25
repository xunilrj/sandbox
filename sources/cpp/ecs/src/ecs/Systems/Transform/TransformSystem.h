//sys_transform
//This is one of the most important systems in the game.
// It commits changes to translation and rotation into a
// translation matrix stored in Transform.World.
// The changes may originate from player's input, AI
// decisions, or animations. In case of nested transforms,
// it also takes into account the changes to parent transforms.
// The World matrix is then sent to the GPU for rendering.
struct TransformSystem : System</*TODO*/>
{
    virtual void update() override
    {
    }
};