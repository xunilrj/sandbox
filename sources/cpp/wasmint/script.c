extern void Print(int size, const char* str);

extern int Wait(int id);
extern int WalkTo(int actorid, int x, int y);
extern int StartTalk(int actorid, int talkid);

#define WASM_EXPORT __attribute__((visibility("default")))

// func Look_CannonFoddler
//     sequence
//         select guybrush, walkto 216+65 184+145
//         talk2 GuybrushLooksLikeRamrod
//     end
WASM_EXPORT
void Look_CannonFoddler()
{
    Print(6, "Daniel");

    int wait0 = WalkTo(0, 216+65, 184+145);
    Wait(wait0);
    int wait1 = StartTalk(0, 0);
    Wait(wait1);
}