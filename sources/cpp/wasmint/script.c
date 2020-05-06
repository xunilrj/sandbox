void lerp(float duration, float start, float end);

#define WASM_EXPORT __attribute__((visibility("default")))

WASM_EXPORT
int helloWorld1() {
    lerp(1, 0, 1);
    return 42;
}

WASM_EXPORT
int helloWorld2() {
    return 21;
}