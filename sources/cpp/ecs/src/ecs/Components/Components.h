#define COMPONENTID(x, store) static const uint32_t ID = x; using TSTORE = store;
#define RESOURCE(x, t) static const uint32_t RID = x; using TSTORE = SingletonStore<t>;
