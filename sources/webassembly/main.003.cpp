

template <typename T>
struct vec3
{
    T x, y, z;

    vec3<T> operator+(const vec3<T> &r) const
    {
        return {x + r.x, y + r.y, z + r.z};
    }
};

using vec3f = vec3<float>;

__attribute__((export_name("make_vec3f"))) vec3f
make_vec3f(float x, float y, float z) { return {x, y, z}; }

static float data[] = {0.0f, 1.0f, 2.0f, 3.0f, 4.0f};
__attribute__((export_name("set"))) void
set(int i, float a) { data[i] = a; }
__attribute__((export_name("get"))) float
get(int i) { return data[i]; }