
struct Shape
{
    virtual float area() const noexcept = 0;
    virtual float circ() const noexcept = 0;
};

struct Circle : public Shape
{
    float R;
    Circle(float r) : R(r){}
    virtual float area() const noexcept { return 3.14*R*R; }
    virtual float circ() const noexcept { return 2.0*3.14*R; }
};

struct Rect : public Shape
{
    float W,H;
    Rect(float w, float h) : W(w), H(h){}
    virtual float area() const noexcept { return W*H; }
    virtual float circ() const noexcept { return 2.0*(W+H); }
};

struct Square : public Shape
{
    float W;
    Square(float w) : W(w){}
    virtual float area() const noexcept { return W*W; }
    virtual float circ() const noexcept { return 4.0*W; }
};

int main(int argc, char ** argv)
{
    return 0;
}