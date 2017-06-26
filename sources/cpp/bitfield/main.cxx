#include <iostream>
#include <gtest/gtest.h>

#include <cstdint>
template <unsigned bits> struct Store;
template <> struct Store<8> { typedef uint8_t Type; };
template <> struct Store<16> { typedef uint16_t Type; };
template <> struct Store<32> { typedef uint32_t Type; };
template <> struct Store<64> { typedef uint64_t Type; };

namespace staticmath{
  template <unsigned ...> struct Sum;
  template <unsigned x> struct Sum<x>{enum {value=x};};
  template <unsigned x, unsigned ... numbers>
    struct Sum<x, numbers...> {
      enum {value = x + Sum<numbers...>::value};
    };
}

template <unsigned... sizes>
class Bitfield{
  typename Store<staticmath::Sum<sizes...>::value>::Type store;
  public:
  template <unsigned pos, unsigned b4, unsigned size, unsigned... more>
  friend unsigned getImpl(Bitfield<size, more...>);
};

TEST(Bitfield, A){
  static_assert(staticmath::Sum<1,2,3>::value == 6, "problem in store size calculation");
}
