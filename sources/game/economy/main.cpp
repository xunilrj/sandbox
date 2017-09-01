#include <iostream>
#include <map>

template <typename T>
struct console_context
{
  T & Item;
};

template <typename T>
console_context<T> as_console(T & item)
{
  return {item};
}


template <template <typename> typename T1, typename T2>
std::ostream & operator << (std::ostream& out, T1<T2> item)
{
  out << item.Item;
  return out;
}

namespace game
{
  class hex
  {
    public:
      hex(int populationId, int populationQtd)
      {
        population[populationId] = populationQtd;
      }
      
      void passTurn()
      {
        population[0] *= 2;
      }
    private:
    std::map<int,int> population;
    std::map<int,float> demands;
    std::map<int,float> supplies;
    
    friend std::ostream & operator << (std::ostream&, console_context<game::hex>);
  };
  
  std::ostream & operator << (std::ostream& out, console_context<game::hex> ctx)
  {
    auto hex = ctx.Item;
    out << "hex" << std::endl;
    out << "-----------------------" << std::endl;
    
    for(auto x : hex.population)
    {
      out << x.first << ":" << x.second << std::endl;
    }
    
    out << "-----------------------" << std::endl;
    
    return out;
  }
  
  hex make_hex()
  {
    return {0,2};
  }
}

int main(int argc, char ** argv)
{
  auto hex1 = game::make_hex();
  std::cout << as_console(hex1) << std::endl;
  hex1.passTurn();
  std::cout << as_console(hex1) << std::endl;
  return 0;
}