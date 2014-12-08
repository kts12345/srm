// Srm632Div2Lev2 PotentialGeometricSequence
// http://community.topcoder.com/stat?c=problem_statement&pm=13390

#include <vector>
#include <iostream>
#include <tuple>
#include <boost/range/adaptors.hpp>
#include <boost/range/numeric.hpp>

//===========================================================
// Srm633Div2Lev2
//-----------------------------------------------------------

namespace haskell {
  //------------------------------------------------------------------
  // head
  // ex)
  // xs := [a] + [b, c, d]
  // head(xs) = a
  template <typename T>
  T head(const std::vector<T>& xs) { return xs[0]; };

  //------------------------------------------------------------------
  // operator +
  // ex)
  // lhs := [a, b, c]
  // rhs := [d, e]
  // lhs + rhs == [a, b, c, d, e] 
  template <typename T>
  std::vector<T> operator+(std::vector<T> lhs, const std::vector<T>& rhs)
  {
    lhs.insert(lhs.end(), rhs.begin(), rhs.end());
    return lhs;
  }

  //------------------------------------------------------------------
  // zip
  // ex)
  //  xs := [ 1,     2,     3,     4   ]
  //  ys := [   a,     b,     c,     d ]
  //  zs == [(1,a), (2,b), (3,c), (4,d)] 
  auto zip = [](const auto& xs, const auto& ys)
  {
    using t = std::decay_t<decltype(xs[0])>;
    using u = std::decay_t<decltype(ys[0])>;
    std::vector<std::tuple<t, u>> zs;
    zs.reserve(std::min(xs.size(), ys.size()));
    for (int i = 0; i < xs.size() && i < ys.size(); ++i)
      zs.push_back(std::make_tuple(xs[i], ys[i]));
    return zs;
  };

  //------------------------------------------------------------------
  // scanl
  // ex)
  //  handler := (+)
  //  init    :=  5
  //  xs      := [1, 2,  3,  4]
  //  ys      == [6, 8, 11, 15] 
  auto scanl = [](const auto handler, const auto& init, const auto& xs)
  {
    using event_t = std::decay_t<decltype(xs[0])>;
    using out_t   = std::decay_t<decltype(handler(init, xs[0]))>;

    std::vector<out_t> ys;
    ys.reserve(xs.size());

    out_t acc = init;
    for (auto& x : xs)
      ys.push_back(acc = handler(acc,x));
    
    return ys;
  };

} // end of namespace haskell

//--------------------------------------------------------------
// typedef 

using list_t = std::vector<int>;

// diff_run_t  := <diff, run_length>
using diff_run_t = std::tuple < int, int >;
auto diff(const diff_run_t& val) { return std::get<0>(val); };
auto run (const diff_run_t& val) { return std::get<1>(val); };

// prev_cur_t := <previous, current>
using prev_cur_t = std::tuple < int, int >;
auto prev(const prev_cur_t& val) { return std::get<0>(val); };
auto cur (const prev_cur_t& val) { return std::get<1>(val); };

//---------------------------------------------------------------
// event handler
diff_run_t handler(const diff_run_t& acc, const prev_cur_t event)
{
  int diff, run; std::tie(diff, run) = acc;
  int prev, cur; std::tie(prev, cur) = event;
  
  const auto new_diff = cur - prev;
  const auto new_run  = new_diff == diff ? run + 1 : 2;
  
  return diff_run_t{new_diff, new_run};
}

//---------------------------------------------------------------
// composition
int number_of_subsequences(const std::vector<int>& xs)
{
  using namespace haskell;
  using namespace boost;
  using namespace boost::adaptors;

  const auto zs   = zip(list_t{0} + xs, xs);  //  [(prev, current)]

  const auto diff = head(xs);  // init_diff
  const auto run  = 0;         // init_run

  return accumulate(scanl(handler, diff_run_t{diff, run}, zs)
                    | transformed(cur)
                    , 0);
}

int main()
{
  auto print = [](auto x) { std::cout << x << std::endl; };

  print(number_of_subsequences({ 0, 1, 2 }));
  print(number_of_subsequences({ 1,2,4 }));
  print(number_of_subsequences({ 3,2,1,0 }));
  print(number_of_subsequences({ 1,2,4,8,16 }));
  print(number_of_subsequences({ 1,3,5,5,5,5,64,4,23,2,3,4,5,4,3 }));
}
//clang++  -std=c++1y  -ldl -lcxxrt --stdlib=libc++  main.cpp && ./a.out

