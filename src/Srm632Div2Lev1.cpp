// Srm632Div2Lev1 RunningAroundPark
// http://community.topcoder.com/stat?c=problem_statement&pm=13391

#include <vector>
#include <iostream>
#include <tuple>
#include <boost/range/adaptors.hpp>
#include <boost/range/numeric.hpp>
#include <boost/range/algorithm/copy.hpp>

namespace haskell {
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
  // filter
  // ex)
  //  handler := even
  //  xs      := [1, 2,  3,  4]
  //  ys      == [   2,      4] 
  auto filter = [](const auto handler, const auto& xs)
  {
    using namespace boost;
    using namespace boost::adaptors;

    std::decay_t<decltype(xs)> ys;
    copy(xs|filtered(handler), std::back_inserter(ys));
    return ys;
  };

} // end of namespace haskell

using namespace haskell;

//--------------------------------------------------------------
// typedef 

using list_t = std::vector<int>;

// prev_cur_t := <previous, current>
using prev_cur_t = std::tuple < int, int >;

//---------------------------------------------------------------
// event handler
bool handler(const prev_cur_t event)
{
  int prev, cur; std::tie(prev, cur) = event;
  return prev >= cur;
}

//---------------------------------------------------------------
// composition
int number_of_lap(int n, const std::vector<int>& xs)
{
  return filter(handler, 
                zip(list_t{n + 1} +xs, xs))
        .size();
}

int main()
{
  auto print = [](auto x) { std::cout << x << std::endl; };

  print(number_of_lap(3, {1,2,3}));
  print(number_of_lap(24,{6,6}));
  print(number_of_lap(3, {3,2,1}));
  print(number_of_lap(50,{1,3,5,7,9,2,4,6,8,10}));
}
//clang++  -std=c++1y  -ldl -lcxxrt --stdlib=libc++  main.cpp && ./a.out

