//-- Srm631Div2Lev1 TaroGrid
//-- http://community.topcoder.com/stat?c=problem_statement&pm=13394

#include <cmath>
#include <vector>
#include <iostream>
#include <tuple>
#include <iterator>
#include <boost/range/numeric.hpp>
#include <boost/range/algorithm.hpp>

namespace haskell {
auto zipwith3 = [](auto op, const auto& x1, const auto& x2, const auto& x3)
{
  std::vector<decltype(op(x1[0], x2[0], x3[0]))> out;
  auto b1 = x1.begin();
  auto b2 = x2.begin();
  auto b3 = x3.begin();
  for (; b1 != x1.end() && b2 != x2.end() && b3 != x3.end(); ++b1, ++b2, ++b3)
    out.emplace_back(op(*b1, *b2, *b3));
  return out;
};
auto zipwith = [](auto op, const auto& x1, const auto& x2)
{
  std::vector<decltype(op(x1[0], x2[0]))> out;
  auto b1 = x1.begin();
  auto b2 = x2.begin();
  for (; b1 != x1.end() && b2 != x2.end(); ++b1, ++b2)
    out.emplace_back(op(*b1, *b2));
  return out;
};

auto foldl = [](const auto& xs, const auto& init, auto handler)
{
  return std::accumulate(xs.begin(), xs.end(), init, handler);
};

auto maximum = [](const auto& xs)
{ // assert (0<xs.size());
  return *std::max_element(xs.begin(), xs.end());
};
} // end of namespace haskell

using namespace haskell;

//handler (cnts,maxs,prevs) nows = (cnts',maxs',nows)
//   where cnts' = zipWith3 (\p n c -> if p==n then c+1 else 1) prevs nows cnts
//         maxs' = zipWith max cnts' maxs
using count_t = std::vector<int>;
using max_t = std::vector<int>;
using event_t = std::string;
using acc_t = std::tuple<count_t, max_t, event_t>;

acc_t handler(const acc_t& acc, const event_t& nows)
{
  const count_t& cnts  = std::get<0>(acc);
  const max_t  & maxs  = std::get<1>(acc);
  const event_t& prevs = std::get<2>(acc);
  auto cnts2 = zipwith3([](auto& p, auto& n, auto&c)->int {return (p == n) ? (c + 1) : 1; },
                        prevs, nows, cnts);
  auto maxs2 = zipwith([](auto a, auto b) { return std::max(a, b); },
                        cnts2, maxs);
  return acc_t{ cnts2, maxs2, nows };
}

//------------------------------------------------------
//taroGrid xs = maximum $(\(_, m, _)->m) $ foldl handler([0, 0..], [0, 0..], head xs) xs
int taroGrid(const std::vector<std::string>& xs)
{
  const count_t initcnt( xs[0].size(), 0 );
  const max_t   initmax( xs[0].size(), 0 );
  const auto init = acc_t{ initcnt, initmax, xs[0] };
  auto result = foldl(xs, init, handler);
  auto maxs   = std::get<1>(result);
  return maximum(maxs);
}

//-----------------------------------------------------------
int main()
{
  auto print = [](auto val) {std::cout << val << std::endl; };
  print ( taroGrid({"W"}) );
  print ( taroGrid({"WB", "BW"}));
  print ( taroGrid({"BWW", "BBB", "BWB"}) );
  print ( taroGrid({"BWBW", "BBWB", "WWWB", "BWWW"}) );
  print ( taroGrid({"BWB", "BBW", "BWB"}) );
  print ( taroGrid({"BBWWBBWW", "BBWWBBWW", "WWBBWWBB", "WWBBWWBB", "BBWWBBWW", "BBWWBBWW", "WWBBWWBB", "WWBBWWBB"}) );
}
//clang++  -std=c++1y  -ldl -lcxxrt --stdlib=libc++  main.cpp && ./a.out

/*
output
1
1
3
3
3
2
*/
