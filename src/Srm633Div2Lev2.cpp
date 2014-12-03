// http://community.topcoder.com/stat?c=problem_statement&pm=13463

#include <cmath>
#include <vector>
#include <iostream>
#include <tuple>
#include <boost/range/numeric.hpp>
#include <boost/range/algorithm.hpp>

//===========================================================
// Srm633Div2Lev2
//-----------------------------------------------------------
// <sum, max, able>
using acc_t = std::tuple<float, float>;
auto sum  = [](const acc_t& a) { return std::get<0>(a); };
auto max  = [](const acc_t& a) { return std::get<1>(a); };

void jumping_able(int x, int y, std::vector<int>&& jumplengths)
{
  // make float list 'xs'
  std::vector<float> xs(jumplengths.size());
  auto to_float = [](auto x) {return static_cast<float>(x); };
  boost::transform(jumplengths, xs.begin(), to_float);
  xs.emplace_back(to_float(std::sqrt(x*x + y*y)));

  // calcuate 'sum' and max'
  auto acc_op = [](acc_t a, float x) { return acc_t{ sum(a) + x, std::max(max(a), x) }; };
  auto acc = boost::accumulate(xs, acc_t{ 0.f, 0.f }, acc_op;

  // calculate 'able' value
  auto able = sum(acc) >= 2 * max(acc);
  std::cout << (able ? "Able" : "Not able") << std::endl;
}

int main()
{
  jumping_able(5, 4, { 2, 5 }); // "Able"
  jumping_able(3, 4, { 4 });   // "Not Able"
  jumping_able(3, 4, { 6 });   // "Not Able"
  jumping_able(0, 1, { 100 , 100});   // "Able"
  jumping_able(300, 400, { 500});   // "Able"
  jumping_able(11, 12, { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 }); // "Able"
  jumping_able(11, 12, { 1, 2, 3, 4, 5, 6, 7, 8, 9, 100 }); // "Not Able"
}
//clang++  -std=c++1y  -ldl -lcxxrt --stdlib=libc++  main.cpp && ./a.out
