// http://community.topcoder.com/stat?c=problem_statement&pm=13463

#include <cmath>
#include <vector>
#include <iostream>
#include <tuple>
#include <iterator>
#include <boost/range/numeric.hpp>
#include <boost/range/algorithm.hpp>

//===========================================================
// Srm633Div2Lev2
//-----------------------------------------------------------
// util
auto to_double = [](auto x) {return static_cast<double>(x);};

auto map = [](auto xs, auto fn) {
    std::vector<decltype(fn(xs[0]))> out;
    boost::transform(xs, std::back_inserter(out), [=](auto& x) { return fn(x); });
    return out;
};

//-----------------------------------------------------------
// helper
using acc_t = std::tuple<double, double>; // <sum, max>
auto sum_  = [](const acc_t& a) { return std::get<0>(a); };
auto max_  = [](const acc_t& a) { return std::get<1>(a); };

//-----------------------------------------------------------
void jumping_able(int x, int y, std::vector<int>&& jumplengths)
{
  // make float list 'xs'
  auto xs = map(jumplengths, to_double);
  xs.emplace_back(std::sqrt(x*x + y*y));

  // calcuate 'sum' and max'
  auto sum = 0.0, max = 0.0;
  auto acc_op  = [](acc_t a, double x) { return acc_t{sum_(a) + x, std::max(max_(a), x) };};
  std::tie(sum, max) = boost::accumulate(xs, acc_t{sum, max}, acc_op);

  // calculate 'able' value
  auto able = max <= sum - max;
  std::cout << (able ? "Able" : "Not able") << std::endl;
}

//-----------------------------------------------------------
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