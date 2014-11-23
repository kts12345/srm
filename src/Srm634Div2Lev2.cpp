-- http://community.topcoder.com/stat?c=problem_statement&pm=13458

#include <vector>        
#include <iostream>
#include <boost/range/numeric.hpp>

auto minValue = [](int n, std::vector<int> xs)
{
  using namespace boost;
  return std::max(0, accumulate(xs, n, [n](int s, int e){return s-n+e;}));
};

int main()
{
  std::cout<< minValue(5,   {3,3})        << std::endl;
  std::cout<< minValue(100, {97})         << std::endl;
  std::cout<< minValue(10,  {9,9,9,9,9})  << std::endl;
  std::cout<< minValue(7,   {1,2,3})      << std::endl;
  std::cout<< minValue(5,   {3,3,3})      << std::endl;
}