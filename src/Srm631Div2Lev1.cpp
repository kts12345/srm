// Srm630Div2Lev1 DoubleLetter
// http://community.topcoder.com/stat?c=problem_statement&pm=13378

#include <string>
#include <stack>
#include <numeric>
#include <iostream>
#include <boost/range/numeric.hpp>

using event_t = char;
using stack_t = std::stack<char>;

auto handler = [](stack_t& stack, const event_t& evt) -> stack_t&
{
  if (stack.empty())
    stack.push(evt);
  else
  {
    if (evt == stack.top())
      stack.pop();
    else
      stack.push(evt);
  }
  return stack;
};

auto doubleLetter = [](const std::string& xs)
{
  stack_t stack;
  auto result = boost::accumulate(xs, stack, handler);
  if (result.empty())
    return "Possible";
  else
    return "Impossibel";
};

int main()
{
  auto print = [](const auto& val) { std::cout << val << std::endl; };
  print(doubleLetter("aabccb"));
  print(doubleLetter("aabccbb"));
  print(doubleLetter("abcddcba"));
  print(doubleLetter("abab"));
  print(doubleLetter("aaaaaaaaaa"));
  print(doubleLetter("aababbabbaba"));
  print(doubleLetter("zzxzxxzxxzzx"));
}
//clang++  -std=c++1y  -ldl -lcxxrt --stdlib=libc++  main.cpp && ./a.out
/*
output
Possible
Impossibel
Possible
Impossibel
Possible
Impossibel
Possible
*/