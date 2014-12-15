//--Srm631Div2Lev2 catsOnTheLine
//-- http://community.topcoder.com/stat?c=problem_statement&pm=13392

#include <cmath>
#include <vector>
#include <iostream>
#include <tuple>
#include <iterator>
#include <string>
#include <boost/range/numeric.hpp>
#include <boost/range/algorithm.hpp>
#include <boost/optional.hpp>


namespace haskell {
  using boost::optional;

  //cJust cond value = if cond then(Just value) else Nothing
  auto cJust = [](auto cond, const auto& value)
  {
    using return_t = boost::optional < std::decay_t<decltype(value)>>;
    return cond ? return_t{ cond } : boost::none ;
  };

  auto foldM = [](auto handler, const auto& init, const auto& xs)
  {
    using maybe_t = boost::optional < std::decay_t<decltype(init)>>;
    maybe_t  acc = init;
    for (auto &x : xs)
    {
      if (!acc)
        return maybe_t{ boost::none };
      acc = handler(*acc, x);
    }
    return acc;
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

  const auto last = [](const auto& xs)
  { // assert (0<xs.size());
    return xs[xs.size() - 1];
  };

} // end of namespace haskell

using namespace haskell;

// handler ends (lo, hi, cnt) = cJust isPossible(ends++[end])
//     where(start, end) = (max(1 + last ends) lo, start + cnt - 1)
//     isPossible = end <= hi
using event_t = std::tuple<int, int, int>;
using acc_t   = std::vector<int>;
optional<acc_t> handler(acc_t ends, const event_t&  evt)
{
  int lo, hi, cnt; std::tie(lo, hi, cnt) = evt;
  const auto start = std::max(1 + last(ends), lo);
  const auto end   = start + cnt - 1;
  ends.emplace_back(end);
  const auto ispossible = end <= hi;
  return cJust(ispossible, ends);
}

// handler' events evt  = cJust isPossible events'
//     where events'    = insert evt events
//           isPossible = isJust $ foldM_ handler [-2001] events'
using events_t = std::vector<event_t>;
optional<events_t> handler1(events_t events, const event_t& evt)
{
  events_t event1; events_t single{ evt };
  std::merge(begin(events), end(events), begin(single), end(single), std::back_inserter(event1));
  auto ispossible = (foldM(handler, acc_t{ -20001 }, event1)) ? true : false;
  return cJust(ispossible, event1);
}

// catsOnTheLine ps cs time = toString $ foldM_ handler' [] events
//     where events = [(p - time, p + time, cnt) | (p, cnt)<-zip ps cs]
//           toString v = if isJust v then "Possible" else "Impossible"
std::string cats_on_the_line(const std::vector<int>& ps, const std::vector<int>& cs, int time)
{
  auto tostring = [](const auto& v) { return v ? "Possible" : "Impossible"; };
  auto events   = zipwith([&](auto p, auto cnt) { return event_t{ p - time, p + time, cnt }; }, ps, cs);
  return tostring(foldM(handler1, events_t{}, events));
}

//-----------------------------------------------------------
int main()
{
  auto print = [](const auto& v) { std::cout << v << std::endl; };
  print( cats_on_the_line({0},{7}, 3) );
  print( cats_on_the_line({0},{8}, 2) );
  print( cats_on_the_line({0, 1},{3, 1}, 0) );
  print( cats_on_the_line({5, 0, 2},{2, 3, 5}, 2) );
  print( cats_on_the_line({5, 1, -10, 7, 12, 2, 10, 20},{3, 4, 2, 7, 1, 4, 3, 4}, 6) );
  return 0;
}

/* Output
"Possible"
"Impossible"
"Impossible"
"Impossible"
"Possible"
*/

//clang++  -std=c++1y  -ldl -lcxxrt --stdlib=libc++  main.cpp && ./a.out
