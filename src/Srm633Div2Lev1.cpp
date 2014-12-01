// http://community.topcoder.com/stat?c=problem_statement&pm=13462

#include <vector>
#include <iostream>

//-----------------------------------------------------------
// typedef && Util

using list_t = std::vector<std::string>;

list_t operator+(list_t&& lhs, const list_t& rhs)
{ 
    lhs.insert(lhs.end(), rhs.begin(), rhs.end());
    return lhs;
}

auto map = [](auto&& list, auto fn)
{
    std::transform(list.begin(), list.end(), list.begin(), 
                   [=](auto& x) { return fn(x); });
    return list;
};

auto print = [](auto&& table)
{ 
    for(auto& e : table) 
        std::cout << e << std::endl; 
};

//===========================================================
// Srm633Div2Lev1
//-----------------------------------------------------------
auto gen= [](char c, auto n) { return "#" + std::string(n-2, c) +"#"; };

//-----------------------------------------------------------

auto target(std::size_t n)
{
    if (n ==1) 
        return list_t{ std::string("#") };
    else
        return list_t{ gen('#', n) }            // -- ["###############"]
             + list_t{ gen(' ', n) }            // -- ["#             #"]
             + map(target(n-4), [](auto& row) { return "# " + row + " #"; })
             + list_t{ gen(' ', n) }            // -- ["#             #"]
             + list_t{ gen('#', n) };           // -- ["###############"]
}

//-----------------------------------------------------------
int main()
{
    print (list_t{"n=5" } + target (5)  + list_t{"\n"});
    print (list_t{"n=9" } + target (9)  + list_t{"\n"});
    print (list_t{"n=13"} + target (13) + list_t{"\n"});
    print (list_t{"n=17"} + target (17) + list_t{"\n"});
}

//clang++  -std=c++1y  -ldl -lcxxrt --stdlib=libc++  main.cpp && ./a.out
