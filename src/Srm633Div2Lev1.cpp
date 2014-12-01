// http://community.topcoder.com/stat?c=problem_statement&pm=13462

#include <vector>
#include <iostream>
#include <numeric>

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
auto target(std::size_t n)
{
    auto accOp = [](auto&& acc, std::size_t n)
    {
        auto gen= [](char c, auto n) { return "#" + std::string(n-2, c) +"#"; };
        
        return 
          list_t{ gen('#', n) }    // -- ["###############"]
        + list_t{ gen(' ', n) }    // -- ["#             #"]
        + map(acc, [](auto& row) { return "# " + row + " #"; })
        + list_t{ gen(' ', n) }    // -- ["#             #"]
        + list_t{ gen('#', n) };   // -- ["###############"]
    };

    // {5, 9, ..., n/4}
    std::vector<std::size_t> l(n/4);
    std::iota(l.begin(), l.end(), 1);
    std::transform(l.begin(), l.end(), l.begin(), [](int n) { return 4*n+1;});
    
    return std::accumulate(l.begin(), l.end(), list_t{"#"}, accOp);
}

//-----------------------------------------------------------
int main()
{
    print (list_t{"n=1" } + target (1)  + list_t{"\n"});
    print (list_t{"n=5" } + target (5)  + list_t{"\n"});
    print (list_t{"n=9" } + target (9)  + list_t{"\n"});
    print (list_t{"n=13"} + target (13) + list_t{"\n"});
    print (list_t{"n=17"} + target (17) + list_t{"\n"});
}

//clang++  -std=c++1y  -ldl -lcxxrt --stdlib=libc++  main.cpp && ./a.out

/*
출력값.

n = 1
#


n = 5
#####
#   #
# # #
#   #
#####


n = 9
#########
#       #
# ##### #
# #   # #
# # # # #
# #   # #
# ##### #
#       #
#########


n = 13
#############
#           #
# ######### #
# #       # #
# # ##### # #
# # #   # # #
# # # # # # #
# # #   # # #
# # ##### # #
# #       # #
# ######### #
#           #
#############


n = 17
#################
#               #
# ############# #
# #           # #
# # ######### # #
# # #       # # #
# # # ##### # # #
# # # #   # # # #
# # # # # # # # #
# # # #   # # # #
# # # ##### # # #
# # #       # # #
# # ######### # #
# #           # #
# ############# #
#               #
#################

*/
