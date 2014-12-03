// http://community.topcoder.com/stat?c=problem_statement&pm=13462
#include <vector>
#include <iostream>
#include <boost/range/numeric.hpp>
#include <boost/range/algorithm.hpp>
#include <boost/range/irange.hpp>

//-----------------------------------------------------------
// typedef & Util

using list_t = std::vector<std::string>;

list_t operator+(list_t lhs, const list_t& rhs)
{ 
    lhs.insert(lhs.end(), rhs.begin(), rhs.end());
    return lhs;
}

auto map = [](auto xs, auto fn)
{
    boost::transform(xs, xs.begin(), [=](auto& x) { return fn(x); });
    return xs;
};

//===========================================================
// Srm633Div2Lev1
//-----------------------------------------------------------
auto acc_op = [](auto&& acc, int x)
{
    auto fill = [=](char e) { return std::string(x-4, e); };
    return 
      list_t{"##" + fill('#') + "##"}  //["###########"]
    + list_t{"# " + fill(' ') + " #"}  //["#         #"]
    + map(acc, [](auto& a){ return        "# "+ a +" #";}) 
    + list_t{"# " + fill(' ') + " #"}  //["#         #"]
    + list_t{"##" + fill('#') + "##"}; //["###########"]
};
    
auto target(int n)
{
    using start_t = int;    
    using last_t  = int;
    using step_t  = int;
    using init_t  = list_t;
    
    auto xs = boost::irange(start_t{5}, last_t{n<5?5:n+1}, step_t{4}); // [5,9,..,n]
    return boost::accumulate(xs, init_t{"#"}, acc_op);
}

//-----------------------------------------------------------
int main()
{
    auto print = [](const auto& table){ 
        for(const auto& e : table) 
            std::cout << e << std::endl; 
    };

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
