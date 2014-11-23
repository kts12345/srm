// http://community.topcoder.com/stat?c=problem_statement&pm=13454

#include <vector>        
#include <iostream>
#include <tuple>
#include <algorithm>
#include <boost/range/algorithm/count_if.hpp>

auto zip3 = [](const auto& xs, 
               const auto& ys, 
               const auto& zs)
{
    std::vector<std::tuple<int, int, int>> zipped;
    for (int i = 0 ; i < xs.size() && i < ys.size() && i < zs.size(); ++i)
        zipped.push_back(std::make_tuple(xs[i], ys[i], zs[i]));
    return zipped;
};

auto tail = [](auto xs)
{
    std::rotate(begin(xs), begin(xs)+1, end(xs));
    xs.resize(xs.size()-1);
    return xs;
};

auto add_head = [](auto x, auto xs)
{
    xs.push_back(x);
    std::rotate(begin(xs), end(xs)-1, end(xs));
    return xs;
};

auto add_last = [](auto xs, auto x)
{
    xs.push_back(x);
    return xs;
};

//-------------------------------------------------------------

int countPeaks(std::vector<int> xs)
{
    if (xs.empty()) 
        return 0;
        
    const auto g  = *std::min_element(begin(xs), end(xs)) - 1;
    const auto ls = add_head(g, xs);
    const auto rs = add_last(tail(xs), g);
    const auto z3 = zip3(add_head(g, xs), xs, add_last(tail(xs), g));
    const auto is_peak = [](auto t) 
    {
        int l, x, r; 
        std::tie(l, x, r) = t;        
        return l <x && x > r;
    };
    
    return boost::count_if(z3, is_peak);
}

int main()
{
    std::cout<< countPeaks ({5, 6, 2, 4}) << std::endl;
    std::cout<< countPeaks ({1, 1, 1, 1, 1, 1, 1}) << std::endl;
    std::cout<< countPeaks ({2, 1}) << std::endl;
    std::cout<< countPeaks ({2,5,3,7,2,8,1,3,1}) << std::endl;
    std::cout<< countPeaks ({1}) << std::endl;
    std::cout<< countPeaks ({1,2,3,4,4,3,2,1}) << std::endl;
    std::cout<< countPeaks ({}) << std::endl;
}