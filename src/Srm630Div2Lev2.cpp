// Srm630Div2Lev2 Egalitarianism3Easy
// http://community.topcoder.com/stat?c=problem_statement&pm=13376
#include <iostream>
#include <string>
#include <map>
#include <vector>
#include <boost/iterator/zip_iterator.hpp>
#include <boost/algorithm/cxx11/all_of.hpp>
#include <boost/range/algorithm.hpp>
#include <boost/range/numeric.hpp>
#include <boost/range/algorithm/max_element.hpp>

using path_t     = std::pair<std::pair<int, int>, int>;
using table_t    = std::map<std::pair<int, int>, int>;
using cluster_t  = std::pair<int, std::vector<int>>;
using clusters_t = std::vector<cluster_t>;

auto update_clusters = [](clusters_t& clusters, std::vector<int> nodes, table_t& table)->clusters_t
{ 
  auto expand_cluster = [&](int n, cluster_t& cluster) -> cluster_t
  {
      auto same_distance = [&](int n2)
      {
        auto it = table.find(std::make_pair(n, n2));
        if (it == table.end())
          it = table.find(std::make_pair(n2, n));
        if (it == table.end())
          return false;
        return it->second == cluster.first;
      };

      const auto joinable = boost::algorithm::all_of(cluster.second, same_distance);
      if (joinable)
        cluster.second.push_back(n);

      return cluster;
    };
 
  auto expand_clustsers = [&](clusters_t& clusters, int n)
  {
    for (auto& cluster : clusters)
      expand_cluster(n, cluster);
    return clusters;
  };

  auto new_clusters = boost::accumulate(nodes, clusters, expand_clustsers);
  return new_clusters;
 };

auto find_newpath = [](table_t& table, const path_t& path)
{
  auto l = path.second;
  auto connect = [&](int a, int b)
  {
    table_t found;
    for (auto& p : table)
    {
      if (p.first.first == b)
        found[std::make_pair(a, p.first.second)] = l + p.second;
      if (p.first.second == a)
        found[std::make_pair(p.first.first, b)] = p.second + l;
      for (auto& p2 : table)
      {
        if (p.first.second == a && p2.first.first == b)
          found[std::make_pair(p.first.first, p2.first.second)] = p.second + p2.second;
      }
    }
    return found;
  };

  auto found1 = table_t{ {path.first, path.second} };
  auto found2 = connect(path.first.first,  path.first.second);
  auto found3 = connect(path.first.second, path.first.first);

  found1.insert(found2.begin(), found2.end());
  found1.insert(found3.begin(), found3.end());

  return found1;
};

auto handler = [](std::pair<clusters_t, table_t>& state, path_t path)->std::pair<clusters_t, table_t>
{
  auto& table = state.second;
  auto new_paths  = find_newpath(table, path);
  table.insert(new_paths.begin(), new_paths.end());
  auto new_table = table;
  std::vector<int> nodes;
  for (auto& p : new_table)
  {
    nodes.push_back(p.first.first);
    nodes.push_back(p.first.second);
  }
  boost::sort(nodes);
  boost::unique(nodes);
  auto& clusters = state.first;
  auto clusters2 = update_clusters(clusters, nodes, new_table);
  auto& new_clusters = clusters2;
  for (auto p : new_paths)
    new_clusters.emplace_back(p.second, std::vector<int>{ p.first.first, p.first.second});

  auto new_state =  std::make_pair(new_clusters, new_table);
  return new_state;
};

auto egalitarianism3_easy = [](int n, const std::vector<int>& as, 
                                      const std::vector<int>& bs, 
                                      const std::vector<int>& lens)
{
  if (n <= 2)
    return n;

  auto begin = boost::make_zip_iterator(boost::make_tuple(as.begin(), bs.begin(), lens.begin()));
  auto end   = boost::make_zip_iterator(boost::make_tuple(as.end(),   bs.end(),   lens.end()));
  std::vector<path_t> pathinfos;
  std::transform(begin, end, std::back_inserter(pathinfos), 
  [](const boost::tuple<const int&, const int&, const int&>& t) {
      return path_t{{t.get<0>(), t.get<1>()}, t.get<2>() };
  });
  
  auto clusters = boost::accumulate(pathinfos, std::make_pair(clusters_t{}, table_t{}),handler).first;
  
  std::vector<int> sizes;
  boost::transform(clusters, std::back_inserter(sizes), [](auto& cluster)
  { return cluster.second.size(); });

  return *boost::max_element(sizes);
};

int main()
{
  auto print = [](const auto& val) { std::cout << val << std::endl; };
  
  print(egalitarianism3_easy(4,  {1,1,1}, 
                                 {2,3,4}, 
                                 {1,1,1}));
                                 
  print(egalitarianism3_easy(6,  {1,2,4,2,3},
                                 {2,5,3,3,6},
                                 {2,2,3,1,3}));
                                 
  print(egalitarianism3_easy(10, {1,1,1,1,1,1,1,1,1},
                                 {2,3,4,5,6,7,8,9,10},
                                 {1000,1000,1000,1000,1000,1000,1000,1000,1000}));
                                   
  print(egalitarianism3_easy(2,  {1},
                                 {2},
                                 {3}));

  print(egalitarianism3_easy(1, {},
                                {},
                                {}));
}

/*  Output
3
3
9
2
1
*/