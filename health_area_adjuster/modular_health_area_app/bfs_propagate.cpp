// bfs_propagate.cpp
//
// Compiled Dijkstra/BFS implementation for health area / team area boundary
// propagation. Used identically for both — a team-area call passes the
// health area's own cells as the universe and its boundary as a hard
// barrier, but the algorithm itself doesn't know the difference.
//
// v2: compactness_pen and max_cost_val removed. Both were always inert in
// every call site in the v1 app (compactness_pen = 0, max_cost = Inf), so
// this is pure simplification — no behavior change.
//
// [[Rcpp::depends(Rcpp)]]

#include <Rcpp.h>
#include <queue>
#include <vector>
#include <cmath>
#include <limits>
#include <string>

using namespace Rcpp;

// ---------------------------------------------------------------------------
// friction_to_cost — mirrors the R version exactly
//   min_cost=1, soft_block_cost=1000, knee=0.90, power_low=2, power_high=6
// ---------------------------------------------------------------------------
inline double friction_to_cost(double x,
                               double min_cost        = 1.0,
                               double soft_block_cost = 1000.0,
                               double knee            = 0.90,
                               double power_low       = 2.0,
                               double power_high      = 6.0) {
  if (x < 0.0) x = 0.0;
  if (x > 1.0) x = 1.0;
  if (x <= knee) {
    return min_cost + std::pow(x / knee, power_low) * 99.0;
  } else {
    return 100.0 + std::pow((x - knee) / (1.0 - knee), power_high) *
      (soft_block_cost - 100.0);
  }
}

// ---------------------------------------------------------------------------
// Priority queue entry
// ---------------------------------------------------------------------------
struct QEntry {
  double cost;
  int    cell;   // 0-based
  int    owner;  // 0-based area index
  bool operator>(const QEntry& o) const { return cost > o.cost; }
};

// ---------------------------------------------------------------------------
// bfs_propagate_cpp
//
// Arguments (all 1-based R indices converted to 0-based internally):
//   n_cells          : total number of grid cells
//   neighbors        : List of IntegerVector, each element = 1-based neighbour indices
//   cell_friction_raw: NumericVector length n_cells, values in [0,1]
//   seed_cells       : IntegerVector of 1-based seed cell indices (one per area)
//   starter_cells_list: List of IntegerVector — expanded starter cells per area (1-based)
//   barrier_mat      : List of IntegerVector — 1-based neighbour indices that are
//                      barrier crossings for each cell (hard block)
//   subdiv_mat       : List of IntegerVector — 1-based neighbour indices that are
//                      subdivision crossings for each cell (soft penalty)
//   subdiv_penalty   : double, raw friction addition for subdivision crossings
//   base_step_fric   : double
//   use_pop          : bool
//   cell_pop         : NumericVector length n_cells (ignored if !use_pop)
//   target_pop       : double (ignored if !use_pop)
//   pop_sat_pct      : double
//   pop_sat_weight   : double
//   pop_sat_max      : double
//   n_areas          : int
//
// Returns a List with:
//   owner      : IntegerVector of 0-based area indices (-1 = unassigned)
//   best_cost  : NumericVector of cumulative costs
// ---------------------------------------------------------------------------

// [[Rcpp::export]]
List bfs_propagate_cpp(
    int            n_cells,
    List           neighbors,
    NumericVector  cell_friction_raw,
    IntegerVector  seed_cells,          // 1-based
    List           starter_cells_list,  // 1-based per area
    List           barrier_mat,         // 1-based, length n_cells; empty = no barriers
    List           subdiv_mat,          // 1-based, length n_cells; empty = no subdivisions
    double         subdiv_penalty,
    double         base_step_fric,
    bool           use_pop,
    NumericVector  cell_pop,
    double         target_pop,
    double         pop_sat_pct,
    double         pop_sat_weight,
    double         pop_sat_max,
    int            n_areas
) {
  const double INF = std::numeric_limits<double>::infinity();
  const bool has_barriers  = (barrier_mat.size()  == (size_t)n_cells);
  const bool has_subdiv    = (subdiv_mat.size()   == (size_t)n_cells);

  std::vector<double> best_cost(n_cells, INF);
  std::vector<int>    owner(n_cells, -1);
  std::vector<bool>   visited(n_cells, false);
  std::vector<double> area_pop(n_areas, 0.0);

  std::priority_queue<QEntry, std::vector<QEntry>, std::greater<QEntry>> pq;

  // ── Seed initialisation ───────────────────────────────────────────────────
  for (int a = 0; a < n_areas; a++) {
    IntegerVector starters = starter_cells_list[a];
    for (int k = 0; k < starters.size(); k++) {
      int ci = starters[k] - 1;   // to 0-based
      if (best_cost[ci] > 0.0) {
        best_cost[ci] = 0.0;
        owner[ci]     = a;
        pq.push({0.0, ci, a});
        if (use_pop) area_pop[a] += cell_pop[ci];
      }
    }
  }

  // ── Main Dijkstra loop ────────────────────────────────────────────────────
  while (!pq.empty()) {
    QEntry top = pq.top(); pq.pop();
    int    ci      = top.cell;
    double cost_i  = top.cost;
    int    owner_i = top.owner;

    if (visited[ci]) continue;
    if (cost_i > best_cost[ci] + 1e-9) continue;
    visited[ci] = true;

    // Population saturation penalty for this area
    double pop_pen = 0.0;
    if (use_pop && target_pop > 0.0) {
      double excess = (area_pop[owner_i] / target_pop) - pop_sat_pct;
      if (excess > 0.0)
        pop_pen = std::min(pop_sat_max, pop_sat_weight * excess);
    }

    IntegerVector nbrs = neighbors[ci];

    for (int ni = 0; ni < nbrs.size(); ni++) {
      int nbr = nbrs[ni] - 1;   // to 0-based
      if (visited[nbr]) continue;

      // Hard barrier check
      if (has_barriers) {
        IntegerVector blist = barrier_mat[ci];
        bool blocked = false;
        for (int b = 0; b < blist.size(); b++) {
          if (blist[b] - 1 == nbr) { blocked = true; break; }
        }
        if (blocked) continue;
      }

      // Subdivision soft penalty
      double sdiv_pen = 0.0;
      if (has_subdiv && subdiv_penalty > 0.0) {
        IntegerVector slist = subdiv_mat[ci];
        for (int s = 0; s < slist.size(); s++) {
          if (slist[s] - 1 == nbr) { sdiv_pen = subdiv_penalty; break; }
        }
      }

      double avg_raw = (cell_friction_raw[ci] + cell_friction_raw[nbr]) * 0.5;
      double eff     = avg_raw + base_step_fric + pop_pen + sdiv_pen;
      if (eff > 1.0) eff = 1.0;

      double move_cost = friction_to_cost(eff);
      double new_cost  = best_cost[ci] + move_cost;

      if (new_cost < best_cost[nbr]) {
        best_cost[nbr] = new_cost;
        owner[nbr]     = owner_i;
        pq.push({new_cost, nbr, owner_i});
        if (use_pop) area_pop[owner_i] += cell_pop[nbr];
      }
    }
  }

  // Return 1-based owners (-1 stays -1 for unassigned; R layer handles flood fill)
  IntegerVector r_owner(n_cells);
  NumericVector r_cost(n_cells);
  for (int i = 0; i < n_cells; i++) {
    r_owner[i] = owner[i];      // 0-based area index, -1 = unassigned
    r_cost[i]  = best_cost[i];
  }

  return List::create(
    Named("owner")     = r_owner,
    Named("best_cost") = r_cost
  );
}
