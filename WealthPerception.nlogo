extensions [ rnd ]
turtles-own [ wealth
              wealth_return
              tax
              perceived_min_wealth
              perceived_median_wealth
              perceived_mean_wealth
              perceived_max_wealth
              perceived_rank
              randomized_rank
              perceived_decile_group
              perceived_gini
              rank
              rank_history
              decile_group
              decile_history
              percentile_group
            ]
links-own [ wealth_diff wealth_logdiff]
globals [ ; these are mostly outcome measures with no direct role for dynamics, useful for BehaviorSpace, more measures in the interface
  ; distribution measures
  tail_exponent ; next are different inequality measure
  total_gini wealth_share_top10 wealth_share_top1
  ; immobility measures
  immobility_top20 immobility_quint2 immobility_quint3 immobility_quint4 immobility_bottom20
  ; perception measures
  perceived_decile_groups_share ; list of ten values
  fraction_perceived_below_real_mean ; summarizing the distortion of the mean perception
  ; collective decision measures
  for_more_tax for_less_tax ; THESE ARE DECISIVE DURING SIMULATION: Whichever is larger decides an upward or downward change of taxrate
                            ; Note: for_more_tax = 1 - for_less_tax
  fraction_below_perceived_mean ; Basic value for for_more_tax without taking efficiency into account
  fraction_below_mean ; Technically not a perception measure but a distribution measure but useful for comparison with perceived
  ; network measures
  abs_wealth_diff
  avg_num_friend_of_friends
  log_wealth_assortativity
  initial_gini
  ; growth
  long_term_growth_factor
]

to setup
  clear-all
  create-turtles N [ turtle_initialize ]
  setup_network
  turtles_ranks
  ask links [link_wealth_diff]
  turtles_perceive
  set perceived_decile_groups_share [0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1] ; only to avoid error in plot
  reset-ticks
  update_output_measures
  visualize
  repeat 3 [layout_step]
end

to setup_network
  ; sets up a random network where each node has minimally min_degree neighbors
  ask turtles [
    let link_demand max list 0 (minimal_degree - count link-neighbors)
    create-links-with n-of link_demand other turtles who-are-not link-neighbors
  ]
end

to setup_one_shot
  clear-all
  create-turtles N [ turtle_initialize ]
  ; generate wealth
  ask turtles [
    set wealth (ifelse-value
      wealth_generation = "lognormal" [random-lognormal 0 1]
      wealth_generation = "exponential" [random-exponential 1]
      wealth_generation = "exponential mixed with pareto" [ wealth-quantile random-float 1 ]
      [1])
  ]
  ; generate network
  ifelse network_generation = "random" [
    setup_network
  ][
    ask turtles [
      ask rnd:weighted-n-of (max list 0 (minimal_degree - count link-neighbors)) (other turtles who-are-not link-neighbors)
        [ exp (- homophily_strength * ( abs (wealth_diff_w1_w2 wealth [wealth] of myself))) ]
        [ create-link-with myself ]
    ]
  ]
  turtles_ranks
  ask links [link_wealth_diff]
  set perceived_decile_groups_share [0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1] ; only to avoid error in plot
  turtles_perceive
  reset-ticks
  update_output_measures
  set initial_gini gini [wealth] of turtles
  visualize
  repeat 3 [layout_step]
  update-plots
end

to turtle_initialize
  set wealth 1
  set rank_history []
  set decile_history []
end

to go
  ; Economic Dynamics: Wealth Change
  if wealth_change? [ turtles_wealth_change ]
  ; Social Dynamics: Network Rewiring
  if network_change? [ turtles_rewire ]
  ; Update measures
  ask links [ link_wealth_diff ]
  turtles_ranks
  ; Political Dynamics: Collective decision
  turtles_perceive
  collective_decision_change_tax
  ; Visualization (no effect on model behavior)
  update_output_measures
  visualize
  if ticks = stop_tick [
    ;save-wealth-distr; to save final wealth distribution in a csv file
    stop
  ]
  tick
end

to turtles_wealth_change
  ask turtles [ set wealth_return wealth * ((random-lognormal mu sigma) - 1) ]
  ask turtles [ set tax (wealth + wealth_return) * taxrate ]
  let tax_revenue (ifelse-value efficiency_only_expected? [1] [efficiency]) * sum [tax] of turtles
  ask turtles [ set wealth (wealth + wealth_return - tax + tax_revenue / count turtles) ]
end

to turtles_rewire
  repeat rewiring_frequency [
    ; Remove Link
    ask turtles [
      ; if count my-links > 1 [
      if count my-links > 1 [
        ifelse (link_removal = "homophilic") [
;          let dying_link rnd:weighted-one-of my-links [1 - exp (- homophily_strength * abs select_wealth_diff_link) ]
;          ask dying_link [die]
          let remaining_links rnd:weighted-n-of ((min list count my-links minimal_degree) - 1) my-links [ exp (- homophily_strength * abs select_wealth_diff_link) ]
          ask my-links who-are-not remaining_links [die]
        ] [
          ask one-of my-links [die]
        ]
      ]
    ]
    ; Create new links
    ask turtles [
      let link_demand max list 0 (minimal_degree - count link-neighbors)
      ; Create New Link
      ; (Optional) first fill link_demand with random friends of friends
      if friends_of_friends_first? [
        let friends_of_friends other (turtle-set [link-neighbors] of link-neighbors) with [not link-neighbor? myself]
        if count friends_of_friends > 0 [
          create-links-with n-of min (list link_demand count friends_of_friends) friends_of_friends [ link_wealth_diff ]
        ]
      ]
      ; Fill all (remaining) link demand with links to random other (either homophilic random or plain random)
      set link_demand max list 0 (minimal_degree - count my-links)
      ifelse link_creation = "homophilic" [
        create-links-with rnd:weighted-n-of link_demand other turtles who-are-not link-neighbors [exp (- homophily_strength * ( abs (wealth_diff_w1_w2 wealth [wealth] of myself)))]  [ link_wealth_diff ]
      ][
        create-links-with n-of link_demand other turtles who-are-not link-neighbors [ link_wealth_diff ]
      ]
    ]
  ]
end

to link_wealth_diff
  set wealth_diff ([wealth] of end1 - [wealth] of end2)
  set wealth_logdiff (log [wealth] of end1 10 - log [wealth] of end2 10)
end

to turtles_ranks
  ask turtles [
    set rank (1 + count turtles with [wealth > [wealth] of myself]) / count turtles
    set rank_history fput rank rank_history
    set decile_group quantile_group 10
    set decile_history fput decile_group decile_history
    set percentile_group quantile_group 100
  ]
end

to turtles_perceive
  ask turtles [
    if (not mass_simulation_speedup?) [
      set perceived_gini gini [wealth] of (turtle-set self link-neighbors)
      set perceived_median_wealth median [wealth] of (turtle-set self link-neighbors)
      set perceived_min_wealth min [wealth] of (turtle-set self link-neighbors)
      set perceived_max_wealth max [wealth] of (turtle-set self link-neighbors)
    ]
    set perceived_rank normalized_rank link-neighbors
    let perceived_upper_rank perceived_rank - 1 / count link-neighbors
    set randomized_rank (perceived_upper_rank + random-float (perceived_rank - perceived_upper_rank)) ; Essentially individuals guess their normalized rank in an infinite population with a draw from a uniform distribution
    set perceived_mean_wealth mean [wealth] of (turtle-set self link-neighbors)
    set perceived_decile_group ceiling (10 * randomized_rank)
    ]
end

to collective_decision_change_tax
  ifelse all_know_mean_wealth? [
    set for_more_tax count turtles with [wealth < efficiency * mean [wealth] of turtles] / count turtles
    set for_less_tax count turtles with [wealth > efficiency * mean [wealth] of turtles] / count turtles
  ][
    set for_more_tax count turtles with [wealth < efficiency * perceived_mean_wealth] / count turtles
    set for_less_tax count turtles with [wealth > efficiency * perceived_mean_wealth] / count turtles
  ]
  if (tax_change?) [
    (ifelse
      for_more_tax > 0.5
      [ set taxrate precision (min list 0.99 (taxrate + tax_change_increment)) 4 ]
      for_less_tax > 0.5
      [ set taxrate precision (max list 0 (taxrate - tax_change_increment)) 4 ]
    )
  ]
end

to update_output_measures
  ; IMPORTANT FOR USING mass_simulation_speedup?: PUT ONLY MEASURES HERE WHICH ARE NOT NEEDED AT RUNTIME
  if (not mass_simulation_speedup?) or (ticks = stop_tick) [
    if ticks > 0 [
      set tail_exponent (tail-exponent-fit [wealth] of turtles)
      set long_term_growth_factor exp (ln mean [wealth] of turtles / ticks )
    ]
    set total_gini gini [wealth] of turtles
    set wealth_share_top1 wealth_fraction_top [wealth] of turtles 0.01
    set wealth_share_top10 wealth_fraction_top [wealth] of turtles 0.1
    set perceived_decile_groups_share map [x -> count turtles with [perceived_decile_group = x] / count turtles] [1 2 3 4 5 6 7 8 9 10]
    if ticks > time_lag [
      set immobility_top20 immobility 1
      set immobility_quint2 immobility 2
      set immobility_quint3 immobility 3
      set immobility_quint4 immobility 4
      set immobility_bottom20 immobility 5
    ]
    set fraction_below_mean count turtles with [wealth < mean [wealth] of turtles] / count turtles
    set fraction_below_perceived_mean count turtles with [wealth < perceived_mean_wealth] / count turtles
    set fraction_perceived_below_real_mean count turtles with [perceived_mean_wealth < mean [wealth] of turtles] / count turtles
    set abs_wealth_diff mean [abs select_wealth_diff_link] of links
    set avg_num_friend_of_friends mean [count other (turtle-set [link-neighbors] of link-neighbors) with [not link-neighbor? myself]] of turtles
    set log_wealth_assortativity log_wealth_assort
  ]
end

to visualize
  if not mass_simulation_speedup? [
    ask patches [ set pcolor 8]
    ask turtles [
      set shape "dot"
      set color scale-color 125 (log wealth 10) (min [log wealth 10] of turtles) (max [log wealth 10] of turtles)
      set size ifelse-value (sqrt wealth < sqrt 0.2) [sqrt 0.2] [sqrt wealth]
     ]
    if layout? [ layout_step ]
    if ticks > time_lag [
     set-current-plot "immobility"
     let labels ["Top 20%" "Quint. 2"  "Quint. 3"  "Quint. 4" "Bottom 20%"]
     foreach (range 0 5) [ [i] ->
      set-current-plot-pen (word item i labels)
      plotxy ticks immobility (i + 1)
      ]
    ]
  ]
end

to layout_step
  if not mass_simulation_speedup? [
    repeat 3 [
      ask turtles [set heading 0 forward ((5.5 - decile_group) / 50 * (1 - (abs ycor) / max-pycor) ^ 3) ]
      let factor sqrt count turtles
      ;; numbers here are arbitrarily chosen for pleasing appearance ||| spring-constant spring-length repulsion-constant
      layout-spring turtles links (1 / factor) (5 / factor) (2 / factor)
    ]
  ]
end


to-report quantile_group [q]
  report ceiling (q * rank)
end

to-report normalized_rank [other-turtles]
  let others-and-self (turtle-set self other-turtles)
  report (1 + count others-and-self with [wealth > [wealth] of myself]) / count others-and-self
end

to-report select_wealth_diff_link ; This is a link context reporter
  report ifelse-value (wealth_diff_type = "log") [wealth_logdiff] [wealth_diff]
end

to-report wealth_diff_w1_w2 [w1 w2] ; This is a link context reporter
  report ifelse-value (wealth_diff_type = "log") [log w1 10 - log w2 10] [w1 - w2]
end

to-report immobility [q] ; compute the fraction of agents who are still in quintile q after time_lag
  ifelse count turtles with [ceiling (decile_group / 2) = q] > 0 [
    report count turtles with [ceiling (decile_group / 2) = q and ceiling (decile_group / 2) = ceiling (item time_lag decile_history / 2)] /
    count turtles with [ceiling (decile_group / 2) = q]
  ][
    report 0
  ]
end

to-report wealth_fraction_top [w topshare]
  report sum (sublist (reverse sort w) 0 round (topshare * count turtles)) / sum w
end
to-report wealth_fraction_bottom [w bottomshare]
  report sum (sublist (sort w) 0 round (bottomshare * count turtles)) / sum w
end
to-report wealth_fraction_mid40 [w]
  let sorted-w sort w
  let total count turtles
  let lower round (0.5 * total)
  let upper round (0.9 * total)
  report sum (sublist sorted-w lower upper) / sum w
end

to-report tail-exponent-fit [w]
  ifelse standard-deviation w > 0 [
    let threshold fit-threshold w
    set w filter [x -> x > threshold] w
    report length w / sum map [x -> ln (x / threshold)] w
  ] [
    report 0
  ]
end

to-report fit-threshold [w]
  report quantile w (1 - quantile_fit)
end

to-report quantile [ data-list quantile-level ]
  let sorted-list sort data-list
  let index (length sorted-list * quantile-level) - 1
  let lower-index floor index
  let upper-index ceiling index
  ifelse lower-index = upper-index [
    report item lower-index sorted-list
  ] [
    let lower-value item lower-index sorted-list
    let upper-value item upper-index sorted-list
    let interpolation-factor (index - lower-index)
    report lower-value + interpolation-factor * (upper-value - lower-value)
  ]
end


to-report random-lognormal [mu_ sigma_]
  report exp (mu_ - sigma_ ^ 2 / 2 + sigma_ * random-normal 0 1)
end

to-report wealth-quantile [ p ]
  let c 1   ; Constant, set to 1 per calibration
  ; Local parameters:
  let T 1
  let omega 1.0
  let alpha 1.1

  ; Define the weight function π(p) with a linear transition between 50th and 90th percentile:
  let pi-value 0
  if p < 0.5 [ set pi-value 0 ]
  if p >= 0.9 [ set pi-value 1 ]
  if (p >= 0.5 and p < 0.9) [ set pi-value (p - 0.5) / 0.4 ]

  ; Exponential component: -T * ln((1-p)/c)
  let exponential-part (- T * ln ((1 - p) / c))

  ; Pareto component: ω * (1-p)^(-1/α)
  let pareto-part (omega * ((1 - p) ^ (-1 / alpha)))

  ; Combine the components using the weight:
  report ((1 - pi-value) * exponential-part) + (pi-value * pareto-part)
end

to-report gini [w]
  let nn length w
  report (2 * sum (map [ [x y] -> x * y ]
                 n-values nn [ x -> x + 1 ]
                 sort w) )
                 / (nn * (sum w)) - (nn + 1) / nn
end

to-report shorrocks-index
   if ticks <= time_lag [
    report 0
  ]
  let diag-sum 0
  ;; Loop over quintiles 1 to 5
  foreach [1 2 3 4 5] [
    q ->
    set diag-sum diag-sum + immobility q
  ]
  report (5 - diag-sum) / 4
end

to-report log_wealth_assort
  let x-values []
  let y-values []
  ask links [ ; by asking the links to compile the x and y lists we ensure that they are in the correct order and not randomly ordered.
    let x [log wealth 10] of end1
    let y [log wealth 10] of end2
    set x-values lput x x-values
    set y-values lput y y-values
  ]
  ; add y list to x and x list to y to ensure the unordered links are counted in both directions
  set x-values sentence x-values y-values
  set y-values sentence y-values x-values
  let x-mean mean x-values
  let y-mean mean y-values
  let numerator sum map [i -> (item i x-values - x-mean) * (item i y-values - y-mean)] (range length x-values)
  let denominator-x sqrt sum map [i -> (item i x-values - x-mean) ^ 2] (range length x-values)
  let denominator-y sqrt sum map [i -> (item i y-values - y-mean) ^ 2] (range length y-values)
  if (denominator-x * denominator-y) = 0 [ report 0 ]
  report numerator / (denominator-x * denominator-y)
end

to save-wealth-distr
  let filename "wealth_data.csv"
  if behaviorspace-run-number = 0 and ticks = 0 [
    ;; Write header only once (first run)
    file-open filename
    file-print "run,tick,who,wealth"
    file-close
  ]
  file-open filename
  ask turtles [
    file-print (word behaviorspace-run-number "," ticks "," who "," wealth)
  ]
  file-close
end

to set_baseline_parameters
  set wealth_change? true
  set network_change? true
  set tax_change? true
  set N 1000
  set minimal_degree 10
  set mu 0.01
  set sigma 0.25
  set taxrate 0.015
  set wealth_diff_type "log"
  set homophily_strength 10
  set link_removal "homophilic"
  set link_creation "homophilic"
  set friends_of_friends_first? false
  set rewiring_frequency 1
  set efficiency 0.95
  set efficiency_only_expected? false
  set all_know_mean_wealth? false
  set tax_change_increment 0.005
  set time_lag 10
  set quantile_fit 0.1
  set stop_tick 200
end
@#$#@#$#@
GRAPHICS-WINDOW
222
10
698
487
-1
-1
14.2
1
10
1
1
1
0
0
0
1
-16
16
-16
16
1
1
1
ticks
30.0

BUTTON
9
119
70
153
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
70
119
125
153
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
9
47
181
80
N
N
10
2000
1000.0
10
1
NIL
HORIZONTAL

SLIDER
9
81
181
114
minimal_degree
minimal_degree
1
50
10.0
1
1
NIL
HORIZONTAL

PLOT
720
646
880
766
degree distribution
NIL
NIL
0.0
30.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "histogram [count link-neighbors] of turtles"

SLIDER
8
330
181
363
mu
mu
0
0.1
0.01
0.005
1
NIL
HORIZONTAL

SLIDER
8
363
181
396
sigma
sigma
0
1
0.25
0.01
1
NIL
HORIZONTAL

SLIDER
8
418
208
451
taxrate
taxrate
0
0.3
0.035
0.005
1
NIL
HORIZONTAL

PLOT
714
41
929
191
distribution
NIL
NIL
0.0
10.0
0.0
200.0
false
false
"" "set-plot-y-range 0 0.1 * count turtles\nset-plot-x-range 0 10 * mean [wealth] of turtles "
PENS
"default" 0.1 1 -16777216 true "" "histogram [wealth] of turtles"

MONITOR
835
192
900
237
mean
mean [wealth] of turtles
3
1
11

MONITOR
773
192
836
237
median
median [wealth] of turtles
3
1
11

PLOT
1150
620
1362
747
wealth
NIL
NIL
0.0
10.0
0.5
1.5
true
true
"" "set-plot-y-range 0.5 4.5"
PENS
"mean" 1.0 0 -13840069 true "" "plot mean [wealth] of turtles"
"median" 1.0 0 -14835848 true "" "plot median [wealth] of turtles"

SLIDER
8
519
179
552
homophily_strength
homophily_strength
0
25
10.0
0.5
1
NIL
HORIZONTAL

MONITOR
714
191
774
236
min
min [wealth] of turtles
3
1
11

MONITOR
900
192
957
237
max
max [wealth] of turtles
3
1
11

MONITOR
1150
86
1252
131
mean perc min
mean [perceived_min_wealth] of turtles
3
1
11

MONITOR
1253
86
1363
131
mean perc. max
mean [perceived_max_wealth] of turtles
3
1
11

MONITOR
1010
192
1074
237
gini
total_gini
3
1
11

PLOT
1150
129
1361
249
perceived decile groups
NIL
NIL
1.0
11.0
0.0
0.3
false
false
"" "set-plot-y-range 0 0.3\n;set-plot-y-range 0 0.3 * count turtles\nset-plot-x-range 1 11"
PENS
"default" 1.0 1 -16777216 true "" "clear-plot\nforeach [1 2 3 4 5 6 7 8 9 10] [x -> plotxy x item (x - 1) perceived_decile_groups_share]\n;histogram [perceived_decile_group] of turtles"

MONITOR
1150
43
1249
88
mean perc gini
mean [perceived_gini] of turtles
3
1
11

PLOT
928
41
1132
191
gini
NIL
NIL
0.0
10.0
0.0
1.0
true
true
"" ""
PENS
"gini" 1.0 0 -16777216 true "" "plot total_gini"
"perc gini" 1.0 0 -8630108 true "" "plot mean [perceived_gini] of turtles"

SWITCH
11
653
117
686
layout?
layout?
0
1
-1000

BUTTON
116
653
205
687
NIL
visualize
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
12
846
200
891
wealth_generation
wealth_generation
"lognormal" "exponential" "exponential mixed with pareto" "1"
0

SWITCH
9
179
171
212
wealth_change?
wealth_change?
0
1
-1000

TEXTBOX
1153
589
1346
615
Growth Monitoring
18
0.0
1

PLOT
880
646
1040
766
abs wealth diff
NIL
NIL
0.0
10.0
0.0
0.5
true
false
"" ""
PENS
"diff" 1.0 0 -16777216 true "" "plot abs_wealth_diff"

PLOT
1154
442
1363
562
perceived rank
normalized rank
NIL
0.0
1.0
0.0
1.0
true
false
"" ""
PENS
"perceived rank" 1.0 2 -5987164 true "" "clear-plot\n(foreach sort-on [wealth] turtles [the-turtle -> plotxy [rank] of the-turtle [randomized_rank] of the-turtle])\n;(foreach sort-on [wealth] turtles [the-turtle -> plotxy [rank] of the-turtle [perceived_rank] of the-turtle])"
"decile mean" 1.0 0 -8630108 true "" "if ticks > 0 [\n  (foreach (range 1 11) [x -> plotxy ((x - 0.5) / 10) mean [perceived_rank] of turtles with [decile_group = x]] )\n]"
"pen-2" 1.0 0 -13345367 true "" "plotxy 0 0 plotxy 1 1"
"pen-3" 1.0 0 -11053225 true "" "plotxy 0 0.5 plotxy 1 0.5"

TEXTBOX
725
616
895
646
Network Measures
18
0.0
1

SLIDER
900
419
993
452
time_lag
time_lag
1
50
10.0
1
1
NIL
HORIZONTAL

PLOT
717
452
993
602
immobility
NIL
NIL
0.0
10.0
0.0
1.0
true
true
"" ""
PENS
"Top 20%" 1.0 0 -16777216 true "" ""
"Quint. 2" 1.0 0 -7500403 true "" ""
"Quint. 3" 1.0 0 -2674135 true "" ""
"Quint. 4" 1.0 0 -955883 true "" ""
"Bottom 20%" 1.0 0 -6459832 true "" ""

MONITOR
994
452
1074
497
Top 20%
immobility_top20
3
1
11

MONITOR
994
557
1074
602
Bottom 20%
immobility_bottom20
3
1
11

TEXTBOX
719
9
1116
36
Inequality Measures
18
0.0
1

TEXTBOX
1148
9
1384
34
Network-based Perception
18
0.0
1

TEXTBOX
723
422
839
446
Immobility
18
0.0
1

PLOT
714
243
927
363
compl. cum. dist (ccdf)
NIL
NIL
0.0
2.0
-1.0
0.1
true
false
"" "if (ticks mod 101 = 0) [clear-plot]"
PENS
"default" 1.0 0 -16777216 true "clear-plot\nset-plot-x-range (floor (min [log wealth 10] of turtles)) (1 + ceiling max [log wealth 10] of turtles)\nset-plot-y-range precision (log (1 - (count turtles - 1) / count turtles) 10) 1 - 0.1 (0)" "let sorted-wealths sort [wealth] of turtles\nplot-pen-up\nplotxy log (item 0 sorted-wealths) 10 (0)\nplot-pen-down\nset-plot-pen-color ticks\nforeach n-values (length sorted-wealths) [i -> i] [id -> plotxy (log (item id sorted-wealths) 10) log (1 - (id) / length sorted-wealths) 10]"

MONITOR
835
362
925
407
tail exponent
tail_exponent
4
1
11

SLIDER
714
363
828
396
quantile_fit
quantile_fit
0
0.1
0.1
0.01
1
NIL
HORIZONTAL

MONITOR
940
241
1043
286
share top 10%
wealth_share_top10
3
1
11

MONITOR
1040
241
1130
286
share top 1%
wealth_share_top1
3
1
11

PLOT
940
286
1133
408
share
NIL
NIL
0.0
10.0
0.0
1.0
true
true
"" ""
PENS
"top 10%" 1.0 0 -16777216 true "" "plot wealth_share_top10"
"top 1%" 1.0 0 -7500403 true "" "plot wealth_share_top1"

SLIDER
1391
459
1563
492
rewiring_frequency
rewiring_frequency
1
10
1.0
1
1
NIL
HORIZONTAL

CHOOSER
1390
575
1535
620
wealth_diff_type
wealth_diff_type
"linear" "log"
1

CHOOSER
1395
164
1597
209
link_removal
link_removal
"random" "homophilic"
1

CHOOSER
1392
364
1592
409
link_creation
link_creation
"random" "homophilic"
1

TEXTBOX
11
493
157
519
Network change
18
0.0
1

TEXTBOX
9
399
125
417
Redistribution
12
0.0
1

TEXTBOX
12
288
158
317
Wealth change
18
0.0
1

PLOT
1154
322
1363
442
log perceived mean
normalized rank
NIL
0.0
1.0
0.0
3.0
true
false
"" ""
PENS
"default" 1.0 2 -9276814 true "" "clear-plot\n(foreach sort-on [wealth] turtles [the-turtle -> plotxy [rank] of the-turtle [log perceived_mean_wealth 10] of the-turtle])"
"mean" 1.0 0 -13345367 true "" "plotxy 0 log mean [wealth] of turtles 10 plotxy 1 log mean [wealth] of turtles 10"

BUTTON
9
10
181
44
NIL
set_baseline_parameters
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
1155
278
1365
323
fraction perceived below real mean
fraction_perceived_below_real_mean
3
1
11

MONITOR
608
885
695
930
below perc
fraction_below_perceived_mean
3
1
11

MONITOR
608
840
695
885
below mean
fraction_below_mean
3
1
11

SWITCH
1393
294
1592
327
friends_of_friends_first?
friends_of_friends_first?
1
1
-1000

PLOT
225
770
607
930
fractions of agents
NIL
NIL
0.0
10.0
0.0
1.0
true
true
"" ""
PENS
"for more tax" 1.0 0 -2674135 true "" "plot for_more_tax"
"for less tax" 1.0 0 -13840069 true "" "plot for_less_tax"
"50%" 1.0 0 -7500403 true "" "plot 0.5"
"below mean" 1.0 0 -13345367 true "" "plot fraction_below_mean"
"below perceived" 1.0 0 -8630108 true "" "plot fraction_below_perceived_mean"

TEXTBOX
226
742
563
767
Preference measures
18
0.0
1

TEXTBOX
225
491
567
517
Collective Decision Tax Rate
18
0.0
1

SLIDER
8
581
208
614
tax_change_increment
tax_change_increment
0.001
0.01
0.005
0.001
1
NIL
HORIZONTAL

MONITOR
224
524
310
569
NIL
for_more_tax
3
1
11

PLOT
224
613
705
733
taxrate
NIL
NIL
0.0
10.0
0.0
0.03
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot taxrate"

MONITOR
224
569
310
614
NIL
for_less_tax
17
1
11

TEXTBOX
1172
563
1203
581
rich
12
0.0
1

TEXTBOX
1332
564
1366
582
poor
12
0.0
1

TEXTBOX
1173
253
1203
272
rich
12
0.0
1

TEXTBOX
1330
251
1363
269
poor
12
0.0
1

TEXTBOX
317
530
607
567
Agents voting for more tax: \nwealth < efficiency * mean_perceived_wealth
12
15.0
1

TEXTBOX
316
571
606
611
Agents voting for less tax:\nwealth > efficiency * mean_perceived_wealth
12
55.0
1

SLIDER
8
457
181
490
efficiency
efficiency
0
1
0.95
0.01
1
NIL
HORIZONTAL

MONITOR
1219
747
1364
792
longterm growth factor
long_term_growth_factor
4
1
11

SWITCH
10
246
171
279
tax_change?
tax_change?
0
1
-1000

TEXTBOX
13
555
120
582
Tax Change
18
0.0
1

SWITCH
1390
540
1591
573
efficiency_only_expected?
efficiency_only_expected?
1
1
-1000

MONITOR
1150
818
1260
863
time average
exp (mu - sigma ^ 2 / 2)
4
1
11

MONITOR
1258
818
1365
863
ensemble
exp mu
4
1
11

MONITOR
1150
863
1365
908
ensemble growth with efficiency
exp mu * (1 - taxrate * (1 - (ifelse-value efficiency_only_expected? [1] [efficiency])))
4
1
11

INPUTBOX
134
117
205
177
stop_tick
200.0
1
0
Number

MONITOR
720
766
920
811
Average number friends of friends
avg_num_friend_of_friends
2
1
11

MONITOR
937
766
1040
811
abs wealth diff
abs_wealth_diff
4
1
11

MONITOR
1074
192
1132
237
sd
standard-deviation [wealth] of turtles
3
1
11

SWITCH
10
213
171
246
network_change?
network_change?
0
1
-1000

BUTTON
12
810
200
844
NIL
setup_one_shot
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
12
311
191
330
Random growth parameters
12
0.0
1

TEXTBOX
1396
99
1583
171
Everyone removes links such that (minimal_degree - 1) remain, or one link (if there are less than minimal_degree
10
0.0
1

TEXTBOX
1396
232
1591
302
Every one creates new links such that minimal_degree is reached. Optionally, first randomly from the friends of friends
10
0.0
1

TEXTBOX
1397
329
1584
363
If still needed create links to any others with
10
0.0
1

TEXTBOX
1393
414
1593
471
Note: Individuals can end up with more than minimal_degree links when individuals updating later link to them. 
10
0.0
1

TEXTBOX
1392
509
1571
530
Other Parameters
18
0.0
1

TEXTBOX
1150
798
1353
817
Theoretical growth factors\n
12
0.0
1

TEXTBOX
1154
911
1367
947
based on current taxrate and if efficiency is not only expected
10
0.0
1

TEXTBOX
15
723
198
816
Create a wealth distribution with a given distribution and then create a homophilic nework, based on minimal_degree and homophily strength and compute network-based wealth perceptions
10
0.0
1

TEXTBOX
13
624
130
648
Visualization
18
0.0
1

TEXTBOX
13
693
205
720
For Testing Perception
18
0.0
1

TEXTBOX
1395
214
1520
232
Link creation
12
0.0
1

TEXTBOX
1395
9
1570
64
Details for further  Testing Purposes
18
0.0
1

TEXTBOX
1394
74
1570
98
Rewiring Mechanism
18
0.0
1

CHOOSER
12
890
201
935
network_generation
network_generation
"homophily_strength" "random"
1

MONITOR
734
817
839
862
log wealth assort.
log_wealth_assortativity
4
1
11

PLOT
841
817
1041
937
log wealth assortativity
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot log_wealth_assortativity"

SWITCH
1390
624
1598
657
all_know_mean_wealth?
all_know_mean_wealth?
1
1
-1000

SWITCH
1389
896
1615
929
mass_simulation_speedup?
mass_simulation_speedup?
1
1
-1000

TEXTBOX
1387
839
1608
892
For Massive Simulation Experiments
18
0.0
1

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.4.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="trajectories" repetitions="25" runMetricsEveryStep="true">
    <setup>setup_one_shot</setup>
    <go>go</go>
    <metric>taxrate</metric>
    <metric>tail_exponent</metric>
    <metric>total_gini</metric>
    <metric>wealth_share_top10</metric>
    <metric>wealth_share_top1</metric>
    <metric>immobility_top20</metric>
    <metric>immobility_quint2</metric>
    <metric>immobility_quint3</metric>
    <metric>immobility_quint4</metric>
    <metric>immobility_bottom20</metric>
    <metric>perceived_decile_groups_share</metric>
    <metric>fraction_perceived_below_real_mean</metric>
    <metric>for_more_tax</metric>
    <metric>for_less_tax</metric>
    <metric>fraction_below_perceived_mean</metric>
    <metric>fraction_below_mean</metric>
    <metric>abs_wealth_diff</metric>
    <metric>avg_num_friend_of_friends</metric>
    <metric>log_wealth_assortativity</metric>
    <metric>initial_gini</metric>
    <enumeratedValueSet variable="rewiring_frequency">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="taxrate">
      <value value="0"/>
      <value value="0.1"/>
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="efficiency_only_expected?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="efficiency">
      <value value="0.95"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimal_degree">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sigma">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tax_change_increment">
      <value value="0.005"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stop_tick">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wealth_generation">
      <value value="&quot;lognormal&quot;"/>
      <value value="&quot;exponential mixed with pareto&quot;"/>
      <value value="&quot;1&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="link_creation">
      <value value="&quot;homophilic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time_lag">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="quantile_fit">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_generation">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="link_removal">
      <value value="&quot;homophilic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wealth_diff_type">
      <value value="&quot;log&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="common_knowledge_mean_wealth?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wealth_change?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mu">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_change?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homophily_strength">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tax_change?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="friends_of_friends_first?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="econ growth efficiency tax" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>long_term_growth_factor</metric>
    <metric>tail_exponent</metric>
    <metric>total_gini</metric>
    <metric>wealth_share_top10</metric>
    <metric>wealth_share_top1</metric>
    <metric>immobility_top20</metric>
    <metric>immobility_quint2</metric>
    <metric>immobility_quint3</metric>
    <metric>immobility_quint4</metric>
    <metric>immobility_bottom20</metric>
    <metric>fraction_perceived_below_real_mean</metric>
    <metric>for_more_tax</metric>
    <metric>for_less_tax</metric>
    <metric>fraction_below_perceived_mean</metric>
    <metric>fraction_below_mean</metric>
    <metric>abs_wealth_diff</metric>
    <metric>log_wealth_assortativity</metric>
    <steppedValueSet variable="taxrate" first="0" step="0.005" last="0.05"/>
    <enumeratedValueSet variable="efficiency">
      <value value="0.9"/>
      <value value="0.95"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mu">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sigma">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wealth_change?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_change?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tax_change?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mass_simulation_speedup?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring_frequency">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="efficiency_only_expected?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimal_degree">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tax_change_increment">
      <value value="0.005"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stop_tick">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="link_creation">
      <value value="&quot;homophilic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time_lag">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="quantile_fit">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wealth_diff_type">
      <value value="&quot;log&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wealth_generation">
      <value value="&quot;lognormal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_generation">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="link_removal">
      <value value="&quot;homophilic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homophily_strength">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="all_know_mean_wealth?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="friends_of_friends_first?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="homophily and perceived mean" repetitions="10" runMetricsEveryStep="true">
    <setup>setup_one_shot</setup>
    <go>go</go>
    <metric>long_term_growth_factor</metric>
    <metric>tail_exponent</metric>
    <metric>total_gini</metric>
    <metric>wealth_share_top10</metric>
    <metric>wealth_share_top1</metric>
    <metric>immobility_top20</metric>
    <metric>immobility_quint2</metric>
    <metric>immobility_quint3</metric>
    <metric>immobility_quint4</metric>
    <metric>immobility_bottom20</metric>
    <metric>fraction_perceived_below_real_mean</metric>
    <metric>for_more_tax</metric>
    <metric>for_less_tax</metric>
    <metric>fraction_below_perceived_mean</metric>
    <metric>fraction_below_mean</metric>
    <metric>abs_wealth_diff</metric>
    <metric>log_wealth_assortativity</metric>
    <enumeratedValueSet variable="wealth_change?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_change?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tax_change?">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="homophily_strength" first="0" step="2" last="14"/>
    <steppedValueSet variable="efficiency" first="0.95" step="0.01" last="1"/>
    <enumeratedValueSet variable="stop_tick">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mass_simulation_speedup?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimal_degree">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="taxrate">
      <value value="0.015"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="efficiency_only_expected?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sigma">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring_frequency">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tax_change_increment">
      <value value="0.005"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="link_removal">
      <value value="&quot;homophilic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wealth_diff_type">
      <value value="&quot;log&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wealth_generation">
      <value value="&quot;lognormal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time_lag">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="link_creation">
      <value value="&quot;homophilic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="quantile_fit">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_generation">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mu">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="all_know_mean_wealth?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="friends_of_friends_first?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="econ-dyn-bl-all" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="200"/>
    <metric>gini [wealth] of turtles</metric>
    <metric>min [wealth] of turtles</metric>
    <metric>max [wealth] of turtles</metric>
    <metric>median [wealth] of turtles</metric>
    <metric>mean [wealth] of turtles</metric>
    <metric>wealth_fraction_top [wealth] of turtles 0.1</metric>
    <metric>wealth_fraction_top [wealth] of turtles 0.01</metric>
    <metric>wealth_fraction_bottom [wealth] of turtles 0.5</metric>
    <metric>wealth_fraction_mid40 [wealth] of turtles</metric>
    <metric>tail_exponent</metric>
    <metric>immobility_top20</metric>
    <metric>immobility_bottom20</metric>
    <metric>long_term_growth_factor</metric>
    <enumeratedValueSet variable="wealth_change?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_change?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tax_change?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimal_degree">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mu">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sigma">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="taxrate">
      <value value="0.015"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wealth_diff_type">
      <value value="&quot;log&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homophily_strength">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="link_removal">
      <value value="&quot;homophilic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="link_creation">
      <value value="&quot;homophilic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="friends_of_friends_first?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring_frequency">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="efficiency">
      <value value="0.95"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="efficiency_only_expected?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tax_change_increment">
      <value value="0.005"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time_lag">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="quantile_fit">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stop_tick">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wealth_generation">
      <value value="&quot;lognormal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mass_simulation_speedup?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="gatsby" repetitions="20" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>gini [wealth] of turtles</metric>
    <metric>immobility_top20</metric>
    <metric>immobility_bottom20</metric>
    <metric>long_term_growth_factor</metric>
    <steppedValueSet variable="sigma" first="0.05" step="0.05" last="0.3"/>
    <steppedValueSet variable="taxrate" first="0.005" step="0.005" last="0.05"/>
    <enumeratedValueSet variable="wealth_change?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_change?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tax_change?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimal_degree">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring_frequency">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="efficiency">
      <value value="0.95"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="efficiency_only_expected?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tax_change_increment">
      <value value="0.005"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stop_tick">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="link_creation">
      <value value="&quot;homophilic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wealth_generation">
      <value value="&quot;lognormal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="link_removal">
      <value value="&quot;homophilic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_generation">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="quantile_fit">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mass_simulation_speedup?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time_lag">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wealth_diff_type">
      <value value="&quot;log&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mu">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homophily_strength">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="all_know_mean_wealth?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="friends_of_friends_first?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="econ-dyn-mu" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>gini [wealth] of turtles</metric>
    <metric>wealth_fraction_top [wealth] of turtles 0.1</metric>
    <metric>wealth_fraction_bottom [wealth] of turtles 0.5</metric>
    <metric>wealth_fraction_mid40 [wealth] of turtles</metric>
    <metric>tail_exponent</metric>
    <metric>immobility_top20</metric>
    <metric>long_term_growth_factor</metric>
    <steppedValueSet variable="mu" first="0.002" step="0.004" last="0.038"/>
    <enumeratedValueSet variable="sigma">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="taxrate">
      <value value="0.015"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="efficiency">
      <value value="0.95"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wealth_change?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_change?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tax_change?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimal_degree">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring_frequency">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="efficiency_only_expected?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tax_change_increment">
      <value value="0.005"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stop_tick">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="link_creation">
      <value value="&quot;homophilic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wealth_generation">
      <value value="&quot;lognormal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="link_removal">
      <value value="&quot;homophilic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_generation">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="quantile_fit">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mass_simulation_speedup?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time_lag">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wealth_diff_type">
      <value value="&quot;log&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homophily_strength">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="all_know_mean_wealth?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="friends_of_friends_first?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="econ-dyn-sigma" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>gini [wealth] of turtles</metric>
    <metric>wealth_fraction_top [wealth] of turtles 0.1</metric>
    <metric>wealth_fraction_bottom [wealth] of turtles 0.5</metric>
    <metric>wealth_fraction_mid40 [wealth] of turtles</metric>
    <metric>tail_exponent</metric>
    <metric>immobility_top20</metric>
    <metric>long_term_growth_factor</metric>
    <enumeratedValueSet variable="mu">
      <value value="0.01"/>
    </enumeratedValueSet>
    <steppedValueSet variable="sigma" first="0.05" step="0.05" last="0.5"/>
    <enumeratedValueSet variable="taxrate">
      <value value="0.015"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="efficiency">
      <value value="0.95"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wealth_change?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_change?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tax_change?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimal_degree">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring_frequency">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="efficiency_only_expected?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tax_change_increment">
      <value value="0.005"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stop_tick">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="link_creation">
      <value value="&quot;homophilic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wealth_generation">
      <value value="&quot;lognormal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="link_removal">
      <value value="&quot;homophilic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_generation">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="quantile_fit">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mass_simulation_speedup?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time_lag">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wealth_diff_type">
      <value value="&quot;log&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homophily_strength">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="all_know_mean_wealth?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="friends_of_friends_first?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="econ-dyn-taxrate" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>gini [wealth] of turtles</metric>
    <metric>wealth_fraction_top [wealth] of turtles 0.1</metric>
    <metric>wealth_fraction_bottom [wealth] of turtles 0.5</metric>
    <metric>wealth_fraction_mid40 [wealth] of turtles</metric>
    <metric>tail_exponent</metric>
    <metric>immobility_top20</metric>
    <metric>long_term_growth_factor</metric>
    <enumeratedValueSet variable="mu">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sigma">
      <value value="0.25"/>
    </enumeratedValueSet>
    <steppedValueSet variable="taxrate" first="0.005" step="0.005" last="0.05"/>
    <enumeratedValueSet variable="efficiency">
      <value value="0.95"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wealth_change?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_change?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tax_change?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimal_degree">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring_frequency">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="efficiency_only_expected?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tax_change_increment">
      <value value="0.005"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stop_tick">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="link_creation">
      <value value="&quot;homophilic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wealth_generation">
      <value value="&quot;lognormal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="link_removal">
      <value value="&quot;homophilic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_generation">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="quantile_fit">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mass_simulation_speedup?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time_lag">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wealth_diff_type">
      <value value="&quot;log&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homophily_strength">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="all_know_mean_wealth?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="friends_of_friends_first?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="econ-dyn-efficiency" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>gini [wealth] of turtles</metric>
    <metric>wealth_fraction_top [wealth] of turtles 0.1</metric>
    <metric>wealth_fraction_bottom [wealth] of turtles 0.5</metric>
    <metric>wealth_fraction_mid40 [wealth] of turtles</metric>
    <metric>tail_exponent</metric>
    <metric>immobility_top20</metric>
    <metric>long_term_growth_factor</metric>
    <enumeratedValueSet variable="mu">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sigma">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="taxrate">
      <value value="0.015"/>
    </enumeratedValueSet>
    <steppedValueSet variable="efficiency" first="0.55" step="0.05" last="1"/>
    <enumeratedValueSet variable="wealth_change?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_change?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tax_change?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimal_degree">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring_frequency">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="efficiency_only_expected?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tax_change_increment">
      <value value="0.005"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stop_tick">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="link_creation">
      <value value="&quot;homophilic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wealth_generation">
      <value value="&quot;lognormal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="link_removal">
      <value value="&quot;homophilic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_generation">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="quantile_fit">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mass_simulation_speedup?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time_lag">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wealth_diff_type">
      <value value="&quot;log&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homophily_strength">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="all_know_mean_wealth?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="friends_of_friends_first?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="homophily_oneshot" repetitions="50" runMetricsEveryStep="false">
    <setup>setup_one_shot</setup>
    <go>go</go>
    <metric>count turtles with [mean [wealth] of turtles &gt; wealth] / count turtles</metric>
    <metric>count turtles with [perceived_mean_wealth &gt; wealth] / count turtles</metric>
    <metric>count turtles with [perceived_decile_group = 1]</metric>
    <metric>count turtles with [perceived_decile_group = 2]</metric>
    <metric>count turtles with [perceived_decile_group = 3]</metric>
    <metric>count turtles with [perceived_decile_group = 4]</metric>
    <metric>count turtles with [perceived_decile_group = 5]</metric>
    <metric>count turtles with [perceived_decile_group = 6]</metric>
    <metric>count turtles with [perceived_decile_group = 7]</metric>
    <metric>count turtles with [perceived_decile_group = 8]</metric>
    <metric>count turtles with [perceived_decile_group = 9]</metric>
    <metric>count turtles with [perceived_decile_group = 10]</metric>
    <metric>log_wealth_assort</metric>
    <enumeratedValueSet variable="mu">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sigma">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="taxrate">
      <value value="0.015"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="efficiency">
      <value value="0.95"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wealth_change?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_change?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tax_change?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimal_degree">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring_frequency">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="efficiency_only_expected?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tax_change_increment">
      <value value="0.005"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stop_tick">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="link_creation">
      <value value="&quot;homophilic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wealth_generation">
      <value value="&quot;lognormal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="link_removal">
      <value value="&quot;homophilic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_generation">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="quantile_fit">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mass_simulation_speedup?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time_lag">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wealth_diff_type">
      <value value="&quot;log&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homophily_strength">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="all_know_mean_wealth?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="friends_of_friends_first?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="tax_rate_new_social_on" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>taxrate</metric>
    <metric>for_more_tax</metric>
    <metric>for_less_tax</metric>
    <metric>fraction_below_mean</metric>
    <metric>fraction_below_perceived_mean</metric>
    <metric>total_gini</metric>
    <metric>mean [perceived_gini] of turtles</metric>
    <metric>immobility_top20</metric>
    <metric>immobility_bottom20</metric>
    <metric>mean [wealth] of turtles</metric>
    <metric>median [wealth] of turtles</metric>
    <metric>min [wealth] of turtles</metric>
    <metric>max [wealth] of turtles</metric>
    <metric>mean [perceived_min_wealth] of turtles</metric>
    <metric>mean [perceived_max_wealth] of turtles</metric>
    <metric>log_wealth_assortativity</metric>
    <enumeratedValueSet variable="efficiency">
      <value value="0.7"/>
      <value value="0.75"/>
      <value value="0.8"/>
      <value value="0.85"/>
      <value value="0.9"/>
      <value value="0.95"/>
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homophily_strength">
      <value value="0"/>
      <value value="2"/>
      <value value="4"/>
      <value value="6"/>
      <value value="8"/>
      <value value="10"/>
      <value value="12"/>
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mu">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sigma">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wealth_change?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_change?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tax_change?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mass_simulation_speedup?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="taxrate">
      <value value="0.015"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimal_degree">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring_frequency">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tax_change_increment">
      <value value="0.005"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stop_tick">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wealth_generation">
      <value value="&quot;lognormal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="link_removal">
      <value value="&quot;homophilic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time_lag">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="link_creation">
      <value value="&quot;homophilic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_generation">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wealth_diff_type">
      <value value="&quot;log&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="quantile_fit">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="all_know_mean_wealth?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="friends_of_friends_first?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
