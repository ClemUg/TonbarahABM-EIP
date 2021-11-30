Turtles-own [mon elc pelc ph pg pc ps pr pw pn pf]                         ; mon = money, elc = electricity, ph = price of heat, pg = price of gas, pc = price of crude
mss-own [heat gas crd slp rpd wtr npk fpd]                                 ; ps = price of sulphur, pr = price of refinery products, pw = price of water, pn = price of npk
mbs-own [heat gas crd slp rpd wtr npk fpd de dh dg dc ds dr dw dn df]      ; pf = price of fertiliser products, crd = crude, rpd = refinery product, fpd = fertiliser product, df = fertiliser demand
rfs-own [heat gas crd slp rpd dh dc de]                                    ; de = electricity demand, ds = sulphur demand, slp = sulpher, eff = effluent, dg = gas demand, dh = heat demand
pps-own [heat gas wtr dg dw]                                               ; wtr = water, dc = crude demand, npk = fertiliser raw material, dr = demand for refinery products, dw = water demand
fps-own [slp npk fpd eff ds dn de]                                         ; dn = npk demand

breed [mss ms]                                                             ; mss = market selling agents (black)
breed [mbs mb]                                                             ; mbs = market buying agents (orange)
breed [rfs rf]                                                             ; rfs = refineries (blue)
breed [pps pp]                                                             ; pps = powerplants (green)
breed [fps fp]                                                             ; fps = fertiliser plants (red)

to setup
  clear-all                                                                ; reset field
  ask patches [set pcolor gray]                                            ; set background color
  setup-breeds                                                             ; create agents
  display-labels                                                           ; show amount of money of each agent
  reset-ticks                                                              ; initialise time
end

to go
  if ticks = 365 * number-of-years [stop]                                  ; setting for contract length
  setup-demand                                                             ; buyers fix requirements
  setup-prices                                                             ; sellers fix prices
  purchase-inputs                                                          ; industrial agents buy input requirements
  produce-outputs                                                          ; industrial agents produce
  sell-products                                                            ; all sellers sell products
  pay-fines                                                                ; industrial agents pay fines for waste released into the environment
  display-labels                                                           ; show each industrial agent's cash balance
  tick                                                                     ; time + 1 period
end

to setup-demand
  ask mbs [set de (random-normal 150 10) set dh (random-normal 5000 100) set dg (random-normal 4000 50)
    set dc (random-normal 50000 1000) set ds (random-normal 50000 100) set dr (random-normal 400000 10000)
    set dw (random-normal 100000 160) set dn (random-normal 50000 300) set df (random-normal 40000 300)]
  ask rf 3 [set dh (random-normal (0.27 * 0.0625 * (sum [dr] of mbs)) 100) set dc (random-normal (0.27 * 1.25 * (sum [dr] of mbs)) 10000)
    set de (random-normal (0.27 * 0.00125 * (sum [dr] of mbs)) 100)]
  ask rf 4 [set dh (random-normal (0.27 * 0.0567 * (sum [dr] of mbs)) 100) set dc (random-normal (0.27 * 1.35 * (sum [dr] of mbs)) 10000)                    ; all buyers' demand set on a gaussian-normal distribution with a mean deamand value and variance
    set de (random-normal (0.27 * 0.00141 * (sum [dr] of mbs)) 100)]                                                                                         ; industrial agents input requiremnts based on market buyers products demand
  ask rf 5 [set dh (random-normal (0.27 * 0.0633 * (sum [dr] of mbs)) 100) set dc (random-normal (0.27 * 1.41 * (sum [dr] of mbs)) 10000)
    set de (random-normal (0.27 * 0.0013 * (sum [dr] of mbs)) 100)]
  ask pp 7 [set dg (random-normal (8.88 * 2000) 50) set dw (random-normal (1.11 * 2000) 500)]
  ask pp 6 [set dg (random-normal (8.33 * 1500) 50) set dw (random-normal (16.67 * 1500) 500)]
  ask fps [set ds (random-normal (0.8 * (sum [df] of mbs) / 3 * 0.03) 100) set dn (random-normal (0.8 * (sum [df] of mbs) / 3 * 1.25) 300)
    set de (random-normal (0.8 * (sum [df] of mbs) / 3 * 0.005) 50)]
end

to setup-prices
  ask mss [set pelc (random-normal (inflationary-factor * 200) 0.5) set ph (random-normal (inflationary-factor * 1) 0.05)
    set pg (random-normal (inflationary-factor * 5) 0.02)                                                                                                   ; market selling agents set prices based on a gaussian-normal distribution with a mean value and variance
    set pc (random-normal (inflationary-factor * 10) 0.3) set ps (random-normal (inflationary-factor * 0.5) 0.01)
    set pr (random-normal (inflationary-factor * 20) 1)
    set pw (random-normal (inflationary-factor * 0.2) 0.001) set pn (random-normal (inflationary-factor * 20) 1.01)
    set pf (random-normal (inflationary-factor * 100) 2)]
  ask rf 3 [set pg (((sum [pg] of mss) / 3) - random 0.5001) set ps (((sum [ps] of mss) / 3) - random 0.05)                                                 ; industrial agents set product prices based on average market prices
    set pr (((sum [pr] of mss) / 3) - random 5.005)]
  ask rf 4 [set pg (((sum [pg] of mss) / 3) - random 0.5001) set ps (((sum [ps] of mss) / 3) - random 0.05)
    set pr (((sum [pr] of mss) / 3) - random 5.005)]
  ask rf 5 [set pg (((sum [pg] of mss) / 3) - random 0.5001) set ps (((sum [ps] of mss) / 3) - random 0.05)
    set pr (((sum [pr] of mss) / 3) - random 5.005)]
  ask pp 6 [set pelc (((sum [pelc] of mss) / 3) - random 5.005) set ph (((sum [ph] of mss) / 3) - random 0.025)]
  ask pp 7 [set pelc (((sum [pelc] of mss) / 3) - random 5.005) set ph (((sum [ph] of mss) / 3) - random 0.025)]
  ask fps [set pf (((sum [pf] of mss) / 3) - random 8)]
end

to purchase-inputs
  if [crd] of rf 3 < [dc] of rf 3 [ask min-one-of mss [pc + 0] [set crd (crd - [dc] of rf 3)
    set mon (mon + [dc] of rf 3 * pc)]]
  ask rf 3 [(if crd < dc [set crd (crd + dc) set mon (mon - (dc * [pc] of min-one-of mss [pc + 0])) set dc 0])]
  if [crd] of rf 5 < [dc] of rf 5 [ask min-one-of mss [pc + 0] [set crd (crd - [dc] of rf 5)
    set mon (mon + [dc] of rf 5 * pc)]]
  ask rf 5 [(if crd < dc [set crd (crd + dc) set mon (mon - (dc * [pc] of min-one-of mss [pc + 0])) set dc 0])]
  if [crd] of rf 4 < [dc] of rf 4 [ask min-one-of mss [pc + 0] [set crd (crd - [dc] of rf 4)
    set mon (mon + [dc] of rf 4 * pc)]]                                                                                                             ; industrial agents purchase inputs not available within the EIP from market selling agents
  ask rf 4 [(if crd < dc [set crd (crd + dc) set mon (mon - (dc * [pc] of min-one-of mss [pc + 0])) set dc 0])]
  if [wtr] of pp 6 < [dw] of pp 6 [ask min-one-of mss [pw + 0] [set wtr (wtr - [dw] of pp 6)
    set mon (mon + [dw] of pp 6 * pw)]]
  ask pp 6 [(if wtr < dw [set wtr (wtr + dw) set mon (mon - dw * [pw] of min-one-of mss [pw + 0]) set dw 0])]
  if [wtr] of pp 7 < [dw] of pp 7 [ask min-one-of mss [pw + 0] [set wtr (wtr - [dw] of pp 7)
    set mon (mon + [dw] of pp 7 * pw)]]
  ask pp 7 [(if wtr < dw [set wtr (wtr + dw) set mon (mon - dw * [pw] of min-one-of mss [pw + 0]) set dw 0])]
  if [npk] of fp 8 < [dn] of fp 8 [ask min-one-of mss [pn + 0] [set npk (npk - [dn] of fp 8)
    set mon (mon + [dn] of fp 8 * pn)]]
  ask fp 8 [(if npk < dn [set npk (npk + dn) set mon (mon - dn * [pn] of min-one-of mss [pn + 0]) set dn 0])]
  ask fp 9 [(if npk < dn [set npk (npk + dn) set mon (mon - dn * [pn] of min-one-of mss [pn + 0]) set dn 0])]
  ask fp 10 [(if npk < dn [set npk (npk + dn) set mon (mon - dn * [pn] of min-one-of mss [pn + 0]) set dn 0])]
end


to produce-outputs
  if random 101 < reliability-ref-1 [ask rf 3 [set gas (gas + (0.02 * dc)) set slp (slp + (0.0044 * dc)) set rpd (rpd + (0.8 * dc))
    set elc (elc - 0.001 * dc) set heat (heat - 0.05 * dc) set crd (crd - dc)]]
  if random 101 < reliability-ref-2 [ask rf 4 [set gas (gas + (0.03 * dc)) set slp (slp + (0.0029 * dc)) set rpd (rpd + (0.74 * dc))
    set elc (elc - 0.00104 * dc) set heat (heat - 0.042 * dc) set crd (crd - dc)]]
  if random 101 < reliability-ref-3 [ask rf 5 [set gas (gas + (0.035 * dc)) set slp (slp + (0.0035 * dc)) set rpd (rpd + (0.71 * dc))              ; industrial agents produce only when factory is not down on maintenance which is dependent on
    set elc (elc - 0.000922 * dc) set heat (heat - 0.056 * dc) set crd (crd - dc)]]                                                                ; the factories' reliability factor
  if random 101 < reliability-pp-1 [ask pp 6 [set elc (elc + (0.113 * dg)) set heat (heat + (4.693 * dg)) set gas (gas - dg)
    set wtr (wtr - 1.251 * dg)]]
  if random 101 < reliability-pp-2 [ask pp 7 [set elc (elc + (0.120 * dg)) set heat (heat + (3.333 * dg)) set gas (gas - dg)
    set wtr (wtr - 2.001 * dg)]]
  if random 101 < reliability-fplants [ask fps [set fpd (fpd + (0.8 * dn)) set eff (eff + (0.01 * dn)) set elc (elc - 0.004 * dn)
    set npk (npk - dn) set slp (slp - 0.024 * dn)]]

  ifelse model-version = "contracts" [if [elc] of pp 7 < ([de] of rf 3 + [de] of rf 5 + [de] of fp 8) [
    ask pp 7 [set mon (mon - (([elc] of pp 6 - ([de] of rf 4 + [de] of fp 9 + [de] of fp 10)) * [pelc] of pp 6))]
    ask pp 6 [set mon (mon + (([elc] of pp 6 - ([elc] of rf 4 + [elc] of fp 9 + [de] of fp 10)) * pelc))]
    ask pp 7 [set elc (elc + ([elc] of pp 6 - ([de] of rf 4 + [de] of fp 9 + [de] of fp 10)))]
    ask pp 6 [set elc (elc - (elc - ([de] of rf 4 + [de] of fp 9 + [de] of fp 10)))]]
  if [elc] of pp 7 < ([de] of rf 3 + [de] of rf 5 + [de] of fp 8) [
    ask pp 7 [set mon (mon - ((([de] of rf 3 + [de] of rf 5 + [de] of fp 8) - elc) * [pelc] of min-one-of mss [pelc + 0]))]                         ; industrial agents purchase all required inputs available in the EIP from EIP agents before looking out
    ask min-one-of mss [pelc + 0] [set mon (mon + ((([de] of rf 3 + [de] of rf 5 + [de] of fp 8) - [elc] of pp 7) * pelc))]                         ; to the market selling agents
    ask pp 7 [set elc (elc + (([de] of rf 3 + [de] of rf 5 + [de] of fp 8) - elc))]                                                                 ; with contracts, only EIP agents in contractual agreement trade directly
    ask min-one-of mss [pelc + 0] [set elc (elc - (([de] of rf 3 + [de] of rf 5 + [de] of fp 8) - [elc] of pp 7))]]                                 ; without contracts, transactions within the EIP is based on the best price (least price)

  if [heat] of pp 7 < ([dh] of rf 3 + [dh] of rf 5) [
    ask pp 7 [set mon (mon - (([heat] of pp 6 - ([dh] of rf 4)) * [ph] of pp 6))]
    ask pp 6 [set mon (mon + ((heat - ([dh] of rf 4)) * ph))]
    ask pp 7 [set heat (heat + ([heat] of pp 6 - ([dh] of rf 4)))]
    ask pp 6 [set heat (heat - (heat - ([dh] of rf 4)))]]
  if [heat] of pp 7 < ([dh] of rf 3 + [dh] of rf 5) [
    ask pp 7 [set mon (mon - ((([dh] of rf 3 + [dh] of rf 5) - heat) * [ph] of  min-one-of mss [ph + 0]))]
    ask  min-one-of mss [ph + 0] [set mon (mon + ((([dh] of rf 3 + [dh] of rf 5) - [heat] of pp 7) * ph))]
    ask pp 7 [set heat (heat + (([dh] of rf 3 + [dh] of rf 5) - heat))]
    ask  min-one-of mss [ph + 0] [set heat (heat - (([dh] of rf 3 + [dh] of rf 5) - [heat] of pp 7))]]


  if [elc] of rf 3 < [de] of rf 3 [ask pp 7 [set elc (elc - [de] of rf 3) set mon (mon + [de] of rf 3 * pelc)]]
  if [heat] of rf 3 < [dh] of rf 3 [ask pp 7[set heat (heat - [dh] of rf 3) set mon (mon + [dh] of rf 3 * ph)]]
  ask rf 3 [(if elc < de [set elc (elc + de) set mon (mon - (de * [pelc] of pp 7)) set de 0])
    (if heat < dh [set heat (heat + dh) set mon (mon - (dh * [ph] of pp 7)) set dh 0])]


  if [elc] of rf 5 < [de] of rf 5 [ask pp 7 [set elc (elc - [de] of rf 5) set mon (mon + [de] of rf 5 * pelc)]]
  if [heat] of rf 5 < [dh] of rf 5 [ask pp 7 [set heat (heat - [dh] of rf 5) set mon (mon + [dh] of rf 5 * ph)]]
  ask rf 5 [(if elc < de [set elc (elc + de) set mon (mon - (de * [pelc] of pp 7)) set de 0])
    (if heat < dh [set heat (heat + dh) set mon (mon - (dh * [ph] of pp 7)) set dh 0])]


  if [elc] of pp 6 < ([de] of rf 4 + [de] of fp 9 + [de] of fp 10) [
    ask pp 6 [set mon (mon - (([elc] of pp 7 - ([de] of rf 3 + [de] of rf 5 + [de] of fp 8)) * [pelc] of pp 7))]
    ask pp 7 [set mon (mon + ((elc - ([de] of rf 3 + [de] of rf 5 + [de] of fp 8)) * pelc))]
    ask pp 6 [set elc (elc + ([elc] of pp 7 - ([de] of rf 3 + [de] of rf 5 + [de] of fp 8)))]
    ask pp 7 [set elc (elc - (elc - ([de] of rf 3 + [de] of rf 5 + [de] of fp 8)))]]
  if [elc] of pp 6 < ([de] of rf 4 + [de] of fp 9 + [de] of fp 10) [
    ask pp 6[set mon (mon - ((([de] of rf 4 + [de] of fp 9 + [de] of fp 10) - elc) * [pelc] of min-one-of mss [pelc + 0]))]
    ask min-one-of mss [pc + 0] [set mon (mon + ((([de] of rf 4 + [de] of fp 9 + [de] of fp 10) - [elc] of pp 6) * pelc))]
    ask pp 6 [set elc (elc + (([de] of rf 4 + [de] of fp 9 + [de] of fp 10) - elc))]
    ask min-one-of mss [pelc + 0] [set elc (elc - (([de] of rf 4 + [de] of fp 9 + [de] of fp 10) - [elc] of pp 6))]]

  if [heat] of pp 6 < ([dh] of rf 4) [
    ask pp 6 [set mon (mon - (([heat] of pp 7 - ([dh] of rf 3 + [dh] of rf 5)) * [ph] of pp 7))]
    ask pp 7 [set mon (mon + ((heat - ([dh] of rf 3 + [dh] of rf 5)) * ph))]
    ask pp 6 [set heat (heat + ([heat] of pp 7 - ([dh] of rf 3 + [dh] of rf 5)))]
    ask pp 7 [set heat (heat - (heat - ([dh] of rf 3 + [dh] of rf 5)))]]
  if [heat] of pp 6 < ([dh] of rf 4) [
    ask pp 6 [set mon (mon - ((([dh] of rf 4) - heat) * [ph] of  min-one-of mss [ph + 0]))]
    ask  min-one-of mss [ph + 0] [set mon (mon + ((([dh] of rf 4) - [heat] of pp 6) * ph))]
    ask pp 6 [set heat (heat + (([dh] of rf 4) - heat))]
    ask  min-one-of mss [ph + 0] [set heat (heat - (([dh] of rf 4) - [heat] of pp 6))]]

  if [elc] of rf 4 < [de] of rf 4 [ask pp 6 [set elc (elc - [de] of rf 4) set mon (mon + [de] of rf 4 * pelc)]]
  if [heat] of rf 4 < [dh] of rf 4 [ask pp 6 [set heat (heat - [dh] of rf 4) set mon (mon + [dh] of rf 4 * ph)]]
  ask rf 4 [(if elc < de [set elc (elc + de) set mon (mon - (de * [pelc] of pp 6)) set de 0])
    (if heat < dh [set heat (heat + dh) set mon (mon - (dh * [ph] of pp 6)) set dh 0])]


  if [gas] of rf 4 < ([dg] of pp 6) and [gas] of rf 5 > ([dg] of pp 6) [
    ask rf 4 [set mon (mon - ([dg] of pp 6 * [pg] of rf 5))]
    ask rf 5 [set mon (mon + ([dg] of pp 6 * pg))]
    ask rf 4 [set gas (gas + [dg] of pp 6)]
    ask rf 5 [set gas (gas - [dg] of pp 6)]]
  if [gas] of rf 4 < ([dg] of pp 6) [
    ask rf 4 [set mon (mon - (([dg] of pp 6 - gas) * [pg] of min-one-of mss [pg + 0]))]
    ask min-one-of mss [pg + 0] [set mon (mon + (([dg] of pp 6 - [gas] of rf 4) * pg))]
    ask rf 4 [set gas (gas + ([dg] of pp 6 - gas))]
    ask min-one-of mss [pg + 0] [set gas (gas - ([dg] of pp 6 - [gas] of rf 4))]]

  if [gas] of pp 6 < [dg] of pp 6 [ask rf 4 [set gas (gas - [dg] of pp 6) set mon (mon + [dg] of pp 6 * pg)]]
  ask pp 6 [(if gas < dg [set gas (gas + dg) set mon (mon - dg * [pg] of rf 4) set dg 0])]


  if [gas] of rf 3 < ([dg] of pp 7) and [gas] of rf 5 > ([dg] of pp 7) [
    ask rf 3 [set mon (mon - ([dg] of pp 7 * [pg] of rf 5))]
    ask rf 5 [set mon (mon + ([dg] of pp 7 * pg))]
    ask rf 3 [set gas(gas + [dg] of pp 7)]
    ask rf 5 [set gas (gas - [dg] of pp 7)]]
  if [gas] of rf 3 < ([dg] of pp 7) [
    ask rf 3 [set mon (mon - (([dg] of pp 7 - gas) * [pg] of min-one-of mss [pg + 0]))]
    ask min-one-of mss [pg + 0] [set mon (mon + (([dg] of pp 7 - [gas] of rf 3) * pg))]
    ask rf 3 [set gas (gas + ([dg] of pp 7 - gas))]
    ask min-one-of mss [pg + 0] [set gas (gas - ([dg] of pp 7 - [gas] of rf 3))]]

  if [gas] of pp 7 < [dg] of pp 7 [ask rf 3 [set gas (gas - [dg] of pp 7) set mon (mon + [dg] of pp 7 * pg)]]
  ask pp 7 [(if gas < dg [set gas (gas + dg) set mon (mon - dg * [pg] of rf 3) set dg 0])]


  if [slp] of rf 3 < [ds] of fp 8 [ask min-one-of mss [ps + 0] [set slp (slp - [ds] of fp 8)
    set mon (mon + [ds] of fp 8 * ps)]]
  ask rf 3 [if slp < [ds] of fp 8 [set slp (slp + [ds] of fp 8) set mon (mon - [ds] of fp 8 * [ps] of min-one-of mss [ps + 0])]]

  if [slp] of fp 8 < [ds] of fp 8 [ask rf 3 [set slp (slp - [ds] of fp 8) set mon (mon + [ds] of fp 8 * ps)]]
  if [elc] of fp 8 < [de] of fp 8 [ask pp 7 [set elc (elc - [de] of fp 8) set mon (mon + [de] of fp 8 * pelc)]]
  ask fp 8 [(if slp < ds [set slp (slp + ds) set mon (mon - ds * [ps] of rf 3) set ds 0])
    (if elc < de [set elc (elc + de) set mon (mon - de * [pelc] of pp 7) set de 0])]


  if [slp] of rf 4 < [ds] of fp 9 [ask min-one-of mss [ps + 0] [set slp (slp - [ds] of fp 9)
    set mon (mon + [ds] of fp 9 * ps)]]
  ask rf 4 [if slp < [ds] of fp 9 [set slp (slp + [ds] of fp 9) set mon (mon - [ds] of fp 9 * [ps] of min-one-of mss [ps + 0])]]

  if [slp] of fp 9 < [ds] of fp 9 [ask rf 4 [set slp (slp - [ds] of fp 9) set mon (mon + [ds] of fp 9 * ps)]]
  if [elc] of fp 9 < [de] of fp 9 [ask pp 6 [set elc (elc - [de] of fp 9) set mon (mon + [de] of fp 9 * pelc)]]
  ask fp 9 [(if slp < ds [set slp (slp + ds) set mon (mon - ds * [ps] of rf 4) set ds 0])
    (if elc < de [set elc (elc + de) set mon (mon - de * [pelc] of pp 6) set de 0])]


  if [slp] of rf 5 < [ds] of fp 10 [ask min-one-of mss [ps + 0] [set slp (slp - [ds] of fp 10)
    set mon (mon + [ds] of fp 10 * ps)]]
  ask rf 5 [if slp < [ds] of fp 10 [set slp (slp + [ds] of fp 10) set mon (mon - [ds] of fp 8 * [ps] of min-one-of mss [ps + 0])]]

  if [slp] of fp 10 < [ds] of fp 10 [ask rf 5 [set slp (slp - [ds] of fp 10) set mon (mon + [ds] of fp 10 * ps)]]
  if [elc] of fp 10 < [de] of fp 10 [ask pp 6 [set elc (elc - [de] of fp 10) set mon (mon + [de] of fp 10 * pelc)]]
  ask fp 10 [(if slp < ds [set slp (slp + ds) set mon (mon - ds * [ps] of rf 5) set ds 0])
    (if elc < de [set elc (elc + de) set mon (mon - de * [pelc] of pp 6) set de 0])]
  ]
  [
    let heatlist sort-on [ph] turtles
    if [dh] of rf 3 > 0 and [heat] of item 9 heatlist > [dh] of rf 3 [ask item 9 heatlist [set mon (mon + [dh] of rf 3 * ph) set heat (heat - [dh] of rf 3)]
      ask rf 3 [set mon (mon - dh * [ph] of item 9 heatlist) set heat (heat + dh) set dh 0]]
    if [dh] of rf 3 > 0 and [heat] of item 9 heatlist < [dh] of rf 3 [ask rf 3 [set mon (mon - [heat] of item 9 heatlist * [ph] of item 9 heatlist)
      set heat (heat + [heat] of item 9 heatlist)  set dh (dh - [heat] of item 9 heatlist)]
      ask item 9 heatlist [set mon (mon + heat * ph) set heat 0]]
    if [dh] of rf 3 > 0 and [heat] of item 10 heatlist > [dh] of rf 3 [ask item 10 heatlist [set mon (mon + [dh] of rf 3 * ph) set heat (heat - [dh] of rf 3)]
      ask rf 3 [set mon (mon - dh * [ph] of item 10 heatlist) set heat (heat + dh) set dh 0]]
    if [dh] of rf 3 > 0 and [heat] of item 10 heatlist < [dh] of rf 3 [ask rf 3 [set mon (mon - [heat] of item 10 heatlist * [ph] of item 10 heatlist)
      set heat (heat + [heat] of item 10 heatlist) set dh (dh - [heat] of item 10 heatlist)]
      ask item 10 heatlist [set mon (mon + heat * ph) set heat 0]]
    if [dh] of rf 3 > 0 [ask item 11 heatlist [set mon (mon + [dh] of rf 3 * ph) set heat (heat - [dh] of rf 3)]
      ask rf 3 [set mon (mon - dh * [ph] of item 11 heatlist) set heat (heat + dh) set dh 0]]

    if [dh] of rf 4 > 0 and [heat] of item 9 heatlist > [dh] of rf 4 [ask item 9 heatlist [set mon (mon + [dh] of rf 4 * ph) set heat (heat - [dh] of rf 4)]
      ask rf 4 [set mon (mon - dh * [ph] of item 9 heatlist) set heat (heat + dh) set dh 0]]
    if [dh] of rf 4 > 0 and [heat] of item 9 heatlist < [dh] of rf 4 [ask rf 4 [set mon (mon - [heat] of item 9 heatlist * [ph] of item 9 heatlist)
      set heat (heat + [heat] of item 9 heatlist)  set dh (dh - [heat] of item 9 heatlist)]
      ask item 9 heatlist [set mon (mon + heat * ph) set heat 0]]
    if [dh] of rf 4 > 0 and [heat] of item 10 heatlist > [dh] of rf 4 [ask item 10 heatlist [set mon (mon + [dh] of rf 4 * ph) set heat (heat - [dh] of rf 4)]
      ask rf 4 [set mon (mon - dh * [ph] of item 10 heatlist) set heat (heat + dh) set dh 0]]
    if [dh] of rf 4 > 0 and [heat] of item 10 heatlist < [dh] of rf 4 [ask rf 4 [set mon (mon - [heat] of item 10 heatlist * [ph] of item 10 heatlist)
      set heat (heat + [heat] of item 10 heatlist) set dh (dh - [heat] of item 10 heatlist)]
      ask item 10 heatlist [set mon (mon + heat * ph) set heat 0]]
    if [dh] of rf 4 > 0 [ask item 11 heatlist [set mon (mon + [dh] of rf 4 * ph) set heat (heat - [dh] of rf 4)]
      ask rf 4 [set mon (mon - dh * [ph] of item 11 heatlist) set heat (heat + dh) set dh 0]]

    if [dh] of rf 5 > 0 and [heat] of item 9 heatlist > [dh] of rf 5 [ask item 9 heatlist [set mon (mon + [dh] of rf 5 * ph) set heat (heat - [dh] of rf 5)]
      ask rf 5 [set mon (mon - dh * [ph] of item 9 heatlist) set heat (heat + dh) set dh 0]]
    if [dh] of rf 5 > 0 and [heat] of item 9 heatlist < [dh] of rf 5 [ask rf 5 [set mon (mon - [heat] of item 9 heatlist * [ph] of item 9 heatlist)
      set heat (heat + [heat] of item 9 heatlist)  set dh (dh - [heat] of item 9 heatlist)]
      ask item 9 heatlist [set mon (mon + heat * ph) set heat 0]]
    if [dh] of rf 5 > 0 and [heat] of item 10 heatlist > [dh] of rf 5 [ask item 10 heatlist [set mon (mon + [dh] of rf 5 * ph) set heat (heat - [dh] of rf 5)]
      ask rf 5 [set mon (mon - dh * [ph] of item 10 heatlist) set heat (heat + dh) set dh 0]]
    if [dh] of rf 5 > 0 and [heat] of item 10 heatlist < [dh] of rf 5 [ask rf 5 [set mon (mon - [heat] of item 10 heatlist * [ph] of item 10 heatlist)
      set heat (heat + [heat] of item 10 heatlist) set dh (dh - [heat] of item 10 heatlist)]
      ask item 10 heatlist [set mon (mon + heat * ph) set heat 0]]
    if [dh] of rf 5 > 0 [ask item 11 heatlist [set mon (mon + [dh] of rf 5 * ph) set heat (heat - [dh] of rf 5)]
      ask rf 5 [set mon (mon - dh * [ph] of item 11 heatlist) set heat (heat + dh) set dh 0]]

    let electlist sort-on [pelc] turtles
    if [de] of rf 3 > 0 and [elc] of item 9 electlist > [de] of rf 3 [ask item 9 electlist [set mon (mon + [de] of rf 3 * pelc) set elc (elc - [de] of rf 3)]
      ask rf 3 [set mon (mon - de * [pelc] of item 9 heatlist) set elc (elc + de) set de 0]]
    if [de] of rf 3 > 0 and [elc] of item 9 electlist < [de] of rf 3 [ask rf 3 [set mon (mon - [elc] of item 9 electlist * [pelc] of item 9 electlist)
      set elc (elc + [elc] of item 9 electlist)  set de (de - [elc] of item 9 electlist)]
      ask item 9 electlist [set mon (mon + elc * pelc) set elc 0]]
    if [de] of rf 3 > 0 and [elc] of item 10 electlist > [de] of rf 3 [ask item 10 electlist [set mon (mon + [de] of rf 3 * pelc) set elc (elc - [de] of rf 3)]
      ask rf 3 [set mon (mon - de * [pelc] of item 10 electlist) set elc (elc + de) set de 0]]
    if [de] of rf 3 > 0 and [elc] of item 10 electlist < [de] of rf 3 [ask rf 3 [set mon (mon - [elc] of item 10 electlist * [pelc] of item 10 electlist)
      set elc (elc + [elc] of item 10 electlist) set de (de - [elc] of item 10 electlist)]
      ask item 10 electlist [set mon (mon + elc * pelc) set elc 0]]
    if [de] of rf 3 > 0 [ask item 11 electlist [set mon (mon + [de] of rf 3 * pelc) set elc (elc - [de] of rf 3)]
      ask rf 3 [set mon (mon - de * [pelc] of item 11 electlist) set elc (elc + de) set de 0]]

    if [de] of rf 4 > 0 and [elc] of item 9 electlist > [de] of rf 4 [ask item 9 electlist [set mon (mon + [de] of rf 4 * pelc) set elc (elc - [de] of rf 4)]
      ask rf 4 [set mon (mon - de * [pelc] of item 9 heatlist) set elc (elc + de) set de 0]]
    if [de] of rf 4 > 0 and [elc] of item 9 electlist < [de] of rf 4 [ask rf 4 [set mon (mon - [elc] of item 9 electlist * [pelc] of item 9 electlist)
      set elc (elc + [elc] of item 9 electlist)  set de (de - [elc] of item 9 electlist)]
      ask item 9 electlist [set mon (mon + elc * pelc) set elc 0]]
    if [de] of rf 4 > 0 and [elc] of item 10 electlist > [de] of rf 4 [ask item 10 electlist [set mon (mon + [de] of rf 4 * pelc) set elc (elc - [de] of rf 3)]
      ask rf 4 [set mon (mon - de * [pelc] of item 10 electlist) set elc (elc + de) set de 0]]
    if [de] of rf 4 > 0 and [elc] of item 10 electlist < [de] of rf 4 [ask rf 4 [set mon (mon - [elc] of item 10 electlist * [pelc] of item 10 electlist)
      set elc (elc + [elc] of item 10 electlist) set de (de - [elc] of item 10 electlist)]
      ask item 10 electlist [set mon (mon + elc * pelc) set elc 0]]
    if [de] of rf 4 > 0 [ask item 11 electlist [set mon (mon + [de] of rf 4 * pelc) set elc (elc - [de] of rf 4)]
      ask rf 4 [set mon (mon - de * [pelc] of item 11 electlist) set elc (elc + de) set de 0]]

    if [de] of rf 5 > 0 and [elc] of item 9 electlist > [de] of rf 5 [ask item 9 electlist [set mon (mon + [de] of rf 5 * pelc) set elc (elc - [de] of rf 5)]
      ask rf 5 [set mon (mon - de * [pelc] of item 9 heatlist) set elc (elc + de) set de 0]]
    if [de] of rf 5 > 0 and [elc] of item 9 electlist < [de] of rf 5 [ask rf 5 [set mon (mon - [elc] of item 9 electlist * [pelc] of item 9 electlist)
      set elc (elc + [elc] of item 9 electlist)  set de (de - [elc] of item 9 electlist)]
      ask item 9 electlist [set mon (mon + elc * pelc) set elc 0]]
    if [de] of rf 5 > 0 and [elc] of item 10 electlist > [de] of rf 5 [ask item 10 electlist [set mon (mon + [de] of rf 5 * pelc) set elc (elc - [de] of rf 5)]
      ask rf 5 [set mon (mon - de * [pelc] of item 10 electlist) set elc (elc + de) set de 0]]
    if [de] of rf 5 > 0 and [elc] of item 10 electlist < [de] of rf 5 [ask rf 5 [set mon (mon - [elc] of item 10 electlist * [pelc] of item 10 electlist)
      set elc (elc + [elc] of item 10 electlist) set de (de - [elc] of item 10 electlist)]
      ask item 10 electlist [set mon (mon + elc * pelc) set elc 0]]
    if [de] of rf 5 > 0 [ask item 11 electlist [set mon (mon + [de] of rf 5 * pelc) set elc (elc - [de] of rf 5)]
      ask rf 5 [set mon (mon - de * [pelc] of item 11 electlist) set elc (elc + de) set de 0]]



    let gaslist sort-on [pg] turtles
    if [dg] of pp 7 > 0 and [gas] of item 8 gaslist > [dg] of pp 7 [ask item 8 gaslist [set mon (mon + [dg] of pp 7 * pg) set gas (gas - [dg] of pp 7)]
      ask pp 7 [set mon (mon - dg * [pg] of item 8 gaslist) set gas (gas + dg) set dg 0]]
    if [dg] of pp 7 > 0 and [gas] of item 8 gaslist < [dg] of pp 7 [ask pp 7 [set mon (mon - [gas] of item 8 gaslist * [pg] of item 8 gaslist)
      set gas (gas + [gas] of item 8 gaslist) set dg (dg - [gas] of item 8 gaslist)]
      ask item 8 gaslist [set mon (mon + gas * pg) set gas 0]]
    if [dg] of pp 7 > 0 and [gas] of item 9 gaslist > [dg] of pp 7 [ask item 9 gaslist [set mon (mon + [dg] of pp 7 * pg) set gas (gas - [dg] of pp 7)]
      ask pp 7 [set mon (mon - dg * [pg] of item 9 gaslist) set gas (gas + dg) set dg 0]]
    if [dg] of pp 7 > 0 and [gas] of item 9 gaslist < [dg] of pp 7 [ask pp 7 [set mon (mon - [gas] of item 9 gaslist * [pg] of item 9 gaslist)
      set gas (gas + [gas] of item 9 gaslist) set dg (dg - [gas] of item 9 gaslist)]
      ask item 9 gaslist [set mon (mon + gas * pg) set gas 0]]
    if [dg] of pp 7 > 0 and [gas] of item 10 gaslist > [dg] of pp 7 [ask item 10 gaslist [set mon (mon + [dg] of pp 7 * pg) set gas (gas - [dg] of pp 7)]
      ask pp 7 [set mon (mon - dg * [pg] of item 10 gaslist) set gas (gas + dg) set dg 0]]
    if [dg] of pp 7 > 0 and [gas] of item 10 gaslist < [dg] of pp 7 [ask pp 7 [set mon (mon - [gas] of item 10 gaslist * [pg] of item 10 gaslist)
      set gas (gas + [gas] of item 10 gaslist) set dg (dg - [gas] of item 10 gaslist)]
      ask item 10 gaslist [set mon (mon + gas * pg) set gas 0]]
    if [dg] of pp 7 > 0 and [gas] of item 11 gaslist > [dg] of pp 7 [ask item 11 gaslist [set mon (mon + [dg] of pp 7 * pg) set gas (gas - [dg] of pp 7)]
      ask pp 7 [set mon (mon - dg * [pg] of item 11 gaslist) set gas (gas + dg) set dg 0]]
    if [dg] of pp 7 > 0 and [gas] of item 11 gaslist < [dg] of pp 7 [ask pp 7 [set mon (mon - [gas] of item 11 gaslist * [pg] of item 11 gaslist)
      set gas (gas + [gas] of item 11 gaslist) set dg (dg - [gas] of item 11 gaslist)]
      ask item 11 gaslist [set mon (mon + gas * pg) set gas 0]]


    if [dg] of pp 6 > 0 and [gas] of item 8 gaslist > [dg] of pp 6 [ask item 8 gaslist [set mon (mon + [dg] of pp 6 * pg) set gas (gas - [dg] of pp 6)]
      ask pp 6 [set mon (mon - dg * [pg] of item 8 gaslist) set gas (gas + dg) set dg 0]]
    if [dg] of pp 6 > 0 and [gas] of item 8 gaslist < [dg] of pp 6 [ask pp 6 [set mon (mon - [gas] of item 8 gaslist * [pg] of item 8 gaslist)
      set gas (gas + [gas] of item 8 gaslist) set dg (dg - [gas] of item 8 gaslist)]
      ask item 8 gaslist [set mon (mon + gas * pg) set gas 0]]
    if [dg] of pp 6 > 0 and [gas] of item 9 gaslist > [dg] of pp 6 [ask item 9 gaslist [set mon (mon + [dg] of pp 6 * pg) set gas (gas - [dg] of pp 6)]
      ask pp 6 [set mon (mon - dg * [pg] of item 9 gaslist) set gas (gas + dg) set dg 0]]
    if [dg] of pp 6 > 0 and [gas] of item 9 gaslist < [dg] of pp 6 [ask pp 6 [set mon (mon - [gas] of item 9 gaslist * [pg] of item 9 gaslist)
      set gas (gas + [gas] of item 9 gaslist) set dg (dg - [gas] of item 9 gaslist)]
      ask item 9 gaslist [set mon (mon + gas * pg) set gas 0]]
    if [dg] of pp 6 > 0 and [gas] of item 10 gaslist > [dg] of pp 6 [ask item 10 gaslist [set mon (mon + [dg] of pp 6 * pg) set gas (gas - [dg] of pp 6)]
      ask pp 6 [set mon (mon - dg * [pg] of item 10 gaslist) set gas (gas + dg) set dg 0]]
    if [dg] of pp 6 > 0 and [gas] of item 10 gaslist < [dg] of pp 6 [ask pp 6 [set mon (mon - [gas] of item 10 gaslist * [pg] of item 10 gaslist)
      set gas (gas + [gas] of item 10 gaslist) set dg (dg - [gas] of item 10 gaslist)]
      ask item 10 gaslist [set mon (mon + gas * pg) set gas 0]]
    if [dg] of pp 6 > 0 and [gas] of item 11 gaslist > [dg] of pp 6 [ask item 11 gaslist [set mon (mon + [dg] of pp 6 * pg) set gas (gas - [dg] of pp 6)]
      ask pp 6 [set mon (mon - dg * [pg] of item 11 gaslist) set gas (gas + dg) set dg 0]]
    if [dg] of pp 6 > 0 and [gas] of item 11 gaslist < [dg] of pp 6 [ask pp 6 [set mon (mon - [gas] of item 11 gaslist * [pg] of item 11 gaslist)
      set gas (gas + [gas] of item 11 gaslist) set dg (dg - [gas] of item 11 gaslist)]
      ask item 11 gaslist [set mon (mon + gas * pg) set gas 0]]


    let sulplist sort-on [ps] turtles
    if [ds] of fp 8 > 0 and [slp] of item 8 sulplist > [ds] of fp 8 [ask item 8 sulplist [set mon (mon - [ds] of fp 8 * ps) set slp (slp - [ds] of fp 8)]
      ask fp 8 [set mon (mon - ds * [ps] of item 8 sulplist) set slp (slp + ds) set ds 0]]
    if [ds] of fp 8 > 0 and [slp] of item 8 sulplist < [ds] of fp 8 [ask fp 8 [set mon (mon - [slp] of item 8 sulplist * [ps] of item 8 sulplist)
      set slp (slp + [slp] of item 8 sulplist) set ds (ds - [slp] of item 8 sulplist)]
      ask item 8 sulplist [set mon (mon + slp * ps) set slp 0]]
    if [ds] of fp 8 > 0 and [slp] of item 9 sulplist > [ds] of fp 8 [ask item 9 sulplist [set mon (mon - [ds] of fp 8 * ps) set slp (slp - [ds] of fp 8)]
      ask fp 8 [set mon (mon - ds * [ps] of item 9 sulplist) set slp (slp + ds) set ds 0]]
    if [ds] of fp 8 > 0 and [slp] of item 9 sulplist < [ds] of fp 8 [ask fp 8 [set mon (mon - [slp] of item 9 sulplist * [ps] of item 9 sulplist)
      set slp (slp + [slp] of item 9 sulplist) set ds (ds - [slp] of item 9 sulplist)]
      ask item 9 sulplist [set mon (mon + slp * ps) set slp 0]]
    if [ds] of fp 8 > 0 and [slp] of item 10 sulplist > [ds] of fp 8 [ask item 10 sulplist [set mon (mon - [ds] of fp 8 * ps) set slp (slp - [ds] of fp 8)]
      ask fp 8 [set mon (mon - ds * [ps] of item 10 sulplist) set slp (slp + ds) set ds 0]]
    if [ds] of fp 8 > 0 and [slp] of item 10 sulplist < [ds] of fp 8 [ask fp 8 [set mon (mon - [slp] of item 10 sulplist * [ps] of item 10 sulplist)
      set slp (slp + [slp] of item 10 sulplist) set ds (ds - [slp] of item 10 sulplist)]
      ask item 10 sulplist [set mon (mon + slp * ps) set slp 0]]
    if [ds] of fp 8 > 0 and [slp] of item 11 sulplist > [ds] of fp 8 [ask item 11 sulplist [set mon (mon - [ds] of fp 8 * ps) set slp (slp - [ds] of fp 8)]
      ask fp 8 [set mon (mon - ds * [ps] of item 11 sulplist) set slp (slp + ds) set ds 0]]
    if [ds] of fp 8 > 0 and [slp] of item 11 sulplist < [ds] of fp 8 [ask fp 8 [set mon (mon - [slp] of item 11 sulplist * [ps] of item 11 sulplist)
      set slp (slp + [slp] of item 11 sulplist) set ds (ds - [slp] of item 11 sulplist)]
      ask item 11 sulplist [set mon (mon + slp * ps) set slp 0]]

    if [ds] of fp 9 > 0 and [slp] of item 8 sulplist > [ds] of fp 9 [ask item 8 sulplist [set mon (mon - [ds] of fp 9 * ps) set slp (slp - [ds] of fp 9)]
      ask fp 9 [set mon (mon - ds * [ps] of item 8 sulplist) set slp (slp + ds) set ds 0]]
    if [ds] of fp 9 > 0 and [slp] of item 8 sulplist < [ds] of fp 9 [ask fp 9 [set mon (mon - [slp] of item 8 sulplist * [ps] of item 8 sulplist)
      set slp (slp + [slp] of item 8 sulplist) set ds (ds - [slp] of item 8 sulplist)]
      ask item 8 sulplist [set mon (mon + slp * ps) set slp 0]]
    if [ds] of fp 9 > 0 and [slp] of item 9 sulplist > [ds] of fp 9 [ask item 9 sulplist [set mon (mon - [ds] of fp 9 * ps) set slp (slp - [ds] of fp 9)]
      ask fp 9 [set mon (mon - ds * [ps] of item 9 sulplist) set slp (slp + ds) set ds 0]]
    if [ds] of fp 9 > 0 and [slp] of item 9 sulplist < [ds] of fp 9 [ask fp 9 [set mon (mon - [slp] of item 9 sulplist * [ps] of item 9 sulplist)
      set slp (slp + [slp] of item 9 sulplist) set ds (ds - [slp] of item 9 sulplist)]
      ask item 9 sulplist [set mon (mon + slp * ps) set slp 0]]
    if [ds] of fp 9 > 0 and [slp] of item 10 sulplist > [ds] of fp 9 [ask item 10 sulplist [set mon (mon - [ds] of fp 9 * ps) set slp (slp - [ds] of fp 9)]
      ask fp 9 [set mon (mon - ds * [ps] of item 10 sulplist) set slp (slp + ds) set ds 0]]
    if [ds] of fp 9 > 0 and [slp] of item 10 sulplist < [ds] of fp 9 [ask fp 9 [set mon (mon - [slp] of item 10 sulplist * [ps] of item 10 sulplist)
      set slp (slp + [slp] of item 10 sulplist) set ds (ds - [slp] of item 10 sulplist)]
      ask item 10 sulplist [set mon (mon + slp * ps) set slp 0]]
    if [ds] of fp 9 > 0 and [slp] of item 11 sulplist > [ds] of fp 9 [ask item 11 sulplist [set mon (mon - [ds] of fp 9 * ps) set slp (slp - [ds] of fp 9)]
      ask fp 9 [set mon (mon - ds * [ps] of item 11 sulplist) set slp (slp + ds) set ds 0]]
    if [ds] of fp 9 > 0 and [slp] of item 11 sulplist < [ds] of fp 9 [ask fp 9 [set mon (mon - [slp] of item 11 sulplist * [ps] of item 11 sulplist)
      set slp (slp + [slp] of item 11 sulplist) set ds (ds - [slp] of item 11 sulplist)]
      ask item 11 sulplist [set mon (mon + slp * ps) set slp 0]]

    if [ds] of fp 10 > 0 and [slp] of item 8 sulplist > [ds] of fp 10 [ask item 8 sulplist [set mon (mon - [ds] of fp 10 * ps) set slp (slp - [ds] of fp 10)]
      ask fp 10 [set mon (mon - ds * [ps] of item 8 sulplist) set slp (slp + ds) set ds 0]]
    if [ds] of fp 10 > 0 and [slp] of item 8 sulplist < [ds] of fp 10 [ask fp 10 [set mon (mon - [slp] of item 8 sulplist * [ps] of item 8 sulplist)
      set slp (slp + [slp] of item 8 sulplist) set ds (ds - [slp] of item 8 sulplist)]
      ask item 8 sulplist [set mon (mon + slp * ps) set slp 0]]
    if [ds] of fp 10 > 0 and [slp] of item 9 sulplist > [ds] of fp 10 [ask item 9 sulplist [set mon (mon - [ds] of fp 10 * ps) set slp (slp - [ds] of fp 10)]
      ask fp 10 [set mon (mon - ds * [ps] of item 9 sulplist) set slp (slp + ds) set ds 0]]
    if [ds] of fp 10 > 0 and [slp] of item 9 sulplist < [ds] of fp 10 [ask fp 10 [set mon (mon - [slp] of item 9 sulplist * [ps] of item 9 sulplist)
      set slp (slp + [slp] of item 9 sulplist) set ds (ds - [slp] of item 9 sulplist)]
      ask item 9 sulplist [set mon (mon + slp * ps) set slp 0]]
    if [ds] of fp 10 > 0 and [slp] of item 10 sulplist > [ds] of fp 10 [ask item 10 sulplist [set mon (mon - [ds] of fp 10 * ps) set slp (slp - [ds] of fp 10)]
      ask fp 10 [set mon (mon - ds * [ps] of item 10 sulplist) set slp (slp + ds) set ds 0]]
    if [ds] of fp 10 > 0 and [slp] of item 10 sulplist < [ds] of fp 10 [ask fp 10 [set mon (mon - [slp] of item 10 sulplist * [ps] of item 10 sulplist)
      set slp (slp + [slp] of item 10 sulplist) set ds (ds - [slp] of item 10 sulplist)]
      ask item 10 sulplist [set mon (mon + slp * ps) set slp 0]]
    if [ds] of fp 10 > 0 and [slp] of item 11 sulplist > [ds] of fp 10 [ask item 11 sulplist [set mon (mon - [ds] of fp 10 * ps) set slp (slp - [ds] of fp 10)]
      ask fp 10 [set mon (mon - ds * [ps] of item 11 sulplist) set slp (slp + ds) set ds 0]]
    if [ds] of fp 10 > 0 and [slp] of item 11 sulplist < [ds] of fp 10 [ask fp 10 [set mon (mon - [slp] of item 11 sulplist * [ps] of item 11 sulplist)
      set slp (slp + [slp] of item 11 sulplist) set ds (ds - [slp] of item 11 sulplist)]
      ask item 11 sulplist [set mon (mon + slp * ps) set slp 0]]
  ]
end


to sell-products
  let rpdlist sort-on [(- pr)] turtles
  if [dr] of mb 11 > 0 and [rpd] of item 5 rpdlist > [dr] of mb 11 [ask item 5 rpdlist [set rpd (rpd - [dr] of mb 11) set mon (mon + [dr] of mb 11 * pr)]
    ask mb 11 [set mon (mon - dr * [pr] of item 5 rpdlist) set rpd (rpd + dr) set dr 0]]
  if [dr] of mb 11 > 0 and [rpd] of item 5 rpdlist < [dr] of mb 11 [ask mb 11 [set mon (mon - [rpd] of item 5 rpdlist * [pr] of item 5 rpdlist)
    set rpd (rpd + [rpd] of item 5 rpdlist) set dr (dr - [rpd] of item 5 rpdlist)]
    ask item 5 rpdlist [set mon (mon + rpd * pr) set rpd 0]]
  if [dr] of mb 11 > 0 and [rpd] of item 4 rpdlist > [dr] of mb 11 [ask item 4 rpdlist [set rpd (rpd - [dr] of mb 11) set mon (mon + [dr] of mb 11 * pr)]
    ask mb 11 [set mon (mon - dr * [pr] of item 4 rpdlist) set rpd (rpd + dr) set dr 0]]
  if [dr] of mb 11 > 0 and [rpd] of item 4 rpdlist < [dr] of mb 11 [ask mb 11 [set mon (mon - [rpd] of item 4 rpdlist * [pr] of item 4 rpdlist)
    set rpd (rpd + [rpd] of item 4 rpdlist) set dr (dr - [rpd] of item 4 rpdlist)]
      ask item 4 rpdlist [set mon (mon + rpd * pr) set rpd 0]]
  if [dr] of mb 11 > 0 and [rpd] of item 3 rpdlist > [dr] of mb 11 [ask item 3 rpdlist [set rpd (rpd - [dr] of mb 11) set mon (mon + [dr] of mb 11 * pr)]
    ask mb 11 [set mon (mon - dr * [pr] of item 3  rpdlist) set rpd (rpd + dr) set dr 0]]
  if [dr] of mb 11 > 0 and [rpd] of item 3 rpdlist < [dr] of mb 11 [ask mb 11 [set mon (mon - [rpd] of item 3 rpdlist * [pr] of item 3  rpdlist)
    set rpd (rpd + [rpd] of item 3 rpdlist) set dr (dr - [rpd] of item 3 rpdlist)]
    ask item 3 rpdlist [set mon (mon + rpd * pr) set rpd 0]]
  if [dr] of mb 11 > 0 [ask item 2 rpdlist [set rpd (rpd - [dr] of mb 11) set mon (mon + [dr] of mb 11 * pr)]
    ask mb 11 [set mon (mon - dr * [pr] of item 2  rpdlist) set rpd (rpd + dr) set dr 0]]

  if [dr] of mb 12 > 0 and [rpd] of item 5 rpdlist > [dr] of mb 12 [ask item 5 rpdlist [set rpd (rpd - [dr] of mb 12) set mon (mon + [dr] of mb 12 * pr)]
    ask mb 12 [set mon (mon - dr * [pr] of item 5 rpdlist) set rpd (rpd + dr) set dr 0]]
  if [dr] of mb 12 > 0 and [rpd] of item 5 rpdlist < [dr] of mb 12 [ask mb 12 [set mon (mon - [rpd] of item 5 rpdlist * [pr] of item 5 rpdlist)
    set rpd (rpd + [rpd] of item 5 rpdlist) set dr (dr - [rpd] of item 5 rpdlist)]
    ask item 5 rpdlist [set mon (mon + rpd * pr) set rpd 0]]
  if [dr] of mb 12 > 0 and [rpd] of item 4 rpdlist > [dr] of mb 12 [ask item 4 rpdlist [set rpd (rpd - [dr] of mb 12) set mon (mon + [dr] of mb 12 * pr)]
    ask mb 12 [set mon (mon - dr * [pr] of item 4 rpdlist) set rpd (rpd + dr) set dr 0]]
  if [dr] of mb 12 > 0 and [rpd] of item 4 rpdlist < [dr] of mb 12 [ask mb 12 [set mon (mon - [rpd] of item 4 rpdlist * [pr] of item 4 rpdlist)
    set rpd (rpd + [rpd] of item 4 rpdlist) set dr (dr - [rpd] of item 4 rpdlist)]
    ask item 4 rpdlist [set mon (mon + rpd * pr) set rpd 0]]
  if [dr] of mb 12 > 0 and [rpd] of item 3 rpdlist > [dr] of mb 12 [ask item 3 rpdlist [set rpd (rpd - [dr] of mb 12) set mon (mon + [dr] of mb 12 * pr)]
    ask mb 12 [set mon (mon - dr * [pr] of item 3  rpdlist) set rpd (rpd + dr) set dr 0]]
  if [dr] of mb 12 > 0 and [rpd] of item 3 rpdlist < [dr] of mb 12 [ask mb 12 [set mon (mon - [rpd] of item 3 rpdlist * [pr] of item 3  rpdlist)
    set rpd (rpd + [rpd] of item 3 rpdlist) set dr (dr - [rpd] of item 3 rpdlist)]
    ask item 3 rpdlist [set mon (mon + rpd * pr) set rpd 0]]
  if [dr] of mb 12 > 0 [ask item 2 rpdlist [set rpd (rpd - [dr] of mb 12) set mon (mon + [dr] of mb 12 * pr)]
    ask mb 12 [set mon (mon - dr * [pr] of item 2  rpdlist) set rpd (rpd + dr) set dr 0]]

  if [dr] of mb 13 > 0 and [rpd] of item 5 rpdlist > [dr] of mb 13 [ask item 5 rpdlist [set rpd (rpd - [dr] of mb 13) set mon (mon + [dr] of mb 13 * pr)]
    ask mb 13 [set mon (mon - dr * [pr] of item 5 rpdlist) set rpd (rpd + dr) set dr 0]]
  if [dr] of mb 13 > 0 and [rpd] of item 5 rpdlist < [dr] of mb 13 [ask mb 13 [set mon (mon - [rpd] of item 5 rpdlist * [pr] of item 5 rpdlist)
    set rpd (rpd + [rpd] of item 5 rpdlist) set dr (dr - [rpd] of item 5 rpdlist)]
    ask item 5 rpdlist [set mon (mon + rpd * pr) set rpd 0]]
  if [dr] of mb 13 > 0 and [rpd] of item 4 rpdlist > [dr] of mb 13 [ask item 4 rpdlist [set rpd (rpd - [dr] of mb 13) set mon (mon + [dr] of mb 13 * pr)]
    ask mb 13 [set mon (mon - dr * [pr] of item 4 rpdlist) set rpd (rpd + dr) set dr 0]]
  if [dr] of mb 13 > 0 and [rpd] of item 4 rpdlist < [dr] of mb 13 [ask mb 13 [set mon (mon - [rpd] of item 4 rpdlist * [pr] of item 4 rpdlist)
    set rpd (rpd + [rpd] of item 4 rpdlist) set dr (dr - [rpd] of item 4 rpdlist)]
    ask item 4 rpdlist [set mon (mon + rpd * pr) set rpd 0]]
  if [dr] of mb 13 > 0 and [rpd] of item 3 rpdlist > [dr] of mb 13 [ask item 3 rpdlist [set rpd (rpd - [dr] of mb 13) set mon (mon + [dr] of mb 13 * pr)]
    ask mb 13 [set mon (mon - dr * [pr] of item 3  rpdlist) set rpd (rpd + dr) set dr 0]]
  if [dr] of mb 13 > 0 and [rpd] of item 3 rpdlist < [dr] of mb 13 [ask mb 13 [set mon (mon - [rpd] of item 3 rpdlist * [pr] of item 3  rpdlist)
    set rpd (rpd + [rpd] of item 3 rpdlist) set dr (dr - [rpd] of item 3 rpdlist)]
    ask item 3 rpdlist [set mon (mon + rpd * pr) set rpd 0]]
  if [dr] of mb 13 > 0 [ask item 2 rpdlist [set rpd (rpd - [dr] of mb 13) set mon (mon + [dr] of mb 13 * pr)]
    ask mb 13 [set mon (mon - dr * [pr] of item 2  rpdlist) set rpd (rpd + dr) set dr 0]]


  let elclist sort-on [(- pelc)] turtles
  if [de] of mb 11 > 0 and [elc] of item 4 elclist > [de] of mb 11 [ask item 4 elclist [set elc (elc - [de] of mb 11) set mon (mon + [de] of mb 11 * pelc)]
    ask mb 11 [set mon (mon - de * [pelc] of item 4 elclist) set elc (elc + de) set de 0]]
  if [de] of mb 11 > 0 and [elc] of item 4 elclist < [de] of mb 11 [ask mb 11 [set mon (mon - [elc] of item 4 elclist * [pelc] of item 4 elclist)
    set elc (elc + [elc] of item 4 elclist) set de (de - [elc] of item 4 elclist)]
    ask item 4 elclist [set mon (mon + elc * pelc) set elc 0]]
  if [de] of mb 11 > 0 and [elc] of item 3 elclist > [de] of mb 11 [ask item 3 elclist [set elc (elc - [de] of mb 11) set mon (mon + [de] of mb 11 * pelc)]
    ask mb 11 [set mon (mon - de * [pelc] of item 3 elclist) set elc (elc + de) set de 0]]
  if [de] of mb 11 > 0 and [elc] of item 3 elclist < [de] of mb 11 [ask mb 11 [set mon (mon - [elc] of item 3 elclist * [pelc] of item 3 elclist)
    set elc (elc + [elc] of item 3 elclist) set de (de - [elc] of item 3 elclist)]
    ask item 3 elclist [set mon (mon + elc * pelc) set elc 0]]
  if [de] of mb 11 > 0 [ask item 2 elclist [set elc (elc - [de] of mb 11) set mon (mon + [de] of mb 11 * pelc)]
    ask mb 11 [set mon (mon - de * [pelc] of item 2  elclist) set elc (elc + de) set de 0]]

  if [de] of mb 12 > 0 and [elc] of item 4 elclist > [de] of mb 12 [ask item 4 elclist [set elc (elc - [de] of mb 12) set mon (mon + [de] of mb 12 * pelc)]
    ask mb 12 [set mon (mon - de * [pelc] of item 4 elclist) set elc (elc + de) set de 0]]
  if [de] of mb 12 > 0 and [elc] of item 4 elclist < [de] of mb 12 [ask mb 12 [set mon (mon - [elc] of item 4 elclist * [pelc] of item 4 elclist)
    set elc (elc + [elc] of item 4 elclist) set de (de - [elc] of item 4 elclist)]
    ask item 4 elclist [set mon (mon + elc * pelc) set elc 0]]
  if [de] of mb 12 > 0 and [elc] of item 3 elclist > [de] of mb 12 [ask item 3 elclist [set elc (elc - [de] of mb 12) set mon (mon + [de] of mb 12 * pelc)]
    ask mb 12 [set mon (mon - de * [pelc] of item 3 elclist) set elc (elc + de) set de 0]]
  if [de] of mb 12 > 0 and [elc] of item 3 elclist < [de] of mb 12 [ask mb 12 [set mon (mon - [elc] of item 3 elclist * [pelc] of item 3 elclist)
    set elc (elc + [elc] of item 3 elclist) set de (de - [elc] of item 3 elclist)]
    ask item 3 elclist [set mon (mon + elc * pelc) set elc 0]]
  if [de] of mb 12 > 0 [ask item 2 elclist [set elc (elc - [de] of mb 12) set mon (mon + [de] of mb 12 * pelc)]
    ask mb 12 [set mon (mon - de * [pelc] of item 2  elclist) set elc (elc + de) set de 0]]

  if [de] of mb 13 > 0 and [elc] of item 4 elclist > [de] of mb 13 [ask item 4 elclist [set elc (elc - [de] of mb 13) set mon (mon + [de] of mb 13 * pelc)]
    ask mb 13 [set mon (mon - de * [pelc] of item 4 elclist) set elc (elc + de) set de 0]]
  if [de] of mb 13 > 0 and [elc] of item 4 elclist < [de] of mb 13 [ask mb 13 [set mon (mon - [elc] of item 4 elclist * [pelc] of item 4 elclist)
    set elc (elc + [elc] of item 4 elclist) set de (de - [elc] of item 4 elclist)]
    ask item 4 elclist [set mon (mon + elc * pelc) set elc 0]]
  if [de] of mb 13 > 0 and [elc] of item 3 elclist > [de] of mb 13 [ask item 3 elclist [set elc (elc - [de] of mb 13) set mon (mon + [de] of mb 13 * pelc)]
    ask mb 13 [set mon (mon - de * [pelc] of item 3 elclist) set elc (elc + de) set de 0]]
  if [de] of mb 13 > 0 and [elc] of item 3 elclist < [de] of mb 13 [ask mb 13 [set mon (mon - [elc] of item 3 elclist * [pelc] of item 3 elclist)
    set elc (elc + [elc] of item 3 elclist) set de (de - [elc] of item 3 elclist)]
    ask item 1 elclist [set mon (mon + elc * pelc) set elc 0]]
  if [de] of mb 13 > 0 [ask item 2 elclist [set elc (elc - [de] of mb 13) set mon (mon + [de] of mb 13 * pelc)]
    ask mb 13 [set mon (mon - de * [pelc] of item 2  elclist) set elc (elc + de) set de 0]]


  let fpdlist sort-on [(- pf)] turtles
  if [df] of mb 11 > 0 and [fpd] of item 5 fpdlist > [df] of mb 11 [ask item 5 fpdlist [set fpd (fpd - [df] of mb 11) set mon (mon + [df] of mb 11 * pf)]
    ask mb 11 [set mon (mon - df * [pf] of item 5 fpdlist) set fpd (fpd + df) set df 0]]
  if [df] of mb 11 > 0 and [fpd] of item 5 fpdlist < [df] of mb 11 [ask mb 11 [set mon (mon - [fpd] of item 5 fpdlist * [pf] of item 5 fpdlist)
    set fpd (fpd + [fpd] of item 5 fpdlist) set df (df - [fpd] of item 5 fpdlist)]
    ask item 5 fpdlist [set mon (mon + fpd * pf) set fpd 0]]
  if [df] of mb 11 > 0 and [fpd] of item 4 fpdlist > [df] of mb 11 [ask item 4 fpdlist [set fpd (fpd - [df] of mb 11) set mon (mon + [df] of mb 11 * pf)]
    ask mb 11 [set mon (mon - df * [pf] of item 4 fpdlist) set fpd (fpd + df) set df 0]]
  if [df] of mb 11 > 0 and [fpd] of item 4 fpdlist < [df] of mb 11 [ask mb 11 [set mon (mon - [fpd] of item 4 fpdlist * [pf] of item 4 fpdlist)
    set fpd (fpd + [fpd] of item 4 fpdlist) set df (df - [fpd] of item 4 fpdlist)] ask item 4 fpdlist [set mon (mon + fpd * pf) set fpd 0]]
  if [df] of mb 11 > 0 and [fpd] of item 3 fpdlist > [df] of mb 11 [ask item 3 fpdlist [set fpd (fpd - [df] of mb 11) set mon (mon + [df] of mb 11 * pf)]
    ask mb 11 [set mon (mon - df * [pf] of item 3  fpdlist) set fpd (fpd + df) set df 0]]
  if [df] of mb 11 > 0 and [fpd] of item 3 fpdlist < [df] of mb 11 [ask mb 11 [set mon (mon - [fpd] of item 3 fpdlist * [pf] of item 3  fpdlist)
    set fpd (fpd + [fpd] of item 3 fpdlist) set df (df - [fpd] of item 3 fpdlist)] ask item 3 fpdlist [set mon (mon + fpd * pf) set fpd 0]]
  if [df] of mb 11 > 0 [ask item 2 fpdlist [set fpd (fpd - [df] of mb 11) set mon (mon + [df] of mb 11 * pf)]
    ask mb 11 [set mon (mon - df * [pf] of item 2  fpdlist) set fpd (fpd + df) set df 0]]

  if [df] of mb 12 > 0 and [fpd] of item 5 fpdlist > [df] of mb 12 [ask item 5 fpdlist [set fpd (fpd - [df] of mb 12) set mon (mon + [df] of mb 12 * pf)]
    ask mb 12 [set mon (mon - df * [pf] of item 5 fpdlist) set fpd (fpd + df) set df 0]]
  if [df] of mb 12 > 0 and [fpd] of item 5 fpdlist < [df] of mb 12 [ask mb 12 [set mon (mon - [fpd] of item 5 fpdlist * [pf] of item 5 fpdlist)
    set fpd (fpd + [fpd] of item 5 fpdlist) set df (df - [fpd] of item 5 fpdlist)]
    ask item 5 fpdlist [set mon (mon + fpd * pf) set fpd 0]]
  if [df] of mb 12 > 0 and [fpd] of item 4 fpdlist > [df] of mb 12 [ask item 4 fpdlist [set fpd (fpd - [df] of mb 12) set mon (mon + [df] of mb 12 * pf)]
    ask mb 12 [set mon (mon - df * [pf] of item 4 fpdlist) set fpd (fpd + df) set df 0]]
  if [df] of mb 12 > 0 and [fpd] of item 4 fpdlist < [df] of mb 12 [ask mb 12 [set mon (mon - [fpd] of item 4 fpdlist * [pf] of item 4 fpdlist)
    set fpd (fpd + [fpd] of item 4 fpdlist) set df (df - [fpd] of item 4 fpdlist)]
    ask item 4 fpdlist [set mon (mon + fpd * pf) set fpd 0]]
  if [df] of mb 12 > 0 and [fpd] of item 3 fpdlist > [df] of mb 12 [ask item 3 fpdlist [set fpd (fpd - [df] of mb 12) set mon (mon + [df] of mb 12 * pf)]
    ask mb 12 [set mon (mon - df * [pf] of item 3  fpdlist) set fpd (fpd + df) set df 0]]
  if [df] of mb 12 > 0 and [fpd] of item 3 fpdlist < [df] of mb 12 [ask mb 12 [set mon (mon - [fpd] of item 3 fpdlist * [pf] of item 3  fpdlist)
    set fpd (fpd + [fpd] of item 3 fpdlist) set df (df - [fpd] of item 3 fpdlist)] ask item 3 fpdlist [set mon (mon + fpd * pf) set fpd 0]]
  if [df] of mb 12 > 0 [ask item 2 fpdlist [set fpd (fpd - [df] of mb 12) set mon (mon + [df] of mb 12 * pf)]
    ask mb 12 [set mon (mon - df * [pf] of item 2  fpdlist) set fpd (fpd + df) set df 0]]

  if [df] of mb 13 > 0 and [fpd] of item 5 fpdlist > [df] of mb 13 [ask item 5 fpdlist [set fpd (fpd - [df] of mb 13) set mon (mon + [df] of mb 13 * pf)]
    ask mb 13 [set mon (mon - df * [pf] of item 5 fpdlist) set fpd (fpd + df) set df 0]]
  if [df] of mb 13 > 0 and [fpd] of item 5 fpdlist < [df] of mb 13 [ask mb 13 [set mon (mon - [fpd] of item 5 fpdlist * [pf] of item 5 fpdlist)
    set fpd (fpd + [fpd] of item 5 fpdlist) set df (df - [fpd] of item 5 fpdlist)]
    ask item 5 fpdlist [set mon (mon + fpd * pf) set fpd 0]]
  if [df] of mb 13 > 0 and [fpd] of item 4 fpdlist > [df] of mb 13 [ask item 4 fpdlist [set fpd (fpd - [df] of mb 13) set mon (mon + [df] of mb 13 * pf)]
    ask mb 13 [set mon (mon - df * [pf] of item 4 fpdlist) set fpd (fpd + df) set df 0]]
  if [df] of mb 13 > 0 and [fpd] of item 4 fpdlist < [df] of mb 13 [ask mb 13 [set mon (mon - [fpd] of item 4 fpdlist * [pf] of item 4 fpdlist)
    set fpd (fpd + [fpd] of item 4 fpdlist) set df (df - [fpd] of item 4 fpdlist)]
    ask item 4 fpdlist [set mon (mon + fpd * pf) set fpd 0]]
  if [df] of mb 13 > 0 and [fpd] of item 3 fpdlist > [df] of mb 13 [ask item 3 fpdlist [set fpd (fpd - [df] of mb 13) set mon (mon + [df] of mb 13 * pf)]
    ask mb 13 [set mon (mon - df * [pf] of item 3  fpdlist) set fpd (fpd + df) set df 0]]
  if [df] of mb 13 > 0 and [fpd] of item 3 fpdlist < [df] of mb 13 [ask mb 13 [set mon (mon - [fpd] of item 3 fpdlist * [pf] of item 3  fpdlist)
    set fpd (fpd + [fpd] of item 3 fpdlist) set df (df - [fpd] of item 3 fpdlist)]
    ask item 3 fpdlist [set mon (mon + fpd * pf) set fpd 0]]
  if [df] of mb 13 > 0 [ask item 2 fpdlist [set fpd (fpd - [df] of mb 13) set mon (mon + [df] of mb 13 * pf)]
    ask mb 13 [set mon (mon - df * [pf] of item 2  fpdlist) set fpd (fpd + df) set df 0]]


  let slplist sort-on [(- ps)] turtles
  if [ds] of mb 11 > 0 and [slp] of item 5 slplist > [ds] of mb 11 [ask item 5 slplist [set slp (slp - [ds] of mb 11) set mon (mon + [ds] of mb 11 * ps)]
    ask mb 11 [set mon (mon - ds * [ps] of item 5 slplist) set slp (slp + ds) set ds 0]]
  if [ds] of mb 11 > 0 and [slp] of item 5 slplist < [ds] of mb 11 [ask mb 11 [set mon (mon - [slp] of item 5 slplist * [ps] of item 5 slplist)
    set slp (slp + [slp] of item 5 slplist) set ds (ds - [slp] of item 5 slplist)]
    ask item 5 slplist [set mon (mon + slp * ps) set slp 0]]
  if [ds] of mb 11 > 0 and [slp] of item 4 slplist > [ds] of mb 11 [ask item 4 slplist [set slp (slp - [ds] of mb 11) set mon (mon + [ds] of mb 11 * ps)]
    ask mb 11 [set mon (mon - ds * [ps] of item 4 slplist) set slp (slp + ds) set ds 0]]
  if [ds] of mb 11 > 0 and [slp] of item 4 slplist < [ds] of mb 11 [ask mb 11 [set mon (mon - [slp] of item 4 slplist * [ps] of item 4 slplist)
    set slp (slp + [slp] of item 4 slplist) set ds (ds - [slp] of item 4 slplist)]
    ask item 4 slplist [set mon (mon + slp * ps) set slp 0]]
  if [ds] of mb 11 > 0 and [slp] of item 3 slplist > [ds] of mb 11 [ask item 3 slplist [set slp (slp - [ds] of mb 11) set mon (mon + [ds] of mb 11 * ps)]
    ask mb 11 [set mon (mon - ds * [ps] of item 3  slplist) set slp (slp + ds) set ds 0]]
  if [ds] of mb 11 > 0 and [slp] of item 3 slplist < [ds] of mb 11 [ask mb 11 [set mon (mon - [slp] of item 3 slplist * [ps] of item 3  slplist)
    set slp (slp + [slp] of item 3 slplist) set ds (ds - [slp] of item 3 slplist)] ask item 3 slplist [set mon (mon + slp * ps) set slp 0]]
  if [ds] of mb 11 > 0 [ask item 2 slplist [set slp (slp - [ds] of mb 11) set mon (mon + [ds] of mb 11 * ps)]
    ask mb 11 [set mon (mon - ds * [ps] of item 2  slplist) set slp (slp + ds) set ds 0]]

  if [ds] of mb 12 > 0 and [slp] of item 5 slplist > [ds] of mb 12 [ask item 5 slplist [set slp (slp - [ds] of mb 12) set mon (mon + [ds] of mb 12 * ps)]
    ask mb 12 [set mon (mon - ds * [ps] of item 5 slplist) set slp (slp + ds) set ds 0]]
  if [ds] of mb 12 > 0 and [slp] of item 5 slplist < [ds] of mb 12 [ask mb 12 [set mon (mon - [slp] of item 5 slplist * [ps] of item 5 slplist)
    set slp (slp + [slp] of item 5 slplist) set ds (ds - [slp] of item 5 slplist)]
    ask item 5 slplist [set mon (mon + slp * ps) set slp 0]]
  if [ds] of mb 12 > 0 and [slp] of item 4 slplist > [ds] of mb 12 [ask item 4 slplist [set slp (slp - [ds] of mb 12) set mon (mon + [ds] of mb 12 * ps)]
    ask mb 12 [set mon (mon - ds * [ps] of item 4 slplist) set slp (slp + ds) set ds 0]]
  if [ds] of mb 12 > 0 and [slp] of item 4 slplist < [ds] of mb 12 [ask mb 12 [set mon (mon - [slp] of item 4 slplist * [ps] of item 4 slplist)
    set slp (slp + [slp] of item 4 slplist) set ds (ds - [slp] of item 4 slplist)]
    ask item 4 slplist [set mon (mon + slp * ps) set slp 0]]
  if [ds] of mb 12 > 0 and [slp] of item 3 slplist > [ds] of mb 12 [ask item 3 slplist [set slp (slp - [ds] of mb 12) set mon (mon + [ds] of mb 12 * ps)]
    ask mb 12 [set mon (mon - ds * [ps] of item 3  slplist) set slp (slp + ds) set ds 0]]
  if [ds] of mb 12 > 0 and [slp] of item 3 slplist < [ds] of mb 12 [ask mb 12 [set mon (mon - [slp] of item 3 slplist * [ps] of item 3  slplist)
    set slp (slp + [slp] of item 3 slplist) set ds (ds - [slp] of item 3 slplist)]
    ask item 3 slplist [set mon (mon + slp * ps) set slp 0]]
  if [ds] of mb 12 > 0 [ask item 2 slplist [set slp (slp - [ds] of mb 12) set mon (mon + [ds] of mb 12 * ps)]
    ask mb 12 [set mon (mon - ds * [ps] of item 2  slplist) set slp (slp + ds) set ds 0]]

  if [ds] of mb 13 > 0 and [slp] of item 5 slplist > [ds] of mb 13 [ask item 5 slplist [set slp (slp - [ds] of mb 13) set mon (mon + [ds] of mb 13 * ps)]
    ask mb 13 [set mon (mon - ds * [ps] of item 5 slplist) set slp (slp + ds) set ds 0]]
  if [ds] of mb 13 > 0 and [slp] of item 5 slplist < [ds] of mb 13 [ask mb 13 [set mon (mon - [slp] of item 5 slplist * [ps] of item 5 slplist)
    set slp (slp + [slp] of item 5 slplist) set ds (ds - [slp] of item 5 slplist)]
    ask item 5 slplist [set mon (mon + slp * ps) set slp 0]]
  if [ds] of mb 13 > 0 and [slp] of item 4 slplist > [ds] of mb 13 [ask item 4 slplist [set slp (slp - [ds] of mb 13) set mon (mon + [ds] of mb 13 * ps)]
    ask mb 13 [set mon (mon - ds * [ps] of item 4 slplist) set slp (slp + ds) set ds 0]]
  if [ds] of mb 13 > 0 and [slp] of item 4 slplist < [ds] of mb 13 [ask mb 13 [set mon (mon - [slp] of item 4 slplist * [ps] of item 4 slplist)
    set slp (slp + [slp] of item 4 slplist) set ds (ds - [slp] of item 4 slplist)]                                                                                   ; all products are bought by buyers based on least seller price
    ask item 4 slplist [set mon (mon + slp * ps) set slp 0]]                                                                                                         ; industrial agents tend to fix lower prices than the market selling agents
  if [ds] of mb 13 > 0 and [slp] of item 3 slplist > [ds] of mb 13 [ask item 3 slplist [set slp (slp - [ds] of mb 13) set mon (mon + [ds] of mb 13 * ps)]
    ask mb 13 [set mon (mon - ds * [ps] of item 3  slplist) set slp (slp + ds) set ds 0]]
  if [ds] of mb 13 > 0 and [slp] of item 3 slplist < [ds] of mb 13 [ask mb 13 [set mon (mon - [slp] of item 3 slplist * [ps] of item 3  slplist)
    set slp (slp + [slp] of item 3 slplist) set ds (ds - [slp] of item 3 slplist)]
    ask item 3 slplist [set mon (mon + slp * ps) set slp 0]]
  if [ds] of mb 13 > 0 [ask item 2 slplist [set slp (slp - [ds] of mb 13) set mon (mon + [ds] of mb 13 * ps)]
    ask mb 13 [set mon (mon - ds * [ps] of item 2  slplist) set slp (slp + ds) set ds 0]]


  let gaslist sort-on [(- pg)] turtles
  if [dg] of mb 11 > 0 and [gas] of item 5 gaslist > [dg] of mb 11 [ask item 5 gaslist [set gas (gas - [dg] of mb 11) set mon (mon + [dg] of mb 11 * pg)]
    ask mb 11 [set mon (mon - dg * [pg] of item 5 gaslist) set gas (gas + dg) set dg 0]]
  if [dg] of mb 11 > 0 and [gas] of item 5 gaslist < [dg] of mb 11 [ask mb 11 [set mon (mon - [gas] of item 5 gaslist * [pg] of item 5 gaslist)
    set gas (gas + [gas] of item 5 gaslist) set dg (dg - [gas] of item 5 gaslist)]
    ask item 5 gaslist [set mon (mon + gas * pg) set gas 0]]
  if [dg] of mb 11 > 0 and [gas] of item 4 gaslist > [dg] of mb 11 [ask item 4 gaslist [set gas (gas - [dg] of mb 11) set mon (mon + [dg] of mb 11 * pg)]
    ask mb 11 [set mon (mon - dg * [pg] of item 4 gaslist) set gas (gas + dg) set dg 0]]
  if [dg] of mb 11 > 0 and [gas] of item 4 gaslist < [dg] of mb 11 [ask mb 11 [set mon (mon - [gas] of item 4 gaslist * [pg] of item 4 gaslist)
    set gas (gas + [gas] of item 4 gaslist) set dg (dg - [gas] of item 4 gaslist)]
    ask item 4 gaslist [set mon (mon + gas * pg) set gas 0]]
  if [dg] of mb 11 > 0 and [gas] of item 3 gaslist > [dg] of mb 11 [ask item 3 gaslist [set gas (gas - [dg] of mb 11) set mon (mon + [dg] of mb 11 * pg)]
    ask mb 11 [set mon (mon - dg * [pg] of item 3  gaslist) set gas (gas + dg) set dg 0]]
  if [dg] of mb 11 > 0 and [gas] of item 3 gaslist < [dg] of mb 11 [ask mb 11 [set mon (mon - [gas] of item 3 gaslist * [pg] of item 3  gaslist)
    set gas (gas + [gas] of item 3 gaslist) set dg (dg - [gas] of item 3 gaslist)]
    ask item 3 gaslist [set mon (mon + gas * pg) set gas 0]]
  if [dg] of mb 11 > 0 [ask item 2 gaslist [set gas (gas - [dg] of mb 11) set mon (mon + [dg] of mb 11 * pg)]
    ask mb 11 [set mon (mon - dg * [pg] of item 2  gaslist) set gas (gas + dg) set dg 0]]

  if [dg] of mb 12 > 0 and [gas] of item 5 gaslist > [dg] of mb 12 [ask item 5 gaslist [set gas (gas - [dg] of mb 12) set mon (mon + [dg] of mb 12 * pg)]
    ask mb 12 [set mon (mon - dg * [pg] of item 5 gaslist) set gas (gas + dg) set dg 0]]
  if [dg] of mb 12 > 0 and [gas] of item 5 gaslist < [dg] of mb 12 [ask mb 12 [set mon (mon - [gas] of item 5 gaslist * [pg] of item 5 gaslist)
    set gas (gas + [gas] of item 5 gaslist) set dg (dg - [gas] of item 5 gaslist)]
    ask item 5 gaslist [set mon (mon + gas * pg) set gas 0]]
  if [dg] of mb 12 > 0 and [gas] of item 4 gaslist > [dg] of mb 12 [ask item 4 gaslist [set gas (gas - [dg] of mb 12) set mon (mon + [dg] of mb 12 * pg)]
    ask mb 12 [set mon (mon - dg * [pg] of item 4 gaslist) set gas (gas + dg) set dg 0]]
  if [dg] of mb 12 > 0 and [gas] of item 4 gaslist < [dg] of mb 12 [ask mb 12 [set mon (mon - [gas] of item 4 gaslist * [pg] of item 4 gaslist)
    set gas (gas + [gas] of item 4 gaslist) set dg (dg - [gas] of item 4 gaslist)]
    ask item 4 gaslist [set mon (mon + gas * pg) set gas 0]]
  if [dg] of mb 12 > 0 and [gas] of item 3 gaslist > [dg] of mb 12 [ask item 3 gaslist [set gas (gas - [dg] of mb 12) set mon (mon + [dg] of mb 12 * pg)]
    ask mb 12 [set mon (mon - dg * [pg] of item 3  gaslist) set gas (gas + dg) set dg 0]]
  if [dg] of mb 12 > 0 and [gas] of item 3 gaslist < [dg] of mb 12 [ask mb 12 [set mon (mon - [gas] of item 3 gaslist * [pg] of item 3  gaslist)
    set gas (gas + [gas] of item 3 gaslist) set dg (dg - [gas] of item 3 gaslist)]
    ask item 3 gaslist [set mon (mon + gas * pg) set gas 0]]
  if [dg] of mb 12 > 0 [ask item 2 gaslist [set gas (gas - [dg] of mb 12) set mon (mon + [dg] of mb 12 * pg)]
    ask mb 12 [set mon (mon - dg * [pg] of item 2  gaslist) set gas (gas + dg) set dg 0]]

  if [dg] of mb 13 > 0 and [gas] of item 5 gaslist > [dg] of mb 13 [ask item 5 gaslist [set gas (gas - [dg] of mb 13) set mon (mon + [dg] of mb 13 * pg)]
    ask mb 13 [set mon (mon - dg * [pg] of item 5 gaslist) set gas (gas + dg) set dg 0]]
  if [dg] of mb 13 > 0 and [gas] of item 5 gaslist < [dg] of mb 13 [ask mb 13 [set mon (mon - [gas] of item 5 gaslist * [pg] of item 5 gaslist)
    set gas (gas + [gas] of item 5 gaslist) set dg (dg - [gas] of item 5 gaslist)]
    ask item 5 gaslist [set mon (mon + gas * pg) set gas 0]]
  if [dg] of mb 13 > 0 and [gas] of item 4 gaslist > [dg] of mb 13 [ask item 4 gaslist [set gas (gas - [dg] of mb 13) set mon (mon + [dg] of mb 13 * pg)]
    ask mb 13 [set mon (mon - dg * [pg] of item 4 gaslist) set gas (gas + dg) set dg 0]]
  if [dg] of mb 13 > 0 and [gas] of item 4 gaslist < [dg] of mb 13 [ask mb 13 [set mon (mon - [gas] of item 4 gaslist * [pg] of item 4 gaslist)
    set gas (gas + [gas] of item 4 gaslist) set dg (dg - [gas] of item 4 gaslist)]
    ask item 4 gaslist [set mon (mon + gas * pg) set gas 0]]
  if [dg] of mb 13 > 0 and [gas] of item 3 gaslist > [dg] of mb 13 [ask item 3 gaslist [set gas (gas - [dg] of mb 13) set mon (mon + [dg] of mb 13 * pg)]
    ask mb 13 [set mon (mon - dg * [pg] of item 3  gaslist) set gas (gas + dg) set dg 0]]
  if [dg] of mb 13 > 0 and [gas] of item 3 gaslist < [dg] of mb 13 [ask mb 13 [set mon (mon - [gas] of item 3 gaslist * [pg] of item 3  gaslist)
    set gas (gas + [gas] of item 3 gaslist) set dg (dg - [gas] of item 3 gaslist)]
    ask item 3 gaslist [set mon (mon + gas * pg) set gas 0]]
  if [dg] of mb 13 > 0 [ask item 2 gaslist [set gas (gas - [dg] of mb 13) set mon (mon + [dg] of mb 13 * pg)]
    ask mb 13 [set mon (mon - dg * [pg] of item 2  gaslist) set gas (gas + dg) set dg 0]]


  let heatlist sort-on [(- ph)] turtles
  if [dh] of mb 11 > 0 and [heat] of item 4 heatlist > [dh] of mb 11 [ask item 4 heatlist [set heat (heat - [dh] of mb 11) set mon (mon + [dh] of mb 11 * ph)]
    ask mb 11 [set mon (mon - dh * [ph] of item 4 heatlist) set heat (heat + dh) set dh 0]]
  if [dh] of mb 11 > 0 and [heat] of item 4 heatlist < [dh] of mb 11 [ask mb 11 [set mon (mon - [heat] of item 4 heatlist * [ph] of item 4 heatlist)
    set heat (heat + [heat] of item 4 heatlist) set dh (dh - [heat] of item 4 heatlist)]
    ask item 4 heatlist [set mon (mon + heat * ph) set heat 0]]
  if [dh] of mb 11 > 0 and [heat] of item 3 heatlist > [dh] of mb 11 [ask item 3 heatlist [set heat (heat - [dh] of mb 11) set mon (mon + [dh] of mb 11 * ph)]
    ask mb 11 [set mon (mon - dh * [ph] of item 3 heatlist) set heat (heat + dh) set dh 0]]
  if [dh] of mb 11 > 0 and [heat] of item 3 heatlist < [dh] of mb 11 [ask mb 11 [set mon (mon - [heat] of item 3 heatlist * [ph] of item 3 heatlist)
    set heat (heat + [heat] of item 3 heatlist) set dh (dh - [heat] of item 3 heatlist)]
    ask item 3 heatlist [set mon (mon + heat * ph) set heat 0]]
  if [dh] of mb 11 > 0 [ask item 2 heatlist [set heat (heat - [dh] of mb 11) set mon (mon + [dh] of mb 11 * ph)]
    ask mb 11 [set mon (mon - dh * [ph] of item 2  heatlist) set heat (heat + dh) set dh 0]]

  if [dh] of mb 12 > 0 and [heat] of item 4 heatlist > [dh] of mb 12 [ask item 4 heatlist [set heat (heat - [dh] of mb 12) set mon (mon + [dh] of mb 12 * ph)]
    ask mb 12 [set mon (mon - dh * [ph] of item 4 heatlist) set heat (heat + dh) set dh 0]]
  if [dh] of mb 12 > 0 and [heat] of item 4 heatlist < [dh] of mb 12 [ask mb 12 [set mon (mon - [heat] of item 4 heatlist * [ph] of item 4 heatlist)
    set heat (heat + [heat] of item 4 heatlist) set dh (dh - [heat] of item 4 heatlist)]
    ask item 4 heatlist [set mon (mon + heat * ph) set heat 0]]
  if [dh] of mb 12 > 0 and [heat] of item 3 heatlist > [dh] of mb 12 [ask item 3 heatlist [set heat (heat - [dh] of mb 12) set mon (mon + [dh] of mb 12 * ph)]
    ask mb 12 [set mon (mon - dh * [ph] of item 3 heatlist) set heat (heat + dh) set dh 0]]
  if [dh] of mb 12 > 0 and [heat] of item 3 heatlist < [dh] of mb 12 [ask mb 12 [set mon (mon - [heat] of item 3 heatlist * [ph] of item 3 heatlist)
    set heat (heat + [heat] of item 3 heatlist) set dh (dh - [heat] of item 3 heatlist)] ask item 3 heatlist [set mon (mon + heat * ph) set heat 0]]
  if [dh] of mb 12 > 0 [ask item 2 heatlist [set heat (heat - [dh] of mb 12) set mon (mon + [dh] of mb 12 * ph)]
    ask mb 12 [set mon (mon - dh * [ph] of item 2  heatlist) set heat (heat + dh) set dh 0]]

  if [dh] of mb 13 > 0 and [heat] of item 4 heatlist > [dh] of mb 13 [ask item 4 heatlist [set heat (heat - [dh] of mb 13) set mon (mon + [dh] of mb 13 * ph)]
    ask mb 13 [set mon (mon - dh * [ph] of item 4 heatlist) set heat (heat + dh) set dh 0]]
  if [dh] of mb 13 > 0 and [heat] of item 4 heatlist < [dh] of mb 13 [ask mb 13 [set mon (mon - [heat] of item 4 heatlist * [ph] of item 4 heatlist)
    set heat (heat + [heat] of item 4 heatlist) set dh (dh - [heat] of item 4 heatlist)]
    ask item 4 heatlist [set mon (mon + heat * ph) set heat 0]]
  if [dh] of mb 13 > 0 and [heat] of item 3 heatlist > [dh] of mb 13 [ask item 3 heatlist [set heat (heat - [dh] of mb 13) set mon (mon + [dh] of mb 13 * ph)]
    ask mb 13 [set mon (mon - dh * [ph] of item 3 heatlist) set heat (heat + dh) set dh 0]]
  if [dh] of mb 13 > 0 and [heat] of item 3 heatlist < [dh] of mb 13 [ask mb 13 [set mon (mon - [heat] of item 3 heatlist * [ph] of item 3 heatlist)
    set heat (heat + [heat] of item 3 heatlist) set dh (dh - [heat] of item 3 heatlist)]
    ask item 3 heatlist [set mon (mon + heat * ph) set heat 0]]
  if [dh] of mb 13 > 0 [ask item 2 heatlist [set heat (heat - [dh] of mb 13) set mon (mon + [dh] of mb 13 * ph)]
    ask mb 13 [set mon (mon - dh * [ph] of item 2  heatlist) set heat (heat + dh) set dh 0]]
end

to pay-fines
  ask rfs [(if slp > 0 [set mon (mon - (slp * cost-per-unit-sulp-disch * ps)) set slp 0]) (if heat > 0 [
    set mon (mon - (heat * cost-per-unit-heat * (([ph] of pp 6 + [ph] of pp 7) / 2))) set heat 0]) (if gas > 0 [set mon (mon - (gas * cost-per-unit-gas-disch * pg)) set gas 0])]
  ask pps [(if heat > 0 [set mon (mon - heat * cost-per-unit-heat * (([ph] of pp 6 + [ph] of pp 7) / 2)) set heat 0])]                                                                  ; industrial agents pay fixed fines for wastes released into the environment
  ask fps [if eff > 0 [set mon (mon - eff * cost-per-unit-eff ) set eff 0]]
end


to display-labels
  ask turtles [set label "" set label-color white]                                      ; setting for available cash amount displayed to the nearest whole number
  ask turtles [set label round mon]
end


to setup-breeds
  create-mss 3 [
    set shape "person"
    set color black
    set size 5
    set mon 1e12
    set elc 1e9
    set crd 1e18
    set npk 1e18
    set gas 1e18
    set heat 1e18
    set gas 1e18
    set wtr 1e18
    set fpd 1e18
    set rpd 1e18
  ]
  ask turtle 0 [setxy -20 20]
  ask turtle 1 [setxy -20 0]
  ask turtle 2 [setxy -20 -20]

  create-rfs 3 [
    set shape "factory"
    set color blue
    set size 7
    set mon 0
    set elc 0
    set crd 0
    set gas 0
    set heat 0
    set slp 0
    set rpd 0
  ]
  ask turtle 3 [setxy -5 20]
  ask turtle 4 [setxy -5 0]
  ask turtle 5 [setxy -5 -20]

  create-pps 2 [
    set shape "factory"
    set color green
    set size 7
    set mon 0
    set elc 0
    set gas 0
    set heat 0
    set wtr 0
  ]
  ask turtle 6 [setxy 5 10]
  ask turtle 7 [setxy 5 -10]

  create-fps 3 [
    set shape "factory"
    set color red
    set size 7
    set mon 0
    set elc 0
    set npk 0
    set slp 0
    set fpd 0
  ]
  ask turtle 8 [setxy 15 20]
  ask turtle 9 [setxy 15 0]
  ask turtle 10 [setxy 15 -20]

  create-mbs 3 [
    set shape "person"
    set color orange
    set size 5
    set mon 1e18
    set elc 0
    set crd 0
    set npk 0
    set gas 0
    set heat 0
    set slp 0
    set wtr 0
    set fpd 0
    set rpd 0
  ]
  ask turtle 11 [setxy 30 20]
  ask turtle 12 [setxy 30 0]
  ask turtle 13 [setxy 30 -20]
end
@#$#@#$#@
GRAPHICS-WINDOW
357
10
1288
812
-1
-1
13.0
1
10
1
1
1
0
1
1
1
-30
40
-30
30
1
1
1
ticks
30.0

BUTTON
86
290
149
323
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
163
291
226
324
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
0

SLIDER
10
6
182
39
inflationary-factor
inflationary-factor
1
3
1.0
0.01
1
%
HORIZONTAL

SLIDER
9
38
181
71
reliability-ref-1
reliability-ref-1
0
100
95.0
1
1
%
HORIZONTAL

SLIDER
9
71
181
104
reliability-ref-2
reliability-ref-2
0
100
94.0
1
1
%
HORIZONTAL

SLIDER
9
104
181
137
reliability-ref-3
reliability-ref-3
0
100
91.0
1
1
%
HORIZONTAL

SLIDER
181
10
353
43
reliability-pp-1
reliability-pp-1
0
100
82.0
1
1
%
HORIZONTAL

SLIDER
181
42
353
75
reliability-pp-2
reliability-pp-2
0
100
91.0
1
1
%
HORIZONTAL

SLIDER
180
75
352
108
reliability-fplants
reliability-fplants
0
100
94.0
1
1
%
HORIZONTAL

SLIDER
181
107
353
140
cost-per-unit-eff
cost-per-unit-eff
0
5
0.1
0.01
1
NIL
HORIZONTAL

SLIDER
9
137
181
170
cost-per-unit-heat
cost-per-unit-heat
0
5
0.1
0.01
1
NIL
HORIZONTAL

SLIDER
9
203
187
236
cost-per-unit-gas-disch
cost-per-unit-gas-disch
0
5
0.5
0.01
1
NIL
HORIZONTAL

SLIDER
8
170
188
203
cost-per-unit-sulp-disch
cost-per-unit-sulp-disch
0
5
0.5
0.01
1
NIL
HORIZONTAL

CHOOSER
10
240
148
285
model-version
model-version
"contracts" "decision-based"
1

SLIDER
172
243
350
276
number-of-years
number-of-years
0
20
3.0
1
1
years
HORIZONTAL

BUTTON
242
294
319
327
go-once
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

@#$#@#$#@
## WHAT IS IT?

This model was designed to simulate, analyse and evaluate the activities within an eco-industrial park to generate insights on how profit distribution takes place in the presence of contractual agreements between industrial agents.

The model also studies the resulting effects of varying inflationary factor and the reliabilities of various industrial agents within the eco-industrial park.

## HOW IT WORKS

The model is designed to operate in two modes: contract-based and decision-based model version. In contract-based model version, the EIP agents purchase input requiremnts available within the EIP based on contractual agreements between them. In decision-based model version, the EIP agents purchase required inputs available within the EIP based on the best price (lowest price). All input requirments available within the EIP are first exhasuted before patronising the market selling agents. Input requirments not available wihtin the EIP are purchased directly from the market selling agents.

The industrial agents produce based on their reliability factors and sell to the market buyers in competition with the market selling agents based on the lowest product price. Market selling agents set product prices based on a gaussian-normal distribution with a mean value and variance. The industrial agents set prodcut prices based on a gausssian-normal distribution with a mean value (based on average market price) and a variance (tending to set prices lower than the market selling agents set prices).

Experiments hves been designed on the behaviour space to run simulations with different combinations of model versions and industrial agents' reliability factors. Results can be saved as tables on an excel file where data analysis can be done and insights drawn using charts.

## HOW TO USE IT

Once the model file is opened in NETLOGO, these steps can be followed:
1. On the interface, click SETUP to initialise the EIP 
2. Click "go" to make the simulation keep running until the set number of years. Clicking again will temporarily stop the model simulation.
3. Click "go-once" to watch the simulation run for just one time step
4. Model versions can be varied using the model-version chooser button
5. Contract length can be varied using the "number-of -years" slider button
6. Reliabilities of the industrial agents can be varied up to 100% using the sliders
7. To run an experiment, open BehaviorSpace and run or modify the already designed experiments.
8. Extract data to desired analysis tool for evaluation and drawing inferences

## THINGS TO NOTICE

Each industrial agents start out making losses but in time breaks even and generates profit. This is typical in the business environment. The market selling agents are considered to be infinite sources of resources which explains their very high stock and cash values. The buyers only purchase what they require at the lowest price from the EIP participants and the market selling agents. The EIP operates on a pull production system.

## THINGS TO TRY

Study the effects of varying the inflationary factor on the EIP revenue generation and profit distribution.

## EXTENDING THE MODEL

The model can be made more complex by adding a seller decision model where sellers only transact with buyers who are eligible (meet certain criteria, say a high "green score", responsible employment, climate awareness, CSRs, etc as obtainable in some industrial sectors) 

## NETLOGO FEATURES

Reliability was modeled by combining random numbers and inequalities
Purchasing items from agents with the least price could be made more efficient with shorter codes if possible


## CREDITS AND REFERENCES

https://github.com/ClemUg/TonbarahABM-EIP.git
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

factory
false
0
Rectangle -7500403 true true 76 194 285 270
Rectangle -7500403 true true 36 95 59 231
Rectangle -16777216 true false 90 210 270 240
Line -7500403 true 90 195 90 255
Line -7500403 true 120 195 120 255
Line -7500403 true 150 195 150 240
Line -7500403 true 180 195 180 255
Line -7500403 true 210 210 210 240
Line -7500403 true 240 210 240 240
Line -7500403 true 90 225 270 225
Circle -1 true false 37 73 32
Circle -1 true false 55 38 54
Circle -1 true false 96 21 42
Circle -1 true false 105 40 32
Circle -1 true false 129 19 42
Rectangle -7500403 true true 14 228 78 270

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
NetLogo 6.2.0
@#$#@#$#@
@#$#@#$#@
1.0
    org.nlogo.sdm.gui.AggregateDrawing 5
        org.nlogo.sdm.gui.StockFigure "attributes" "attributes" 1 "FillColor" "Color" 225 225 182 148 58 60 40
            org.nlogo.sdm.gui.WrappedStock "" "" 0
        org.nlogo.sdm.gui.ConverterFigure "attributes" "attributes" 1 "FillColor" "Color" 130 188 183 268 47 50 50
            org.nlogo.sdm.gui.WrappedConverter "" ""
        org.nlogo.sdm.gui.BindingConnection 2 220 75 269 73 NULL NULL 0 0 0
            org.jhotdraw.standard.ChopBoxConnector REF 1
            org.jhotdraw.contrib.ChopDiamondConnector REF 3
        org.nlogo.sdm.gui.RateConnection 3 192 110 229 194 267 278 NULL NULL 0 0 0
            org.jhotdraw.standard.ChopBoxConnector REF 1
            org.jhotdraw.figures.ChopEllipseConnector
                org.nlogo.sdm.gui.ReservoirFigure "attributes" "attributes" 1 "FillColor" "Color" 192 192 192 257 276 30 30
            org.nlogo.sdm.gui.WrappedRate "" "" REF 2
                org.nlogo.sdm.gui.WrappedReservoir  0   REF 11
@#$#@#$#@
<experiments>
  <experiment name="Total profit experiment" repetitions="40" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>(sum [mon] of turtles - sum [mon] of mss - sum [mon] of mbs)</metric>
    <enumeratedValueSet variable="reliability-ref-2">
      <value value="94"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cost-per-unit-eff">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cost-per-unit-gas-disch">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cost-per-unit-heat">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reliability-ref-3">
      <value value="91"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inflationary-factor">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cost-per-unit-sulp-disch">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reliability-pp-1">
      <value value="82"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reliability-pp-2">
      <value value="91"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reliability-ref-1">
      <value value="95"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reliability-fplants">
      <value value="94"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="model-version">
      <value value="&quot;contracts&quot;"/>
      <value value="&quot;decision-based&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Profit distribution experiment" repetitions="80" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>[mon] of rf 3</metric>
    <metric>[mon] of rf 4</metric>
    <metric>[mon] of rf 5</metric>
    <metric>[mon] of pp 6</metric>
    <metric>[mon] of pp 7</metric>
    <metric>[mon] of fp 8</metric>
    <metric>[mon] of fp 9</metric>
    <metric>[mon] of fp 10</metric>
    <enumeratedValueSet variable="model-version">
      <value value="&quot;contracts&quot;"/>
      <value value="&quot;decision-based&quot;"/>
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
