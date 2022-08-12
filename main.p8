pico-8 cartridge // http://www.pico-8.com
version 36
__lua__
-- main

function _init()
-- global tables for game
-- can be modified by game
-- state ini functions
  g = {} -- games state
  actors = {}
  chars = {}
  ens = {}
  winds = {}
  floats={}
  debugs = {}
  path = {}

-- useful parameters 
  T = 0
  timer = 0
  dt = 0
  buttbuff=-1
  debug = true
  -- debug = false
  fps = 30

-- making roster
  pc = make_pc()
  m = make_map()
  -- replace enemy tiles with 
  -- actual enemies
  for i=0,15 do
    for j=0,15 do
      if mget(i,j)==30 then
        make_en(34,i*8,j*8)
        mset(i,j,64)
      end
    end
  end

  chars = {pc}

  -- mmenu_ini()
  -- exp_ini()
  -- init_ini()
  cmbt_ini()
end

function _update()
  T+=1
  -- m:up()
  foreach(actors, move_actor)
  foreach(winds, update_wind)
  foreach(floats, move_float)
  g.upd()
end

function _draw()
  cls()
  m:dr()
  foreach(actors, draw_actor)
  foreach(winds, draw_wind)
  g.drw()
  for f in all(floats) do
    oprint(f.txt,f.x,f.y,f.c,0)
  end
  for i,p in ipairs(path) do
    local x,y=8*p[1],8*p[2]
    rect(x,y,x+8,y+8,8)
    ?i-1,x+2,y+2,12
  end


  cursor(4,4)
  color(7)
  ?"turn order:"..pturn.."/"..#chars
  print_char(chars[pturn])
  for d in all(debugs) do
    ?d
  end
  debugs = {}
  -- end
end

-- make an actor with sprite _sp
-- add it to global collection
-- x,y are pixel coordinates of
-- actor
function make_actor(_sp,_x,_y,_b)
  a = {}
  a.sp=_sp or nil
  a.spo=0 -- sprite offset
  a.x=_x or 0
  a.y=_y or 1
  a.tx,a.ty=_x,_y
  a.dx,a.dy=0,0
  a.ox,a.oy=0,0
  a.t,a.tdur=0,0
  a.easef=linear
  a.frame,a.frames=0,1
  a.anim,a.cycles=0,2
  a.flip = false
  a.ini=0
  a.ct=0
  a.i=function(s) return s.x\8 end
  a.j=function(s) return s.y\8 end
  add(actors,a)
  return a
end

-- draw actor to screen. if a 
-- transparent color _a.ct is set that
-- is not black then will change
-- that color
-- TODO: come remove the local variables
-- here I don't think its saving me
-- any tokens
function draw_actor(_a)
  local sp,spo,f = _a.sp,_a.spo,_a.frame
  local x,y    = _a.x,_a.y
  if _a.ct>0 then
    palt(_a.ct,true)
    palt(0,false)
  end
  for p in all(_a.pals) do
    pal(p)
  end
  spr(sp+spo+f,x,y,1,1,_a.flip)
  pal()
end

-- sets an actors "tween" to go 
-- from current location to 
-- (_x,_y) in duration _d seconds
-- _type sets if it is a full
-- motion or a bump that returns
-- to its start position
function set_actor_tween(_a,_x,_y,_d,_type)
  if (_a.anim!=0) return

  _a.tx,_a.ty = _x,_y
  _a.tdur=flr(_d*fps)
  _a.dx,_a.dy = (_a.tx-_a.x),(_a.ty-_a.y)
  _a.ox,_a.oy = _a.x,_a.y
  _a.anim = _type
end

-- fulfills the direction made
-- by set_actor_tween. does the
-- actual updating. can use 
-- different easing functions by
-- setting that in the actor 
-- directly
function move_actor(_a)
  if (flr(_a.tdur) <= 0) return

  local tme = _a.t
  -- bump animation
  if _a.anim==2 and tme>_a.tdur\2 then
    tme = _a.tdur - tme
  end

  _a.x = _a.easef(tme,_a.ox,_a.dx,_a.tdur)
  _a.y = _a.easef(tme,_a.oy,_a.dy,_a.tdur)
  _a.frame=(tme*_a.cycles\_a.tdur)%_a.frames

  _a.t+=1
  
  if(_a.t > _a.tdur) then
    _a.dx,_a.dy  = 0,0
    _a.ox,_a.oy  = 0,0
    _a.tx,_a.ty  = 0,0
    _a.t,_a.tdur = 0,0
    _a.frame    = 0
    _a.anim = 0
  end
end

-- makes a floating text object
-- at location (x,y) with color 
-- c that lasts for dur seconds
function make_float(_txt,_x,_y,_c,_dur)
  _dur = _dur or 1 -- seconds
  add(floats,{txt=_txt,x=_x,y=_y,ty=_y-10,t=0,tdur=_dur*fps,c=_c})
end

-- animates the float up using a
-- simple exponential lerp
-- will delete float when no
-- longer needed
function move_float(f)
  f.t+=1
  if f.t > f.tdur then
    del(floats,f)
  end
  f.y+=(f.ty-f.y)/10
end

function make_pc()
  a = make_actor(34,7*8,7*8)
  a.name="the knight"
  a.ct=10
  a.frames=2
  a.mvmt=6
  a.ini=23
  a.hpmax=40
  a.hp=a.hpmax
  a.tail={}
  a.bktrk=0
  a.type='pc'
  a.atksel = 0 -- zero index for attack type
  a.atks = {
    {sp=205, name="great sword", dmg="2d6+3", rng=1},
    {sp=221, name="longbow", dmg="1d8+2", rng=30}
  }
  a.atk= a.atks[1]
  a.opts={204, a.atk.sp, 206, 207} -- combat options
  a.num_attacks = 2 -- number of attacks pc can make
  a.pals = {{[14]=5}}
  return a
end

function make_en(_sp,_i,_j)
  local a = make_actor(_sp,_i,_j)
  a.name='enemy'
  a.ct=10
  a.frames=2
  a.mvmt=4
  a.ini=16
  a.hpmax=20
  a.hp=a.hpmax
  a.tail={}
  a.bktrk=0
  a.type='en'
  a.atksel = 0 -- zero index for attack type
  a.atks = {
    {sp=205, name="great sword", dmg="2d6+3", rng=1}
  }
  a.atk= a.atks[1]
  a.opts={204, a.atk.sp, 206, 207} -- combat options
  a.num_attacks = 3 -- number of attacks to be made
  a.pals = {{[6]=5},{[7]=1},{[5]=4},{[1]=0},{[13]=2},{[14]=6}}
  return a
end

function make_map()
  local m = {}
  m.i = 0
  m.j = 0
  m.ox = 0
  m.oy = 0
  m.w = 64
  m.h = 32
  m.up = 
    function(s)
      local p = actors[1]
      local newi = (p.i\16)*16
      local newj = (p.j\16)*16

      -- Scroll map instead of jumping
      if newi - m.i > 0 then
        m.ox = 128
      elseif newi - m.i < 0 then
        m.ox = -128
      end
      if newj - m.j > 0 then
        m.oy = 128
      elseif newj - m.j < 0 then
        m.oy = -128
      end
      if m.ox > 0 then
        m.ox-=8
      elseif m.ox < 0 then
        m.ox+=8
      end
      if m.oy > 0 then
        m.oy-=8
      elseif m.oy < 0 then
        m.oy+=8
      end
      -- Update values
      m.i = newi
      m.j = newj
    end
  m.dr = 
    function(s)
      map(0,0,0,0,s.w,s.h)
      camera(m.i*8-m.ox,m.j*8-m.oy)
    end

  return m
end

-- windows
-- makes window object held in winds
-- is a child objet of actor so
-- can use actor based functions
function make_wind(x,y,w,h,ic,bc)
  local _w = make_actor(nil,x,y)
  del(actors,_w)-- hacky way to keep window from showin up in actors list
  -- anim params
  _w.x,_w.y,_w.tx,_w.ty=x,y,x,y
  _w.ox,_w.oy,_w.dx,_w.dy=0,0,0,0
  _w.cx,_w.cy=x,y
  _w.t,_w.tdur = 0,0
  -- drawing params
  _w.w,_w.h=w,h
  _w.ic,_w.bc=ic,bc
  
  add(winds,_w)
  return _w
end

-- updates window _w by moving
-- it and then checking if the
-- string should be reverted to
-- an older string
function update_wind(_w)
  move_actor(_w)

  if _w.strdur!=nil then
    _w.strdur-=1
    if _w.strdur<=0 then
      _w.str,_w.oldstr=_w.oldstr,""
      _w.strdur=nil
    end
  end
end

-- draws a window _w based on 
-- its parameters
function draw_wind(_w)
  local x,y,sps = _w.x,_w.y,_w.sps
  text_box(_w.str,x,y,_w.w,_w.h,_w.ic,_w.bc)
  if sps and #sps > 0 then
    for i,s in ipairs(sps) do
      spr(s,x+4+(i-1)*16,y+4)
    end
  end
end

-- sets the string of window _w
-- to _s. optional duration _d
-- will set a new string _s, but
-- revert to old string after _d
-- seconds
function set_wind_msg(_w,_s,_d)
  if _d then
    _w.str,_w.strold = _s,_w.str
    _w.strdur = flr(_d*fps)
  end

  if _w.strdur==nil then
    _w.str = _s
  end
end
-->8
-- main menu

function mmenu_ini()
 g.upd = mmenu_upd
 g.drw = mmenu_drw
end

function mmenu_upd()
 if btn(🅾️) or btn(❎) then
  ctscn_ini()
 end
end

function mmenu_drw()
 cls()
 print("press 🅾️/❎ to start")
end
-->8
-- cutscene/dialogue
ctscn = {start=0}

function ctscn_ini()
 ctscn.start = time()
 g.upd = ctscn_upd
 g.drw = ctscn_drw
end

function ctscn_upd()
 if time()-ctscn.start < 1 then
  return
 end
 if time()-ctscn.start > 8 or
    btn(❎) or btn(🅾️) then
  cmbt_ini()
 end
end

function ctscn_drw()
 cls()
 local t = time() - ctscn.start
 local s = "hello"
 if t > 1 then
  s = s..sub("...",1,t-1)
 end
 if t > 5 then
  s = s.." this is the end of the\nscene"
 end
 print(s)
end
-->8
-- general exploration
function exp_ini()
  g.upd = exp_upd
  g.drw = exp_drw
end

function exp_upd()
  if (btnp()) pc:mv(getlr(),getud())

  foreach(actors, function(s) s:up() end)

  -- Dummy way to enter combat
  if pc.i==1 and pc.j==1 and
     pc.ox==0 and pc.oy==0 then
    init_ini()
  end
end

function exp_drw()
  foreach(actors, function(s) s:dr() end)
  -- Portal to enter combat
  rect(7,8,15,16,9)
end

-->8
-- combat
function cmbt_ini()
  g.upd = cmbt_upd
  g.drw = cmbt_drw

-- initiative order
  order_chars()
  pturn = 1
  pcurr = chars[pturn]
-- combat menu
  local x,y,w,h=64,112,64,16
  winds={} -- clear winds for combat
  -- text window
  wtext=make_wind(0,y,w,h,6,5)
  -- action bins
  -- TODO: make_bin() func
  bins={}
  wmvtxt=make_wind(x+2,y,h-4,h-6,7,0)
  wmvtxt.open = false
  bins['wmvtxt']=wmvtxt
  -- action menu bgnd
  wbgnd=make_wind(x,y,w,h,7,0)
  wbgnd.sps = pcurr.opts
  -- action menu selection wind
  wsel=make_wind(x+2,y+2,h-4,h-4,-1,9)
  wsel.s=0
  
-- enemy selection
  ens={}
  enp=nil

end

function cmbt_upd()
  set_wind_msg(wtext,"what will "..pcurr.name.." do?")

  -- get button input
  if(buttbuff<0) buttbuff=getbutt()

  -- no button input
  if (buttbuff<0) return

-- menu selection
  -- scroll l/r through combat opts
  if buttbuff==0 or buttbuff==1 then
    wsel.s=(wsel.s+dirx[buttbuff+1])%#pcurr.opts
    wsel.x = wsel.cx+wsel.s*16
  end

  --scroll u/d through attack opts
  if (buttbuff==2 or buttbuff==3) 
     and wsel.s==1 then
      pcurr.atksel=(pcurr.atksel+diry[buttbuff+1])%#pcurr.atks
      pcurr.atk = pcurr.atks[pcurr.atksel+1]
      pcurr.opts[2] = pcurr.atk.sp
      set_wind_msg(wtext,pcurr.atk.name..": "..pcurr.atk.dmg,2)
  end

  -- enter selected modes
  if buttbuff==5 then
    -- movement
    if wsel.s==0 then
      bsel = bins['wmvtxt']
      binoc(bsel)
      g.upd = cmbt_move_upd
    -- attacks
    elseif wsel.s==1 then
      if pcurr.num_attacks>0 then
        ens = find_actors_in_range(pcurr,pcurr.atk.rng,'en')
        if #ens>0 then
          g.upd = cmbt_atk_upd
          enp = 0
        else
          set_wind_msg(wtext,'no enemies within range!',1.5)
          enp = nil
        end
      else
        set_wind_msg(wtext,"no more attacks!",1.5)
      end
    -- inventory
    elseif wsel.s==2 then

    -- end turn
    elseif wsel.s==3 then
      end_turn(pcurr)
    end
  end

  -- TODO: move to own cmbt_atk_upd function
  -- scroll through attack opts
  -- if selwin.s==1 then
  --   -- 2 => +1 shift
  --   -- 3 => -1 shift
  --   if (buttbuff==2 or buttbuff==3) pcurr:rot_att(-2*(buttbuff%2)+1)
  -- end

  buttbuff=-1
end

function cmbt_drw()
  -- draw current movement path
  pc_path(pcurr)
  -- redraw current player so 
  -- they appear on top of 
  -- movement path
  draw_actor(pcurr)

  if g.upd==cmbt_upd then
    bouncebutt(5,wsel.x-4,wsel.y-6,7,0)
  end

  -- marching ants for attack select
  -- only care about if we're in
  -- attack mode
  if g.upd==cmbt_atk_upd then
    for i,e in ipairs(ens) do
        ants_box(e:i()*8-1,e:j()*8-1,9,9,7,i==(enp+1))
    end
  end
end

-- loops while we are running through
-- combat motion
function cmbt_move_upd()
  local p = pcurr

  set_wind_msg(wtext,pcurr.name.." is on the move!")
  set_wind_msg(wmvtxt,5*(pcurr.mvmt-#pcurr.tail-pcurr.bktrk))

  if (buttbuff<0) buttbuff=getbutt()
  if (buttbuff<0) return

  -- listen for arrow keys to move
  if buttbuff>=0 and buttbuff<4 then
    local di,dj=dirx[buttbuff+1],diry[buttbuff+1]
    actor_dir(p,di,dj)
    if can_move(p,di,dj) and
        pcurr.anim==0 then
      if g.drw == cmbt_drw then
        track_tail(p,di,dj)
      end
      -- walk
      set_actor_tween(p,p.x+8*di,p.y+8*dj,0.125,1)
    else
      -- bump
      set_actor_tween(p,p.x+8*di,p.y+8*dj,0.125,2)
    end
  end

  -- done with motion
  if (buttbuff==5) then
    binoc(bsel)
    bsel = nil
    g.upd=cmbt_upd
  end

  buttbuff=-1
end

-- loops while we are handling combat attack
function cmbt_atk_upd()
  local p = pcurr

  set_wind_msg(wtext,"❎confirm 🅾️cancel")

  if (buttbuff<0) buttbuff=getbutt()
  if (buttbuff<0) return

  -- cycle through enemies
  -- within range
  if buttbuff==2 or buttbuff==3 then
    enp+=diry[buttbuff+1]
    enp=enp%#ens
  end

  if buttbuff==4 then
    -- cancel
    g.upd = cmbt_upd
  elseif buttbuff==5 then
    -- complete attack
    attack(p,ens[enp+1])
    g.upd=cmbt_upd
  end

  buttbuff=-1
end

function end_turn(p)
  p.tail = {}
  p.bktrk = 0
  p.num_attacks=2
  pturn=pturn%#chars + 1
  pcurr = chars[pturn]
  if pcurr.type == 'en' then
    path = a_star(pcurr:i(),pcurr:j(),pc:i(),pc:j())
    set_wind_msg(wtext,"ai turn!")
    g.upd = ai_turn_upd
  end
end

function ai_turn_upd()
  local bot = pcurr

  if T%15 == 0 then -- move every half second
    if #path>0 then 
        local step = deli(path)
        set_actor_tween(bot,8*step[1],8*step[2],0.25,1)
        actor_dir(bot,step[1]-bot:i(),step[2]-bot:j())
    else
      local pcs = find_actors_in_range(bot,1,'pc')
      if #pcs>0 and bot.num_attacks>0 then
        attack(bot,pcs[1])
      end
    end
  end
end

-- performs attack from actor atk
-- to actor def 
function attack(atk,def)
  -- set animation stuff
  local dx,dy=(atk.x-def.x),(atk.y-def.y)
  set_actor_tween(atk,atk.x-dx,atk.y-dy,0.2,2)
  actor_dir(atk,-dx,-dy)

  -- prevent backtracking after attack
  atk.bktrk+=#atk.tail
  atk.tail={}

  -- roll for damage
  local dmg = roll_dmg(atk.atk.dmg)
  make_float("-"..dmg,def.x,def.y,8)
  def.hp-=dmg
  -- remove defending actor from list
  -- if defeated
  -- TODO: probably move this part
  -- outside of attack and into an
  -- update function
  if def.hp<=0 then
    del(actors,def)
  end
  atk.num_attacks-=1
end

-- opens/closes a bin
-- bin is a type of window that we
-- animate
function binoc(b)
  if b.open then
    set_actor_tween(b,b.cx,b.cy,0.2,1)
  else
    set_actor_tween(b,b.cx,b.cy-b.h+2,0.2,1)
  end
  b.open = not b.open
end

-- big funciton to check if the
-- tile the actor p is open
-- check for:
-- 1. not a wall
-- 2. within map bounds
-- 3. if there is movement left
-- 3a. edge case when 0 movement,
--     but we are backtacking
-- 4. if other actors are in the
--    way
-- TODO: this can probably be 
-- refactored
function can_move(p,di,dj)
  local newi = p:i()+di
  local newj = p:j()+dj
  -- check for wall and inbounds
  local test = is_walkable(newi,newj)
  -- Do we have movement left?
  -- Only relevant for combat
  test = test and (p.mvmt - #p.tail - p.bktrk) > 0
  -- Edge case, need to check if
  -- backtracking our path
  if not test then
    for q in all(p.tail) do
      test = q[1] == newi
      test = test and (q[2] == newj)
      if (test) break
    end
  end

  -- check for other actors in the way
  test = test and (get_actor(newi,newj) == nil)
  
  return test
end

function is_walkable(i,j)
  local WALL = 0
  return not is_tile(WALL,i,j) and in_bounds(i,j)
end

function in_bounds(i,j)
  local ret = i>=0 and i<=m.w
  return ret and j>=0 and j<=m.h
end

-- tracks/builds list of tail that
-- pc has already traveled. gets
-- zeroed out after an attack
-- rebuilds tail every call which
-- lets us account for backtracking
function track_tail(p,di,dj)
  local newi,newj=p:i()+di,p:j()+dj
  local nw_tail = {}
  for q in all(p.tail) do
    if newi==q[1] and
       newj==q[2] then
      break
    end
    add(nw_tail,q)
  end

  if #nw_tail >= #p.tail then
    add(nw_tail, {p:i(), p:j()})
  end
  p.tail = nw_tail
end

-- draws tail for current movement
function pc_path(pc)  
 if #pc.tail == 0 then
  return
 end
 for n=1,#pc.tail do
  local x0 = pc.tail[n][1]*8
  local y0 = pc.tail[n][2]*8
  rect(x0,y0,x0+7,y0+7,8)
 end
end

-- rolling initiative
function init_ini()
  g.drw = init_drw
  g.upd = init_upd
  roll = false
  rtime = 1
  rval = 20
  p = 0
  dflash=0
  foreach(actors, function(s) s:reset() end)
end

function init_drw()
  cls()
  pcenter("press ❎ to roll",40)
  -- draw roster
  for i,c in ipairs(chars) do
    c:dr(64+(i-1)*10,64)
    local dx = 0
    if (c.ini < 10) dx=2
    ?c.ini,64+(i-1)*10+dx,73,8
  end
  -- draw d20
  if rval==20 and dflash>0 then
    pal(7,dflash)
    dflash-=1
  end
  spr(192,16,56,2,4)
  spr(192,31,56,2,4,true)
  pal()
  -- show number on d20
  local rx = 28
  if (rval<10) rx+=2
  local rcolor = 10
  if (not roll) rcolor=8
  ?rval,rx,73,rcolor
  if (roll and debug) ?"ROLLING",0,0
end

function init_upd()
  if btnp(❎) then
    rtime=1
    roll=true
    p+=1
    g.upd = rollad20
  end
  -- +1 here helps us not change
  -- screens immediately once
  -- we roll the last char
  if (p==#chars+1) cmbt_ini()
end

-- Exponentially slow dice rolling
function rollad20()
  if (T%rtime==0) rval = flr(rnd(20)) + 1

  if (T%10==0) rtime*=2 

  if (rtime > 127) then
    g.upd = init_upd
    if (rval == 20) dflash=30
    chars[p].ini = rval
    roll = false
  end
end

-->8
-- credits

-->8
--tools

-- finds shortest path from map coordinates
-- (i0,j0) --> (i1,j1)
-- https://gist.github.com/damienstanton/7de65065bf584a43f96a
-- https://github.com/lattejed/a-star-lua/blob/master/a-star.lua
function a_star(i0,j0,i1,j1,h)
  h = h or dist
  local start = {i0,j0}
  local goal  = {i1,j1}
  local closedset = {}
  local openset   = {start}
  local came_from = {}

  local g_score, f_score = {}, {}
  local istart = v2i(start)
  g_score[istart] = 0
  f_score[istart] = h(start,goal)

  while #openset>0 and #openset<1000 do
    -- get node in openset with lowest f_score
    local current = lowest_f_score(openset, f_score)

    if nodes_eq(current,goal) then
      -- add(debugs,'goal found!')
      -- Note:
      -- no need to save the 
      -- goal position because
      -- we can only go up to it
      -- not on top of it
      local path = {}
      while not nodes_eq(current,start) do
        current = came_from[v2i(current)]
        add(path,current)
      end
      -- Note:
      -- no need to reverse the
      -- order because we pop
      -- the elements off in
      -- reverse order for
      -- animation

      -- pop the end which is 
      -- the current location
      deli(path)
      return path
    end

    -- remove from openset and
    -- move to closed set since
    -- we're exploring it now
    del(openset,current)
    add(closedset,current)
    
    -- generate cardinal direction
    -- children
    local neighbors = get_neighbors(current)
    -- add(debugs,"#neighbors:"..#neighbors)
    -- local neighbors = {}
    -- for i=1,4 do
    --   add(neighbors,{current[1]+dirx[i],current[2]+diry[i]})
    -- end

    for _,neighbor in pairs(neighbors) do
      if not is_in(closedset, neighbor) then
        local icurrent = v2i(current)
        local tentative_g_score = g_score[icurrent] + 1
        local ineighbor = v2i(neighbor)
        if not is_in(openset, neighbor) or tentative_g_score < g_score[ineighbor] then
          came_from[ineighbor] = current
          g_score[ineighbor]   = tentative_g_score
          f_score[ineighbor]   = tentative_g_score + h(neighbor,goal)
          if not is_in(openset,neighbor) then
            add(openset,neighbor)
          end
        end
      end
    end
  -- add(debugs,"#openset:"..#openset)
  end
  return {}
end

-- vector to index and
-- index to vector
-- used to index 2d map indices
-- into and out of arrays
function v2i(v,W)
  W = W or 128
  return v[1]+v[2]*W
end
function i2v(i,W)
  W = W or 128
  return {i%W,i\W}
end

function get_neighbors(node)
  local neighbors = {}
  for i=1,4 do
    local newi,newj=node[1]+dirx[i],node[2]+diry[i]
    if (is_walkable(newi,newj)) add(neighbors,{newi,newj})
  end
  return neighbors
end

function is_in(nodes, query)
  for _,node in pairs(nodes) do
    if (nodes_eq(node,query)) return true
  end
  return false
end

function lowest_f_score(nodes, f_score)
  local INF = 1/0
  local lowest, bestnode = INF,nil
  for _,node in pairs(nodes) do
    local score = f_score[v2i(node)]
    if score < lowest then
      lowest,bestnode=score,node
    end
  end
  return bestnode
end
-- checks if two nodes are the 
-- same. again this is needed
-- because tables with same 
-- values are not == equal
function nodes_eq(a,b)
  return a[1]==b[1] and a[2]==b[2]
end

-- Calculates distance between
-- two 2D points a and b. types are
-- 'man': manhattan distance (l1-norm)
-- 'euc': euclidean distance (l2-norm)
function dist(a,b,type)
  type = type or 'euc'
  local dx,dy = (b[1]-a[1]),(b[2]-a[2])
  if type=='euc' then
    return sqrt(dx*dx+dy*dy)
  elseif type=='man' then
    return abs(dx) + abs(dy)
  end
  return nil
end

-- reverse an ordered list in place
function reverse(list)
  local n = #list
  local i = 1
  while i < n do
    list[i],list[n] = list[n],list[i]
    i+=1
    n-=1
  end
end

function bouncebutt(_b,_x,_y,_c1,_c2)
  --           ⬅️     ➡️     ⬆️     ⬇️     🅾️     ❎
  local b = {"\139","\145","\148","\131","\142","\151"}
  _b = mid(0,_b,5)
  local bb = b[_b+1]

  if T%fps > fps\2 then
    _y+=1
  end
  oprint(bb,_x,_y+2,_c1,_c2,8)
  oprint(bb,_x,_y,_c1,_c2,4)
end

-- parses an actors dmg string
-- to generate the damage
-- e.g. actor dmg is '2d6+4'
-- this function will roll 2d6
-- and add 4 to the result and 
-- return that value
function roll_dmg(roll)
  -- find number before the d
  local t1 = split(roll,'d')
  -- find the number after the d
  -- find the number after the +
  local t2 = split(t1[2],'+')
  local dmg = 0

  -- roll 
  for i=1,t1[1] do
    dmg+=rollad(t2[1])
  end
  dmg+=t2[2]
  return dmg
end

-- set the correct sprite for 
-- actor a, based on direction
-- di and dj. assumes particualar
-- sprite order of 2 frame motion
-- with right, up, down
function actor_dir(_a,_di,_dj)
    if _dj > 0 then
      _a.spo = 2
    elseif _dj < 0 then
      _a.spo = 4
    else
      _a.spo = 0
    end
    _a.flip = _di < 0
end

-- sets the palette to gray scale
-- useful for objects we want to
-- indicate are not selectable
function grayscale(p)
  -- 3 palettes
  -- 0: draw palette
  -- 1: display palette
  -- 2: secondary palette
  p = p or 0
  pal({1,1,5,5,5,6,7,13,6,7,7,6,13,6,7,1}, p)
end

-- checks to see if there is an
-- actor at map tile location
-- (i,j)
function get_actor(_i,_j)
  for a in all(actors) do
    if a:i()==_i and a:j()==_j then
      return a
    end
  end
  return nil
end

-- debugging tool
function print_char(c,col)
  col = col or 0
  ?c.name
  ?"init:"..c.ini
  ?"hp:"..c.hp
  ?"mvmt:"..c.mvmt
  ?"bktrk:"..c.bktrk
  ?"#tail:"..#c.tail
  ?"atk:"..c.atk.name
  ?"atkdmg:"..c.atk.dmg
  ?"atkrng:"..c.atk.rng
end

-- quick loop to return any btnp
-- or importantly let us know 
-- that nothing was pressed
function getbutt()
  for i=0,5 do
    if (btnp(i)) then
      return i
    end
  end
  return -1
end

-- Ref
-- https://www.lexaloffle.com/bbs/?tid=2464
function linear(t,b,c,d)
  return c*t/d+b
end

-- used to see if an attack can 
-- land. dnd rules say a square 
-- is 5 feet so we don't worry
-- about euclidean norm
function in_range(x0,y0,x1,y1,r)
  return (abs(x0-x1)<=r) and (abs(y0-y1)<=r)
end
  
-- find all actors of type t 
-- within range r of character p
-- sort by closest
function find_actors_in_range(p,r,t)
  r = r or 1
  local c = {}
  local dists = {}
  for a in all(actors) do
    if a.type==t then
      local pi,pj,ai,aj = p:i(),p:j(),a:i(),a:j()
      if in_range(pi,pj,ai,aj,r) then
        add(c,a)
        add(dists,dist({pi,pj},{ai,aj}))
      end
    end
  end

  -- sort c based on dists
  sort(dists,c)
  return c
end

-- insertion sort a list in place
-- will also sort a second list 
-- using the first as the comparator
function sort(a,b)
  assert(#a==#b)
  for i=1,#a do
      local j = i
      while j > 1 and a[j-1] > a[j] do
          a[j],a[j-1] = a[j-1],a[j]
          -- if b is not present
          -- it evaluates to nil
          -- which is false
          if (b) b[j],b[j-1] = b[j-1],b[j]
          j = j - 1
      end
  end
end

-- draws my fancy box with fancy
-- corners
function draw_box(x,y,w,h,ic,bc)
  ic = ic or 5
  bc = bc or 0
  if bc>=0 then
    rect(x, y+1,(x+w)-1, y+h-2, bc)
    rect(x+1,y,(x+w)-2,y+h-1,bc)
  end
  if ic>=0 then
    rectfill(x+1,y+2,(x+w)-2,(y+h)-3,ic)
    rectfill(x+2,y+1,(x+w)-3,(y+h)-2,ic)
  end
end

-- draws a box and puts text
-- that *should* autowrap to fit
-- the desired box size
-- can overflow the box height
-- if h is specified
function text_box(s,x,y,w,h,ic,bc,tc)
  s = s or "" -- string
  w = w or 64  -- box width
  local lines = fit_string(s,w-4)
  h = h or 6*#lines -- box height
  x = x or 32  -- x position
  y = y or 32  -- y position
  ic = ic or 5 -- fill color
  bc = bc or 0 -- border color
  tc = tc or 0 -- text color
  draw_box(x,y,w,h,ic,bc)
  clip(x+2,y+2,w-4,h-4)
  for i,line in ipairs(lines) do
    ?line,x+2,y+2+6*(i-1),tc
  end
  clip()
end

-- box of marching ants. good 
-- for showing a selection among
-- a bunch of other similar things.
-- can be animated or not animated
-- by bool an
function ants_box(x,y,w,h,c,an)
  c = c or 0x8 -- red
  local bf = 0x936c.936c
  if an then
    bf = bf>><(t()<<5&12) | 0x0.8
  end
  fillp(bf)
  rect(x,y,x+w,y+h,c)
  fillp()
end

-- fits a string into a width
-- creating new lines to ensure
-- it doesn't overflow the width
-- s: input string
-- w: max width
-- d: string splitting delimiter
function fit_string(_s,_w,_d)
  _d = _d or " "
  local strs = split(_s,_d,false)
  local builder = ""
  local lines = {}

  for str in all(strs) do
    if 4*(#builder+#str) > _w then
      add(lines,builder)
      builder = ""
    end
    builder..=str.." "
  end
  add(lines,builder)
  return lines
end

function rollad(num)
  return flr(rnd(num))+1
end

-- sorts a list of actors by their
-- initiative. simple insertion sort
-- stores sorted list into chars
function order_chars()
  local c = {}
  -- filter for characters
  for a in all(actors) do
    if a.type=='en' or a.type=='pc' then
      add(c,a)
    end
  end

  -- sort by init
  for i=1,#c do
    local j = i
    while (j > 1) and (c[j-1].ini < c[j].ini) do
      c[j],c[j-1] = c[j-1],c[j]
      j = j - 1
    end
  end

  chars = c
end

--[[ 
  getlr and getud are used to 
  simplify the button tracking
  process for dpad input
]]
-- Get left/right
function getlr()
  return band(btnp(),2)/2-band(btnp(),1)
end

-- Get up/down
function getud()
  return band(btnp(),8)/8-band(btnp(),4)/4
end

function is_tile(tile_type,i,j)
  return fget(mget(i,j),tile_type)
end

function is_char(a)
  return a.type=='pc' or 
         a.type=='en'
end

function swap_tile(i,j)
  mset(i,j,mget(i,j)+1)
end

function interact(e1,e2,i,j)
  if (e1.type=="pc" and e2.type=="en")
      and (i==e2.i and j==e2.j) then
    e1.hp-=5
    return true
  end
  return false
end

-- prints text with outline
function oprint(_t,_x,_y,_c,_c2,_n)
  _n = _n or 8
  for i=1,_n do
    ?_t,_x+dirx[i],_y+diry[i],_c2
  end
  ?_t,_x,_y,_c
end

-- useful constants
dirx={-1,1,0,0,1,1,-1,-1}
diry={0,0,-1,1,-1,1,1,-1}

__gfx__
000000000000000088e8800088e8800008e8880008e8880c00888e80c0888e800000000000000000000000000000000008e888000088000008e8880000880000
000000000000000008ee880c08ee880008ee880c08ee8804c088ee804088ee800000000000000000000000000000000008ee880c08e8880008ee880c08e88800
007007000000000008f0f00408f0f0c00f0f0f040f0f0f0440f888f040f888f0000000000000000000000000000000000f0f0f0408ee88c00f0f0f0408ee88c0
00077000000000000effff040effff40ffffff040fffffff40ff88fffff88ff0000000000000000000000000000000000fffff040f0f0f400fffff040f0f0f40
00077000000000008888888f08888840088888fff8888800ff8888800088888f00000000000000000000000000000000888888ffffffff40888888ffffffff40
0070070000000000f8ee88000fee88f008ee880008ee88000088ee800088ee8000000000000000000000000000000000f8ee880008ee88f0f8ee880008ee88f0
00000000000000000888880008888800088888808888880008888880008888880000000000000000000000000000000008888800088888000888880008888800
00000000000000008888880088888880888800000000888000008888088800000000000000000000000000000000000088888880888888808888888088888880
000000000000000033bb30e0033b300003bb300f03bb3000f0033b3000033b30000000000000000000000000000000000000000000000000851155a800000000
000000000000000003bbb30f33bbb3f003bbb30f03bbb30ff033bb30f033bb30000000000000000000000000000000000000000000000000a810508600000000
00000000000000000340400f034040f00404040f0404040ff0433340f0433340000000000000000000000000000000000000000000000000a58058a600000000
00000000000000000144440f014444f0044444340444440f43443340f0433440000000000000000000000000000000000000000000000000a55885a600000000
000000000000000033bbb33403bbb34033bbb30e33bbb334e03bbb33433bbb330000000000000000000000000000000000000000000000004448855500000000
00000000000000004555650e045565e04555650e0555650ee0565554e0565550000000000000000000000000000000000000000000000000408558a600000000
00000000000000000333330e033333e00323330e0333230ee0333230e03233300000000000000000000000000000000000000000000000004845558a00000000
0000000000000000002020e002000200000020000020000e00020000e00002000000000000000000000000000000000000000000000000008a2a2aa800000000
0000000000000000a67766aea67766eaa67766aea67766aeea66776aea66776a0000000000000000000000000000000000000000000000000000000000000000
0000000000000000a67060aea67060eaa60606aea60606aeea66666aea66666a0000000000000000000000000000000000000000000000000000000000000000
0000000000000000a66060aea66060eaa60606aea60606aeea66666aea66666a0000000000000000000000000000000000000000000000000000000000000000
0000000000000000a66666aea66666ea555666aea6666666ea6666656666666a0000000000000000000000000000000000000000000000000000000000000000
000000000000000055577666a555766a51567666555676ae66677665ea6776650000000000000000000000000000000000000000000000000000000000000000
000000000000000051566daea5156dea555dd6ae515dd6aaea6d6665aa6d66650000000000000000000000000000000000000000000000000000000000000000
0000000000000000555666aaa55566aaaa66d6aa555666aaaa6666aaaa6666550000000000000000000000000000000000000000000000000000000000000000
0000000000000000aadadaaaadaaadaaaadaaaaaaaaadaaaaaaaadaaaaadaaaa0000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
44444440444944444444444044444440000000000000000000000000000000000000000000000000000000000000000044444444444904444444444444490444
49994440999444444999444049994440000000000000000000000000000000000000000000000000000000000000000049994449999404494999444999940449
44499440444449944449944044499440000000000000000000000000000000000000000000000000000000000000000044499444444409994449944444440999
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
44444444444444404444444444444444000000000000000000000000000000000000000000000000000000000000000044440444444444444444044444444444
99944444499944409994444499944444000000000000000000000000000000000000000000000000000000000000000099940449499944499994044949994449
44494994444444404449499444494994000000000000000000000000000000000000000000000000000000000000000044490999444444444449099944444444
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
44444440444944444444444044444440000000000000000000000000000000000000000000000000000000000000000044444444444904444444444444490444
44494440999444444449444044494440000000000000000000000000000000000000000000000000000000000000000044494449999404494449444999940449
44994440444449944499444044994440000000000000000000000000000000000000000000000000000000000000000044994444444409994499444444440999
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
99944444444994409994444499944444000000000000000000000000000000000000000000000000000000000000000099944444444994409994444444499440
44949994444444404494999444949994000000000000000000000000000000000000000000000000000000000000000044949999444444404494999944444440
44449444444444404444944444449444000000000000000000000000000000000000000000000000000000000000000044449444444444404444944444444440
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
44444440444944444444444044444440000000000000000000000000000000000000000000000000000000000000000044444444444904444444444444490444
49994440999444444999444049994440000000000000000000000000000000000000000000000000000000000000000049994449999404494999444999940449
44499440444449944449944044499440000000000000000000000000000000000000000000000000000000000000000044499444444409994449944444440999
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
44444444444444404444444444444444000000000000000000000000000000000000000000000000000000000000000044440444444444444444044444444444
99944444499944409994444499944444000000000000000000000000000000000000000000000000000000000000000099940449499944499994044949994449
44494994444444404449499444494994000000000000000000000000000000000000000000000000000000000000000044490999444444444449099944444444
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
44444440444944444444444044444440000000000000000000000000000000000000000000000000000000000000000044444444444904444444444444490444
44494440999444444449444044494440000000000000000000000000000000000000000000000000000000000000000044494449999404494449444999940449
44994440444449944499444044994440000000000000000000000000000000000000000000000000000000000000000044994444444409994499444444440999
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
99944444444994409994444499944444000000000000000000000000000000000000000000000000000000000000000099944444444994409994444444499440
44949994444444404494999444949994000000000000000000000000000000000000000000000000000000000000000044949999444444404494999944444440
44449444444444404444944444449444000000000000000000000000000000000000000000000000000000000000000044449444444444404444944444444440
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000070000000000000000000000000000000000000000000000000000000000000000cccccccc00000000000000000000066600044440820000e8
00000000000007770000000000000000000000000000000000000000000000000000000000000000cc11cccc00000000fff400000000677d00004400e8200e82
00000000000777070000000000000000000000000000000000000000000000000000000000000000c111111c00000000ff44000000067f7d000999900e82e820
00000000077700070000000000000000000000000000000000000000000000000000000000000000ccccc1cc00000000f44400006667f7d00444444900e88200
00000000770000070000000000000000000000000000000000000000000000000000000000000000cccccccc00000000f44490006c7f7d004444444900e88200
00000077700000070000000000000000000000000000000000000000000000000000000000000000cc11cccc000000004444494006c7d000444444210e82e820
00007770000000070000000000000000000000000000000000000000000000000000000000000000ccc1111c0000000044444444616cd00024422211e8200e82
00077000000000070000000000000000000000000000000000000000000000000000000000000000cccccc110000000022222222d50dd00002221110820000e8
00777777777777770000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000660000000000000000
0077000000000077000000000000000000000000000000000000000000000000000000000000000000000000000060000000b00000000f7d0000000000000000
0077000000000070000000000000000000000000000000000000000000000000000000000000000000000000000066000000bb000000f9400000000000000000
0077000000000770000000000000000000000000000000000000000000000000000000000000000000000000006606600099bbb0000f94000000000000000000
0077700000000700000000000000000000000000000000000000000000000000000000000000000000000000006060000099900000f940000000000000000000
00707000000077000000000000000000000000000000000000000000000000000000000000000000000000000600600009994000ee9400000000000000000000
00707000000770000000000000000000000000000000000000000000000000000000000000000000000000000606000009940000082000000000000000000000
00707000000700000000000000000000000000000000000000000000000000000000000000000000000000006660000099400000002000000000000000000000
007077000077000000000000000000000000000000000000000000000000000000000066000006660000066600000000000000000c000c0000c000c000000000
007007000070000000000000000000000000000000000000000000000000000000000f7d00000f9d000000460000b0000c000c00cec0cec00cec0cec00c000c0
00700700077000000000000000000000000000000000000000000000000000000000f9400000f94d000004060000bb00cec0cec0cec0cec00cec0cec0cec0cec
0070077007000000000000000000000000000000000000000000000000000000000f9400000f9400000040000009bbb0cec0cec0ccccccc00ccccccc0cec0cec
007000707700000000000000000000000000000000000000000000000000000000f9400000f940000004000000099000ccccccc0c1ccc1c00c1ccc1c0ccccccc
0070007070000000000000000000000000000000000000000000000000000000ee940000a9940000a9a0000000000000c1ccc1c0ccccccc00ccccccc0c1ccc1c
0070007770000000000000000000000000000000000000000000000000000000082000000a9000000990000000000000ccccccc00ccecc0000ccecc00ccccccc
00777777777777770000000000000000000000000000000000000000000000000020000040a0000040a00000000000000ccecc00000000000000000000ccecc0
0007700770000000000000000000000000000000000000000000000000000000000000000000af00004444400000006600000000000000000000000000000000
0000777077000000000000000000000000000000000000000000000000000000000000000000ff0000044400000006660700070004000400050005000c000c00
0000007777770000000000000000000000000000000000000000000000000000000000000033300000099990000066607e707e704e404e405e505e50cec0cec0
0000000077777000000000000000000000000000000000000000000000000000000000000303333004444449000666007e707e704e404e405e505e50cec0cec0
000000000077770000000000000000000000000000000000000000000000000000000000000330004444444955556000777777704444444055555550ccccccc0
000000000007777000000000000000000000000000000000000000000000000000000000004004004444444401550000787778704044404050555050c0ccc0c0
000000000000077700000000000000000000000000000000000000000000000000000000040004004444444401150000777777704444444055555550ccccccc0
000000000000000700000000000000000000000000000000000000000000000000000000000004000444444050050000077e7700044e4400055e55000ccecc00
__gff__
0000010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000101000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000002020000000000000000000000000000000101010100000000000000000000000001010101
__map__
4041424343414243404142434041424300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
50515253505152535051ca53501e525300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
60616263606162636061ca636061626300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
7071727370717273707172ca7071727300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
404142434040404040404040caca424300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
5051404040404343434343434040525300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
6061626360614343434343436061626300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
7071727370715373707172437071727300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
4041424340414243404142434041424300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
5051525350515241505152435051525300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
6061406360616241606162436040626300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
7071724040717273404040404040727300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
4041424340404243404040404041424300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
5051525350404040404040405051525300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
6061626360616240404062636061626300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
7071727370717273704072737071727300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__sfx__
010501021824018440000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
780c000009870098701587009870098001587009870098700987309870158700987009800158700987009870098730987015870098700980015870098700987007870078701387007870078730e870138701f870
780c000005870058701187005870098001187005870058700587305870118700587009800118700587005870048700487010870048700980010870048700487004873048701087004870048730b870108701c870
900c00000c3530030000300003000c3530030000300003000c3530030000300003000c3530030000300003000c3530030000300003000c3530030000300003000c3530030000300003000c353003000030000300
3c0c0000000002144024450174002445021440000001c44021450000001c4402345000000234402845000000284402d4600000026400264400000026440294500000028450244300000023440244501f44000000
3c0c000000000214402445017400244501c440000001c4402445000000214501c4400000023440244500000028460244500140000000244402645000000264502444000000244402645023440244501f44024400
780c00000987009870158700987009800158700987009870098730987015870098700980015870098700987009873098701587009870098001587009870098701587015870098701887018870098401a8701c870
780c000005870058701187005870098001187005870058700587305870118700587009800118700587005870048700487010870048700980010870048701c8701c8701c87018870188701a870188701387013870
4a0c00080c3733f6353e6303f6353e6723f6353e6303f6350c3003f6003c6003f6000c3003f6003c6003f6000c3000030000300003000c3000030000300003000c3000030000300003000c300003000030000300
450c0000157761c766217661a766157561c746217361a726157061c706217761a76615706197061c7762176615766197661c7562174615736197261c7162171615706197161c7062171615706197061c70621706
450c0000157761c7661f76621766157561c7461f73621726157061c7061f7762176613706177061a7761f76613766177661a7561f74613736177261a7161f71613706177161a7061f71613706177061a7061f706
__music__
00 08424344
00 09424344
00 080a0b44
00 090a0c44
00 0d0f0b10
00 0e0f0c11
00 0d0f0b10
02 0e0f0c11

