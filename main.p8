pico-8 cartridge // http://www.pico-8.com
version 36
__lua__
-- main

key = -1

-- ***** WINDOW STACK ***** --
WindowStack = {}
function WindowStack:new()
  local o = {stack={}}
  setmetatable(o, self)
  self.__index = self
  return o
end
function WindowStack:push(window)
  add(self.stack,window)
end
function WindowStack:pop()
  return deli(self.stack)
end
function WindowStack:peek()
  return self.stack[#self.stack]
end
function WindowStack:update()
  self:peek():update()
end
function WindowStack:draw()
  for window in all(self.stack) do
    window:draw()
  end
end

-- ***** WINDOW ***** --
Window = {}
function Window:new(name,x,y,w,h,bc,ic)
  local o = {name=name,x=x,y=y,w=w,h=h,bc=bc,ic=ic,s=0}
  setmetatable(o, self)
  self.__index = self
  return o
end
function Window:draw()
  draw_box(self.x,self.y,self.w,self.h,self.bc,self.ic)
end
function Window:update()
  -- ??? what do we do here?
end

function _init()
-- global tables for game
-- can be modified by game
-- state ini functions
  g = {} -- games state
  actors = {}
  chars = {}
  ens = {}
  floats={}
  debugs = {}
  path = {}
  windowStack = WindowStack:new()

-- useful parameters 
  T = 0
  timer = 0
  dt = 0
  debug = true
  -- debug = false
  fps = 30

-- making roster
  -- the knight
  knight = make_knight('the knight',64,64)

  m = make_map()
  -- replace enemy tiles with 
  -- actual enemies
  for i=0,15 do
    for j=0,15 do
      if mget(i,j)==30 then
        mset(i,j,64)
        en = make_knight('the dark knight',i*8,j*8)
        en.pals  = {[6]=5,[7]=1,[5]=4,[1]=0,[13]=2,[14]=6}
        en.type='en'
        en.ini = 19
      end
    end
  end


  -- mmenu_ini()
  -- exp_ini()
  -- init_ini()
  cmbt_ini()
end

function _update()
  T+=1
  key = checkKeyPressed()
  windowStack:update()
  for _,a in pairs(actors) do
    a:update()
  end
  for _,f in pairs(floats) do
    f:update()
  end
  g.upd()
  key = -1
end

function _draw()
  cls()
  m:dr()
  windowStack:draw()
  for _,a in pairs(actors) do
    a:draw()
  end
  for _,f in pairs(floats) do
    f:draw()
  end
  g.drw()
  -- vvvvvvvv debugging vvvvvvvv
  -- a* path rendering
  for i,p in ipairs(path) do
    local x,y=8*p[1],8*p[2]
    rect(x,y,x+8,y+8,8)
    ?i-1,x+2,y+2,12
  end


  cursor(4,4)
  color(7)
  -- print_char(chars[pturn])
  for d in all(debugs) do
    ?d
  end
  debugs = {}
  -- end
end

-- table that holds default
-- values. useful for stats like
-- hp, mvmt, etc.
function statblock(t)
  local d = {}
  for k,v in pairs(t) do
    d[k] = v
  end
  t.__def = d

  function t:reset()
    for k,v in pairs(self.__def) do
      self[k] = v
    end
  end

  return t
end

object={
  name="",sp=255,
  x=64,y=64,w=1,h=1,
  fliplr=false,tx=64,ty=64,
  dx=0,dy=0,ox=0,oy=0,
  t=0,tdur=0,
  atype='none'
}
function object:new(o)
  o = o or {}
  o.super = object
  return setmetatable(o, {__index = self})
end
-- get map tile coordinates i,j
function object:i()
  return self.x\8
end
function object:j()
  return self.y\8
end
-- draw sprite, apply palette
-- swaps if needed
function object:draw()
  if (self.sp==0) return
  -- expects something of the form
  -- pals = {[1]=3,[5]=8...}
  if (self.pals) pal(self.pals)
  -- expects a 16 bit number 
  -- with bits to turn on/off
  -- transparency
  -- e.g. 0b1000000000000000
  -- makes black transparent
  if (self.palts) palt(self.palts)
  spr(self.sp,self.x,self.y,self.w,self.h,self.fliplr)
  pal()
end
-- sets tween, does not do any
  -- actual movement
function object:set_tween(x,y,d,type)
  -- prevent tweening before
  -- we're done with last tween
  if (self.atype~='none') return

  self.tx,self.ty=x,y
  self.tdur=flr(d*fps)
  self.dx,self.dy=(x-self.x),(y-self.y)
  self.ox,self.oy=self.x,self.y
  self.atype=type
  self:set_dir(self.dx,self.dy)
end
-- sets sprite direction if 
-- there is a sprite 
-- to be changed
function object:set_dir(dx,dy)
  if (not self.frames) return
  self.fliplr = dx<0
  local fs = self.frame_sets
  local f
  if dy < 0 then
    f = fs[3]
  elseif dy > 0 then 
    f = fs[2]
  else
    f = fs[1]
  end
  self.frames = f
  self.sp = f[1]
end
-- performs actual movement
-- and updates sprite for 
-- animation
function object:move()
  if (flr(self.tdur)<=0) return

  local tme = self.t
  if self.atype=='bump' and 
     tme>self.tdur\2 then
        tme = self.tdur - tme
  end

  self.x = linear(tme,self.ox,self.dx,self.tdur)
  self.y = linear(tme,self.oy,self.dy,self.tdur)
  
  if self.frames then
    local tt = (tme*4\self.tdur)%#self.frames+1
    self.sp = self.frames[tt] 
  end

  self.t+=1

  if (self.t > self.tdur) then
    self.dx,self.dy  = 0,0
    self.ox,self.oy  = 0,0
    self.tx,self.ty  = 0,0
    self.t,self.tdur = 0,0
    self.atype = 'none'
    if (self.frames) self.sp = self.frames[1]
  end
end

actor = object:new()
function actor:update()
  self:move()
end
function actor:draw()
  -- draw movement path underneath
  local tail = self.tail
  if tail then
    for n=1,#tail do
      local x0 = tail[n][1]*8
      local y0 = tail[n][2]*8
      rect(x0,y0,x0+7,y0+7,8)
    end
  end
  -- then draw sprite
  self.super.draw(self)
end
function actor:end_turn()
  self.stats:reset()
  self.tail = {}
end

-- windowStack
-- window = object:new{txt="",sp=0}
-- function window:new(name,x,y,w,h,bc,ic)
--   local o = {name=name,x=x,y=y,w=w,h=h,bc=bc,ic=ic}
--   return setmetatable(o, {__index = self})
-- end
-- function window:upd()
--   self:move()
--   -- revert text for temporary 
--   -- messages
--   if self.txtdur then
--     self.txtdur-=1
--     if self.txtdur <=0 then
--       self.txt,self.oldtxt = self.oldtxt,nil
--       self.txtdur = nil
--     end
--   end
-- end
-- function window:draw()
--   text_box(self.txt,self.x,self.y,self.w,self.h,self.bc,self.ic)
-- end
-- -- sets window text
-- -- can be temporary for dur 
-- -- seconds
-- function window:set_txt(txt,dur)
--   if dur then
--     self.oldtxt = self.txt
--     self.txt = txt
--     self.txtdur = flr(dur*fps)
--     return
--   end
--   if (not self.txtdur) self.txt = txt
-- end
-- function window:pop_self()
--   del(windowStack,self)
-- end

-- -- makes a floating text object
-- -- at location (x,y) with color 
-- -- c that lasts for dur seconds
float = {}
function float:new(txt,x,y,c,dur)
  c = c or 8
  dur = dur or 0.5
  local o = {txt=txt,x=x,y=y,
            ty=y-6,c=c,t=0,
            tdur=flr(dur*fps)
            }
  return setmetatable(o, {__index = self})
end
function float:draw()
  oprint(self.txt,self.x,self.y,self.c,0)
end
function float:update()
  self.t+=1
  if self.t > self.tdur then
    del(floats,self)
  end
  self.y+=(self.ty-self.y)/10
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

-- characters
-- https://roll20.net/compendium/dnd5e/Knight#content
function make_knight(name,x,y)
  local k = actor:new{name=name,x=x,y=y}
  -- pals and palts interfaces 
  -- are different :'(
  k.pals={[14]=5}
  k.palts = 0b0000000000100000
  k.sp = 34
  k.frames = {34,35}
  k.frame_sets = {{34,35},{36,37},{38,39}}
  k.stats = statblock{ac=18,hp=52,num_attacks=2,mvmt=6}
  k.abil = statblock{str=16,dex=11,con=14,int=11,wis=11,cha=15}
  k.tail = {}
  k.atk = 1 -- currently selected attack in atks
  k.acts = {
    {sp=204, name="MOVE"},
    {sp=205, to_hit=5, name="GREAT SWORD", dmg="2D6+3", rng=1},
    {sp=221, to_hit=2, name="HEAVY CROSSBOW", dmg="1D10", rng=20}
  }
  k.type='pc'
  k.ini = 21
  add(actors,k)
  return k
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
-- combat menu stack
  windowStack = WindowStack:new()
  local x,y,w,h=64,112,86,16

  -- ***** SELECTION MENU *****
  local selectionWindow = Window:new('selection',128-w,128-h,w,h,0,7)
  selectionWindow.items = {'actions','special','inventory','end'}
  selectionWindow.handlers={combatActionsClick}
  selectionWindow.arrowHandler = selectionClick
  selectionWindow.oHandler = nil
  selectionWindow.xHandler = combatActionsClick
  function selectionWindow:draw()
    draw_box(self.x,self.y,self.w,self.h,self.bc,self.ic)
    for i, item in ipairs(self.items) do
      ?item,self.x+6+36*((i-1)\2),self.y+2+7*((i-1)%2),0
    end
    ?"\23",self.x+2+36*(self.s\2),self.y+2+7*(self.s%2),0
  end
  function selectionWindow:update()
    local s = self.s
    if (key < 0) return
    if key <= 1 then
      s = (s+2*dirx[key+1])%4
    elseif key <= 3 then
      s = (s+diry[key+1])%2 + flr(s/2)*2
    elseif key == 4 then
      s = 3
    elseif key == 5 then
      self.xHandler()
    end
    self.s = s
  end
  windowStack:push(selectionWindow)





  -- ***** SPECIAL MENU *****
  -- subwin = Window:new('special',128-w,128-h,w,h,0,7)

  -- ***** INVENTORY MENU *****
  -- subwin = Window:new('inventory',128-w,128-h,w,h,0,7)

  -- ----- move sub-sub-window ----
  -- wmove = window:new('move',128-w,128-h,w,h,0,7)
  -- wmove.txt = {'on the move!'}
  -- function wmove:upd(bb)
  --   if (bb>=4) then
  --     self:pop_self()
  --     return
  --   end
  -- end
  -- add(wact.sub_wins,wmove)
  -- ------------------------------
  -- ----- attack sub-sub-window --
  -- watk = window:new('attack',128-w,128-h,w,h,0,7)
  -- watk.txt = {'attack!'}

  -- function watk:upd(bb)
  --   if (bb==4) then
  --     self:pop_self()
  --     return
  --   end
  --   ens = {}
  --   enp = nil
  --   if pcurr.stats.num_attacks<=0 then
  --     self.txt = {'no more attacks :('}
  --     return
  --   else
  --   end

  --   if bb==2 or bb==3 then
  --     enp+=diry[bb+1]
  --     enp=enp%#ens
  --   end

  --   if bb==5 then
  --     attack(pcurr,ens[enp+1],wact.s)
  --   end
  -- end
  -- add(wact.sub_wins,watk)
  -- -- hacky way to ensure both 
  -- -- types of attacks work
  -- add(wact.sub_wins,watk)


-- enemy selection
  ens={}
  enp=nil

end

function cmbt_upd()

end

function cmbt_drw()

  -- hp info
  local x,y,w,h = 70,80,128-76,20
  oprint(pcurr.name,x,y,7)
  y+=8
  oprint("HP",x,y,7)
  local hp,dhp = pcurr.stats.hp,pcurr.stats.__def.hp
  -- rectfill(x+8+2,y+3,x+w-3,y+1+5,5) -- shadow?
  rectfill(x+8,y+1,x+w-4,y+1+4,0)
  rectfill(x+8+1,y+2,x+w*hp/dhp-5,y+4,3)
  y+=6
  oprint(hp.."/"..dhp,x+w\2-10,y,7)
  y+=6
  oline(x,y,x+w,y,0,7)
  oline(x+w,y-24,x+w,y,0,7)
end

function cmbt_end_turn()
  pcurr:end_turn()
  pturn=pturn%#chars + 1
  pcurr = chars[pturn]
  wsel.s = 0
  if pcurr.type == 'en' then
    pcs = find_actors_in_range(pcurr,16,'pc')
    path = a_star(pcurr:i(),pcurr:j(),pcs[1]:i(),pcs[1]:j())
    wtxt:set_txt('ai turn! attacking '..pcs[1].name)
    g.upd = ai_cmbt_upd
  else
    g.upd = cmbt_upd
  end
end

function ai_cmbt_upd()
  local bot = pcurr

  if T%15 == 0 then -- move every half second
    if #path>0 then 
      local di,dj = unpack(deli(path))
      bot:set_tween(8*di,8*dj,0.25,'walk')
    elseif bot.stats.num_attacks>0 then
        wsel.s = 1
        attack(bot,pcs[1])
    else
      cmbt_end_turn()
    end
  end
end

-- performs attack from actor atk
-- to actor def 
-- TODO: lots of chances for 
-- token optimization
function attack(atk,def,atk_idx)
  -- default attack is the first one
  atk_idx = atk_idx or 1
  local def_x,def_y=def.x,def.y
  local astats,dstats = atk.stats,def.stats
  local a = atk.acts[atk_idx+1]

  -- set animation stuff
  atk:set_tween(def_x,def_y,0.2,'bump')

  -- prevent backtracking after attack
  astats.mvmt-=#atk.tail
  atk.tail={}

  -- roll for attack
  local hit, crit = roll_atk(a.to_hit, dstats.ac)
  if hit then
    -- roll for damage
    local dmg = roll_dmg(a.dmg,crit)
    add(floats,float:new("-"..dmg,def_x,def_y,8))
    dstats.hp-=dmg
  else
    add(floats,float:new(":(",atk.x,atk.y,9))
    wtxt:set_txt(pcurr.name.." missed!",2)
  end
  astats.num_attacks-=1
end

function new_atk(attacker,defender,atk)
  local astats,dstats = attacker.stats,defender.stats
  local hit, crit = roll_atk(atk.to_hit, dstats.ac)
  if hit then
    local dmg = roll_dmg(atk.dmg,crit)
    add(floats,float:new("-"..dmg,defender.x,defender.y,8))
    dstats.hp-=dmg
  else
    add(floats,float:new(":(",attacker.x,attacker.y,9))
    -- wtxt:set_txt(attacker.name.." missed!",2)
  end
  astats.num_attacks-=1
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
  test = test and (p.stats.mvmt - #p.tail) > 0
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

function wait(n)
  for i=1,n*fps do
    yield()
  end
end

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
  -- oprint(bb,_x,_y+2,_c1,_c2,8)
  oprint(bb,_x,_y,_c1,_c2,8)
end

-- roll to attack
function roll_atk(to_hit,def_ac)
  local roll = rollad(20)
  return roll + to_hit >= def_ac, roll==20
end

-- parses an actors dmg string
-- to generate the damage
-- e.g. actor dmg is '2d6+4'
-- this function will roll 2d6
-- and add 4 to the result and 
-- return that value
function roll_dmg(roll,crit)
  crit = crit or false
  -- find number before the d
  local t1 = split(roll,'D')
  if (crit) t1[1]*=2
  -- find the number after the d
  -- find the number after the +
  local t2 = split(t1[2],'+')
  local dmg = 0

  -- roll 
  for i=1,t1[1] do
    dmg+=rollad(t2[1])
  end
  if (t2[2]) dmg+=t2[2]
  return dmg
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

-- a wrapper on spr that also
-- calls grayscale if statement
-- g is true
function gspr(g,...)
  if (g) grayscale()
  spr(...)
  if (g) pal()
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
end

-- quick loop to return any btnp
-- or importantly let us know 
-- that nothing was pressed
-- write to global key
-- gets called once per update
function checkKeyPressed()
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
function draw_box(x,y,w,h,bc,ic)
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
function text_box(s,x,y,w,h,bc,ic,tc)
  s = s or "" -- string
  if (type(s) ~= 'table') s = {s}
  w = w or 32  -- box width
  -- local width, lines = fit_string(s,w-4)
  h = h or 32 -- box height
  x = x or 32  -- x position
  y = y or 32  -- y position
  ic = ic or 7 -- fill color
  bc = bc or 0 -- border color
  tc = tc or 0 -- text color
  draw_box(x,y,w,h,bc,ic)
  clip(x+2,y+2,w-4,h-4)
  for i,line in ipairs(s) do
    ?line,x+6+36*((i-1)\2),y+4+10*((i-1)%2),tc
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
  local widest = 0

  for str in all(strs) do
    if 4*(#builder+#str) > _w then
      add(lines,builder)
      builder = ""
    end
    builder..=str.." "
    widest = max(#builder, widest)
  end
  add(lines,builder)
  return widest,lines
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

function oline(x0,y0,x1,y1,c0,c1)
  rectfill(x0-1,y0-1,x1+1,y1+1,c0)
  line(x0,y0,x1,y1,c1)
end

-- useful constants
dirx={-1,1,0,0,1,1,-1,-1}
diry={0,0,-1,1,-1,1,1,-1}

-- HANDLERS
function combatActionsClick()
  local x,y,w,h=64,112,86,24
  -- make action window
  local actionWindow = Window:new('action',128-w,128-h,w,h,0,7)
  -- TODO: probably should populate this dynamically?
  actionWindow.handlers = {actionMoveClick, actionAttackClick, actionAttackClick}
  function actionWindow:draw()
    -- get action list of pcurr
    draw_box(self.x,self.y,self.w,self.h,self.bc,self.ic)
    for i, item in ipairs(self.items) do
      ?item,self.x+6,self.y+2+5*(i-1),0
    end
    ?"\23",self.x+2,self.y+2+5*self.s,0
  end
  function actionWindow:update()
    -- update action list from player
    self.items = {}
    for i, act in ipairs(pcurr.acts) do
      add(self.items,act.name)
    end
    if (key < 0) return
    if (key >= 0 and key < 4) then
      local d = dirx[key+1] | diry[key+1]
      self.s=(self.s+d)%#self.items
    end
    if (key == 4) then
      windowStack:pop()
    end
    if (key == 5) then
      self.handlers[self.s+1](pcurr.acts[self.s+1])
    end
  end
  -- push action window
  windowStack:push(actionWindow)
end

function actionMoveClick()
  local x,y,w,h=64,112,86,24
  -- make move window
  local moveWindow = Window:new('move',128-w,128-h,w,h,0,7)
  moveWindow.txt = {'on the move!'}
  function moveWindow:draw()
    draw_box(self.x,self.y,self.w,self.h,self.bc,self.ic)
    ?self.txt[1],self.x+6,self.y+2,0
  end
  function moveWindow:update()
    if (key < 0) return
    if (key >= 0 and key < 4) then
      local di,dj=dirx[key+1],diry[key+1]
      local atype = 'bump'
      if can_move(pcurr,di,dj) and
          pcurr.atype=='none' then
        if g.drw == cmbt_drw then
          track_tail(pcurr,di,dj)
        end
        atype = 'walk'
      end
      pcurr:set_tween(pcurr.x+8*di,pcurr.y+8*dj,.25,atype)
    end
    if (key == 4 or key == 5) windowStack:pop()
  end
  windowStack:push(moveWindow)
end

function actionAttackClick(atk)
  local x,y,w,h=64,112,86,24
  -- make attack window
  local attackWindow = Window:new('attack',128-w,128-h,w,h,0,7)

  -- update enemy list
  ens = find_actors_in_range(pcurr,atk.rng,'en')
  attackWindow.txt={#ens.." enemies in range\nfor attack\n"..atk.name}
  enp = 0

  function attackWindow:draw()
    draw_box(self.x,self.y,self.w,self.h,self.bc,self.ic)
    ?self.txt[1],self.x+6,self.y+2,0
    for i,e in ipairs(ens) do
        ants_box(e:i()*8-1,e:j()*8-1,9,9,7,i==(enp+1))
    end
  end

  function attackWindow:update()
    if (key < 0) return
    if (key >= 0 and key < 4) then
      local d = dirx[key+1] | diry[key+1]
      enp=(enp+d)%#ens
    end
    if (key == 4) windowStack:pop()
    if (key == 5) then
      new_atk(pcurr,ens[enp+1],atk)
      windowStack:pop()
    end
  end
  windowStack:push(attackWindow)
end
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

