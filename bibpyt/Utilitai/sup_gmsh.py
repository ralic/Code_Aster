#@ MODIF sup_gmsh Utilitai  DATE 05/10/2004   AUTEUR CIBHHLV L.VIVAN 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
# THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY  
# IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY  
# THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR     
# (AT YOUR OPTION) ANY LATER VERSION.                                                  
#                                                                       
# THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT   
# WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF            
# MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU      
# GENERAL PUBLIC LICENSE FOR MORE DETAILS.                              
#                                                                       
# YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE     
# ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,         
#    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.        
# ======================================================================

import os.path, string, os, copy
from Numeric import *

try :
  from Cata.cata import *
  from Accas import _F
except :
  print 'Fonctionnalites Aster indisponibles'    


_CARAC = {
  'prec' : 1.E-8
  } 


def Class_search(class_name, target_class) :

  """
    Check that class_name inherits from target_class
    (run recursively through the inheritance lists)
  """
  
  if class_name == target_class : return 1
  
  for cl in class_name.__bases__ :
    if Class_search(cl, target_class) : return 1
  return 0


def Progress(L,**para) :

  """
    Compute the unknown parameters for a geometric progression :
      r = ratio
      N = number of elements
      h = initial size
      
    So that :
      L = Sum(i=0:N-1, h(i)) where h(i+1) = h(i)*r, h(0)=h
      
    Usage :
      Progress(L,r=...,N=...) -> h
      Progress(L,r=...,h=...) -> N
      Progress(L,h=...,N=...) -> r
      
  """
  
  prec = 1.E-4
  
 # Calcul de N  
  if 'N' not in para.keys() :
    r = float(para['r'])
    h = float(para['h'])
    N = log(1+(r-1)*L/h)/log(r)
    N = int(N+0.5)
    return N
    
 # Calcul de h
  elif 'h' not in para.keys() : 
    r = float(para['r'])
    N = int(para['N']+0.5)
    h = L*(r-1)/(r**N-1)
    return h
    
 # Calcul de r
  elif 'r' not in para.keys() :
    h = float(para['h'])
    N = int(para['N']+0.5)
    a = L/h
    if N > a : 
      x = 0
    else :
      x = a**(1./(N-1))
    
    for i in xrange(100) :
      res = x**N - a*x + a-1
      if abs(res) < prec*(x-1)*a :
        return x
      dx = res/(a-N*x**(N-1))
      x = x+dx
      
    raise 'Solution failure'       
  
  else :
    raise 'Unknown parameters'
    
  
  
def Is_Geometric(object) :

  """
    return true if the object inherits of the Geometric class
  """

  return Class_search(object.__class__, Geometric)


  
# --------------------------------------------------------------------------------------

class Geometric :

  """
    GENERIC CLASS FOR GEOMETRICAL OBJECTS
    
    private attribute
     parameters  : dictionnary of the attributes (except relation and parameters itself)
                   see __getattr__ and __setattr__


    Attributes
     num         : index among gmsh objects
     md          : mesh descriptor
     mesh        : related mesh object
     relation    : model object in case of coincidence
     
     
    Public methods 
     Is_point : return true is the object inherits of the Point class

     Is_line  : return true is the object inherits of the Line class

     Is_surface : return true is the object inherits of the Surface class

     Is_volume : return true is the object inherits of the Volume class
     
     Is_same_dimension : return true is both objects are of the same dimension
                         (point, line, surface or volume)
       in -> object to compare to self                  

     Duplicate   : duplicate an object and base its mesh_descriptor
                   on the mesh_descriptor of the model

     Coincide    : assert that an object is coincident with a model one
                   All the attributes are then automatically read from 
                   the model object (see __setattr__ and __getattr__).
       in -> model object

    Private method 
    
     Root :
       Provides the root object of an object, ie the object itself if there is no relation
       or the deepest model in case of relation.
       
     Geometric_coincide : check if a geometrical coincidence is possible
                          return information about the coincidence, false else.
       in -> model object
       
     Deep_coincide : proceed recursively to ensure coincidence of the relevant sub-objects
       in -> model object
       in -> correspond (information returned by Geometric_coincide)
       
     __setattr__ : distinguish two sets of attributes 
                    relation (to express a relation with a model object in case of coincidence)
                    all the other attributes which are stored in the dictionnary parameters
                    instead of the usual __dict__ if there is no relation (see Coincide)
                    and in the model object if there is a coincidence
                    
     __getattr__ : if the object is related (relation <> None) the attribute is read
                   in the model object. Else, it is read in the current object, actually
                   in the dictionnary parameters (see __setattr__) 
                   
     Thanks to these two overloaded methods, the access to the attributes is usual if 
     there is no relation whereas the attributes of the model object are accessed 
     transparently if there is a relation.

     __cmp__ :
       The comparison of two objects involves possible coincidence. It is no more the object ids
       that are compared but the object roots (.relation if any).
     
     Gmsh  : produce the source code for Gmsh
       in -> mesh

     Gmsh_send : send a line code to the gmsh interpreter
       in -> line_code (string)
       
     Intermediate_meshing : produce the source code for the intermediate objects
       in -> mesh
       
     Object meshing : produce the source code for the current object
       var -> object number (modified if several objects are created)

  """
    
  
  def __init__(self) :
    self.relation    = None
    self.parameters  = {}      
    self.num         = 0 
    self.md          = Mesh_Descriptor()


  def Is_point(self) :
    return Class_search(self.__class__, Point)
  
  def Is_line(self) :
    return Class_search(self.__class__, Line)
    
  def Is_surface(self) :    
    return Class_search(self.__class__, Surface)
    
  def Is_volume(self) :
    return Class_search(self.__class__, Volume)

  def Is_same_dimension(self, obj) :
  
    return (
      (self.Is_point()    and  obj.Is_point()   ) or
      (self.Is_line()     and  obj.Is_line()    ) or
      (self.Is_surface()  and  obj.Is_surface() ) or
      (self.Is_volume()   and  obj.Is_volume()  )
      )
  
    
  def __setattr__(self, attr, value) :

    if attr in ['relation','parameters'] :
      self.__dict__[attr] = value
    else :
      if self.relation :
        setattr(self.relation,attr,value)
      else :
        self.parameters[attr] = value
    

  def __getattr__(self,attr) :
    
    if self.relation :
      return (getattr(self.relation,attr))
    else :
      if attr in self.parameters.keys() :
        return self.parameters[attr]
      else :
        raise AttributeError,attr

  
  def Root(self) :
  
    o = self
    while o.relation : o = o.relation
    return o
      
      
  def __cmp__(self,obj) :
  
    if self.Root() is obj.Root() :
      return 0
    else :
      return -1


  def Geometric_coincide(self,obj) : return 0     
  
  def Deep_coincide(self,obj,correspond) : pass
      
  def Coincide(self, obj) :
  
    if self == obj : return       # in that way recursive loops cannot exist
    
    if self.relation :            # the root is put in coincidence, not the object itself
      self.Root().Coincide(obj)
      return
      
    if not self.Is_same_dimension(obj) :
      raise 'Coincidence impossible : objects are not of the same dimension'
      
    correspond = self.Geometric_coincide(obj)
    if not correspond :
      raise 'The objects are not geometrically coincident'

    self.Deep_coincide(obj,correspond)      
    self.relation = obj
    
           
  def Duplicate(self) :
  
    return copy.deepcopy(self)    # special deepcopy for the Mesh_Descriptor
    
    
  def Gmsh(self,mesh) : 

    if self.num : return              # already meshed object
    self.mesh = mesh                  # Storing the mesh
    self.Intermediate_meshing(mesh)   # creation of the intermediate objects  
    num = mesh.num+1                  # New object number
    self.Object_meshing(num)          # object meshing (with current number num)
    mesh.num = num                    # Updating the current gmsh pointer
    self.num = num                    # Storing the current object number


  def Gmsh_send(self, line_code) :
  
    self.mesh.command.append(line_code)


  def Intermediate_meshing(self,mesh) : 
    pass


  def Object_meshing(self,num) :
    raise "Creation of the Gmsh source not implemented"
    



# -------------------- POINT OBJECTS ---------------------    

    
class Point(Geometric) :

  """
    POINT OBJECT
    
    Public methods
     __init__ :  
       in -> coordinates (the 3rd is zero by default)
       
     Size  : set the size of the neighbouring elements
       in -> size
     
     Attractor : define the point as an attractor
       in -> scale_x : size amplification factor in the x-direction
       in -> scale_y : size amplification factor in the y-direction
       in -> distance: influence distance for the perturbation
     
    Attributes
     coor : coordinates
     size : neighbouring element size
     attractor : parameters of the attractor
  """
  
  def __init__(self,x,y,z=0) :

    Geometric.__init__(self)  
    self.coor = array([x,y,z], Float)
    self.attractor = None
    

  def Geometric_coincide(self,obj) :

    global _CARAC
    prec = _CARAC['prec']
    
    d = VectorNorm(self.coor - obj.coor)
    if d < prec*self.md.size :
      return 1
    else :
      return None
    
    
  def Size(self,h) :
  
    self.md.size = float(h)
    
    
  def Attractor(self, scale_x, scale_y, distance) :
  
    self.attractor = (float(scale_x), float(scale_y), float(distance))
    

  def Translate(self,tran) :
  
    self.coor = self.coor + tran
    

  def Object_meshing(self,num) :
  
    ch = (
        'Point(' + `num` + ') = {'
      + `self.coor[0]` + ', '
      + `self.coor[1]` + ', '
      + `self.coor[2]` + ', '
      + `self.md.size` + '};'
      )
    self.Gmsh_send(ch)    
    
    if self.attractor :
      ch = (
        'Attractor Point{' + `num` + '} = {'
        + `self.attractor[0]`+','
        + `self.attractor[1]`+','
        + `1./self.attractor[2]` + '};'
        )
      self.Gmsh_send(ch)



# -------------------- LINE OBJECTS ----------------------    


class Line(Geometric) :

  """
    LINE OBJECT
    
    Public methods

     Attractor : define the point as an attractor
       in -> scale_x : size amplification factor in the x-direction
       in -> scale_y : size amplification factor in the y-direction
       in -> distance: influence distance for the perturbation

  """
     

  def __init__(self,*points) :
  
    Geometric.__init__(self)  

    if len(points) <=1 : 
      raise "There should be at least two points"
      
    for point in points :
      if not point.Is_point() :
        raise "Arguments should be points"
      
    self.points = list(points)
    self.attractor = None
        

  def Geometric_coincide(self,obj) :
      
    nb_points = len(self.points)
    if nb_points <> len(obj.points) :
      raise 'To coincide, lines should have the same number of points'
        
   # same order of points
    info = range(nb_points)
    for i in range(nb_points) :
      p1 = self.points[i]
      p2 = obj.points[info[i]]
      if not p1.Geometric_coincide(p2) :
        break
    else :
      return info
         
   # reverse order of points
    info.reverse()
    for i in range(nb_points) :
      p1 = self.points[i]
      p2 = obj.points[info[i]]
      if not p1.Geometric_coincide(p2) :
        break
    else :
      return info
    
    return None
         
         
  def Deep_coincide(self,obj,info) :

    for i in range(len(info)) :
      p1 = self.points[i]
      p2 = obj.points[info[i]]
      p1.Coincide(p2)      
    
    

  def Translate(self,tran) :
  
    for point in self.points :
      point.Translate(tran)
      
          
  def Transfinite(self,number,progression = 1) :
  
    self.md.number      = int(number)
    self.md.progression = float(progression)
    
  
  def Attractor(self,scale_x, scale_y, distance) :
  
    self.attractor = (float(scale_x), float(scale_y), float(distance))
    

  def __rmul__(self,base) :
  
    if len(self.points) > 2 :
      raise "Support (right argument) should be a straight line"
      
    if self.points[0] in base.points : 
      supp_orig = 0   
      supp_extr = 1
    elif self.points[1] in base.points : 
      supp_orig = 1
      supp_extr = 0
    else :
      raise "No common point"
    
    if self.points[supp_orig] == base.points[0] :
      base_orig = 0
      base_extr = -1
    else :
      base_orig = -1
      base_extr = 0

  # Translation vector    
    ce = self.points[supp_extr].coor
    co = self.points[supp_orig].coor
    tran = ce-co

  # Definition of the edge opposite to the base
    opp_base = base.Duplicate()
    opp_base.Translate(tran)
    opp_base.points[base_orig] = self.points[supp_extr]

  # Definition of the edge opposite to the support 
    opp_supp = self.Duplicate()
    opp_supp.points[0] = base.points[base_extr]
    opp_supp.points[1] = opp_base.points[base_extr]   

    surf = Surface(base,self,opp_base,opp_supp)
    
    if len(base.points) > 2 : surf.Ruled()
    
    return surf    
       

  def Intermediate_meshing(self,mesh) :
  
    for point in self.points :
      point.Gmsh(mesh)
    
    
  def Object_meshing(self,num) :
  
    ch = 'Line(' + `num` + ') = {'
    for point in self.points :
      ch = ch + `point.num` + ','
    ch = ch[:-1] + '};'
    self.Gmsh_send(ch)

    if self.md.transfinite :
      ch = (
          'Transfinite Line{' + `num` + '} = ' + 
          `self.md.number+1` + 
          ' Using Progression ' + `self.md.progression` + ';'
          )
      self.Gmsh_send(ch)
          
    if self.attractor :
      ch = (
        'Attractor Line{' + `num` + '} = {'
        + `self.attractor[0]`+','
        + `self.attractor[1]`+','
        + `1./self.attractor[2]` + '};'
        )
      self.Gmsh_send(ch)



class Circle(Line) : 

  def __init__(self,O,C,E) :

    Line.__init__(self,O,C,E)


  def Object_meshing(self,num) :

    ch = 'Circle(' + `num` + ') = {'
    for point in self.points :
      ch = ch + `point.num` + ','
    ch = ch[:-1] + '};'
    self.Gmsh_send(ch)

    if self.md.transfinite :
      ch = (
          'Transfinite Line{' + `num` + '} = ' + 
          `self.md.number+1` + 
          ' Using Progression ' + `self.md.progression` + ';'
          )
      self.Gmsh_send(ch)

    if self.attractor :
      ch = (
        'Attractor Line{' + `num` + '} = {'
        + `self.attractor[0]`+','
        + `self.attractor[1]`+','
        + `1./self.attractor[2]` + '};'
        )
      self.Gmsh_send(ch)



def Curve(l_x,l_y,l_z=None) :

  if not l_z :
    l_z = [0.] * len(l_x)
    
  l_P = []
  for x,y,z in map(None,l_x,l_y,l_z) :
    l_P.append(Point(x,y,z))

  line = apply(Line,l_P)
  return line  
    




# -------------------- SURFACE OBJECTS ---------------------    


class Surface(Geometric) :

  """
    SURFACE OBJECT (inherit from the Geometric class)
    
    Public methods
     __init__ :  
       in -> lines : external bounday of the surface (lines should be connected)
       
     Holes : set the internal holes (surfaces)
       in -> holes : list of holes

     Boundary : checks that the boundary is a closed loop and returns the orientation of the edges

     Ruled : declare the surface is a ruled one
     
     Translate : translate the surface
       in -> tran : (numpy) vector of translation
     
     Recombine : recombine the surface (try to mesh with quadrangles instead of triangles)
     
     Transfinite : Declare the mesh to be transfinite
     
            
    Attributes
     lines : list of external boundary lines
     holes : list of internal holes (surfaces)
     ruled : indicates (false or true) if the surface is a ruled surface
     loops : list of boundary (external and internal) loops (computed when meshing)      
  """


  def __init__(self,*lines) :
  
    Geometric.__init__(self)  
    self.lines = list(lines)
    self.holes = []
    self.ruled = 0

  # Check Assumptions
    for line in lines :
      if not line.Is_line() : 
        raise "Arguments should be lines : " + repr(line)
    if lines == 0 : raise "There should be at least one line"
    self.Boundary()    


  def Boundary(self) :
  
  # checking the boundary is a loop
    orie = []
    tmp  = list(self.lines) + [self.lines[0]]
    for i in xrange(len(self.lines)) :
      lb = tmp[i]
      la = tmp[i+1]
      if lb.points[-1] in la.points :
        orie.append(1)
      elif lb.points[0] in la.points :
        orie.append(-1)
      else :
        raise "This is not a loop"
        
  # checking the boundary is closed
    if orie[0]  ==  1 : pi = self.lines[0].points[0]
    if orie[0]  == -1 : pi = self.lines[0].points[-1]
    if orie[-1] ==  1 : pf = self.lines[-1].points[-1]
    if orie[-1] == -1 : pf = self.lines[-1].points[0]
    if pi <> pf : raise "The loop is not closed"

    return orie
          

  def Holes(self,*holes) :

    for hole in holes :
      if not hole.Is_surface() :
        raise "Holes should be surfaces"
    self.holes = list(holes)
    

  def Geometric_coincide(self,obj) :

    """
      return (line_order, hole_order) :
        line_order : list of the coupled lines ith line of self with line_order[i]th line of obj
        hole_order : same as line_order but with the internal holes
    """
          
    if len(self.lines) <> len(obj.lines) :
      raise 'To coincide, surfaces should have the same number of border lines'
      
    if len(self.holes) <> len(obj.holes) :
      raise 'To coincide, surfaces should have the same number of internal holes'

   # Coincidence of the surface holes
    hole_order = []
    nb_holes = len(self.holes)
    for hole_1 in self.holes :
      for i in xrange(nb_holes) :
        if i in hole_order : 
          continue
        hole_2 = obj.holes[i]
        if hole_1.Geometric_coincide(hole_2) :
          hole_order.append(i)
          break
      else :
        return None
        
   # Coincidence of the external boundary lines
    line_order = []
    nb_lines = len(self.lines)
    for line_1 in self.lines :
      for i in xrange(nb_lines) :
        if i in line_order : 
          continue
        line_2 = obj.lines[i]
        if line_1.Geometric_coincide(line_2) :
          line_order.append(i)
          break
      else :
        return None
        
    return (line_order, hole_order)         
         

  def Deep_coincide(self,obj,info) :

    line_order = info[0]
    hole_order = info[1]
    
    for i,j in map(None,xrange(len(line_order)),line_order) :
      l1 = self.lines[i]
      l2 = obj.lines[j]
      l1.Coincide(l2)      
    
    for i,j in map(None,xrange(len(hole_order)),hole_order) :
      h1 = self.holes[i]
      h2 = obj.holes[j]
      h1.Coincide(h2)
      
    
    
  def Ruled(self) :
  
    self.ruled = 1
    
    if len(self.lines) not in [3,4] : 
      raise "Ruled surfaces require 3 or 4 edges"
      
                
  def Translate(self,tran) :
  
    l_points = []
    for surf in [self] + self.holes :
      for line in surf.lines :
        for point in line.points :
          if point not in l_points : l_points.append(point)
    
    for point in l_points :
      point.Translate(tran)
      
          
  def Recombine(self,val=1) :
  
    self.md.recombine = val
    
    
  def Transfinite(self) :
    
    self.Ruled()
     
    if len(self.lines) == 4 :
      self.Recombine()
    
    self.md.transfinite = 1
       
    for line in self.lines :
      if not line.md.transfinite :
        raise "Transfinite surfaces require transfinite edges"
        
    if (
      self.lines[0].md.number <> self.lines[2].md.number or
      self.lines[1].md.number <> self.lines[3].md.number 
    ) :
      raise "Coupled edges should have the same number of elements"
      


  def Intermediate_meshing(self,mesh) :
  
    self.loops = []
    for surf in [self]+self.holes :
      loop = LineLoop(surf)
      self.loops.append(loop)
      loop.Gmsh(mesh)  


  def Object_meshing(self,num) :
    
  # Creation of the surface
    if self.ruled :
      ch = 'Ruled Surface(' + `num` + ') = {'
    else :
      ch = 'Plane Surface(' + `num` + ') = {'
    for loop in self.loops :
      ch = ch + `loop.num` + ','
    ch = ch[:-1] + '};'
    self.Gmsh_send(ch)

  # Declaration of transfinite surface
    if self.md.transfinite :
      ch = 'Transfinite Surface {' + `num` + '} = {'
      for line, orie in map(None,self.lines,self.Boundary()) :
        if orie == 1 :
          summit = line.points[0]
        else :
          summit = line.points[1]
        ch = ch + `summit.num` + ','
      ch = ch[:-1] + '};'
      self.Gmsh_send(ch)

  # Recombine elements if requested
    if self.md.recombine :
      self.Gmsh_send('Recombine Surface {' + `num` + '} ;')
      


class LineLoop(Geometric) :    # Used only during the meshing phase


  def __init__(self,surface) :
  
    Geometric.__init__(self)
    self.surface = surface    


  def Intermediate_meshing(self,mesh) :    

    for line in self.surface.lines :
      line.Gmsh(mesh)


  def Object_meshing(self,num) :

    ch = 'Line Loop(' + `num` + ') = {'
    for line,orie in map(None,self.surface.lines,self.surface.Boundary()) :
      ch = ch + `orie*line.num` + ','
    ch = ch[:-1] +  '};'
    self.Gmsh_send(ch)    


  
# ------------------- GEOMETRICAL TRANSFORMATION --------------

def VectorProduct(u,v) :

  return array([u[1]*v[2]-u[2]*v[1],u[2]*v[0]-u[0]*v[2],u[0]*v[1]-u[1]*v[0]])
  

def VectorNorm(u) :

  return sqrt(dot(u,u))
  
    
class Rotation :

  def __init__(self,A,C,B) :
  
    self.C = c
    self.a = A-C
    n = VectorProduct(self.a,B-C)
    self.n = n / VectorNorm(n)
    
    
  def Proj(self,M) :
  
    lbd = dot(M-self.C,self.n)
    H = self.C + lbd*self.n
    return H




# -------------------- MESHING OPERATIONS ---------------------    
    
class Mesh_Descriptor :

  """
    Attributes 
      relation     Another mesh descriptor provides the mesh parameters
      parameters   dictionnary of the mesh parameters
                        size         Point size
                        transfinite  Transfinite mesh (0 or 1)
                        number       Number of elements along a line (transfinite)
                        progression  Progression of element size (transfinite)
                        recombine    Recombine mesh or not
                        
     Specific access :
       md.parameter_name = xxx -> the relation is destroyed (set to None)
       xxx = md.parameter_name -> if there is a relation, the effective
                                  parameter is looked for recursively
                                  
     Deep copying : a relation is set to the model instead of a true copy
  """
  
  List_Attr = ['size','transfinite','number','progression','recombine']

  
  def __init__(self) :

    self.relation = None
    self.parameters = {
      'size'       :  1.  ,     # Point size
      'transfinite':  0   ,     # Transfinite mesh (0 or 1)
      'recombine'  :  0         # Recombine mesh or not
      }
    
    
  def __setattr__(self, attr, value) :

    if attr in Mesh_Descriptor.List_Attr :
      self.relation = None
      self.parameters[attr] = value
      
      if attr == 'number' :
        self.transfinite = 1
        
    else :
      self.__dict__[attr] = value
    

  def __getattr__(self,attr) :
    
    if self.relation :
      return (getattr(self.relation,attr))
    else :
      if attr in self.parameters.keys() :
        return self.parameters[attr]
      else :
        raise AttributeError
    
    
  def __deepcopy__(self,visit) :
  
    md = copy.copy(self)
    md.parameters = copy.copy(self.parameters)
    md.relation = self
    return md
    
        
        
class Mesh :

  def __init__(self, algo = 2, gmsh='gmsh') :
  
    self.num_ph  = 0
    self.num     = 0
    self.command = ['Mesh.Algorithm = ' + repr(algo) + ' ;']
    self.physicals = {}
    self.gmsh    = gmsh

    
  def Physical(self, name, *l_obj) :
  
  # Checking the name
    if type(name) <> type(' ') :
      raise 'First argument should be the name of the physical'
    if name in self.physicals.keys() :
      raise 'Physical '+name+' already exists'
      
  # Checking all objects are geometric
    for obj in l_obj :
      if not Is_Geometric(obj) : 
        raise "Non geometrical object : " + repr(obj)

    cl = l_obj[0].__class__      
  # Checking all objects are of the same dimension
  #  ref_dim = l_obj[0]
  #  for obj in l_obj[1:] :
  #    if not ref_dim.Is_same_dimension(obj) :
  #      raise "All objects are not of the same dimension : " + repr(obj)
        
  # Creation of the objects if necessary
    for obj in l_obj :
      obj.Gmsh(self)
      
  # Creation of the physical
    self.num_ph= self.num_ph + 1
    ch = 'Physical ' + cl.__name__ + '(' + `self.num_ph` + ') = {'
    for obj in l_obj :
      ch = ch + `obj.num` + ','
    ch = ch[:-1] + '};'
    self.command.append(ch)
  
  # Name of the physical
    name_gmsh = 'GM'+`self.num_ph`
    self.physicals[name] = name_gmsh
        
        
  def Save(self, file = 'fort.geo') :
  
    if os.path.isfile(file) :
      os.remove(file)
      
    f = open(file,'w')
    f.write(string.joinfields(self.command,'\n'))
    f.close()

    
  def View(self) :
  
    self.Save('fort.geo')
#    os.system('gmsh fort.geo')
    os.system(self.gmsh + ' fort.geo')
    os.remove('fort.geo')
    
    
  def Create(self, file = 'fort.19') :
  
    self.Save()
#    os.system('gmsh -3 fort.geo')
    os.system(self.gmsh + ' -3 fort.geo')
    os.rename('fort.msh',file)


  def Name(self, MA, CREA_GROUP_NO) :

    l_gma = []
    l_mcf =  []
    for gma in self.physicals.keys() :
      l_gma.append(self.physicals[gma])
      l_mcf.append(_F(GROUP_MA = self.physicals[gma],NOM=gma))

    DEFI_GROUP(reuse = MA,
      MAILLAGE = MA,
      CREA_GROUP_MA = tuple(l_mcf),
      )

    SMESH_02 = CREA_MAILLAGE(
      MAILLAGE = MA,
      DETR_GROUP_MA = _F(GROUP_MA = tuple(l_gma)),
      )
    
    DETRUIRE(CONCEPT = _F(NOM = MA), INFO=1)
    
    if CREA_GROUP_NO == 'OUI' :
      DEFI_GROUP(reuse = SMESH_02,
        MAILLAGE = SMESH_02,
        CREA_GROUP_NO = _F(TOUT_GROUP_MA = 'OUI'),
        )
        
    else :
#    Traitement des GROUP_NO qui sont des points
      info_gno = SMESH_02.LIST_GROUP_NO()
      l_gno = []
      for gno in info_gno :
        if gno[1] == 1 : l_gno.append(gno[0])
        
      l_gma = []
      for gma in self.physicals.keys() :
        nom_gmsh = self.physicals[gma]
        if nom_gmsh in l_gno :
          l_gma.append(gma)

      if l_gma :        
        DEFI_GROUP(reuse = SMESH_02,
          MAILLAGE = SMESH_02,
          CREA_GROUP_NO = _F(GROUP_MA = tuple(l_gma)),
          )

    return SMESH_02


  def LIRE_GMSH(self,
    UNITE_GMSH     = 19, 
    UNITE_MAILLAGE = 20,
    MODI_QUAD      = 'NON',
    CREA_GROUP_NO  = 'OUI'
    ) :
    
    """
      Lecture du maillage (format Aster) a partir de sa definition
      (format sup_gmsh)
      UNITE_GMSH     = Numero d'unite logique pour le fichier msh
      UNITE_MAILLAGE = Numero d'unite logique pour le fichier mail
      MODI_QUAD      = 'OUI' si line->quad, 'NON' sinon
      CREA_GROUP_NO  = 'OUI' si on cree les group_no, 'NON' sinon
    """

    nom_gmsh = 'fort.' + repr(UNITE_GMSH)
    self.Create(nom_gmsh)

    PRE_GMSH(UNITE_GMSH=UNITE_GMSH, UNITE_MAILLAGE=UNITE_MAILLAGE)
    SMESH_00 = LIRE_MAILLAGE(UNITE = UNITE_MAILLAGE)
    DEFI_FICHIER(ACTION='LIBERER',UNITE = UNITE_GMSH)
    DEFI_FICHIER(ACTION='LIBERER',UNITE = UNITE_MAILLAGE)

    if MODI_QUAD == 'OUI' :
      SMESH_01 = CREA_MAILLAGE(
        MAILLAGE = SMESH_00, 
        LINE_QUAD = _F(TOUT = 'OUI')
        )  
      DETRUIRE(CONCEPT=_F(NOM=SMESH_00), INFO=1)
      SMESH_00 = SMESH_01
      
    SMESH_00 = self.Name(SMESH_00,CREA_GROUP_NO)
    
    return SMESH_00
      
