#@ MODIF xmgrace Stanley  DATE 06/09/2004   AUTEUR MCOURTOI M.COURTOIS 
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
# =========================================================================
#                       TERMINAL GRAPHIQUE XMGRACE
# =========================================================================

# -*- coding: iso-8859-1 -*-


import os, string, glob
from popen2 import Popen3
import aster
from time import sleep

TERMINAL = 0    # terminal actif ou non (au plus un terminal en meme temps)

  
class Xmgr :

  DEJA_ACTIF = 'Terminal xmgrace deja actif'
  

  def __init__(self, gr_max = 10, options=None) :
  
  
#  Declaration
    global TERMINAL
      
#  Precondition  
    if TERMINAL : raise Xmgr.DEJA_ACTIF
    
#  Initialisation
    TERMINAL = 1
    self.gr_max = gr_max        # nombre de graphes 
    self.gr_act = 0             # numero du graphe actif
    self.sets   = [0]*gr_max    # nombre de sets par graphe
    self.nom_pipe = 'xmgr.pipe' # nom du pipe de communication
    
#  Ouverture du pipe de communication avec xmgrace
    if os.path.exists(self.nom_pipe) :
      os.remove(self.nom_pipe)
    os.mkfifo(self.nom_pipe)
    self.pipe = open(self.nom_pipe,'a+')
 
#  Lancement de xmgrace
    if options != None :
       shell = aster.repout() + '/xmgrace -noask ' + options + ' -graph ' + repr(gr_max-1) + ' -npipe ' + self.nom_pipe
    else :
       shell = aster.repout() + '/xmgrace -noask ' +           ' -graph ' + repr(gr_max-1) + ' -npipe ' + self.nom_pipe
       
    self.controle = Popen3(shell)  
    
#  Mise a l'echelle des graphes
    for i in xrange(gr_max) :
      gr = 'G'+repr(i)
      self.Send('WITH ' + gr)
      self.Send('VIEW XMIN 0.10')
      self.Send('VIEW XMAX 0.95')
      self.Send('VIEW YMIN 0.10')
      self.Send('VIEW YMAX 0.95')
      
#  Activation du graphe G0
    self.Active(0)
    
    

  def Terminal_ouvert(self) :
  
# Retourne 1 si le terminal est ouvert, 0 sinon


    etat = self.controle.poll()
    if etat == -1 :
      return(1)
    else :
      return(0)



  def Fermer(self) :

# Ferme le terminal (si necessaire)  
# Fais le menage dans l'objet

#  Declaration
    global TERMINAL
      
    if self.Terminal_ouvert() :
      self.Send('Exit')
    self.pipe.close()
    os.remove(self.nom_pipe)
    for fich in glob.glob('xmgr.*.dat') :
      os.remove(fich)
    TERMINAL = 0


    
  def Attendre(self) :
  
# Attend que l'on quitte xmgrace

  
    self.controle.wait()
    self.Fermer()


      
  def Send(self,command, echo = None) :

# Envoie une commande a l'interpreteur de xmgrace

    if self.Terminal_ouvert() :
      
      self.pipe.write(command + '\n')
      self.pipe.flush()
      if echo : print command 
    



  def Active(self, graphe) :

# Active un graphique
# IN  graphe : Numero du graphe a activer


    if graphe >= self.gr_max : raise 'Graphe inexistant'

   # On efface tous les graphes
    for i in xrange(self.gr_max) :
      gr = 'G'+repr(i) 
      self.Send(gr + ' OFF')
      
   # On active et on affiche le graphe courant
    gr = 'G' + repr(graphe)
    self.Send(gr + ' ON')
    self.Send('redraw')
    
   # On met a jour le graphe actif    
    self.gr_act = graphe


  def Nouveau_graphe(self) :
  
    """ 
      Active un nouveau graphe
      (en pratique, celui qui suit le graphe actuel)
    """
    
    gr_act = self.gr_act
    self.Active(gr_act+1)
    
    
  def Titre(self, titre, sous_titre = '') :

  
    self.Send('WITH G'+repr(self.gr_act))
    self.Send('TITLE SIZE 1.2')
    self.Send('TITLE "' + titre + '"')
    self.Send('SUBTITLE "' + sous_titre + '"')
    self.Send('redraw')


  def Axe_x(self, label) :
  
    self.Send('WITH G'+repr(self.gr_act))
    self.Send('XAXIS LABEL "' + label + '"')
    self.Send('XAXIS LABEL CHAR SIZE 0.75')
    self.Send('REDRAW')
    
    
  def Axe_y(self, label) :
  
    self.Send('WITH G'+repr(self.gr_act))
    self.Send('YAXIS LABEL "' + label + '"')
    self.Send('YAXIS LABEL CHAR SIZE 0.75')
    self.Send('REDRAW')
    
    
  def Legende(self, set, legende) :

    self.Send('WITH G'+repr(self.gr_act))
    self.Send('LEGEND ON')
    self.Send('LEGEND LOCTYPE VIEW')
    self.Send('LEGEND 0.79, 0.85')
    self.Send('LEGEND CHAR SIZE 0.75')
    self.Send('LEGEND BOX OFF')
    self.Send('LEGEND STRING ' + repr(set) + ' "' + legende + '"')
    self.Send('REDRAW')
      
  def Sortie_EPS(self,nom_fich) :

    self.Send('HARDCOPY DEVICE "EPS" ')
    self.Send('PRINT TO "' + nom_fich +'"') 
    self.Send('PRINT')  
    
  def Courbe(self, courbe, legende = None) :
  
# Trace une courbe dans le graphique actif
# IN  courbe : objet de type courbe a tracer

    
    set = self.sets[self.gr_act]
    self.sets[self.gr_act] = set+1
    
    name = 'xmgr.' + repr(self.gr_act) + '.' + repr(set) + '.dat'
    courbe.Sauve(name)
    self.Send('WITH G'+repr(self.gr_act))
    self.Send('read "' + name + '"')
    self.Send('redraw')
    
    if legende :
      self.Legende(set, legende)
      
    return set

