#@ MODIF B_ENTITE Build  DATE 06/10/2003   AUTEUR DURAND C.DURAND 
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
#                                                                       
#                                                                       
# ======================================================================
"""

"""
# Modules Python

# Modules Eficas
import B_utils

class ENTITE:
   """

   """
   def get_entite(self,nom,typ=None):
      """ 
          Cette méthode retourne la sous entite de nom nom et de type typ
          Les blocs sont explorés recursivement mais pas les mots cles facteurs
          Si typ == None on ne vérifie pas le type. Sinon, on ne retourne la sous entité
          que si elle est du bon type.
          Si aucune sous entite ne satisfait les criteres la methode retourne None
      """
      for k,v in self.entites.items():
        if k == nom:
          if typ == None  :
             return v
          elif isinstance(v,typ):
             return v
        if v.label == 'BLOC':
          o=v.get_entite(nom,typ)
          if o:return o
      return None

   def get_nb_mcs(self):
      """
          Cette methode retourne le nombre total de mots cles simples
          sous l'objet self
      """
      nb=0
      for k,v in self.entites.items():
        if v.label in ("BLOC","FACT"):
           nb=nb+v.get_nb_mcs()
        elif v.label in ("SIMP",):
           nb=nb+1
        else: 
           pass
      return nb

   def getmfm(self,motfac,nbval):
      """
          Cette methode retourne la liste des mots cles simples sous 
          la commande si motfac == '' ou sous le mot cle facteur motfac
          ainsi que la liste des types associes aux mots cles simples
      """
      mcf=self.get_entite(motfac)
      lmc=[]
      lty=[]
      if (mcf):
         lmc=mcf.get_mc_simp()
         lty=mcf.get_ty_mc()
      else:
         lmc=self.get_mc_simp()
         lty=self.get_ty_mc()
      return (lmc,lty)

   def get_ty_mc(self):
      """
          Cette methode retourne la liste des types de concept attendus derriere
          un mot cle simple 
          self peut etre une commande ou un mot cle facteur
      """
      l=[]
      for k,v in self.entites.items():
        if  v.label == 'BLOC' :
          l=l+v.get_ty_mc()
        elif v.label == 'SIMP' :
          l.append(B_utils.Typast(v.type))
      return l

   def get_mc_simp(self,niv=1):
      """
          Cette methode retourne la liste des mots cles simple sous self
          Ne franchit pas le niveau des MCFACTS
      """
      motcles=[]
      for k,v in self.entites.items():
          if  v.label == 'BLOC' :
              motcles=motcles+v.get_mc_simp()
          elif v.label == 'SIMP' :
              motcles.append(k)
          elif v.label == 'FACT' :
###    on ne veut conserver que les mcsimp de haut niveau
              if   niv==1 : pass
###    on veut "eliminer" les mcfacts pour avoir tous les mcsimp
              elif niv==2 : motcles=motcles+v.get_mc_simp()
      return motcles

   def get_mc_fact(self):
      """
          Cette methode retourne la liste des noms de mots cles facteurs
          sous self
      """
      motcles=[]
      for k,v in self.entites.items():
          if  v.label == 'BLOC' :
              motcles=motcles+v.get_mc_fact()
          elif v.label == 'SIMP' :
              pass
          elif v.label == 'FACT' :
              motcles.append(k)
      return motcles

   def get_li_mc_fact(self):
      """
          Cette methode retourne la liste des mots cles facteurs sous self
      """
      motcles=[]
      for k,v in self.entites.items():
          if  v.label == 'BLOC' :
              motcles=motcles+v.get_li_mc_fact()
          elif v.label == 'SIMP' :
              pass
          elif v.label == 'FACT' :
              motcles.append(v)
      return motcles

   def getmcfs(self,nom_motfac):
      """ 
          Retourne la liste des mots cles facteurs de nom nom_motfac
          contenus dans la definition self
          Tous les mots cles facteurs contenus dans tous les blocs
          sont mis dans cette liste

          Si la definition ne comporte aucun mot cle facteur de nom
          nom_motfac, on retourne une liste vide []
      """
      l=[]
      for k,v in self.entites.items():
        # Si l'entite a pour nom nom_motfac et est de type FACT, on l'ajoute a la liste
        if k == nom_motfac and v.label == 'FACT':
           l.append(v)
        if v.label == 'BLOC':
           # S'il s'agit d'un bloc, on lui demande de retourner la liste des mots cles facteurs
           # qu'il contient. (On peut avoir plusieurs sous blocs) et on l'ajoute a la liste globale
           o=v.getmcfs(nom_motfac)
           if o:l=l+o
      return l

   def getmat(self):
      """
      Retourne :
        le nombre de mots cles facteur sous la commande, y compris en eliminant les blocs
        la liste de leur noms
        Attention : les doublons sont elimines
      """
      if CONTEXT.debug : prbanner("getmat")
      assert(hasattr(self,'nom')),"self n'a pas d'attribut nom PB"
      liste_nom=self.get_mc_fact()
      for child in liste_nom:
          while liste_nom.count(child)>1 : liste_nom.remove(child)
      return (liste_nom,)

   def getfns(self,nom_motfac):
      """
         Retourne le nombre max de mots cles simples sous les mots cles facteurs 
         portant le nom nomfac
           nom_motfac   : nom du mot cle facteur
           Retour:
           nbmcs        : nombre max de mcsimp sous les mcfacts de nom nom_motfac
      """
      if CONTEXT.debug : prbanner("getfns")
      assert(hasattr(self,'nom')),"self n'a pas d'attribut nom PB"
      liste=self.get_li_mc_fact()
      nbmcs=0
      for child in liste :
          if child.nom==nom_motfac:
             i=len(child.get_mc_simp())
             if i>nbmcs : nbmcs=i
      return nbmcs

