#@ MODIF B_OBJECT Build  DATE 17/08/2004   AUTEUR DURAND C.DURAND 
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
import string,types

# Modules Eficas
from Noyau.N_MCSIMP import MCSIMP
from Noyau.N_MCFACT import MCFACT
from Noyau.N_MCBLOC import MCBLOC
from Noyau.N_MCLIST import MCList
from Noyau.N_Exception import AsException

class OBJECT:
   """
   """
   def getfac( self , nom_motfac ) :
      """
          Method : MCBLOC.getfac
          Auteur : Antoine Yessayan
          Intention : Rechercher le nombre d'occurences d'un mot-cle de nom
                      nom_motcle parmi les fils d'un MCBLOC.
                      Cette methode est appelee par
                      EXECUTION.getfac (commandes.py)
      """
      nomfac=string.strip(nom_motfac)
      taille=0
      # On cherche d'abord dans les mots cles presents a l'exclusion des BLOCs
      for child in self.mc_liste:
          if child.nom == nomfac :
             if isinstance(child,MCFACT) :
                taille=1
                return taille
             elif isinstance(child,MCList) :
                taille=len(child.data)
                return taille
             else :
                raise AsException( "incoherence de type dans getfac" )

      # Ensuite on cherche dans les eventuels defauts
      # Recherche dans la definition de l'existence d'une occurrence du mot-cle de nom
      # nomfac.
      if taille == 0 :
          assert(hasattr(self,'definition'))
          assert(hasattr(self.definition,'entites'))
          if self.definition.entites.has_key(nomfac) :
                  assert(type(self.definition.entites[nomfac]) == types.InstanceType)
                  assert(hasattr(self.definition.entites[nomfac],'statut'))
                  if self.definition.entites[nomfac].statut == 'd' :
                          taille=1
                          return taille

      # Enfin on explore les BLOCs
      for child in self.mc_liste:
          if isinstance(child,MCBLOC) :
             taille= child.getfac( nom_motfac )
             if taille : break
      return taille

   def getlfact(self):
      """
          Retourne :
            la liste des noms de mots cles facteur sous l etape
      """
      liste=[]
      for child in self.mc_liste :
        if isinstance(child[0],MCFACT) :
          liste.append(child[0].nom)
        elif isinstance(child,MCBLOC) :
          liste= liste+child.getlfact()
      return liste

   def getlsimp(self):
      """
          Retourne :
            la liste des noms de mots cles simples sous self
      """
      liste=[]
      for child in self.mc_liste :
        if isinstance(child,MCSIMP) :
          liste.append(child)
        elif isinstance(child,MCBLOC) :
          liste= liste+child.getlsimp()
      return liste
