#@ MODIF B_ENTITE Build  DATE 27/03/2002   AUTEUR DURAND C.DURAND 
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

   def getmnb(self,nomcmd):
      """
         Cette methode retourne le nombre de mots cles facteurs
         de mots cles simples et de descripteurs = simples+facteurs
         sous la commande self
      """
      nbmcle=0
      nbfac=0
      for k,e in self.entites.items():
        if e.label == 'FACT' :
          nbfac=nbfac+1
          nbmcle=nbmcle+self.entites[k].get_nb_mc()
        elif e.label == 'SIMP' :
          nbmcle=nbmcle+1
        else:
          #XXX Je ne vois pas pourquoi on leve une exception. Si e est un BLOC ???
          #raise AsException("erreur sur le type d entite")
          pass
      return (nbfac,nbmcle,nbfac+nbmcle)

   def get_nb_mc(self):
      """
          Cette methode retourne le nombre total de mots cles simples
          sous l'objet self
      """
      nb=0
      for k,v in self.entites.items():
        if v.label in ("BLOC","FACT"):
           nb=nb+v.get_nb_mc()
        elif v.label in ("SIMP",):
           nb=nb+1
        else: 
           pass
      return nb

   def getmfa(self,nomcmd,ifac):
      """
          Cette methode retourne le nom du ième mot clé facteur, le nombre
          de mots cles simples sous ce mot cle facteur et le nombre total
          d'arguments du mot cle facteur (ne sert pas ici)
      """
      if nomcmd != self.nom:
        raise AsException("le nom de la commande ne correspond pas",
                            nomcmd,self.nom)
      n=0
      for k,v in self.entites.items():
        if v.label == 'FACT' :
           n=n+1
        if n == ifac:
        # on a trouve le mot cle facteur
           return(k,v.get_nb_mc(),0)
      # si on a rien trouve
      return ('',0,0)

   def getmfm(self,motfac,nbval):
      """
          Cette methode retourne la liste des mots cles simples sous 
          la commande si motfac == '' ou sous le mot cle facteur motfac
          ainsi que la liste des types associes aux mots cles simples
      """
      lmc=[]
      lty=[]
      if motfac=='':
         lmc=self.get_motcles_simples()
         lty=self.get_ty_mc()
      #XXX : tres probablement il faudrait chercher le FACT sous les BLOC
      # en utilisant get_entite. La recherche suivante est restrictive
      elif self.entites.has_key(motfac):
         v=self.entites[motfac]
         lmc=v.get_motcles_simples()
         lty=v.get_ty_mc()
      else:
         raise AsException("GETMFM : le MCFACT ",motfac," n appartient pas a la commande")
      return (lmc,lty)

   def get_motcles_simples(self):
      """
          Retourne la liste des noms des sous entites SIMP en parcourant le niveau BLOC
          mais sans parcourir les autres objets (FACT,...)
      """
      motcles=[]
      for k,v in self.entites.items():
          if  v.label == 'BLOC' :
              motcles=motcles+v.get_motcles_simples()
          elif v.label == 'SIMP' :
              motcles.append(k)
      return motcles

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


