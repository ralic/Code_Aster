#@ MODIF B_MACRO_ETAPE Build  DATE 26/06/2002   AUTEUR DURAND C.DURAND 
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
import string, traceback,sys
import types

# Modules EFicas
import B_ETAPE
from Noyau.N_Exception import AsException
from Noyau import N__F
from Noyau.N_utils import AsType
import B_utils

def build_formule(self):
  for mc in self.mc_liste:
    if mc.nom in ('REEL','ENTIER','COMPLEXE'):
      texte= self.sd.get_name()+ string.strip(mc.valeur)
      mc.valeur=B_utils.ReorganisationDe(texte,80)
  # ATTENTION : FORMULE est une des rares commandes qui a besoin de
  # connaitre son numero d execution avant d etre construite
  # heureusement il est facile a calculer
  self.icmd=self.jdc.icmd+1
  ier=self.codex.opsexe(self,self.icmd,-1,-self.definition.op)
  # Il faut cependant mettre self.icmd a 1 pour dire au superviseur
  # que cette commande compte pour 1 dans la numerotation
  self.icmd=1
  return ier

def build_debut(self):
  """
     Cette fonction est utilisee pour les commandes DEBUT et POURSUITE
  """
  if self.jdc.par_lot == 'NON' :
    self.jdc._Build()
  if self.definition.nom == "DEBUT":
    # Il s agit de la commande DEBUT
    # On l execute ce qui initialise les bases
    # Cette execution est indispensable avant toute autre action sur ASTER
    lot,ier=self.codex.debut(self,1)
    self.icmd=1
    return ier
  else:
    # Pour POURSUITE on ne modifie pas la valeur initialisee dans ops.POURSUITE
    # Il n y a pas besoin d executer self.codex.poursu (deja fait dans ops.POURSUITE)
    return 0


class MACRO_ETAPE(B_ETAPE.ETAPE):
   """
      Description de la classe
   """
   macros={-5:build_formule,0:build_debut}
   NOCMDOPER= (
            -1  # INCLUDE
           ,-2  # RETOUR
           ,-3  # PROCEDURE
           ,-14 # INCLUDE_MATERIAU
           )

   def __init__(self):
      pass

   def Build(self):
      """ 
          Fonction : Construction d'une étape de type MACRO
                     La construction n'est à faire que pour les macros
                     implémentées en Fortran
                     Ensuite on boucle sur les sous étapes construites
                     en leur demandant de se construire selon le meme processus
      """
      self.set_current_step()
      self.building=None
      # Chaque macro_etape doit avoir un attribut cr du type CR (compte-rendu) pour
      # stocker les erreurs eventuelles
      # et ajoute au cr de l'etape parent pour avoir un compte-rendu hierarchique
      self.cr=self.CR(debut='Etape : '+self.nom + '    ligne : '+`self.appel[0]` + '    fichier : '+`self.appel[1]`,
                       fin = 'Fin Etape : '+self.nom)

      self.parent.cr.add(self.cr)
      ret = self._Build()
      ier=ret
      if ret == 0 and self.jdc.par_lot == 'OUI':
        # Traitement par lot
        for e in self.etapes:
          if not e.isactif():continue
          ret=e.Build()
          ier=ier+ret

      if self.icmd != None:
        self.icmd=self.jdc.icmd=self.jdc.icmd+self.icmd
      self.reset_current_step()
      return ier

   def _Build(self):
      """
         Cette méthode réalise le traitement de construction pour 
         l'objet lui meme
      """
      if CONTEXT.debug : print "MACRO_ETAPE._Build ",self.nom,self.definition.op

      if self.definition.op is None:
        # On est dans le cas d'une macro en Python. On evalue la fonction self.definition.proc
        # dans le contexte des valeurs de mots clés (d)
        try:
          d=self.cree_dict_valeurs(self.mc_liste)
          ier= apply(self.definition.proc,(self,),d)
          return ier
        except AsException,e:
          ier=1
          self.cr.fatal("impossible de construire la macro "+self.nom+'\n'+str(e))
          return ier
        except EOFError:
          raise
        except:
          ier=1
          l=traceback.format_exception(sys.exc_info()[0],sys.exc_info()[1],sys.exc_info()[2])
          self.cr.fatal("impossible de construire la macro "+self.nom+'\n'+string.join(l))
          return ier

      elif self.macros.has_key(self.definition.op):
        try:
          return self.macros[self.definition.op](self)
        except AsException,e:
          raise AsException("impossible de construire la macro "+self.nom+'\n',e)
        except EOFError:
          raise
        except:
          l=traceback.format_exception(sys.exc_info()[0],sys.exc_info()[1],sys.exc_info()[2])
          raise AsException("impossible de construire la macro "+self.nom+'\n',
                                 string.join(l),
                                )
      else:
        # Pour presque toutes les commandes (sauf FORMULE et POURSUITE)
        # le numero de la commande n est pas utile en phase de construction
        # On le met donc a 0
        self.icmd=0
        ier=self.codex.opsexe(self,self.icmd,-1,-self.definition.op)
        # On indique au superviseur s il faut numeroter la commande
        # et quel increment il faut utiliser
        # En general on donne 1 ou None pour ne pas numeroter
        #  Le numero de la commande est utile en phase d execution
        if self.definition.op in self.NOCMDOPER:
          self.icmd=None
        else:
          # On demande d incrementer le compteur de la commande de 1
          self.icmd=1
        return ier

   def setmode(self,mode):
      """
         Met le mode d execution a 1 ou 2
         1 = verification par le module Fortran correspondant a la commande
         2 = execution du module Fortran
      """
      if mode in (1,2):
        for e in self.etapes:e.setmode(mode)
        self.modexec=mode

   def gcncon(self,type):
      """
          Entrees:
            type vaut soit
                    '.' : le concept sera detruit en fin de job
                    '_' : le concept ne sera pas detruit
          Sorties:
            resul  nom d'un concept delivre par le superviseur
                   Ce nom est de la forme : type // '9ijklmn' ou ijklmn est un nombre
                   incremente a chaque appel pour garantir l unicite des noms
                   Il est donc differencie tant que l'on ne depasse pas 8999999,
                   de ceux donnes par le fortran qui sont de
                   la forme : type // 'ijklmnp'
          Fonction:
            Delivrer un nom de concept non encore utilise et unique
      """
      self.jdc.nsd=self.jdc.nsd+1
      return type + "9" + string.zfill(str(self.jdc.nsd),6)

   def DeclareOut(self,nom,concept):
      """ 
          Methode utilisee dans une macro lors de la construction
          de cette macro en Python (par opposition a une construction en Fortran).
          Elle a pour but de specifier le mapping entre un nom de concept local
          a la macro (nom) et le concept de sortie effectif qui existe deja
          au moment de la construction
          Cette information sera utilisee lors de la creation du concept produit
          par une sous commande créée postérieurement.
      """
      self.Outputs[nom]=concept

   def smcdel(self,iold,inew):
      """
          Entrees:
            iold numero initial de la command
            inew nouveau numero de la commande
          Sorties:
            ier code retour erreur (0 pas d erreur)
          Fonction:
            Detruit la commande numero iold si inew < iold
            sinon la deplace
      """
      if CONTEXT.debug : print "smcdel: ",iold,inew
      ier=0
      return ier

   def smdcmd(self,jcmd,result,comman):
      """
          Entrees:
            jcmd numero d ordre de la commande en cours
            result nom du resultat produit par la commande
                   Si ce nom commence par &, il s'agit d'un concept reutilise
            comman nom de la commande a debuter
          Sorties:
            ier code retour d erreur
          Fonction:
            Demarrer la constitution d une commande apres la position jcmd
      """
      if CONTEXT.debug : print "SMDCMD : ",comman,result

      comman=string.strip(comman)
      result=string.strip(result)
      ier=0
      cmd=self.get_cmd(comman)
      etape=cmd.make_objet(mc_list='non')
      # on n a pas besoin d ajouter l etape dans self.etapes car etape
      # s enregistre elle meme aupres de l etape courante cad self
      if CONTEXT.debug : print "SMDCMD : ",etape
      # On ne peut pas creer le concept produit tant que les mots cles
      #    ne sont pas renseignés et valides
      # On se contente de memoriser le nom du concept produit et l etape en construction
      etape.sdnom=result
      if result and result[0] == '&' :
        # On est en presence d'un concept reutilise,
        # on le recherche dans les etapes precedentes
        etape.reuse = self.get_sd_avant_etape(result[1:],etape)
      self.building=etape
      self.dic_cur=etape.valeur
      return ier

   def putvid(self,motcle,nbval,kval):
      """
          Entrees:
            motcle nom du mot cle
            nbval nombre de concepts (ne sert pas, la longueur de
                  la liste kval est connue dans Python)
            kval liste des concepts
          Sorties:
            ierusr code retour d erreur
          Fonction:
            Rentrer une liste d argument de type identificateur (concept)
      """
      if CONTEXT.debug : print "putvid ",motcle,kval

      ierusr=0
      if type(kval) == types.TupleType:
        t=()
        for no in kval:
          sd=self.get_sd_avant_etape(no,self.building)
          if sd:t=t+(sd,)
          else :
            # Si le concept n'existe pas on le traite comme un nom
            t=t+(no,)
      else:
        t=None
        sd=self.get_sd_avant_etape(kval,self.building)
        if sd:t=sd
        else:
          # Si le concept n'existe pas on le traite comme un nom
          t=kval
      self.dic_cur[motcle]=t
      return ierusr

   def get_sd_avant_etape(self,nom,etape):
      """ 
          Retourne le concept de nom nom defini avant l etape etape
      """
      sd=self.parent.get_sd_avant_etape(nom,self)
      if not sd:
         d=self.get_contexte_avant(etape)
         sd= d.get(nom,None)
      return sd

   def putvis(self,motcle,nbval,ival):
      """
          Entrees:
            motcle nom du mot cle
            nbval nombre d entiers
            ival liste des entiers
          Sorties:
            ierusr code retour d erreur
          Fonction:
            Rentrer une liste d argument de type  entier
      """
      if CONTEXT.debug : print "putvis ",motcle,ival

      ierusr=0
      try:
        self.dic_cur[motcle]=ival
      except:
        ierusr=1
      return ierusr

   def putvtx(self,motcle,nbval,kval,ival):
      """
          Entrees:
            motcle nom du mot cle
            nbval nombre de texte (ne sert pas ici)
            kval liste des textes
            ival longueur des textes (ne sert pas ici)
          Sorties:
            ierusr code retour d erreur
          Fonction:
            Rentrer une liste d argument de type texte
      """
      if CONTEXT.debug : print "putvtx ",motcle,kval

      ierusr=0
      try:
        self.dic_cur[motcle]=kval
      except:
        ierusr=1
      return ierusr

   def smdmcf(self,motf):
      """
          Entrees:
            motf mot cle facteur a debuter
          Sorties:
            ier code retour d erreur
          Fonction:
            Demarrer la constitution du mot cle facteur motf
      """
      if CONTEXT.debug : print "SMDMCF : ",motf

      ier=0
      motf=string.strip(motf)
      motfac=N__F._F()
      if self.dic_cur.has_key(motf): 
        # un mot cle facteur de nom motf existe deja
        self.dic_cur[motf].append(motfac)
      else:
        self.dic_cur[motf]=[motfac]
      # On change de dictionnaire courant. On utilise le mot cle facteur cree
      self.dic_cur=motfac
      return ier

   def putvr8(self,motcle,nbval,rval):
      """
          Entrees:
            motcle nom du mot cle
            nbval nombre de flottants (ne sert pas ici)
            rval liste des flottants
          Sorties:
            ierusr code retour d erreur
          Fonction:
            Rentrer une liste d argument de type flottant
      """
      if CONTEXT.debug : print "putvr8 ",motcle,rval

      ierusr=0
      try:
        self.dic_cur[motcle]=rval
      except:
        ierusr=1
      return ierusr

   def smfmcf(self):
      """
          Entrees: aucune
          Sorties:
            - ier code retour d erreur
          Fonction:
            Clore la constitution du mot cle facteur en cours
      """
      if CONTEXT.debug : print "SMFMCF : "

      ier=0
      # On remet le dictionnaire de l etape comme dictionnaire courant
      self.dic_cur=self.building.valeur
      return ier

   def smfcmd(self):
      """
          Fonction:
                Construire et finaliser la commande en cours de constitution
          Sorties:
            ier code retour d erreur (0 si tout est OK)
      """
      if CONTEXT.debug : print "SMFCMD : "
      ier=0
      try:
         # On construit la structure de données interne de l'étape
         # C'est maintenant possible car on connait tous les mots clés
         self.building.McBuild()
         if hasattr(self.building.definition,'sd_prod'):
            # On est en presence d'une etape susceptible de produire des concepts
            # Si le concept produit existe déjà, on ne le crée pas. On utilise celui qui existe.
            # Si le concept n'existe pas dans la macro mais est réutilisé, on ne le crée pas. 
            #                   On utilise le concept reutilisé
            # Si il n'existe pas dans la macro et n'est pas réutilisé, on le crée.
            sd= self.get_sdprod_byname(self.building.sdnom)
            if sd != None:
               # Le concept préexiste dans les concepts produits de la macro self
               # On ne le crée pas, on attache le concept à l'étape
               self.building.sd=sd
               sd.etape=self.building
            elif self.building.sdnom and self.building.sdnom[0] == '&':
               # Le concept est réutilisé. On ne fait rien car le traitement a été fait dans smdcmd
               # On se contente de l'utiliser. On ne change pas son rattachement
               self.building.sd=self.building.reuse
               self.building.sdnom=self.building.sdnom[1:]
            else:
               # Le concept n'existe pas, on le crée et on demande le nommage au parent
               sd= self.building.get_sd_prod()
               self.parent.NommerSdprod(sd,self.building.sdnom)

      except:
         if CONTEXT.debug : traceback.print_exc()
         ier=1

      if self.jdc.par_lot == 'NON' and ier == 0:
         # En mode commande par commande on execute immediatement la commande construite
         self.building.Execute()

      self.building=None
      self.dic_cur=None
      return ier

   def get_sdprod_byname(self,name):
      """
          Fonction : Retourne le concept produit de la macro self dont le nom est name
                     Si aucun concept produit n'a le nom name, retourne None
      """
      sd=None
      if self.sd and self.sd.nom == name :
         sd= self.sd
      else :
         for sdprod in self.sdprods:
            if sdprod.nom == name :
               sd= sdprod
               break
      return sd




