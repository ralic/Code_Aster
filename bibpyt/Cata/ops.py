#@ MODIF ops Cata  DATE 23/10/2002   AUTEUR DURAND C.DURAND 
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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

# Modules Python
import types
import string,linecache,os,traceback,re

# Modules Eficas
import Accas
from Accas import ASSD

try:
   import aster
   # Si le module aster est présent, on le connecte
   # au JDC
   import Build.B_CODE
   Build.B_CODE.CODE.codex=aster
except:
   pass

def DEBUT(self,PAR_LOT,**args):
   """
       Fonction sdprod de la macro DEBUT
   """
   self.jdc.set_par_lot(PAR_LOT)

def POURSUITE(self,PAR_LOT,**args):
   """
       Fonction sdprod de la macro POURSUITE
   """
   self.jdc.set_par_lot(PAR_LOT)
   if self.codex and os.path.isfile("glob.1"):
     # Le module d'execution est accessible et glob.1 est present
     if hasattr(self,'fichier_init'):return
     self.fichier_init='glob.1'
     self.jdc.initexec()
     lot,ier,lonuti,concepts=self.codex.poursu(self,1)
     self.icmd=lonuti
     #print "Fin de debut",ier,lot,lonuti
     pos=0
     d={}
     while pos+80 < len(concepts)+1:
       nomres=concepts[pos:pos+8]
       concep=concepts[pos+8:pos+24]
       nomcmd=concepts[pos+24:pos+40]
       statut=concepts[pos+40:pos+48]
       if nomres[0] not in (' ','.','&') and statut != '&DETRUIT':
          exec nomres+'='+string.lower(concep)+'()' in self.parent.g_context,d
       pos=pos+80
     for k,v in d.items():
       self.parent.NommerSdprod(v,k)
     self.g_context=d
     return
   else:
     # Si le module d'execution n est pas accessible ou glob.1 absent on 
     # demande un fichier (EFICAS)
     # Il faut éviter de réinterpréter le fichier à chaque appel de
     # POURSUITE
     if hasattr(self,'fichier_init'):
        return
     self.make_poursuite()

def POURSUITE_context(self,d):
   """
       Fonction op_init de la macro POURSUITE
   """
   # self représente la macro POURSUITE ...
   d.update(self.g_context)
   # Une commande POURSUITE n'est possible qu'au niveau le plus haut
   # On ajoute directement les concepts dans le contexte du jdc
   # XXX est ce que les concepts ne sont pas ajoutés plusieurs fois ??
   for v in self.g_context.values():
      if isinstance(v,ASSD) : self.jdc.sds.append(v)

def INCLUDE(self,UNITE,**args):
   """ 
       Fonction sd_prod pour la macro INCLUDE
   """
   if not UNITE : return
   if hasattr(self,'unite'):return
   self.unite=UNITE

   if self.jdc and self.jdc.par_lot == 'NON':
      # On est en mode commande par commande
      # On teste la validite de la commande avec interruption eventuelle
      cr=self.report()
      self.parent.cr.add(cr)
      if not cr.estvide():
         raise EOFError

   self.make_include(unite=UNITE)

def INCLUDE_context(self,d):
   """ 
       Fonction op_init pour macro INCLUDE
   """
   for k,v in self.g_context.items():
      d[k]=v

def detruire(self,d):
   """
       Cette fonction est la fonction op_init de la PROC DETRUIRE
   """
   sd=[]
   for mc in self["CONCEPT"]:
     mcs=mc["NOM"]
     if type(mcs) == types.ListType or type(mcs) == types.TupleType:
       for e in mcs:
         if isinstance(e,ASSD):
           sd.append(e)
           e=e.nom
         if d.has_key(e):del d[e]
         if self.jdc.sds_dict.has_key(e):del self.jdc.sds_dict[e]
     else:
       if isinstance(mcs,ASSD):
         sd.append(mcs)
         mcs=mcs.nom
       if d.has_key(mcs):del d[mcs]
       if self.jdc.sds_dict.has_key(mcs):del self.jdc.sds_dict[mcs]
   for s in sd:
     # On signale au parent que le concept s n'existe plus apres l'étape self
     self.parent.delete_concept_after_etape(self,s)

def subst_materiau(text,NOM_MATER,EXTRACTION):
   """
       Cette fonction retourne un texte obtenu à partir du texte passé en argument (text)
       en substituant le nom du materiau par NOM_MATER 
       et en réalisant les extractions spéciifées dans EXTRACTION
   """
   lines=string.split(text,'\n')
   if EXTRACTION:
     ll=[]
     regmcf=re.compile(r" *(.*) *= *_F\( *## +(.*) +(.*)")
     regmcs=re.compile(r" *(.*) *= *([^ ,]*) *, *## +([^ ]*) *([^ ]*)")
     regfin=re.compile(r" *\) *")
     temps={};lmcf=[]
     for e in EXTRACTION:
       mcf=e['COMPOR']
       lmcf.append(mcf)
       temps[mcf]=e['TEMP_EVAL']
     FLAG=0
     for l in lines:
       m=regmcf.match(l)
       if m: # On a trouve un mot cle facteur "commentarise"
         if m.group(2) == "SUBST": # il est de plus substituable
           if temps.has_key(m.group(3)): # Il est a substituer
             ll.append(" "+m.group(3)+"=_F(")
             mcf=m.group(3)
             TEMP=temps[mcf]
             FLAG=1 # Indique que l'on est en cours de substitution
           else: # Il n est pas a substituer car il n est pas dans la liste demandee
             ll.append(l)
         else: # Mot cle facteur commentarise non substituable
           ll.append(l)
       else:  # La ligne ne contient pas un mot cle facteur commentarise
         if FLAG == 0: # On n est pas en cours de substitution
           ll.append(l)
         else: # On est en cours de substitution. On cherche les mots cles simples commentarises
           m=regmcs.match(l)
           if m: # On a trouve un mot cle simple commentarise
             if m.group(3) == "EVAL":
               ll.append("  "+m.group(1)+' = EVAL("'+m.group(4)+"("+str(TEMP)+')"),')
             elif m.group(3) == "SUPPR":
               pass
             else:
               ll.append(l)
           else: # On cherche la fin du mot cle facteur en cours de substitution
             m=regfin.match(l)
             if m: # On l a trouve. On le supprime de la liste
               FLAG=0
               del temps[mcf]
             ll.append(l)
   else:
     ll=lines

   for l in ll:
     print l
   lines=ll
   ll=[]
   for l in lines:
     l=re.sub(" *MAT *= *",NOM_MATER+" = ",l,1)
     ll.append(l)
   text=string.join(ll,'\n')
   return text

def INCLUDE_MATERIAU(self,NOM_AFNOR,TYPE_MODELE,VARIANTE,TYPE_VALE,NOM_MATER,
                    EXTRACTION,INFO,**args):
  """ 
      Fonction sd_prod pour la macro INCLUDE_MATERIAU
  """
  mat=string.join((NOM_AFNOR,'_',TYPE_MODELE,'_',VARIANTE,'.',TYPE_VALE),'')
  if not hasattr(self,'mat') or self.mat != mat or self.nom_mater != NOM_MATER :
    # On récupère le répertoire des matériaux dans les arguments 
    # supplémentaires du JDC
    rep_mat=self.jdc.args.get("rep_mat","NOrep_mat")
    f=os.path.join(rep_mat,mat)
    self.mat=mat
    self.nom_mater=NOM_MATER
    if not os.path.isfile(f):
       del self.mat
       self.make_contexte(f,"#Texte sans effet pour reinitialiser le contexte a vide\n")
       raise "Erreur sur le fichier materiau: "+f
    # Les materiaux sont uniquement disponibles en syntaxe Python
    # On lit le fichier et on supprime les éventuels \r
    text=string.replace(open(f).read(),'\r\n','\n')
    # On effectue les substitutions necessaires
    self.prefix=NOM_MATER
    self.text= subst_materiau(text,NOM_MATER,EXTRACTION)
    if INFO == 2:
      print "INCLUDE_MATERIAU: ", self.mat,' ',NOM_MATER,'\n'
      print self.text
    # on execute le texte fourni dans le contexte forme par
    # le contexte de l etape pere (global au sens Python)
    # et le contexte de l etape (local au sens Python)
    # Il faut auparavant l'enregistrer aupres du module linecache (utile pour nommage.py)
    linecache.cache[f]=0,0,string.split(self.text,'\n'),f
    if self.jdc.par_lot == 'NON':
      # On est en mode commande par commande
      # On teste la validite de la commande avec interruption eventuelle
      cr=self.report()
      self.parent.cr.add(cr)
      if not cr.estvide():
        raise EOFError
      # Et en plus il faut executer la fonction ops014 avant les sous
      # commandes car le prefixe PRFXCO doit etre initialise dans le Fortran
      self.codex.opsexe(self,0,-1,-self.definition.op)  

    self.make_contexte(f,self.text)

