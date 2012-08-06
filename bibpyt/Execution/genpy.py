#@ MODIF genpy Execution  DATE 06/08/2012   AUTEUR CUVILLIE M.CUVILLIEZ 
# -*- coding: iso-8859-1 -*-
# RESPONSABLE COURTOIS M.COURTOIS
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
   Ce module contient la classe VISITEUR genpy
   Cette classe parcourt l'arborescence d'une ETAPE et produit
   une chaine de caracteres representative de cette commande
"""
# Modules Python
import sys

# Modules Eficas
from Noyau.N_ASSD import ASSD
from Noyau.N_types import is_float, is_enum
import E_utils


class genpy:
  def __init__(self,defaut='sans',simp='tous',indent=2):
    self._defaut=defaut
    self._simp=simp
    self.l=[]
    self.l_jdc=[]
    self.indent=[indent]
    self.width=50
    self.indent_courant=self.indent[-1]

  def visitJDC(self,node):
    for e in node.etapes:
      self.l=[]
      e.accept(self)
#      self.l_jdc.append(self.l,self.args)
      self.l_jdc.append(self.l)

  def visitETAPE_NIVEAU(self,node):
    if node.etapes_niveaux == []:
      for etape in node.etapes:
        self.l=[]
        etape.accept(self)
#        self.l_jdc.append(self.l,self.args)
        self.l_jdc.append(self.l)
    else:
      for etape in node.etapes_niveaux:
        self.l=[]
        etape.accept(self)
#        self.l_jdc.append(self.l,self.args)
        self.l_jdc.append(self.l)

  def visitASSD(self,node):
    self.sdname=node.get_name()

  def visitPROC_ETAPE(self,node):
    label=node.definition.nom+'('
    self.l.append(label)
    args={}
    for v in node.mc_liste:
      v.accept(self)
      if v.isBLOC() :
        args.update(self._result)
      else:
        args[v.nom]=self._result
    if self._defaut == 'avec':
      # on complete avec les valeurs par defaut
      self.AjouteDefaut(node,args)
    self.args=args

  def visitMACRO_ETAPE(self,node):
    try:
      if node.sd != None:
        node.sd.accept(self)
        label=self.sdname + '='+node.definition.nom+'('
      else:
        label=node.definition.nom+'('
        self.sdname=''
    except:
      self.sdname='sansnom'
      label=self.sdname + '='+node.definition.nom+'('
    self.l.append(label)
    if node.reuse != None:
      self.l.append('reuse = ' + self.sdname)
      self.l.append(',')
    args={}
    for v in node.mc_liste:
      v.accept(self)
      if v.isBLOC() :
        args.update(self._result)
      else:
        args[v.nom]=self._result
    if self._defaut == 'avec':
      # on complete avec les valeurs par defaut
      self.AjouteDefaut(node,args)
    self.args=args

  def visitETAPE(self,node):
    try:
      node.sd.accept(self)
    except:
      self.sdname='sansnom'
    label=self.sdname + '='+node.definition.nom+'('
    self.l.append(label)
    if node.reuse != None:
      self.l.append('reuse = ' + self.sdname)
      self.l.append(',')
    args={}
    for v in node.mc_liste:
      v.accept(self)
      if v.isBLOC() :
        args.update(self._result)
      else:
        args[v.nom]=self._result
    if self._defaut == 'avec':
      # on complete avec les valeurs par defaut
      self.AjouteDefaut(node,args)
    self.args=args

  def AjouteDefaut(self,node,args):
    for k,v in node.definition.entites.items():
      if args.has_key(k):continue
      # On ajoute les defauts pour les mots cles simples et facteurs
      # Ce n'est pas necessaire pour les blocs qui sont tous crees
      # a la construction meme ceux conditionnes par des mots cles
      # simples non presents avec defaut
##      pour ne pas imprimer les mots clés cachés dans le fichier code
##      if v.label == 'SIMP' and v.statut != 'c' :
      if v.label == 'SIMP' :
        # Mot cle simple
        if v.defaut != None : 
           if v.statut != 'c' : args[k]=self.evalMCSIMP(v.defaut)
      elif v.label == 'FACT' :
        if v.statut in ('d',) :
          # On cree un objet MCFACT avec valeur par defaut
          # On ne se preoccupe pas de la creation des sous blocs
          # conditionnels car ils sont crees automatiquement
          # meme ceux dependant de mots cles simples non presents
          # avec defaut
          mcf=v(val=None,nom=k,parent=node)
          # on parcourt l'objet
          mcf.accept(self)
          args[k]=self._result
          # On appelle supprime pour eliminer toute reference arriere
          # sur mcf en particulier parent
          mcf.supprime()

  def evalMCSIMP(self,object):
    if type(object) is tuple:
      st = '('
      for val in object :
        if is_float(val):
          st = st + E_utils.repr_float(val)
        else :
          st = st + `val`
        st = st +','
      st = st + ')'
    elif is_float(object):
      st = E_utils.repr_float(object)
    else :
      st=`object`
    return st

  def visitMCSIMP(self,node):
    if is_enum(node.valeur):
      st = ['(',]
      # Si la liste est trop longue, on ne l'imprime pas completement
      listVal=node.valeur
      trail=')'

      for val in listVal :
        if isinstance(val, ASSD):
          val.accept(self)
          if hasattr(node.etape,'sdprods') and val in node.etape.sdprods:
            st.append("CO('"+ self.sdname+ "')")
          else:
            st.append(self.sdname)
        elif is_float(val):
          st.append(E_utils.repr_float(val))
        else :
          st.append(`val`)
        st.append(',')
      st.append(trail)
      st=''.join(st)

    elif isinstance(node.valeur, ASSD):
      node.valeur.accept(self)
      if hasattr(node.etape,'sdprods') and node.valeur in node.etape.sdprods:
        st="CO('"+self.sdname+"')"
      else:
        st = self.sdname
    elif is_float(node.valeur):
      st = E_utils.repr_float(node.valeur)
    elif node.definition.type[0] == 'shell':
      # Texte FORMULE
      # Le texte initial de la formule est stocke dans node.val
      # Celui stocke dans node.valeur a ete reformate sans
      # saut de ligne et justifie a 80 colonnes
      # On utilise donc node.val pour ce traitement
      st='"""'+node.val+'"""'
    else :
      st=`node.valeur`

    if (node.definition.into==None and self._simp=='into') :
      self._result=''
    else :
      self._result=st

  def visitMCList(self,node):
    l=[]
    for data in node.data:
      data.accept(self)
      l.append(self._result)
    if len(l)==1 : self._result=l[0]
    else         : self._result=l

  def visitMCFACT(self,node):
    fact={}
    for v in node.mc_liste:
      v.accept(self)
      if v.isBLOC() :
        fact.update(self._result)
      else:
        fact[v.nom]=self._result
    if self._defaut == 'avec':
      # on complete avec les valeurs par defaut
      self.AjouteDefaut(node,fact)
    self._result=fact

  def visitMCBLOC(self,node):
    bloc={}
    for v in node.mc_liste:
      v.accept(self)
      if v.isBLOC() :
        bloc.update(self._result)
      else:
        bloc[v.nom]=self._result
    if self._defaut == 'avec':
      # on complete avec les valeurs par defaut
      self.AjouteDefaut(node,bloc)
    self._result=bloc

  def formate_etape(self):
    self.setap=''
    self.line=' '*self.indent_courant
    self.indent.append(len(self.l[0])+len(self.line))
    self.indent_courant=self.indent[-1]
    for v in self.l:
      if v == ',':
        self.setap=self.setap+self.line+',\n'
        self.line=' '*self.indent_courant
      else:
        self.line=self.line+v
    for k,v in self.args.items():
      if not v:
          continue
      self.line=self.line+k+'='
      self.__format(v)
      self.setap=self.setap+self.line+',\n'
      self.line=' '*self.indent_courant
    self.setap=self.setap+self.line+');\n'
    return self.setap

  def __format(self,object):
    if type(object) is list:
      #MCList
      self.line=self.line+'('
      self.indent.append(len(self.line))
      self.indent_courant=len(self.line)
      sep=''
      for e in object:
        if sep == ',':
          self.setap=self.setap+self.line+',\n'
          self.line=' '*self.indent_courant
        self.__format(e)
        sep=','
      self.line=self.line+')'
      del self.indent[-1]
      self.indent_courant=self.indent[-1]
    elif type(object) is dict:
      #MCFACT
      self.line=self.line+'_F('
      self.indent.append(len(self.line))
      self.indent_courant=len(self.line)
      sep=''
      for k,v in object.items():
        if sep == ',':
          self.setap=self.setap+self.line+',\n'
          self.line=' '*self.indent_courant
        self.line=self.line+k+'='
        self.__format(v)
        sep=','
      self.line=self.line+')'
      del self.indent[-1]
      self.indent_courant=self.indent[-1]
    else:
      #MCSIMP
      if '\n' in object:
        self.line=self.line+object.replace('\n','\n'+' '*self.indent_courant)
      else:
        self.line=self.line+object

