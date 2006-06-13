#@ MODIF N_CR Noyau  DATE 14/09/2004   AUTEUR MCOURTOI M.COURTOIS 
# -*- coding: iso-8859-1 -*-
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


""" Ce module contient la classe compte-rendu de validation
"""

import string

class CR :
   """ 
        Classe servant à la construction et à l'affichage des objets Comptes-rendus 
   """
   def __init__(self,verbeux = 'non',debut='',fin='',dec='   '):
      """
         Attributs
          - verbeux
          - debut
          - fin
          - dec
      """
      self.verbeux = verbeux
      self.debut=debut
      self.fin=fin
      self.dec=dec
      self.crok=[]
      self.crwarn=[]
      self.crfatal=[]
      self.crexception=[]
      self.subcr=[]

   def ok(self,comment):
      """ Ajoute un commentaire OK à la liste crok"""
      self.crok.append(comment)

   def warn(self,comment):
      """ Ajoute un commentaire Warning à la liste crwarn"""
      self.crwarn.append(comment)

   def fatal(self,comment):
      """ Ajoute un commentaire Erreur Fatale à la liste crfatal"""
      self.crfatal.append(comment)

   def exception(self,comment):
      """ Ajoute un commentaire Exception à la liste crexception"""
      self.crexception.append(comment)

   def add(self,cr):
      """ Ajoute un objet CR à la liste subcr :il s'agit de l'objet CR d'un fils de self """
      self.subcr.append(cr)

   def estvide(self):
      """ 
           Retourne 1 si self ne contient aucun message grave (fatal ou exception) et
                          aucun CR qui en contienne,
                    0 sinon
      """
      if self.crexception : return 0
      if self.crfatal : return 0
      for s in self.subcr :
        if not s.estvide(): return 0
      return 1

   def purge(self):
      """ 
           Purge complètement le CR sauf les exceptions
      """
      self.debut=''
      self.fin=''
      self.dec='   '
      self.crok=[]
      self.crwarn=[]
      self.crfatal=[]
      self.subcr=[]

   def beautifie_messages(self):
      """
        Beautifie les messages stockés dans crok,crfatal,crexception et crwarn
      """
      l=[]
      for mess in self.crok:
         l.append(mess+'\n')
      self.crok_belle=l
      l=[]
      for mess in self.crwarn:
         l.append(encadre_message(mess,'*'))
      self.crwarn_belle = l
      l=[]
      for mess in self.crfatal:
         l.append(encadre_message(mess,'!'))
      self.crfatal_belle = l
      l=[]
      for mess in self.crexception:
         l.append(encadre_message(mess,'!'))
      self.crexception_belle = l

   def indent(self,s):
      """
        Insère en tete de chaque ligne du texte s la chaine self.dec
      """
      l = string.split(s,'\n')
      return self.dec+string.join(l,'\n'+self.dec)[:-3]

   def __str__(self):
      """
        Retourne une chaine de caractères décorée et représentative de self
      """
      s=''
      self.beautifie_messages()
      s=s+string.join(self.crok_belle,'')
      s=s+string.join(self.crwarn_belle,'')
      s=s+string.join(self.crfatal_belle,'')
      s=s+string.join(self.crexception_belle,'')
      for subcr in self.subcr:
         if self.verbeux == 'oui':
            s=s+str(subcr)+'\n'
         else:
            if not subcr.estvide():
               s=s+str(subcr)
      if s != '':
         s=self.debut+'\n'+self.indent(s)+self.fin+'\n'
      else :
         s=self.debut+'\n'+self.fin+'\n'
      return s

   def report(self,decalage = 2):
      """
        Retourne une chaine de caractères non encadrée mais représentative de self
      """
      s=''
      # on stocke dans s les messages de premier niveau
      for mess in self.crok :
        s=s + decalage*self.dec + mess + self.dec + '\n'
      for mess in self.crwarn:
        s=s + decalage*self.dec + mess + self.dec + '\n'
      for mess in self.crfatal:
        s=s + decalage*self.dec + mess + self.dec + '\n'
      for mess in self.crexception:
        s=s + decalage*self.dec + mess + self.dec + '\n'
      # on récupère les messages des sous comptes-rendus ...
      for subcr in self.subcr:
        if not subcr.estvide():
            s=s+subcr.report(decalage = decalage + 1)
      # on rajoute les flags de début et de fin ... (si self n'est pas vide)
      if not self.estvide() :
        s = (decalage-1)*self.dec+self.debut+'\n'+s+ \
            (decalage-1)*self.dec+self.fin+'\n'
      return s

   def get_mess_fatal(self):
      """
          Retourne une chaine de caractères contenant les messages de 
          la liste crfatal (du dernier au premier)
      """
      self.crfatal.reverse()
      s=''
      for elem in self.crfatal :
        s=s+elem
      self.crfatal.reverse()
      return s

   def get_mess_exception(self):
      """
          Retourne une chaine de caractères contenant les messages 
          de la liste crexception (du dernier au premier)
      """
      self.crexception.reverse()
      s=''
      for elem in self.crexception :
        s=s+elem
      self.crexception.reverse()
      return s




def justify_text_old(texte='',cesure=50):
  """
      Prend la chaine de caractères 'texte' et la retourne avec un retour chariot
      tous les 'cesure' caractères s'il y a lieu (le retour chariot est placé dans un blanc
      et non au milieu d'un mot
  """
  texte = string.strip(texte)
  if len(texte) < cesure : return texte
  liste_lignes = string.split(texte,'\n')
  texte_justifie = ''
  for ligne in liste_lignes :
    ligne = string.strip(ligne)
    if len(ligne) <= cesure :
      texte_justifie = texte_justifie + ligne + '\n'
      continue
    longueur = 0
    new_text = ''
    liste_mots = string.split(ligne,' ')
    for mot in liste_mots :
      new_longueur = longueur + len(mot)+1
      if new_longueur < cesure :
        new_text = new_text+' '+mot
        longueur = longueur + len(mot) + 1
      else :
        longueur = 0
        new_text = new_text + '\n'+mot
    texte_justifie = texte_justifie + string.strip(new_text) + '\n'
  return texte_justifie[0:-1]

def encadre_message_old(texte,motif):
  """ 
     Retourne la chaine de caractères texte entourée d'un cadre formés
     d'éléments 'motif'
  """
  texte = justify_text(texte,cesure=80)
  lignes = string.split(texte,'\n')
  longueur = 0
  for ligne in lignes :
    if len(ligne)> longueur : longueur = len(ligne)
  longueur = longueur + 4
  txt = motif*longueur+'\n'
  for ligne in lignes :
    txt = txt + motif + ' '+ligne+' '*(longueur-len(motif+ligne)-2)+motif+'\n'
  txt = txt + motif*longueur+'\n'
  return txt




separateurs=(' ',',','/')
def split(ligne,cesure):
       ligne= string.rstrip(ligne)
       if len(ligne) <= cesure :
          return ligne
       else:
          coupure=cesure
          while ligne[coupure] not in separateurs and coupure > 0:
             coupure = coupure - 1
          if coupure == 0:
             # Il faut augmenter la cesure
             coupure =cesure
             while ligne[coupure] not in separateurs and coupure < len(ligne)-1 :
                coupure = coupure + 1
          if coupure == len(ligne)-1:
             return ligne
          else:
             return ligne[:coupure+1]+ '\n' + split(ligne[coupure+1:],cesure)

def justify_text(texte='',cesure=50):
       texte = string.strip(texte)
       liste_lignes = string.split(texte,'\n')
       l=[split(l,cesure) for l in liste_lignes]
       texte_justifie=string.join(l,'\n')
       return texte_justifie

def encadre_message(texte,motif):
  """
     Retourne la chaine de caractères texte entourée d'un cadre formés
     d'éléments 'motif'
  """
  texte = justify_text(texte,cesure=80)
  lignes = string.split(texte,'\n')
  longueur = 0
  for ligne in lignes :
    ligne=string.rstrip(ligne)
    if len(ligne)> longueur : longueur = len(ligne)
  longueur = longueur + 4
  txt = motif*longueur+'\n'
  for ligne in lignes :
    txt = txt + motif + ' '+ligne+' '*(longueur-len(motif+ligne)-2)+motif+'\n'
  txt = txt + motif*longueur+'\n'
  return txt




