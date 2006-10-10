#@ MODIF lire_table_ops Macro  DATE 10/10/2006   AUTEUR MCOURTOI M.COURTOIS 
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

import os
import re

# ------------------------------------------------------------------------------
def msplit(chaine, separ):
   """Equivalent de chaine.split(separ) en acceptant une ou plusieurs
   occurrences du séparateur.
   """
   return re.split('%s+' % re.escape(separ), chaine.strip(separ))

# ------------------------------------------------------------------------------
def lecture_table(texte, nume, separ):
   """Méthode de construction de l'objet Table à partir d'un texte d'une table
   au format ASTER.
   """
   from Utilitai.transpose import transpose
   from Utilitai.Table     import Table
   
   tab_lue = {}
   nume_lign = []
   idt_deb = '#DEBUT_TABLE\n'
   idt_fin = '#FIN_TABLE\n'
   idt_tit = '#TITRE'
   id_vide = '-'
   
   # expression régulière pour découper les N tables du fichier
   exp = re.compile(re.escape(idt_deb) + '(.*?)' + re.escape(idt_fin),
                    re.MULTILINE | re.DOTALL)
   l_txt = exp.findall(texte)
   nbbloc = len(l_txt)
   if nume > nbbloc:
      message = """NUME_TABLE=%d incorrect : il n'y a que %d blocs de tables""" \
                """ dans le fichier""" % (nume, nbbloc)
      return 1, message, None
   txttab = l_txt[nume - 1]
  
   # expression régulière pour extraire le titre
   exp = re.compile(re.escape(idt_tit) + '(.*)$', re.MULTILINE)
   titre_tab = os.linesep.join([s.strip(separ) for s in exp.findall(txttab)])
  
   # restent dans la table les lignes non vides qui ne sont pas des titres
   txttab = [line for line in txttab.splitlines() \
                     if line.strip(separ) != '' and not line.startswith(idt_tit)]
  
   # ligne des paramètres et des types
   list_para = msplit(txttab.pop(0), separ)
   list_type = msplit(txttab.pop(0), separ)
   nb_para = len(list_type)
   
   # format de lecture
   fmt = {
      'I' : '([0-9\-\+]+)',
      'R' : '([0-9\.,\-\+eEdD]+)',
      'K' : '(.{%(len)s})'
   }
   lfmt = ('%s+' % re.escape(separ)).join(
      [fmt[typ[0]] % { 'len' : typ[1:] } for typ in list_type]
   )
   
   # construction des lignes de la Table
   l_rows = []
   for i, line in enumerate(txttab):
      mat = re.search(lfmt, line)
      if mat is None or nb_para != len(mat.groups()):
         message = """Nombre de champs incorrect ligne %d.
Il y a %d paramètres""" % (i + 1, nb_para)
         if hasattr(mat, 'groups'):
            message += """, on a lu %d champs.""" % len(mat.groups())
         return 1, message, None
      dico = {}
      for para, typ, ch in zip(list_para, list_type, mat.groups()):
         ch = ch.strip()
         if ch != id_vide:
            if typ == 'I':
               val = int(ch)
            elif typ == 'R':
               val = float(ch)
            else:
               val = ch
            dico[para] = val
      l_rows.append(dico)
   
   tab = Table(l_rows, list_para, list_type, titre_tab)
   return 0, '', tab


# ------------------------------------------------------------------------------
def lire_table_ops(self, **args):
   """Méthode corps de la macro LIRE_TABLE
   """
   from Utilitai.Utmess     import UTMESS
   from Utilitai.UniteAster import UniteAster
   
   ier = 0
   nompro = 'LIRE_TABLE'
   ### On importe les definitions des commandes a utiliser dans la macro
   CREA_TABLE = self.get_cmd('CREA_TABLE')
   UNITE      = self['UNITE']
   FORMAT     = self['FORMAT']
   NUME_TABLE = self['NUME_TABLE']
   SEPARATEUR = self['SEPARATEUR']
   PARA       = self['PARA']
   TITRE      = self['TITRE']
   
   ### La macro compte pour 1 dans la numerotation des commandes
   self.set_icmd(1)
   
   ### Lecture de la table dans un fichier d unité logique UNITE
   UL = UniteAster()
   nomfich=UL.Nom(UNITE)
   if not os.path.isfile(nomfich):
      UTMESS('F', nompro, "le fichier '%s' est introuvable" % nomfich)
   
   texte = open(nomfich,'r').read()
   # remet UNITE dans son état initial
   UL.EtatInit()
   
   ### mise en forme de la liste de valeurs suivant le format choisi :
   # pour le moment uniquement ASTER
   if FORMAT=='ASTER':
      ier, message, tab_lue = lecture_table(texte, NUME_TABLE, SEPARATEUR)
      if ier != 0 :
         UTMESS('F', nompro, message)
   else:
      pass
   
   ### création de la table ASTER :
   self.DeclareOut('ut_tab', self.sd)
   motscles = tab_lue.dict_CREA_TABLE()
   ut_tab=CREA_TABLE(**motscles)
   
   return ier
