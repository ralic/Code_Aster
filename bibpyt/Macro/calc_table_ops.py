#@ MODIF calc_table_ops Macro  DATE 10/04/2006   AUTEUR MCOURTOI M.COURTOIS 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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

# RESPONSABLE MCOURTOI M.COURTOIS
from types import *
EnumTypes = (ListType, TupleType)

def calc_table_ops(self, TABLE, ACTION, INFO, **args):
   """
   Macro CALC_TABLE permettant de faire des opérations sur une table
   """
   import aster

   macro = 'CALC_TABLE'
   from Accas               import _F
   from Cata.cata import table_jeveux
   from Utilitai.Utmess     import UTMESS
   from Utilitai            import transpose
   from Utilitai.Table      import Table, merge
   from Utilitai.Sensibilite import NomCompose

   ier = 0
   # La macro compte pour 1 dans la numerotation des commandes
   self.set_icmd(1)

   # Le concept sortant (de type table_sdaster ou dérivé) est tab
   self.DeclareOut('tabout', self.sd)

   # On importe les definitions des commandes a utiliser dans la macro
   # Le nom de la variable doit etre obligatoirement le nom de la commande
   CREA_TABLE    = self.get_cmd('CREA_TABLE')
   DETRUIRE      = self.get_cmd('DETRUIRE')

   # 0. faut-il utiliser une table dérivée
   form_sens='\n... SENSIBILITE AU PARAMETRE %s (SD COMP %s)'
   if args['SENSIBILITE']:
      ncomp = NomCompose(TABLE, args['SENSIBILITE'], msg='F')
      sdtab = table_jeveux(ncomp)
      tab = sdtab.EXTR_TABLE()
   else:
      tab = TABLE.EXTR_TABLE()

   #----------------------------------------------
   # Boucle sur les actions à effectuer
   for fOP in ACTION:
      occ = fOP.cree_dict_valeurs(fOP.mc_liste)
      for mc, val in occ.items():
         if val == None:
            del occ[mc]
   
      #----------------------------------------------
      # 1. Traitement du FILTRE
      # format pour l'impression des filtres
      form_filtre = '\nFILTRE -> NOM_PARA: %-16s CRIT_COMP: %-4s VALE: %s'
      if occ['OPERATION'] == 'FILTRE':
         col = getattr(tab, occ['NOM_PARA'])
         # peu importe le type, c'est la meme méthode d'appel
         opts = [occ[k] for k in ('VALE','VALE_I','VALE_C','VALE_K') if occ.has_key(k)]
         kargs = {}
         for k in ('CRITERE','PRECISION'):
            if occ.has_key(k):
               kargs[k] = occ[k]
         tab = tab & ( getattr(col, occ['CRIT_COMP'])(*opts,**kargs) )
         # trace l'operation dans le titre
         #if FORMAT in ('TABLEAU','ASTER'):
         tab.titr += form_filtre % (occ['NOM_PARA'], occ['CRIT_COMP'], \
            ' '.join([str(v) for v in opts]))

      #----------------------------------------------
      # 2. Traitement de EXTR
      if occ['OPERATION'] == 'EXTR':
         lpar = occ['NOM_PARA']
         if not type(lpar) in EnumTypes:
            lpar = [lpar]
         for p in lpar:
            if not p in tab.para:
               UTMESS('F', macro, 'Paramètre %s inexistant dans la table %s' % (p, TABLE.nom))
         tab = tab[occ['NOM_PARA']]

      #----------------------------------------------
      # 3. Traitement de RENOMME
      if occ['OPERATION'] == 'RENOMME':
         try:
            tab.Renomme(*occ['NOM_PARA'])
         except KeyError, msg:
            UTMESS('F', macro, msg)

      #----------------------------------------------
      # 4. Traitement du TRI
      if occ['OPERATION'] == 'TRI':
         tab.sort(CLES=occ['NOM_PARA'], ORDRE=occ['ORDRE'])
   
      #----------------------------------------------
      # 5. Traitement de COMB
      if occ['OPERATION'] == 'COMB':
         tab2 = occ['TABLE'].EXTR_TABLE()
         opts = [tab, tab2]
         if occ['NOM_PARA']<>None:
            lpar = occ['NOM_PARA']
            if not type(lpar) in EnumTypes:
               lpar = [lpar]
            for p in lpar:
               if not p in tab.para:
                  UTMESS('F', macro, 'Paramètre %s inexistant dans la table %s' % (p, TABLE.nom))
               if not p in tab2.para:
                  UTMESS('F', macro, 'Paramètre %s inexistant dans la table %s' % (p, occ['TABLE'].nom))
            opts.append(lpar)
         tab = merge(*opts)
   
      #----------------------------------------------
      # 6. Traitement de OPER
      if occ['OPERATION'] == 'OPER':
         # ajout de la colonne dans la table
         tab.fromfunction(occ['NOM_PARA'], occ['FORMULE'])
         if INFO == 2:
            vectval = getattr(tab, occ['NOM_PARA']).values()
            aster.affiche('MESSAGE', 'Ajout de la colonne %s : %s' % (occ['NOM_PARA']+repr(vectval))+'\n')

   #----------------------------------------------
   # 99. Création de la table_sdaster résultat
   # cas réentrant : il faut détruire l'ancienne table_sdaster
   if self.sd.nom == TABLE.nom:
      DETRUIRE(CONCEPT=_F(NOM=TABLE.nom,), INFO=1)

   dprod = tab.dict_CREA_TABLE()
   if INFO == 2:
      echo_mess = []
      echo_mess.append( '@-'*30+'\n' )
      echo_mess.append( tab )
      from pprint import pformat
      echo_mess.append( pformat(dprod) )
      echo_mess.append( '@-'*30+'\n' )
      texte_final = ' '.join(echo_mess)
      aster.affiche('MESSAGE', texte_final)

   # surcharge par le titre fourni
   tit = args['TITRE']
   if tit != None:
      if not type(tit) in EnumTypes:
         tit = [tit]
      dprod['TITRE'] = tuple(['%-80s' % lig for lig in tit])
   # type de la table de sortie à passer à CREA_TABLE
   tabout = CREA_TABLE(**dprod)
   
   return ier
