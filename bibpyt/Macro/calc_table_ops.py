#@ MODIF calc_table_ops Macro  DATE 03/01/2006   AUTEUR REZETTE C.REZETTE 
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

def calc_table_ops(self, TABLE, FILTRE, EXTR, RENOMME, TRI, COMB, OPER,
                   INFO, **args):
   """
   Macro CALC_TABLE permettant de faire des opérations sur une table
   """
   import aster

   macro = 'CALC_TABLE'
   from Accas               import _F
   from Utilitai.Utmess     import UTMESS
   from Utilitai            import transpose
   from Utilitai.Table      import Table, merge

   ier = 0
   # La macro compte pour 1 dans la numerotation des commandes
   self.set_icmd(1)

   # Le concept sortant (de type table_sdaster ou dérivé) est tab
   self.DeclareOut('tabout', self.sd)

   # On importe les definitions des commandes a utiliser dans la macro
   # Le nom de la variable doit etre obligatoirement le nom de la commande
   CREA_TABLE    = self.get_cmd('CREA_TABLE')
   DETRUIRE      = self.get_cmd('DETRUIRE')

   tab = TABLE.EXTR_TABLE()

   #----------------------------------------------
   # 1. Traitement du FILTRE
   Filtre = []
   # format pour l'impression des filtres
   form_filtre = '\nFILTRE -> NOM_PARA: %-16s CRIT_COMP: %-4s VALE: %s'
   if FILTRE != None:
      for Fi in FILTRE:
         dF = Fi.cree_dict_valeurs(Fi.mc_liste)
         for mc in dF.keys():
            if dF[mc] == None:
               del dF[mc]
         Filtre.append(dF)

   for Fi in Filtre:
      col = getattr(tab, Fi['NOM_PARA'])
      # peu importe le type
      opts = [Fi[k] for k in ('VALE','VALE_I','VALE_C','VALE_K') if Fi.has_key(k)]
      kargs = {}
      for k in ('CRITERE','PRECISION'):
         if Fi.has_key(k):
            kargs[k] = Fi[k]
      tab = tab & ( getattr(col, Fi['CRIT_COMP'])(*opts,**kargs) )
      # trace l'operation dans le titre
      #if FORMAT in ('TABLEAU','ASTER'):
      tab.titr += form_filtre % (Fi['NOM_PARA'], Fi['CRIT_COMP'], \
         ' '.join([str(v) for v in opts]))

   #----------------------------------------------
   # 2. Traitement de EXTR
   if EXTR != None:
      lpar = EXTR['NOM_PARA']
      if not type(lpar) in EnumTypes:
         lpar = [lpar]
      for p in lpar:
         if not p in tab.para:
            UTMESS('F', macro, 'Paramètre %s inexistant dans la table %s' % (p, TABLE.nom))
      tab = tab[EXTR['NOM_PARA']]

   #----------------------------------------------
   # 3. Traitement de RENOMME
   if RENOMME != None:
      for MCFi in RENOMME:
         try:
            tab.Renomme(*MCFi['NOM_PARA'])
         except KeyError, msg:
            UTMESS('F', macro, msg)

   #----------------------------------------------
   # 4. Traitement du TRI
   if TRI != None:
      tab.sort(CLES=TRI['NOM_PARA'], ORDRE=TRI['ORDRE'])

   #----------------------------------------------
   # 5. Traitement de COMB
   if COMB != None:
      tab2 = COMB['TABLE'].EXTR_TABLE()
      opts = [tab, tab2]
      if COMB['NOM_PARA']<>None:
         lpar = COMB['NOM_PARA']
         if not type(lpar) in EnumTypes:
            lpar = [lpar]
         for p in lpar:
            if not p in tab.para:
               UTMESS('F', macro, 'Paramètre %s inexistant dans la table %s' % (p, TABLE.nom))
            if not p in tab2.para:
               UTMESS('F', macro, 'Paramètre %s inexistant dans la table %s' % (p, COMB['TABLE'].nom))
         opts.append(lpar)
      tab = merge(*opts)

   #----------------------------------------------
   # 6. Traitement de OPER
   if OPER != None:
      for MCFi in OPER:
         if MCFi['NOM_PARA'] in tab.para :
            UTMESS('F', macro, 'Le paramètre %s existe déjà dans la table %s' % (MCFi['NOM_PARA'], TABLE.nom))
         func = MCFi['FORMULE']
         tabpar = []
         for para in func.nompar :
            if para not in tab.para :
               UTMESS('F', macro, 'Le paramètre de la formule %s est inexistant dans la table %s' % (para, TABLE.nom))
#             i = tab.para.index(para)
#             if tab.type[i] != 'R' :
#                UTMESS('F', macro, 'Le paramètre %s doit etre réel dans la table %s' % (para, TABLE.nom))
            vals = getattr(tab,para).values()
            tabpar.append(vals)
         tabpar = transpose.transpose(tabpar)
         vectval = []
         for lpar in tabpar:
            # si un paramètre est absent, on ne peut pas évaluer la formule
            if None in lpar:
               vectval.append(None)
            else:
               vectval.append(func(*lpar))
         # ajout de la colonne dans la table
         if INFO == 2:
            aster.affiche('MESSAGE', 'Ajout de la colonne %s : %s' % (MCFi['NOM_PARA']+repr(vectval))+'\n')
         tab[MCFi['NOM_PARA']] = vectval

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
