#@ MODIF impr_fonction_ops Macro  DATE 03/11/2004   AUTEUR MCOURTOI M.COURTOIS 
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

# RESPONSABLE MCOURTOI M.COURTOIS

import os.path
from Utilitai.Graph  import Graph
from Utilitai.Utmess import UTMESS

# ------------------------------------------------------------------------------
def impr_fonction_ops(self, FORMAT, COURBE, INFO, **args):
   """
   Macro IMPR_FONCTION permettant d'imprimer dans un fichier des fonctions,
   colonnes de table...
   Erreurs<S> dans IMPR_FONCTION pour ne pas perdre la base.
   """
   macro='IMPR_FONCTION'
   import aster
   from Accas import _F
   ier=0
   # La macro compte pour 1 dans la numerotation des commandes
   self.set_icmd(1)

   # On importe les definitions des commandes a utiliser dans la macro
   # Le nom de la variable doit etre obligatoirement le nom de la commande
   CALC_FONC_INTERP = self.get_cmd('CALC_FONC_INTERP')
   DEFI_LIST_REEL   = self.get_cmd('DEFI_LIST_REEL')
   DEFI_FICHIER     = self.get_cmd('DEFI_FICHIER')
   DETRUIRE         = self.get_cmd('DETRUIRE')

   #----------------------------------------------
   # 0. Traitement des arguments, initialisations
   # unité logique des fichiers réservés
   ul_reserve=(8,)

   # 0.1. Fichier
   nomfich=None
   if args['UNITE'] and args['UNITE']<>6:
      nomfich='fort.'+str(args['UNITE'])
      if INFO==2:
         print ' Nom du fichier :',nomfich
   if nomfich and os.path.exists(nomfich):
      print ' <A> Le fichier '+nomfich+' existe déjà.'
      if FORMAT=='TABLEAU':
         print '     On écrit à la suite du fichier'
      else:
         print '     On écrase le contenu précédent'

   # 0.2. Récupération des valeurs sous COURBE
   unparmi=('FONCTION','LIST_RESU','FONC_X','ABSCISSE')

   Courbe=[]
   for Ci in COURBE:
      dC = Ci.cree_dict_valeurs(Ci.mc_liste)
      for mc in dC.keys():
         if dC[mc]==None: del dC[mc]
      Courbe.append(dC)
   if INFO==2:
      print ' Nombre de fonctions à analyser : ',len(Courbe)

   # 0.3. Devra-t-on interpoler globalement ?
   #      Dans ce cas, linter__ est le LIST_PARA
   #      ou, à défaut, les abscisses de la première courbe
   interp=False
   if FORMAT=='TABLEAU':
      interp=True
      dCi=Courbe[0]
      if dCi.has_key('LIST_PARA'):
         linter__=dCi['LIST_PARA']
      else:
         obj=None
         for typi in unparmi:
            if dCi.has_key(typi):
               obj=dCi[typi]
               typ=obj.__class__.__name__
               break
         if obj==None:
            UTMESS('S',macro,'incohérence entre le catalogue et la macro.', self)
         if typi=='FONCTION':
            if typ=='nappe_sdaster':
               lpar,lval=obj.Valeurs()
               linterp=lval[0][0]
            else:
               linterp=obj.Valeurs()[0]
         elif typi=='FONC_X':
            lbid,linterp=obj.Valeurs()
         elif typi=='ABSCISSE':
            linterp=obj
         linter__=DEFI_LIST_REEL(VALE=linterp)
      if INFO==2:
         print ' Interpolation globale sur la liste :\n',linter__.Valeurs()


   #----------------------------------------------
   # 1. Récupération des valeurs des N courbes sous forme
   #    d'une liste de N listes
   #----------------------------------------------
   graph=Graph()
   iocc=-1
   for dCi in Courbe:
      iocc=iocc+1

      # 1.1. Type d'objet à traiter
      obj=None
      for typi in unparmi:
         if dCi.has_key(typi):
            obj=dCi[typi]
            typ=obj.__class__.__name__
            break
      if obj==None:
         UTMESS('S',macro,'incohérence entre le catalogue et la macro.',self)

      # 1.2. Extraction des valeurs

      # 1.2.1. Mot-clé FONCTION
      if   typi=='FONCTION':
         if typ=='nappe_sdaster':
            lpar,lval=obj.Valeurs()
            dico,ldicf=obj.Parametres()
            for i in range(len(lpar)):
               p=lpar[i]
               lx=lval[i][0]
               ly=lval[i][1]
               # sur quelle liste interpoler chaque fonction
               if i==0:
                  if interp:
                     li__=linter__
                  elif dCi.has_key('LIST_PARA'):
                     li__=dCi['LIST_PARA']
                  else:
                     li__=DEFI_LIST_REEL(VALE=lx)
               # compléter les paramètres d'interpolation
               dic=dico.copy()
               dic.update(ldicf[i])
               
               if (interp or dCi.has_key('LIST_PARA')) and i>0:
                  ftmp__=CALC_FONC_INTERP(
                     FONCTION=obj,
                     VALE_PARA=p,
                     LIST_PARA_FONC=li__,
                     **dic
                  )
                  pv,lv2=ftmp__.Valeurs()
                  lx=lv2[0][0]
                  ly=lv2[0][1]
               # on stocke les données dans le Graph
               dicC={
                  'Val' : [lx,ly],
                  'Lab' : [dic['NOM_PARA_FONC'],dic['NOM_RESU']]
               }
               AjoutParaCourbe(dicC, args=dCi)
               graph.AjoutCourbe(**dicC)
         else:
            ftmp__=obj
            dpar=ftmp__.Parametres()
            if interp:
               ftmp__=CALC_FONC_INTERP(
                  FONCTION=obj,
                  LIST_PARA=linter__,
                  **dpar
               )
            elif dCi.has_key('LIST_PARA'):
               ftmp__=CALC_FONC_INTERP(
                  FONCTION=obj,
                  LIST_PARA=dCi['LIST_PARA'],
                  **dpar
               )
            lval=list(ftmp__.Valeurs())
            lx=lval[0]
            lr=lval[1]
            if typ=='fonction_c' and dCi.has_key('PARTIE'):
               if dCi['PARTIE']=='COMPLEXE' : lr=lval[2]
            # on stocke les données dans le Graph
            if typ=='fonction_c' and not dCi.has_key('PARTIE'):
               dicC={
                  'Val' : lval,
                  'Lab' : [dpar['NOM_PARA'],dpar['NOM_RESU']+'_R',dpar['NOM_RESU']+'_I']
               }
            else:
               dicC={
                  'Val' : [lx,lr],
                  'Lab' : [dpar['NOM_PARA'],dpar['NOM_RESU']]
               }
            AjoutParaCourbe(dicC, args=dCi)
            graph.AjoutCourbe(**dicC)

      # 1.2.2. Mot-clé LIST_RESU
      elif typi=='LIST_RESU':
         if interp and iocc>0:
            UTMESS('S',macro,"""Il n'y a pas de règles d'interpolation pour LIST_PARA/LIST_RESU,
     LIST_PARA/LIST_RESU ne peut donc apparaitre qu'une seule fois
     et à la première occurence de COURBE""",self)
         lx=dCi['LIST_PARA'].Valeurs()
         lr=obj.Valeurs()
         if len(lx)<>len(lr):
            UTMESS('S',macro,"LIST_PARA et LIST_RESU n'ont pas la meme taille",self)
         # on stocke les données dans le Graph
         dicC={
            'Val' : [lx,lr],
            'Lab' : [dCi['LIST_PARA'].get_name(),obj.get_name()]
         }
         AjoutParaCourbe(dicC, args=dCi)
         graph.AjoutCourbe(**dicC)

      # 1.2.3. Mot-clé FONC_X
      # exemple : obj(t)=sin(t), on imprime x=sin(t), y=cos(t)
      #           ob2(t)=cos(t)
      elif typi=='FONC_X':
         ob2=dCi['FONC_Y']
         # peut-on blinder au niveau du catalogue
         if typ=="nappe_sdaster" or ob2.__class__.__name__=="nappe_sdaster":
            UTMESS('S',macro,"FONC_X/FONC_Y ne peuvent pas etre des nappes !",self)
         ftmp__=obj
         dpar=ftmp__.Parametres()
         ftm2__=ob2
         dpa2=ftm2__.Parametres()
         intloc=False
         if interp:
            intloc=True
            li__=linter__
         elif dCi.has_key('LIST_PARA'):
            intloc=True
            li__=dCi['LIST_PARA']
         if intloc:
            ftmp__=CALC_FONC_INTERP(
               FONCTION=obj,
               LIST_PARA=li__,
               **dpar
            )
            lt,lx=ftmp__.Valeurs()
            ftm2__=CALC_FONC_INTERP(
               FONCTION=ob2,
               LIST_PARA=li__,
               **dpa2
            )
         else:
            lt,lx=ftmp__.Valeurs()
            li__=DEFI_LIST_REEL(VALE=lt)
            ftm2__=CALC_FONC_INTERP(
               FONCTION=ob2,
               LIST_PARA=li__,
               **dpa2
            )
         
         lbid,ly=ftm2__.Valeurs()
         # on stocke les données dans le Graph
         if interp:
            dicC={
               'Val' : [lt,lx,ly],
               'Lab' : [dpar['NOM_PARA'],dpar['NOM_RESU'],dpa2['NOM_RESU']]
            }
         else:
            dicC={
               'Val' : [lx,ly],
               'Lab' : [dpar['NOM_PARA'],dpa2['NOM_RESU']]
            }
         AjoutParaCourbe(dicC, args=dCi)
         graph.AjoutCourbe(**dicC)

      # 1.2.4. Mot-clé ABSCISSE / ORDONNEE
      elif typi=='ABSCISSE':
         if interp and iocc>0:
            UTMESS('S',macro,"""Il n'y a pas de règles d'interpolation pour ABSCISSE/ORDONNEE,
     ABSCISSE/ORDONNEE ne peut donc apparaitre qu'une seule fois
     et à la première occurence de COURBE""",self)
         lx=obj
         lr=dCi['ORDONNEE']
         if len(lx)<>len(lr):
            UTMESS('S',macro,"ABSCISSE et ORDONNEE n'ont pas la meme taille",self)
         # on stocke les données dans le Graph
         dicC={
            'Val' : [lx,lr],
            'Lab' : ['Absc','Ordo']
         }
         AjoutParaCourbe(dicC, args=dCi)
         graph.AjoutCourbe(**dicC)

      # 1.2.99. ménage
      DETRUIRE(CONCEPT=_F(NOM=('li__','ftmp__','ftm2__'),),
               ALARME='NON',INFO=1)

   # 1.3. dbg
   if INFO==2:
      print '\n'+'-'*70+'\n Contenu du Graph : \n'+'-'*70
      print graph
      print '-'*70+'\n'

   #----------------------------------------------
   # 2. Impression du 'tableau' de valeurs
   #----------------------------------------------

   # 2.0. Surcharge des propriétés du graphique et des axes
   if args['TITRE']<>None:          graph.Titre=args['TITRE']
   if args['SOUS_TITRE']<>None:     graph.SousTitre=args['SOUS_TITRE']
   if FORMAT in ('XMGRACE','AGRAF'):
      if args['BORNE_X']<>None:
                                       graph.Min_X=args['BORNE_X'][0]
                                       graph.Max_X=args['BORNE_X'][1]
      if args['BORNE_Y']<>None:
                                       graph.Min_Y=args['BORNE_Y'][0]
                                       graph.Max_Y=args['BORNE_Y'][1]
      if args['LEGENDE_X']<>None:      graph.Legende_X=args['LEGENDE_X']
      if args['LEGENDE_Y']<>None:      graph.Legende_Y=args['LEGENDE_Y']
      if args['ECHELLE_X']<>None:      graph.Echelle_X=args['ECHELLE_X']
      if args['ECHELLE_Y']<>None:      graph.Echelle_Y=args['ECHELLE_Y']
      if args['GRILLE_X']<>None:       graph.Grille_X=args['GRILLE_X']
      if args['GRILLE_Y']<>None:       graph.Grille_Y=args['GRILLE_Y']

   kargs={
      'FORMAT'    : FORMAT,
      'FICHIER'   : nomfich,
   }
   
   # 2.1. au format TABLEAU
   if FORMAT=='TABLEAU':
      # surcharge par les formats de l'utilisateur
      kargs['dform']={
         'csep'  : args['SEPARATEUR'],
         'ccom'  : args['COMMENTAIRE'],
         'cdeb'  : args['DEBUT_LIGNE'],
         'cfin'  : args['FIN_LIGNE']
      }

   # 2.2. au format AGRAF
   elif FORMAT=='AGRAF':
      nomdigr=None
      if args['UNITE_DIGR']<>6:
         nomdigr='fort.'+str(args['UNITE_DIGR'])
      kargs['FICHIER']=[nomfich, nomdigr]
      kargs['dform']={ 'formR' : '%12.5E' }

   # 2.3. au format XMGRACE et dérivés
   elif FORMAT=='XMGRACE':
      kargs['dform']={ 'formR' : '%.8g' }
      kargs['PILOTE']=args['PILOTE']

   # 2.39. Format inconnu
   else:
      UTMESS('S',macro,'Format inconnu : '+FORMAT,self)

   # Traiter le cas des UL réservées
   if args['UNITE'] and args['UNITE'] in ul_reserve:
      DEFI_FICHIER( ACTION='LIBERER', UNITE=args['UNITE'], )
   if FORMAT=='AGRAF' and args['UNITE_DIGR']<>args['UNITE'] \
         and args['UNITE_DIGR'] in ul_reserve:
      DEFI_FICHIER( ACTION='LIBERER', UNITE=args['UNITE_DIGR'], )

   # 2.4. On trace !
   graph.Trace(**kargs)

   # 99. Traiter le cas des UL réservées
   if args['UNITE'] and args['UNITE'] in ul_reserve:
      DEFI_FICHIER( ACTION='ASSOCIER', UNITE=args['UNITE'],
            TYPE='ASCII', ACCES='APPEND' )
   if FORMAT=='AGRAF' and args['UNITE_DIGR']<>args['UNITE'] \
         and args['UNITE_DIGR'] in ul_reserve:
      DEFI_FICHIER( ACTION='ASSOCIER', UNITE=args['UNITE_DIGR'],
            TYPE='ASCII', ACCES='APPEND' )

   return ier

# ------------------------------------------------------------------------------
def AjoutParaCourbe(dCourbe, args):
   """Ajoute les arguments fournis par l'utilisateur (args) dans le dictionnaire
   décrivant la courbe (dCourbe).
   """
   # correspondance : mot-clé Aster / clé du dico de l'objet Graph
   keys={
      'LEGENDE'         : 'Leg',
      'STYLE'           : 'Sty',
      'COULEUR'         : 'Coul',
      'MARQUEUR'        : 'Marq',
      'FREQ_MARQUEUR'   : 'FreqM',
      'TRI'             : 'Tri',
   }
   for mc, key in keys.items():
      if args.has_key(mc):
         dCourbe[key]=args[mc]

