#@ MODIF lire_fonction_ops Macro  DATE 22/03/2011   AUTEUR COURTOIS M.COURTOIS 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
# RESPONSABLE COURTOIS M.COURTOIS

import os.path as osp
import numpy

class LectureBlocError(Exception):
   pass

# ------------------------------------------------------------------------------
def lire_blocs(nomfich, SEPAR):
   """Retourne la liste des blocs
   """
   def info(ib, nlig, ncol):
      """Affiche les infos d'un bloc"""
      print "   . Bloc %2d : %6d lignes, %6d colonnes" % (ib, nlig, ncol)
   print "  Lecture des blocs du fichier '%s'" % nomfich
   fich=open(nomfich, 'r')
   if SEPAR == 'None':
      SEPAR=None
   blocs  = []
   lignes = []
   llen=0
   il=0
   for line in fich:
      il+=1
      try:
         if line.strip()=='':
            raise ValueError
         splin = line.split(SEPAR)
         lignes.append(map(float, splin))
         if llen==0:
            llen=len(splin)
         elif len(splin) != llen:
            raise LectureBlocError,  'Ligne %d : %d champs au lieu de %d attendus' % (il, len(splin), llen)
      except ValueError:
         if lignes==[]:
            pass  # dans ce cas, on a plusieurs lignes délimitant 2 fonctions
         else:
            blocs.append(numpy.array(lignes))
            info(len(blocs), len(lignes), llen)
            lignes=[]
            llen=0
   fich.close()
   if len(lignes) > 0 :
      blocs.append(numpy.array(lignes))
      info(len(blocs), len(lignes), llen)
   return blocs

# ------------------------------------------------------------------------------
def liste_double(nomfich,INDIC_PARA,INDIC_RESU,SEPAR):
  """Méthode de construction du VALE pour le format libre
  format LIBRE
  Les lignes contenant autre chose que des séquences de nombres
  réels et de séparateurs sont considérées comme délimitant deux
  fonctions différentes. Cette situation correspond à l exception
  ValueError levée par le map de float. Le deuxieme indice de
  INDIC_PARA et INDIC_RESU est l indice permettant de pointer sur la
  fonction voulue, au sens de ce découpage.
  """
  from Utilitai.transpose import transpose
  blocs = lire_blocs(nomfich, SEPAR)

  # vérifications de cohérences lignes et colonnes
  nb_blocs = len(blocs)
  bloc_para = INDIC_PARA[0]
  col_para  = INDIC_PARA[1]
  bloc_resu = INDIC_RESU[0]
  col_resu  = INDIC_RESU[1]
  if bloc_para > nb_blocs :
     raise LectureBlocError, "Il y a %d blocs or INDIC_PARA=(%d, .)" % (nb_blocs, bloc_para)
  if bloc_resu > nb_blocs:
     raise LectureBlocError, "Il y a %d blocs or INDIC_RESU=(%d, .)" % (nb_blocs, bloc_resu)

  if col_para > len(blocs[bloc_para-1][0]):
     raise LectureBlocError, "Le bloc %d comporte %d colonnes or INDIC_PARA=(., %d)" % \
         (bloc_para, len(blocs[bloc_para-1][0]), col_para)
  if col_resu > len(blocs[bloc_resu-1][0]):
     raise LectureBlocError, "Le bloc %d comporte %d colonnes or INDIC_RESU=(., %d)" % \
         (bloc_resu, len(blocs[bloc_resu-1][0]), col_resu)

  # construction du VALE de la fonction par recherche des indices
  # de colonnes et de fonctions dans le tableau blocs
  vale_para = blocs[bloc_para-1][:,col_para-1]
  vale_resu = blocs[bloc_resu-1][:,col_resu-1]
  if len(vale_para) != len(vale_resu) :
     print 'VALE_PARA =', vale_para
     print 'VALE_RESU =', vale_resu
     message="""Les deux colonnes extraites n'ont pas la meme longueur
         %d lignes pour PARA
         %d lignes pour RESU""" % (len(vale_para), len(vale_resu))
     raise LectureBlocError, message

  laux=transpose([vale_para, vale_resu])
  liste_vale=[]
  for v in laux:
     liste_vale.extend(v)
  return liste_vale

# ------------------------------------------------------------------------------
def liste_simple(nomfich,INDIC_PARA,SEPAR):
  """recherche d'une liste simple
  """
  blocs = lire_blocs(nomfich, SEPAR)

  # vérifications de cohérences lignes et colonnes
  nb_blocs = len(blocs)
  bloc_para = INDIC_PARA[0]
  col_para  = INDIC_PARA[1]
  if bloc_para > nb_blocs :
     raise LectureBlocError, "Il y a %d blocs or INDIC_PARA=(%d, .)" % (nb_blocs, bloc_para)
  if col_para > len(blocs[bloc_para-1][0]):
     raise LectureBlocError, "Le bloc %d comporte %d colonnes or INDIC_PARA=(., %d)" % \
         (bloc_para, len(blocs[bloc_para-1][0]), col_para)

  # construction du VALE de la fonction par recherche des indices
  # de colonnes et de fonctions dans le tableau l_fonc
  vale_1=blocs[bloc_para-1][:,col_para-1]
  return vale_1.tolist()

# ------------------------------------------------------------------------------
def lire_fonction_ops(self,FORMAT,TYPE,SEPAR,INDIC_PARA,UNITE,
                      NOM_PARA,NOM_RESU,INTERPOL,PROL_DROITE,
                      PROL_GAUCHE,VERIF,INFO,TITRE,**args):
  """Méthode corps de la macro
  """
  from Accas import _F
  from Utilitai.Utmess     import  UTMESS
  from Utilitai.UniteAster import UniteAster

  ier=0
  # La macro compte pour 1 dans la numerotation des commandes
  self.set_icmd(1)

  # On recopie le mot cle defi_fonction pour le proteger
  if TYPE=='NAPPE' :
     mc_DEFI_FONCTION=args['DEFI_FONCTION']

  # On importe les definitions des commandes a utiliser dans la macro
  DEFI_FONCTION  =self.get_cmd('DEFI_FONCTION')
  DEFI_NAPPE     =self.get_cmd('DEFI_NAPPE')

  nompro='LIRE_FONCTION'

  # Lecture de la fonction dans un fichier d unité logique UNITE
  UL = UniteAster()
  nomfich=UL.Nom(UNITE)
  if not osp.isfile(nomfich):
     UTMESS('F','FONCT0_41',valk=nomfich)

  # fonction(_c) ou nappe en sortie
  self.DeclareOut('ut_fonc',self.sd)

  if   TYPE=='FONCTION':
    # mise en forme de la liste de valeurs suivant le format choisi :
    try:
       liste_vale = liste_double(nomfich,INDIC_PARA,args['INDIC_RESU'],SEPAR)
    except LectureBlocError, message:
       UTMESS('F', 'FONCT0_42', valk=message)

    # création de la fonction ASTER :
    ut_fonc=DEFI_FONCTION( NOM_PARA   =NOM_PARA,
                           NOM_RESU   =NOM_RESU,
                           PROL_DROITE=PROL_DROITE,
                           PROL_GAUCHE=PROL_GAUCHE,
                           INTERPOL   =INTERPOL,
                           INFO       =INFO,
                           TITRE      =TITRE,
                           VERIF      =VERIF,
                           VALE       =liste_vale,)

  elif TYPE=='FONCTION_C':
    # mise en forme de la liste de valeurs suivant le format choisi :
    if 'INDIC_REEL' in args :
                              indic1=args['INDIC_REEL']
                              indic2=args['INDIC_IMAG']
    if 'INDIC_MODU' in args :
                              indic1=args['INDIC_MODU']
                              indic2=args['INDIC_PHAS']
    try:
       liste_vale_r = liste_double(nomfich,INDIC_PARA,indic1,SEPAR)
    except LectureBlocError, message:
       UTMESS('F', 'FONCT0_42', valk=message)

    try:
       liste_vale_i = liste_double(nomfich,INDIC_PARA,indic2,SEPAR)
    except LectureBlocError, message:
       UTMESS('F', 'FONCT0_42', valk=message)

    liste=[]
    if   'INDIC_REEL' in args :
      for i in range(len(liste_vale_r)/2) :
        liste.extend([liste_vale_r[2*i],liste_vale_r[2*i+1],liste_vale_i[2*i+1]])
    elif 'INDIC_MODU' in args :
      for i in range(len(liste_vale_r)/2) :
        module=liste_vale_r[2*i+1]
        phase =liste_vale_i[2*i+1]
        liste.extend([liste_vale_r[2*i],module*cos(phase),module*sin(phase)])

    # création de la fonction ASTER :
    ut_fonc=DEFI_FONCTION( NOM_PARA    =NOM_PARA,
                           NOM_RESU    =NOM_RESU,
                           PROL_DROITE =PROL_DROITE,
                           PROL_GAUCHE =PROL_GAUCHE,
                           INTERPOL    =INTERPOL,
                           INFO        =INFO,
                           TITRE       =TITRE,
                           VERIF       =VERIF,
                           VALE_C      =liste,)

  elif TYPE=='NAPPE':

    # création de la nappe ASTER :
    motscles={}
    motscles['DEFI_FONCTION']=[]
    for elem in mc_DEFI_FONCTION:
       try:
          liste_vale=liste_double(nomfich,args['INDIC_ABSCISSE'],elem['INDIC_RESU'],SEPAR)
       except LectureBlocError, message:
          UTMESS('F', 'FONCT0_42', valk=message)

       motscles['DEFI_FONCTION'].append( _F( VALE       =liste_vale,
                                             INTERPOL   =args['INTERPOL_FONC'],
                                             PROL_DROITE=args['PROL_DROITE_FONC'],
                                             PROL_GAUCHE=args['PROL_GAUCHE_FONC'] ) )
    try:
       liste_para = liste_simple(nomfich,INDIC_PARA,SEPAR)
    except LectureBlocError, message:
       UTMESS('F', 'FONCT0_42', valk=message)

    # création de la nappe
    ut_fonc=DEFI_NAPPE( PARA          =liste_para,
                        NOM_PARA      =NOM_PARA,
                        NOM_PARA_FONC =args['NOM_PARA_FONC'],
                        NOM_RESU      =NOM_RESU,
                        PROL_DROITE   =PROL_DROITE,
                        PROL_GAUCHE   =PROL_GAUCHE,
                        INTERPOL      =INTERPOL,
                        INFO          =INFO,
                        TITRE         =TITRE,
                        VERIF         =VERIF,
                        **motscles)
  # remet UNITE dans son état initial
  UL.EtatInit()
  return ier
