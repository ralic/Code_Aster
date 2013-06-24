# coding=utf-8
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
# person_in_charge: aimery.assire at edf.fr


# ===========================================================================
#           CORPS DE LA MACRO "DEFI_PART_FETI"
#           -------------------------------------
# USAGE :
#  MAILLAGE        maillage a partitionner
#  MODELE          modele (facultatif)
#  NB_PART         nb de sous-domaines
#  EXCIT           liste des chargements
#  METHODE         PMETIS, KMETIS ou AUTRE
#  LOGICIEL        si AUTRE alors on attend un chemin complet vers executable
#  NOM_GROUP_MA    Un nom de base pour les group_ma contenant les SD
#  INFO            1,2
#
# ===========================================================================
# script PYTHON : lancement de DEFI_PART_PA_OPS et de DEFI_PART_OPS


def defi_part_feti_ops(self,NB_PART,METHODE,TRAITER_BORDS,NOM_GROUP_MA,CORRECTION_CONNEX,INFO,**args):

  import aster, string, sys

  from Accas import _F
  from Noyau.N_utils import AsType
  from Utilitai.Utmess import  UTMESS

  from Utilitai import partition

  # DEBUT DE LA MACRO
  # La macro compte pour 1 dans la numerotation des commandes
  self.set_icmd(1)
  ier=0

  # On importe les definitions des commandes a utiliser dans la macro
  DEFI_PART_OPS   = self.get_cmd('DEFI_PART_OPS')
  INFO_EXEC_ASTER = self.get_cmd('INFO_EXEC_ASTER')
  DEFI_FICHIER    = self.get_cmd('DEFI_FICHIER')
  DEFI_PART_PA_OPS   = self.get_cmd('DEFI_PART_PA_OPS')

  nompro='DEFI_PART_FETI'

  # Maillage
  if args.has_key('MODELE'):
    if args['MODELE'] != None:
      __MOD = string.ljust(args['MODELE'].nom,8)
      __MOD =__MOD+'.MODELE    .LGRF        '
      __LMAIL = aster.getvectjev(__MOD)
      __MAIL  = string.strip(__LMAIL[0])
      MAILLAGE=self.get_sd_avant_etape(__MAIL,self)
    else:
      MAILLAGE=args['MAILLAGE']

  # Nom des GROUP_MA générés
  NOM_GROUP_MA = string.strip(NOM_GROUP_MA)

  # Test sur le nombre de caractères de NOM_GROUP_MA
  if ( len(NOM_GROUP_MA)+len(str(NB_PART)) > 7 ):
    ln=7-len(str(NB_PART))
    UTMESS('F','FETI0_1',vali=ln)

  # Verification que des GROUP_MA ne portent pas deja les memes noms
  _lst = []
  for i in MAILLAGE.LIST_GROUP_MA():
    _lst.append( string.strip(i[0]) )
  for i in range(NB_PART):
    if ( NOM_GROUP_MA+str(i) in _lst ):
      ngrma=NOM_GROUP_MA+str(i)
      UTMESS('F','FETI0_2',valk=ngrma)
    if args.has_key('NOM_GROUP_MA_BORD') :
      if args['NOM_GROUP_MA_BORD'] != None :
        if ( args['NOM_GROUP_MA_BORD']+str(i) in _lst ):
          ngrma=args['NOM_GROUP_MA_BORD']+str(i)
          UTMESS('F','FETI0_2',valk=ngrma)

  # Le concept sortant dans le contexte de la macro
  self.DeclareOut('_SDFETI',self.sd)

  # Debut :

  # Regeneration des mots-cles GROUPAGE passés en argument de la macro
  motscle1= {}
  if args.has_key('GROUPAGE'):
    if args['GROUPAGE'] != None :
      dGroup=[]
      for j in args['GROUPAGE']:
        dGroup.append(j.cree_dict_valeurs(j.mc_liste))
        for i in dGroup[-1].keys():
          if dGroup[-1][i]==None : del dGroup[-1][i]
        motscle1['GROUPAGE']=dGroup

  # Regeneration des mots-cles POIDS_MAILLES passés en argument de la macro
  if args.has_key('POIDS_MAILLES'):
    if args['POIDS_MAILLES'] != None :
      dEval=[]
      for j in args['POIDS_MAILLES']:
        dEval.append(j.cree_dict_valeurs(j.mc_liste))
        for i in dEval[-1].keys():
          if dEval[-1][i]==None : del dEval[-1][i]
        motscle1['POIDS_MAILLES']=dEval

  # Y a t'il présence du mot clé : NOM_GROUP_MA_BORD
  if args.has_key('GROUP_MA_BORD'):
    if args['GROUP_MA_BORD'] != None :
      motscle1['GROUP_MA_BORD']=args['GROUP_MA_BORD']

  # Y a t'il présence du mot clé : LOGICIEL
  if args.has_key('LOGICIEL'):
    if args['LOGICIEL'] != None :
      motscle1['LOGICIEL']=args['LOGICIEL']

  # Y a t'il présence du mot clé : MODELE
  if args.has_key('MODELE'):
    if args['MODELE'] != None :
      motscle1['MODELE']=args['MODELE']

  # Partitionnement
  DEFI_PART_PA_OPS(
                   MAILLAGE=MAILLAGE,
                   INFO=INFO,
                   METHODE=METHODE,
                   NB_PART=NB_PART,
                   CORRECTION_CONNEX=CORRECTION_CONNEX,
                   TRAITER_BORDS=TRAITER_BORDS,
                   NOM_GROUP_MA=NOM_GROUP_MA,
                   **motscle1
                     );

  # Liste des groupes de mailles du maillage
  _LST_GMA = MAILLAGE.LIST_GROUP_MA()
  _LST_GMA = map(lambda x: x[0], _LST_GMA)

  # Creation de la SDFETI
  if args.has_key('MODELE'):
    if args['MODELE'] != None :
      _tmp  = []
      for i in range(NB_PART):
        txt = { 'GROUP_MA': NOM_GROUP_MA + str(i) }
        _tmp.append( txt )

        if args.has_key('NOM_GROUP_MA_BORD') :
          if args['NOM_GROUP_MA_BORD'] != None :
            if ( args['NOM_GROUP_MA_BORD']+str(i) in _LST_GMA ):
              txt['GROUP_MA_BORD'] = string.strip(args['NOM_GROUP_MA_BORD']) + str(i)
            _tmp.append( txt )

      motscle2= {'DEFI': _tmp }

      # Regeneration des mots-cles EXCIT passés en argument de la macro
      if args.has_key('EXCIT'):
        if args['EXCIT'] != None :
          dExcit=[]
          for j in args['EXCIT']:
            dExcit.append(j.cree_dict_valeurs(j.mc_liste))
            for i in dExcit[-1].keys():
              if dExcit[-1][i]==None : del dExcit[-1][i]
          motscle2['EXCIT']=dExcit

      _SDFETI=DEFI_PART_OPS(NOM='SDD',
                            MODELE=args['MODELE'],
                            INFO=1,
                            **motscle2
                            );
    else:
      _SDFETI=None

  else:
    _SDFETI=None


  # Fin :

  return ier
