# coding=utf-8
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
#           CORPS DE LA MACRO "DEFI_CABLE_BP"
#           -------------------------------------
# USAGE :
# Entrée :
#  - MODELE
#  - CABLE
#  - CHAM_MATER
#  - CARA_ELEM
#  - GROUP_MA_BETON
#  - DEFI_CABLE
#  - TYPE_ANCRAGE
#  - TENSION_INIT
#  - RECUL_ANCRAGE
#  - RELAXATION
#  - CONE
#      RAYON
#      LONGUEUR
#      PRESENT          OUI ou NON deux fois
#  - TITRE
#  - INFO               1 / 2
#
# ===========================================================================



def defi_cable_bp_ops(self,MODELE,CHAM_MATER,CARA_ELEM,GROUP_MA_BETON,
                           DEFI_CABLE,TYPE_ANCRAGE,TENSION_INIT,RECUL_ANCRAGE,
                           TYPE_RELAX,CONE,TITRE,INFO,**args):

  """
     Ecriture de la macro DEFI_CABLE_BP
  """
  from Accas import _F
  import aster,string, types
  from Utilitai.Utmess import UTMESS
  ier=0

  # On importe les definitions des commandes a utiliser dans la macro
  DEFI_GROUP      = self.get_cmd('DEFI_GROUP')
  IMPR_RESU       = self.get_cmd('IMPR_RESU')
  DEFI_CABLE_OP   = self.get_cmd('DEFI_CABLE_OP')

  # La macro compte pour 1 dans la numerotation des commandes
  self.set_icmd(1)

  # Le concept sortant (de type char_meca) est nomme CHCABLE dans
  # le contexte de la macro

  self.DeclareOut('__DC',self.sd)

  # ---------------------------------------------------------------------------- #
  #                  Début de la Macro :

  motscles={}

  # RECUPERATION DES INFOS DONNEES PAR LE MOT-CLE "CONE"

  if CONE:
    dCONE=CONE[0].cree_dict_valeurs(CONE[0].mc_liste)
    for i in dCONE.keys():
      if dCONE[i]==None : del dCONE[i]

    RAYON    = dCONE['RAYON']
    LONGUEUR = dCONE['LONGUEUR']

    motscles['CONE']=[]
    motscles['CONE'].append( dCONE )

    # RECUPERATION DU MAILLAGE A PARTIR DU MODELE
    __MAIL = aster.getvectjev( string.ljust(MODELE.nom,8) + '.MODELE    .LGRF        ' )
    __MAIL= __MAIL[0].strip()
    MAILLAGE = self.get_sd_avant_etape(__MAIL,self)

    # DEFINITION DU NOM DES GROUP_NO
    __NOM = 'AN__'
    __LGNO = MAILLAGE.LIST_GROUP_NO()
    __LGN1 = []
    for i in __LGNO :
      __LGN1.append( i[0][:len(__NOM)] )

    __NB  = __LGN1.count(__NOM)

# FIN RECUPERATION DES INFOS DONNEES PAR LE MOT-CLE "CONE"


  # RECUPERATION DES INFOS DONNEES PAR LE MOT-CLE "DEFI_CABLE"

  dDEFI_CABLE=[]
  for j in DEFI_CABLE :
      dDEFI_CABLE.append(j.cree_dict_valeurs(j.mc_liste))
      for i in dDEFI_CABLE[-1].keys():
          if dDEFI_CABLE[-1][i]==None : del dDEFI_CABLE[-1][i]


  # BOUCLE SUR LES FACTEURS DU MOT-CLE "DEFI_CABLE"

  motscles['DEFI_CABLE']=[]

  for i in dDEFI_CABLE:
#   CAS OU ON RENTRE UNE TENSION INITIALE DU CABLE (TYPE_RELAX='ETCC_REPRISE')
    motscle3={}
    if i.has_key('TENSION_CT') ==1:
       motscle3 = {'TENSION_CT' : i['TENSION_CT']} 

    # CAS OU L'ON A DEFINI LE MOT-CLE "CONE"
    if CONE:

      # CREATION DU PREMIER TUNNEL

      if dCONE['PRESENT'][0] == 'OUI':
        __NB = __NB + 1
        __NOM1 = __NOM + str( int(__NB) )

        motscle2={}
        motscle2['CREA_GROUP_NO']=[]

        if i.has_key('GROUP_MA') == 1:
          __CAB = i['GROUP_MA']

          if type(GROUP_MA_BETON) in [types.TupleType, types.ListType]: gma = list(GROUP_MA_BETON)
          else:                                                         gma = [ GROUP_MA_BETON ]
          gma.insert(0, __CAB)

          motscle2= {'CREA_GROUP_NO': [{'LONGUEUR': LONGUEUR, 'RAYON': RAYON, 'OPTION': 'TUNNEL', 'GROUP_MA': gma, 'GROUP_MA_AXE': __CAB, 'NOM': __NOM1}]}
        if i.has_key('MAILLE') == 1:
          UTMESS('F','CABLE0_2')
        if i.has_key('GROUP_NO_ANCRAGE') == 1:
          __PC1 = i['GROUP_NO_ANCRAGE'][0]
          motscle2['CREA_GROUP_NO'][0]['GROUP_NO_ORIG'] = __PC1
        if i.has_key('NOEUD_ANCRAGE') == 1:
          __PC1 = i['NOEUD_ANCRAGE'][0]
          motscle2['CREA_GROUP_NO'][0]['NOEUD_ORIG'] = __PC1

        DEFI_GROUP( reuse=MAILLAGE,
                    MAILLAGE=MAILLAGE,
                    INFO=INFO,
                    ALARME='NON',
                    **motscle2
                   ) ;

      # CREATION DU DEUXIEME TUNNEL

      if dCONE['PRESENT'][1] == 'OUI':
        __NB = __NB + 1
        __NOM2 = __NOM + str( int(__NB) )

        motscle2={}
        motscle2['CREA_GROUP_NO']=[]

        if i.has_key('GROUP_MA') == 1:
          __CAB = i['GROUP_MA']

          if type(GROUP_MA_BETON) in [types.TupleType, types.ListType]: gma = list(GROUP_MA_BETON)
          else:                                                         gma = [ GROUP_MA_BETON ]
          gma.insert(0, __CAB)

          motscle2= {'CREA_GROUP_NO': [{'LONGUEUR': LONGUEUR, 'RAYON': RAYON, 'OPTION': 'TUNNEL', 'GROUP_MA': gma, 'GROUP_MA_AXE': __CAB, 'NOM': __NOM2}]}
        if i.has_key('MAILLE') == 1:
          UTMESS('F','CABLE0_2')
        if i.has_key('GROUP_NO_ANCRAGE') == 1:
          __PC1 = i['GROUP_NO_ANCRAGE'][1]
          motscle2['CREA_GROUP_NO'][0]['GROUP_NO_ORIG'] = __PC1
        if i.has_key('NOEUD_ANCRAGE') == 1:
          __PC1 = i['NOEUD_ANCRAGE'][1]
          motscle2['CREA_GROUP_NO'][0]['NOEUD_ORIG'] = __PC1

        DEFI_GROUP( reuse=MAILLAGE,
                    MAILLAGE=MAILLAGE,
                    INFO=INFO,
                    ALARME='NON',
                    **motscle2
                   ) ;


      # CREATION DES NOUVEAUX FACTEURS DU MOT-CLE "DEFI_CABLE" POUR DEFI_CABLE_BP

      if dCONE['PRESENT'][0] == 'OUI' and dCONE['PRESENT'][1] == 'OUI':
        if i.has_key('GROUP_MA') == 1 and i.has_key('GROUP_NO_ANCRAGE') == 1:
          motscles['DEFI_CABLE'].append( _F(GROUP_MA=i['GROUP_MA'],
                                            GROUP_NO_ANCRAGE=i['GROUP_NO_ANCRAGE'],
                                            GROUP_NO_FUT=( __NOM1,__NOM2, ),
                                            **motscle3 ), )
        if i.has_key('GROUP_MA') == 1 and i.has_key('NOEUD_ANCRAGE') == 1:
          motscles['DEFI_CABLE'].append( _F(GROUP_MA=i['GROUP_MA'],
                                            NOEUD_ANCRAGE=i['NOEUD_ANCRAGE'],
                                            GROUP_NO_FUT=( __NOM1,__NOM2, ),
                                            **motscle3 ), )

      if dCONE['PRESENT'][0] == 'OUI' and dCONE['PRESENT'][1] == 'NON':
        if i.has_key('GROUP_MA') == 1 and i.has_key('GROUP_NO_ANCRAGE') == 1:
          motscles['DEFI_CABLE'].append( _F(GROUP_MA=i['GROUP_MA'],
                                            GROUP_NO_ANCRAGE=i['GROUP_NO_ANCRAGE'],
                                            GROUP_NO_FUT=( __NOM1, ),
                                            **motscle3 ), )
        if i.has_key('GROUP_MA') == 1 and i.has_key('NOEUD_ANCRAGE') == 1:
          motscles['DEFI_CABLE'].append( _F(GROUP_MA=i['GROUP_MA'],
                                            NOEUD_ANCRAGE=i['NOEUD_ANCRAGE'],
                                            GROUP_NO_FUT=( __NOM1, ),
                                            **motscle3 ), )

      if dCONE['PRESENT'][0] == 'NON' and dCONE['PRESENT'][1] == 'OUI':
        if i.has_key('GROUP_MA') == 1 and i.has_key('GROUP_NO_ANCRAGE') == 1:
          motscles['DEFI_CABLE'].append( _F(GROUP_MA=i['GROUP_MA'],
                                            GROUP_NO_ANCRAGE=i['GROUP_NO_ANCRAGE'],
                                            GROUP_NO_FUT=( __NOM2, ),
                                            **motscle3 ), )
        if i.has_key('GROUP_MA') == 1 and i.has_key('NOEUD_ANCRAGE') == 1:
          motscles['DEFI_CABLE'].append( _F(GROUP_MA=i['GROUP_MA'],
                                            NOEUD_ANCRAGE=i['NOEUD_ANCRAGE'],
                                            GROUP_NO_FUT=( __NOM2, ),
                                            **motscle3 ), )

      if dCONE['PRESENT'][0] == 'NON' and dCONE['PRESENT'][1] == 'NON':
        if i.has_key('GROUP_MA') == 1 and i.has_key('GROUP_NO_ANCRAGE') == 1:
          motscles['DEFI_CABLE'].append( _F(GROUP_MA=i['GROUP_MA'],
                                            GROUP_NO_ANCRAGE=i['GROUP_NO_ANCRAGE'],
                                            **motscle3 ), )
        if i.has_key('GROUP_MA') == 1 and i.has_key('NOEUD_ANCRAGE') == 1:
          motscles['DEFI_CABLE'].append( _F(GROUP_MA=i['GROUP_MA'],
                                            NOEUD_ANCRAGE=i['NOEUD_ANCRAGE'],
                                            **motscle3 ), )


    # CAS OU L'ON A PAS DEFINI LE MOT-CLE "CONE"
    else:
      if i.has_key('GROUP_MA') == 1 and i.has_key('GROUP_NO_ANCRAGE') == 1:
        motscles['DEFI_CABLE'].append( _F(GROUP_MA=i['GROUP_MA'],
                                          GROUP_NO_ANCRAGE=i['GROUP_NO_ANCRAGE'],
                                          **motscle3 ), )

      if i.has_key('GROUP_MA') == 1 and i.has_key('NOEUD_ANCRAGE') == 1:
        motscles['DEFI_CABLE'].append( _F(GROUP_MA=i['GROUP_MA'],
                                          NOEUD_ANCRAGE=i['NOEUD_ANCRAGE'],
                                          **motscle3 ), )

      if i.has_key('MAILLE') == 1 and i.has_key('GROUP_NO_ANCRAGE') == 1:
        motscles['DEFI_CABLE'].append( _F(MAILLE=i['MAILLE'],
                                          GROUP_NO_ANCRAGE=i['GROUP_NO_ANCRAGE'],
                                          **motscle3 ), )

      if i.has_key('MAILLE') == 1 and i.has_key('NOEUD_ANCRAGE') == 1:
        motscles['DEFI_CABLE'].append( _F(MAILLE=i['MAILLE'],
                                          NOEUD_ANCRAGE=i['NOEUD_ANCRAGE'],
                                          **motscle3 ), )



# FIN BOUCLE sur i in DEFI_CABLE

     
  # LANCEMENT DE DEFI_CABLE_BP
#    TRAITEMENT DE LA RELAXATION 
  
  if TYPE_RELAX=='ETCC_DIRECT':
    motscles['NBH_RELAX']=args['NBH_RELAX'];

  if TYPE_RELAX=='ETCC_REPRISE':
    motscles['NBH_RELAX']=args['NBH_RELAX'];

  if TYPE_RELAX=='BPEL':
    motscles['R_J']=args['R_J'];
      
#  if PERT_ELAS=='OUI':
#    motscles['ESP_CABLE']=args['ESP_CABLE'] ;
#    motscles['EP_BETON']=args['EP_BETON'] ; 
      
    
#    dRelaxation=RELAXATION[0].cree_dict_valeurs(RELAXATION[0].mc_liste)
#    for i in dRelaxation.keys():
#      if dRelaxation[i]==None : del dRelaxation[i]
#  if TYPE_RELAX!='SANS':

  __DC=DEFI_CABLE_OP(MODELE=MODELE,
                       CHAM_MATER=CHAM_MATER,
                       CARA_ELEM=CARA_ELEM,
                       GROUP_MA_BETON=GROUP_MA_BETON,
                       TYPE_ANCRAGE=TYPE_ANCRAGE,
                       TENSION_INIT=TENSION_INIT,
                       RECUL_ANCRAGE=RECUL_ANCRAGE,
                       TYPE_RELAX=TYPE_RELAX,
                     #  RELAXATION=dRelaxation,
                       INFO=INFO,
                       **motscles
                       );

#  else:

#    __DC=DEFI_CABLE_OP(MODELE=MODELE,
#                       CHAM_MATER=CHAM_MATER,
#                       CARA_ELEM=CARA_ELEM,
#                       GROUP_MA_BETON=GROUP_MA_BETON,
#                       TYPE_ANCRAGE=TYPE_ANCRAGE,
#                       TENSION_INIT=TENSION_INIT,
#                       RECUL_ANCRAGE=RECUL_ANCRAGE,
#                       PERT_ELAS=PERT_ELAS,
#                       INFO=INFO,
#                       **motscles
#                       );

  return ier
