#@ MODIF defi_cable_bp_ops Macro  DATE 19/01/2004   AUTEUR DURAND C.DURAND 
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
# RESPONSABLE ASSIRE A.ASSIRE

# ===========================================================================
#           CORPS DE LA MACRO "DEFI_CABLE_BP"
#           -------------------------------------
# USAGE :
# Entrée :
#  - MAILLAGE
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

def defi_cable_bp_ops(self,MAILLAGE,MODELE,CHAM_MATER,CARA_ELEM,GROUP_MA_BETON,
                           DEFI_CABLE,TYPE_ANCRAGE,TENSION_INIT,RECUL_ANCRAGE,
                           RELAXATION,CONE,TITRE,INFO,**args):

  """
     Ecriture de la macro DEFI_CABLE_BP
  """
  from Accas import _F
  import aster,string
  ier=0

  # On importe les definitions des commandes a utiliser dans la macro
  DEFI_GROUP      = self.get_cmd('DEFI_GROUP')
  IMPR_RESU       = self.get_cmd('IMPR_RESU')
  DEFI_CABLE_OP   = self.get_cmd('DEFI_CABLE_OP')
  RECU_TABLE      = self.get_cmd('RECU_TABLE')
  IMPR_TABLE      = self.get_cmd('IMPR_TABLE')
  IMPR_CO         = self.get_cmd('IMPR_CO')

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
    dCONE=CONE.cree_dict_valeurs(CONE.mc_liste)
    for i in dCONE.keys():
      if dCONE[i]==None : del dCONE[i]

    RAYON    = dCONE['RAYON']
    LONGUEUR = dCONE['LONGUEUR']
    
    motscles['CONE']=[]
    motscles['CONE'].append( dCONE )


  # VERIFICATION QUE LE MAILLAGE EST COHERENT AVEC LE MODELE

    __MAIL = aster.getvectjev( string.ljust(MODELE.nom,8) + '.MODELE    .NOMA        ' )
    if string.ljust(MAILLAGE.nom,8) != __MAIL[0] :
      self.DeclareOut(MAILLAGE.nom,maillage)
      print ' '
      print '  # ---------------------------------------------------------------------------'
      print '  # DEFI_CABLE_BP - Erreur : LE CONCEPT MAILLAGE RENSEIGNE NE CORRESPOND'
      print '  #                            PAS A CELUI UTILISE DANS LE MODELE !'
      print '  # ',MAILLAGE.nom,' - ',__MAIL[0]
      print '  # ---------------------------------------------------------------------------'
      print ' '
      ier=ier+1
      self.cr.fatal("""<F> <DEFI_CABLE_BP> Erreur dans la macro""")
      return ier


  # DEFINITION DU NOM DES GROUP_NO

    __NOM = '_AN_'
    __LGNO = MAILLAGE.LIST_GROUP_NO()
    __LGN1 = []
    for i in __LGNO : 
      __LGN1.append( i[0][:len(__NOM)] )
  
    __NB  = __LGN1.count(__NOM)

# FIN RECUPERATION DES INFOS DONNEES PAR LE MOT-CLE "CONE"


  # RECUPERATION DES INFOS DONNEES PAR LE MOT-CLE "DEFI_CABLE"

  if DEFI_CABLE.__class__.__name__=='MCList' :
     dDEFI_CABLE=[]
     for j in DEFI_CABLE :
         dDEFI_CABLE.append(j.cree_dict_valeurs(j.mc_liste))
         for i in dDEFI_CABLE[-1].keys():
             if dDEFI_CABLE[-1][i]==None : del dDEFI_CABLE[-1][i]

  elif DEFI_CABLE.__class__.__name__=='MCFACT' :
     dDEFI_CABLE=[]
     dDEFI_CABLE.append(DEFI_CABLE.cree_dict_valeurs(DEFI_CABLE.mc_liste))
     for i in dDEFI_CABLE[-1].keys():
         if dDEFI_CABLE[-1][i]==None : del dDEFI_CABLE[-1][i]


  # BOUCLE SUR LES FACTEURS DU MOT-CLE "DEFI_CABLE"

  motscles['DEFI_CABLE']=[]

  for i in dDEFI_CABLE:
    
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
          motscle2= {'CREA_GROUP_NO': [{'LONGUEUR': LONGUEUR, 'RAYON': RAYON, 'OPTION': 'TUNNEL', 'GROUP_MA': [ GROUP_MA_BETON, __CAB ], 'GROUP_MA_AXE': __CAB, 'NOM': __NOM1}]}
        if i.has_key('MAILLE') == 1: 
          print ' '
          print '  # ---------------------------------------------------------------------------'
          print '  # DEFI_CABLE_BP - Erreur : MAILLE INTERDIT - UTILISER GROUP_MA'
          print '  # ---------------------------------------------------------------------------'
          print ' '
          ier=ier+1
          self.cr.fatal("""<F> <DEFI_CABLE_BP> Erreur dans la macro""")
          return ier
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
          motscle2= {'CREA_GROUP_NO': [{'LONGUEUR': LONGUEUR, 'RAYON': RAYON, 'OPTION': 'TUNNEL', 'GROUP_MA': [ GROUP_MA_BETON, __CAB ], 'GROUP_MA_AXE': __CAB, 'NOM': __NOM2}]}
        if i.has_key('MAILLE') == 1: 
          print ' '
          print '  # ---------------------------------------------------------------------------'
          print '  # DEFI_CABLE_BP - Erreur : MAILLE INTERDIT - UTILISER GROUP_MA'
          print '  # ---------------------------------------------------------------------------'
          print ' '
          ier=ier+1
          self.cr.fatal("""<F> <DEFI_CABLE_BP> Erreur dans la macro""")
          return ier
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
                                            GROUP_NO_FUT=( __NOM1,__NOM2, ), ), )
        if i.has_key('GROUP_MA') == 1 and i.has_key('NOEUD_ANCRAGE') == 1: 
          motscles['DEFI_CABLE'].append( _F(GROUP_MA=i['GROUP_MA'],
                                            NOEUD_ANCRAGE=i['NOEUD_ANCRAGE'],
                                            GROUP_NO_FUT=( __NOM1,__NOM2, ), ), )
  
      if dCONE['PRESENT'][0] == 'OUI' and dCONE['PRESENT'][1] == 'NON':
        if i.has_key('GROUP_MA') == 1 and i.has_key('GROUP_NO_ANCRAGE') == 1: 
          motscles['DEFI_CABLE'].append( _F(GROUP_MA=i['GROUP_MA'],
                                            GROUP_NO_ANCRAGE=i['GROUP_NO_ANCRAGE'],
                                            GROUP_NO_FUT=( __NOM1, ), ), )
        if i.has_key('GROUP_MA') == 1 and i.has_key('NOEUD_ANCRAGE') == 1: 
          motscles['DEFI_CABLE'].append( _F(GROUP_MA=i['GROUP_MA'],
                                            NOEUD_ANCRAGE=i['NOEUD_ANCRAGE'],
                                            GROUP_NO_FUT=( __NOM1, ), ), )
  
      if dCONE['PRESENT'][0] == 'NON' and dCONE['PRESENT'][1] == 'OUI':
        if i.has_key('GROUP_MA') == 1 and i.has_key('GROUP_NO_ANCRAGE') == 1: 
          motscles['DEFI_CABLE'].append( _F(GROUP_MA=i['GROUP_MA'],
                                            GROUP_NO_ANCRAGE=i['GROUP_NO_ANCRAGE'],
                                            GROUP_NO_FUT=( __NOM2, ), ), )
        if i.has_key('GROUP_MA') == 1 and i.has_key('NOEUD_ANCRAGE') == 1: 
          motscles['DEFI_CABLE'].append( _F(GROUP_MA=i['GROUP_MA'],
                                            NOEUD_ANCRAGE=i['NOEUD_ANCRAGE'],
                                            GROUP_NO_FUT=( __NOM2, ), ), )
 
      if dCONE['PRESENT'][0] == 'NON' and dCONE['PRESENT'][1] == 'NON':
        if i.has_key('GROUP_MA') == 1 and i.has_key('GROUP_NO_ANCRAGE') == 1: 
          motscles['DEFI_CABLE'].append( _F(GROUP_MA=i['GROUP_MA'],
                                            GROUP_NO_ANCRAGE=i['GROUP_NO_ANCRAGE'], ), )
        if i.has_key('GROUP_MA') == 1 and i.has_key('NOEUD_ANCRAGE') == 1: 
          motscles['DEFI_CABLE'].append( _F(GROUP_MA=i['GROUP_MA'],
                                            NOEUD_ANCRAGE=i['NOEUD_ANCRAGE'], ), )


    # CAS OU L'ON A PAS DEFINI LE MOT-CLE "CONE"
    else:
      if i.has_key('GROUP_MA') == 1 and i.has_key('GROUP_NO_ANCRAGE') == 1: 
        motscles['DEFI_CABLE'].append( _F(GROUP_MA=i['GROUP_MA'],
                                          GROUP_NO_ANCRAGE=i['GROUP_NO_ANCRAGE'], ), )

      if i.has_key('GROUP_MA') == 1 and i.has_key('NOEUD_ANCRAGE') == 1: 
        motscles['DEFI_CABLE'].append( _F(GROUP_MA=i['GROUP_MA'],
                                          NOEUD_ANCRAGE=i['NOEUD_ANCRAGE'], ), )

      if i.has_key('MAILLE') == 1 and i.has_key('GROUP_NO_ANCRAGE') == 1: 
        motscles['DEFI_CABLE'].append( _F(MAILLE=i['MAILLE'],
                                          GROUP_NO_ANCRAGE=i['GROUP_NO_ANCRAGE'], ), )

      if i.has_key('MAILLE') == 1 and i.has_key('NOEUD_ANCRAGE') == 1: 
        motscles['DEFI_CABLE'].append( _F(MAILLE=i['MAILLE'],
                                          NOEUD_ANCRAGE=i['NOEUD_ANCRAGE'], ), )


# FIN BOUCLE sur i in DEFI_CABLE


  # LANCEMENT DE DEFI_CABLE_BP

  if RELAXATION:
    dRelaxation=RELAXATION.cree_dict_valeurs(RELAXATION.mc_liste)
    for i in dRelaxation.keys():
      if dRelaxation[i]==None : del dRelaxation[i]

    __DC=DEFI_CABLE_OP(MODELE=MODELE,
                       CHAM_MATER=CHAM_MATER,
                       CARA_ELEM=CARA_ELEM,
                       GROUP_MA_BETON=GROUP_MA_BETON,
                       TYPE_ANCRAGE=TYPE_ANCRAGE,
                       TENSION_INIT=TENSION_INIT,
                       RECUL_ANCRAGE=RECUL_ANCRAGE,
                       RELAXATION=dRelaxation,
                       INFO=INFO,
                       **motscles
                       );

  else:

    __DC=DEFI_CABLE_OP(MODELE=MODELE,
                       CHAM_MATER=CHAM_MATER,
                       CARA_ELEM=CARA_ELEM,
                       GROUP_MA_BETON=GROUP_MA_BETON,
                       TYPE_ANCRAGE=TYPE_ANCRAGE,
                       TENSION_INIT=TENSION_INIT,
                       RECUL_ANCRAGE=RECUL_ANCRAGE,
                       INFO=INFO,
                       **motscles
                       );

#   __TCAB = RECU_TABLE(CO=__DC,NOM_TABLE='CABLE_BP');
#   IMPR_TABLE(TABLE=__TCAB);
 
  return ier
