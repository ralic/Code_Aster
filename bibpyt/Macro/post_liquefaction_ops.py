# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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

#-------------------------------------------------------
# CRITERE DE LIQUEFACTION : [SIP(t)-SIP(t0)]/SIV(t0)
#-------------------------------------------------------

def post_liquefaction_ops(self,AXE,RESU_REF,RESULTAT,INST_REF,**args):

  ier=0
  import aster
  import os,string,types
  from Accas import _F

  # La macro compte pour 1 dans la numerotation des commandes
  self.set_icmd(1)

  ### On importe les definitions des commandes a utiliser dans la macro
  CREA_CHAMP     = self.get_cmd('CREA_CHAMP')
  CREA_RESU      = self.get_cmd('CREA_RESU')
  FORMULE        = self.get_cmd('FORMULE')
  DETRUIRE       = self.get_cmd('DETRUIRE')

 ### RECUPERATION DU MODELE A PARTIR DU RESULTAT

#  modele
  iret, ibid, n_modele = aster.dismoi('MODELE', RESU_REF.nom, 'RESULTAT', 'F')
  __model = self.get_concept(n_modele)

  ### Declaration concept sortant
  self.DeclareOut('LIQ',self.sd)

  ### Extraction du champ SIEF_ELGA a la fin de l'etat reference INST_REF
  __sigini = CREA_CHAMP (OPERATION = 'EXTR',
                 TYPE_CHAM = 'ELGA_SIEF_R',
                         RESULTAT  = RESU_REF, 
                 NOM_CHAM  = 'SIEF_ELGA',
                 INST      = INST_REF,)

  ### Transformation des champs SIP(tref) et SIYY(tref) en champs de type NEUT
  if AXE == 'X': 
    __sig1 = CREA_CHAMP(OPERATION = 'ASSE', 
                TYPE_CHAM = 'ELGA_NEUT_R',
                PROL_ZERO = 'OUI', 
                MODELE    = __model,
                       ASSE      = _F(TOUT         = 'OUI',
                         CHAM_GD      = __sigini,
                              NOM_CMP      = ('SIPXX','SIXX'),
                       NOM_CMP_RESU = ('X1','X2'),),)
  elif AXE == 'Y': 
    __sig1 = CREA_CHAMP(OPERATION = 'ASSE', 
                TYPE_CHAM = 'ELGA_NEUT_R',
                PROL_ZERO = 'OUI', 
                MODELE    = __model,
                       ASSE      = _F(TOUT         = 'OUI',
                         CHAM_GD      = __sigini,
                              NOM_CMP      = ('SIPYY','SIYY'),
                       NOM_CMP_RESU = ('X1','X2'),),)
  elif AXE == 'Z': 
    __sig1 = CREA_CHAMP(OPERATION = 'ASSE', 
                TYPE_CHAM = 'ELGA_NEUT_R',
                PROL_ZERO = 'OUI', 
                MODELE    = __model,
                       ASSE      = _F(TOUT         = 'OUI',
                         CHAM_GD      = __sigini,
                              NOM_CMP      = ('SIPZZ','SIZZ'),
                       NOM_CMP_RESU = ('X1','X2'),),)

  ### Formule pour evaluer le critere de liquefaction (qui vaut 0 si jamais SIYY(tref) vaut 0)
  def fmul0(x,y,z) :
    if abs(y)<=1e-12:
        resu=0.0
    else:
        resu=(z-x)/y
    return resu

  self.update_const_context({'fmul0': fmul0})
          
  __fmul = FORMULE(NOM_PARA = ('X1','X2','X3'), 
               VALE     = 'fmul0(X1,X2,X3)')

  __chfmu = CREA_CHAMP(OPERATION = 'AFFE',
               TYPE_CHAM = 'ELGA_NEUT_F',
               MODELE    = __model,
                    PROL_ZERO = 'OUI',
                       AFFE      = _F(TOUT    = 'OUI',
                      NOM_CMP = 'X4',
                      VALE_F  = __fmul),)

  ###-------------
  ### DEBUT BOUCLE
  ###-------------

  ### Acces aux numeros d'ordre de RESULTAT pour l'indicage de la boucle
  __dico = RESULTAT.LIST_VARI_ACCES()
  __numo = __dico['NUME_ORDRE']
  __n    = __numo[-1]

  ### Initialisation des variables de la boucle
  __liqq  = [None]*(__n+1)
  __sigf = [None]*(__n+1)
  __siff = [None]*(__n+1)
                                
  for i in range(0,__n+1):

     ### Extraction du champ SIEF_ELGA 
    __sigt = CREA_CHAMP (OPERATION  = 'EXTR',
                    TYPE_CHAM  = 'ELGA_SIEF_R',
                            RESULTAT   = RESULTAT, 
                    NOM_CHAM   = 'SIEF_ELGA',
                    NUME_ORDRE = i,)

     ### Transformation du champ SIP(t) en champ de type NEUT
    if AXE == 'X': 
      __sig2 = CREA_CHAMP(OPERATION = 'ASSE', 
               TYPE_CHAM = 'ELGA_NEUT_R',
               MODELE    = __model,
               PROL_ZERO = 'OUI',
                      ASSE      = _F(TOUT         = 'OUI',
                      CHAM_GD      = __sigt,
                            NOM_CMP      = ('SIPXX',),
                     NOM_CMP_RESU = ('X3',),),)
    elif AXE == 'Y': 
      __sig2 = CREA_CHAMP(OPERATION = 'ASSE', 
               TYPE_CHAM = 'ELGA_NEUT_R',
               MODELE    = __model,
               PROL_ZERO = 'OUI',
                      ASSE      = _F(TOUT         = 'OUI',
                      CHAM_GD      = __sigt,
                            NOM_CMP      = ('SIPYY',),
                     NOM_CMP_RESU = ('X3',),),)
    elif AXE == 'Z': 
      __sig2 = CREA_CHAMP(OPERATION = 'ASSE', 
               TYPE_CHAM = 'ELGA_NEUT_R',
               MODELE    = __model,
               PROL_ZERO = 'OUI',
                      ASSE      = _F(TOUT         = 'OUI',
                      CHAM_GD      = __sigt,
                            NOM_CMP      = ('SIPZZ',),
                     NOM_CMP_RESU = ('X3',),),)

     ### Assemblage de SIP(t0),SIYY(t0) et SIP(t) dans le meme champ SIG
    __sig = CREA_CHAMP(OPERATION = 'ASSE',
                    MODELE    = __model, 
               TYPE_CHAM ='ELGA_NEUT_R',
                    ASSE      = (_F(CHAM_GD = __sig1 , 
                       TOUT ='OUI',
                                     CUMUL='OUI', 
                       COEF_R = 1.),
                             _F(CHAM_GD = __sig2 ,
                       TOUT ='OUI',
                         CUMUL='OUI', 
                       COEF_R = 1.),),)
   
     ### Calcul du critere de liquefaction  
    __liqq[i] = CREA_CHAMP(OPERATION = 'EVAL', 
                     TYPE_CHAM = 'ELGA_NEUT_R',
                             CHAM_F    = __chfmu,
                     CHAM_PARA = (__sig,),)
  
     ### Creation d'un champ contenant le resultat du calcul 
    __sigf[i] = CREA_CHAMP(OPERATION = 'ASSE', 
                TYPE_CHAM = 'ELGA_NEUT_R',
                MODELE    = __model,
                PROL_ZERO = 'OUI',
                       ASSE      = _F(TOUT         = 'OUI',
                      CHAM_GD      = __liqq[i],
                             NOM_CMP      = ('X4',),
                      NOM_CMP_RESU = ('X4',),),)

     ### Transformer le champ SIGF de type NEUT en champ de type SIEF_ELGA (sous la variable SIP)
    if AXE == 'X': 
      __siff[i] = CREA_CHAMP(OPERATION = 'ASSE', 
                  TYPE_CHAM = 'ELGA_SIEF_R',
                  PROL_ZERO = 'OUI', 
                  MODELE    = __model,
                         ASSE      = _F(TOUT         = 'OUI',
                        CHAM_GD      = __sigf[i],
                               NOM_CMP      = ('X4',),
                        NOM_CMP_RESU = ('SIPXX',),),)
    if AXE == 'Y': 
      __siff[i] = CREA_CHAMP(OPERATION = 'ASSE', 
                  TYPE_CHAM = 'ELGA_SIEF_R',
                  PROL_ZERO = 'OUI', 
                  MODELE    = __model,
                         ASSE      = _F(TOUT         = 'OUI',
                        CHAM_GD      = __sigf[i],
                               NOM_CMP      = ('X4',),
                        NOM_CMP_RESU = ('SIPYY',),),)
    if AXE == 'Z': 
      __siff[i] = CREA_CHAMP(OPERATION = 'ASSE', 
                  TYPE_CHAM = 'ELGA_SIEF_R',
                  PROL_ZERO = 'OUI', 
                  MODELE    = __model,
                         ASSE      = _F(TOUT         = 'OUI',
                        CHAM_GD      = __sigf[i],
                               NOM_CMP      = ('X4',),
                        NOM_CMP_RESU = ('SIPZZ',),),)

  ###-------------
  ### FIN BOUCLE
  ###-------------

  # Acces aux instants de RESULTAT pour creer la nouvelle SD resultat LIQ 
  __numo2 = __dico['INST']

  __liste=[]
  for k in range(0,__n+1):
     __liste.append( _F(CHAM_GD= __siff[k],MODELE=__model,INST = __numo2[k],),)

  LIQ = CREA_RESU(OPERATION = 'AFFE',
          TYPE_RESU = 'EVOL_NOLI',
          NOM_CHAM  = 'SIEF_ELGA',
                  AFFE      = (__liste),)

  return ier
