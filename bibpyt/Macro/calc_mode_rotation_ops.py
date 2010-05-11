#@ MODIF calc_mode_rotation_ops Macro  DATE 11/05/2010   AUTEUR COURTOIS M.COURTOIS 

#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2009  EDF R&D                  WWW.CODE-ASTER.ORG
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
# RESPONSABLE Mohamed TORKHANI

import aster 
from Accas import _F
from types import ListType, TupleType


def calc_mode_rotation_ops(self,MATR_A, MATR_B, MATR_AMOR, MATR_GYRO,
                         VITE_ROTA,METHODE,CALC_FREQ,VERI_MODE,**args):
# Macro pour calculer les frequences et modes en fonction des vitesses de rotation
# MATR_A, matrice de raideur
# MATR_B, matrice de masse
# MATR_AMOR, matrice d'amortissement
# MATR_GYRO, matrice de gyroscopie
# VITE_ROTA, liste de vitesses de rotation
# METHODE, methode de calcul, QZ par defaut ou SORENSEN
# CALC_FREQ
# VERI_MODE
    from Utilitai.Table import Table
    ier=0
     
    # On importe les definitions des commandes a utiliser dans la macro
    MODE_ITER_SIMULT  =self.get_cmd('MODE_ITER_SIMULT')
    COMB_MATR_ASSE    =self.get_cmd('COMB_MATR_ASSE')
    IMPR_RESU         =self.get_cmd('IMPR_RESU')
    DETRUIRE          =self.get_cmd('DETRUIRE')
    CREA_TABLE        =self.get_cmd('CREA_TABLE')

    # La macro compte pour 1 dans la numerotation des commandes
    self.set_icmd(1)

    motscit={}
    if METHODE=='QZ':
        motscit['CALC_FREQ']=_F(OPTION  ='PLUS_PETITE',
                             NMAX_FREQ  =CALC_FREQ['NMAX_FREQ'])                         
        
    if METHODE=='SORENSEN':        
        motscit['CALC_FREQ']=_F(OPTION  ='CENTRE',
                             NMAX_FREQ  =CALC_FREQ['NMAX_FREQ'],
                             FREQ       =CALC_FREQ['FREQ'])
    
    motscit['VERI_MODE']=_F(STOP_ERREUR=VERI_MODE['STOP_ERREUR'],
                            SEUIL      =VERI_MODE['SEUIL'],
                            STURM      =VERI_MODE['STURM'],
                            PREC_SHIFT =VERI_MODE['PREC_SHIFT'])

    
    self.DeclareOut('tab_out',self.sd)
    
    NBV=len(VITE_ROTA);
    
    _mod=[None]*NBV;
    
    tab=Table()
    for ii in range(0,NBV):
        OM=VITE_ROTA[ii]
        nom = 'OM_'+str(ii)

        # ----------------------------------
        # Ajout des effets gyroscopiques w*G
        # dans la matrice d amortissement C
        # ----------------------------------

        
        GYOM=COMB_MATR_ASSE(COMB_R=(_F(MATR_ASSE=MATR_GYRO, COEF_R=OM,),
                            _F(MATR_ASSE=MATR_AMOR, COEF_R=1.,),))

        _mod[ii]=MODE_ITER_SIMULT(MATR_A=MATR_A,
                       MATR_B=MATR_B,
                       MATR_C=GYOM,
                       METHODE=METHODE,
                       **motscit)
        
        DETRUIRE(CONCEPT=_F(NOM=GYOM),INFO=1,)
        tab.append({'NUME_VITE' : ii, 'VITE_ROTA' : OM, 'NOM_OBJET': 'MODE_MECA', 'TYPE_OBJET': 'MODE_MECA', 'NOM_SD' : _mod[ii].nom})

    motcles=tab.dict_CREA_TABLE()
    tab_out=CREA_TABLE(TYPE_TABLE = 'TABLE_CONTENEUR', **motcles);
    return ier
