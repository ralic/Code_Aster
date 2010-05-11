#@ MODIF defi_fonc_elec_ops Macro  DATE 11/05/2010   AUTEUR COURTOIS M.COURTOIS 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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

from math import cos,exp,pi

def FcompletGR1(T,I1,I2,FR,TR,PHI1,PHI2,TAU1,TAU2) :
    fxt= 4.E-7 * I1 * I2
    fxt= fxt * (cos(2*pi*FR*(T-TR)+PHI1*pi/180.)-exp(-(T-TR)/TAU1)*cos(PHI1*pi/180.))
    fxt= fxt * (cos(2*pi*FR*(T-TR)+PHI2*pi/180.)-exp(-(T-TR)/TAU2)*cos(PHI2*pi/180.))
    return fxt

def FcontinuGR1(T,I1,I2,TR,PHI1,PHI2,TAU1,TAU2) :
    ft1= exp(-(T-TR)*(1./TAU1+1./TAU2))
    ft1= ft1*cos(PHI1*pi/180.)*cos(PHI2*pi/180.)
    ft1= ft1+0.5*cos(PHI2*pi/180.-PHI1*pi/180.)
    fxt= 4.E-7 * I1 * I2 * ft1
    return fxt

def FcompletGR2(T,I1,I2,FR,TR,PHI1,PHI2,TAU1,TAU2,D) :
    fxt= 4.E-7 * I1 * I2 / D
    fxt= fxt * (cos(2*pi*FR*(T-TR)+PHI1*pi/180.)-exp(-(T-TR)/TAU1)*cos(PHI1*pi/180.))
    fxt= fxt * (cos(2*pi*FR*(T-TR)+PHI2*pi/180.)-exp(-(T-TR)/TAU2)*cos(PHI2*pi/180.))
    return fxt

def FcontinuGR2(T,I1,I2,TR,PHI1,PHI2,TAU1,TAU2,D) :
    ft1= exp(-(T-TR)*(1./TAU1+1./TAU2))
    ft1= ft1*cos(PHI1*pi/180.)*cos(PHI2*pi/180.)
    ft1= ft1+0.5*cos(PHI2*pi/180.-PHI1*pi/180.)
    fxt= 4.E-7 * I1 * I2 * ft1 / D
    return fxt

## fonction post réenclenchement, valable entre l'instant de réenclenchement et l'instant de fin de réenclenchement. Sinon 0.
def FcompletGR2R(T,I1R,I2R,FR,TRR,PHIR1,PHI2R,TAU1R,TAU2R,D) :
    fxt= 4.E-7 * I1R * I2R / D
    fxt= fxt * (cos(2*pi*FR*(T-TRR)+PHI1R*pi/180.)-exp(-(T-TRR)/TAU1R)*cos(PHI1R*pi/180.))
    fxt= fxt * (cos(2*pi*FR*(T-TRR)+PHI2R*pi/180.)-exp(-(T-TRR)/TAU2R)*cos(PHI2R*pi/180.))
    return fxt

## fonction post réenclenchement, valable entre l'instant de réenclenchement et l'instant de fin de réenclenchement. Sinon 0.
def FcontinuGR2R(T,I1R,I2R,TRR,PHI1R,PHI2R,TAU1R,TAU2R,D) :
    ft1= exp(-(T-TRR)*(1./TAU1R+1./TAU2R))
    ft1= ft1*cos(PHI1R*pi/180.)*cos(PHI2R*pi/180.)
    ft1= ft1+0.5*cos(PHI2R*pi/180.-PHI1R*pi/180.)
    fxt= 4.E-7 * I1R * I2R * ft1 / D
    return fxt



def defi_fonc_elec_ops(self,FREQ,SIGNAL,COUR,COUR_PRIN,COUR_SECO,**args):
        ier=0
        from Utilitai.Utmess     import  UTMESS
        import numpy

        # On importe les definitions des commandes a utiliser dans la macro
        # Le nom de la variable doit etre obligatoirement le nom de la commande
        DEFI_FONCTION     =self.get_cmd('DEFI_FONCTION')
        FORMULE           =self.get_cmd('FORMULE')
        CALC_FONC_INTERP  =self.get_cmd('CALC_FONC_INTERP')

        ### Comptage commandes + déclaration concept sortant
        self.set_icmd(1)
        self.DeclareOut('C_out',self.sd)
#
        if COUR : 
                TINI =COUR[ 0]['INST_CC_INIT']
                TFIN =COUR[-1]['INST_CC_FIN']
                pas_t=1./(40.*FREQ)
#
                temps   = []
                fff     = []
#
                T2moins = COUR[0]['INST_CC_FIN']
                TR      = COUR[0]['INST_CC_INIT']
                premier = 1
                for k_cour in COUR :
                        I1   = k_cour['INTE_CC_1']
                        I2   = k_cour['INTE_CC_2']
                        PHI1 = k_cour['PHI_CC_1']
                        PHI2 = k_cour['PHI_CC_2']
                        TAU1 = k_cour['TAU_CC_1']
                        TAU2 = k_cour['TAU_CC_2']
                        T1   = k_cour['INST_CC_INIT']
                        T2   = k_cour['INST_CC_FIN']
                        if (abs(T1-T2moins)<1.E-7) : pass
                        elif (premier==1)          : pass
                        else :
                                TR=T1
                                temps.append(T2moins)
                                fff.append(0.)
                                T2moins=T2
                        premier=0
                        t_k_cour=numpy.arange((T2-T1)/pas_t)
                        t_k_cour=t_k_cour*pas_t
                        t_k_cour=t_k_cour+T1
                        t_k_cour=t_k_cour.tolist()
                        print T1,T2,FREQ
                        temps=temps+t_k_cour
                        if   SIGNAL=='CONTINU' :
                                for t in t_k_cour :
                                  fff.append( FcontinuGR1(t,I1,I2,TR,PHI1,PHI2,TAU1,TAU2) )
                        elif SIGNAL=='COMPLET' :
                                for t in t_k_cour :
                                  fff.append(FcompletGR1(t,I1,I2,FREQ,TR,PHI1,PHI2,TAU1,TAU2))
#
        elif COUR_PRIN :
                TINI =COUR_PRIN[0]['INST_CC_INIT']
                TFIN =COUR_PRIN[0]['INST_CC_FIN']
#
                TINIR = COUR_PRIN[0]['INST_RENC_INIT']
                TFINR = COUR_PRIN[0]['INST_RENC_FIN']
#
                pas_t=1./(40.*FREQ)
#
                temps   = []
                fff     = []
                T2moins = max(TFIN,TFINR)
                TR      = COUR_PRIN[0]['INST_CC_INIT']
                TRR     = COUR_PRIN[0]['INST_RENC_INIT']
                I1      = COUR_PRIN[0]['INTE_CC_1']
                I1R     = COUR_PRIN[0]['INTE_RENC_1']
                PHI1    = COUR_PRIN[0]['PHI_CC_1']
                PHI1R   = COUR_PRIN[0]['PHI_RENC_1']
                TAU1    = COUR_PRIN[0]['TAU_CC_1']
                TAU1R   = COUR_PRIN[0]['TAU_RENC_1']
#
                fff.append(0.)
#
                if (abs(TR-T2moins)<1.E-7) : pass
                else :
                        temps.append(0)
                        t_k_cour=numpy.arange((T2moins-TR)/pas_t)
                        t_k_cour=t_k_cour*pas_t
                        t_k_cour=t_k_cour+TR
                        t_k_cour=t_k_cour.tolist()
                        temps=temps+t_k_cour
#
                for k_cour in COUR_SECO :
                        I2    = k_cour['INTE_CC_2']
                        PHI2  = k_cour['PHI_CC_2']
                        TAU2  = k_cour['TAU_CC_2']
                        I2R   = k_cour['INTE_RENC_2']
                        PHI2R = k_cour['PHI_RENC_2']
                        TAU2R = k_cour['TAU_RENC_2']
                        DIST  = k_cour['DIST']
#
                        if SIGNAL=='CONTINU' :
                           for i in range(len(temps)) :
                               if temps[i]>TINI :
                                  if temps[i]<TFIN :
                                     fff[i]=fff[i]+FcontinuGR2(temps[i],I1,I2,TR,PHI1,PHI2,TAU1,TAU2,DIST)
                               if temps[i]>TINIR :
                                  if temps[i]<TFINR : 
                                     fff[i]=fff[i]+FcontinuGR2R(temps[i],I1R,I2R,TRR,PHI1R,PHI2R,TAU1R,TAU2R,DIST)
#
                        if SIGNAL=='COMPLET' :
                           for i in range(len(temps)) :
                               if temps[i]>TINI :
                                  if temps[i]<TFIN :
                                     fff[i]=fff[i]+FcompletGR2(temps[i],I1,I2,TR,PHI1,PHI2,TAU1,TAU2,DIST)
                               if temps[i]>TINIR :
                                  if temps[i]<TFINR : 
                                     fff[i]=fff[i]+FcompletGR2R(temps[i],I1R,I2R,TRR,PHI1R,PHI2R,TAU1R,TAU2R,DIST)
#
        vale=[]
        for i in range(len(temps)) :
              vale.append(temps[i])
              vale.append(fff[i])
        vale.append(temps[-1]+2*pas_t)
        vale.append(0.)
#
        C_out=DEFI_FONCTION(NOM_PARA='INST',NOM_RESU='ELEC',
                            VALE=vale,
                            PROL_DROITE='CONSTANT',
                            PROL_GAUCHE='CONSTANT',)
        return ier
