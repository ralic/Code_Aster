      FUNCTION CALOR(ALPHA0,K0,T,DT,DEPSV,DP1,DP2,SIGNE,ALP11,ALP12,
     +                                                            COEPS)
      IMPLICIT      NONE
      REAL*8        ALPHA0,K0,T,DT,DEPSV,DP1,DP2,ALP11,ALP12,SIGNE,COEPS
      REAL*8        CALOR
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 31/01/2005   AUTEUR ROMEO R.FERNANDES 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY  
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY  
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR     
C (AT YOUR OPTION) ANY LATER VERSION.                                   
C                                                                       
C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT   
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF            
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU      
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.                              
C                                                                       
C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE     
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,         
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C ======================================================================
C --- CALCUL DE LA CHALEUR REDUITE Q' SELON FORMULE DOCR ---------------
C ======================================================================
      CALOR = ALPHA0*3.0D0*K0*(T-DT/2.0D0)*DEPSV
     +               + 3.0D0*ALP11*(T-DT/2.0D0)*SIGNE*DP1
     +               - 3.0D0*(ALP11+ALP12)*(T-DT/2.D0)*DP2
     +               + COEPS*DT
C ======================================================================
      END
