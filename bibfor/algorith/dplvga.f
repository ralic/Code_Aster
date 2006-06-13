      SUBROUTINE DPLVGA(YATE,RHO11,RHO12,R,T,KH,CONGEM,DIMCON,ADCP11,
     +                  ADCP12,NDIM,PADP,DP11P1,DP11P2,DP12P1,DP12P2,
     +                  DP21P1,DP21P2,DP11T,DP12T,DP21T)
      IMPLICIT      NONE
      INTEGER       YATE,ADCP11,ADCP12,NDIM,DIMCON
      REAL*8        RHO11,RHO12,R,T,KH,CONGEM(DIMCON),PADP
      REAL*8        DP11P1,DP11P2,DP12P1,DP12P2,DP21P1,DP21P2
      REAL*8        DP11T,DP12T,DP21T
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
C TOLE CRP_21
C ======================================================================
C --- CALCUL DES DERIVEES PARTIELLES DES PRESSIONS ---------------------
C --- UNIQUEMENT DANS LE CAS LIQU_AD_GAZ_VAPE --------------------------
C ======================================================================
      REAL*8        L
      DP11P1 = 1/((RHO12*R*T/RHO11/KH)-1)
      DP11P2 = (R*T/KH - 1)/((RHO12*R*T/RHO11/KH)-1)
      DP12P1 = 1/(R*T/KH-(RHO11/RHO12))
      DP12P2 = (R*T/KH-1)*DP12P1
      DP21P1 = - DP12P1
      DP21P2 = 1 - DP12P2
CC      DP22P1 = -1- DP11P1
CC      DP22P2 = 1- DP11P2
      IF ((YATE.EQ.1)) THEN
         L = (CONGEM(ADCP12+NDIM+1)-CONGEM(ADCP11+NDIM+1))
         DP11T = (-L*R*RHO12/KH+PADP/T)/((RHO12*R*T/RHO11/KH)-1)
         DP12T = (-L*RHO11+PADP)/T*DP12P1
         DP21T =  - DP12T
CC         DP22T =  - DP11T
      ENDIF
C ======================================================================
      END
