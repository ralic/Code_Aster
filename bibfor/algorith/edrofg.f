      SUBROUTINE EDROFG (Y, DP, S, SEUIL, DSEUIL)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 07/07/2003   AUTEUR ADBHHVV V.CANO 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE ADBHHVV V.CANO

      IMPLICIT NONE
      REAL*8  Y, DP, S, SEUIL, DSEUIL

C **************************************************************
C *       INTEGRATION DE LA LOI DE ROUSSELIER NON LOCAL        *
C *          CALCUL DE DP DU SEUIL ET DE SA DERIVEE            *
C **************************************************************

C IN  Y       : PARAMETRE Y = K X/SIG1
C OUT DP      : INCREMENT DE DEFORMATION PLASTIQUE CUMULEE
C                     DP = Y*SIG1*EXP(Y)/(FONC*K)
C OUT S       : VALEUR DE LA FONCTION S(Y)
C               S(Y)=-SIG1*FONC*EXP(-Y)+R(PM+DP)+C1*(PM+DP)-C2
C OUT SEUIL   : VALEUR DU SEUIL(Y)  
C               SEUIL(Y)=2*MU*EQETR-S(Y)-3*MU*DP(Y)      SI L2<0 ET L1>0
C               SEUIL(Y)=NAGRTR/LB-S(Y)-C3*DP/(LB**2)    SI L1<0 ET L2>0
C               SEUIL(Y)=2*MU*EQETR+3*MU*LB*NAGRTR/C3
C                     -(1+3*MU*LB*LB/C3)*S(Y)-3*MU*DP(Y) SI L1>0 ET L2>0
C OUT DSEUIL  : DERIVEE DU SEUIL PAR RAPPORT A Y

      INTEGER ITEMAX, JPROLP, JVALEP, NBVALP
      REAL*8  YOUNG,NU,MU,K,SIGY
      REAL*8  SIG1,D,F0,FCR,ACCE
      REAL*8  FONC,EQETR,PM,RPM,PENTEM,PENTE,PREC
      REAL*8  C1,C2,C3,C4(3),LB,LC
      COMMON /EDROU/ YOUNG,NU,MU,K,SIGY,
     &               SIG1,D,F0,FCR,ACCE,
     &               FONC,EQETR,PM,RPM,PENTEM,PENTE,PREC,
     &               C1,C2,C3,C4,LB,LC,
     &               ITEMAX, JPROLP, JVALEP, NBVALP

      REAL*8  DS,DDP

C 1 - CALCULE DE DP - S ET LA DERIVEE DE S 

      CALL EDROFS(Y, DP, S, DS)      
           
C 3 - CALCUL DU SEUIL ET DE SA DERIVEE   
      
      DDP = SIG1*(1+Y)*EXP(Y)/(K*FONC)      
      SEUIL=2.D0*MU*EQETR - S - 3.D0*MU*DP
      DSEUIL=- DS - 3.D0*MU*DDP       
                  
      END 
