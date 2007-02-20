      SUBROUTINE LCROHY(X,DP,EM,EP)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 20/02/2007   AUTEUR MICHEL S.MICHEL 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT NONE
      REAL*8   DP,EM(6),EP(6),X
      
C ----------------------------------------------------------------------
C                          LOI DE ROUSSELIER
C         CORRECTION HYDROSTATIQUE DE LA DEFORMATION ELASTIQUE
C ----------------------------------------------------------------------
C IN  X       TRACE DE LA DEFORMATION (INCONNUE PRINCIPALE DE L'INTEG.)
C IN  DP      INCREMENT DE DEFORMATION PLASTIQUE CUMULEE
C IN  EM      DEFORMATION ELASTIQUE AU DEBUT DU PAS DE TEMPS
C VAR EP      DEFORMATION ELASTIQUE (XX,YY,ZZ,RAC2*XY,RAC2*XZ,RAC2*YZ)
C ----------------------------------------------------------------------
C  COMMON LOI DE COMPORTEMENT ROUSSELIER

      INTEGER ITEMAX, JPROLP, JVALEP, NBVALP
      REAL*8  PREC,YOUNG,NU,ALPHA,SIGY,SIG1,ROUSD,F0,FCR,ACCE
      REAL*8  PM,RPM,FONC,FCD,DFCDDJ,DPMAXI
      COMMON /LCROU/ PREC,YOUNG,NU,ALPHA,SIGY,SIG1,ROUSD,F0,FCR,ACCE,
     &               PM,RPM,FONC,FCD,DFCDDJ,DPMAXI,
     &               ITEMAX, JPROLP, JVALEP, NBVALP
C ----------------------------------------------------------------------
C  COMMON GRANDES DEFORMATIONS CANO-LORENTZ

      INTEGER IND1(6),IND2(6)
      REAL*8  KR(6),RAC2,RC(6)
      REAL*8  LAMBDA,MU,DEUXMU,UNK,TROISK,COTHER
      REAL*8  JM,DJ,JP,DJDF(3,3)
      REAL*8  ETR(6),DVETR(6),EQETR,TRETR,DETRDF(6,3,3)
      REAL*8  DTAUDE(6,6)

      COMMON /GDCLC/
     &          IND1,IND2,KR,RAC2,RC,
     &          LAMBDA,MU,DEUXMU,UNK,TROISK,COTHER,
     &          JM,DJ,JP,DJDF,
     &          ETR,DVETR,EQETR,TRETR,DETRDF,
     &          DTAUDE
C ----------------------------------------------------------------------
C ----------------------------------------------------------------------
      INTEGER IJ
      REAL*8 BEM(6),DETBEM,JELASM,JPLASM,JPLASP,JELASP
C ----------------------------------------------------------------------
      
C    CALCUL DE BE EN T-
      DO 10 IJ = 1,6
        BEM(IJ) = KR(IJ) - 2*EM(IJ)
 10   CONTINUE

      CALL LCDETE(BEM,DETBEM) 
      JELASM = SQRT(DETBEM)
      JPLASM = JM/JELASM
      JPLASP = JPLASM * EXP(X)
      JELASP = JP/JPLASP
      CALL GDCLHY(JELASP,EP)

      END
