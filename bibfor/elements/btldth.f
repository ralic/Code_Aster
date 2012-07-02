      SUBROUTINE BTLDTH(FAMI,XI3,NB1,KPG,BTILD,WGT,INDIC,YOUNG,NU,
     &                  ALPHA,TEMPER,FORTHI)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C
      IMPLICIT NONE
C
      INCLUDE 'jeveux.h'
      INTEGER NB1,KPG
      REAL*8 WGT,YOUNG,NU,ALPHA,XI3
      REAL*8 BTILD(5,42),FORTHI(1),VECTHR(2)
      INTEGER JCOU,IMOY,IADZI,IAZK24
      CHARACTER*24 VALK(3)
      CHARACTER*8 NOMMAI
      CHARACTER*4 FAMI
      REAL*8 P1XI3,P2XI3,P3XI3
C
C
C     CALCUL DE TEMPERATURE AUX PTS D'INTEGRATION
C
C
C-----------------------------------------------------------------------
      INTEGER I ,INDIC ,IRET1 ,IRET2 ,IRET3 ,IRET4 ,K 

      REAL*8 R8NNEM ,TEMPER ,TINF ,TMOY ,TREF ,TSUP 
C-----------------------------------------------------------------------
      P1XI3= 1-XI3*XI3
      P2XI3=-XI3*(1-XI3)/2.D0
      P3XI3= XI3*(1+XI3)/2.D0
      CALL JEVECH('PNBSP_I','L',JCOU)
      IMOY=(3*ZI(JCOU)+1)/2
      CALL RCVARC(' ','TEMP','REF',FAMI,1,1,TREF,IRET1)
      CALL RCVARC(' ','TEMP','+',FAMI,KPG,1,TINF,IRET2)
      CALL RCVARC(' ','TEMP','+',FAMI,KPG,IMOY,TMOY,IRET3)
      CALL RCVARC(' ','TEMP','+',FAMI,KPG,3*ZI(JCOU),TSUP,IRET4)
      IF ((IRET2+IRET3+IRET4).EQ.0) THEN
        IF ((IRET1.EQ.1).OR.(INDIC.EQ.0)) THEN
          CALL TECAEL(IADZI,IAZK24)
          NOMMAI=ZK24(IAZK24-1+3)(1:8)
          VALK(1)=NOMMAI
          VALK(2)='TEMP_REF'
          CALL U2MESK('F','CALCULEL_32',2,VALK)
        ELSE
          TEMPER=(TMOY*P1XI3+TINF*P2XI3+TSUP*P3XI3)-TREF
        ENDIF
      ELSE
        TEMPER = R8NNEM()
      ENDIF
C
C
      IF (INDIC.EQ.1) THEN
C
      VECTHR(1)=YOUNG*ALPHA*TEMPER/(1.D0-NU)
      VECTHR(2)=VECTHR(1)
C
C     CONSTRUCTION DES EFFORTS DUS AUX DILATATIONS THERMIQUES
C
      DO 30 I=1,5*NB1+2
         FORTHI(I)=0.D0
      DO 40 K=1,2
         FORTHI(I)=FORTHI(I)+BTILD(K,I)*VECTHR(K)*WGT
C        FORTHI(I)=FORTHI(I)+BTILD(K,I)*VECTHR(K)
 40   CONTINUE
 30   CONTINUE
C
      ENDIF
C
C
      END
