      SUBROUTINE PRLGMA(MATI,SINA,COSA,SINB,COSB,SING,COSG,MATF)
      IMPLICIT NONE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/12/2006   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C-----------------------------------------------------------------------
C DESCRIPTION : PASSAGE REPERE LOCAL -> REPERE GLOBAL
C -----------
C               APPELANT : MDCHOE
C
C-------------------   DECLARATION DES VARIABLES   ---------------------
C
C ARGUMENTS
C ---------
      REAL*8     MATI(3,3), SINA, COSA, SINB, COSB, SING, COSG,
     &           MATF(3,3)
C
C VARIABLES LOCALES
C -----------------
      INTEGER    I, L, K, J, IER, IPROD
      REAL*8     RX(3,3), RY(3,3), RZ(3,3), RZYX(3,3), P(3), TEMP(3,3)
C
C FONCTIONS INTRINSEQUES
C ----------------------
C     INTRINSIC  ABS
C
C ROUTINES EXTERNES
C -----------------
C     EXTERNAL   PRMAMA
C
C-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
C
      RZ(1,1) =  1.0D0
      RY(2,2) =       RZ(1,1)
      RX(3,3) =       RY(2,2)
      RX(1,1) =  COSA
      RX(1,2) =  SINA
      RX(2,1) = -SINA
      RX(2,2) =  COSA
      RX(3,2) =  0.0D0
      RX(3,1) =       RX(3,2)
      RX(2,3) =       RX(3,1)
      RX(1,3) =       RX(2,3)
      RY(1,1) =  COSB
      RY(1,3) = -SINB
      RY(3,1) =  SINB
      RY(3,3) =  COSB
      RY(3,2) =  0.0D0
      RY(2,3) =       RY(3,2)
      RY(2,1) =       RY(2,3)
      RY(1,2) =       RY(2,1)
      RZ(2,2) =  COSG
      RZ(2,3) =  SING
      RZ(3,2) = -SING
      RZ(3,3) =  COSG
      RZ(3,1) =  0.0D0
      RZ(2,1) =       RZ(3,1)
      RZ(1,3) =       RZ(2,1)
      RZ(1,2) =       RZ(1,3)
C
      DO 10 L = 1, 3
         DO 10 K = 1, 3
            IF ( ABS(RX(K,L)).LT.1.0D-06 ) RX(K,L) = 0.0D0
            IF ( ABS(RY(K,L)).LT.1.0D-06 ) RY(K,L) = 0.0D0
            IF ( ABS(RZ(K,L)).LT.1.0D-06 ) RZ(K,L) = 0.0D0
  10  CONTINUE
C
      DO 20 J = 1, 3
         DO 20 I = 1, 3
            RZYX(I,J) = 0.0D0
            DO 20 K = 1, 3
               P(K) = 0.0D0
               DO 21 L = 1, 3
                  P(K) = P(K) + RZ(I,L)*RY(L,K)
  21           CONTINUE
               RZYX(I,J) = RZYX(I,J) + P(K)*RX(K,J)
  20  CONTINUE
C
      IPROD = 1
      IER = 0
      CALL PRMAMA(IPROD,MATI,3,3,3,RZYX,3,3,3,TEMP,3,3,3,IER)
      IF ( IER.NE.0 )
     &   CALL U2MESS('F','ALGORITH10_2')
C
C                                     SINA = -SINA
C POUR LA TRANSFORMATION INVERSE  =>  SINB = -SINB
C ------------------------------      SING = -SING
C
      RX(1,2)= -SINA
      RX(2,1)=  SINA
C
      RY(1,3)=  SINB
      RY(3,1)= -SINB
C
      RZ(2,3)= -SING
      RZ(3,2)=  SING
C
      DO 30 L = 1, 3
         DO 30 K = 1, 3
            IF ( ABS(RX(K,L)).LT.1.0D-06 ) RX(K,L) = 0.0D0
            IF ( ABS(RY(K,L)).LT.1.0D-06 ) RY(K,L) = 0.0D0
            IF ( ABS(RZ(K,L)).LT.1.0D-06 ) RZ(K,L) = 0.0D0
  30  CONTINUE
C
      DO 40 J = 1, 3
         DO 40 I = 1, 3
            RZYX(I,J) = 0.0D0
            DO 40 K = 1, 3
               P(K) = 0.0D0
               DO 41 L = 1, 3
                  P(K) = P(K) + RX(I,L)*RY(L,K)
  41           CONTINUE
               RZYX(I,J) = RZYX(I,J) + P(K)*RZ(K,J)
  40  CONTINUE
C
      IER = 0
      CALL PRMAMA(IPROD,RZYX,3,3,3,TEMP,3,3,3,MATF,3,3,3,IER)
      IF ( IER.NE.0 )
     &   CALL U2MESS('F','ALGORITH10_2')
C
C --- FIN DE PRLGMA.
      END
