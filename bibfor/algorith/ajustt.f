      SUBROUTINE AJUSTT (ABSAC,ACSAB,ABSBC,ACSBC,KSI1,KSI2)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 25/10/2004   AUTEUR MABBAS M.ABBAS 
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
C
      IMPLICIT NONE
C
      REAL*8 ABSAC,ACSAB,ABSBC,ACSBC,KSI1,KSI2
C
C ----------------------------------------------------------------------
C ROUTINE APPELEE PAR : PROJTR
C ----------------------------------------------------------------------
C LORSQUE LA PROJECTION NE "TOMBE" PAS A L'INTERIEUR DU TRIANGLE, ON
C LA "RAMENE" SUR LE NOEUD OU L'ARETE LA PLUS PROCHE (ORTHOGONALEMENT).
C
C IN  ABSAC : (AB,AC) / (AC,AC)
C IN  ACSAB : (AB,AC) / (AB,AB)
C IN  ABSBC : (AB,AC) / (BC,BC)
C IN  ACSBC : (AC,BC) / (BC,BC)
C VAR KSI1  : 1ERE COORDONNEE PARAMETRIQUE DE LA "PROJECTION"
C VAR KSI2  : 2NDE COORDONNEE PARAMETRIQUE DE LA "PROJECTION"
C
C ----------------------------------------------------------------------
C     LORSQUE LA PROJECTION TOMBE EN DEHORS DU TRIANGLE, ON LA
C     RAMENE EN UN NOEUD OU SUR UNE ARETE.
C
C                    \ KSI1<0 /
C                     \KSI3<0/                KSI3 = 1 - KSI1 - KSI2
C                      \    /
C                       \  /
C                        \/
C                        /\
C                       /  \
C           KSI1 < 0   /    \   KSI3 < 0
C                     /      \
C                    / KSI1>0 \
C                   /  KSI2>0  \
C                  /   KSI3>0   \
C         _______ /______________\________
C         KSI1<0 /                \  KSI2 < 0
C         KSI2<0/     KSI2 < 0     \ KSI3 < 0
C              /                    \
C
C ----------------------------------------------------------------------
C
      REAL*8 KSI10,KSI20,KSI30
C
C ----------------------------------------------------------------------
C
      KSI10 = KSI1
      KSI20 = KSI2
      KSI30 = 1.D0 - KSI1 - KSI2

      IF (ABS(KSI10).LE.1D-14) KSI10 = 0.D0
      IF (ABS(KSI20).LE.1D-14) KSI20 = 0.D0
      IF (ABS(KSI30).LE.1D-14) KSI30 = 0.D0

C
      IF ((KSI10.LT.0.D0).AND.(KSI20.LT.0.D0)) THEN
         KSI1 = 0.D0
         KSI2 = 0.D0
         GO TO 999
      END IF
C
      IF ((KSI10.LT.0.D0).AND.(KSI30.LT.0.D0)) THEN
         KSI1 = 0.D0
         KSI2 = 1.D0
         GO TO 999
      END IF
C
      IF ((KSI20.LT.0.D0).AND.(KSI30.LT.0.D0)) THEN
         KSI1 = 1.D0
         KSI2 = 0.D0
         GO TO 999
      END IF
C
      IF (KSI10.LT.0.D0) THEN
        KSI1 = 0.D0
        KSI2 = KSI20 + KSI10 * ABSAC
        GO TO 999
      END IF
C
      IF (KSI20.LT.0.D0) THEN
        KSI1 = KSI10 + KSI20 * ACSAB
        KSI2 = 0.D0
        GO TO 999
      END IF
C
      IF (KSI30.LT.0.D0) THEN
        KSI1 = -KSI10 * ABSBC + (1.D0-KSI20) * ACSBC
        KSI2 = 1.D0 - KSI1
        GO TO 999
      END IF

C
 999  CONTINUE
C
C ----------------------------------------------------------------------
C
      END
