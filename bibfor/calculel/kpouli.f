      SUBROUTINE KPOULI (E,A,NX,L0,L1,L2,NORML1,NORML2,   AMAT)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 05/01/95   AUTEUR G8BHHAC A.Y.PORTABILITE 
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
C ......................................................................
C    - FONCTION REALISEE:  CALCUL MATRICE DE RIGIDITE MEPOULI
      IMPLICIT REAL*8 (A-H,O-Z)
C                          OPTION : 'FULL_MECA        '
C                          OPTION : 'RIGI_MECA_TANG   '
C
C    - ARGUMENTS:
C        DONNEES:
C
C ......................................................................
C
      REAL*8             E,A,NX,L1(3),L2(3),C123(3),C456(3)
      REAL*8             NORML1,NORML2,L0,COEF1,COEF2,COEF3,COEF4,COEF5
      REAL*8             AMAT(*)
      INTEGER            I,J,IMAT
C
C
      COEF1 = (E*A/L0 - NX/NORML1) / NORML1**2
      COEF2 = E*A / (L0*NORML1*NORML2)
      COEF3 = (E*A/L0 - NX/NORML2) / NORML2**2
      COEF4 = NX / NORML1
      COEF5 = NX / NORML2
      IMAT = 0
C
C*** LIGNES 1, 2, 3
C
      DO 12 I=1,3
      DO 11 J=1,I
      IMAT = IMAT + 1
      AMAT(IMAT) = COEF1 * L1(I) * L1(J)
   11 CONTINUE
      AMAT(IMAT) = AMAT(IMAT) + COEF4
   12 CONTINUE
C
C*** LIGNES 4, 5, 6
C
      DO 23 I=1,3
      DO 21 J=1,3
      IMAT = IMAT + 1
      AMAT(IMAT) = COEF2 * L2(I) * L1(J)
   21 CONTINUE
      DO 22 J=1,I
      IMAT = IMAT + 1
      AMAT(IMAT) = COEF3 * L2(I) * L2(J)
   22 CONTINUE
      AMAT(IMAT) = AMAT(IMAT) + COEF5
   23 CONTINUE
C
C*** LIGNES 7, 8, 9
C
      DO 34 I=1,3
      DO 31 J=1,3
      IMAT = IMAT + 1
      AMAT(IMAT) = -COEF1 * L1(I) * L1(J)
     .             -COEF2 * L2(I) * L1(J)
      IF (J.EQ.I) THEN
       AMAT(IMAT) = AMAT(IMAT) - COEF4
      ENDIF
      C123(J) = AMAT(IMAT)
   31 CONTINUE
      DO 32 J=1,3
      IMAT = IMAT + 1
      AMAT(IMAT) = -COEF2 * L1(I) * L2(J)
     .             -COEF3 * L2(I) * L2(J)
      IF (J.EQ.I) THEN
       AMAT(IMAT) = AMAT(IMAT) - COEF5
      ENDIF
      C456(J) = AMAT(IMAT)
   32 CONTINUE
      DO 33 J=1,I
      IMAT = IMAT + 1
      AMAT(IMAT) = -C123(J) - C456(J)
   33 CONTINUE
   34 CONTINUE
      END
