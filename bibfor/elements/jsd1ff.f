      SUBROUTINE JSD1FF(IP,XL,PHIY,PHIZ,B)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER IP
      REAL*8 XL,A,K,B(7,14)
      REAL*8 PHIY,PHIZ
C ......................................................................
C    - FONCTION REALISEE:  CALCUL DES FONCTIONS DE FORME DE DEFORMATIONS
C               GENERALISEES POUTRE 7 DDL A TROIS  POINTS DE GAUSS
C
C    - ARGUMENTS:
C        DONNEES:           IP      -->   POINTS DE GAUSS
C                           XL      -->   LONGUEUR DE L'ELEMENT
C                          PHIY     -->  COEFF DE CISAILLEMENT SUIVANT Y
C                          PHIZ     -->  COEFF DE CISAILLEMENT SUIVANT Z
C
C        RESULTATS:
C                         B     <--  MATRICE D'INTERPOLATION
C
C     DESCRIPTION DE LA NUMEROTATION DU SEG2
C
C       +-----------------+
C       1                 2
C
C    L'ORDRE DE DDL EST  : 1   UX   -SUIVANT L'AXE DE LA POUTRE
C                          2   UY   -I
C                          3   UZ    I DANS LA SECTION
C
C                          4   TX   -.ROTATIONS SUIVANT OX,OY,OZ
C                          5   TY    .
C                          6   TZ    .
C                          7   TX'   .PARAMETRE DE GAUCHISSEMENT
C    DEFORMATIONS        : 1   UX'   .LOGITUDINALE......................
C                          2 UY'-TZ. .CISAILLEMENT OY
C                          3 UZ'+TY. .CISAILLEMENT  OZ
C                          4   TX'   .COURBURE TORSION
C                          5   TY'   .COURBURE FLEXION OY
C                          6   TZ'   .COURBURE FLEXION OZ
C                          7   TX''  .COURBURE GAUCHISSEMENT
C ......................................................................
C
      A = 0.5D0*XL
C
      IF (IP.EQ.1) THEN
          K = -SQRT(0.6D0)
      ELSE IF (IP.EQ.2) THEN
          K = 0.D0
      ELSE IF (IP.EQ.3) THEN
          K = SQRT(0.6D0)
      END IF
      DO 2 I = 1,7
          DO 1 J = 1,14
              B(I,J) = 0
    1     CONTINUE
    2 CONTINUE
C
C                                     TIMOCH
C   UX'
C
      B(1,1) = -0.5D0/A
      B(1,8) = 0.5D0/A
C
C   UY'- TZ
C
      DY = 1.D0/ (1.D0+PHIY)
      B(2,2) = (3.D0*DY*K**2-2.D0-DY)/ (4.D0*A) - 3.D0*DY*
     &                                      (K**2-1.D0)/ (4.D0*A)
      B(2,6) = (3.D0*DY*K**2-2.D0*K-DY)/ (4.D0) -
     +         (3.D0*DY* (K**2-1.D0)+2.D0* (1.D0-K))/4.D0
      B(2,9) = -B(2,2)
      B(2,13) = (3.D0*DY*K**2+2.D0*K-DY)/ (4.D0) -
     +          (3.D0*DY* (K**2-1.D0)+2.D0* (1.D0+K))/4.D0
C
C   UZ' + TY
C
      DZ = 1.D0/ (1.D0+PHIZ)
      B(3,3) = (3.D0*DZ*K**2-2.D0-DZ)/ (4.D0*A) - 3.D0*DZ*
     &                                      (K**2-1.D0)/ (4.D0*A)
      B(3,5) = - (3.D0*DZ*K**2-2.D0*K-DZ)/ (4.D0) +
     +         (3.D0*DZ* (K**2-1.D0)+2.D0* (1.D0-K))/4.D0
      B(3,10) = -B(3,3)
      B(3,12) = - (3.D0*DZ*K**2+2.D0*K-DZ)/ (4.D0) +
     +          (3.D0*DZ* (K**2-1.D0)+2.D0* (1.D0+K))/4.D0
C
C TX'
C
      B(4,4) = (3.D0*K**2-3)/ (4.D0*A)
      B(4,7) = (3.D0*K**2-2.D0*K-1.D0)/ (4.D0)
      B(4,11) = (-3.D0*K**2+3.D0)/ (4.D0*A)
      B(4,14) = (3.D0*K**2+2.D0*K-1.D0)/ (4.D0)
C
C TY'
C
      B(5,3) = - (6.D0*DZ*K)/ (4.D0*A*A)
      B(5,5) = (6.D0*DZ*K-2.D0)/ (4.D0*A)
      B(5,10) = -B(5,3)
      B(5,12) = (6.D0*DZ*K+2.D0)/ (4.D0*A)
C
C TZ'
C
      B(6,2) = + (6.D0*DY*K)/ (4.D0*A*A)
      B(6,6) = (6.D0*DY*K-2.D0)/ (4.D0*A)
      B(6,9) = -B(6,2)
      B(6,13) = (6.D0*DY*K+2.D0)/ (4.D0*A)
C
C TX''
C
      B(7,4) = (6.D0*K)/ (4.D0*A*A)
      B(7,7) = (6.D0*K-2.D0)/ (4.D0*A)
      B(7,11) = -B(7,4)
      B(7,14) = (6.D0*K+2.D0)/ (4.D0*A)
      END
