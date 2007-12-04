      SUBROUTINE BSIGMA(IP,XL,PHIY,PHIZ,B,INTPOL)
      IMPLICIT NONE
      INTEGER IP,INTPOL
      REAL*8  XL,PHIY,PHIZ,B(4,14)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 27/06/2006   AUTEUR DURAND C.DURAND 
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
C    - FONCTION REALISEE:  CALCUL DES FONCTIONS DE FORME DE DEFORMATIONS
C               GENERALISEES POUTRE 7 DDL A TROIS  POINTS DE GAUSS
C               POUR LE CALCUL DE LA RIGIDITE GEOMETRIQUE
C
C    - ARGUMENTS:
C        DONNEES:           IP      -->   POINTS DE GAUSS
C                           XL      -->   LONGUEUR DE L'ELEMENT
C                          PHIY     -->  COEFF DE CISAILLEMENT SUIVANT Y
C                          PHIZ     -->  COEFF DE CISAILLEMENT SUIVANT Z
C                         INTPOL    -->  INTERPOLATION DES FCTS DE FORME
C                                        (0) LINEAIRE
C                                        (1) CUBIQUE TORSION/FLEXION
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
C
C    DEFORMATIONS        : 1   TX    .ROTATION SUIVANT OX
C                          2   UY'   .DERIVEE DE UY PAR RAPPORT A X
C                          3   UZ'   .DERIVEE DE UZ PAR RAPPORT A X
C                          4   TX'   .PARAMETRE DE GAUCHISSEMENT
C ......................................................................
      INTEGER I,J
      REAL*8 A,K,DY,DZ
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
      
      DO 2 J = 1,14
          DO 1 I = 1,4
              B(I,J) = 0
    1     CONTINUE
    2 CONTINUE
    
      IF (INTPOL .EQ. 0) THEN
         GOTO 100
      ELSE IF (INTPOL .EQ. 1) THEN
         GOTO 200
      END IF

C -------------------------------------------------------
200   CONTINUE
C --- INTERPOLATION CUBIQUE COHERENTE AVEC CELLE CHOISIE
C --- POUR LE CALCUL DE LA MATRICE DE RIGIDITE MATERIELLE
C
C TX
C
      B(1,4) = ((1.D0-K)*(1.D0-K)*(2.D0+K))/ (4.D0)
      B(1,7) = A*((1.D0-K)*(1.D0-(K**2)))/ (4.D0)
      B(1,11) = ((1.D0+K)*(1.D0+K)*(2.D0-K))/ (4.D0)
      B(1,14) = A*((1.D0+K)*(-1.D0+(K**2)))/ (4.D0)
C
C UY'
C
      DY = 1.D0/ (1.D0+PHIY)
      B(2,2) = (3.D0*DY*K**2-2.D0-DY)/ (4.D0*A)
      B(2,6) = (3.D0*DY*K**2-2.D0*K-DY)/ (4.D0)
      B(2,9) = -B(2,2)
      B(2,13) = (3.D0*DY*K**2+2.D0*K-DY)/ (4.D0)
C
C UZ'
C
      DZ = 1.D0/ (1.D0+PHIZ)
      B(3,3) = (3.D0*DZ*K**2-2.D0-DZ)/ (4.D0*A)
      B(3,5) = - (3.D0*DZ*K**2-2.D0*K-DZ)/ (4.D0)
      B(3,10) = -B(3,3)
      B(3,12) = - (3.D0*DZ*K**2+2.D0*K-DZ)/ (4.D0)
C
C TX'
C
      B(4,4) = (3.D0*K**2-3)/ (4.D0*A)
      B(4,7) = (3.D0*K**2-2.D0*K-1.D0)/ (4.D0)
      B(4,11) = (-3.D0*K**2+3.D0)/ (4.D0*A)
      B(4,14) = (3.D0*K**2+2.D0*K-1.D0)/ (4.D0)
      
      GOTO 9999

C -------------------------------------------------------
100   CONTINUE
C --- INTERPOLATION LINEAIRE POUR TOUS LES DDLS
C
C TX
C
      B(1,4) = (1.D0-K)/ (2.D0)
      B(1,11) = (1.D0+K)/ (2.D0)
C
C UY'
C
      B(2,2) = (-1.D0)/ (XL)
      B(2,9) = (1.D0)/ (XL)
C
C UZ'
C
      B(3,3) = (-1.D0)/ (XL)
      B(3,10) = (1.D0)/ (XL)
C
C TX'
C
      B(4,7) = (-1.D0)/ (XL)
      B(4,14) = (1.D0)/ (XL)

      
9999  CONTINUE
      END
