      SUBROUTINE I2ISGC (EPSI,XA,YA,XB,YB,X1,Y1,X2,Y2,X3,Y3,
     +                   NPI,S1,S2,R1,R2,ELI)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 17/01/97   AUTEUR VABHHTS J.PELLET 
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
      IMPLICIT REAL*8 (A-H,O-Z)
C
C AJPUT POSTRELEVE
C
C*******************************************************************
C
C             CALCUL DE L' INTERSECTION DE LA PARABOLE
C             PASSANT PAR LES POINTS (XI,YI) AVEC LE
C             SEGMENT (AB)
C
C             NPI = 0  ==> INTERSECTION VIDE
C
C             NPI = 1  ==> INTERSECTION REDUITE AU POINT
C                          D' ABSCISSE S1 SUIVANT (AB)
C                          ALORS LE BOOLEEN ELI INDIQUE SI CE
C                          POINT DOIT ETRE PRIS EN COMPTE OU NON
C                          (LE POINT EST ELIMINE QUAND LE SEGMENT
C                           EST TANGENT A LA PARABOLE EN CE POINT)
C
C             NPI = 2  ==> INTERSECTION REDUITE AUX POINTS
C                          D' ABSCISSES S1 ET S2 SUIVANT (AB)
C
C*******************************************************************
C
      INTEGER NPI
      REAL*8  EPSI,XA,YA,XB,YB,X1,Y1,X2,Y2,X3,Y3,S1,S2,R1,R2
      LOGICAL ELI
C
      INTEGER NBRAC,ORD1,ORD2
      REAL*8  DELTAX,DELTAY,COEF0,COEF1,COEF2,N1,N2,N3,N4
      REAL*8  L,XR,YR,NORM2,RAC1,RAC2,AUX
      LOGICAL LSR10,LS1R1,LE0R11
      LOGICAL LSR20,LS1R2,LE0R21
      LOGICAL LSS10,LS1S1,LE0S11
      LOGICAL LSS20,LS1S2,LE0S21
C
      NPI    = 0
      S1     = 0.0D0
      S2     = 0.0D0
      R1     = 0.0D0
      R2     = 0.0D0
      XR     = 0.0D0
      YR     = 0.0D0
      AUX    = 0.0D0
      RAC1   = 0.0D0
      RAC2   = 0.0D0
      NBRAC  = 0
      ORD1   = 0
      ORD2   = 0
C
C-----------COORDONNEES ET CARRE DE LA NORME DU VECTEUR AB-----------
C
      DELTAX = XB - XA
      DELTAY = YB - YA
      NORM2  = DELTAX*DELTAX + DELTAY*DELTAY
C
C----------COEFFICIENTS DE LA REPRESENTATION PARAMETRIQUE------------
C----------DE L' ARC DE PARABOLE                         ------------
C
      N1     = X1    - 2*X2 + X3
      N1     = 2*N1
      N3     = Y1    - 2*Y2 + Y3
      N3     = 2*N3
      N2     = -3*X1 + 4*X2 - X3
      N4     = -3*Y1 + 4*Y2 - Y3
C
C------------COEFFICIENTS DE L 'EQUATION DEFFINISSANT L' INTERSECTION---
C
      COEF0  = DELTAY*(X1-XA) - DELTAX*(Y1-YA)
      COEF1  = DELTAY*N2 - DELTAX*N4
      COEF2  = DELTAY*N1 - DELTAX*N3
C
C------------BOOLEENS DE POSITIONEMENT DES RACINES-------------------
C
      LSR10  = .FALSE.
      LS1R1  = .FALSE.
      LE0R11 = .FALSE.
      LSR20  = .FALSE.
      LS1R2  = .FALSE.
      LE0R21 = .FALSE.
      LSS10  = .FALSE.
      LS1S1  = .FALSE.
      LE0S11 = .FALSE.
      LSS20  = .FALSE.
      LS1S2  = .FALSE.
      LE0S21 = .FALSE.
C
C------------BOOLEEN D' ELIMINATION DES POINTS TANGENTS-------------
C
      ELI    = .FALSE.
C
C------------CALCUL DES PARAMETRES SUR LA PARABOLE DES--------------
C------------POINTS D' INTERSECTION                   --------------
C
      CALL I2REQ2 (EPSI,COEF2,COEF1,COEF0,NBRAC,RAC1,RAC2,ORD1,ORD2)
C
C------------REAJUSTEMENT DES PARAMETRES RACINES-------------------
C
      IF ( ABS(RAC1) .LT. EPSI ) THEN
C
         RAC1 = 0.0D0
C
      ENDIF
C
      IF ( ABS(RAC2) .LT. EPSI ) THEN
C
         RAC2 = 0.0D0
C
      ENDIF
C
      IF ( ABS(RAC1-1.0D0) .LT. EPSI ) THEN
C
         RAC1 = 1.0D0
C
      ENDIF
C
      IF ( ABS(RAC2-1.0D0) .LT. EPSI ) THEN
C
         RAC2 = 1.0D0
C
      ENDIF
C
C------------DEPOUILLEMENT DES RESULTATS----------------------------
C
      IF ( NBRAC .EQ. 1) THEN
C
         IF ( (RAC1 .LE. 1.0D0) .AND. (RAC1 .GE. 0.0D0) )THEN
C
            XR = (N1*RAC1 + N2)*RAC1 + X1 - XA
            YR = (N3*RAC1 + N4)*RAC1 + Y1 - YA
            S1 = DELTAX*XR + DELTAY*YR
C
            IF ( ABS(S1) .LT. EPSI ) THEN
C
                 S1 = 0.0D0
C
            ENDIF
C
            IF ( ABS(S1-NORM2) .LT. EPSI ) THEN
C
                 S1 = NORM2
C
            ENDIF
C
            IF ( (S1 .LE. NORM2) .AND. (S1 .GE. 0.0D0) ) THEN
C
               NPI = 1
               S1  = S1/NORM2
               R1  = RAC1
C
               IF ( (ORD1 . GE. 2) .AND. (R1 .GT. EPSI)
     +                             .AND. (S1 .GT. EPSI)
     +                    .AND. ( ABS(R1-1.0D0) .GT. EPSI)
     +                    .AND. ( ABS(S1-1.0D0) .GT. EPSI) ) THEN
C
                  ELI = .TRUE.
C
               ENDIF
C
            ENDIF
C
         ENDIF
C
      ENDIF
C
      IF ( NBRAC .EQ. 2) THEN
C
         LSR10  = (RAC1 .LT. 0.0D0)
         LS1R1  = (RAC1 .GT. 1.0D0)
         LE0R11 = ( (.NOT. LSR10) .AND. (.NOT. LS1R1) )
         LSR20  = (RAC2 .LT. 0.0D0)
         LS1R2  = (RAC2 .GT. 1.0D0)
         LE0R21 = ( (.NOT. LSR20) .AND. (.NOT. LS1R2) )
C
         IF ( (LSR10 .OR. LS1R1) .AND. (LE0R21) ) THEN
C
            XR = (N1*RAC2 + N2)*RAC2 + X1 - XA
            YR = (N3*RAC2 + N4)*RAC2 + Y1 - YA
            S1 = DELTAX*XR + DELTAY*YR
C
            IF ( ABS(S1) .LT. EPSI ) THEN
C
                 S1 = 0.0D0
C
            ENDIF
C
            IF ( ABS(S1-NORM2) .LT. EPSI ) THEN
C
                 S1 = NORM2
C
            ENDIF
C
            IF ( (S1 .LE. NORM2) .AND. (S1 .GE. 0.0D0) ) THEN
C
               NPI = 1
               S1  = S1/NORM2
               R1  = RAC2
C
            ENDIF
C
         ENDIF
C
         IF ( (LE0R11) .AND. (LSR20 .OR. LS1R2) ) THEN
C
            XR = (N1*RAC1 + N2)*RAC1 + X1 - XA
            YR = (N3*RAC1 + N4)*RAC1 + Y1 - YA
            S1 = DELTAX*XR + DELTAY*YR
C
            IF ( ABS(S1) .LT. EPSI ) THEN
C
                 S1 = 0.0D0
C
            ENDIF
C
            IF ( ABS(S1-NORM2) .LT. EPSI ) THEN
C
                 S1 = NORM2
C
            ENDIF
C
            IF ( (S1 .LE. NORM2) .AND. (S1 .GE. 0.0D0) ) THEN
C
               NPI = 1
               S1  = S1/NORM2
               R1  = RAC1
C
            ENDIF
C
         ENDIF
C
         IF (LE0R11 .AND. LE0R21) THEN
C
            XR = (N1*RAC1 + N2)*RAC1 + X1 - XA
            YR = (N3*RAC1 + N4)*RAC1 + Y1 - YA
            S1 = DELTAX*XR + DELTAY*YR
            XR = (N1*RAC2 + N2)*RAC2 + X1 - XA
            YR = (N3*RAC2 + N4)*RAC2 + Y1 - YA
            S2 = DELTAX*XR + DELTAY*YR
C
            IF ( ABS(S1) .LT. EPSI ) THEN
C
                 S1 = 0.0D0
C
            ENDIF
C
            IF ( ABS(S1-NORM2) .LT. EPSI ) THEN
C
                 S1 = NORM2
C
            ENDIF
C
            IF ( ABS(S2) .LT. EPSI ) THEN
C
                 S2 = 0.0D0
C
            ENDIF
C
            IF ( ABS(S2-NORM2) .LT. EPSI ) THEN
C
                 S2 = NORM2
C
            ENDIF
C
            LSS10  = (S1 .LT. 0.0D0)
            LS1S1  = (S1 .GT. NORM2)
            LE0S11 = ( (.NOT. LSS10) .AND. (.NOT. LS1S1) )
            LSS20  = (S2 .LT. 0.0D0)
            LS1S2  = (S2 .GT. NORM2)
            LE0S21 = ( (.NOT. LSS20) .AND. (.NOT. LS1S2) )
C
            IF ( (LSS10 .OR. LS1S1) .AND. (LE0S21) ) THEN
C
               NPI = 1
               S1  = S2/NORM2
               R1  = RAC2
C
            ENDIF
C
            IF ( (LE0S11) .AND. (LSS20 .OR. LS1S2) ) THEN
C
               NPI = 1
               S1  = S1/NORM2
               R1  = RAC1
C
            ENDIF
C
            IF ( LE0S11 .AND. LE0S21 ) THEN
C
               NPI = 2
               S1  = S1/NORM2
               S2  = S2/NORM2
               R1  = RAC1
               R2  = RAC2
C
               IF ( S1 .GT. S2 ) THEN
C
                  AUX = S1
                  S1  = S2
                  S2  = AUX
                  AUX = R1
                  R1  = R2
                  R2  = AUX
C
               ENDIF
C
            ENDIF
C
         ENDIF
C
      ENDIF
C
      END
