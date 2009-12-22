      SUBROUTINE MMGAUS(ALIAS,TYPI,NORD,XPG,YPG,HPG)
C      
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 22/12/2009   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE ABBAS M.ABBAS
C TOLE CRP_20
C
      IMPLICIT NONE
      CHARACTER*8 ALIAS
      INTEGER     TYPI
      INTEGER     NORD
      REAL*8      XPG
      REAL*8      YPG
      REAL*8      HPG
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (UTILITAIRE)
C
C RETOURNE LES COORDONNEES ET LE POIDS DU POINT D'INTEGRATION
C      
C ----------------------------------------------------------------------
C
C
C IN  ALIAS  : NOM D'ALIAS DE L'ELEMENT
C IN  TYPI   : TYPE D'INTEGRATION
C                1 NOEUDS
C                2 GAUSS
C                3 SIMPSON
C IN  NORD   : NUMERO DU POINT D'INTEGRATION
C OUT XPG    : COORDONNEE X DU POINT D'INTEGRATION
C OUT YPG    : COORDONNEE Y DU POINT D'INTEGRATION
C OUT HPG    : POIDS DU POINT D'INTEGRATION
C
C ----------------------------------------------------------------------
C
      REAL*8 A,B,P1,P2
C
C ----------------------------------------------------------------------
C
      IF (ALIAS(1:3) .EQ. 'QU4') THEN
C
        IF (TYPI .EQ. 2) THEN
C LES POINTS DE GAUSS
          IF (NORD .EQ. 1) THEN
            XPG = -1.D0/SQRT(3.D0)
            YPG = -1.D0/SQRT(3.D0)
          ELSEIF (NORD .EQ. 2) THEN
            XPG =  1.D0/SQRT(3.D0)
            YPG = -1.D0/SQRT(3.D0)
          ELSEIF (NORD .EQ. 3) THEN
            XPG =  1.D0/SQRT(3.D0)
            YPG =  1.D0/SQRT(3.D0)
          ELSEIF (NORD .EQ. 4) THEN
            XPG = -1.D0/SQRT(3.D0)
            YPG =  1.D0/SQRT(3.D0)
          ELSE
            CALL ASSERT(.FALSE.)
          END IF
          HPG = 1.D0
C LES NOEUDS
        ELSEIF (TYPI .EQ. 1) THEN
C
          IF (NORD .EQ. 1) THEN
            XPG = -1.D0
            YPG = -1.D0
          ELSEIF (NORD .EQ. 2) THEN
            XPG =  1.D0
            YPG = -1.D0
          ELSEIF (NORD .EQ. 3) THEN
            XPG =  1.D0
            YPG =  1.D0
          ELSEIF (NORD .EQ. 4) THEN
            XPG = -1.D0
            YPG =  1.D0
          ELSE
            CALL ASSERT(.FALSE.)
          END IF
          HPG = 1.D0
C SIMPSON
        ELSEIF (TYPI .EQ. 3) THEN
          IF (NORD .EQ. 1) THEN
            XPG = -1.D0
            YPG = -1.D0
            HPG =  1.D0 / 9.D0
          ELSEIF (NORD .EQ. 2) THEN
            XPG =  1.D0
            YPG = -1.D0
            HPG =  1.D0 / 9.D0
          ELSEIF (NORD .EQ. 3) THEN
            XPG =  1.D0
            YPG =  1.D0
            HPG =  1.D0 / 9.D0
          ELSEIF (NORD .EQ. 4) THEN
            XPG = -1.D0
            YPG =  1.D0
            HPG =  1.D0 / 9.D0
          ELSEIF (NORD .EQ. 5) THEN
            XPG =  0.D0
            YPG = -1.D0
            HPG =  4.D0 / 9.D0
          ELSEIF (NORD .EQ. 6) THEN
            XPG =  1.D0
            YPG =  0.D0
            HPG =  4.D0 / 9.D0
          ELSEIF (NORD .EQ. 7) THEN
            XPG =  0.D0
            YPG =  1.D0
            HPG =  4.D0 / 9.D0
          ELSEIF (NORD .EQ. 8) THEN
            XPG = -1.D0
            YPG = -0.D0
            HPG =  4.D0 / 9.D0
          ELSEIF (NORD .EQ. 9) THEN
            XPG =  0.D0
            YPG =  0.D0
            HPG = 16.D0 / 9.D0
          ELSE
            CALL ASSERT(.FALSE.)
          END IF
C NEWTON COTES A 16 POINTS
       ELSE IF (TYPI.EQ.6) THEN
          IF (NORD.EQ.1) THEN
            XPG = -1.D0
            YPG = -1.D0
            HPG =  1.D0/16.D0
          ELSE IF (NORD.EQ.2) THEN
            XPG =  1.D0
            YPG = -1.D0
            HPG =  1.D0/16.D0
          ELSE IF (NORD.EQ.3) THEN
            XPG =  1.D0
            YPG =  1.D0
            HPG =  1.D0/16.D0
          ELSE IF (NORD.EQ.4) THEN
            XPG = -1.D0
            YPG =  1.D0
            HPG =  1.D0/16.D0
          ELSE IF (NORD.EQ.5) THEN
            XPG = -1.D0/3.D0
            YPG = -1.D0
            HPG =  3.D0/16.D0
          ELSE IF (NORD.EQ.6) THEN
            XPG =  1.D0/3.D0
            YPG = -1.D0
            HPG =  3.D0/16.D0
          ELSE IF (NORD.EQ.7) THEN
            XPG =  1.D0
            YPG = -1.D0/3.D0
            HPG =  3.D0/16.D0
          ELSE IF (NORD.EQ.8) THEN
            XPG =  1.D0
            YPG =  1.D0/3.D0
            HPG =  3.D0/16.D0
          ELSE IF (NORD.EQ.9) THEN
            XPG =  1.D0/3.D0
            YPG =  1.D0
            HPG =  3.D0/16.D0
          ELSE IF (NORD.EQ.10) THEN
            XPG = -1.D0/3.D0
            YPG =  1.D0
            HPG =  3.D0/16.D0
          ELSE IF (NORD.EQ.11) THEN
            XPG = -1.D0
            YPG =  1.D0/3.D0
            HPG =  3.D0/16.D0
          ELSE IF (NORD.EQ.12) THEN
            XPG = -1.D0
            YPG = -1.D0/3.D0
            HPG =  3.D0/16.D0
          ELSE IF (NORD.EQ.13) THEN
            XPG = -1.D0/3.D0
            YPG = -1.D0/3.D0
            HPG =  9.D0/16.D0
          ELSE IF (NORD.EQ.14) THEN
            XPG =  1.D0/3.D0
            YPG = -1.D0/3.D0
            HPG =  9.D0/16.D0
          ELSE IF (NORD.EQ.15) THEN
            XPG =  1.D0/3.D0
            YPG =  1.D0/3.D0
            HPG =  9.D0/16.D0
          ELSE IF (NORD.EQ.16) THEN
            XPG = -1.D0/3.D0
            YPG =  1.D0/3.D0
            HPG =  9.D0/16.D0
          ELSE
            CALL ASSERT(.FALSE.)
          END IF
C NEWTON COTES A 25 POINTS
        ELSE IF (TYPI.EQ.7) THEN
          IF (NORD.EQ.1) THEN
            XPG = -1.D0
            YPG = -1.D0
            HPG = 49.D0/2025.D0
          ELSE IF (NORD.EQ.2) THEN
            XPG =  1.D0
            YPG = -1.D0
            HPG = 49.D0/2025.D0
          ELSE IF (NORD.EQ.3) THEN
            XPG =  1.D0
            YPG =  1.D0
            HPG = 49.D0/2025.D0
          ELSE IF (NORD.EQ.4) THEN
            XPG = -1.D0
            YPG =  1.D0
            HPG =  49.D0/2025.D0
          ELSE IF (NORD.EQ.5) THEN
            XPG =  0.D0
            YPG = -1.D0
            HPG = 84.D0/2025.D0
          ELSE IF (NORD.EQ.6) THEN
            XPG =  1.D0
            YPG =  0.D0
            HPG = 84.D0/2025.D0
          ELSE IF (NORD.EQ.7) THEN
            XPG =  0.D0
            YPG =  1.D0
            HPG = 84.D0/2025.D0
          ELSE IF (NORD.EQ.8) THEN
            XPG = -1.D0
            YPG =  0.D0
            HPG = 84.D0/2025.D0
          ELSE IF (NORD.EQ.9) THEN
            XPG =  0.D0
            YPG =  0.D0
            HPG =144.D0/2025.D0
          ELSE IF (NORD.EQ.10) THEN
            XPG = -1.D0
            YPG = 0.5D0
            HPG = 224.D0/2025.D0
          ELSE IF (NORD.EQ.11) THEN
            XPG = -0.5D0
            YPG = 0.D0
            HPG = 384.D0/2025.D0
          ELSE IF (NORD.EQ.12) THEN
            XPG = 0.D0
            YPG = 0.5D0
            HPG = 384.D0/2025.D0
          ELSE IF (NORD.EQ.13) THEN
            XPG = -0.5D0
            YPG = 1.D0
            HPG = 224.D0/2025.D0
          ELSE IF (NORD.EQ.14) THEN
            XPG = -0.5D0
            YPG =  0.5D0
            HPG = 1024.D0/2025.D0
          ELSE IF (NORD.EQ.15) THEN
            XPG = -1.D0
            YPG = -0.5D0
            HPG = 224.D0/2025.D0
          ELSE IF (NORD.EQ.16) THEN
            XPG = -0.5D0
            YPG = -1.D0
            HPG = 224.D0/2025.D0
          ELSE IF (NORD.EQ.17) THEN
            XPG = 0.D0
            YPG = -0.5D0
            HPG = 384.D0/2025.D0
          ELSE IF (NORD.EQ.18) THEN
            XPG = -0.5D0
            YPG = -0.5D0
            HPG = 1024.D0/2025.D0
          ELSE IF (NORD.EQ.19) THEN
            XPG = 0.5D0
            YPG = 0.D0
            HPG = 384.D0/2025.D0
          ELSE IF (NORD.EQ.20) THEN
            XPG = 1.D0
            YPG = 0.5D0
            HPG = 224.D0/2025.D0
          ELSE IF (NORD.EQ.21) THEN
            XPG = 0.5D0
            YPG = 1.D0
            HPG = 224.D0/2025.D0
          ELSE IF (NORD.EQ.22) THEN
            XPG = 0.5D0
            YPG = 0.5D0
            HPG = 1024.D0/2025.D0
          ELSE IF (NORD.EQ.23) THEN
            XPG = 0.5D0
            YPG = -1.D0
            HPG = 224.D0/2025.D0
          ELSE IF (NORD.EQ.24) THEN
            XPG = 1.D0
            YPG = -0.5D0
            HPG = 224.D0/2025.D0
          ELSE IF (NORD.EQ.25) THEN
            XPG = 0.5D0
            YPG = -0.5D0
            HPG = 1024.D0/2025.D0
          ELSE
            CALL ASSERT(.FALSE.)
          END IF
C NEWTON COTES A 100 POINTS (FORTES INCOMPATIBILITES)
        ELSE IF (TYPI.EQ.8) THEN
          IF (NORD.EQ.1) THEN
            XPG = -1.D0
            YPG = -1.D0
            HPG = 1.D0/144.D0
          ELSE IF (NORD.EQ.2) THEN
            XPG = -1.D0
            YPG = -0.7778D0
            HPG = 1.D0/48.D0
          ELSE IF (NORD.EQ.3) THEN
            XPG = -1.D0
            YPG = -0.5556D0
            HPG = 1.D0/48.D0
          ELSE IF (NORD.EQ.4) THEN
            XPG = -1.D0
            YPG = -0.3333D0
            HPG = 1.D0/72.D0
          ELSE IF (NORD.EQ.5) THEN
            XPG = -1.D0
            YPG = -0.1111D0
            HPG = 1.D0/48.D0
          ELSE IF (NORD.EQ.6) THEN
            XPG = -1.D0
            YPG = 0.1111D0
            HPG = 1.D0/48.D0
          ELSE IF (NORD.EQ.7) THEN
            XPG = -1.D0
            YPG = 0.3333D0
            HPG = 1.D0/72.D0
          ELSE IF (NORD.EQ.8) THEN
            XPG = -1.D0
            YPG = 0.5556D0
            HPG = 1.D0/48.D0
          ELSE IF (NORD.EQ.9) THEN
            XPG = -1.D0
            YPG = 0.7778D0
            HPG = 1.D0/48.D0
          ELSE IF (NORD.EQ.10) THEN
            XPG = -1.D0
            YPG = 1.D0
            HPG = 1.D0/144.D0
          ELSE IF (NORD.EQ.11) THEN
            XPG = -0.7778D0
            YPG = -1.D0
            HPG = 1.D0/48.D0
          ELSE IF (NORD.EQ.12) THEN
            XPG = -0.7778D0
            YPG = -0.7778D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.13) THEN
            XPG = -0.7778D0
            YPG = -0.5556D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.14) THEN
            XPG = -0.7778D0
            YPG =  -0.3333D0
            HPG = 1.D0/24.D0
          ELSE IF (NORD.EQ.15) THEN
            XPG = -0.7778D0
            YPG = -0.1111D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.16) THEN
            XPG = -0.7778D0
            YPG = 0.1111D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.17) THEN
            XPG = -0.7778D0
            YPG = 0.3333D0
            HPG = 1.D0/24.D0
          ELSE IF (NORD.EQ.18) THEN
            XPG = -0.7778D0
            YPG = 0.5556D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.19) THEN
            XPG = -0.7778D0
            YPG = 0.7778D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.20) THEN
            XPG = -0.7778D0
            YPG = 1.D0
            HPG = 1.D0/48.D0
          ELSE IF (NORD.EQ.21) THEN
            XPG = -0.5556D0
            YPG = -1.D0
            HPG = 1.D0/48.D0
          ELSE IF (NORD.EQ.22) THEN
            XPG = -0.5556D0
            YPG = -0.7778D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.23) THEN
            XPG = -0.5556D0
            YPG = -0.5556D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.24) THEN
            XPG = -0.5556D0
            YPG =  -0.3333D0
            HPG = 1.D0/24.D0
          ELSE IF (NORD.EQ.25) THEN
            XPG = -0.5556D0
            YPG = -0.1111D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.26) THEN
            XPG = -0.5556D0
            YPG = 0.1111D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.27) THEN
            XPG = -0.5556D0
            YPG = 0.3333D0
            HPG = 1.D0/24.D0
          ELSE IF (NORD.EQ.28) THEN
            XPG = -0.5556D0
            YPG = 0.5556D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.29) THEN
            XPG = -0.5556D0
            YPG = 0.7778D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.30) THEN
            XPG = -0.5556D0
            YPG = 1.D0
            HPG = 1.D0/48.D0
          ELSE IF (NORD.EQ.31) THEN
            XPG = -0.3333D0
            YPG = -1.D0
            HPG = 1.D0/72.D0
          ELSE IF (NORD.EQ.32) THEN
            XPG = -0.3333D0
            YPG = -0.7778D0
            HPG = 1.D0/24.D0
          ELSE IF (NORD.EQ.33) THEN
            XPG = -0.3333D0
            YPG = -0.5556D0
            HPG = 1.D0/24.D0
          ELSE IF (NORD.EQ.34) THEN
            XPG = -0.3333D0
            YPG =  -0.3333D0
            HPG = 1.D0/36.D0
          ELSE IF (NORD.EQ.35) THEN
            XPG = -0.3333D0
            YPG = -0.1111D0
            HPG = 1.D0/24.D0
          ELSE IF (NORD.EQ.36) THEN
            XPG = -0.3333D0
            YPG = 0.1111D0
            HPG = 1.D0/24.D0
          ELSE IF (NORD.EQ.37) THEN
            XPG = -0.3333D0
            YPG = 0.3333D0
            HPG = 1.D0/36.D0
          ELSE IF (NORD.EQ.38) THEN
            XPG = -0.3333D0
            YPG = 0.5556D0
            HPG = 1.D0/24.D0
          ELSE IF (NORD.EQ.39) THEN
            XPG = -0.3333D0
            YPG = 0.7778D0
            HPG = 1.D0/24.D0
          ELSE IF (NORD.EQ.40) THEN
            XPG = -0.3333D0
            YPG = 1.D0
            HPG = 1.D0/72.D0
          ELSE IF (NORD.EQ.41) THEN
            XPG = -0.1111D0
            YPG = -1.D0
            HPG = 1.D0/48.D0
          ELSE IF (NORD.EQ.42) THEN
            XPG = -0.1111D0
            YPG = -0.7778D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.43) THEN
            XPG = -0.1111D0
            YPG = -0.5556D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.44) THEN
            XPG = -0.1111D0
            YPG =  -0.3333D0
            HPG = 1.D0/24.D0
          ELSE IF (NORD.EQ.45) THEN
            XPG = -0.1111D0
            YPG = -0.1111D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.46) THEN
            XPG = -0.1111D0
            YPG = 0.1111D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.47) THEN
            XPG = -0.1111D0
            YPG = 0.3333D0
            HPG = 1.D0/24.D0
          ELSE IF (NORD.EQ.48) THEN
            XPG = -0.1111D0
            YPG = 0.5556D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.49) THEN
            XPG = -0.1111D0
            YPG = 0.7778D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.50) THEN
            XPG = -0.1111D0
            YPG = 1.D0
            HPG = 1.D0/48.D0
          ELSE IF (NORD.EQ.51) THEN
            XPG = 0.1111D0
            YPG = -1.D0
            HPG = 1.D0/48.D0
          ELSE IF (NORD.EQ.52) THEN
            XPG = 0.1111D0
            YPG = -0.7778D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.53) THEN
            XPG = 0.1111D0
            YPG = -0.5556D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.54) THEN
            XPG = 0.1111D0
            YPG =  -0.3333D0
            HPG = 1.D0/24.D0
          ELSE IF (NORD.EQ.55) THEN
            XPG = 0.1111D0
            YPG = -0.1111D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.56) THEN
            XPG = 0.1111D0
            YPG = 0.1111D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.57) THEN
            XPG = 0.1111D0
            YPG = 0.3333D0
            HPG = 1.D0/24.D0
          ELSE IF (NORD.EQ.58) THEN
            XPG = 0.1111D0
            YPG = 0.5556D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.59) THEN
            XPG = 0.1111D0
            YPG = 0.7778D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.60) THEN
            XPG = 0.1111D0
            YPG = 1.D0
            HPG = 1.D0/48.D0
          ELSE IF (NORD.EQ.61) THEN
            XPG = 0.3333D0
            YPG = -1.D0
            HPG = 1.D0/72.D0
          ELSE IF (NORD.EQ.62) THEN
            XPG = 0.3333D0
            YPG = -0.7778D0
            HPG = 1.D0/24.D0
          ELSE IF (NORD.EQ.63) THEN
            XPG = 0.3333D0
            YPG = -0.5556D0
            HPG = 1.D0/24.D0
          ELSE IF (NORD.EQ.64) THEN
            XPG = 0.3333D0
            YPG =  -0.3333D0
            HPG = 1.D0/36.D0
          ELSE IF (NORD.EQ.65) THEN
            XPG = 0.3333D0
            YPG = -0.1111D0
            HPG = 1.D0/24.D0
          ELSE IF (NORD.EQ.66) THEN
            XPG = 0.3333D0
            YPG = 0.1111D0
            HPG = 1.D0/24.D0
          ELSE IF (NORD.EQ.67) THEN
            XPG = 0.3333D0
            YPG = 0.3333D0
            HPG = 1.D0/36.D0
          ELSE IF (NORD.EQ.68) THEN
            XPG = 0.3333D0
            YPG = 0.5556D0
            HPG = 1.D0/24.D0
          ELSE IF (NORD.EQ.69) THEN
            XPG = 0.3333D0
            YPG = 0.7778D0
            HPG = 1.D0/24.D0
          ELSE IF (NORD.EQ.70) THEN
            XPG = 0.3333D0
            YPG = 1.D0
            HPG = 1.D0/72.D0
          ELSE IF (NORD.EQ.71) THEN
            XPG = 0.5556D0
            YPG = -1.D0
            HPG = 1.D0/48.D0
          ELSE IF (NORD.EQ.72) THEN
            XPG = 0.5556D0
            YPG = -0.7778D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.73) THEN
            XPG = 0.5556D0
            YPG = -0.5556D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.74) THEN
            XPG = 0.5556D0
            YPG =  -0.3333D0
            HPG = 1.D0/24.D0
          ELSE IF (NORD.EQ.75) THEN
            XPG = 0.5556D0
            YPG = -0.1111D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.76) THEN
            XPG = 0.5556D0
            YPG = 0.1111D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.77) THEN
            XPG = 0.5556D0
            YPG = 0.3333D0
            HPG = 1.D0/24.D0
          ELSE IF (NORD.EQ.78) THEN
            XPG = 0.5556D0
            YPG = 0.5556D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.79) THEN
            XPG = 0.5556D0
            YPG = 0.7778D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.80) THEN
            XPG = 0.5556D0
            YPG = 1.D0
            HPG = 1.D0/48.D0
          ELSE IF (NORD.EQ.81) THEN
            XPG = 0.7778D0
            YPG = -1.D0
            HPG = 1.D0/48.D0
          ELSE IF (NORD.EQ.82) THEN
            XPG = 0.7778D0
            YPG = -0.7778D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.83) THEN
            XPG = 0.7778D0
            YPG = -0.5556D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.84) THEN
            XPG = 0.7778D0
            YPG =  -0.3333D0
            HPG = 1.D0/24.D0
          ELSE IF (NORD.EQ.85) THEN
            XPG = 0.7778D0
            YPG = -0.1111D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.86) THEN
            XPG = 0.7778D0
            YPG = 0.1111D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.87) THEN
            XPG = 0.7778D0
            YPG = 0.3333D0
            HPG = 1.D0/24.D0
          ELSE IF (NORD.EQ.88) THEN
            XPG = 0.7778D0
            YPG = 0.5556D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.89) THEN
            XPG = 0.7778D0
            YPG = 0.7778D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.90) THEN
            XPG = 0.7778D0
            YPG = 1.D0
            HPG = 1.D0/48.D0
          ELSE IF (NORD.EQ.91) THEN
            XPG = 1.D0
            YPG = -1.D0
            HPG = 1.D0/144.D0
          ELSE IF (NORD.EQ.92) THEN
            XPG = 1.D0
            YPG = -0.7778D0
            HPG = 1.D0/48.D0
          ELSE IF (NORD.EQ.93) THEN
            XPG = 1.D0
            YPG = -0.5556D0
            HPG = 1.D0/48.D0
          ELSE IF (NORD.EQ.94) THEN
            XPG = 1.D0
            YPG = -0.3333D0
            HPG = 1.D0/72.D0
          ELSE IF (NORD.EQ.95) THEN
            XPG = 1.D0
            YPG = -0.1111D0
            HPG = 1.D0/48.D0
          ELSE IF (NORD.EQ.96) THEN
            XPG = 1.D0
            YPG = 0.1111D0
            HPG = 1.D0/48.D0
          ELSE IF (NORD.EQ.97) THEN
            XPG = 1.D0
            YPG = 0.3333D0
            HPG = 1.D0/72.D0
          ELSE IF (NORD.EQ.98) THEN
            XPG = 1.D0
            YPG = 0.5556D0
            HPG = 1.D0/48.D0
          ELSE IF (NORD.EQ.99) THEN
            XPG = 1.D0
            YPG = 0.7778D0
            HPG = 1.D0/48.D0
          ELSE IF (NORD.EQ.100) THEN
            XPG = 1.D0
            YPG = 1.D0
            HPG = 1.D0/144.D0
          ELSE
            CALL ASSERT(.FALSE.)
          END IF
        ELSE
            CALL ASSERT(.FALSE.)
        END IF
C
      ELSEIF (ALIAS(1:3) .EQ. 'QU8') THEN
C
        IF ((TYPI.EQ.2) .OR. (TYPI.EQ.1) .OR. (TYPI.EQ.3)) THEN
          IF (NORD .EQ. 1) THEN
            XPG = -1.D0
            YPG = -1.D0
            HPG =  1.D0 / 9.D0
          ELSEIF (NORD .EQ. 2) THEN
            XPG =  1.D0
            YPG = -1.D0
            HPG =  1.D0 / 9.D0
          ELSEIF (NORD .EQ. 3) THEN
            XPG =  1.D0
            YPG =  1.D0
            HPG =  1.D0 / 9.D0
          ELSEIF (NORD .EQ. 4) THEN
            XPG = -1.D0
            YPG =  1.D0
            HPG =  1.D0 / 9.D0
          ELSEIF (NORD .EQ. 5) THEN
            XPG =  0.D0
            YPG = -1.D0
            HPG =  4.D0 / 9.D0
          ELSEIF (NORD .EQ. 6) THEN
            XPG =  1.D0
            YPG =  0.D0
            HPG =  4.D0 / 9.D0
          ELSEIF (NORD .EQ. 7) THEN
            XPG =  0.D0
            YPG =  1.D0
            HPG =  4.D0 / 9.D0
          ELSEIF (NORD .EQ. 8) THEN
            XPG = -1.D0
            YPG = -0.D0
            HPG =  4.D0 / 9.D0
          ELSEIF (NORD .EQ. 9) THEN
            XPG =  0.D0
            YPG =  0.D0
            HPG = 16.D0 / 9.D0
          ELSE
            CALL ASSERT(.FALSE.)
          END IF
C NEWTON COTES A 16 POINTS
       ELSE IF (TYPI.EQ.6) THEN
          IF (NORD.EQ.1) THEN
            XPG = -1.D0
            YPG = -1.D0
            HPG =  1.D0/16.D0
          ELSE IF (NORD.EQ.2) THEN
            XPG =  1.D0
            YPG = -1.D0
            HPG =  1.D0/16.D0
          ELSE IF (NORD.EQ.3) THEN
            XPG =  1.D0
            YPG =  1.D0
            HPG =  1.D0/16.D0
          ELSE IF (NORD.EQ.4) THEN
            XPG = -1.D0
            YPG =  1.D0
            HPG =  1.D0/16.D0
          ELSE IF (NORD.EQ.5) THEN
            XPG = -1.D0/3.D0
            YPG = -1.D0
            HPG =  3.D0/16.D0
          ELSE IF (NORD.EQ.6) THEN
            XPG =  1.D0/3.D0
            YPG = -1.D0
            HPG =  3.D0/16.D0
          ELSE IF (NORD.EQ.7) THEN
            XPG =  1.D0
            YPG = -1.D0/3.D0
            HPG =  3.D0/16.D0
          ELSE IF (NORD.EQ.8) THEN
            XPG =  1.D0
            YPG =  1.D0/3.D0
            HPG =  3.D0/16.D0
          ELSE IF (NORD.EQ.9) THEN
            XPG =  1.D0/3.D0
            YPG =  1.D0
            HPG =  3.D0/16.D0
          ELSE IF (NORD.EQ.10) THEN
            XPG = -1.D0/3.D0
            YPG =  1.D0
            HPG =  3.D0/16.D0
          ELSE IF (NORD.EQ.11) THEN
            XPG = -1.D0
            YPG =  1.D0/3.D0
            HPG =  3.D0/16.D0
          ELSE IF (NORD.EQ.12) THEN
            XPG = -1.D0
            YPG = -1.D0/3.D0
            HPG =  3.D0/16.D0
          ELSE IF (NORD.EQ.13) THEN
            XPG = -1.D0/3.D0
            YPG = -1.D0/3.D0
            HPG =  9.D0/16.D0
          ELSE IF (NORD.EQ.14) THEN
            XPG =  1.D0/3.D0
            YPG = -1.D0/3.D0
            HPG =  9.D0/16.D0
          ELSE IF (NORD.EQ.15) THEN
            XPG =  1.D0/3.D0
            YPG =  1.D0/3.D0
            HPG =  9.D0/16.D0
          ELSE IF (NORD.EQ.16) THEN
            XPG = -1.D0/3.D0
            YPG =  1.D0/3.D0
            HPG =  9.D0/16.D0
          ELSE
            CALL ASSERT(.FALSE.)
          END IF
C NEWTON COTES A 25 POINTS
        ELSE IF (TYPI.EQ.7) THEN
          IF (NORD.EQ.1) THEN
            XPG = -1.D0
            YPG = -1.D0
            HPG = 49.D0/2025.D0
          ELSE IF (NORD.EQ.2) THEN
            XPG =  1.D0
            YPG = -1.D0
            HPG = 49.D0/2025.D0
          ELSE IF (NORD.EQ.3) THEN
            XPG =  1.D0
            YPG =  1.D0
            HPG = 49.D0/2025.D0
          ELSE IF (NORD.EQ.4) THEN
            XPG = -1.D0
            YPG =  1.D0
            HPG =  49.D0/2025.D0
          ELSE IF (NORD.EQ.5) THEN
            XPG =  0.D0
            YPG = -1.D0
            HPG = 84.D0/2025.D0
          ELSE IF (NORD.EQ.6) THEN
            XPG =  1.D0
            YPG =  0.D0
            HPG = 84.D0/2025.D0
          ELSE IF (NORD.EQ.7) THEN
            XPG =  0.D0
            YPG =  1.D0
            HPG = 84.D0/2025.D0
          ELSE IF (NORD.EQ.8) THEN
            XPG = -1.D0
            YPG =  0.D0
            HPG = 84.D0/2025.D0
          ELSE IF (NORD.EQ.9) THEN
            XPG =  0.D0
            YPG =  0.D0
            HPG =144.D0/2025.D0
          ELSE IF (NORD.EQ.10) THEN
            XPG = -1.D0
            YPG = 0.5D0
            HPG = 224.D0/2025.D0
          ELSE IF (NORD.EQ.11) THEN
            XPG = -0.5D0
            YPG = 0.D0
            HPG = 384.D0/2025.D0
          ELSE IF (NORD.EQ.12) THEN
            XPG = 0.D0
            YPG = 0.5D0
            HPG = 384.D0/2025.D0
          ELSE IF (NORD.EQ.13) THEN
            XPG = -0.5D0
            YPG = 1.D0
            HPG = 224.D0/2025.D0
          ELSE IF (NORD.EQ.14) THEN
            XPG = -0.5D0
            YPG =  0.5D0
            HPG = 1024.D0/2025.D0
          ELSE IF (NORD.EQ.15) THEN
            XPG = -1.D0
            YPG = -0.5D0
            HPG = 224.D0/2025.D0
          ELSE IF (NORD.EQ.16) THEN
            XPG = -0.5D0
            YPG = -1.D0
            HPG = 224.D0/2025.D0
          ELSE IF (NORD.EQ.17) THEN
            XPG = 0.D0
            YPG = -0.5D0
            HPG = 384.D0/2025.D0
          ELSE IF (NORD.EQ.18) THEN
            XPG = -0.5D0
            YPG = -0.5D0
            HPG = 1024.D0/2025.D0
          ELSE IF (NORD.EQ.19) THEN
            XPG = 0.5D0
            YPG = 0.D0
            HPG = 384.D0/2025.D0
          ELSE IF (NORD.EQ.20) THEN
            XPG = 1.D0
            YPG = 0.5D0
            HPG = 224.D0/2025.D0
          ELSE IF (NORD.EQ.21) THEN
            XPG = 0.5D0
            YPG = 1.D0
            HPG = 224.D0/2025.D0
          ELSE IF (NORD.EQ.22) THEN
            XPG = 0.5D0
            YPG = 0.5D0
            HPG = 1024.D0/2025.D0
          ELSE IF (NORD.EQ.23) THEN
            XPG = 0.5D0
            YPG = -1.D0
            HPG = 224.D0/2025.D0
          ELSE IF (NORD.EQ.24) THEN
            XPG = 1.D0
            YPG = -0.5D0
            HPG = 224.D0/2025.D0
          ELSE IF (NORD.EQ.25) THEN
            XPG = 0.5D0
            YPG = -0.5D0
            HPG = 1024.D0/2025.D0
          ELSE
            CALL ASSERT(.FALSE.)
          END IF
C NEWTON COTES A 100 POINTS (FORTES INCOMPATIBILITES)
        ELSE IF (TYPI.EQ.8) THEN
          IF (NORD.EQ.1) THEN
            XPG = -1.D0
            YPG = -1.D0
            HPG = 1.D0/144.D0
          ELSE IF (NORD.EQ.2) THEN
            XPG = -1.D0
            YPG = -0.7778D0
            HPG = 1.D0/48.D0
          ELSE IF (NORD.EQ.3) THEN
            XPG = -1.D0
            YPG = -0.5556D0
            HPG = 1.D0/48.D0
          ELSE IF (NORD.EQ.4) THEN
            XPG = -1.D0
            YPG = -0.3333D0
            HPG = 1.D0/72.D0
          ELSE IF (NORD.EQ.5) THEN
            XPG = -1.D0
            YPG = -0.1111D0
            HPG = 1.D0/48.D0
          ELSE IF (NORD.EQ.6) THEN
            XPG = -1.D0
            YPG = 0.1111D0
            HPG = 1.D0/48.D0
          ELSE IF (NORD.EQ.7) THEN
            XPG = -1.D0
            YPG = 0.3333D0
            HPG = 1.D0/72.D0
          ELSE IF (NORD.EQ.8) THEN
            XPG = -1.D0
            YPG = 0.5556D0
            HPG = 1.D0/48.D0
          ELSE IF (NORD.EQ.9) THEN
            XPG = -1.D0
            YPG = 0.7778D0
            HPG = 1.D0/48.D0
          ELSE IF (NORD.EQ.10) THEN
            XPG = -1.D0
            YPG = 1.D0
            HPG = 1.D0/144.D0
          ELSE IF (NORD.EQ.11) THEN
            XPG = -0.7778D0
            YPG = -1.D0
            HPG = 1.D0/48.D0
          ELSE IF (NORD.EQ.12) THEN
            XPG = -0.7778D0
            YPG = -0.7778D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.13) THEN
            XPG = -0.7778D0
            YPG = -0.5556D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.14) THEN
            XPG = -0.7778D0
            YPG =  -0.3333D0
            HPG = 1.D0/24.D0
          ELSE IF (NORD.EQ.15) THEN
            XPG = -0.7778D0
            YPG = -0.1111D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.16) THEN
            XPG = -0.7778D0
            YPG = 0.1111D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.17) THEN
            XPG = -0.7778D0
            YPG = 0.3333D0
            HPG = 1.D0/24.D0
          ELSE IF (NORD.EQ.18) THEN
            XPG = -0.7778D0
            YPG = 0.5556D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.19) THEN
            XPG = -0.7778D0
            YPG = 0.7778D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.20) THEN
            XPG = -0.7778D0
            YPG = 1.D0
            HPG = 1.D0/48.D0
          ELSE IF (NORD.EQ.21) THEN
            XPG = -0.5556D0
            YPG = -1.D0
            HPG = 1.D0/48.D0
          ELSE IF (NORD.EQ.22) THEN
            XPG = -0.5556D0
            YPG = -0.7778D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.23) THEN
            XPG = -0.5556D0
            YPG = -0.5556D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.24) THEN
            XPG = -0.5556D0
            YPG =  -0.3333D0
            HPG = 1.D0/24.D0
          ELSE IF (NORD.EQ.25) THEN
            XPG = -0.5556D0
            YPG = -0.1111D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.26) THEN
            XPG = -0.5556D0
            YPG = 0.1111D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.27) THEN
            XPG = -0.5556D0
            YPG = 0.3333D0
            HPG = 1.D0/24.D0
          ELSE IF (NORD.EQ.28) THEN
            XPG = -0.5556D0
            YPG = 0.5556D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.29) THEN
            XPG = -0.5556D0
            YPG = 0.7778D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.30) THEN
            XPG = -0.5556D0
            YPG = 1.D0
            HPG = 1.D0/48.D0
          ELSE IF (NORD.EQ.31) THEN
            XPG = -0.3333D0
            YPG = -1.D0
            HPG = 1.D0/72.D0
          ELSE IF (NORD.EQ.32) THEN
            XPG = -0.3333D0
            YPG = -0.7778D0
            HPG = 1.D0/24.D0
          ELSE IF (NORD.EQ.33) THEN
            XPG = -0.3333D0
            YPG = -0.5556D0
            HPG = 1.D0/24.D0
          ELSE IF (NORD.EQ.34) THEN
            XPG = -0.3333D0
            YPG =  -0.3333D0
            HPG = 1.D0/36.D0
          ELSE IF (NORD.EQ.35) THEN
            XPG = -0.3333D0
            YPG = -0.1111D0
            HPG = 1.D0/24.D0
          ELSE IF (NORD.EQ.36) THEN
            XPG = -0.3333D0
            YPG = 0.1111D0
            HPG = 1.D0/24.D0
          ELSE IF (NORD.EQ.37) THEN
            XPG = -0.3333D0
            YPG = 0.3333D0
            HPG = 1.D0/36.D0
          ELSE IF (NORD.EQ.38) THEN
            XPG = -0.3333D0
            YPG = 0.5556D0
            HPG = 1.D0/24.D0
          ELSE IF (NORD.EQ.39) THEN
            XPG = -0.3333D0
            YPG = 0.7778D0
            HPG = 1.D0/24.D0
          ELSE IF (NORD.EQ.40) THEN
            XPG = -0.3333D0
            YPG = 1.D0
            HPG = 1.D0/72.D0
          ELSE IF (NORD.EQ.41) THEN
            XPG = -0.1111D0
            YPG = -1.D0
            HPG = 1.D0/48.D0
          ELSE IF (NORD.EQ.42) THEN
            XPG = -0.1111D0
            YPG = -0.7778D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.43) THEN
            XPG = -0.1111D0
            YPG = -0.5556D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.44) THEN
            XPG = -0.1111D0
            YPG =  -0.3333D0
            HPG = 1.D0/24.D0
          ELSE IF (NORD.EQ.45) THEN
            XPG = -0.1111D0
            YPG = -0.1111D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.46) THEN
            XPG = -0.1111D0
            YPG = 0.1111D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.47) THEN
            XPG = -0.1111D0
            YPG = 0.3333D0
            HPG = 1.D0/24.D0
          ELSE IF (NORD.EQ.48) THEN
            XPG = -0.1111D0
            YPG = 0.5556D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.49) THEN
            XPG = -0.1111D0
            YPG = 0.7778D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.50) THEN
            XPG = -0.1111D0
            YPG = 1.D0
            HPG = 1.D0/48.D0
          ELSE IF (NORD.EQ.51) THEN
            XPG = 0.1111D0
            YPG = -1.D0
            HPG = 1.D0/48.D0
          ELSE IF (NORD.EQ.52) THEN
            XPG = 0.1111D0
            YPG = -0.7778D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.53) THEN
            XPG = 0.1111D0
            YPG = -0.5556D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.54) THEN
            XPG = 0.1111D0
            YPG =  -0.3333D0
            HPG = 1.D0/24.D0
          ELSE IF (NORD.EQ.55) THEN
            XPG = 0.1111D0
            YPG = -0.1111D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.56) THEN
            XPG = 0.1111D0
            YPG = 0.1111D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.57) THEN
            XPG = 0.1111D0
            YPG = 0.3333D0
            HPG = 1.D0/24.D0
          ELSE IF (NORD.EQ.58) THEN
            XPG = 0.1111D0
            YPG = 0.5556D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.59) THEN
            XPG = 0.1111D0
            YPG = 0.7778D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.60) THEN
            XPG = 0.1111D0
            YPG = 1.D0
            HPG = 1.D0/48.D0
          ELSE IF (NORD.EQ.61) THEN
            XPG = 0.3333D0
            YPG = -1.D0
            HPG = 1.D0/72.D0
          ELSE IF (NORD.EQ.62) THEN
            XPG = 0.3333D0
            YPG = -0.7778D0
            HPG = 1.D0/24.D0
          ELSE IF (NORD.EQ.63) THEN
            XPG = 0.3333D0
            YPG = -0.5556D0
            HPG = 1.D0/24.D0
          ELSE IF (NORD.EQ.64) THEN
            XPG = 0.3333D0
            YPG =  -0.3333D0
            HPG = 1.D0/36.D0
          ELSE IF (NORD.EQ.65) THEN
            XPG = 0.3333D0
            YPG = -0.1111D0
            HPG = 1.D0/24.D0
          ELSE IF (NORD.EQ.66) THEN
            XPG = 0.3333D0
            YPG = 0.1111D0
            HPG = 1.D0/24.D0
          ELSE IF (NORD.EQ.67) THEN
            XPG = 0.3333D0
            YPG = 0.3333D0
            HPG = 1.D0/36.D0
          ELSE IF (NORD.EQ.68) THEN
            XPG = 0.3333D0
            YPG = 0.5556D0
            HPG = 1.D0/24.D0
          ELSE IF (NORD.EQ.69) THEN
            XPG = 0.3333D0
            YPG = 0.7778D0
            HPG = 1.D0/24.D0
          ELSE IF (NORD.EQ.70) THEN
            XPG = 0.3333D0
            YPG = 1.D0
            HPG = 1.D0/72.D0
          ELSE IF (NORD.EQ.71) THEN
            XPG = 0.5556D0
            YPG = -1.D0
            HPG = 1.D0/48.D0
          ELSE IF (NORD.EQ.72) THEN
            XPG = 0.5556D0
            YPG = -0.7778D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.73) THEN
            XPG = 0.5556D0
            YPG = -0.5556D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.74) THEN
            XPG = 0.5556D0
            YPG =  -0.3333D0
            HPG = 1.D0/24.D0
          ELSE IF (NORD.EQ.75) THEN
            XPG = 0.5556D0
            YPG = -0.1111D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.76) THEN
            XPG = 0.5556D0
            YPG = 0.1111D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.77) THEN
            XPG = 0.5556D0
            YPG = 0.3333D0
            HPG = 1.D0/24.D0
          ELSE IF (NORD.EQ.78) THEN
            XPG = 0.5556D0
            YPG = 0.5556D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.79) THEN
            XPG = 0.5556D0
            YPG = 0.7778D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.80) THEN
            XPG = 0.5556D0
            YPG = 1.D0
            HPG = 1.D0/48.D0
          ELSE IF (NORD.EQ.81) THEN
            XPG = 0.7778D0
            YPG = -1.D0
            HPG = 1.D0/48.D0
          ELSE IF (NORD.EQ.82) THEN
            XPG = 0.7778D0
            YPG = -0.7778D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.83) THEN
            XPG = 0.7778D0
            YPG = -0.5556D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.84) THEN
            XPG = 0.7778D0
            YPG =  -0.3333D0
            HPG = 1.D0/24.D0
          ELSE IF (NORD.EQ.85) THEN
            XPG = 0.7778D0
            YPG = -0.1111D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.86) THEN
            XPG = 0.7778D0
            YPG = 0.1111D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.87) THEN
            XPG = 0.7778D0
            YPG = 0.3333D0
            HPG = 1.D0/24.D0
          ELSE IF (NORD.EQ.88) THEN
            XPG = 0.7778D0
            YPG = 0.5556D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.89) THEN
            XPG = 0.7778D0
            YPG = 0.7778D0
            HPG = 1.D0/16.D0
          ELSE IF (NORD.EQ.90) THEN
            XPG = 0.7778D0
            YPG = 1.D0
            HPG = 1.D0/48.D0
          ELSE IF (NORD.EQ.91) THEN
            XPG = 1.D0
            YPG = -1.D0
            HPG = 1.D0/144.D0
          ELSE IF (NORD.EQ.92) THEN
            XPG = 1.D0
            YPG = -0.7778D0
            HPG = 1.D0/48.D0
          ELSE IF (NORD.EQ.93) THEN
            XPG = 1.D0
            YPG = -0.5556D0
            HPG = 1.D0/48.D0
          ELSE IF (NORD.EQ.94) THEN
            XPG = 1.D0
            YPG = -0.3333D0
            HPG = 1.D0/72.D0
          ELSE IF (NORD.EQ.95) THEN
            XPG = 1.D0
            YPG = -0.1111D0
            HPG = 1.D0/48.D0
          ELSE IF (NORD.EQ.96) THEN
            XPG = 1.D0
            YPG = 0.1111D0
            HPG = 1.D0/48.D0
          ELSE IF (NORD.EQ.97) THEN
            XPG = 1.D0
            YPG = 0.3333D0
            HPG = 1.D0/72.D0
          ELSE IF (NORD.EQ.98) THEN
            XPG = 1.D0
            YPG = 0.5556D0
            HPG = 1.D0/48.D0
          ELSE IF (NORD.EQ.99) THEN
            XPG = 1.D0
            YPG = 0.7778D0
            HPG = 1.D0/48.D0
          ELSE IF (NORD.EQ.100) THEN
            XPG = 1.D0
            YPG = 1.D0
            HPG = 1.D0/144.D0
          ELSE
            CALL ASSERT(.FALSE.)
          END IF
        ELSE
            CALL ASSERT(.FALSE.)
        END IF
C_______________________________________________________________________
C
      ELSEIF (ALIAS(1:3) .EQ. 'TR3') THEN
C
C    POINTS DE GAUSS
        IF (TYPI .EQ. 2) THEN
          IF (NORD .EQ. 1) THEN
            XPG = 1.D0/6.D0
            YPG = 1.D0/6.D0
          ELSEIF (NORD .EQ. 2) THEN
            XPG = 2.D0/3.D0
            YPG = 1.D0/6.D0
          ELSEIF (NORD .EQ. 3) THEN
            XPG = 1.D0/6.D0
            YPG = 2.D0/3.D0
          ELSE
            CALL ASSERT(.FALSE.)
          END IF
          HPG = 1.D0/6.D0
C   NOEUDS
        ELSEIF (TYPI .EQ. 1) THEN
          IF (NORD .EQ. 1) THEN
            XPG = 0.D0
            YPG = 0.D0
          ELSEIF (NORD .EQ. 2) THEN
            XPG = 1.D0
            YPG = 0.D0
          ELSEIF (NORD .EQ. 3) THEN
            XPG = 0.D0
            YPG = 1.D0
          ELSE
            CALL ASSERT(.FALSE.)
          END IF
          HPG = 1.D0/6.D0
C SIMPSON
        ELSEIF (TYPI .EQ. 3) THEN
          IF (NORD .EQ. 1) THEN
            XPG = 0.D0
            YPG = 0.D0
            HPG = 1.D0 / 30.D0
          ELSEIF (NORD .EQ. 2) THEN
            XPG = 1.D0
            YPG = 0.D0
            HPG = 1.D0 / 30.D0
          ELSEIF (NORD .EQ. 3) THEN
            XPG = 0.D0
            YPG = 1.D0
            HPG = 1.D0 / 30.D0
          ELSEIF (NORD .EQ. 4) THEN
            XPG = 1.D0 / 2.D0
            YPG = 0.D0
            HPG = 4.D0 / 30.D0
          ELSEIF (NORD .EQ. 5) THEN
            XPG = 1.D0 / 2.D0
            YPG = 1.D0 / 2.D0
            HPG = 4.D0 / 30.D0
          ELSEIF (NORD .EQ. 6) THEN
            XPG = 0.D0
            YPG = 1.D0 / 2.D0
            HPG = 4.D0 / 30.D0
          ELSE
            CALL ASSERT(.FALSE.)
          END IF
C HAMMER A 4 POINTS
        ELSE IF (TYPI .EQ. 6) THEN
          IF (NORD.EQ.1) THEN
            XPG = 1.D0 / 5.D0
            YPG = 1.D0 / 5.D0
            HPG = 25.D0/96.D0
          ELSE IF (NORD.EQ.2) THEN
            XPG = 3.D0 / 5.D0
            YPG = 1.D0 / 5.D0
            HPG = 25.D0/96.D0
          ELSE IF (NORD.EQ.3) THEN
            XPG = 1.D0 / 5.D0
            YPG = 3.D0 / 5.D0
            HPG = 25.D0/96.D0
          ELSE IF (NORD.EQ.4) THEN
            XPG = 1.D0 / 3.D0
            YPG = 1.D0 / 3.D0
            HPG = -27.D0/96.D0
          ELSE
            CALL ASSERT(.FALSE.)
          END IF
C HAMMER A 6 POINTS
        ELSE IF (TYPI .EQ. 7) THEN
          A=0.445948490915965D0
          B=0.091576213509771D0
C ATHOMAS VALEURS A VERIFIER
          P1=0.111690794839005D0
          P2=0.054975871827661D0
          IF (NORD.EQ.1) THEN
            XPG = B
            YPG = B
            HPG = P2
          ELSE IF (NORD.EQ.2) THEN
            XPG = 1.D0 - 2.D0*B
            YPG = B
            HPG = P2
          ELSE IF (NORD.EQ.3) THEN
            XPG = B
            YPG = 1.D0 - 2.D0*B
            HPG = P2
          ELSE IF (NORD.EQ.4) THEN
            XPG = A
            YPG = 1.D0 - 2.D0*A
            HPG = P1
          ELSE IF (NORD.EQ.5) THEN
            XPG = A
            YPG = A
            HPG = P1
          ELSE IF (NORD.EQ.6) THEN
            XPG = 1.D0 - 2.D0*A
            YPG = A
            HPG = P1
          ELSE
            CALL ASSERT(.FALSE.)
          END IF
        ELSE
            CALL ASSERT(.FALSE.)
        END IF
C
      ELSEIF (ALIAS(1:3) .EQ. 'TR6') THEN
C
C    POINTS DE GAUSS
        IF ((TYPI.EQ.2) .OR. (TYPI.EQ.1) .OR. (TYPI.EQ.3)
     &       .OR. (TYPI.EQ.7)) THEN
          A  = 0.445948490915965D0
          B  = 0.091576213509771D0
C ATHOMAS VALEURS A VERIFIER
          P1 = 0.111690794839005D0
          P2 = 0.054975871827661D0
          IF (NORD.EQ.1) THEN
            XPG = B
            YPG = B
            HPG = P2
          ELSE IF (NORD.EQ.2) THEN
            XPG = 1.D0 - 2.D0*B
            YPG = B
            HPG = P2
          ELSE IF (NORD.EQ.3) THEN
            XPG = B
            YPG = 1.D0 - 2.D0*B
            HPG = P2
          ELSE IF (NORD.EQ.4) THEN
            XPG = A
            YPG = 1.D0 - 2.D0*A
            HPG = P1
          ELSE IF (NORD.EQ.5) THEN
            XPG = A
            YPG = A
            HPG = P1
          ELSE IF (NORD.EQ.6) THEN
            XPG = 1.D0 - 2.D0*A
            YPG = A
            HPG = P1
          ELSE
            CALL ASSERT(.FALSE.)
          END IF
C HAMMER A 4 POINTS
        ELSE IF (TYPI .EQ. 6) THEN
          IF (NORD.EQ.1) THEN
            XPG = 1.D0 / 5.D0
            YPG = 1.D0 / 5.D0
            HPG = 25.D0/96.D0
          ELSE IF (NORD.EQ.2) THEN
            XPG = 3.D0 / 5.D0
            YPG = 1.D0 / 5.D0
            HPG = 25.D0/96.D0
          ELSE IF (NORD.EQ.3) THEN
            XPG = 1.D0 / 5.D0
            YPG = 3.D0 / 5.D0
            HPG = 25.D0/96.D0
          ELSE IF (NORD.EQ.4) THEN
            XPG = 1.D0 / 3.D0
            YPG = 1.D0 / 3.D0
            HPG = -27.D0/96.D0
          ELSE
            CALL ASSERT(.FALSE.)
          END IF
        ELSE
            CALL ASSERT(.FALSE.)
        END IF
C
      ELSEIF (ALIAS(1:3) .EQ. 'SE2') THEN
C
C POINTS DE GAUSS
C
        IF ((TYPI .EQ. 2).OR.(TYPI .EQ. 12)) THEN
          IF (NORD .EQ. 1) THEN
            XPG = -1.D0/SQRT(3.D0)
            YPG =  0.D0
          ELSEIF (NORD .EQ. 2) THEN
            XPG =  1.D0/SQRT(3.D0)
            YPG =  0.D0
          ELSE
            CALL ASSERT(.FALSE.)
          END IF
          HPG = 1.D0
        ELSEIF (TYPI .EQ. 13) THEN
          IF (NORD .EQ. 1) THEN
            XPG = -0.774596669241483D0
            YPG =  0.D0
            HPG =  0.555555555555556D0
          ELSEIF (NORD .EQ. 2) THEN
            XPG =  0.774596669241483D0
            YPG =  0.D0
            HPG =  0.555555555555556D0
          ELSEIF (NORD .EQ. 3) THEN
            XPG =  0.D0
            YPG =  0.D0
            HPG =  0.888888888888889D0
          ELSE
            CALL ASSERT(.FALSE.)
          END IF
        ELSEIF (TYPI .EQ. 14) THEN
          IF (NORD .EQ. 1) THEN
            XPG =  0.339981043584856D0
            YPG =  0.D0
            HPG =  0.652145154862546D0
          ELSEIF (NORD .EQ. 2) THEN
            XPG = -0.339981043584856D0
            YPG =  0.D0
            HPG =  0.652145154862546D0
          ELSEIF (NORD .EQ. 3) THEN
            XPG =  0.861136311594053D0
            YPG =  0.D0
            HPG =  0.347854845137454D0
          ELSEIF (NORD .EQ. 4) THEN
            XPG = -0.861136311594053D0
            YPG =  0.D0
            HPG =  0.347854845137454D0
          ELSE
            CALL ASSERT(.FALSE.)
          END IF
C LES NOEUDS
        ELSEIF (TYPI .EQ. 1) THEN
          IF (NORD .EQ. 1) THEN
            XPG = -1.D0
            YPG =  0.D0
          ELSEIF (NORD .EQ. 2) THEN
            XPG =  1.D0
            YPG =  0.D0
          ELSE
            CALL ASSERT(.FALSE.)
          END IF
          HPG = 1.D0
C SYMPSON
        ELSEIF (TYPI .EQ. 3) THEN
          IF (NORD .EQ. 1) THEN
            XPG = -1.D0
            YPG =  0.D0
            HPG =  1.D0 / 3.D0
          ELSEIF (NORD .EQ. 2) THEN
            XPG =  1.D0
            YPG =  0.D0
            HPG =  1.D0 / 3.D0
          ELSEIF (NORD .EQ. 3) THEN
            XPG =  0.D0
            YPG =  0.D0
            HPG =  4.D0 / 3.D0
          ELSE
            CALL ASSERT(.FALSE.)
          END IF
C SYMPSON1
        ELSEIF (TYPI .EQ. 4) THEN
          IF (NORD .EQ. 1) THEN
            XPG = -1.D0
            YPG =  0.D0
            HPG =  1.D0 / 6.D0
          ELSEIF (NORD .EQ. 2) THEN
            XPG =  1.D0
            YPG =  0.D0
            HPG =  1.D0 / 6.D0
          ELSEIF (NORD .EQ. 3) THEN
            XPG =  0.D0
            YPG =  0.D0
            HPG =  1.D0 / 3.D0
          ELSEIF (NORD .EQ. 4) THEN
            XPG = -0.5D0
            YPG =  0.D0
            HPG =  2.D0 / 3.D0
          ELSEIF (NORD .EQ. 5) THEN
            XPG =  0.5D0
            YPG =  0.D0
            HPG =  2.D0 / 3.D0
          ELSE
            CALL ASSERT(.FALSE.)
          END IF
C    SYMPSON2
        ELSEIF (TYPI .EQ. 5) THEN
          IF (NORD .EQ. 1) THEN
            XPG = -1.D0
            YPG =  0.D0
            HPG =  1.D0 / 12.D0
          ELSEIF (NORD .EQ. 2) THEN
            XPG =  1.D0
            YPG =  0.D0
            HPG =  1.D0 / 12.D0
          ELSEIF (NORD .EQ. 3) THEN
            XPG =  0.D0
            YPG =  0.D0
            HPG =  1.D0 / 6.D0
          ELSEIF (NORD .EQ. 4) THEN
            XPG = -0.75D0
            YPG =  0.D0
            HPG =  1.D0 / 3.D0
          ELSEIF (NORD .EQ. 5) THEN
            XPG = -0.5D0
            YPG =  0.D0
            HPG =  1.D0 / 6.D0
          ELSEIF (NORD .EQ. 6) THEN
            XPG = -0.25D0
            YPG =  0.D0
            HPG =  1.D0 / 3.D0
          ELSEIF (NORD .EQ. 7) THEN
            XPG =  0.25D0
            YPG =  0.D0
            HPG =  1.D0 / 3.D0
          ELSEIF (NORD .EQ. 8) THEN
            XPG =  0.5D0
            YPG =  0.D0
            HPG =  1.D0 / 6.D0
          ELSEIF (NORD .EQ. 9) THEN
            XPG =  0.75D0
            YPG =  0.D0
            HPG =  1.D0 / 3.D0
          ELSE
            CALL ASSERT(.FALSE.)
          END IF
C NEWTON COTES A 4 POINTS
        ELSE IF (TYPI.EQ.6) THEN
          IF (NORD.EQ.1) THEN
            XPG = -1.D0
            YPG =  0.D0
            HPG =  1.D0/4.D0
          ELSE IF (NORD.EQ.2) THEN
            XPG =  1.D0
            YPG =  0.D0
            HPG =  1.D0/4.D0
          ELSE IF (NORD.EQ.3) THEN
            XPG = -1.D0/3.D0
            YPG =  0.D0
            HPG =  3.D0/4.D0
          ELSE IF (NORD.EQ.4) THEN
            XPG =  1.D0/3.D0
            YPG =  0.D0
            HPG =  3.D0/4.D0
          ELSE
            CALL ASSERT(.FALSE.)
          END IF
C NEWTON COTES A 5 POINTS
        ELSE IF (TYPI.EQ.7) THEN
          IF (NORD.EQ.1) THEN
            XPG = -1.D0
            YPG =  0.D0
            HPG =  7.D0/45.D0
          ELSE IF (NORD.EQ.2) THEN
            XPG =  1.D0
            YPG =  0.D0
            HPG =  7.D0/45.D0
          ELSE IF (NORD.EQ.3) THEN
            XPG =  0.D0
            YPG =  0.D0
            HPG = 12.D0/45.D0
          ELSE IF (NORD.EQ.4) THEN
            XPG = -0.5D0
            YPG =  0.D0
            HPG = 32.D0/45.D0
          ELSE IF (NORD.EQ.5) THEN
            XPG =  0.5D0
            YPG =  0.D0
            HPG = 32.D0/45.D0
          ELSE
            CALL ASSERT(.FALSE.)
          END IF
C NEWTON COTES A 4 POINTS + SUBDIVISION --> 10 POINTS
        ELSE IF (TYPI.EQ.8) THEN
          IF (NORD.EQ.1) THEN
            XPG = -1.D0
            YPG =  0.D0
            HPG =  1.D0/12.D0
          ELSE IF (NORD.EQ.2) THEN
            XPG =  1.D0
            YPG =  0.D0
            HPG =  1.D0/12.D0
          ELSE IF (NORD.EQ.3) THEN
            XPG = -7.D0/9.D0
            YPG =  0.D0
            HPG =  1.D0/4.D0
          ELSE IF (NORD.EQ.4) THEN
            XPG = -5.D0/9.D0
            YPG =  0.D0
            HPG =  1.D0/4.D0
          ELSE IF (NORD.EQ.5) THEN
            XPG = -1.D0/3.D0
            YPG =  0.D0
            HPG =  1.D0/6.D0
          ELSE IF (NORD.EQ.6) THEN
            XPG = -1.D0/9.D0
            YPG =  0.D0
            HPG =  1.D0/4.D0
          ELSE IF (NORD.EQ.7) THEN
            XPG =  1.D0/9.D0
            YPG =  0.D0
            HPG =  1.D0/4.D0
          ELSE IF (NORD.EQ.8) THEN
            XPG =  1.D0/3.D0
            YPG =  0.D0
            HPG =  1.D0/6.D0
          ELSE IF (NORD.EQ.9) THEN
            XPG =  5.D0/9.D0
            YPG =  0.D0
            HPG =  1.D0/4.D0
          ELSE IF (NORD.EQ.10) THEN
            XPG =  7.D0/9.D0
            YPG =  0.D0
            HPG =  1.D0/4.D0
          ELSE
            CALL ASSERT(.FALSE.)
          END IF
        ELSE
            CALL ASSERT(.FALSE.)
        END IF
C
      ELSEIF (ALIAS(1:3) .EQ. 'SE3') THEN
C
C POINTS DE GAUSS
C
        IF (TYPI .EQ. 2) THEN
          IF (NORD .EQ. 1) THEN
            XPG = -0.774596669241483D0
            YPG =  0.D0
            HPG =  0.555555555555556D0
          ELSEIF (NORD .EQ. 2) THEN
            XPG =  0.774596669241483D0
            YPG =  0.D0
            HPG =  0.555555555555556D0
          ELSEIF (NORD .EQ. 3) THEN
            XPG =  0.D0
            YPG =  0.D0
            HPG =  0.888888888888889D0
          ELSE
            CALL ASSERT(.FALSE.)
          END IF
C LES NOEUDS
        ELSEIF (TYPI .EQ. 1) THEN
          IF (NORD .EQ. 1) THEN
            XPG = -1.D0
            YPG =  0.D0
            HPG =  1.D0/ 2.D0
          ELSEIF (NORD .EQ. 2) THEN
            XPG =  1.D0
            YPG =  0.D0
            HPG =  1.D0/ 2.D0
          ELSEIF (NORD .EQ. 3) THEN
            XPG =  0.D0
            YPG =  0.D0
            HPG =  1.D0
          ELSE
            CALL ASSERT(.FALSE.)
          END IF
C SYMPSON
        ELSEIF (TYPI .EQ. 3) THEN
          IF (NORD .EQ. 1) THEN
            XPG = -1.D0
            YPG =  0.D0
            HPG =  1.D0 / 3.D0
          ELSEIF (NORD .EQ. 2) THEN
            XPG =  1.D0
            YPG =  0.D0
            HPG =  1.D0 / 3.D0
          ELSEIF (NORD .EQ. 3) THEN
            XPG =  0.D0
            YPG =  0.D0
            HPG =  4.D0 / 3.D0
          ELSE
            CALL ASSERT(.FALSE.)
          END IF
C SYMPSON1
        ELSEIF (TYPI .EQ. 4) THEN
          IF (NORD .EQ. 1) THEN
            XPG = -1.D0
            YPG =  0.D0
            HPG =  1.D0 / 6.D0
          ELSEIF (NORD .EQ. 2) THEN
            XPG =  1.D0
            YPG =  0.D0
            HPG =  1.D0 / 6.D0
          ELSEIF (NORD .EQ. 3) THEN
            XPG =  0.D0
            YPG =  0.D0
            HPG =  1.D0 / 3.D0
          ELSEIF (NORD .EQ. 4) THEN
            XPG = -0.5D0
            YPG =  0.D0
            HPG =  2.D0 / 3.D0
          ELSEIF (NORD .EQ. 5) THEN
            XPG =  0.5D0
            YPG =  0.D0
            HPG =  2.D0 / 3.D0
          ELSE
            CALL ASSERT(.FALSE.)
          END IF
C
C    SYMPSON2
        ELSEIF (TYPI .EQ. 5) THEN
          IF (NORD .EQ. 1) THEN
            XPG = -1.D0
            YPG =  0.D0
            HPG =  1.D0 / 12.D0
          ELSEIF (NORD .EQ. 2) THEN
            XPG =  1.D0
            YPG =  0.D0
            HPG =  1.D0 / 12.D0
          ELSEIF (NORD .EQ. 3) THEN
            XPG =  0.D0
            YPG =  0.D0
            HPG =  1.D0 / 6.D0
          ELSEIF (NORD .EQ. 4) THEN
            XPG = -0.75D0
            YPG =  0.D0
            HPG =  1.D0 / 3.D0
          ELSEIF (NORD .EQ. 5) THEN
            XPG = -0.5D0
            YPG =  0.D0
            HPG =  1.D0 / 6.D0
          ELSEIF (NORD .EQ. 6) THEN
            XPG = -0.25D0
            YPG =  0.D0
            HPG =  1.D0 / 3.D0
          ELSEIF (NORD .EQ. 7) THEN
            XPG =  0.25D0
            YPG =  0.D0
            HPG =  1.D0 / 3.D0
          ELSEIF (NORD .EQ. 8) THEN
            XPG =  0.5D0
            YPG =  0.D0
            HPG =  1.D0 / 6.D0
          ELSEIF (NORD .EQ. 9) THEN
            XPG =  0.75D0
            YPG =  0.D0
            HPG =  1.D0 / 3.D0
          ELSE
            CALL ASSERT(.FALSE.)
          END IF
C NEWTON COTES A 4 POINTS
        ELSE IF (TYPI.EQ.6) THEN
          IF (NORD.EQ.1) THEN
            XPG = -1.D0
            YPG =  0.D0
            HPG =  1.D0/4.D0
          ELSE IF (NORD.EQ.2) THEN
            XPG =  1.D0
            YPG =  0.D0
            HPG =  1.D0/4.D0
          ELSE IF (NORD.EQ.3) THEN
            XPG = -1.D0/3.D0
            YPG =  0.D0
            HPG =  3.D0/4.D0
          ELSE IF (NORD.EQ.4) THEN
            XPG =  1.D0/3.D0
            YPG =  0.D0
            HPG =  3.D0/4.D0
          ELSE
            CALL ASSERT(.FALSE.)
          END IF
C NEWTON COTES A 5 POINTS
        ELSE IF (TYPI.EQ.7) THEN
          IF (NORD.EQ.1) THEN
            XPG = -1.D0
            YPG =  0.D0
            HPG =  7.D0/45.D0
          ELSE IF (NORD.EQ.2) THEN
            XPG =  1.D0
            YPG =  0.D0
            HPG =  7.D0/45.D0
          ELSE IF (NORD.EQ.3) THEN
            XPG =  0.D0
            YPG =  0.D0
            HPG = 12.D0/45.D0
          ELSE IF (NORD.EQ.4) THEN
            XPG = -0.5D0
            YPG =  0.D0
            HPG = 32.D0/45.D0
          ELSE IF (NORD.EQ.5) THEN
            XPG =  0.5D0
            YPG =  0.D0
            HPG = 32.D0/45.D0
          ELSE
            CALL ASSERT(.FALSE.)
          END IF
C NEWTON COTES A 4 POINTS + SUBDIVISION --> 10 POINTS
        ELSE IF (TYPI.EQ.8) THEN
          IF (NORD.EQ.1) THEN
            XPG = -1.D0
            YPG =  0.D0
            HPG =  1.D0/12.D0
          ELSE IF (NORD.EQ.2) THEN
            XPG =  1.D0
            YPG =  0.D0
            HPG =  1.D0/12.D0
          ELSE IF (NORD.EQ.3) THEN
            XPG = -7.D0/9.D0
            YPG =  0.D0
            HPG =  1.D0/4.D0
          ELSE IF (NORD.EQ.4) THEN
            XPG = -5.D0/9.D0
            YPG =  0.D0
            HPG =  1.D0/4.D0
          ELSE IF (NORD.EQ.5) THEN
            XPG = -1.D0/3.D0
            YPG =  0.D0
            HPG =  1.D0/6.D0
          ELSE IF (NORD.EQ.6) THEN
            XPG = -1.D0/9.D0
            YPG =  0.D0
            HPG =  1.D0/4.D0
          ELSE IF (NORD.EQ.7) THEN
            XPG =  1.D0/9.D0
            YPG =  0.D0
            HPG =  1.D0/4.D0
          ELSE IF (NORD.EQ.8) THEN
            XPG =  1.D0/3.D0
            YPG =  0.D0
            HPG =  1.D0/6.D0
          ELSE IF (NORD.EQ.9) THEN
            XPG =  5.D0/9.D0
            YPG =  0.D0
            HPG =  1.D0/4.D0
          ELSE IF (NORD.EQ.10) THEN
            XPG =  7.D0/9.D0
            YPG =  0.D0
            HPG =  1.D0/4.D0
          ELSE
            CALL ASSERT(.FALSE.)
          END IF
        ELSE
          CALL ASSERT(.FALSE.)
        END IF
      ELSE
        CALL ASSERT(.FALSE.)
      END IF
C
      END
