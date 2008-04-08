      SUBROUTINE ARLMAX(DIME  ,IAN   ,IAC   ,NNC   ,B     ,
     &                  ILGN  ,MX)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 08/04/2008   AUTEUR MEUNIER S.MEUNIER 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C
C
C ======================================================================
C RESPONSABLE MEUNIER S.MEUNIER
C
      IMPLICIT NONE
      INTEGER  DIME
      LOGICAL  IAN,IAC
      INTEGER  NNC
      REAL*8   B(*)
      INTEGER  ILGN(*)
      REAL*8   MX(2,*)
C
C ----------------------------------------------------------------------
C
C ROUTINE ARLEQUIN
C
C CALCUL DES MAXIMA EN VALEUR ABSOLUE DES TERMES SUIVANT LES LIGNES DE
C  LA MATRICE ARLEQUIN MORSE POUR L'ADIMENSIONNEMENT DES RELATIONS LIN.
C
C ----------------------------------------------------------------------
C
C
C IN  DIME   : DIMENSION DE L'ESPACE
C IN  IAN    : .TRUE. SI MODELE ZONE MECANIQUE EST DE TYPE COQUE
C IN  IAC    : .TRUE. SI MODELE ZONE COLLAGE EST DE TYPE COQUE
C IN  NNC    : NOMBRE DE NOEUDS MAILLES DE COLLAGE
C IN  B      : VALEURS DE LA MATRICE ARLEQUIN MORSE (CF ARLCAL)
C IN  ILGN   : LONGUEUR CUMULEE COLLECTION NOEUDS COLONNES DE B
C OUT MX     : MAXIMA EN VALEUR ABSOLUE TERMES DE LA MATRICE
C                       ARLEQUIN MORSE POUR NOEUDS DU DOMAINE DE COLLAGE
C                       SUIVANT LES DDLS DE TRANSLATION ET DE ROTATION
C                       ( MAX.LIGNE.1.TRANSLATION, MAX.LIGNE.1.ROTATION,
C                         MAX.LIGNE.2.TRANSLATION, ... )
C
C ----------------------------------------------------------------------
C
      INTEGER  P0,P1,P2,I,J,K,L,DR
      REAL*8   R
C
C ----------------------------------------------------------------------
C
      DR = 2*DIME - 3
      P1 = ILGN(1)
      P2 = 0
C
      DO 10 I = 1, NNC
        P0 = P1
        P1 = ILGN(I+1)
        DO 11 J = 1, P1-P0
C
C --- COUPLAGE TRANSLATION / TRANSLATION
C
          DO 20 K = 1, DIME
            DO 20 L = 1, DIME
              P2 = P2 + 1
              R = ABS(B(P2))
              IF (R.GT.MX(1,I)) MX(1,I) = R
 20       CONTINUE
C
C --- COUPLAGE ROTATION / TRANSLATION
C
          IF (IAC) THEN
            DO 30 K = 2, DIME
              DO 30 L = 1, DIME
                P2 = P2 + 1
                R = ABS(B(P2))
                IF (R.GT.MX(2,I)) MX(2,I) = R
 30         CONTINUE
          ENDIF
C
C --- COUPLAGE TRANSLATION / ROTATION
C
          IF (IAN) THEN
            DO 40 K = 1, DIME
              DO 40 L = 1, DR
                P2 = P2 + 1
                R = ABS(B(P2))
                IF (R.GT.MX(1,I)) MX(1,I) = R
 40         CONTINUE
C
C --- COUPLAGE ROTATION / ROTATION
C
            IF (IAC) THEN
              DO 50 K = 2, DIME
                DO 50 L = 1, DR
                  P2 = P2 + 1
                  R = ABS(B(P2))
                  IF (R.GT.MX(2,I)) MX(2,I) = R
 50           CONTINUE
            ENDIF
          ENDIF
 11     CONTINUE
 10   CONTINUE
      END
