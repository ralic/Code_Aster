      SUBROUTINE FORMEN(TYPEMA,TYPEDG,DEGRE)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 14/09/2010   AUTEUR REZETTE C.REZETTE 
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
C A_UTIL
C ----------------------------------------------------------------------
C                     DEGRES DES FONCTIONS DE FORMES
C ----------------------------------------------------------------------
C VARIABLE D'ENTREE
C CHARACTER*8   TYPEMA     :  TYPE DE MAILLE
C
C VARIABLE D'ENTREE / SORTIE
C INTEGER       TYPEDG     :  TYPE DE DEGRE (CF NPGAUS)
C                             EN NOTANT LES TERMES X**A Y**B Z**C
C                               1D      ->       MAX(A)
C                               2D TYPEDG = 1 -> MAX(A+B)
C                                  TYPEDG = 2 -> MAX(A,B)
C                               3D TYPEDG = 1 -> MAX(A+B+C)
C                                  TYPEDG = 2 -> MAX(A,B+C)
C                                  TYPEDG = 3 -> MAX(A,B,C)
C
C                              SI TYPEDG = 0, CHOIX AUTOMATIQUE
C                                 TYPEMA = SEG   -> TYPEDG = 1
C                                 TYPEMA = TRIA  -> TYPEDG = 1
C                                 TYPEMA = QUAD  -> TYPEDG = 2
C                                 TYPEMA = TETRA -> TYPEDG = 1
C                                 TYPEMA = PENTA -> TYPEDG = 2
C                                 TYPEMA = HEXA  -> TYPEDG = 3
C
C VARIABLE DE SORTIE
C INTEGER       DEGRE(4)   :  1 -> DEGRE A L'ORDRE 0
C                             2 -> DEGRE A L'ORDRE 1
C                             3 -> DEGRE DU JACOBIEN
C                             4 -> DEGRE COMATRICE JACOBIENNE
C ----------------------------------------------------------------------

      IMPLICIT NONE

C --- VARIABLES
      CHARACTER*8 TYPEMA
      INTEGER     TYPEDG,INDEX,I,DEGRE(4)

C --- TABLES
      INTEGER DEGRES(4,3,20)
      DATA DEGRES /
     &  1,0,0,0,  1,0,0,0,  1,0,0,0,   2,1,1,1,  2,1,1,1,  2,1,1,1,
     &  1,0,0,0,  1,0,0,0,  1,0,0,0,   2,1,2,1,  2,1,2,1,  2,1,2,1,
     &  3,2,3,2,  2,2,3,2,  2,2,3,2,   2,1,1,1,  1,1,1,1,  1,1,1,1,
     &  3,2,3,2,  2,2,3,2,  2,2,3,2,   3,2,4,2,  2,2,3,2,  2,2,3,2,
     &  4,3,5,3,  2,2,3,2,  2,2,3,2,   1,0,0,0,  1,0,0,0,  1,0,0,0,
     &  2,1,3,2,  2,1,3,2,  2,1,3,2,   2,1,2,2,  1,1,2,2,  1,1,2,2,
     &  3,2,5,4,  2,2,4,3,  2,2,4,3,   4,3,7,5,  3,3,6,5,  2,2,5,4,
     &  3,2,6,4,  2,2,5,4,  2,2,5,4,   3,2,4,3,  2,2,3,3,  1,1,2,2,
     &  4,3,8,6,  3,3,7,5,  2,2,5,4,   5,4,10,7, 4,4,9,7,  2,2,5,4,
     &  4,3,9,6,  3,3,7,5,  2,2,5,4,   6,5,13,9, 4,4,9,7,  2,2,5,4 /

      IF (TYPEDG.EQ.0) THEN

        IF (TYPEMA(1:4).EQ.'HEXA') THEN
          TYPEDG = 3
        ELSEIF (TYPEMA(1:4).EQ.'QUAD') THEN
          TYPEDG = 2
        ELSEIF (TYPEMA(1:5).EQ.'PENTA') THEN
          TYPEDG = 2
        ELSE
          TYPEDG = 1
        ENDIF

      ENDIF

      IF (TYPEMA(1:3).EQ.'SEG') THEN

        IF (TYPEMA(4:4).EQ.'2') THEN
          INDEX = 1
        ELSEIF (TYPEMA(4:4).EQ.'3') THEN
          INDEX = 2
        ELSE
          GOTO 20
        ENDIF

      ELSEIF (TYPEMA(1:4).EQ.'TRIA') THEN

        IF (TYPEMA(5:5).EQ.'3') THEN
          INDEX = 3
        ELSEIF (TYPEMA(5:5).EQ.'6') THEN
          INDEX = 4
        ELSEIF (TYPEMA(5:5).EQ.'7') THEN
          INDEX = 5
        ELSE
          GOTO 20
        ENDIF

      ELSEIF (TYPEMA(1:4).EQ.'QUAD') THEN

        IF (TYPEMA(5:5).EQ.'4') THEN
          INDEX = 6
        ELSEIF (TYPEMA(5:5).EQ.'6') THEN
          INDEX = 7
        ELSEIF (TYPEMA(5:5).EQ.'8') THEN
          INDEX = 8
        ELSEIF (TYPEMA(5:5).EQ.'9') THEN
          INDEX = 9
        ELSE
          GOTO 20
        ENDIF

      ELSEIF (TYPEMA(1:5).EQ.'TETRA') THEN

        IF (TYPEMA(6:6).EQ.'4') THEN
          INDEX = 10
        ELSEIF (TYPEMA(6:7).EQ.'10') THEN
          INDEX = 11
        ELSE
          GOTO 20
        ENDIF

      ELSEIF (TYPEMA(1:5).EQ.'PENTA') THEN

        IF (TYPEMA(6:6).EQ.'6') THEN
          INDEX = 12
        ELSEIF (TYPEMA(6:7).EQ.'12') THEN
          INDEX = 13
        ELSEIF (TYPEMA(6:7).EQ.'14') THEN
          INDEX = 14
        ELSEIF (TYPEMA(6:7).EQ.'15') THEN
          INDEX = 15
        ELSEIF (TYPEMA(6:7).EQ.'18') THEN
          INDEX = 15
        ELSE
          GOTO 20
        ENDIF

      ELSEIF (TYPEMA(1:4).EQ.'HEXA') THEN

        IF (TYPEMA(5:5).EQ.'8') THEN
          INDEX = 16
        ELSEIF (TYPEMA(5:6).EQ.'16') THEN
          INDEX = 17
        ELSEIF (TYPEMA(5:6).EQ.'18') THEN
          INDEX = 18
        ELSEIF (TYPEMA(5:6).EQ.'20') THEN
          INDEX = 19
        ELSEIF (TYPEMA(5:6).EQ.'27') THEN
          INDEX = 20
        ELSE
          GOTO 20
        ENDIF

      ELSE
        GOTO 20
      ENDIF

      DO 10 I = 1, 4
        DEGRE(I) = DEGRES(I,TYPEDG,INDEX)
 10   CONTINUE

      GOTO 30

 20   CONTINUE

      CALL U2MESS('F','CALCULEL_16')

 30   CONTINUE

       END
