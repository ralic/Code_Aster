      SUBROUTINE NPGAUS(TYPEMA,DEGRE,FG)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 08/11/2004   AUTEUR DURAND C.DURAND 
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
C A_UTIL
C ----------------------------------------------------------------------
C      TYPE DE MAILLE + DEGRE -> FAMILLE D'INTEGRATION A UTILISER
C ----------------------------------------------------------------------
C VARIABLES D'ENTREE
C CHARACTER*8    TYPEMA     : TYPE DE LA MAILLE
C INTEGER        DEGRE      : DEGRE DE L'INTEGRANDE (CF FORMEN)
C                             EN NOTANT LES TERMES X**A Y**B Z**C
C                               TYPEMA = SEG   -> DEGRE = MAX(A)
C                               TYPEMA = TRIA  -> DEGRE = MAX(A+B)
C                               TYPEMA = QUAD  -> DEGRE = MAX(A,B)
C                               TYPEMA = TETRA -> DEGRE = MAX(A+B+C)
C                               TYPEMA = PENTA -> DEGRE = MAX(A,B+C)
C                               TYPEMA = HEXA  -> DEGRE = MAX(A,B,C)
C VARIABLE DE SORTIE
C INTEGER        FG         : FAMILLE D'INTEGRATION
C ----------------------------------------------------------------------
C ATTENTION : ON N'UTILISE QUE LES FORMULES DE QUADRATURE DONT LES
C             POINTS D'ENCHANTILLONAGE SONT STRICTEMENT A L'INTERIEUR
C             DU DOMAINE D'INTEGRATION ET DONT LES POIDS SONT 
C             STRICTEMENT POSITIFS
C ----------------------------------------------------------------------

      IMPLICIT NONE

C --- VARIABLES
      CHARACTER*8 TYPEMA
      INTEGER     FG,DEGRE,I
      INTEGER     SEG(2,4),TRI(2,5),QUA(2,4),TET(2,2),PEN(2,2),HEX(2,3)

C --- DONNEES (DEGRE,FAMILLE, DEGRE,FAMILLE, ...)
      DATA SEG /1,1, 3,2, 5,3, 7,4/
      DATA TRI /1,1, 2,4, 4,6, 5,7, 6,8/
      DATA QUA /1,1, 3,3, 5,6, 7,7/
      DATA TET /2,1, 5,3/
      DATA PEN /2,2, 5,4/
      DATA HEX /3,1, 5,2, 7,3/

      I = 0

      IF (TYPEMA(1:3).EQ.'SEG') THEN
 10     CONTINUE
        I = I + 1
        IF ((I.LT.4).AND.(SEG(1,I).LT.DEGRE)) GOTO 10
        FG = SEG(2,I)

      ELSEIF (TYPEMA(1:4).EQ.'TRIA') THEN
 20     CONTINUE
        I = I + 1
        IF ((I.LT.5).AND.(TRI(1,I).LT.DEGRE)) GOTO 20
        FG = TRI(2,I)
        
      ELSEIF (TYPEMA(1:4).EQ.'QUAD') THEN
 30     CONTINUE
        I = I + 1
        IF ((I.LT.4).AND.(QUA(1,I).LT.DEGRE)) GOTO 30
        FG = QUA(2,I)

      ELSEIF (TYPEMA(1:5).EQ.'TETRA') THEN
 40     CONTINUE
        I = I + 1
        IF ((I.LT.2).AND.(TET(1,I).LT.DEGRE)) GOTO 40
        FG = TET(2,I)
  
      ELSEIF (TYPEMA(1:5).EQ.'PENTA') THEN
 50     CONTINUE
        I = I + 1
        IF ((I.LT.2).AND.(PEN(1,I).LT.DEGRE)) GOTO 50
        FG = PEN(2,I)

      ELSEIF (TYPEMA(1:4).EQ.'HEXA') THEN
 60     CONTINUE
        I = I + 1
        IF ((I.LT.3).AND.(HEX(1,I).LT.DEGRE)) GOTO 60
        FG = HEX(2,I)

      ELSE

        CALL UTMESS('F','NPGAUS','TYPE DE MAILLE INDISPONIBLE')

      ENDIF

      END
