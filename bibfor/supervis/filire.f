      SUBROUTINE FILIRE(ICLASS,IVAL,RVAL,KVAL)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER           ICLASS,IVAL
      REAL*8                        RVAL(2)
      CHARACTER*(*)                      KVAL
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 04/01/95   AUTEUR G8BHHAC A.Y.PORTABILITE 
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
C     LECTURE D'UN ITEM POUR L'ANALYSEUR SYNTAXIQUE DE FONCTIONS
C     INTERPRETEES
C     ------------------------------------------------------------------
C OUT ICLASS  CLASSE DE CE QUE L'ON A TROUVE
C     ------------------------------------------------------------------
C           -- TYPE -----    ---- INFORMATION --------------------------
C      -1   FIN DE FICHIER
C       0   ERREUR           KVAL DE TYPE CHARACTER*(*) DE LONGUEUR IVAL
C       1   ENTIER           IVAL DE TYPE INTEGER
C       2   REEL             RVAL(1) DE TYPE REAL*8
C       3   IDENTIFICATEUR   KVAL DE TYPE CHARACTER*(*) DE LONGUEUR IVAL
C       4   FONCTION         KVAL DE TYPE CHARACTER*(*) DE LONGUEUR IVAL
C       5   COMPLEXE         RVAL(1),RVAL(2) DE TYPE REAL*8
C       6   BOOLEEN          IVAL 1 VRAI , IVAL = 0 FAUX
C
C           SEPARATEUR       KVAL DE TYPE CHARACTER*(*) DE LONGUEUR 1
C       7 : '('       8 : ')'           9 :  ','
C      10 : ':'      11 : '='          12 :  ';'
C      13 : '*'      14 : '/'          15 :  '+'
C      16 : '-'      17 : SEPARATEUR INDEFINI
C     ------------------------------------------------------------------
C     ROUTINE(S) UTILISEE(S) :
C         LXLIRE
C     ROUTINE(S) FORTRAN     :
C
C     ------------------------------------------------------------------
C FIN FILIRE
C     ------------------------------------------------------------------
      CHARACTER*2 OPERA
C     ------------------------------------------------------------------
C
C
C     --- LECTURE D'UNE ENTITE LEXICALE ---
C     INITIALISATION DES VALEURS POUR QU'IL N'Y AIT PAS REMANENCE D'UNE
C     LECTURE A L'AUTRE
      ICLASS=0
      IVAL=0
      RVAL(1) = 0.0D0
      RVAL(2) = 0.0D0
      KVAL=' '
      CALL SNLIRE(ICLASS,IVAL,RVAL,KVAL)
C     ------------------------------------------------------------------
C                          ICLASS      CODE DE CE QUE L'ON A TROUVE
C           -- TYPE -----    ---- INFORMATION --------------------------
C      -1   FIN DE FICHIER
C       0   ERREUR           KVAL DE TYPE CHARACTER*(*) DE LONGUEUR IVAL
C       1   ENTIER           IVAL DE TYPE INTEGER
C       2   REEL             RVAL(1) DE TYPE REAL*8
C       3   IDENTIFICATEUR   KVAL DE TYPE CHARACTER*(*) DE LONGUEUR IVAL
C       4   TEXTE            KVAL DE TYPE CHARACTER*(*) DE LONGUEUR IVAL
C       5   COMPLEXE         RVAL(1),RVAL(2) DE TYPE REAL*8
C       6   BOOLEEN          IVAL 1 VRAI , IVAL = 0 FAUX
C
C           SEPARATEUR       KVAL DE TYPE CHARACTER*(*) DE LONGUEUR 1
C       7   '('        8   ')'            9   ','        10   ':'
C      11   '='       12   ';'           13   SEPARATEUR INDEFINI
C     ------------------------------------------------------------------
      IF (ICLASS.EQ.13)  THEN
C
C         --- EXTENSION DES SEPARATEURS ---
          IF ( KVAL(:1).EQ.'*' ) THEN
             ICLASS = 13
          ELSEIF ( KVAL(:1).EQ.'/' ) THEN
             ICLASS = 14
          ELSEIF ( KVAL(:1).EQ.'+' ) THEN
             ICLASS = 15
          ELSEIF ( KVAL(:1).EQ.'-' ) THEN
             ICLASS = 16
          ELSE
             ICLASS = 17
             IF ( KVAL(:1).EQ.'.' ) THEN
C               --- ON CHERCHE A RECUPERER .EQ. .NE. .GT. .GE .LE .LT.
                CALL SNLIRE(ICLASS,IVAL,RVAL,KVAL)
                IF (ICLASS.EQ.3) THEN
                   IF (IVAL.EQ.2) THEN
                      OPERA = KVAL(1:2)
                      CALL SNLIRE(ICLASS,IVAL,RVAL,KVAL)
                      IF (ICLASS.EQ.13.AND.KVAL(1:1).EQ.'.') THEN
                         IVAL   = 4
                         KVAL   = '.'//OPERA//'.'
                         ICLASS = 3
                      ELSE
                         ICLASS = 0
                         CALL UTMESS('E','FILIRE.01',
     +                           'ECRITURE D''OPERATEUR LOGIQUE ERRONE')
                      ENDIF
                   ELSE
                      ICLASS = 0
                      CALL UTMESS('E','FILIRE.01',
     +                           'ECRITURE D''OPERATEUR LOGIQUE ERRONE')
                   ENDIF
                ELSE
                   ICLASS = 0
                   CALL UTMESS('E','FILIRE.01',
     +                           'ECRITURE D''OPERATEUR LOGIQUE ERRONE')
                ENDIF
             ENDIF
          ENDIF
      ENDIF
C
      END
