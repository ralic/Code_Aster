      SUBROUTINE SNLIRE(ICLASS,IVAL,RVAL,CVAL)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER           ICLASS,IVAL
      REAL*8                        RVAL(2)
      CHARACTER*(*)                      CVAL
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 05/12/2001   AUTEUR DURAND C.DURAND 
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
C              LECTURE D'UN ITEM POUR L'ANALYSEUR SYNTAXIQUE
C     ------------------------------------------------------------------
C OUT ICLASS  CLASSE DE CE QUE L'ON A TROUVE
C     ------------------------------------------------------------------
C           -- TYPE -----    ---- INFORMATION --------------------------
C      -1   FIN DE FICHIER
C       0   ERREUR           CVAL DE TYPE CHARACTER*(*) DE LONGUEUR IVAL
C       1   ENTIER           IVAL DE TYPE INTEGER
C       2   REEL             RVAL(1) DE TYPE REAL*8
C       3   IDENTIFICATEUR   CVAL DE TYPE CHARACTER*(*) DE LONGUEUR IVAL
C       4   TEXTE            CVAL DE TYPE CHARACTER*(*) DE LONGUEUR IVAL
C       5   COMPLEXE         RVAL(1),RVAL(2) DE TYPE REAL*8
C       6   BOOLEEN          IVAL 1 VRAI , IVAL = 0 FAUX
C
C           SEPARATEUR       CVAL DE TYPE CHARACTER*(*) DE LONGUEUR 1
C       7   '('
C       8   ')'
C       9   ','
C      10   ':'
C      11   '='
C      12   ';'
C      13   SEPARATEUR INDEFINI
C     ------------------------------------------------------------------
C     REMARQUE SUR LES COMPLEXES
C
C        L'ANALYSEUR RENVOI LES COMPLEXES DANS LA REPRESENTATION RI
C
C        RAPPEL DE LA SYNTAXE EXTERNE DES COMPLEXES
C                   RI (,) &REEL (,)  &REEL
C                   MP (,) &REEL (,)  &REEL
C     ------------------------------------------------------------------
C     ROUTINE(S) UTILISEE(S) :
C         LXLIRE  LXLIGN  LXCAPS   LXERR
C     ROUTINE(S) FORTRAN     :
C         MOD     SIN     COS
C     ------------------------------------------------------------------
C FIN SNLIRE
C     ------------------------------------------------------------------
C
      CHARACTER*1   COMMEN
      PARAMETER   ( COMMEN = '%' )
C     ------------------------------------------------------------------
C     VARIABLES LOCALES SPECIFIQUES A LA REPRESENTATION DES COMPLEXES
      INTEGER CMPLX
C             CMPLX --- TYPE DE REPRESENTATION D'UN COMPLEXE
C             CMPLX = 1 : COMPLEXE REPRESENTE EN REEL-IMAGINAIRE
C             CMPLX = 2 : COMPLEXE REPRESENTE EN MODULE-PHASE
      INTEGER IETAT
C             IETAT --- ETAT DU MINI AUTOMATE
C             IETAT =  0  ON ATTEND UNE VIRGULE OU UN 1-ER NUMERIQUE
C             IETAT =  1  ON ATTEND SEULEMENT UN 1-ER NUMERIQUE
C             IETAT =  2  ON ATTEND UNE VIRGULE OU UN 2-ND NUMERIQUE
C             IETAT =  3  ON ATTEND SEULEMENT UN 2-ND NUMERIQUE
C             IETAT =  4  C'EST FINI
C
      REAL*8  R(2)
      CHARACTER*2 KMES
C        TYPE DE DEFINITION DU COMPLEXE MI OU RP POUR DONNER PLUS D"INFO
C        LORS D'UNE ERREUR DE DEFINITION
C     ------------------------------------------------------------------
C
C
      CMPLX = 0
  1   CONTINUE
C
C        LECTURE D'UNE ENTITE LEXICALE
C
         CALL LXLIRE(ICLASS,IVAL,RVAL,CVAL)
C        ---------------------------------------------------------------
C         ICLASS      CLASSE DE CE QUE L'ON A TROUVE
C        ----- TYPE -----    ---- INFORMATION --------------------------
C         -1 FIN DE FICHIER (RIEN A LIRE)
C          0 ERREUR         CVAL DE TYPE CHARACTER*(*) DE LONGUEUR IVAL
C          1 ENTIER         IVAL DE TYPE INTEGER
C          2 REEL           RVAL DE TYPE REAL*8
C          3 IDENTIFICATEUR CVAL DE TYPE CHARACTER*(*) DE LONGUEUR IVAL
C          4 TEXTE          CVAL DE TYPE CHARACTER*(*) DE LONGUEUR IVAL
C          5 SEPARATEUR     CVAL DE TYPE CHARACTER*(*) DE LONGUEUR 1
C        ---------------------------------------------------------------
   2     CONTINUE
         IF (ICLASS.EQ.5 .AND. CVAL(1:1).EQ.COMMEN ) THEN
            CALL LXLIGN(ICLASS,IVAL,RVAL,CVAL)
            GOTO 2
         ENDIF
C
         IF ( CMPLX.NE.0 ) THEN
C
C           TRAITEMENT DES COMPLEXES EN COURS
C              RAPPEL DES ETATS
C                IETAT = 0  ON ATTEND UNE VIRGULE OU UN 1-ER NUMERIQUE
C                IETAT = 1  ON ATTEND SEULEMENT UN 1-ER NUMERIQUE
C                IETAT = 2  ON ATTEND UNE VIRGULE OU UN 2-ND NUMERIQUE
C                IETAT = 3  ON ATTEND SEULEMENT UN 2-ND NUMERIQUE
C                IETAT = 4  FIN DE LECTURE
C
            IF ( ICLASS.EQ.5 .AND. CVAL(1:1).EQ.',' ) THEN
               IF ( MOD(IETAT,2) .NE.0 ) THEN
                  GOTO 99001
               ELSE
                  IETAT = IETAT + 1
               ENDIF
            ELSEIF ( ICLASS.EQ.1 ) THEN
               IACT    = IETAT/2 + 1
               R(IACT) = IVAL
               IETAT   = 2*IACT
            ELSEIF ( ICLASS.EQ.2 ) THEN
               IACT    = IETAT/2 + 1
               R(IACT) = RVAL(1)
               IETAT   = 2*IACT
            ELSE
               GOTO 99001
            ENDIF
C
            IF ( IETAT.EQ. 4 ) THEN
C             ON A FINI DE LIRE NOTRE COMPLEXE. VOUS AVEZ DIT COMPLEXE?
              ICLASS = 5
              IF ( CMPLX .EQ. 2 ) THEN
C                CONVERSION AUTOMATIQUE DEGRE EN RADIAN
                 R(2)=R8DGRD()*R(2)
                 RVAL(1) = R(1) * COS( R(2) )
                 RVAL(2) = R(1) * SIN( R(2) )
              ELSE
                 RVAL(1) = R(1)
                 RVAL(2) = R(2)
              ENDIF
            ELSE
C             ON CONTINUE LA LECTURE
              GOTO 1
            ENDIF
C
         ELSEIF ( ICLASS.EQ.3 ) THEN
C
C           TRAITEMENT DES IDENTIFICATEURS
C
C           ON PASSE EN MAJUSCULE
CCC            CALL LXCAPS(CVAL(:IVAL))
C
C           TRAITEMENT SI ON VA LIRE UN COMPLEXE
            IF ( IVAL .EQ. 2 ) THEN
               IF (CVAL(1:2) .EQ. 'RI' ) THEN
                  CMPLX  =  1
                  IETAT  =  0
               ELSEIF (CVAL(1:2) .EQ. 'MP' ) THEN
                  CMPLX  =  2
                  IETAT  =  0
               ENDIF
               IF ( CMPLX.NE.0 ) GOTO 1
C
            ELSEIF ( IVAL .EQ. 4 ) THEN
C
C               TRAITEMENT DES BOOLEENS
C
               IF (CVAL(1:4) .EQ. 'VRAI' ) THEN
                  IVAL   = 1
                  ICLASS = 6
               ELSEIF (CVAL(1:4) .EQ. 'FAUX' ) THEN
                  IVAL   = 0
                  ICLASS = 6
               ENDIF
           ENDIF
C
      ELSEIF (ICLASS.EQ.5) THEN
C
C        TRAITEMENT DES SEPARATEURS
C
          IF     ( CVAL(:1).EQ.':' ) THEN
             ICLASS = 10
          ELSEIF ( CVAL(:1).EQ.'(' ) THEN
             ICLASS =  7
          ELSEIF ( CVAL(:1).EQ.')' ) THEN
             ICLASS =  8
          ELSEIF ( CVAL(:1).EQ.',' ) THEN
             ICLASS =  9
          ELSEIF ( CVAL(:1).EQ.'=' ) THEN
             ICLASS = 11
          ELSEIF ( CVAL(:1).EQ.';' ) THEN
            ICLASS = 12
          ELSE
            ICLASS = 13
          ENDIF
      ENDIF
C
      GOTO 9999
C
C     --- SORTIE EN ERREUR
99001 CONTINUE
      IF (CMPLX .EQ. 1) THEN
         KMES='RI'
      ELSE
         KMES='MP'
      ENDIF
      CALL LXERR(CVAL(1:IVAL),'CODAGE DE COMPLEXE INCORRECT.'//
     +        '    ('//KMES//' EST UN MOT-CLE DU SUPERVISEUR)')
      ICLASS = 0
 9999 CONTINUE
      END
