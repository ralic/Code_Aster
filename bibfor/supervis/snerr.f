      SUBROUTINE SNERR ( ERREUR, ICLASS, IVAL, CVAL )
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER            ERREUR, ICLASS, IVAL
      CHARACTER*(*)                                  CVAL
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 08/03/2004   AUTEUR REZETTE C.REZETTE 
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
C     EMISSION DE MESSAGE D'ERREUR DEPUIS L'ANALYSEUR SYNTAXIQUE.
C     ------------------------------------------------------------------
      INTEGER        JLIG, JCOL , LG
      CHARACTER*72   CHAINE, LIGNE
C     ------------------------------------------------------------------
      CHARACTER*24   TYPE  (-1:13)
      INTEGER        LGTYPE(-1:13)
C     ------------------------------------------------------------------
C                         1234567890123456789012
      DATA    TYPE(-1) / 'UNE FIN DE FICHIER    '/ , LGTYPE(-1) / 18/
      DATA    TYPE( 0) / 'UNE ERREUR LEXICALE   '/ , LGTYPE( 0) / 19/
      DATA    TYPE( 1) / 'UN ENTIER             '/ , LGTYPE( 1) /  9/
      DATA    TYPE( 2) / 'UN REEL               '/ , LGTYPE( 2) /  7/
      DATA    TYPE( 3) / 'UN IDENTIFICATEUR     '/ , LGTYPE( 3) / 17/
      DATA    TYPE( 4) / 'UN TEXTE              '/ , LGTYPE( 4) /  8/
      DATA    TYPE( 5) / 'UN COMPLEXE           '/ , LGTYPE( 5) / 11/
      DATA    TYPE( 6) / 'UN BOOLEEN            '/ , LGTYPE( 6) / 10/
      DATA    TYPE( 7) / 'LE SEPARATEUR "("     '/ , LGTYPE( 7) / 17/
      DATA    TYPE( 8) / 'LE SEPARATEUR ")"     '/ , LGTYPE( 8) / 17/
      DATA    TYPE( 9) / 'LE SEPARATEUR ","     '/ , LGTYPE( 9) / 17/
      DATA    TYPE(10) / 'LE SEPARATEUR ":"     '/ , LGTYPE(10) / 17/
      DATA    TYPE(11) / 'LE SEPARATEUR "="     '/ , LGTYPE(11) / 17/
      DATA    TYPE(12) / 'LE SEPARATEUR ";"     '/ , LGTYPE(12) / 17/
      DATA    TYPE(13) / 'UN SEPARATEUR INDEFINI'/ , LGTYPE(13) / 22/
C     ------------------------------------------------------------------
      CHAINE = CVAL
      LG     = MIN (72,IVAL)
C
      CALL LXINFO(LIGNE,JLIG,JCOL)
C
      IFE = IUNIFI('ERREUR')
      IFM = IUNIFI('RESULTAT')
      IF (IFE.GT.0)
     +             WRITE(IFE,'(1X,I5,''!'',A,''!'')' ) JLIG, LIGNE
      IF ( IFM.GT.0 .AND. IFE.NE.IFM )
     +             WRITE(IFM,'(1X,I5,''!'',A,''!'')' ) JLIG, LIGNE
C
      CALL PRSOUL('*', JCOL-IVAL,IVAL)
C
C     --- EMISSION DU MESSAGE D'ERREUR LEXICAL ---
      IF ( ICLASS .EQ. 00 ) THEN
C       DOIT DISPARAITRE SI LE MESSAGE EST EMIS PAR L'ANALYSEUR LEXICAL
         CALL UTMESS('E','ERREUR LEXICALE (99)',
     +                   '"'//CHAINE(1:LG)//'" CHAINE INCOHERENTE')
      ELSE
         CALL UTMESS('E','ERREUR',
     +                   'ON A TROUVE '//TYPE(ICLASS)(:LGTYPE(ICLASS)))
      ENDIF
C
C     --- EMISSION DU MESSAGE D'ERREUR SYNTAXIQUE ---
      IF ( ERREUR .EQ. 00) THEN
         CALL UTMESS('E','ERREUR SYNTAXIQUE (00)',
     +                                        'ERREUR NON REPERTORIEE')
      ELSEIF ( ERREUR .EQ. 1 ) THEN
         CALL UTMESS('E','ERREUR SYNTAXIQUE (01)',
     +                   'CHAINE NON ATTENDUE POUR DEBUTER UNE PHRASE.')
      ELSEIF ( ERREUR .EQ. 2 ) THEN
         CALL UTMESS('E','ERREUR SYNTAXIQUE (02)',
     +                   'ON ATTENDAIT  "=" PRECEDENT UN OPERATEUR, '//
     +                   'OU UNE "(" POUR UNE PROCEDURE.')
      ELSEIF ( ERREUR .EQ. 3 ) THEN
         CALL UTMESS('E','ERREUR SYNTAXIQUE (03)',
     +                   'ON ATTENDAIT LE NOM D''UN OPERATEUR.')
      ELSEIF ( ERREUR .EQ. 4 ) THEN
         CALL UTMESS('E','ERREUR SYNTAXIQUE (04)',
     +                   'ON ATTENDAIT UNE "(".')
      ELSEIF ( ERREUR .EQ. 5 ) THEN
         CALL UTMESS('E','ERREUR SYNTAXIQUE (05)',
     +                   'ON ATTENDAIT UN MOT_CLE, '//
     +                   'OU UN MOT CLE FACTEUR.')
      ELSEIF ( ERREUR .EQ. 6 ) THEN
         CALL UTMESS('E','ERREUR SYNTAXIQUE (06)',
     +                   'ON ATTENDAIT UN ":".')
      ELSEIF ( ERREUR .EQ. 7 ) THEN
         CALL UTMESS('E','ERREUR SYNTAXIQUE (07)',
     +                   'DERRIERE LE SIGNE "&" INDIQUANT UNE DONNEE '//
     +                   'MODIFIEE ON ATTENDAIT UN NOM DE CONCEPT.')
      ELSEIF ( ERREUR .EQ. 8 ) THEN
         CALL UTMESS('E','ERREUR SYNTAXIQUE (08)',
     +                   'ON ATTENDAIT UN ARGUMENT DERRIERE UN MOT_'//
     +                   'CLE OU UNE "("  DERRIERE UN MOT_CLE_FACTEUR.')
      ELSEIF ( ERREUR .EQ. 9 ) THEN
         CALL UTMESS('E','ERREUR SYNTAXIQUE (09)',
     +                   'ON ATTENDAIT UN ARGUMENT OU UN MOT_CLE.')
      ELSEIF ( ERREUR .EQ.10 ) THEN
         CALL UTMESS('E','ERREUR SYNTAXIQUE (10)',
     +                   'ON ATTENDAIT UN ARGUMENT OU UN MOT_CLE '//
     +                   'OU UN MOT_CLE_FACTEUR OU ":" OU ")".')
      ELSEIF ( ERREUR .EQ. 11) THEN
         CALL UTMESS('E','ERREUR SYNTAXIQUE (11)',
     +                  'ON ATTENDAIT UN ARGUMENT DERRIERE UN MOT_CLE.')
      ELSEIF ( ERREUR .EQ. 12) THEN
         CALL UTMESS('E','ERREUR SYNTAXIQUE (12)',
     +                   'ON ATTENDAIT UN '//
     +                   'MOT_CLE DERRIERE UN MOT_CLE_FACTEUR.')
      ELSEIF ( ERREUR .EQ. 13) THEN
         CALL UTMESS('E','ERREUR SYNTAXIQUE (13)',
     +                   'ON ATTENDAIT UN ARGUMENT.')
      ELSEIF ( ERREUR .EQ. 14) THEN
         CALL UTMESS('E','ERREUR SYNTAXIQUE (14)',
     +                   'ON ATTENDAIT UN ARGUMENT OU '//
     +                   'UNE ")" DE FIN DE COMMANDE.')
      ELSEIF ( ERREUR .EQ. 15) THEN
         CALL UTMESS('E','ERREUR SYNTAXIQUE (15)',
     +                   'ON ATTENDAIT UN ARGUMENT OU'//
     +                   'UNE ")" DE FIN DE LISTE.')
      ELSEIF ( ERREUR .EQ. 16) THEN
         CALL UTMESS('E','ERREUR SYNTAXIQUE (16)',
     +                   'ON ATTENDAIT UN ELEMENT D''UNE LISTE.')
      ELSEIF ( ERREUR .EQ. 17) THEN
         CALL UTMESS('E','ERREUR SYNTAXIQUE (17)',
     +                   'ON ATTENDAIT UN ARGUMENT D''UN MOT_CLE '//
     +                   '(SOUS UN MOT_CLE_FACTEUR).')
      ELSEIF ( ERREUR .EQ. 18) THEN
         CALL UTMESS('E','ERREUR SYNTAXIQUE (18)',
     +                   'ON ATTENDAIT UN MOT_CLE '//
     +                   '(SOUS UN MOT_CLE_FACTEUR).')
      ELSEIF ( ERREUR .EQ. 19) THEN
         CALL UTMESS('E','ERREUR SYNTAXIQUE (19)',
     +                   'ON ATTENDAIT UN ";" '//
     +                   'MARQUANT LA FIN DE LA PHRASE.')
      ELSEIF ( ERREUR .EQ. 20) THEN
         CALL UTMESS('E','ERREUR SYNTAXIQUE (20)',
     +                   'ON ATTENDAIT UNE "("  D''UNE NOUVELLE '//
     +                   'OCCURENCE DU MOT_CLE_FACTEUR OU '//
     +                   'UNE ")" DE FIN DE COMMANDE OU '//
     +                   'UN MOT_CLE OU UN MOT_CLE_FACTEUR.')
      ELSE
         CALL UTMESS('E','ERREUR SYNTAXIQUE (99)',
     +                   '"'//CHAINE(1:LG)//'" CHAINE NON ATTENDUE')
      ENDIF
C
      END
