      SUBROUTINE SMSCAN( IETAT, ICLASS, IVAL, RVAL, CVAL )
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER            IETAT, ICLASS, IVAL
      REAL*8                                  RVAL(*)
      CHARACTER*(*)                                 CVAL
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 08/03/2002   AUTEUR D6BHHJP J.P.LEFEBVRE 
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
C TOLE CRP_20
C     ANALYSEUR SYNTAXIQUE DES COMMANDES
C     ------------------------------------------------------------------
C
C        ---------------------------------------------------------------
C        ICLASS :  CODE DE CE QUE L'ON A TROUVE
C           -- TYPE -----    ---- INFORMATION --------------------------
C        -1 FIN D'ENREGISTREMENT  (RIEN A LIRE)
C         0 ERREUR           CVAL DE TYPE CHARACTER*(*) DE LONGUEUR IVAL
C         1 NOM D'OPERATEUR  CVAL DE TYPE CHARACTER*(*) DE LONGUEUR IVAL
C         2 NOM DE COMMANDE  CVAL DE TYPE CHARACTER*(*) DE LONGUEUR IVAL
C         3 CONCEPT RESULTAT CVAL DE TYPE CHARACTER*(*) DE LONGUEUR IVAL
C         4 MOT CLE FACTEUR  CVAL DE TYPE CHARACTER*(*) DE LONGUEUR IVAL
C         5 FIN MOT CLE FAC.
C         6 NOUVELLE OCCURENCE D'UN MOT CLE FACTEUR
C         7 DEBUT DE LISTE D'ARGUMENT
C         8 FIN DE LISTE D'ARGUMENT
C         9 MOT CLE          CVAL DE TYPE CHARACTER*(*) DE LONGUEUR IVAL
C        10 FIN D'OPERATEUR
C        11 FIN DE PHRASE
C        ---------------------------------------------------------------
C        21 ARGUMENT ENTIER  IVAL DE TYPE INTEGER
C        22 ARGUMENT REEL    RVAL(1) DE TYPE REAL*8
C        23 ARGUMENT CONCEPT CVAL DE TYPE CHARACTER*(*) DE LONGUEUR IVAL
C        24 ARGUMENT TEXTE   CVAL DE TYPE CHARACTER*(*) DE LONGUEUR IVAL
C        25 ARGUMENT COMPLEX RVAL(1),RVAL(2) DE TYPE REAL*8
C        26 ARGUMENT LOGIQUE IVAL 1 VRAI , IVAL = 0 FAUX
C     ------------------------------------------------------------------
C     ROUTINE(S) UTILISEE(S) :
C         SMPILE
C         SNLIRE
C     ------------------------------------------------------------------
C FIN SMSCAN
C     ------------------------------------------------------------------
C
C     --- TABLE DES ETATS/ACTIONS/ERREURS ------------------------------
      PARAMETER ( MXETAT = 49 , MXCLA1 = 13)
      CHARACTER*2 CLTYPE(MXCLA1)
      INTEGER     NEWETA(MXCLA1,MXETAT)
      INTEGER     ACTION(MXCLA1,MXETAT)
      INTEGER     ERREUR(MXCLA1,MXETAT)
C
C     --- VARIABLES LOCALES REMANENTES ---------------------------------
      LOGICAL DEPILE
      SAVE    DEPILE
C
C     ------- COMMUN DEBUG SUPERVISEUR ---------------------------------
      LOGICAL         LDBG
      INTEGER                IFV
      COMMON /CXSU00/ LDBG , IFV
C     ------------------------------------------------------------------
C     SEPARATEUR DE TYPE 'UNDEF' QUAND MEME RECONNU
      INTEGER     IPERLU
      CHARACTER*80 CTEMP
      CHARACTER*1 CPERLU,CDBLEQ
C     ------------------------------------------------------------------
      DATA        CPERLU /'&'/, CDBLEQ/'!'/
      DATA        CLTYPE /'IS', 'R8', 'CO', 'TX', 'C8', 'LS', '( ',
     +                    ') ', ', ', ': ', '= ', '; ', '??'/
C     ------------------------------------------------------------------
C                              TABLE DES ETATS
C     ------------------------------------------------------------------
C  CLASSE  01  02  03  04  05  06  07  08  09  10  11  12  13
C   0-     EN  RE  ID  TX  C8  LS   (   )   ,  :    =   ;  UNDEF
C  DEBUT D'UNE PHRASE
C  OPERATEUR OU COMMANDE OU AFFECTATION DIRECTE
C  CORPS D'APPEL
C RAJOUT POUR '&' SUR LE RESULTAT
      DATA  ((NEWETA(ICL,IET),ICL=1,13),IET=01,09)/
     1     00 ,00, 02, 00, 00, 00, 00, 00, 00, 00, 00, 01, 09,
     2     00 ,00, 00, 00, 00, 00, 05, 00, 00, 00, 03, -1, 00,
     3     41 ,41, 04, 41, 41, 41, 41, 00, 00, 00, 00, -1, 00,
     4     00 ,00, 00, 00, 00, 00, 05, 00, 00, 00, 00, 41, 00,
     5     00 ,00, 07, 00, 00, 00, 00, 40, 00, 00, 00, -1, 00,
     6     00 ,00, 07, 00, 00, 00, 00, 40, 05, 00, 00, -1, 00,
     7     00 ,00, 00, 00, 00, 00, 00, 00, 00, 11, 00, -1, 00,
     8     00 ,00, 00, 00, 00, 00, 00, 00, 00, 00, 00, -1, 00,
     9     00 ,00, 02, 00, 00, 00, 00, 00, 00, 00, 00, -1, 00/
C
C  RECHERCHE D'IDENTIFICATION DE L'IDENTIFICATEUR
C  CLASSE  01  02  03  04  05  06  07  08  09  10  11  12  13
C   1-     EN  RE  ID  TX  C8  LS   (   )   ,  :    =   ;  UNDEF
C  MOT_CLE_SIMPLE
C  MOT_CLE_FACTEUR
      DATA  ((NEWETA(ICL,IET),ICL=1,13),IET=10,19)/
     O     00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00,
     1     14 ,14, 14, 14, 14, 14, 12, 00, 00, 00, 00, -1, 00,
     2     14 ,14, 13, 14, 14, 14, 00, 15, 00, 00, 00, -1, 00,
     3     14 ,14, 14, 14, 14, 14, 00, 14, 14, 15, 00, -1, 00,
     4     21 ,21, 21, 21, 21, 21, 25, 00, 00, 00, 00, -1, 00,
     5     00 ,00, 16, 00, 00, 00, 00, 00, 00, 00, 00, -1, 00,
     6     00 ,00, 31, 00, 00, 00, 16, 17, 00, 16, 00, -1, 00,
     7     00 ,00, 07, 00, 00, 00, 32, 40, 17, 00, 00, -1, 00,
     8     00 ,00, 00, 00, 00, 00, 00, 00, 00, 00, 00, -1, 00,
     9     00 ,00, 00, 00, 00, 00, 00, 00, 00, 00, 00, -1, 00/
C
C ARGUMENTS D'UN MOT CLE
C  CLASSE  01  02  03  04  05  06  07  08  09  10  11  12  13
C   2-     EN  RE  ID  TX  C8  LS   (   )   ,  :    =   ;  UNDEF
C LISTE
      DATA  ((NEWETA(ICL,IET),ICL=1,13),IET=20,29)/
     O     21, 21, 21, 21, 21, 21, 25, 21, 21, 21, 21, 21, 21,
     1     22, 22, 22, 22, 22, 22, 25, 00, 00, 00, 00, -1, 00,
     2     22, 22, 24, 22, 22, 22, 25, 40, 23, 00, 00, -1, 00,
     3     22, 22, 24, 22, 22, 22, 25, 00, 00, 00, 00, -1, 00,
     4     21, 21, 21, 21, 21, 21, 00, 21, 22, 11, 00, -1, 00,
     5     26, 26, 28, 26, 26, 26,  0, 00, 00, 00, 00, -1, 00,
     6     26, 26, 28, 26, 26, 26,  0, 22, 27, 00, 00, -1, 00,
     7     26, 26, 28, 26, 26, 26,  0, 00, 00, 00, 00, -1, 00,
     8     26, 26, 26, 26, 26, 26, 00, 22, 26, 00, 00, -1, 00,
     9     00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
C
C ARGUMENTS D'UN MOT CLE DE MOT CLE FACTEUR
C  CLASSE  01  02  03  04  05  06  07  08  09  10  11  12  13
C   3-     EN  RE  ID  TX  C8  LS   (   )   ,  :    =   ;  UNDEF
C LISTE
      DATA  ((NEWETA(ICL,IET),ICL=1,13),IET=30,39)/
     O     00, 00, 31, 00, 00, 00, 00, 00, 00, 00, 00, -1, 00,
     1     32, 32, 32, 32, 32, 32, 35, 17, 00, 11, 00, -1, 00,
     2     32, 32, 34, 32, 32, 32, 35, 17, 33, 00, 00, -1, 00,
     3     32, 32, 34, 32, 32, 32, 35, 31, 00, 00, 00, -1, 00,
     4     31, 31, 31, 31, 31, 31, 00, 31, 31, 30, 00, -1, 00,
     5     36, 36, 38, 36, 36, 36,  0, 00, 00, 00, 00, -1, 00,
     6     36, 36, 38, 36, 36, 36,  0, 32, 37, 00, 00, -1, 00,
     7     36, 36, 38, 36, 36, 36,  0, 00, 00, 00, 00, -1, 00,
     8     36, 36, 38, 36, 36, 36, 00, 32, 37, 00, 00, -1, 00,
     9     00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, -1, 00/
C
C FIN
C  CLASSE  01  02  03  04  05  06  07  08  09  10  11  12  13
C   4-     EN  RE  ID  TX  C8  LS   (   )   ,  :    =   ;  UNDEF
C  AFFECTATION DIRECTE
      DATA  ((NEWETA(ICL,IET),ICL=1,13),IET=40,49)/
     O     00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 01, 00,
     1     42, 42, 42, 42, 42, 42, 47, 00, 00, 00, 00, -1, 00,
     2     43, 43, 43, 43, 43, 43, 00, 00, 00, 00, 00, -1, 00,
     3     00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 40, 00,
     4     45, 45, 45, 45, 45, 45, 00, 00, 00, 00, 00, -1, 00,
     5     45, 45, 45, 45, 45, 45, 00, 43, 46, 00, 00, -1, 00,
     6     45, 45, 45, 45, 45, 45, 00, 00, 00, 00, 00, -1, 00,
     7     48, 48, 48, 48, 48, 48, 00, 00, 00, 00, 00, -1, 00,
     8     44, 44, 44, 44, 44, 44, 00, 00, 00, 00, 00, -1, 00,
     9     00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, -1, 00/
C
C     ------------------------------------------------------------------
C                              TABLE DES ACTIONS
C     ------------------------------------------------------------------
C  CLASSE  01  02  03  04  05  06  07  08  09  10  11  12  13
C   0-     EN  RE  ID  TX  C8  LS   (   )   ,  :    =   ;  UNDEF
C  DEBUT D'UNE PHRASE
C  OPERATEUR OU COMMANDE OU AFFECTATION DIRECTE
C  CORPS D'APPEL
C RAJOUT POUR '&' SUR LE RESULTAT
      DATA  ((ACTION(ICL,IET),ICL=1,13),IET=01,09)/
     1     00, 00,100, 00, 00, 00, 00, 00, 00, 00, 00, 00, 01,
     2     00, 00, 00, 00, 00, 00, 02, 00, 00, 00, 03, -1, 00,
     3    124,124,100,124,124,124,124, 00, 00, 00, 00, -1, 00,
     4     00, 00, 00, 00, 00, 00, 04, 00, 00, 00, 00, 31, 00,
     5     00, 00,100, 00, 00, 00, 00, 16, 00, 00, 00, 00, 00,
     6     00, 00,100, 00, 00, 00, 00, 16, 00, 00, 00, 00, 00,
     7     00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00,
     8     00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00,
     9     00 ,00,100, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
C
C  RECHERCHE D'IDENTIFICATION DE L'IDENTIFICATEUR
C  CLASSE  01  02  03  04  05  06  07  08  09  10  11  12  13
C   1-     EN  RE  ID  TX  C8  LS   (   )   ,  :    =   ;  UNDEF
C  MOT_CLE_SIMPLE
C  MOT_CLE_FACTEUR
      DATA  ((ACTION(ICL,IET),ICL=1,13),IET=10,19)/
     O     00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00,
     1    106,106,106,106,106,106,100, 00, 00, 00, 00, 00, 00,
     2    106,106,100,106,106,106, 00,117, 00, 00, 00, 00, 00,
     3    106,106,106,106,106,106, 00,106, 06, 17, 00, 00, 00,
     4    117,117,117,117,117,117, 22, 00, 00, 00, 00, 00, 00,
     5     00 ,00, 07, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00,
     6     00 ,00, 08, 00, 00, 00, 00, 19, 00, 00, 00, 00, 00,
     7     00 ,00,100, 00, 00, 00, 23, 16, 00, 00, 00, 00, 00,
     8     00 ,00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00,
     9     00 ,00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
C
C ARGUMENTS D'UN MOT CLE
C  CLASSE  01  02  03  04  05  06  07  08  09  10  11  12  13
C   2-     EN  RE  ID  TX  C8  LS   (   )   ,  :    =   ;  UNDEF
C LISTE
      DATA  ((ACTION(ICL,IET),ICL=1,13),IET=20,29)/
     O    117,117,117,117,117,117,022,117,117,117,117,100,117,
     1     09, 10, 11, 12, 13, 14, 20, 00, 00, 00, 00, 00, 00,
     2     09, 10,100, 12, 13, 14, 20, 16, 00, 00, 00, 00, 00,
     3     09, 10,100, 12, 13, 14, 20, 00, 00, 00, 00, 00, 00,
     4    117,117,117,117,117,117, 00,117, 00, 00, 00, 00, 00,
     5     09, 10, 11, 12, 13, 14, 00, 21, 00, 00, 00, 00, 00,
     6     09, 10, 11, 12, 13, 14, 00, 21, 00, 00, 00, 00, 00,
     7     09, 10, 11, 12, 13, 14, 00, 21, 00, 00, 00, 00, 00,
     8     09, 10, 11, 12, 13, 14, 00, 21, 00, 00, 00, 00, 00,
     9     00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
C
C ARGUMENTS D'UN MOT CLE DE MOT CLE FACTEUR
C  CLASSE  01  02  03  04  05  06  07  08  09  10  11  12  13
C   3-     EN  RE  ID  TX  C8  LS   (   )   ,  :    =   ;  UNDEF
C LISTE
      DATA  ((ACTION(ICL,IET),ICL=1,13),IET=30,39)/
     O     00, 00, 08, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00,
     1     09, 10, 11, 12, 13, 14, 20, 19, 00, 00, 00, 00, 00,
     2     09, 10,100, 12, 13, 14, 00, 19, 00, 00, 00, 00, 00,
     3     09, 10,100, 12, 13, 14, 00,117, 00, 00, 00, 00, 00,
     4    117,117,117,117,117,117, 00,117, 17, 17, 00, 00, 00,
     5     09, 10, 11, 12, 13, 14, 00, 21, 00, 00, 00, 00, 00,
     6     09, 10, 11, 12, 13, 14, 00, 21, 00, 00, 00, 00, 00,
     7     09, 10, 11, 12, 13, 14, 00, 21, 00, 00, 00, 00, 00,
     8     09, 10, 11, 12, 13, 14, 00, 21, 00, 00, 00, 00, 00,
     9     00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
C
C FIN
C  CLASSE  01  02  03  04  05  06  07  08  09  10  11  12  13
C   4-     EN  RE  ID  TX  C8  LS   (   )   ,  :    =   ;  UNDEF
C  AFFECTATION DIRECTE
      DATA  ((ACTION(ICL,IET),ICL=1,13),IET=40,49)/
     O     00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 18, 00,
     1    125,125,125,125,125,125, 00, 00, 00, 00, 00, 00, 00,
     2     09, 10, 11, 12, 13, 14, 20, 00, 00, 00, 00, 00, 00,
     3     00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00,126, 00,
     4     09, 10, 11, 12, 13, 14, 00, 00, 00, 00, 00, 00, 00,
     5     09, 10, 11, 12, 13, 14, 00, 21, 00, 00, 00, 00, 00,
     6     09, 10, 11, 12, 13, 14, 00, 00, 00, 00, 00, 00, 00,
     7    125,125,125,125,125,125, 00, 00, 00, 00, 00, 00, 00,
     8    122,122,122,122,122,122, 00, 00, 00, 00, 00, 00, 00,
     9     00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
C     ------------------------------------------------------------------
C
C     ------------------------------------------------------------------
C                              TABLE DES ERREURS
C     REMARQUE : -1 INDIQUE UN ETAT NORMAL
C     ------------------------------------------------------------------
C  CLASSE  01  02  03  04  05  06  07  08  09  10  11  12  13
C   0-     EN  RE  ID  TX  C8  LS   (   )   ,  :    =   ;  UNDEF
C  DEBUT D'UNE PHRASE
C  OPERATEUR OU COMMANDE OU AFFECTATION DIRECTE
C  CORPS D'APPEL
C RAJOUT POUR '&' SUR LE RESULTAT
      DATA  ((ERREUR(ICL,IET),ICL=1,13),IET=01,09)/
     1     01, 01, -1, 01, 01, 01, 01, 01, 01, 01, 01, -1, -1,
     2     02, 02, 02, 02, 02, 02, -1, 02, 02, 02, -1, 02, 02,
     3     00, 00, -1, 00, 00, 00, 00, 03, 03, 03, 03, 03, 03,
     4     04, 04, 04, 04, 04, 04, -1, 04, 04, 04, 04, 04, 04,
     5     05, 05, -1, 05, 05, 05, 05, -1, 05, 05, 05, 05, 05,
     6     05, 05, -1, 05, 05, 05, 05, -1, -1, 05, 05, 05, 05,
     7     06, 06, 06, 06, 06, 06, 06, 06, 06, -1, 06, 06, 06,
     8     00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00,
     9     07 ,07, -1, 07, 07, 07, 07, 07, 07, 07, 07, 07, 07/
C
C  RECHERCHE D'IDENTIFICATION DE L'IDENTIFICATEUR
C  CLASSE  01  02  03  04  05  06  07  08  09  10  11  12  13
C   1-     EN  RE  ID  TX  C8  LS   (   )   ,  :    =   ;  UNDEF
C  MOT_CLE_SIMPLE
C  MOT_CLE_FACTEUR
      DATA  ((ERREUR(ICL,IET),ICL=1,13),IET=10,19)/
     O     00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00,
     1     -1, -1, -1, -1, -1, -1, -1, 08, 08, 08, 08, 08, 08,
     2     -1, -1, -1, -1, -1, -1, 09, 00, 09, 09, 09, 09, 09,
     3     -1, -1, -1, -1, -1, -1, 10, -1, -1, -1, 10, 10, 10,
     4     -1, -1, -1, -1, -1, -1, -1, 11, 11, 11, 11, 11, 11,
     5     12, 12, -1, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12,
     6     12, 12, -1, 12, 12, 12, -1,  0, 12, -1, 12, 12, 12,
     7     20 ,20, -1, 20, 20, 20, -1, -1, -1, 20, 20, 20, 20,
     8     00 ,00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 20, 00,
     9     00 ,00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 20, 00/
C
C ARGUMENTS D'UN MOT CLE
C  CLASSE  01  02  03  04  05  06  07  08  09  10  11  12  13
C   2-     EN  RE  ID  TX  C8  LS   (   )   ,  :    =   ;  UNDEF
C LISTE
      DATA  ((ERREUR(ICL,IET),ICL=1,13),IET=20,29)/
     O     -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 13, -1,
     1     -1, -1, -1, -1, -1, -1, -1, 13, 13, 13, 13, 13, 13,
     2     -1, -1, -1, -1, -1, -1, -1, -1, -1, 14, 14, 14, 14,
     3     -1, -1, -1, -1, -1, -1, -1, 13, 13, 13, 13, 13, 13,
     4     -1, -1, -1, -1, -1, -1, 15, -1, 15, 15, 15, 15, 15,
     5     -1, -1, -1, -1, -1, -1, 16, 16, 16, 16, 16, 16, 16,
     6     -1, -1, -1, -1, -1, -1, 16, -1, -1, 16, 16, 16, 16,
     7     -1, -1, -1, -1, -1, -1, 16, 16, 16, 16, 16, 16, 16,
     8     -1, -1, -1, -1, -1, -1, 16, -1, -1, 16, 16, 16, 16,
     9     00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 16, 00/
C
C ARGUMENTS D'UN MOT CLE DE MOT CLE FACTEUR
C  CLASSE  01  02  03  04  05  06  07  08  09  10  11  12  13
C   3-     EN  RE  ID  TX  C8  LS   (   )   ,  :    =   ;  UNDEF
C LISTE
      DATA  ((ERREUR(ICL,IET),ICL=1,13),IET=30,39)/
     O     18, 18, -1, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18,
     1     -1, -1, -1, -1, -1, -1, -1, -1, 17, -1, 17, 17, 17,
     2     -1, -1, -1, -1, -1, -1, 17, -1, 17, 17, 17, 17, 17,
     3     -1, -1, -1, -1, -1, -1, 17, -1, 17, 17, 17, 17, 17,
     4     -1, -1, -1, -1, -1, -1, 17, -1, -1, -1, 17, 17, 17,
     5     -1, -1, -1, -1, -1, -1, 16, -1, 16, 16, 16, 16, 16,
     6     -1, -1, -1, -1, -1, -1, 16, -1, 16, 16, 16, 16, 16,
     7     -1, -1, -1, -1, -1, -1, 16, -1, 16, 16, 16, 16, 16,
     8     -1, -1, -1, -1, -1, -1, 16, -1, 16, 16, 16, 16, 16,
     9     00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00/
C
C FIN
C  CLASSE  01  02  03  04  05  06  07  08  09  10  11  12  13
C   4-     EN  RE  ID  TX  C8  LS   (   )   ,  :    =   ;  UNDEF
C  AFFECTATION DIRECTE
      DATA  ((ERREUR(ICL,IET),ICL=1,13),IET=40,49)/
     O     19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, -1, 19,
     1     -1, -1, -1, -1, -1, -1, 00, 00, 00, 00, 00, 17, 00,
     2     -1, -1, -1, -1, -1, -1, -1, 00, 00, 00, 00, 17, 00,
     3     00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 17, 00,
     4     00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 17, 00,
     5     00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 17, 00,
     6     00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 17, 00,
     7     -1, -1, -1, -1, -1, -1, 00, 00, 00, 00, 00, 17, 00,
     8     -1, -1 ,-1, -1, -1, -1, 00, 00, 00, 00, 00, 17, 00,
     9     00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 17, 00/
C     ------------------------------------------------------------------
C
      IPERLU =  0
      KCLASS = -1
      IF ( IETAT .EQ. 1 ) THEN
         DEPILE = .FALSE.
         CALL SMPILE('INITPI',ICLASS,IVAL,RVAL,CVAL,DEPILE)
      ENDIF
C
C
   1  CONTINUE
         IF ( DEPILE ) THEN
            CALL SMPILE('DEPILE',ICLASS,IVAL,RVAL,CVAL,DEPILE)
         ELSE
            CALL SNLIRE(ICLASS,IVAL,RVAL,CVAL)
         ENDIF
         IF (LDBG) CALL SNDBG(IFV,ICLASS,IVAL,RVAL,CVAL)
C        ---------------------------------------------------------------
C        ICLASS   CLASSE  DE CE QUE L'ON A TROUVE
C         -- TYPE --------- ----- INFORMATION --------------------------
C         -1 FIN DE FICHIER
C          0 ERREUR         CVAL DE TYPE CHARACTER*(*) DE LONGUEUR IVAL
C          1 ENTIER         IVAL DE TYPE INTEGER
C          2 REEL           RVAL(1) DE TYPE REAL*8
C          3 IDENTIFICATEUR CVAL DE TYPE CHARACTER*(*) DE LONGUEUR IVAL
C          4 TEXTE          CVAL DE TYPE CHARACTER*(*) DE LONGUEUR IVAL
C          5 COMPLEXE       RVAL(1),RVAL(2) DE TYPE REAL*8
C          6 BOOLEEN        IVAL 1 VRAI , IVAL = 0 FAUX
C
C            SEPARATEUR     CVAL DE TYPE CHARACTER*(*) DE LONGUEUR 1
C          7 '('
C          8 ')'
C          9 ','
C         10 ':'
C         11 '='
C         12 ';'
C         13 SEPARATEUR INDEFINI
C        ---------------------------------------------------------------
         IF (ICLASS.EQ.00) GOTO 998
         IF (ICLASS.EQ.-1) GOTO 999
C
         IACT = ACTION(ICLASS,IETAT)
         IF (LDBG) CALL SMDBGS(IFV,IETAT,ICLASS,IACT,IVAL,RVAL,CVAL)
C
         IPILE = IACT / 100
         IF ( IPILE .EQ. 1 ) THEN
C            ACTION SPECIALE :  ON EMPILE (POUR PLUS TARD)
             CALL SMPILE('EMPILE',ICLASS,IVAL,RVAL,CVAL,DEPILE)
          ENDIF
          IACT = IACT - 100*IPILE
C
          IF ( IACT .EQ. 0 ) THEN
C            ACTION NO  00  :  ON NE FAIT RIEN
C
          ELSEIF ( IACT .EQ. 01 ) THEN
C            ACTION NO  01  :  ON VIENT DE TROUVER UN SEPARATEUR 'UNDEF'
             IF ( CVAL(1:1) .EQ. CPERLU ) THEN
                IPERLU = 1
             ELSEIF ( CVAL(1:1) .EQ. CDBLEQ ) THEN
                IPERLU = 2
             ELSE
C               KCLASS = 00
                IETAT  = 00
             ENDIF
C
          ELSEIF ( IACT .EQ. 02 ) THEN
C            ACTION NO  02  :  ON AVAIT TROUVE LE NOM D'UNE COMMANDE
             CALL SMPILE('DEPILE',IBID,IVAL,RVAL,CVAL,DEPILE)
             KCLASS = 02
             IF ( IPERLU.EQ.1 ) THEN
C               KCLASS = 00
                IETAT  = 00
                IPERLU = 0
                CTEMP  = CVAL(1:IVAL)
                CVAL(1:1)    = CPERLU
                IVAL         = IVAL + 1
                CVAL(2:IVAL) = CTEMP
             ELSEIF ( IPERLU.EQ.2 ) THEN
                CTEMP        = CVAL(1:IVAL)
                CVAL(1:1)    = CDBLEQ
                IVAL         = IVAL + 1
                CVAL(2:IVAL) = CTEMP
             ENDIF
             IF (LDBG) CALL SMDBGS(IFV,IETAT,ICLASS,IACT,IVAL,RVAL,CVAL)
C
          ELSEIF ( IACT .EQ. 3 ) THEN
C            ACTION NO  03  :  ON AVAIT TROUVE LE NOM DU RESULTAT
             CALL SMPILE('DEPILE',IBID,IVAL,RVAL,CVAL,DEPILE)
             KCLASS = 03
             IF ( IPERLU .EQ. 1 ) THEN
                IPERLU = 0
                CTEMP  = CVAL(1:IVAL)
                CVAL(1:1)    = CPERLU
                IVAL         = IVAL + 1
                CVAL(2:IVAL) = CTEMP
             ENDIF
             IF (LDBG) CALL SMDBGS(IFV,IETAT,ICLASS,IACT,IVAL,RVAL,CVAL)
C
          ELSEIF ( IACT .EQ. 4 ) THEN
C            ACTION NO  04  :  ON A  TROUVE LE NOM DE L'OPERATEUR
             CALL SMPILE('DEPILE',IBID,IVAL,RVAL,CVAL,DEPILE)
             KCLASS = 01
C
          ELSEIF ( IACT .EQ. 5 ) THEN
C            ON A UN IDENTIFICATEUR : MOT_CLE OU UN MOT_CLE_FACTEUR?
C
          ELSEIF ( IACT .EQ. 6 ) THEN
C            --- ON AVAIT UN MOT_CLE ---
             CALL SMPILE('DEPILE',IBID,IVAL,RVAL,CVAL,DEPILE)
             KCLASS = 09
             IF (LDBG) CALL SMDBGS(IFV,IETAT,ICLASS,IACT,IVAL,RVAL,CVAL)
C
          ELSEIF ( IACT .EQ. 7 ) THEN
C            --- ON A UN MOT_CLE FACTEUR ---
             KCLASS = 04
C
          ELSEIF ( IACT .EQ. 8 ) THEN
C            --- MOT CLE D"UN MOT CLE FACTEUR ---
             KCLASS = 09
C
          ELSEIF ( IACT .EQ. 9 ) THEN
C            --- ARGUMENT A VALEUR_ENTIERE ---
             KCLASS = 21
C
          ELSEIF ( IACT .EQ. 10 ) THEN
C            --- ARGUMENT A VALEUR_REELLE ---
             KCLASS = 22
C
          ELSEIF ( IACT .EQ. 11) THEN
C            --- ARGUMENT A VALEUR DE CONCEPT ---
             KCLASS = 23
C
          ELSEIF ( IACT .EQ. 12) THEN
C            --- ARGUMENT A VALEUR TEXTE ---
             KCLASS = 24
C
          ELSEIF ( IACT .EQ. 13) THEN
C            --- ARGUMENT A VALEUR COMPLEXE ---
             KCLASS = 25
C
          ELSEIF ( IACT .EQ. 14) THEN
C            --- ARGUMENT A VALEUR LOGIQUE ---
             KCLASS = 26
C
          ELSEIF ( IACT .EQ. 15) THEN
C
          ELSEIF ( IACT .EQ. 16) THEN
C            --- FIN DE L'OPERATEUR ---
             KCLASS = 10
C
          ELSEIF ( IACT .EQ. 17) THEN
C            --- IL FAUT DEPILER ---
             DEPILE = .TRUE.
C
          ELSEIF ( IACT .EQ. 18) THEN
C            --- FIN DE PHRASE ---
             KCLASS = 11
C
          ELSEIF ( IACT .EQ. 19) THEN
C            --- FIN DE MOT CLE FACTEUR ---
             KCLASS = 05
C
          ELSEIF ( IACT .EQ. 20) THEN
C            --- DEBUT DE LISTE DE VALEUR ---
             KCLASS = 07
C
          ELSEIF ( IACT .EQ. 21) THEN
C            --- FIN DE LISTE DE VALEUR ---
             KCLASS = 08
C
          ELSEIF ( IACT .EQ. 22) THEN
C            --- DEBUT LISTE DE VALEUR, IL FAUT DEPILER ---
             DEPILE = .TRUE.
             KCLASS = 07
C
          ELSEIF ( IACT .EQ. 23) THEN
C            --- NOUVELLE OCCURENCE DU MOT_CLE_FACTEUR ---
             KCLASS = 06
C
          ELSEIF ( IACT .EQ. 24) THEN
C            --- AFFECTATION: DECLARATION DE L'OPERATEUR ---
             KCLASS = 01
             IVAL   = 11
             CVAL   = 'DEFI_VALEUR'
             DEPILE = .TRUE.
C
          ELSEIF ( IACT .EQ. 25) THEN
C            --- AFFECTATION: DECLARATION DU MOT CLE ---
             KCLASS = 09
             IF ( ICLASS .EQ. 3 ) THEN
                CALL GCUTYB(CVAL(1:IVAL),CVAL)
             ELSE
                CVAL   = CLTYPE(ICLASS)
             ENDIF
             IVAL   = 02
             DEPILE = .TRUE.
C
          ELSEIF ( IACT .EQ. 26) THEN
C            --- FIN D'AFFECTATION   ---
             KCLASS = 10
             DEPILE = .TRUE.
C
          ELSEIF ( IACT .EQ. 27) THEN
C            --- AFFECTATION D'UN COMPLEXE ---
             KCLASS = 34
C
          ELSEIF ( IACT .EQ. 28) THEN
C            --- AFFECTATION D'UN LOGIQUE ---
             KCLASS = 35
C
          ELSEIF ( IACT .EQ. 29) THEN
C            --- AFFECTATION D'UNE LISTE ---
             KCLASS = 36
C
          ELSEIF ( IACT .EQ. 30) THEN
C            --- FIN D'AFFECTATION D'UNE LISTE ---
             KCLASS = 37
C
          ELSEIF ( IACT .EQ. 31) THEN
C            --- AFFECTATION: DECLARATION DE L'OPERATEUR SUR CONCEPT ---
             CALL SMPILE('DEPILE',IBID,IVAL,RVAL,CVAL,DEPILE)
             CALL SMPILE('EMPILE',IBID,IVAL,RVAL,CVAL,DEPILE)
             CALL SMPILE('EMPILE',IBID,IVAL,RVAL,CVAL,DEPILE)
             IBID = 12
             IVAL = 1
             RVAL1 = 0.D0
             CVAL = ';'
             CALL SMPILE('EMPILE',IBID,IVAL,RVAL1,CVAL,DEPILE)
             IBID = 12
             IVAL = 1
             RVAL1 = 0.D0
             CVAL = ';'
             CALL SMPILE('EMPILE',IBID,IVAL,RVAL1,CVAL,DEPILE)
             KCLASS = 01
             IVAL   = 11
             CVAL   = 'DEFI_VALEUR'
             DEPILE = .TRUE.
C
          ELSE
C            --- ERREUR  DE PROGRAMMATION DES AUTOMATES ---
          ENDIF
C
          IETAT0 = IETAT
          IETAT  = NEWETA(ICLASS,IETAT)
          IF ( IETAT .LT.  1  ) THEN
             KCLASS = 00
          ENDIF
C
C     ------ TEST D'ARRET -----
      IF ( KCLASS .LT. 0 ) THEN
C        --- ON N'A PAS FINI ---
         GOTO 1
      ELSEIF ( KCLASS .EQ. 0 )  THEN
C        --- ON A FINI POUR CAUSE D'ERREUR ---
         CALL SNERR(ERREUR(ICLASS,IETAT0),ICLASS,IVAL,RVAL,CVAL)
C        --- PAS DE RATTRAPAGE D'ERREUR ---
         ICLASS = 00
      ELSE
C        --- ON A FINI PAR RECONNAITRE QUELQUE CHOSE ---
         ICLASS = KCLASS
      ENDIF
  998 CONTINUE
      GOTO 9999
C
C     ---  EOF ---
  999 CONTINUE
      ICLASS = -1
 9999 CONTINUE
      END
