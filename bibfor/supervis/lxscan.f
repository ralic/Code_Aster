      SUBROUTINE LXSCAN(CHIN,IDEB,ICLASS,IVAL,RVAL,CVAL)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)     CHIN,                      CVAL
      INTEGER                IDEB,ICLASS,IVAL
      REAL*8                                  RVAL
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 01/12/2008   AUTEUR COURTOIS M.COURTOIS 
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
C TOLE CRP_6
C                          ANALYSEUR LEXICAL
C     ------------------------------------------------------------------
C IN  CHIN   CHAINE A DECODER
C VAR IDEB   INDICE DE DEBUT DE LA CHAINE A DECODER            (IN)
C            INDICE DE DEBUT DE LA CHAINE SUIVANTE A DECODER   (OUT)
C OUT ICLASS CLASSE DE L'ITEM TROUVE
C           -- TYPE -----    ---- INFORMATION --------------------------
C      -1   FIN D'ENREGISTREMENT (RIEN A LIRE)
C       0   ERREUR           CVAL DE TYPE CHARACTER*(*) DE LONGUEUR IVAL
C       1   ENTIER           IVAL DE TYPE INTEGER
C       2   REEL             RVAL DE TYPE REAL*8
C       3   IDENTIFICATEUR   CVAL DE TYPE CHARACTER*(*) DE LONGUEUR IVAL
C       4   TEXTE            CVAL DE TYPE CHARACTER*(*) DE LONGUEUR IVAL
C       5   SEPARATEUR       CVAL DE TYPE CHARACTER*(*) DE LONGUEUR 1
C     ------------------------------------------------------------------
C     ROUTINE(S) UTILISEE(S) :
C         LXERR
C     ROUTINE(S) FORTRAN     :
C         ICHAR
C     ------------------------------------------------------------------
C FIN LXSCAN
C     ------------------------------------------------------------------
C
C     --- VARIABLES GLOBALES -------------------------------------------
      PARAMETER ( MXCLAS = 10 , MXCHAR = 255 , MXDELI = 15 )
C
      INTEGER           CLNUM , CLLET , CLSIG , CLPNT , CLEXP , CLQUO ,
     +                  CLBLS , CLBL  , CLILL , CLEOR , NBDELI
      COMMON /LXCN01/   CLNUM , CLLET , CLSIG , CLPNT , CLEXP , CLQUO ,
     +                  CLBLS , CLBL  , CLILL , CLEOR , NBDELI
C
      CHARACTER*1       CLASS(0:MXCHAR) , CLDELI(MXDELI)
      COMMON /LXCC01/   CLASS           , CLDELI
C
      COMMON /LXFI00/ ISIGNE
C     --- VARIABLES LOCALES --------------------------------------------
      PARAMETER ( MXETAT = 13 , MXCLA1 = MXCLAS+1)
      INTEGER    NEWETA(MXCLA1,MXETAT)
C
      LOGICAL    NBENEG, EXPNEG
      CHARACTER*1  CAREXT
      INTEGER    CLASSE
      REAL*8     XNDEC,XDEC
      INTEGER    NIVAL,INIVAL
      CHARACTER*80 SERR
C
C     ------------------------------------------------------------------
C                       TABLE DES ETATS ET DES ACTIONS
C     ------------------------------------------------------------------
C  CLASSE  01  02  03  04  05  06  07  08  09  10  11
C          NBE LET  +-  .  ED   '   _      AR  EOR DELIM
C  AR EST MIS ICI POUR AROBASE (CARACTERE INTERDIT)
C  DEBUT
C  NUMERIQUES
C  CHAINE QUOTEE
C  IDENTIFIEUR
C CORRECTION BOGUE '.E+'
      DATA       NEWETA/
     1     03 ,12, 02, 13, 12, 09, 00, 01, 00, -6,-15,
     2     03 ,-7, -7, 04, -7, 00, 00, -7, 00, -7, -7,
     3     03 ,00, -1, 04, 00, 00, 00, -1, 00, -1, -1,
     4     05 ,00, -2, 00, 06, 00, 00, -2, 00, -2, -2,
     5     05 ,00, -2, 00, 06, 00, 00, -2, 00, -2, -2,
     6     08 ,00, 07, 00, 00, 00, 00, 07, 00,  0,  0,
     7     08 ,00, 00, 00, 00, 00, 00, 00, 00,  0,  0,
     8     08 ,00, -2, 00, 00, 00, 00, -2, 00, -2, -2,
     9     10 ,10, 10, 10, 10, 11, 10, 10, 10, 00, 10,
     A     10 ,10, 10, 10, 10, 11, 10, 10, 10, 00, 10,
     B     00 ,00, 00, 00, 00, 10, 00, -4, 00, -4, -4,
     C     12 ,12, -3, -3, 12, 00, 12, -3, 00, -3, -3,
     D     05 ,-5, 00, 00, -5, 00, 00, 00, 00, 00, 00/
C     ------------------------------------------------------------------
C     SORTIES DE LA TABLE : VOIR ICLASS ,
C     LES SORTIES SONT NEGATIVES ET LA DIZAINE INDIQUE QU'IL FAUT LIRE
C     UN CARACTERE D'AVANCE
C     ------------------------------------------------------------------
      DATA   MINEXP / -78     /
      DATA   MAXEXP /  75     /
      DATA   RINFIN / 1.D75   /
C     ------------------------------------------------------------------
C     FONCTIONS INTRINSEQUES
      CLASSE(CAREXT) = ICHAR(CLASS(ICHAR(CAREXT)))
      NUM   (CAREXT) = ICHAR(CAREXT)-ICHAR('0')
      LCLASS(KCLASS) = MIN(KCLASS,MXCLA1)
C     ------------------------------------------------------------------
C
C     ------------------------------------------------------------------
      IETAT  = 1
      LGMAX  = LEN(CHIN)
      IVAL   = 0
      KDEB   = IDEB - 1
      NBENEG = .FALSE.
      ISIGNE = 0
      EXPNEG = .TRUE.
C
   1  CONTINUE
         KDEB = KDEB + 1
         IF (KDEB.LE.LGMAX) THEN
            CAREXT = CHIN(KDEB:KDEB)
            KCLASS = LCLASS(CLASSE(CAREXT))
         ELSE
            KCLASS = CLEOR
         ENDIF
         IETAT = NEWETA(KCLASS,IETAT)
C
C        ------------------ EXECUTION DE L'ACTION ASSOCIEE -------------
         GOTO (10,20,30,40,50,60,70,80,90,100,110,120,130) IETAT
         ILIRE  = - IETAT / 10
         ICLASS = MOD(-IETAT,10)
CCCC     ICLASS = -IETAT + 10*ILIRE
         GOTO (1000,1001,1002,1003,1004,1005,1006,1007) ICLASS+1
         CALL LXERR('LXSCAN','ERREUR DANS L''ANALYSEUR LEXICAL')
C
C           --- ELIMINATION DES BLANCS ---
   10    CONTINUE
            IDEB = IDEB+1
         GOTO 1
C
C           --- NUMERIQUE : SIGNE ---
   20    CONTINUE
            NBENEG = CAREXT.EQ.'-'
            ISIGNE = 1
         GOTO 1
C
C           --- NUMERIQUE : PARTIE ENTIERE ---
   30    CONTINUE
C           TEST VALEURE ENTIERE < MAX_INT
            NIVAL=IVAL*10
            INIVAL=NIVAL/10
            IF (INIVAL .NE. IVAL) THEN
C              ON AVANCE JUSQU'A TROUVER UN BLANC, UN DELIMITEUR OU EOR
  777          CONTINUE
                  KDEB  = KDEB+1
                  IF (KDEB.LE.LGMAX) THEN
                     CAREXT = CHIN(KDEB:KDEB)
                     KCLASS = LCLASS(CLASSE(CAREXT))
                     IF (KCLASS.LE.MXCLAS.AND.KCLASS.NE.CLBL) GOTO 777
                  ENDIF
               IVAL = KDEB-IDEB
               CVAL = CHIN(IDEB:KDEB-1)
               WRITE (SERR,*) 'VALEUR ENTIERE TROP GRANDE   ( MAX = ',
     +              ISMAEM(),')'
               CALL LXERR(CHIN(IDEB:KDEB-1),SERR)
               GOTO 9999    
            ENDIF
            IVAL = IVAL*10 + NUM(CAREXT)
         GOTO 1
C
C           --- NUMERIQUE : POINT DECIMAL ---
   40    CONTINUE
            IEXP  = 0
            XNDEC = 1.0D0
            XDEC  = 0.D0
         GOTO 1
C
C           --- NUMERIQUE : PARTIE DECIMALE ---
   50    CONTINUE
            XNDEC = XNDEC * 10
            XDEC  = XDEC + DBLE(NUM(CAREXT)) / XNDEC
         GOTO 1
C
C           --- NUMERIQUE : EXPOSANT RENCONTRE  ---
   60    CONTINUE
            EXPNEG = .FALSE.
         GOTO 1
C
C           --- NUMERIQUE : SIGNE DE L'EXPOSANT ---
   70    CONTINUE
            EXPNEG = CAREXT.EQ.'-'
         GOTO 1
C
C           --- NUMERIQUE : MODIFICATION DE L'EXPOSANT ---
   80    CONTINUE
            IEXP = IEXP*10 + NUM(CAREXT)
         GOTO 1
C
C           --- CHAINE : QUOTE INITIAL ---
   90    CONTINUE
         GOTO 1
C
C           --- CHAINE : SAISIE DU TEXTE ---
  100    CONTINUE
            IVAL  = IVAL  + 1
            CVAL(IVAL:IVAL) = CAREXT
         GOTO 1
C
C           --- CHAINE : QUOTE FINAL OU DOUBLE QUOTE ---
  110    CONTINUE
         GOTO 1
C
C           --- IDENT : SAISIE DE L'IDENTIFICATEUR ---
  120    CONTINUE
            IVAL  = IVAL  + 1
            CVAL(IVAL:IVAL) = CAREXT
         GOTO 1
C
C           --- NUMERIQUE : POINT DECIMAL EN PREMIERE POSITION ---
  130    CONTINUE
            IEXP  = 0
            XNDEC = 1.0D0
            XDEC  = 0.D0
         GOTO 1
C     ---------------- SORTIE DE L'AUTOMATE ----------------------------
C
C
C        --- UNE ERREUR A ETE DETECTEE ---
 1000    CONTINUE
            IF (KCLASS.EQ.CLILL) CALL LXERR(CAREXT,'CARACTERE INVALIDE')
C           ON AVANCE JUSQU'A TROUVER UN BLANC, UN DELIMITEUR OU EOR
  999       CONTINUE
            KDEB  = KDEB+1
            IF (KDEB.LE.LGMAX) THEN
               CAREXT = CHIN(KDEB:KDEB)
               KCLASS = LCLASS(CLASSE(CAREXT))
               IF (KCLASS.LE.MXCLAS.AND.KCLASS.NE.CLBL) GOTO 999
            ENDIF
            IVAL = KDEB-IDEB
            CVAL = CHIN(IDEB:KDEB-1)
            IF (IVAL.GT.1)  CALL LXERR(CVAL(:IVAL),'CHAINE INVALIDE')
         GOTO 9999
C
C        --- UN ENTIER A ETE RECONNU ---
 1001    CONTINUE
            IF (NBENEG) IVAL = -IVAL
         GOTO 9999
C
C        --- UN REEL A ETE RECONNU ---
 1002    CONTINUE
            IF (EXPNEG) IEXP = -IEXP
            RVAL = DBLE(IVAL) + XDEC
            IF ( IEXP .LT. MINEXP ) THEN
               RVAL = 0
               WRITE(SERR,*)'EXPOSANT TROP PETIT ( LIMITE = ',MINEXP,')'
               CALL LXERR(CHIN(IDEB:KDEB-1),SERR)
            ELSEIF ( IEXP .GT. MAXEXP ) THEN
               RVAL = RINFIN
               WRITE(SERR,*)'EXPOSANT TROP GRAND ( LIMITE = ',MAXEXP,')'
               CALL LXERR(CHIN(IDEB:KDEB-1),SERR)
            ELSE
               RVAL = RVAL * (10.D0**IEXP)
            ENDIF
            ICLASS = 2
            IF (NBENEG) RVAL = -RVAL
C           REMISE A ZERO DE IVAL POUR EVITER CERTAINES REMANENCES
            IVAL=0
         GOTO 9999
C
C        --- UN IDENTIFICATEUR A ETE RECONNU ---
 1003    CONTINUE
         GOTO 9999
C
C        --- UN TEXTE A ETE RECONNU ---
 1004    CONTINUE
         GOTO 9999
C
C        --- UN SEPARATEUR A ETE RECONNU ---
 1005    CONTINUE
            IVAL  = 1
            CVAL  = CAREXT
            IF (ILIRE.EQ.1)  KDEB  = KDEB+1
         GOTO 9999
C
C        --- UNE FIN DE LIGNE A ETE RECONNUE ---
 1006    CONTINUE
            ICLASS = -1
         GOTO 9999
C
C        --- UN + OU UN - ISOLE A ETE TROUVE ---
 1007    CONTINUE
            IF ( NBENEG ) THEN
               CAREXT =  '-'
            ELSE
               CAREXT =  '+'
            ENDIF
            IVAL  = 1
            CVAL  = CAREXT
C           --- EN FAIT IL FAUT TRAITER AU NIVEAU SUPERIEUR ---
            ICLASS = 5
          GOTO 9999
C
C     ----- SORTIE ---------------------
 9999    CONTINUE
      IDEB = KDEB
      END
