      SUBROUTINE FIPREP(TYPE,NOMFIN,BASE,RESUSR,IER)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)     TYPE,NOMFIN,     RESUSR
      CHARACTER*1                   BASE
      INTEGER                                   IER
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 13/02/2001   AUTEUR DURAND C.DURAND 
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
C     PREPARATION A LA MISE SOUS FORME POLONAISE INVERSE ET CONTROLE DES
C     EXPRESSIONS.
C     ------------------------------------------------------------------
C IN  TYPE IS TYPE DE LA LECTURE
C       TYPE = 'FONCTION_X'    F(X,Y) = ......
C                       'I' = ENTIER,
C                       'R' = REEL
C                       'C' = COMPLEXE
C                       ' ' = CONVENTION FORTRAN
C            = 'EVAL'        EVAL(....)
C VAR NOMFIN : K19 : STRUCTURE DE DONNEES FONCTION_INTERPRETEE
C     ------------------------------------------------------------------
C
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      COMMON /FISY01/ IPARG,IPARD,IVIRG,IEGAL,IPTVI,ILOAD,IPLUS,IFONC
      COMMON /FICL01/ ICSTE , IOPE1, IOPE2, LOPE1, LOPE2
      COMMON /LXFI00/ ISIGNE
C
C     ------- COMMUN DEBUG SUPERVISEUR ---------------------------------
      LOGICAL         LDBG
      INTEGER                IFV
      COMMON /CXSU00/ LDBG , IFV
C     ------------------------------------------------------------------
C     COMMON SPECIFIQUE A L'INCLUDE_MATERIAU POUR CONSERVER LE NOM
C     DE MATERIAU DEVANT PREFIXER LES CONCEPTS INCLUS DANS LE FICHIER
      CHARACTER*8 PRFXCO
      INTEGER     LPRFXC
      COMMON /INCMAT/ PRFXCO  
      COMMON /INCMAI/ LPRFXC  
C     ------------------------------------------------------------------
C     ------------------------------------------------------------------
      INTEGER           ICLASS,IVAL
      REAL*8                        RVAL(2),RVAL2(2)
      COMPLEX*16                    CVAL
      CHARACTER*8       NOM,CDIR
      CHARACTER*16      CTYPE
      CHARACTER*19      RESUFI
      CHARACTER*72                       KVAL,KVAL2
C     --- VARIABLES LOCALES --------------------------------------------
      PARAMETER ( MXETAT =  9 , MXCLAS = 17 )
      INTEGER    NEWETA(MXCLAS,MXETAT)
      INTEGER    ACTION(MXCLAS,MXETAT)
      PARAMETER        (MXITER=50, MXARGU=10)
C     ------------------------------------------------------------------
C     TABLE DES ETATS
C     ------------------------------------------------------------------
C  CLASSE  01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17
C          EN RE ID TX CX LO (  )  ,  :  =  ;  *  /  +  -  UNDEF
C  DEBUT
C  PARTIE DEFINITION DE LA PARTIE GAUCHE DE L'EGALITE
C  ON ATTEND UN SIGNE EGAL
C  PARTIE DEFINITION DE LA PARTIE DROITE DE L'EGALITE
C  CAS D'UNE FONCTION OU D'UN TABLEAU
      DATA       NEWETA/
     1     00,00,02,00,00,00,01,00,00,00,00,01,00,00,00,00,00,
     2     00,00,00,00,00,00,03,00,00,00,06,00,00,00,00,00,00,
     3     04,00,04,00,00,00,00,00,00,00,00,00,00,00,00,00,00,
     4     00,00,00,00,00,00,00,05,03,00,00,00,00,00,00,00,00,
     5     00,00,00,00,00,00,00,00,00,00,06,00,00,00,00,00,00,
     6     07,07,09,00,07,07,06,00,06,00,00,00,00,00,06,06,00,
     7     07,07,00,07,07,00,00,07,06,00,00,01,08,06,06,06,00,
     8     07,07,09,00,07,07,06,07,00,00,00,00,06,00,00,00,00,
     9     07,07,00,00,07,07,06,07,06,00,00,00,08,06,06,06,00/
C     ------------------------------------------------------------------
C     TABLE DES ACTIONS
C     ------------------------------------------------------------------
C  CLASSE  01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17
C          EN RE ID TX CX LO (  )  ,  :  =  ;  *  /  +  -  UNDEF
C  DEBUT
C  PARTIE DEFINITION DE LA PARTIE GAUCHE DE L'EGALITE
C  ON ATTEND UN SIGNE EGAL
C  PARTIE DEFINITION DE LA PARTIE DROITE DE L'EGALITE
C  CAS D'UNE FONCTION OU D'UN TABLEAU
      DATA       ACTION/
     1     00,00,02,00,00,00,00,00,00,00,06,00,00,00,00,00,00,
     2     00,00,00,00,00,00,03,00,00,00,05,00,00,00,00,00,00,
     3     04,00,04,00,00,00,00,00,00,00,00,00,00,00,00,00,00,
     4     00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,
     5     00,00,00,00,00,00,00,00,00,00,09,00,00,00,00,00,00,
     6     06,06,06,00,06,06,06,00,06,00,00,01,00,00,07,07,00,
     7     11,11,00,11,11,00,00,06,06,00,00,01,00,06,06,06,00,
     8     -6,-6,-6,00,-6,-6,-6,-6,00,00,00,01,08,00,00,00,00,
     9     11,11,00,00,11,11,10,06,06,00,00,00,00,06,06,06,00/
C     ------------------------------------------------------------------
C     REMARQUE POUR LA DEFINITION DE LA PARTIE DROITE DE L'EGALITE:
C            ?? POUR TOUT IDENTIFICATEUR ON VERIFIE SON EXISTENCE ET
C            ?? SI C'EST UN NOM DE FONCTION SA CLASSE DEVIENT "4".
C     ------------------------------------------------------------------
      CALL JEMARQ()
      IETAT  = 1
      IER    = 0
      KER    = 0
      ITERM  = 0
      IVARS  = 0
      MXTERM = MXITER
      MXVARS = MXARGU
      RESUFI = NOMFIN
      IF ( TYPE.EQ. 'EVAL') THEN
         IETAT = 6
         CALL JEEXIN(RESUFI//'.INFX',IRET)
         IF (IRET.NE.0) THEN
            CALL JEDETR(RESUFI//'.POLO')
            CALL JEDETR(RESUFI//'.INFX')
            CALL JEDETR(RESUFI//'.NOVA')
            CALL JEDETR(RESUFI//'.ADVA')
         END IF
         CALL WKVECT(RESUFI//'.INFX',BASE//' V I',MXTERM,LEXPR)
         ITYPFO = 0
         CDIR   = TYPE
      ELSEIF (TYPE.EQ.'FONCTION_R') THEN
         ITYPFO = 32
         CDIR   = TYPE
      ELSEIF (TYPE.EQ.'FONCTION_I') THEN
         ITYPFO = 31
         CDIR   = TYPE
      ELSEIF (TYPE.EQ.'FONCTION_C') THEN
         ITYPFO = 35
         CDIR   = TYPE
      ELSEIF (TYPE.EQ.'FONCTION') THEN
         ITYPFO = 0
         CDIR   = TYPE
      ENDIF
C
      NIVEAU = 0
   1  CONTINUE
        CALL FILIRE(ICLASS,IVAL,RVAL,KVAL)
        IF (LDBG)  CALL FIDBG (IFV,ICLASS,IVAL,RVAL,KVAL)
C       ----------------------------------------------------------------
C       ICLASS  CLASSE DE CE QUE L'ON A TROUVE
C       ----------------------------------------------------------------
C           -- TYPE -----    ---- INFORMATION --------------------------
C       -1   FIN DE FICHIER
C        0   ERREUR          KVAL DE TYPE CHARACTER*(*) DE LONGUEUR IVAL
C        1   ENTIER          IVAL DE TYPE INTEGER
C        2   REEL            RVAL(1) DE TYPE REAL*8
C        3   IDENTIFICATEUR  KVAL DE TYPE CHARACTER*(*) DE LONGUEUR IVAL
C        4   TEXTE           KVAL DE TYPE CHARACTER*(*) DE LONGUEUR IVAL
C        5   COMPLEXE        RVAL(1),RVAL(2) DE TYPE REAL*8
C        6   BOOLEEN         IVAL 1 VRAI , IVAL = 0 FAUX
C
C            SEPARATEUR      KVAL DE TYPE CHARACTER*(*) DE LONGUEUR 1
C        7 : '('             8 : ')'             9 : ','
C       10 : ':'            11 : '='            12 : ';'
C       13 : '*'            14 : '/'            15 : '+'
C       16 : '-'            17 : SEPARATEUR INDEFINI
C       ------------------------------------------------------------
        IF (ICLASS .EQ. 7) THEN
           NIVEAU = NIVEAU + 1
        ELSEIF (ICLASS .EQ. 8) THEN
           NIVEAU = NIVEAU - 1
           IF ( NIVEAU .EQ. 0 ) THEN
              IF (CDIR .EQ. 'EVAL') THEN
                 IF ( ITERM+2 .GT. MXTERM ) THEN
                    MXTERM = MXTERM + MXITER
                    CALL JUVECA(RESUFI//'.INFX',MXTERM)
                    CALL JEVEUO(RESUFI//'.INFX','E',LEXPR)
                 ENDIF
                 ITERM = ITERM + 1
                 ZI(LEXPR-1+ITERM) = IPARD
                 ITERM = ITERM + 1
                 ZI(LEXPR-1+ITERM) = IPTVI
                 CALL JEECRA(RESUFI//'.INFX','LONUTI',ITERM,' ')
                 CALL WKVECT(RESUFI//'.POLO',BASE//' V I',MXTERM,LPOLO)
                 DO 10  JTERM=1,ITERM
                    ZI(LPOLO+JTERM-1) = ZI(LEXPR+JTERM-1)
  10             CONTINUE
                 IF (LDBG) CALL FIIMPR(IFV,ITERM,ZI(LPOLO),'AV FIPOLO')
                 IF (IER .EQ. 0 ) THEN
                    CALL FIPOLO(ITERM,ZI(LPOLO),IER)
                    CALL JEECRA(RESUFI//'.POLO','LONUTI',ITERM,' ')
                 ENDIF
                 GOTO 999
              ELSEIF (CDIR .EQ. 'FONCTION') THEN
                 ITERM = ITERM + 1
                 IF ( ITERM .GT. MXTERM ) THEN
                    MXTERM = MXTERM + MXITER
                    CALL JUVECA(RESUFI//'.INFX',MXTERM)
                    CALL JEVEUO(RESUFI//'.INFX','E',LEXPR)
                 ENDIF
                 ZI(LEXPR-1+ITERM) = IPTVI
                 CALL JEECRA(RESUFI//'.INFX','LONUTI',ITERM,' ')
                 CALL WKVECT(RESUFI//'.POLO',BASE//' V I',MXTERM,LPOLO)
                 DO 20  JTERM=1,ITERM
                     ZI(LPOLO+JTERM-1) = ZI(LEXPR+JTERM-1)
  20             CONTINUE
                 IF (IER .EQ. 0 ) THEN
                    CALL FIPOLO(ITERM,ZI(LPOLO),IER)
                    CALL JEECRA(RESUFI//'.POLO','LONUTI',ITERM,' ')
                 ENDIF
                 GOTO 999
              ENDIF
           ELSEIF ( NIVEAU .LT. 0 ) THEN
              IER = IER + 1
              CALL UTMESS('E','SUPERVISEUR.(ERREUR.FIPREP.01)',
     +                        'ERREUR DE PARENTHESAGE')
              GOTO 9999
           ENDIF
        ELSEIF (ICLASS .EQ.-1) THEN
           GOTO 998
        ELSEIF (ICLASS .EQ.0 ) THEN
           ICLASS=3
        ENDIF

C       ------------------- EXECUTION DE L'ACTION ASSOCIEE -------------
        IF ( IETAT .EQ. 0 ) THEN
            IER = IER + 1
            CALL UTMESS('E','SUPERVISEUR.(ERREUR.FIPREP.02)',
     +                      'ERREUR GRAVE DE SYNTAXE.')
            GOTO 9999
        ENDIF
C
        IACT  = ACTION(ICLASS,IETAT)
         IF ( IACT .EQ.  1 ) THEN
C           --- FIN DE PHRASE: ON LA METS EN POLONAIS POSTFIXE ---
            CALL UTMESS('E','SUPERVISEUR.(ERREUR.FIPREP.03)',
     +                      'POINT VIRGULE (";") INTERDIT.')
            ITERM = ITERM + 1
            IF ( ITERM .GT. MXTERM ) THEN
               MXTERM = MXTERM + MXITER
               CALL JUVECA(RESUFI//'.INFX',MXTERM)
               CALL JEVEUO(RESUFI//'.INFX','E',LEXPR)
            ENDIF
            ZI(LEXPR-1+ITERM) = IPTVI
            CALL JEECRA(RESUFI//'.INFX','LONUTI',ITERM,' ')
            CALL WKVECT(RESUFI//'.POLO',BASE//' V I',MXTERM,LPOLO)
            DO 101 JTERM=1,ITERM
                ZI(LPOLO+JTERM-1) = ZI(LEXPR+JTERM-1)
  101       CONTINUE
            IF (IER .EQ. 0 ) THEN
               CALL FIPOLO(ITERM,ZI(LPOLO),IER)
               CALL JEECRA(RESUFI//'.POLO','LONUTI',ITERM,' ')
            ENDIF
            GOTO 999
C
         ELSEIF ( IACT .EQ.   2 ) THEN
C           --- DEFINITION:  NOM DE LA VARIABLE OU DE LA FONCTION ? --
            NOM = KVAL(1:IVAL)
            IF (PRFXCO .NE. '?') THEN            
C              ---- CAS INCLUDE_MATERIAU 
               IF(KVAL(1:LPRFXC) .NE. PRFXCO(1:LPRFXC))THEN
                 NOM=PRFXCO(1:LPRFXC)//KVAL(1:IVAL)
                 IVAL=IVAL+LPRFXC
               ENDIF
            ENDIF
C
         ELSEIF ( IACT .EQ.   3 ) THEN
C           --- DEFINITION:  NOM D'UNE FONCTION ---
            RESUFI(1:8) = NOM
            NOMFIN(1:8) = NOM
            RESUSR      = NOM
            CALL WKVECT(RESUFI//'.INFX',BASE//' V I',MXTERM,LEXPR)
            CALL WKVECT(RESUFI//'.NOVA',BASE//' V K8',MXVARS,LNOVA)
            CALL WKVECT(RESUFI//'.ADVA',BASE//' V I ',MXVARS,LADVA)
            IVARS = 0
C
            IF (ITYPFO.EQ.0) THEN
               KCLASS = 32
               IF ( INDEX('IJKLMN',KVAL(1:1)) .NE. 0 ) KCLASS = 31
            ELSE
               KCLASS = ITYPFO
            ENDIF
            IVAL = 0
            RVAL(1) = 0.D0
            RVAL(2) = 0.D0
            CALL FIREMP(0,KCLASS,IVAL,RVAL,NOM,JER )
            IF ( JER .LT. 1 ) THEN
               IER = IER + 1
               CALL UTMESS('E','SUPERVISEUR.(ERREUR.FIPREP.04)',
     +                         ' "'//NOM//'" FONCTION DEJA DEFINIE')
            ENDIF
C
         ELSEIF ( IACT .EQ.   4 ) THEN
C           --- DEFINITION DES PARAMETRES FORMELS ---
            IER1 = 0
            IF ( KVAL(1:IVAL) .EQ. 'REEL' ) THEN
               KCLASS = 12
               CALL FILIRE(ICLAS2,IVAL2,RVAL,KVAL2)
               IF (ICLAS2.NE.10) IER1 = 1
               CALL FILIRE(ICLASS,IVAL,RVAL,KVAL)
            ELSEIF ( KVAL(1:IVAL) .EQ. 'ENTIER' ) THEN
               KCLASS = 11
               CALL FILIRE(ICLAS2,IVAL2,RVAL,KVAL2)
               IF (ICLAS2.NE.10) IER1 = 1
               CALL FILIRE(ICLASS,IVAL,RVAL,KVAL)
            ELSEIF ( KVAL(1:IVAL) .EQ. 'COMPLEXE' ) THEN
               KCLASS = 15
               CALL FILIRE(ICLAS2,IVAL2,RVAL,KVAL2)
               IF (ICLAS2.NE.10) IER1 = 1
               CALL FILIRE(ICLASS,IVAL,RVAL,KVAL)
            ELSE
               KCLASS = 12
               IF ( INDEX('IJKLMN',KVAL(1:1)) .NE. 0 ) KCLASS = 11
            ENDIF
            IF ( ITERM+2 .GT. MXTERM ) THEN
               MXTERM = MXTERM + MXITER
               CALL JUVECA(RESUFI//'.INFX',MXTERM)
               CALL JEVEUO(RESUFI//'.INFX','E',LEXPR)
            ENDIF
            IF (IER1.NE.0) THEN
               IER = IER + 1
               CALL SNERR(06,ICLASS,IVAL,RVAL,KVAL)
            ENDIF
            IVARS = IVARS + 1
            IF ( IVARS .GT. MXVARS ) THEN
               MXVARS = MXVARS + MXARGU
               CALL JUVECA(RESUFI//'.NOVA',MXTERM)
               CALL JEVEUO(RESUFI//'.NOVA','E',LNOVA)
            ENDIF
C           --- VERIFICATION DE L'INDICE MUET ---
            NOM    = KVAL(1:IVAL)
            KCLAS2= 0
            ZK8(LNOVA+IVARS-1) = NOM(1:IVAL)
            CALL FIREMP(1,KCLAS2,IVAL,RVAL,NOM,JER )
            IF ( JER .LT. 1 ) THEN
               CALL FICNCP(NOM,JER,KER)
               IF ( JER.EQ.0 ) THEN
                  DO 401 KVARS=1,IVARS
                     IF(ZK8(LNOVA+KVARS-1).EQ.NOM) JER=ZI(LADVA+KVARS-1)
  401             CONTINUE
               ENDIF
            ENDIF
            IF (JER.GT.0) THEN
               IER = IER + 1
               CALL UTMESS('E','SUPERVISEUR.(ERREUR.FIPREP.05)',
     +                           ' "'//NOM//'" ARGUMENT DEJA DEFINI')
            ENDIF
            ZI (LADVA+IVARS-1) = ICSTE + 1
            NOM = '$'
            CALL CODENT(ICSTE+1,'G',NOM(2:8))
            CALL FIREMP(0,KCLASS,IVAL,RVAL,NOM,JER )
            IF ( JER .LT. 1 ) THEN
               IER = IER + 1
               CALL UTMESS('E','SUPERVISEUR.(ERREUR.FIPREP.06)',
     +                         ' "'//NOM//'" ARGUMENT DEJA DEFINI')
            ENDIF
C
         ELSEIF ( IACT .EQ.   5 ) THEN
C           --- DEFINITION: NOM D'UNE VARIABLE ---
            KCLASS = 12
            IF ( INDEX('IJKLMN',NOM(1:1)) .NE. 0 ) KCLASS = 11
            CALL FIREMP(0,KCLASS,IVAL,RVAL,NOM,JER )
            IF ( ITERM+2 .GT. MXTERM ) THEN
               MXTERM = MXTERM + MXITER
               CALL JUVECA(RESUFI//'.INFX',MXTERM)
               CALL JEVEUO(RESUFI//'.INFX','E',LEXPR)
            ENDIF
C
         ELSEIF ( ABS(IACT) .EQ.   6 ) THEN
C           --- ON ARCHIVE             ---
            ITERM = ITERM + 2
            IF ( ITERM .GT. MXTERM ) THEN
               MXTERM = MXTERM + MXITER
               CALL JUVECA(RESUFI//'.INFX',MXTERM)
               CALL JEVEUO(RESUFI//'.INFX','E',LEXPR)
            ENDIF
            IF ( IACT .EQ. -6 ) THEN
               KCLASS = 0
               IVAL2     = 1
               RVAL2(1)  = 1.D0
               RVAL2(2)  = 0.D0
               CALL FIREMP(1,KCLASS,IVAL2,RVAL2,'*',JER )
               ZI(LEXPR-2+ITERM) = ABS(JER)
            ELSE
               ITERM = ITERM - 1
            ENDIF
            IF ( ICLASS .EQ. 3  ) THEN

               NOM    = KVAL(1:IVAL)
               IF (PRFXCO .NE. '?') THEN
C                 CAS INCLUDE_MATERIAU
C                 RECHERCHE SI C'EST UN PARAMETRE
                  DO 123 IP=1,IVARS
                     IF (ZK8(LNOVA+IP-1) .EQ. KVAL(1:IVAL)) THEN
                        GOTO 124
                     ENDIF
 123              CONTINUE
C                 CA N'EST PAS UN PARAMETRE ON AJOUTE LE PREFIXE
                  NOM = PRFXCO(1:LPRFXC)//KVAL(1:IVAL)
                  IVAL = IVAL + LPRFXC
 124              CONTINUE
               ENDIF
               KCLASS = 0
C              VERIFICATION DE L'EXISTANCE DU CONCEPT
               CALL GETCMC(ICMDC)
               CALL GCUCDT(ICMDC,NOM,IERDT)
CCAR           WRITE(6,*)'GCUCDT: ',NOM,ICMDC,IERDT
CCAR: GCUCDT FOURNIT UN RESULTAT > 0 SI LE CONCEPT EST PRESENT 
CCAR: NUL S IL EST ABSENT ET -1 S IL EST PRESENT MAIS DETRUIT
CCAR: CE TEST NE SERT QUE POUR DECLENCHER UNE ERREUR SI NOM FAIT
CCAR: REFERENCE A UN CONCEPT DETRUIT

CCAR: LA RECHERCHE DES REFERENCES COMMENCE PAR UNE RECHERCHE 
CCAR: (VIA FIREMP) DANS LES
CCAR: NOMS CONNUS DES FONCTIONS INTERPRETEES (NOMS PREDEFINIS,
CCAR: FONCTIONS DEFINIES,
CCAR: CONSTANTES ET CONCEPTS DEJA REFERENCES)
               CALL FIREMP(1,KCLASS,IVAL,RVAL,NOM,JER )
CCAR           WRITE(6,*)'FIREMP: ',NOM,JER
               IF ( JER .LT. 1 ) THEN
CCAR: SI LA PREMIERE RECHERCHE EST INFRUCTUEUSE ON CHERCHE DANS JEVEUX
CCAR: FICNCP FAIT CETTE RECHERCHE ET COPIE LA REFERENCE DANS LES TABLES
CCAR:  FI
                  CALL FICNCP(NOM,JER,KER)
CCAR           WRITE(6,*)'FICNCP: ',NOM,JER,KER
                  IF(JER.EQ.0)THEN
                  CALL FICNC2(NOM,JER,KER)
CCAR           WRITE(6,*)'FICNC2: ',NOM,JER,KER
                  ENDIF
                  IF( JER.EQ.0 .AND. CDIR.EQ.'FONCTION') THEN
CCAR: SI ON A ENCORE RIEN TROUVE, ON CHERCHE DANS LES ARGUMENTS 
CCAR: DE LA FONCTION SI C'EN EST UNE
                     DO 301 KVARS=1,IVARS
                        IF( ZK8(LNOVA+KVARS-1).EQ. NOM ) THEN
                          JER = ZI(LADVA+KVARS-1)
                        ENDIF
  301                CONTINUE
CCAR           WRITE(6,*)'FIARGS: ',NOM,JER
CCAR: SI ON A TOUJOURS RIEN TROUVE ON CREE UNE REFERENCE ALTEREE (????)
                     IF(JER.EQ.0) THEN
                        CALL GETTCO(NOM,CTYPE)
CCAR           WRITE(6,*)'GETTCO: ',NOM,CTYPE
                        KCLASS = 42
                        IVAL = 1
                        CALL FIREMP(0,KCLASS,IVAL,RVAL,NOM,JER )
                        CALL FIREMP(3,KCLASS,IVAL,RVAL,NOM,JER )
                     ENDIF
                  ENDIF
                  IF (JER.LT.1 .OR. IERDT .LT. 0) THEN
                        IER = IER + 1
                        CALL UTMESS('E','SUPERVISEUR.(ERREUR'
     +                       //'.FIPREP.07)',
     +                       ' "'//NOM//'" JAMAIS DEFINIE')
                  ENDIF
               ENDIF
               ZI(LEXPR-1+ITERM) = ABS(JER)
            ELSEIF ( ICLASS .GT. 6 ) THEN
               CALL FIREMP(0,ICLASS,IVAL,RVAL,KVAL,JER )
               ZI(LEXPR-1+ITERM) = ABS(JER)
               IF ( JER .GT. 0 ) THEN
                  IER = IER + 1
                  I = IVAL
                  CALL UTMESS('E','SUPERVISEUR.(ERREUR.FIPREP.08)',
     +                         ' "'//KVAL(:I)//'" JAMAIS DEFINI')
               ENDIF
            ELSE
                  NOM = ' '
               CALL FIREMP(0,ICLASS,IVAL,RVAL,NOM,JER )
               ZI(LEXPR-1+ITERM) = ABS(JER)
            ENDIF
C
C
         ELSEIF ( IACT .EQ.   7 ) THEN
C           --- ON ARCHIVE  + OU - UNAIRE ---
            ITERM = ITERM + 1
            IF ( ITERM .GT. MXTERM ) THEN
               MXTERM = MXTERM + MXITER
               CALL JUVECA(RESUFI//'.INFX',MXTERM)
               CALL JEVEUO(RESUFI//'.INFX','E',LEXPR)
            ENDIF
            IVAL = 8
            KVAL(2:IVAL) = ' UNAIRE'
            NOM    = KVAL(1:IVAL)
            KCLASS = 0
            CALL FIREMP(1,KCLASS,IVAL,RVAL,NOM,JER )
            ZI(LEXPR-1+ITERM) = ABS(JER)
C
         ELSEIF ( IACT .EQ.   8 ) THEN
C           --- ON ARCHIVE  **   ---
            ITERM = ITERM + 1
            IF ( ITERM .GT. MXTERM ) THEN
               MXTERM = MXTERM + MXITER
               CALL JUVECA(RESUFI//'.INFX',MXTERM)
               CALL JEVEUO(RESUFI//'.INFX','E',LEXPR)
            ENDIF
            IVAL = 2
            KVAL = '** '
            NOM    = KVAL(1:IVAL)
            KCLASS = 0
            CALL FIREMP(1,KCLASS,IVAL,RVAL,NOM,JER )
            ZI(LEXPR-1+ITERM) = ABS(JER)
C
         ELSEIF ( IACT .EQ.   9 ) THEN
C           --- ON ARCHIVE  L'ARITE DE LA FONCTION
            CALL JEECRA(RESUFI//'.NOVA','LONUTI',IVARS,' ')
            CALL JEECRA(RESUFI//'.ADVA','LONUTI',IVARS,' ')
            KCLASS = 31
            NOM    = RESUFI
            CALL FIREMP(3,KCLASS,IVARS,RVAL,NOM,JER )
C
         ELSEIF ( IACT .EQ.  10 ) THEN
C           --- METTRE UN &LOAD DEVANT UN TABLEAU OU UNE FONCTION ---
            IF ( ITERM+2 .GT. MXTERM ) THEN
               MXTERM = MXTERM + MXITER
               CALL JUVECA(RESUFI//'.INFX',MXTERM)
               CALL JEVEUO(RESUFI//'.INFX','E',LEXPR)
            ENDIF
            CALL FIEXTR(ZI(LEXPR-1+ITERM),KCLASS,IVAL,RVAL,CVAL,
     +           KVAL,IBID,IBID)
            IF (KCLASS .GT. 20 .AND. KCLASS .LT. 40) THEN
               ZI(LEXPR+ITERM)   =  ZI(LEXPR-1+ITERM)
               ZI(LEXPR-1+ITERM) =  ILOAD
               ITERM = ITERM + 1
            ELSE IF ( KCLASS .GT. 40) THEN
               ZI(LEXPR+ITERM)   =  ZI(LEXPR-1+ITERM)
               ZI(LEXPR-1+ITERM) =  IFONC
               ITERM = ITERM + 1
            ENDIF
            ITERM = ITERM + 1
            ZI(LEXPR-1+ITERM) =  IPARG
         ELSEIF ( IACT .EQ.  11 ) THEN
C           --- METTRE UN '+' ENTRE DEUX NOMBRES  ---
            IF ( ITERM+2 .GT. MXTERM ) THEN
               MXTERM = MXTERM + MXITER
               CALL JUVECA(RESUFI//'.INFX',MXTERM)
               CALL JEVEUO(RESUFI//'.INFX','E',LEXPR)
            ENDIF
            IF(ISIGNE .NE. 0) THEN
               ITERM = ITERM + 1
               ZI(LEXPR-1+ITERM) = IPLUS
            ELSE
               IER = IER + 1
               CALL UTMESS('E','SUPERVISEUR.(ERREUR.FIPREP.09)',
     +              'MANQUE OPERATEUR ENTRE DEUX NOMBRES')
            ENDIF
            CALL FIREMP(0,ICLASS,IVAL,RVAL,KVAL,JER )
            ITERM = ITERM + 1
            ZI(LEXPR-1+ITERM) =  ABS(JER)
         ENDIF
         IETAT = NEWETA(ICLASS,IETAT)
      IF ( IETAT .GE. 0 ) GOTO  1
C
C     ---------------- SORTIE DE L'AUTOMATE ----------------------------
 998  CONTINUE
      IER = -1
 999  CONTINUE
C
 9999 CONTINUE
      CALL JEDEMA()
      END
