      SUBROUTINE FIINIT(BASE,NOMCMD)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*1       BASE
      CHARACTER*(*)          NOMCMD
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 08/06/2004   AUTEUR CIBHHLV L.VIVAN 
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
C     INITIALISATION DE L'ANALYSEUR DE FONCTIONS INTERPRETEES
C     ------------------------------------------------------------------
C IN  BASE   : K1  : NOM DE LA BASE DE CREATION DES TABLES
C IN  NOMCMD : K16 : NOM DE LA COMMANDE APPELANTE
C                    SI NOMCMD = 'DEBUT' ALORS  ON CREE LES TABLES
C                    SINON ON LES RECUPERE POUR INITIALISER LES COMMUNS
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
C     ------------------------------------------------------------------
C     NBOPE1 : NOMBRE D'OPERATEURS UNAIRES
C     NBOPE2 : NOMBRE D'OPERATEURS BINAIRES
C     NBCSTE : NOMBRE DE CONSTANTES GLOBALES
C     ------------------------------------------------------------------
      PARAMETER      (NBOPE1=22, NBOPE2=17, NBCSTE=03)
      PARAMETER      (LDOPE1=       1, LFOPE1=       NBOPE1)
      PARAMETER      (LDOPE2=LFOPE1+1, LFOPE2=LFOPE1+NBOPE2)
      PARAMETER      (LDCSTE=LFOPE2+1, LFCSTE=LFOPE2+NBCSTE)
C     ------------------------------------------------------------------
C
C     DEBUT INCLUDE($INCLUDE) ------------------------------------------
      CHARACTER*24
     +   KNOMOP, KARITE, KPRIOR, KCLASS, KVALIS, KVALR8, KVALC8
      INTEGER
     +   LNOMOP, LARITE, LPRIOR, LCLASS, LVALIS, LVALR8, LVALC8
C     ------------------------------------------------------------------
      COMMON /FICK01/
     +   KNOMOP, KARITE, KPRIOR, KCLASS, KVALIS, KVALR8, KVALC8
      COMMON /FICI01/
     +   LNOMOP, LARITE, LPRIOR, LCLASS, LVALIS, LVALR8, LVALC8
C     ------------------------------------------------------------------
C     ICSTE  : NOMBRE D'INFORMATION ARCHIVES
C     IOPE1  : NOMBRE D'OPERATEURS UNAIRES
C     IOPE2  : NOMBRE D'OPERATEURS BINAIRES
C     LOPE1  : PREMIER NUMERO D'ORDRE DES OPERATEURS UNAIRES
C     LOPE2  : PREMIER NUMERO D'ORDRE DES OPERATEURS BINAIRES
      COMMON /FICL01/ ICSTE , IOPE1, IOPE2, LOPE1, LOPE2
      COMMON /FISY01/ IPARG,IPARD,IVIRG,IEGAL,IPTVI,ILOAD,IPLUS,IFONC
C     FIN INCLUDE($INCLUDE) --------------------------------------------
C
C
C     ------------------------------------------------------------------
C     REGLES SUR LES PRIORITES :
C      - L'ORDRE D'EVALUATION SE FAIT SELON LA PLUS FORTE PRIORITE
C      - SUR LES OPERATEURS  INFIXES A PRIORITE EGALE, C'EST L'ORDRE
C        D'APPARITION QUI FAIT FOI.
C     ------------------------------------------------------------------
C     CONVENTION :
C          XXFIXE(-) = -I POUR LES INFIXES  (I= NOMBRE D'OPERANDES)
C                    =  I POUR LES PREFIXES (I= NOMBRE D'OPERANDES)
C     ---------+---------------------------------------------------
C     CLASSE   !  OPERATEUR  CONSTANTE  VARIABLE  TABLEAU  FONCTION
C     ---------+---------------------------------------------------
C     ENTIER   !     0          1          11        21       31
C     REEL     !     0          2          12        22       32
C     COMPLEXE !     0          5          15        25       35
C     LOGIQUE  !     0          6          16        26       36
C     ---------+---------------------------------------------------
      CHARACTER*8     CBID,KHDF
      REAL*8          R8PI,R8DGRD,R8RDDG
      CHARACTER*8     XXOPER(LFCSTE)
      INTEGER         XXPRIO(LFCSTE)
      INTEGER         XXFIXE(LFCSTE)
      INTEGER         JCLASS
C
C     DECLARATION DES OPERATEURS UNAIRES -------------------------------
      DATA ( XXOPER(I),XXPRIO(I),XXFIXE(I), I= LDOPE1, LFOPE1 )
     +     /'+ UNAIRE',4, 1,     '- UNAIRE',4, 1,
     +      'INT  '   ,6, 1,     'REAL '   ,6, 1,      'AIMAG'   ,6, 1,
     +      'ABS  '   ,6, 1,     'SQRT '   ,6, 1,
     +      'EXP  '   ,6, 1,     'LOG  '   ,6, 1,      'LOG10'   ,6, 1,
     +      'SIN  '   ,6, 1,     'COS  '   ,6, 1,      'TAN  '   ,6, 1,
     +      'ASIN '   ,6, 1,     'ACOS '   ,6, 1,      'ATAN '   ,6, 1,
     +      'SINH '   ,6, 1,     'COSH '   ,6, 1,      'TANH '   ,6, 1,
     +      '&LOAD'   ,6, 1,    'HEAVYSID' ,6, 1,      '&FONC'   ,6, 1/
C
C     DECLARATION DES OPERATEURS BINAIRES ------------------------------
      DATA ( XXOPER(I),XXPRIO(I),XXFIXE(I),  I= LDOPE2,LFOPE2 )
     +     /'(    '   ,1, 2,
     +      '+    '   ,2,-2,      '-    '  ,2,-2,      '*    '   ,3,-2,
     +      '/    '   ,3,-2,      '**   '  ,5,-2,      '=    '   ,2,-2,
     +      'MOD  '   ,6, 2,      'MIN  '  ,6, 2,      'MAX  '   ,6, 2,
     +      '.EQ. '   ,4,-2,      '.NE. '  ,4,-2,      '.GT. '   ,4,-2,
     +      '.LT. '   ,4,-2,      '.GE. '  ,4,-2,      '.LE. '   ,4,-2,
     +      'ATAN2'   ,6, 2/
C
C     DECLARATION DES CONSTANTES GLOBALES PARTICULIERES AU SYSTEME -----
      DATA ( XXOPER(I),XXPRIO(I),XXFIXE(I), I= LDCSTE,LFCSTE)
     +     /')    '   ,0, 0,      ',    '  ,0, 0,      ';    '   ,0, 0/
C     ------------------------------------------------------------------
C
C     --- INITIALISATION DES COMMUNS ---
      CALL JEMARQ()
      IPREDF =  NBOPE1+NBOPE2+NBCSTE
C
      KNOMOP   = '&&SYS FI.NOMS       '
      KARITE   = '&&SYS FI.ARITE      '
      KPRIOR   = '&&SYS FI.PRIORITE   '
      KCLASS   = '&&SYS FI.CLASSE     '
      KVALIS   = '&&SYS FI.VALEURS_IS '
      KVALR8   = '&&SYS FI.VALEURS_R8 '
      KVALC8   = '&&SYS FI.VALEURS_C8 '
C
      KHDF = 'NON'
      CALL GETVTX(' ','FORMAT_HDF',1,1,1,KHDF,NBOCC)
      IF ( NOMCMD .EQ. 'DEBUT' ) THEN
         IF ( KHDF .EQ. 'NON' ) THEN 
           LONMAX = IPREDF + 100
           CALL WKVECT( KNOMOP ,BASE//' V K8',LONMAX,LNOMOP)
           CALL WKVECT( KARITE ,BASE//' V I ',LONMAX,LARITE)
           CALL WKVECT( KPRIOR ,BASE//' V I ',LONMAX,LPRIOR)
           CALL WKVECT( KCLASS ,BASE//' V I ',LONMAX,LCLASS)
           CALL WKVECT( KVALIS ,BASE//' V I ',LONMAX,LVALIS)
           CALL WKVECT( KVALR8 ,BASE//' V R ',LONMAX,LVALR8)
           CALL WKVECT( KVALC8 ,BASE//' V C ',LONMAX,LVALC8)
        ENDIF
      ENDIF
      CALL JEVEUS( KNOMOP ,'E',LNOMOP)
      CALL JEVEUS( KARITE ,'E',LARITE)
      CALL JEVEUS( KPRIOR ,'E',LPRIOR)
      CALL JEVEUS( KCLASS ,'E',LCLASS)
      CALL JEVEUS( KVALIS ,'E',LVALIS)
      CALL JEVEUS( KVALR8 ,'E',LVALR8)
      CALL JEVEUS( KVALC8 ,'E',LVALC8)
      IF ( NOMCMD .NE. 'DEBUT' .OR. KHDF .EQ. 'OUI' ) THEN
         CALL JELIRA( KNOMOP ,'LONUTI',ICSTE,CBID)
         GOTO 9999   
      ENDIF
C
      DO 100 I = 1, IPREDF
         ZK8(LNOMOP+I-1) = XXOPER(I)
         ZI (LPRIOR+I-1) = XXPRIO(I)
         ZI (LARITE+I-1) = XXFIXE(I)
         ZI (LCLASS+I-1) = 0
         ZI (LVALIS+I-1) = 0
         ZR (LVALR8+I-1) = 0.D0
         ZC (LVALC8+I-1) = 0.D0
  100 CONTINUE
C
      ICSTE = IPREDF
C
C     --- CONSTANTE PRE-DEFINIE: "PI" ---
      ZK8(LNOMOP+ICSTE) = 'PI'
      ZI (LARITE+ICSTE) = 0
      ZI (LPRIOR+ICSTE) = 0
      ZI (LCLASS+ICSTE) = 2
      ZI (LVALIS+ICSTE) = 0
      ZR (LVALR8+ICSTE) = R8PI()
      ZC (LVALC8+ICSTE) = 0.D0
      ICSTE = ICSTE + 1
C
C     --- CONSTANTE PRE-DEFINIE: "VRAI"--
      ZK8(LNOMOP+ICSTE) = 'VRAI'
      ZI (LARITE+ICSTE) = 0
      ZI (LPRIOR+ICSTE) = 0
      ZI (LCLASS+ICSTE) = 6
      ZI (LVALIS+ICSTE) = 1
      ZR (LVALR8+ICSTE) = 0.D0
      ZC (LVALC8+ICSTE) = 0.D0
      ICSTE = ICSTE + 1
C
C     --- CONSTANTE PRE-DEFINIE: "FAUX"--
      ZK8(LNOMOP+ICSTE) = 'FAUX'
      ZI (LARITE+ICSTE) = 0
      ZI (LPRIOR+ICSTE) = 0
      ZI (LCLASS+ICSTE) = 6
      ZI (LVALIS+ICSTE) = 0
      ZR (LVALR8+ICSTE) = 0.D0
      ZC (LVALC8+ICSTE) = 0.D0
      ICSTE = ICSTE + 1
C
C     --- CONSTANTE PRE-DEFINIE: "RD_DG"--
      ZK8(LNOMOP+ICSTE) = 'RD_DG'
      ZI (LARITE+ICSTE) = 0
      ZI (LPRIOR+ICSTE) = 0
      ZI (LCLASS+ICSTE) = 2
      ZI (LVALIS+ICSTE) = 0
      ZR (LVALR8+ICSTE) = R8RDDG()
      ZC (LVALC8+ICSTE) = 0.D0
      ICSTE = ICSTE + 1
C
C     --- CONSTANTE PRE-DEFINIE: "DG_RD"--
      ZK8(LNOMOP+ICSTE) = 'DG_RD'
      ZI (LARITE+ICSTE) = 0
      ZI (LPRIOR+ICSTE) = 0
      ZI (LCLASS+ICSTE) = 2
      ZI (LVALIS+ICSTE) = 0
      ZR (LVALR8+ICSTE) = R8DGRD()
      ZC (LVALC8+ICSTE) = 0.D0
      ICSTE = ICSTE + 1
C
C     --- LES CONSTANTES 0, 0. 2, 2.
C     --- SONT PLACEES SYSTEMATIQUEMENT APRES POUR ETRE RECONNUE PLUS
C     --- FACILEMENT
      ZK8(LNOMOP+ICSTE) = ' '
      ZI (LARITE+ICSTE) = 0
      ZI (LPRIOR+ICSTE) = 0
      ZI (LCLASS+ICSTE) = 1
      ZI (LVALIS+ICSTE) = 0
      ZR (LVALR8+ICSTE) = 0.D0
      ZC (LVALC8+ICSTE) = 0.D0
      ICSTE = ICSTE + 1
C
      ZK8(LNOMOP+ICSTE) = ' '
      ZI (LARITE+ICSTE) = 0
      ZI (LPRIOR+ICSTE) = 0
      ZI (LCLASS+ICSTE) = 2
      ZI (LVALIS+ICSTE) = 0
      ZR (LVALR8+ICSTE) = 0.D0
      ZC (LVALC8+ICSTE) = 0.D0
      ICSTE = ICSTE + 1
C
      ZK8(LNOMOP+ICSTE) = ' '
      ZI (LARITE+ICSTE) = 0
      ZI (LPRIOR+ICSTE) = 0
      ZI (LCLASS+ICSTE) = 1
      ZI (LVALIS+ICSTE) = 2
      ZR (LVALR8+ICSTE) = 0.D0
      ZC (LVALC8+ICSTE) = 0.D0
      ICSTE = ICSTE + 1
C
      ZK8(LNOMOP+ICSTE) = ' '
      ZI (LARITE+ICSTE) = 0
      ZI (LPRIOR+ICSTE) = 0
      ZI (LCLASS+ICSTE) = 2
      ZI (LVALIS+ICSTE) = 0
      ZR (LVALR8+ICSTE) = 2.D0
      ZC (LVALC8+ICSTE) = 0.D0
      ICSTE = ICSTE + 1
C
C
C
 9999 CONTINUE
C
C     --- NOTER LE NOMBRE DE SYMBOLES AFFECTES ---
      CALL JEECRA(KNOMOP,'LONUTI',ICSTE,'  ')
C
C     --- INDICATEURS DIVERS ---
      IOPE1 = NBOPE1
      IOPE2 = NBOPE2
      LOPE1 = LDOPE1
      LOPE2 = LDOPE2
C
C     --- INITIALISATION DU COMMUN DES SYMBOLES SPECIAUX ---
      IIVAL = 1
      RRVAL = 0.D0
      JCLASS = 3
      CALL FIREMP(1,JCLASS,IIVAL,RRVAL,'(',IPARG)
      CALL FIREMP(1,JCLASS,IIVAL,RRVAL,')',IPARD)
      CALL FIREMP(1,JCLASS,IIVAL,RRVAL,',',IVIRG)
      CALL FIREMP(1,JCLASS,IIVAL,RRVAL,'=',IEGAL)
      CALL FIREMP(1,JCLASS,IIVAL,RRVAL,';',IPTVI)
      CALL FIREMP(1,JCLASS,IIVAL,RRVAL,'+',IPLUS)
      IIVAL = 5
      CALL FIREMP(1,JCLASS,IIVAL,RRVAL,'&LOAD',ILOAD)
      CALL FIREMP(1,JCLASS,IIVAL,RRVAL,'&FONC',IFONC)
C
      CALL JEDEMA()
      END
