      SUBROUTINE FIREMP(IMODE,JCLASS,IVAL,RVAL,NOM,IPLACE)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER           IMODE,JCLASS,IVAL,         IPLACE
      REAL*8                              RVAL(2)
      CHARACTER*(*)                            NOM
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 21/02/96   AUTEUR VABHHTS J.PELLET 
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
C     INSERTION/EXTRACTION D'UN ITEM DANS LES TABLES DES FONCTIONS
C     INTERPRETEES
C     ------------------------------------------------------------------
C IN  IMODE   : CODE DE RECHERCHE :
C          0: ON EST EN MODE INSERTION,L'ITEM NE DOIT PAS ETRE PRESENT
C             SI IL EST PRESENT IPLACE EST NEGATIF
C          1: ON EST EN MODE EXTRACTION, L'ITEM DOIT ETRE PRESENT
C          2: ON EST EN MODE MODIFICATION, L'ITEM DOIT ETRE PRESENT
C          3: ON EST EN MODE ALTERATION, L'ITEM DOIT ETRE PRESENT
C          4: ON EST EN MODE ALTERATION, MODE DIRECT: PLACE EST IN
C IN  JCLASS : CLASSE
C  OPERATEUR     0:
C  CONSTANTE        :    1: ENTIER,  2: REEL,  5: COMPLEXE,  6: LOGIQUE
C  VARIABLE         :   11: ENTIER, 12: REEL, 15: COMPLEXE, 16: LOGIQUE
C  TABLEAU          :   21: ENTIER, 22: REEL, 25: COMPLEXE, 26: LOGIQUE
C  FONCTION         :   31: ENTIER, 32: REEL, 35: COMPLEXE, 36: LOGIQUE
C  FONCTION TABULEE :   41: ENTIER, 42: REEL, 45: COMPLEXE, 46: LOGIQUE
C     ------------------------------------------------------------------
C     REMARQUE POUR JCLASS = 1 OU 2
C        - "NOM"         EST IGNORE
C        - "IMODE = 2"   N'EST PAS AUTORISE
C     ------------------------------------------------------------------
C     REMARQUE POUR JCLASS = 3
C        - SI "IMODE = 1"   IVAL CONTIENT SON TYPE
C     ------------------------------------------------------------------
C OUT IPLACE  : CODE DE RETOUR
C       <=0: ERREUR PLUS DE PLACE DANS LES TABLES.
C       >0 : POSITION DANS LA TABLE
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
      CHARACTER*8 K8BID
C     FIN INCLUDE($INCLUDE) --------------------------------------------
C
C
C     ---- RECHERCHE DANS LES TABLES ----
      CALL JEMARQ()
      IF (IMODE.EQ.4) THEN
C        --- ACCES PAR ADRESSE ---
         IF ( IPLACE.LT.1 .OR. IPLACE .GT. ICSTE ) THEN
             CALL UTDEBM('F','SUPERVISEUR.(ERREUR.FIREMP.10)','ERREUR')
             CALL UTIMPI('L','L''ADRESSE FOURNIT ',1,IPLACE)
             CALL UTIMPK('S',' EST HORS MEMOIRE ADRESSABLE.',0,' ')
             CALL UTIMPI('L',' ADRESSE MAXIMALE AUTORISEE',1,ICSTE)
             CALL UTFINM()
         ENDIF
C
      ELSEIF ( JCLASS .EQ. 1 .OR. JCLASS .EQ. 2 .OR.
     +     JCLASS .EQ.5  .OR. JCLASS .EQ. 6     ) THEN
C        --- ACCES PAR VALEUR ---
         IPLACE = 0
         DO 10 I = 1, ICSTE
            IF ( JCLASS .EQ. ZI(LCLASS-1+I) ) THEN
               IF ( JCLASS .EQ. 1 ) THEN
                  IF ( ZI(LVALIS-1+I) . EQ. IVAL ) THEN
                     IPLACE = I
                     GOTO 11
                  ENDIF
               ELSEIF ( JCLASS .EQ. 2 ) THEN
                  IF ( ZR(LVALR8-1+I) . EQ. RVAL(1) ) THEN
                     IPLACE = I
                     GOTO 11
                  ENDIF
               ELSEIF ( JCLASS .EQ. 5 ) THEN
                  IF ( ZC(LVALC8-1+I). EQ.DCMPLX(RVAL(1),RVAL(2))) THEN
                     IPLACE = I
                     GOTO 11
                  ENDIF
               ELSEIF ( JCLASS .EQ. 6 ) THEN
               CALL UTMESS('F','SUPERVISEUR.(ERREUR.FIREMP.01)',
     +                         'TERME DE TYPE LOGIQUE NON PREVU')
                  IF ( ZI(LVALIS-1+I) . EQ. IVAL ) THEN
                     IPLACE = I
                     GOTO 11
                  ENDIF
               ENDIF
            ENDIF
 10      CONTINUE
         IPLACE = 0
 11      CONTINUE
C
      ELSE
C        --- ACCES PAR NOM ---
         IPLACE = 0
         DO 20 I = 1, ICSTE
            IF ( NOM .EQ. ZK8(LNOMOP-1+I) ) THEN
               IPLACE = I
               GOTO 21
            ENDIF
 20      CONTINUE
         IPLACE = 0
 21      CONTINUE
      ENDIF
C     ------------------------------------------------------------------
C     --- ACTION SELON IMODE ---
      IF ( IMODE .EQ. 0 ) THEN
C        --- ON EST EN MODE INSERTION ---
         IF ( IPLACE .NE. 0 ) THEN
C           --- ERREUR ---
            IPLACE = -IPLACE
         ELSE
            ICSTE = ICSTE + 1
            CALL JELIRA(KNOMOP,'LONMAX',LONMAX,K8BID)
            IF ( ICSTE.GT.LONMAX ) THEN
               IPLACE =  0
               LONMAX = LONMAX + 100
               CALL JUVECA( KNOMOP ,LONMAX)
               CALL JEVEUS( KNOMOP ,'E',LNOMOP)
               CALL JUVECA( KARITE ,LONMAX)
               CALL JEVEUS( KARITE ,'E',LARITE)
               CALL JUVECA( KPRIOR ,LONMAX)
               CALL JEVEUS( KPRIOR ,'E',LPRIOR)
               CALL JUVECA( KCLASS ,LONMAX)
               CALL JEVEUS( KCLASS ,'E',LCLASS)
               CALL JUVECA( KVALIS ,LONMAX)
               CALL JEVEUS( KVALIS ,'E',LVALIS)
               CALL JUVECA( KVALR8 ,LONMAX)
               CALL JEVEUS( KVALR8 ,'E',LVALR8)
               CALL JUVECA( KVALC8 ,LONMAX)
               CALL JEVEUS( KVALC8 ,'E',LVALC8)
            ENDIF
            CALL JEECRA(KNOMOP,'LONUTI',ICSTE,K8BID)
            IPLACE = ICSTE
            ZK8(LNOMOP-1+IPLACE) = NOM
C-TEMP      IF (JCLASS.LT.10)  ZK8(LNOMOP-1+IPLACE) = ' '
            ZI(LPRIOR-1+IPLACE) = -1
            ZI(LARITE-1+IPLACE) = 0
            ZI(LCLASS-1+IPLACE) = JCLASS
            ZI(LVALIS-1+IPLACE) = IVAL
            ZR(LVALR8-1+IPLACE) = RVAL(1)
            ZC(LVALC8-1+IPLACE) = DCMPLX(RVAL(1),RVAL(2))
         ENDIF
      ELSEIF ( IMODE .EQ. 1 ) THEN
C        --- ON EST EN MODE RECUPERATION ---
         IF ( IPLACE.NE.0 ) THEN
            JCLASS  = ZI(LCLASS-1+IPLACE)
            IVAL    = ZI(LVALIS-1+IPLACE)
            RVAL(1) = ZR(LVALR8-1+IPLACE)
            IF (MOD(JCLASS,10).EQ.5) THEN
               RVAL(1) = ZC(LVALC8-1+IPLACE)
               RVAL(2) = DIMAG(ZC(LVALC8-1+IPLACE))
            ENDIF
         ELSE
            IPLACE = -(ICSTE+1)
         ENDIF
      ELSEIF ( IMODE .EQ. 2 .OR. IMODE.EQ. 4) THEN
C        --- ON EST EN MODE MODIFICATION ---
         ZI(LCLASS-1+IPLACE) = JCLASS
         ZI(LVALIS-1+IPLACE) = IVAL
         ZR(LVALR8-1+IPLACE) = RVAL(1)
         ZC(LVALC8-1+IPLACE) = DCMPLX( RVAL(1), RVAL(2))
      ELSEIF ( IMODE .EQ. 3 ) THEN
C        --- ON EST EN MODE LTERATION ---
         IF ( IPLACE .EQ. 0 ) GOTO 9999
         IF ( JCLASS .GT.30 .AND. JCLASS .LT. 47 ) THEN
            ZI(LPRIOR-1+IPLACE) = 6
            ZI(LARITE-1+IPLACE) = IVAL
         ENDIF
      ELSE
         CALL UTMESS('F','SUPERVISEUR.(ERREUR.FIREMP.02)',
     +               'ERREUR PROGRAMMATION VALEUR DE IMODE INCONNU')
      ENDIF
 9999 CONTINUE
      CALL JEDEMA()
      END
