      SUBROUTINE PIQUAG ( EPSI, RIP, REP, RIT, RET, BET, ESO, HSO,
     +                    H2, H3, L3, L4, L5, L6, TETAF, XMAX, YMAX,
     +                    LMAX, NT, MAILLA, NOGRNO, TYPSOU )
      IMPLICIT   NONE
      INTEGER             NT
      REAL*8              EPSI, RIP, REP, RIT, RET, BET, ESO, HSO,
     +                    H2, H3, L3, L4, L5, L6, TETAF, XMAX, YMAX,
     +                    LMAX
      CHARACTER*8         TYPSOU
      CHARACTER*8         MAILLA, NOGRNO
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SOUSTRUC  DATE 19/10/2010   AUTEUR DELMAS J.DELMAS 
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
C TOLE  CRP_21
C     OPERATEUR: "DEFI_GROUP" , MOTCLE FACTEUR "EQUE_PIQUA"
C     AUTEUR Y. WADIER
C
C     REALISE LA TRANSFORMATION : GEOMETRIE DE REFERENCE --> PIQUAGE
C
C-----------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
      CHARACTER*32       JEXNOM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER  INO, NBNOEU, NUME, IAGRN, JVALE
      REAL*8   R8PI, X0,Y0,Z0,X1,Y1,Z1
      REAL*8   XP,YP,ZP,X,Y,Z
      REAL*8   ALP,R1,TETAX
      LOGICAL  ROTATI, ALLONG
      LOGICAL  ROTA, PLAQ, PIQU, ALLO
      LOGICAL  ZONE1,ZONE2,ZONE3,ZONE4,ZONE5,ZONE6,ZONE7,ZONE8
      LOGICAL  QUAR1,QUAR2,QUAR3,QUAR4
      CHARACTER*8  K8B
      CHARACTER*24 GRPNOE, COOVAL
C     ------------------------------------------------------------------
C
      CALL JEMARQ ( )
C
C-------------INITIALISATION DES DONNEES LOGIQUES-----------------------
C
C     ROTATI = .TRUE. : ON FAIT LA ROTATION
C     ALLONG = .TRUE. : ON FAIT L'ALLONGEMENT
C
      ROTATI = .TRUE.
      ALLONG = .TRUE.
C
C     ROND = .TRUE. : ON ECRIT LES COORDONNEES DU PIQUAGE ROND
C     ROTA = .TRUE. : ON ECRIT LES COORDONNEES DU PIQUAGE APRES ROTATION
C     PLAQ = .TRUE. : ON ECRIT LES COORDONNEES DU PIQUAGE SUR PLAQUE
C     PIQU = .TRUE. : ON ECRIT LES COORDONNEES DU PIQUAGE
C     ALLO = .TRUE. : ON ECRIT LES COORDONNEES DU PIQUAGE ALLONGE
C
C      ROND = .TRUE.
      ROTA = .TRUE.
      PLAQ = .TRUE.
      PIQU = .TRUE.
      ALLO = .TRUE.
C
      ROTA =  ROTA .AND. ROTATI
      ALLO =  ALLO .AND. ALLONG
C
      GRPNOE = MAILLA//'.GROUPENO       '
      COOVAL = MAILLA//'.COORDO    .VALE'
      CALL JELIRA (JEXNOM(GRPNOE,NOGRNO),'LONUTI',NBNOEU,K8B)
      CALL JEVEUO (JEXNOM(GRPNOE,NOGRNO),'L',IAGRN)
      CALL JEVEUO ( COOVAL, 'E', JVALE )
C
      DO 10 INO = 1 , NBNOEU
C
C------------ LECTURE DES COORDONNEES DES NOEUDS DE L'EQUERRE ----------
C
         NUME = ZI(IAGRN+INO-1)
         X0 = ZR(JVALE-1+3*(NUME-1)+1)
         Y0 = ZR(JVALE-1+3*(NUME-1)+2)
         Z0 = ZR(JVALE-1+3*(NUME-1)+3)
C
C--------------- TRANSFORMATION EQUERRE  --->  PIQUAGE ROND ------------
C
         ALP = Y0 * R8PI() / ( 2.0D0 * YMAX )
         X1  = X0 * COS( ALP )
         Y1  = X0 * SIN( ALP )
         Z1  = Z0
C
C------------------------ ROTATION DU PIQUAGE ROND ---------------------
C
         IF ( ROTATI ) THEN
            CALL PIQROT ( X1, Y1, TETAX,
     +                    NT, RET, RIT, REP, TETAF, EPSI  )
         ENDIF
C
         X1  = X0 * COS(ALP+TETAX)
         Y1  = X0 * SIN(ALP+TETAX)
         Z1  = Z0
C
C-------------------------- REPERAGE DES 4 QUARTS ----------------------
C
         QUAR1 = .FALSE.
         QUAR2 = .FALSE.
         QUAR3 = .FALSE.
         QUAR4 = .FALSE.
C
         IF ( X1.GE.0.D0 .AND. Y1.GE.0.D0 ) QUAR1 = .TRUE.
C
         IF ( X1.LT.0.D0 .AND. Y1.GE.0.D0 ) QUAR2 = .TRUE.
C
         IF ( X1.LT.0.D0 .AND. Y1.LT.0.D0 ) QUAR3 = .TRUE.
C
         IF ( X1.GE.0.D0 .AND. Y1.LT.0.D0 ) QUAR4 = .TRUE.
C
C-------------------- RETOUR DANS LE PREMIER QUART  --------------------
C
         CALL PIQSYM ( X1, Y1, QUAR1, QUAR2, QUAR3, QUAR4 )
C
         R1 = SQRT( X1**2 + Y1**2 )
C
C------------------------ DEFINITION DES 8 ZONES -----------------------
C
         ZONE1 = .FALSE.
         ZONE2 = .FALSE.
         ZONE3 = .FALSE.
         ZONE4 = .FALSE.
         ZONE5 = .FALSE.
         ZONE6 = .FALSE.
         ZONE7 = .FALSE.
         ZONE8 = .FALSE.
C
         IF  (H2.LT.Z1)                                 ZONE1=.TRUE.
         IF ((H3.LT.Z1).AND.(Z1.LE.H2))                 ZONE2=.TRUE.
C
         IF (((RIP-EPSI).LE.R1).AND.(R1.LT.(L3+EPSI))) THEN
            IF ((RET.LT.Z1).AND.(Z1.LE.H3))             ZONE3=.TRUE.
            IF ((0.D0.LE.Z1).AND.(Z1.LE.RET))           ZONE4=.TRUE.
         ENDIF
C
         IF (( (L3-EPSI).LE.R1).AND.(R1.LT.(L4+EPSI))) THEN
            IF ((RET.LT.Z1).AND.(Z1.LE.(H3+EPSI)))      ZONE5=.TRUE.
            IF ((0.D0.LE.Z1).AND.(Z1.LE.RET))           ZONE6=.TRUE.
         ENDIF
C
         IF (((L4-EPSI).LE.R1).AND.(R1.LT.(L5+EPSI)))   ZONE7=.TRUE.
         IF  (L5.LE.R1)                                 ZONE8=.TRUE.
C
C
C------------ TRANSFORMATION PIQUAGE ROND ---> PIQUAGE CARRE -----------
C
         CALL PIQPLA (X1, Y1, Z1, XP, YP, ZP, ZONE7,ZONE8, L4,L6, EPSI)
C
         IF ( PLAQ ) THEN
            CALL PIQSYM ( XP, YP, QUAR1, QUAR2, QUAR3, QUAR4 )
C
            CALL PIQSYM ( XP, YP, QUAR1, QUAR2, QUAR3, QUAR4 )
         ENDIF
C
C------------ TRANSFORMATION PIQUAGE CARRE ---> PIQUAGE ----------------
C
         CALL PIQPIQ ( XP, YP, ZP, X, Y, Z, REP, RET, RIT, BET, ESO,
     &                 HSO, H2, H3, L4, L5, ZONE1, ZONE2, ZONE3,
     &                 ZONE4, ZONE5, ZONE6, ZONE7, ZONE8, TYPSOU )
C
         IF ( PIQU ) THEN
            CALL PIQSYM ( X, Y, QUAR1, QUAR2, QUAR3, QUAR4 )
C
            CALL PIQSYM ( X, Y, QUAR1, QUAR2, QUAR3, QUAR4 )
         ENDIF
C
C--------------------- ALLONGEMENT DU PIQUAGE --------------------------
C
         IF ( ALLONG ) CALL PIQALL (X, RET, RIT, REP, XMAX,LMAX, EPSI)
C
         IF ( ALLO ) THEN
            CALL PIQSYM (X, Y, QUAR1, QUAR2, QUAR3, QUAR4)
C
         ENDIF
C
         ZR(JVALE-1+3*(NUME-1)+1) = X
         ZR(JVALE-1+3*(NUME-1)+2) = Y
         ZR(JVALE-1+3*(NUME-1)+3) = Z
C
 10   CONTINUE
C
      CALL JEDEMA ( )
C
      END
