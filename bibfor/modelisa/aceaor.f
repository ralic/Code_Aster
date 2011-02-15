      SUBROUTINE ACEAOR(NOMA,NOMO,LMAX,NBEPO,NBEDI,NBTEL,NTYELE,
     &                  NOMELE,IVR,IFM,NBOCC)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER           LMAX,NBEPO,NBEDI,NTYELE(*),IVR(*),NBOCC(*)
      CHARACTER*8       NOMA,NOMO
      CHARACTER*16      NOMELE(*)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 15/02/2011   AUTEUR FLEJOU J-L.FLEJOU 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ----------------------------------------------------------------------
C     AFFE_CARA_ELEM
C     AFFECTATION DES CARACTERISTIQUES POUR LES ORIENTATIONS
C ----------------------------------------------------------------------
C IN  : NOMA   : NOM DU MAILLAGE
C IN  : NOMO   : NOM DU MODELE
C ----------------------------------------------------------------------
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
      CHARACTER*32     JEXNUM, JEXNOM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      PARAMETER    ( NBCAR = 100 , NBVAL = 1000 , NCO = 4 )
      REAL*8       VAL(NBVAL), X1(3), X2(3), X3(3)
      REAL*8       R8MIEM,TSM,R8RDDG,RDDG
      REAL*8       ALPHA,BETA,GAMMA
      CHARACTER*8  NOMU,CAR(NBCAR),NOMMAI,NOMNOE,CARORI(NCO)
      CHARACTER*4  EXITUY
      CHARACTER*16 CONCEP, CMD,NUNOEL
      CHARACTER*19 CARTOR
      CHARACTER*24 TMPNOR, TMPVOR, TMPORI
      CHARACTER*24 MLGNMA, MLGNNO, MLGTMA, MLGGNO, MLGGMA,MLGCOO,MLGCNX
      CHARACTER*24 MODNOE, MODNEM, MODMAI
      CHARACTER*1 K1BID
C     ------------------------------------------------------------------
      DATA CARORI  /'VECT_Y ','VECT_X_Y ','ANGL_NAUT','ANGL_VRIL'/
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      TSM  = R8MIEM()
      RDDG = R8RDDG()
      IER  = 0
      CALL GETRES(NOMU,CONCEP,CMD)
      TMPORI = NOMU//'.ORIENTATION'
C
C --- CONSTRUCTION DES CARTES
      CARTOR = NOMU//'.CARORIEN'
      TMPNOR = CARTOR//'.NCMP'
      TMPVOR = CARTOR//'.VALV'
C
C --- RECONSTRUCTION DES NOMS JEVEUX DU CONCEPT MODELE
      MODMAI = NOMO//'.MAILLE'
      MODNOE = NOMO//'.NOEUD'
      MODNEM = NOMO//'.MODELE    .NEMA'
C
C --- RECONSTRUCTION DES NOMS JEVEUX DU CONCEPT MAILLAGE ASSOCIE
      MLGNMA = NOMA//'.NOMMAI'
      MLGNNO = NOMA//'.NOMNOE'
      MLGTMA = NOMA//'.TYPMAIL'
      MLGCNX = NOMA//'.CONNEX'
      MLGGNO = NOMA//'.GROUPENO'
      MLGGMA = NOMA//'.GROUPEMA'
      MLGCOO = NOMA//'.COORDO    .VALE'
C
      CALL JELIRA(MLGNMA,'NOMMAX',NBMAIL,K1BID)
      CALL JEEXIN(MODMAI,IXMA)
      CALL JEEXIN(MODNEM,IXNW)
      CALL JEEXIN(MODNOE,IXNO)
      NBMTRD = 0
      IF (IXMA.NE.0) CALL JEVEUO(MODMAI,'L',JDME)
      IF (IXNO.NE.0) CALL JEVEUO(MODNOE,'L',JDNE)
      IF (IXNW.NE.0) THEN
         CALL JELIRA(MODNEM,'NMAXOC',NBMTRD,K1BID)
         CALL JEVEUO(MODNEM,'L',JDNW)
      ENDIF
      NBMTOT = NBMAIL + NBMTRD
C
C --- RECUPERATION DES ADRESSES JEVEUX UTILES
      CALL JEVEUO(MLGTMA,'L',JDTM)
      CALL JEVEUO(MLGCOO,'L',JDCO)
C
C --- RECUPERATION DES NUMEROS DES TYPES MAILLES POI1/SEG2
      CALL JENONU(JEXNOM('&CATA.TM.NOMTM','POI1') ,NTPOI)
      CALL JENONU(JEXNOM('&CATA.TM.NOMTM','SEG2') ,NTSEG)
C
C ================ MODIF ===============
C
      CALL JENONU(JEXNOM('&CATA.TM.NOMTM','SEG3') ,NTSEG3)
      CALL JENONU(JEXNOM('&CATA.TM.NOMTM','SEG4') ,NTSEG4)
C
C ================ MODIF ===============
C
      CALL WKVECT('&&TMPORIEN','V V K8',LMAX,JDLS)
      CALL WKVECT(TMPORI,'V V R',NBMTOT*3,JDOR)
C
C --- INITIALISATION DES VALEURS DES ANGLES NAUTIQUES PAR DEFAUT
C                      SUR TOUTES LES MAILLES (REPERE LOCAL PAR DEFAUT)
      DO 10 I = 1 , NBMTOT*3
         ZR(JDOR+I-1) = 0.D0
 10   CONTINUE
C
      DO 20 NUMMAI = 1 , NBMAIL
         NUTYMA = ZI(JDTM+NUMMAI-1)
         JAD = JDOR + (NUMMAI*3) - 3
C
         IF (NUTYMA.EQ.NTSEG) THEN
C
            CALL JEVEUO(JEXNUM(MLGCNX,NUMMAI),'L',JDNO)
            NO1 = ZI(JDNO)
            NO2 = ZI(JDNO+1)
            DO 22 I = 1 , 3
               X1(I) = ZR(JDCO+(NO1-1)*3+I-1)
               X2(I) = ZR(JDCO+(NO2-1)*3+I-1)
 22         CONTINUE
            CALL VDIFF(3,X2,X1,X3)
            IF (  ABS(X3(1)).GT.TSM .OR.
     &            ABS(X3(2)).GT.TSM .OR.
     &            ABS(X3(3)).GT.TSM ) THEN
               CALL ANGVX(X3,ALPHA,BETA)
               ZR(JAD)   = ZR(JAD)   + ALPHA
               ZR(JAD+1) = ZR(JAD+1) + BETA
            ENDIF
         ENDIF
 20   CONTINUE
C
C --- AFFECTATION DES VALEURS LUES DANS L OBJET TAMPON :
C     --------------------------------------------------
      IF (NBOCC(4).NE.0) THEN
         DO 30 IOC = 1 , NBOCC(4)
            CALL GETVEM(NOMA,'GROUP_MA','ORIENTATION','GROUP_MA',
     &                  IOC,1,LMAX ,ZK8(JDLS),NG)
            CALL GETVEM(NOMA,'MAILLE','ORIENTATION','MAILLE',
     &                  IOC,1,LMAX ,ZK8(JDLS),NM)
            CALL GETVEM(NOMA,'GROUP_NO','ORIENTATION','GROUP_NO',
     &                  IOC,1,LMAX ,ZK8(JDLS),NJ)
            CALL GETVEM(NOMA,'NOEUD','ORIENTATION','NOEUD',
     &                  IOC,1,LMAX ,ZK8(JDLS),NN)
            CALL GETVTX('ORIENTATION','CARA',IOC,1,NBCAR,CAR,NCAR)
            CALL GETVR8('ORIENTATION','VALE',IOC,1,NBVAL,VAL,NVAL)
C
C ---       "GROUP_MA" = TOUTES LES MAILLES POSSIBLES DE LA LISTE DES
C                                                    GROUPES DE MAILLES
            IF (NG.GT.0) THEN
               DO 36 I = 1 , NG
                  CALL JEVEUO(JEXNOM(MLGGMA,ZK8(JDLS+I-1)),'L',JDGM)
                  CALL JELIRA(JEXNOM(MLGGMA,ZK8(JDLS+I-1)),'LONUTI',
     &                                                 NBMAGR,K1BID)
                  DO 38 J = 1,NBMAGR
                     NUMMAI = ZI(JDGM+J-1)
                     CALL JENUNO(JEXNUM(MLGNMA,NUMMAI),NOMMAI)
                     CALL JEVEUO(JEXNUM(MLGCNX,NUMMAI),'L',JDNO)
                     NUTYMA = ZI(JDTM+NUMMAI-1)
                     JAD = JDOR + (NUMMAI*3) - 3
                     IF (  (NUTYMA.NE.NTSEG3) .AND.
     &                     (NUTYMA.NE.NTSEG4)) THEN
                        CALL AFFORI('MAILLE',NOMMAI,CAR(1),VAL,JAD,
     &                              JDNO,JDCO,IVR,NUTYMA,NTSEG,
     &                              CARORI,NCO,IER)
                     ENDIF
 38               CONTINUE
 36            CONTINUE
            ENDIF
C
C ---    "MAILLE" = TOUTES LES MAILLES POSSIBLES DE LA LISTE DE MAILLES
            IF (NM.GT.0) THEN
               DO 40 I = 1 , NM
                  NOMMAI = ZK8(JDLS+I-1)
                  CALL JENONU(JEXNOM(MLGNMA,NOMMAI),NUMMAI)
                  CALL JEVEUO(JEXNUM(MLGCNX,NUMMAI),'L',JDNO)
                  NUTYMA = ZI(JDTM+NUMMAI-1)
                  JAD = JDOR + (NUMMAI*3) - 3
                  IF ((NUTYMA.NE.NTSEG3).AND.(NUTYMA.NE.NTSEG4)) THEN
                     CALL AFFORI('MAILLE',NOMMAI,CAR(1),VAL,JAD,JDNO,
     &                            JDCO,IVR,NUTYMA,NTSEG,CARORI,NCO,IER)
                  ENDIF
 40            CONTINUE
            ENDIF
C
C ---       SI DES MAILLES TARDIVES EXISTENT POUR CE MODELE :
            IF (IXNW.NE.0) THEN
C ----         "GROUP_NO" = TOUTES LES MAILLES TARDIVES POSSIBLES DE LA
C                                            LISTE DE GROUPES DE NOEUDS
               IF (NJ.GT.0) THEN
                  DO 42 I = 1 , NJ
                     CALL JEVEUO(JEXNOM(MLGGNO,ZK8(JDLS+I-1)),'L',JDGN)
                     CALL JELIRA(JEXNOM(MLGGNO,ZK8(JDLS+I-1)),'LONUTI',
     &                                                  NBNOGR,K1BID)
                     DO 44 J = 1,NBNOGR
                        NUMNOE = ZI(JDGN+J-1)
                        DO 46 K = 1 , NBMTRD
                           IF (ZI(JDNW+K*2-2).EQ.NUMNOE) NUMTRD=K+NBMAIL
 46                     CONTINUE
                        CALL JENUNO(JEXNUM(MLGNNO,NUMNOE),NOMNOE)
                        JAD = JDOR + (NUMTRD*3) - 3
                        CALL AFFORI('NOEUD',NOMNOE,CAR(1),VAL,JAD,JDNO,
     &                             JDCO,IVR,NTPOI,NTSEG,CARORI,NCO,IER)
 44                  CONTINUE
 42               CONTINUE
               ENDIF
C ---          "NOEUD" = TOUTES LES MAILLES TARDIVES POSSIBLES DE LA
C                                                       LISTE DE NOEUDS
               IF (NN.GT.0) THEN
                  DO 48 I = 1 , NN
                     NOMNOE = ZK8(JDLS+I-1)
                     CALL JENONU(JEXNOM(MLGNNO,NOMNOE),NUMNOE)
                     DO 50 K = 1 , NBMTRD
                        IF (ZI(JDNW+K*2-2).EQ.NUMNOE) NUMTRD=K+NBMAIL
 50                  CONTINUE
                     JAD = JDOR + (NUMTRD*3) - 3
                     CALL AFFORI('NOEUD',NOMNOE,CAR(1),VAL,JAD,JDNO,
     &                             JDCO,IVR,NTPOI,NTSEG,CARORI,NCO,IER)
 48               CONTINUE
               ENDIF
            ENDIF
 30      CONTINUE
      ENDIF
C
      IF (IER.NE.0) THEN
         CALL U2MESS('F','MODELISA_12')
      ENDIF
C
C --- IMPRESSION DES VALEURS DES ORIENTATIONS SI DEMANDE
      NOCAOR = 0
      IF (IVR(3).EQ.1) WRITE(IFM,1000)
      DO 60 NUMMAI = 1 , NBMAIL
         NUTYEL = ZI(JDME+NUMMAI-1)
         DO 62 J = 1 , NBTEL
            IF (NUTYEL.EQ.NTYELE(J)) THEN
               NOCAOR = NOCAOR + 1
               IF (IVR(3).EQ.1) THEN
                  CALL JENUNO(JEXNUM(MLGNMA,NUMMAI),NOMMAI)
                  JAD = JDOR + NUMMAI*3 - 3
                  ALPHA = RDDG * ZR(JAD)
                  BETA  = RDDG * ZR(JAD+1)
                  GAMMA = RDDG * ZR(JAD+2)
                  WRITE(IFM,1010)NOMMAI,NOMELE(J),ALPHA,BETA,GAMMA
               ENDIF
               GOTO 60
            ENDIF
 62      CONTINUE
 60   CONTINUE
C++++++
      IF (IXNW.NE.0) THEN
         IF (IVR(3).EQ.1) WRITE(IFM,1020)
         DO 64 I = 1 , NBMTRD
            NUMNOE = ZI(JDNW+I*2-2)
            NUTYEL = ZI(JDNE+NUMNOE-1)
            DO 66 J = NBEPO+1 , NBEPO + NBEDI
               IF (NUTYEL.EQ.NTYELE(J)) THEN
                  NOCAOR = NOCAOR + 1
                  IF (IVR(3).EQ.1) THEN
                     CALL JENUNO(JEXNUM(MLGNNO,ZI(JDNW+I*2-2)),NOMNOE)
                     JAD = JDOR + (NBMAIL+I)*3 - 3
                     ALPHA = RDDG * ZR(JAD)
                     BETA  = RDDG * ZR(JAD+1)
                     GAMMA = RDDG * ZR(JAD+2)
                     WRITE(IFM,1010)NOMNOE,NOMELE(J),ALPHA,BETA,GAMMA
                  ENDIF
                  GOTO 64
               ENDIF
 66         CONTINUE
 64      CONTINUE
      ENDIF
C
1000  FORMAT(/,3X,'<ANGL> ORIENTATIONS SUR LES MAILLES DE TYPE POUTRE,'
     &  ,' BARRE OU DISCRET',//,3X,
     &  'NOM      TYPE             ALPHA         BETA',
     &  '          GAMMA')
1010  FORMAT(3X,A8,1X,A16,1X,3(D12.6,2X))
1020  FORMAT(/,3X,'<ANGL> ORIENTATIONS SUR LES NOEUDS DE TYPE ',
     &  'DISCRET',//,3X,'NOM      TYPE             ALPHA         BETA',
     &  '          GAMMA')
C
C --- AFFECTATION DES VALEURS DU TAMPON DANS LA CARTE ORIENTATION :
C     -------------------------------------------------------------
      IF ( NOCAOR .GT. 0  ) THEN
         CALL ALCART('G',CARTOR,NOMA,'CAORIE')
         CALL JEVEUO(TMPNOR,'E',JDCMPO)
         CALL JEVEUO(TMPVOR,'E',JDVLVO)
         ZK8(JDCMPO)   = 'ALPHA'
         ZK8(JDCMPO+1) = 'BETA'
         ZK8(JDCMPO+2) = 'GAMMA'
C
C ---    AFFECTATION DES MAILLES DU MAILLAGE (POUTRE, BARRE OU DISCRET)
         DO 68 NUMMAI = 1 , NBMAIL
            NUTYEL = ZI(JDME+NUMMAI-1)
            DO 70 J = 1 , NBTEL
               IF (NUTYEL.EQ.NTYELE(J)) THEN
C ---    RECUPERATION DES NUMEROS DES NOMS DES ELEMENTS
                  CALL JENUNO(JEXNUM('&CATA.TE.NOMTE',NUTYEL),NUNOEL)
                  IF (  (NUNOEL.NE.'MET3SEG3') .AND.
     &                  (NUNOEL.NE.'MET3SEG4') .AND.
     &                  (NUNOEL.NE.'MET6SEG3')) THEN
                     ZR(JDVLVO)    = ZR(JDOR+NUMMAI*3-3)
                     ZR(JDVLVO+1)  = ZR(JDOR+NUMMAI*3-2)
                     ZR(JDVLVO+2)  = ZR(JDOR+NUMMAI*3-1)
                     CALL NOCART(CARTOR,3,' ','NUM',1,' ',NUMMAI,' ',3)
                     GOTO 68
                  ENDIF
               ENDIF
 70         CONTINUE
 68      CONTINUE
C
C ---    AFFECTATION DES MAILLES TARDIVES DU MODELE (TYPE DISCRET)
         DO 72 I = 1 , NBMTRD
            NUMNOE = ZI(JDNW+I*2-2)
            NUTYEL = ZI(JDNE+NUMNOE-1)
            DO 74 J = NBEPO+1 , NBEPO + NBEDI
               IF (NUTYEL.EQ.NTYELE(J)) THEN
                  ZR(JDVLVO)    = ZR(JDOR+(I+NBMAIL)*3-3)
                  ZR(JDVLVO+1)  = ZR(JDOR+(I+NBMAIL)*3-2)
                  ZR(JDVLVO+2)  = ZR(JDOR+(I+NBMAIL)*3-1)
                  CALL NOCART(CARTOR,-3,' ','NUM',1,' ',-I,
     &                        NOMO//'.MODELE    ',3)
                  GOTO 72
               ENDIF
 74         CONTINUE
 72      CONTINUE
      ENDIF
C
CJMP AFFECTATION DES ELEMENTS TUYAUX
C
      CALL DISMOI('F','EXI_TUYAU',NOMO,'MODELE',IBID,EXITUY,IBID)
      IF (EXITUY.EQ.'OUI') THEN
      CALL ACEATU(NOMA,NOMO,NBEPO,NTYELE,IVR,IFM,NBOCC)
      ENDIF
      CALL JEDETR('&&TMPORIEN')
      CALL JEDETR(TMPNOR)
      CALL JEDETR(TMPVOR)
      CALL JEDETR(TMPORI)
C
      CALL JEDEMA()
      END
