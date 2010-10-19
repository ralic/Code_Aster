      SUBROUTINE ACEINC(NOMA,NOMO,NBMCF,MCLF,NTYELE,NBOCC,IVR,
     &                  NBEPO,NBEDI,NBECO,NBECA,NBEBA,NBEMA,NBEGB,
     &                  NBTEL,NOCACO,NOCAGB,JDLM,JDLN,LMAX,IER)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER           NOCACO,NBMCF,NTYELE(*),NBOCC(*),IVR(*),NOCAGB
      INTEGER           NBEPO,NBEDI,NBECO,NBECA,NBEBA,NBEGB,NBTEL
      INTEGER           JDLM,JDLN,LMAX,IER
      CHARACTER*8       NOMA,NOMO
      CHARACTER*16      MCLF(*)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 19/10/2010   AUTEUR DELMAS J.DELMAS 
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
C TOLE CRP_21
C ----------------------------------------------------------------------
C     AFFE_CARA_ELEM
C     INCREMENTATION DES COMPTEURS D'APPELS A NOCART
C        POUR LES DISCRETS, COQUES, DEFI_ARCS, CABLES
C     VERIFICATION QUE TOUS LES ELEMENTS DU MODELE ONT ETE AFFECTES
C        PAR DES CARACTERISTIQUES.
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
C     ------------------------------------------------------------------
      PARAMETER    ( NBCAR = 100 )
      CHARACTER*4  EXITUY
      CHARACTER*6  KIOC
      CHARACTER*8  NOMU, NOMMAI, NOMNOE, CAR(NBCAR)
      CHARACTER*16 CONCEP, CMD
      CHARACTER*24 MLGNMA, MLGNNO, MLGGNO, MLGGMA
      CHARACTER*24 MODMAI, MODNEM, MODNOE
      CHARACTER*8 K8BID
      CHARACTER*1 K1BID
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL GETRES(NOMU,CONCEP,CMD)
C
      NOCACO = 0
      NOCAGB = 0
      NNOE   = 0
C
C --- RECONSTRUCTION DES NOMS JEVEUX DU CONCEPT MAILLAGE ET MODELE
      MODNEM = NOMO//'.MODELE    .NEMA'
      MODMAI = NOMO//'.MAILLE'
      MODNOE = NOMO//'.NOEUD'
      MLGNMA = NOMA//'.NOMMAI'
      MLGNNO = NOMA//'.NOMNOE'
      MLGGNO = NOMA//'.GROUPENO'
      MLGGMA = NOMA//'.GROUPEMA'
      CALL JELIRA(MLGNMA,'NOMMAX',NBMAIL,K1BID)
      CALL JEEXIN(MODNEM,IXNW)
      CALL JEEXIN(MODMAI,IXMA)
      CALL JEEXIN(MODNOE,IXNO)
      NBMTRD = 0
      IF (IXMA.NE.0) CALL JEVEUO(MODMAI,'L',JDME)
      IF (IXNO.NE.0) CALL JEVEUO(MODNOE,'L',JDNE)
      IF (IXNW.NE.0) THEN
         CALL JELIRA(MODNEM,'NMAXOC',NBMTRD,K1BID)
         CALL JEVEUO(MODNEM,'L',JDNW)
      ENDIF
C
      CALL WKVECT('&&TMPINC','V V K8',LMAX,JDLS)
C
      DO 10 MCL = 1 , NBMCF
        IF (MCL.EQ.12) GOTO 10
        DO 20 IOC = 1 , NBOCC(MCL)
          CALL CODENT(IOC,'G',KIOC)
          NG = 0
          NM = 0
          NJ = 0
          NN = 0
          IF ( MCL.EQ.10 ) THEN
            CALL GETVEM(NOMA,'GROUP_MA_POI1',MCLF(MCL),
     &                 'GROUP_MA_POI1',IOC,1,LMAX,ZK8(JDLS),NG)
            IF ( NG .EQ. 0 ) THEN
              CALL GETVEM(NOMA,'GROUP_MA_SEG2',MCLF(MCL),
     &                    'GROUP_MA_SEG2',IOC,1,LMAX,ZK8(JDLS),NG)
            ENDIF
          ELSE
            CALL GETVEM(NOMA,'GROUP_MA',MCLF(MCL),'GROUP_MA',
     &                  IOC,1,LMAX,ZK8(JDLS),NG)
            CALL GETVEM(NOMA,'MAILLE',MCLF(MCL),'MAILLE',
     &                  IOC,1,LMAX,ZK8(JDLS),NM)
          ENDIF
          IF ( MCL.EQ.3 .OR. MCL.EQ.4 .OR. MCL.EQ.13 ) THEN
            CALL GETVEM(NOMA,'GROUP_NO',MCLF(MCL),'GROUP_NO',
     &                  IOC,1,LMAX,ZK8(JDLS),NJ)
            CALL GETVEM(NOMA,'NOEUD',MCLF(MCL),'NOEUD',
     &                  IOC,1,LMAX,ZK8(JDLS),NN)
          ELSEIF ( MCL.EQ.11 ) THEN
            CALL GETVR8(MCLF(MCL),'ORIG_AXE',IOC,1,0,R8B,NORIG)
          ENDIF
          IF (MCL.EQ.1 .OR. MCL.EQ.3 .OR .MCL.EQ.4 .OR .MCL.EQ.13 .OR.
     &        MCL.EQ.10) THEN
            CALL GETVTX(MCLF(MCL),'CARA',IOC,1,NBCAR,CAR,NCAR)
            IF (NCAR.GT.0) NCARA = NCAR
          ENDIF
C
C ---     DES NOEUDS SONT AFFECTES :
          IF (NJ.GT.0 .OR. NN.GT.0) NNOE = 1
C
C ---     "GROUP_MA" = MAILLES DANS LA LISTE DES GROUPES DE MAILLES
          IF (NG.GT.0) THEN
            IF (MCL.EQ.2)  NOCACO = NOCACO + 1
            IF (MCL.EQ.11) NOCAGB = NOCAGB + 1
            DO 34 I = 1 , NG
              CALL JEVEUO(JEXNOM(MLGGMA,ZK8(JDLS+I-1)),'L',JDGM)
              CALL JELIRA(JEXNOM(MLGGMA,ZK8(JDLS+I-1)),'LONUTI',
     &                                              NBMAGR,K1BID)
              DO 36 J = 1,NBMAGR
                NUMMAI = ZI(JDGM+J-1)
                CALL JENUNO(JEXNUM(MLGNMA,NUMMAI),NOMMAI)
                NUTYEL = ZI(JDME+NUMMAI-1)
                IF (MCL.NE.4) ZI(JDLM+NUMMAI-1) = -MCL
                CALL VAFCAR('MAILLE',MCLF(MCL),NOMMAI,NBEPO,NBEDI,
     &                      NBECO,NBECA,NBEBA,NBEMA,NBEGB,
     &                            NUTYEL,NTYELE,CAR,NCARA,IVR,KIOC,IER)
 36           CONTINUE
 34         CONTINUE
          ENDIF
C
C ---     "MAILLE" = MAILLES DE LA LISTE DE MAILLES
          IF (NM.GT.0) THEN
            IF (MCL.EQ.2) NOCACO = NOCACO + 1
            IF (MCL.EQ.11) NOCAGB = NOCAGB + 1
            DO 46 I = 1 , NM
              NOMMAI = ZK8(JDLS+I-1)
              CALL JENONU(JEXNOM(MLGNMA,NOMMAI),NUMMAI)
              NUTYEL = ZI(JDME+NUMMAI-1)
              IF (MCL.NE.4) ZI(JDLM+NUMMAI-1) = -MCL
              CALL VAFCAR('MAILLE',MCLF(MCL),NOMMAI,NBEPO,NBEDI,NBECO,
     &                    NBECA,NBEBA,NBEMA,NBEGB,
     &                            NUTYEL,NTYELE,CAR,NCARA,IVR,KIOC,IER)
 46         CONTINUE
          ENDIF
C
C ---     MAILLES TARDIVES EXISTENT POUR CE MODELE :
          IF (IXNW.NE.0 .AND. (MCL.EQ.3.OR.MCL.EQ.4.OR.MCL.EQ.13) )THEN
C
C ---   "GROUP_NO" = MAILLES TARDIVES DANS LA LISTE DE GROUPES DE NOEUDS
            IF (NJ.GT.0) THEN
              DO 48 I = 1 , NJ
                CALL JEVEUO(JEXNOM(MLGGNO,ZK8(JDLS+I-1)),'L',JDGN)
                CALL JELIRA(JEXNOM(MLGGNO,ZK8(JDLS+I-1)),'LONUTI',
     &                                                NBNOGR,K1BID)
                DO 50 J = 1,NBNOGR
                  NUMNOE = ZI(JDGN+J-1)
                  IF (MCL.NE.4) THEN
                    DO 52 K = 1 , NBMTRD
                      IF(ZI(JDNW+K*2-2).EQ.NUMNOE) ZI(JDLN+K-1)=-MCL
 52                 CONTINUE
                  ENDIF
                  CALL JENUNO(JEXNUM(MLGNNO,NUMNOE),NOMNOE)
                  NUTYEL = ZI(JDNE+NUMNOE-1)
                  CALL VAFCAR('NOEUD',MCLF(MCL),NOMNOE,NBEPO,NBEDI,
     &                        NBECO,NBECA,NBEBA,NBEMA,NBEGB,
     &                            NUTYEL,NTYELE,CAR,NCARA,IVR,KIOC,IER)
 50             CONTINUE
 48           CONTINUE
            ENDIF
C
C ---       "NOEUD" = MAILLES TARDIVES  DE LA LISTE DE NOEUDS
            IF (NN.GT.0) THEN
              DO 58 I = 1 , NN
                NOMNOE = ZK8(JDLS+I-1)
                CALL JENONU(JEXNOM(MLGNNO,NOMNOE),NUMNOE)
                IF (MCL.NE.4) THEN
                  DO 60 K = 1 , NBMTRD
                    IF (ZI(JDNW+K*2-2).EQ.NUMNOE) ZI(JDLN+K-1)=-MCL
 60               CONTINUE
                ENDIF
                NUTYEL = ZI(JDNE+NUMNOE-1)
                CALL VAFCAR('NOEUD',MCLF(MCL),NOMNOE,NBEPO,NBEDI,
     &                      NBECO,NBECA,NBEBA,NBEMA,NBEGB,
     &                            NUTYEL,NTYELE,CAR,NCARA,IVR,KIOC,IER)
 58           CONTINUE
            ENDIF
          ENDIF
 20     CONTINUE
 10   CONTINUE
C
C --- AUCUNE MAILLE TARDIVE N'EXISTE SUR CE MODELE :
      CALL DISMOI('F','EXI_TUYAU',NOMO,'MODELE',IBID,EXITUY,IBID)
      IF (EXITUY.NE.'OUI') THEN
         IF (IXNW.EQ.0 .AND. NNOE.NE.0) THEN
            CALL U2MESS('E','MODELISA_37')
            IER = IER + 1
         ENDIF
C      ELSE
C         IER=0
      ENDIF
C
C --- VERIFICATION QUE TOUS LES ELEMENTS SONT AFFECTES :
C     --------------------------------------------------
      DO 80 NUMMAI = 1 , NBMAIL
        CALL JENUNO(JEXNUM(MLGNMA,NUMMAI),NOMMAI)
        IF ( NBOCC(1) .NE. 0 ) THEN
          DO 81 I = 1 , NBEPO
            IF (ZI(JDLM+NUMMAI-1).EQ.NTYELE(I)) THEN
               CALL U2MESK('A','MODELISA_38',1,NOMMAI)
            ENDIF
 81       CONTINUE
        ENDIF
        IF ( NBOCC(3).NE.0 .OR. NBOCC(10).NE.0) THEN
          DO 82 I = NBEPO+1 , NBEPO+NBEDI
            IF (ZI(JDLM+NUMMAI-1).EQ.NTYELE(I)) THEN
               CALL U2MESK('A','MODELISA_39',1,NOMMAI)
            ENDIF
 82       CONTINUE
        ENDIF
        IF ( NBOCC(6) .NE. 0 ) THEN
         DO 84 I = NBEPO+NBEDI+NBECO+1 , NBEPO+NBEDI+NBECO+NBECA
            IF (ZI(JDLM+NUMMAI-1).EQ.NTYELE(I)) THEN
               CALL U2MESK('A','MODELISA_40',1,NOMMAI)
            ENDIF
 84       CONTINUE
        ENDIF
        IF ( NBOCC(7) .NE. 0 ) THEN
          DO 85 I = NBEPO+NBEDI+NBECO+NBECA+1 ,
     &                 NBEPO+NBEDI+NBECO+NBECA+NBEBA
            IF (ZI(JDLM+NUMMAI-1).EQ.NTYELE(I)) THEN
               CALL U2MESK('A','MODELISA_41',1,NOMMAI)
            ENDIF
 85       CONTINUE
        ENDIF
        IF ( NBOCC(12) .NE. 0 ) THEN
          DO 88 I = NBEPO+NBEDI+NBECO+NBECA+NBEBA+NBEMA+1,NBTEL
            IF (ZI(JDLM+NUMMAI-1).EQ.NTYELE(I)) THEN
               CALL U2MESK('A','MODELISA_42',1,NOMMAI)
            ENDIF
 88       CONTINUE
        ENDIF
 80   CONTINUE
      IF (IXNW.NE.0) THEN
         DO 100 K = 1 , NBMTRD
            NUMNOE = ZI(JDNW+K*2-2)
            CALL JENUNO(JEXNUM(MLGNNO,NUMNOE),NOMNOE)
            DO 102 I = NBEPO+1 , NBEPO+NBEDI
               IF (ZI(JDLN+K-1).EQ.NTYELE(I)) THEN
                  CALL U2MESK('A','MODELISA_43',1,NOMNOE)
               ENDIF
 102        CONTINUE
 100     CONTINUE
      ENDIF
C
      CALL JEDETR('&&TMPINC')
C
      CALL JEDEMA()
      END
