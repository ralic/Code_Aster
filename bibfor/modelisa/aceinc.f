      SUBROUTINE ACEINC(NOMA,NOMO,NBMCF,MCLF,NTYELE,NBOCC,IVR,
     +                  NBEPO,NBEDI,NBECO,NBECA,NBEBA,NBEMA,NBEGR,NBEGB,
     +                  NBTEL,NOCAPC,NMTGPC,NOCADI,NMTGDI,NOCACO,NMTGCO,
     +                  NOCACA,NMTGCA,NOCAMA,NMTGMA,NOCAPF,NMTGPF,
     +                  NOCAGR,NMTGGR,NOCAGB,NMTGGB,
     +                  JDLM,JDLN,LMAX,IER)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER                     NBMCF,       NTYELE(*),NBOCC(*),IVR(*)
      INTEGER           NBEPO,NBEDI,NBECO,NBECA,NBEBA,NBEGR,NBEGB,NBTEL
      INTEGER           NOCAPC,NMTGPC,NOCADI(*),NMTGDI(*),NOCACO,NMTGCO
      INTEGER           NOCACA,NMTGCA,NOCAMA,NMTGMA,NOCAPF,NMTGPF
      INTEGER           NOCAGR,NMTGGR,NOCAGB,NMTGGB
      INTEGER           JDLM,JDLN,LMAX,IER
      CHARACTER*8       NOMA,NOMO
      CHARACTER*16      MCLF(*)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 06/04/2004   AUTEUR DURAND C.DURAND 
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
      CHARACTER*1  KMA(3)
      CHARACTER*4  EXITUY
      CHARACTER*6  KIOC
      CHARACTER*8  NOMU, NOMMAI, NOMNOE, CAR(NBCAR)
      CHARACTER*16 CONCEP, CMD
      CHARACTER*24 MLGNMA, MLGNNO, MLGGNO, MLGGMA
      CHARACTER*24 MODMAI, MODNEM, MODNOE
      CHARACTER*8 K8BID
      CHARACTER*1 K1BID
      DATA KMA / 'K' , 'M' , 'A' /
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL GETRES(NOMU,CONCEP,CMD)
      DO 70 II = 1 ,3
         NOCADI(II) = 0
         NMTGDI(II) = 0
 70   CONTINUE
      NOCACO = 0
      NMTGCO = 0
      NOCAPC = 0
      NMTGPC = 0
      NOCACA = 0
      NMTGCA = 0
C
      NOCAMA = 0
      NMTGMA = 0
      NOCAPF = 0
      NMTGPF = 0
      NOCAGR = 0
      NMTGGR = 0
      NOCAGB = 0
      NMTGGB = 0
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
        IF (MCL.EQ.10.OR.MCL.EQ.13) GOTO 10
        DO 20 IOC = 1 , NBOCC(MCL)
          CALL CODENT(IOC,'G',KIOC)
          NG = 0
          NM = 0
          NJ = 0
          NN = 0
          CALL GETVEM(NOMA,'GROUP_MA',MCLF(MCL),'GROUP_MA',
     +              IOC,1,LMAX,ZK8(JDLS),NG)
          CALL GETVEM(NOMA,'MAILLE',MCLF(MCL),'MAILLE',
     +            IOC,1,LMAX,ZK8(JDLS),NM)
          IF ( MCL.EQ.3 .OR. MCL.EQ.4 ) THEN
            CALL GETVEM(NOMA,'GROUP_NO',MCLF(MCL),'GROUP_NO',
     +                IOC,1,LMAX,ZK8(JDLS),NJ)
            CALL GETVEM(NOMA,'NOEUD',MCLF(MCL),'NOEUD',
     +             IOC,1,LMAX,ZK8(JDLS),NN)
          ELSEIF ( MCL.EQ.12 ) THEN
            CALL GETVR8(MCLF(MCL),'ORIG_AXE',IOC,1,0,R8B,NORIG)
          ENDIF
          IF (MCL.EQ.1 .OR. MCL.EQ.3 .OR .MCL.EQ.4)THEN
            CALL GETVTX(MCLF(MCL),'CARA',IOC,1,NBCAR,CAR,NCAR)
            IF (NCAR.GT.0) NCARA = NCAR
          ENDIF
C
C ---     DES NOEUDS SONT AFFECTES :
          IF (NJ.GT.0 .OR. NN.GT.0) NNOE = 1
C
C ---     "GROUP_MA" = MAILLES DANS LA LISTE DES GROUPES DE MAILLES
          IF (NG.GT.0) THEN
            DO 34 I = 1 , NG
              IF (MCL.EQ.2) NOCACO = NOCACO + 1
C              IF (MCL.EQ.5) NOCAPC = NOCAPC + 1
              IF (MCL.EQ.6) NOCACA = NOCACA + 1
              IF (MCL.EQ.8) NOCAMA = NOCAMA + 1
              IF (MCL.EQ.9) NOCAPF = NOCAPF + 1
              IF (MCL.EQ.11) NOCAGR = NOCAGR + 1
              CALL JEVEUO(JEXNOM(MLGGMA,ZK8(JDLS+I-1)),'L',JDGM)
              CALL JELIRA(JEXNOM(MLGGMA,ZK8(JDLS+I-1)),'LONMAX',
     +                                              NBMAGR,K1BID)
              IF (MCL.EQ.5) THEN
                 NOCAPC = NOCAPC + NBMAGR
              ENDIF
              IF (MCL.EQ.12) THEN
                 IF (NORIG.EQ.0) THEN
                    NOCAGB = NOCAGB + 1
                 ELSE
                    NOCAGB = NOCAGB + NBMAGR
                 ENDIF
              ENDIF
              DO 36 J = 1,NBMAGR
                NUMMAI = ZI(JDGM+J-1)
                CALL JENUNO(JEXNUM(MLGNMA,NUMMAI),NOMMAI)
                NUTYEL = ZI(JDME+NUMMAI-1)
                IF (MCL.NE.4) ZI(JDLM+NUMMAI-1) = -MCL
                CALL VAFCAR('MAILLE',MCLF(MCL),NOMMAI,NBEPO,NBEDI,
     +                      NBECO,NBECA,NBEBA,NBEMA,NBEGR,NBEGB,
     +                            NUTYEL,NTYELE,CAR,NCARA,IVR,KIOC,IER)
 36           CONTINUE
              IF (MCL.EQ.3) THEN
                DO 38 II = 1 , NCARA
                  DO 40 JJ = 1 , 3
                    IF (CAR(II)(1:1).EQ.KMA(JJ)) NOCADI(JJ)=NOCADI(JJ)+1
 40               CONTINUE
 38             CONTINUE
              ENDIF
 34         CONTINUE
          ENDIF
C
C ---     "MAILLE" = MAILLES DE LA LISTE DE MAILLES
          IF (NM.GT.0) THEN
            IF (MCL.EQ.2) THEN
               NOCACO = NOCACO + 1
               NMTGCO = NMTGCO + NM
            ELSEIF (MCL.EQ.3) THEN
               DO 42 II = 1 , NCARA
                  DO 44 JJ = 1 , 3
                     IF (CAR(II)(1:1).EQ.KMA(JJ)) THEN
                        NOCADI(JJ) = NOCADI(JJ) + 1
                        NMTGDI(JJ) = NMTGDI(JJ) + NM
                        GOTO 42
                     ENDIF
 44               CONTINUE
 42            CONTINUE
            ELSEIF (MCL.EQ.5) THEN
C               NOCAPC = NOCAPC + 1
               NOCAPC = NOCAPC + NM
               NMTGPC = NMTGPC + NM
            ELSEIF (MCL.EQ.6) THEN
               NOCACA = NOCACA + 1
               NMTGCA = NMTGCA + NM
            ELSEIF (MCL.EQ.8) THEN
               NOCAMA = NOCAMA + 1
               NMTGMA = NMTGMA + NM
            ELSEIF (MCL.EQ.9) THEN
               NOCAPF = NOCAPF + 1
               NMTGPF = NMTGPF + NM
            ELSEIF (MCL.EQ.11) THEN
               NOCAGR = NOCAGR + 1
               NMTGGR = NMTGGR + NM
            ELSEIF (MCL.EQ.12) THEN
               IF (NORIG.EQ.0) THEN
                  NOCAGB = NOCAGB + 1
                  NMTGGB = NMTGGB + NM
               ELSE
                  NOCAGB = NOCAGB + NM
               ENDIF
            ENDIF
            DO 46 I = 1 , NM
              NOMMAI = ZK8(JDLS+I-1)
              CALL JENONU(JEXNOM(MLGNMA,NOMMAI),NUMMAI)
              NUTYEL = ZI(JDME+NUMMAI-1)
              IF (MCL.NE.4) ZI(JDLM+NUMMAI-1) = -MCL
              CALL VAFCAR('MAILLE',MCLF(MCL),NOMMAI,NBEPO,NBEDI,NBECO,
     +                    NBECA,NBEBA,NBEMA,NBEGR,NBEGB,
     +                            NUTYEL,NTYELE,CAR,NCARA,IVR,KIOC,IER)
 46         CONTINUE
          ENDIF
C
C ---     MAILLES TARDIVES EXISTENT POUR CE MODELE :
          IF (IXNW.NE.0 .AND. (MCL.EQ.3.OR.MCL.EQ.4) )THEN
C
C ---   "GROUP_NO" = MAILLES TARDIVES DANS LA LISTE DE GROUPES DE NOEUDS
            IF (NJ.GT.0) THEN
              DO 48 I = 1 , NJ
                CALL JEVEUO(JEXNOM(MLGGNO,ZK8(JDLS+I-1)),'L',JDGN)
                CALL JELIRA(JEXNOM(MLGGNO,ZK8(JDLS+I-1)),'LONMAX',
     +                                                NBNOGR,K1BID)
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
     +                        NBECO,NBECA,NBEBA,NBEMA,NBEGR,NBEGB,
     +                            NUTYEL,NTYELE,CAR,NCARA,IVR,KIOC,IER)
 50             CONTINUE
                IF (MCL.EQ.3) THEN
                  DO 54 II = 1 , NCARA
                    DO 56 JJ = 1 , 3
                      IF (CAR(II)(1:1).EQ.KMA(JJ)) THEN
                        NOCADI(JJ) = NOCADI(JJ) + 1
                        NMTGDI(JJ) = NMTGDI(JJ) + NBNOGR
                        GOTO 54
                      ENDIF
 56                 CONTINUE
 54               CONTINUE
                ENDIF
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
     +                      NBECO,NBECA,NBEBA,NBEMA,NBEGR,NBEGB,
     +                            NUTYEL,NTYELE,CAR,NCARA,IVR,KIOC,IER)
 58           CONTINUE
              IF (MCL.EQ.3) THEN
                DO 62 II = 1 , NCARA
                  DO 64 JJ = 1 , 3
                    IF (CAR(II)(1:1).EQ.KMA(JJ)) THEN
                      NOCADI(JJ) = NOCADI(JJ) + 1
                      NMTGDI(JJ) = NMTGDI(JJ) + NN
                      GOTO 62
                    ENDIF
 64               CONTINUE
 62             CONTINUE
              ENDIF
            ENDIF
          ENDIF
 20     CONTINUE
 10   CONTINUE
C
C --- AUCUNE MAILLE TARDIVE N'EXISTE SUR CE MODELE :
      CALL DISMOI('F','EXI_TUYAU',NOMO,'MODELE',IBID,EXITUY,IBID)
      IF (EXITUY.NE.'OUI') THEN
         IF (IXNW.EQ.0 .AND. NNOE.NE.0) THEN
            CALL UTMESS('E',CMD,'IMPOSSIBLE D"AFFECTER DES'//
     +              ' CARACTERISTIQUES A DES NOEUDS DE CE MODELE CAR'//
     +                           ' AUCUN NOEUD NE SUPPORTE UN ELEMENT')
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
               CALL UTMESS('A',CMD,'LA MAILLE '//NOMMAI//' N''A PAS ETE'
     +                //' AFFECTEE PAR DES CARACTERISTIQUES DE POUTRE.')
            ENDIF
 81       CONTINUE
        ENDIF
        IF ( NBOCC(3) .NE. 0 ) THEN
          DO 82 I = NBEPO+1 , NBEPO+NBEDI
            IF (ZI(JDLM+NUMMAI-1).EQ.NTYELE(I)) THEN
               CALL UTMESS('A',CMD,'LA MAILLE '//NOMMAI//' N''A PAS ETE'
     +                         //' AFFECTEE PAR UNE MATRICE (DISCRET).')
            ENDIF
 82       CONTINUE
        ENDIF
        IF ( NBOCC(2) .NE. 0 ) THEN
          DO 83 I = NBEPO+NBEDI+1 , NBEPO+NBEDI+NBECO
            IF (ZI(JDLM+NUMMAI-1).EQ.NTYELE(I)) THEN
               CALL UTMESS('A',CMD,'LA MAILLE '//NOMMAI//' N''A PAS ETE'
     +                 //' AFFECTEE PAR DES CARACTERISTIQUES DE COQUE.')
            ENDIF
 83       CONTINUE
        ENDIF
        IF ( NBOCC(6) .NE. 0 ) THEN
         DO 84 I = NBEPO+NBEDI+NBECO+1 , NBEPO+NBEDI+NBECO+NBECA
            IF (ZI(JDLM+NUMMAI-1).EQ.NTYELE(I)) THEN
               CALL UTMESS('A',CMD,'LA MAILLE '//NOMMAI//' N''A PAS ETE'
     +                 //' AFFECTEE PAR DES CARACTERISTIQUES DE CABLE.')
            ENDIF
 84       CONTINUE
        ENDIF
        IF ( NBOCC(7) .NE. 0 ) THEN
          DO 85 I = NBEPO+NBEDI+NBECO+NBECA+1 ,
     +                 NBEPO+NBEDI+NBECO+NBECA+NBEBA
            IF (ZI(JDLM+NUMMAI-1).EQ.NTYELE(I)) THEN
               CALL UTMESS('A',CMD,'LA MAILLE '//NOMMAI//' N''A PAS ETE'
     +                 //' AFFECTEE PAR DES CARACTERISTIQUES DE BARRE.')
            ENDIF
 85       CONTINUE
        ENDIF
CCCCC   IF ( NBOCC(8) .NE. 0 ) THEN
C         DO 86 I = NBEPO+NBEDI+NBECO+NBECA+NBEBA+1,
C    +                 NBEPO+NBEDI+NBECO+NBECA+NBEBA+NBEMA
C          IF (ZI(JDLM+NUMMAI-1).EQ.NTYELE(I)) THEN
C              CALL UTMESS('A',CMD,'LA MAILLE '//NOMMAI//' N''A PAS ETE'
C    +                //' AFFECTEE PAR DES CARACTERISTIQUES DE MASSIF.')
C           ENDIF
C86       CONTINUE
CCCCC   ENDIF
        IF ( NBOCC(11) .NE. 0 ) THEN
          DO 87 I = NBEPO+NBEDI+NBECO+NBECA+NBEBA+NBEMA+1,
     +                 NBEPO+NBEDI+NBECO+NBECA+NBEBA+NBEMA+NBEGR
            IF (ZI(JDLM+NUMMAI-1).EQ.NTYELE(I)) THEN
               CALL UTMESS('A',CMD,'LA MAILLE '//NOMMAI//' N''A PAS ETE'
     +            //' AFFECTEE PAR DES CARACTERISTIQUES DE ASSE_GRIL.')
            ENDIF
 87       CONTINUE
        ENDIF
        IF ( NBOCC(12) .NE. 0 ) THEN
          DO 88 I = NBEPO+NBEDI+NBECO+NBECA+NBEBA+NBEMA+NBEGR+1,NBTEL
            IF (ZI(JDLM+NUMMAI-1).EQ.NTYELE(I)) THEN
               CALL UTMESS('A',CMD,'LA MAILLE '//NOMMAI//' N''A PAS ETE'
     +            //' AFFECTEE PAR DES CARACTERISTIQUES DE GRILLE.')
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
                  CALL UTMESS('A',CMD,'LE NOEUD '//NOMNOE//' N''A PAS'//
     +                                  ' ETE AFFECTE PAR UNE MATRICE.')
               ENDIF
 102        CONTINUE
 100     CONTINUE
      ENDIF
C
      CALL JEDETR('&&TMPINC')
C
      CALL JEDEMA()
      END
