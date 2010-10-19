      SUBROUTINE ACEADI(NOMA,NOMO,LMAX,NBOCC,IVR,IFM)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER         LMAX,NBOCC,IVR(*),IFM
      CHARACTER*8     NOMA,NOMO
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
C
C --- ------------------------------------------------------------------
C     AFFE_CARA_ELEM
C     AFFECTATION DES CARACTERISTIQUES POUR LES ELEMENTS DISCRET
C --- ------------------------------------------------------------------
C  IN
C     NOMA   : NOM DU MAILLAGE
C     NOMO   : NOM DU MODELE
C     LMAX   : NOMBRE MAX DE MAILLE OU GROUPE DE MAILLE
C     NBOCC  : NOMBRE D'OCCURENCES DU MOT CLE DISCRET
C     IVR    : TABLEAU DES INDICES DE VERIFICATION
C --- ------------------------------------------------------------------
C
C --- -- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
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
C --- --  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      PARAMETER    ( NBCAR = 100 , NBVAL = 1000 , NRD = 2 )
      INTEGER      JDC(3), JDV(3), NVALF,LVAL
      INTEGER      JDCINF,  JDVINF
      REAL*8       VAL(NBVAL), ETA
      CHARACTER*1  KMA(3), K1BID
      CHARACTER*6  KI
      CHARACTER*8  K8B, NOMU
      CHARACTER*9  CAR(NBCAR)
      CHARACTER*16 SEC, REP, TOU, REPDIS(NRD), CONCEP, CMD, MCF, K16BID
      CHARACTER*16 SYM, SYMDIS(NRD)
      CHARACTER*19 CART(3), LIGMO, K19B, CARTDI
      CHARACTER*24 TMPND(3), TMPVD(3)
      CHARACTER*24 TMPDIS, MLGGNO, MLGNNO, TMCINF, TMVINF, MODNEM
C
      DATA REPDIS /'GLOBAL          ','LOCAL           '/
      DATA SYMDIS /'OUI             ','NON             '/
      DATA KMA    /'K','M','A'/

C --- --------------------------------------------------------------
C
      CALL JEMARQ()
      CALL GETRES(NOMU,CONCEP,CMD)
      TMPDIS = NOMU//'.DISCRET'
      MLGGNO = NOMA//'.GROUPENO'
      MLGNNO = NOMA//'.NOMNOE'
      LIGMO  = NOMO//'.MODELE    '
      MODNEM = NOMO//'.MODELE    .NEMA'
      I3D = 0
      I2D = 0
C
C --- RECUPERATION DE LA DIMENSION DU MAILLAGE
      NDIM1 = 3
      CALL DISMOI('F','Z_CST',NOMO,'MODELE',IBID,K8B,IER)
      IF ( K8B(1:3) .EQ. 'OUI' )  NDIM1 = 2
C
C --- ON REGARDE SI LE MODELE COMPORTE DES ELEMENTS DISCRETS 3D
      CALL MODEXI(NOMO,'DIS_',I3D)
C
C --- ON REGARDE SI LE MODELE COMPORTE DES ELEMENTS DISCRETS 2D
      CALL MODEXI(NOMO,'2D_DIS_',I2D)
C
C --- ON INTERDIT SUR UN MAILLAGE 2D D'AVOIR DES ELEMENTS DISCRETS
C --- 2D ET 3D
      IF (I2D.EQ.1.AND.I3D.EQ.1.AND.NDIM1.EQ.2) THEN
          CALL U2MESS('F','MODELISA_8')
      ENDIF
C
C --- ON INTERDIT SUR UN MAILLAGE 3D D'AVOIR DES ELEMENTS DISCRETS
C --- 2D
      IF (I2D.EQ.1.AND.NDIM1.EQ.3) THEN
          CALL U2MESS('F','MODELISA_9')
      ENDIF
C
C --- DIMENSION DU PROBLEME
      MCF  = ' '
      IF (I3D.EQ.1) THEN
        NDIM = 3
        MCF  = 'DISCRET'
      ELSEIF (I2D.EQ.1) THEN
        NDIM = 2
        MCF  = 'DISCRET_2D'
      ENDIF
      CALL JEEXIN(MODNEM,IXNW)
      NBMTRD = 0
      IF (IXNW.NE.0) THEN
         CALL JELIRA(MODNEM,'NMAXOC',NBMTRD,K1BID)
         CALL JEVEUO(MODNEM,'L',JDNW)
         CALL WKVECT(TMPDIS,'V V I',NBMTRD,JDDI)
      ENDIF
      CALL WKVECT('&&TMPDISCRET','V V K8',LMAX,JDLS)
C
C --- CONSTRUCTION DES CARTES ET ALLOCATION
C
C     CARTE INFO POUR TOUS LES DISCRETS
      CARTDI = NOMU//'.CARDINFO'
      CALL ALCART('G',CARTDI,NOMA,'CINFDI')
      TMCINF = CARTDI//'.NCMP'
      TMVINF = CARTDI//'.VALV'
      CALL JEVEUO(TMCINF,'E',JDCINF)
      CALL JEVEUO(TMVINF,'E',JDVINF)
C     PAR DEFAUT POUR M, A, K : REPERE GLOBAL , MATRICE SYMETRIQUE
      DO 200 I = 1 , 3
         ZK8(JDCINF+I-1) = 'REP'//KMA(I)//'    '
         ZR (JDVINF+I-1) = 1.D0
         ZK8(JDCINF+I+2) = 'SYM'//KMA(I)//'    '
         ZR (JDVINF+I+2) = 1.D0
200   CONTINUE
      ZK8(JDCINF+6) = 'ETAK    '
      ZR (JDVINF+6) = 0.D0
C
      CALL NOCART(CARTDI,1,' ',' ',0,' ',0,' ',7)
      IF (IXNW.NE.0) THEN
         CALL NOCART(CARTDI,-1,' ',' ',0,' ',0,LIGMO,7)
      ENDIF
      DO 220 I = 1, 3
C        CARTE POUR LES DISCRETS
         CART(I)  = NOMU//'.CARDISC'//KMA(I)
         TMPND(I) = CART(I)//'.NCMP'
         TMPVD(I) = CART(I)//'.VALV'
         CALL ALCART('G',CART(I),NOMA,'CADIS'//KMA(I))
         CALL JEVEUO(TMPND(I),'E',JDC(I))
         CALL JEVEUO(TMPVD(I),'E',JDV(I))
220   CONTINUE
C
C --- AFFECTATION SYSTEMATIQUE DE VALEURS NULLES DANS LES CARTES
C     POUR TOUTES LES MAILLES AFIN DE POUVOIR CALCULER LES MATRICES
C     K,M,A DANS TOUS LES CAS DANS LE REPERE GLOBAL PAR DEFAUT
      DO 20 I = 1 , 3
         DO 22 J = 1 , 144
            CALL CODENT(J,'G',KI)
            ZR(JDV(I)+J-1)  = 0.D0
            ZK8(JDC(I)+J-1) = KMA(I)//KI
22       CONTINUE
         CALL NOCART(CART(I),  1,' ',' ',0,' ',0,' ',144)
         IF (IXNW.NE.0) THEN
            CALL NOCART(CART(I),  -1,' ',' ',0,' ',0,LIGMO,144)
         ENDIF
20    CONTINUE

C
C --- BOUCLE SUR LES OCCURENCES DE DISCRET
      DO 30 IOC = 1 , NBOCC
         ETA  = 0.0D0
         IREP = 1
         ISYM = 1
         DO 31 I = 1 , NBVAL
            VAL(I) = 0.0D0
31       CONTINUE
         CALL GETVEM(NOMA,'GROUP_MA',MCF,'GROUP_MA',
     &                     IOC,1,LMAX,ZK8(JDLS),NG)
         CALL GETVEM(NOMA,'MAILLE'  ,MCF,'MAILLE',
     &                     IOC,1,LMAX,ZK8(JDLS),NM)
         CALL GETVEM(NOMA,'GROUP_NO',MCF,'GROUP_NO',
     &                     IOC,1,LMAX,ZK8(JDLS),NJ)
         CALL GETVEM(NOMA,'NOEUD'   ,MCF,'NOEUD',
     &                     IOC,1,LMAX,ZK8(JDLS),NN)
         CALL GETVR8(MCF,'VALE',IOC,1,NBVAL,VAL,NVAL)
         CALL ASSERT( NBVAL .GE. 1 )
         CALL GETVTX(MCF,'CARA',IOC,1,NBCAR,CAR,NCAR)
         IF (NCAR .GT. 0) NCARAC = NCAR
         CALL ASSERT( NCARAC .EQ. 1 )

C        POUR LES PARA_SENSI
         CALL GETVID ( MCF, 'VALE_F',IOC,1,0, K8B , NVALF)
         IF (NVALF .NE. 0) THEN
            CALL WKVECT('&&TMPVALEF','V V K8',NBVAL,JVALV)
            NVALF = -NVALF
            CALL GETVID (MCF,'VALE_F',IOC,1,NVALF,ZK8(JVALV),NVALF)
            DO 50 I = 1,NVALF
               K19B = ZK8(JVALV-1+I)
               CALL JEVEUO(K19B//'.VALE','L',LVAL)
               VAL(I) = ZR(LVAL-1+2)
50          CONTINUE
            CALL JEDETR('&&TMPVALEF')
         ENDIF

         CALL GETVTX(MCF,'REPERE'   ,IOC,1,1,REP,NREP)
         CALL GETVR8(MCF,'AMOR_HYST',IOC,1,1,ETA,NETA)
         IF (IOC.EQ.1 .AND. NREP.EQ.0) REP = REPDIS(1)
         DO 32 I = 1 , NRD
            IF (REP.EQ.REPDIS(I)) IREP = I
 32      CONTINUE

C        MATRICE SYMETRIQUE OU NON-SYMETRIQUE : PAR DEFAUT SYMETRIQUE
         CALL GETVTX(MCF,'SYME',IOC,1,1,SYM,NSYM)
         IF ( NSYM.EQ.0 ) SYM = SYMDIS(1)
         DO 33 I = 1 , NRD
            IF (SYM.EQ.SYMDIS(I)) ISYM = I
33       CONTINUE
C
         IF ( IVR(3) .EQ. 1 ) THEN
            IF ( ISYM .EQ. 1) THEN
               WRITE(IFM,1000) REP,'SYMETRIQUE',IOC
            ELSE
               WRITE(IFM,1000) REP,'NON-SYMETRIQUE',IOC
            ENDIF
1000        FORMAT(/,3X,
     &      '<DISCRET> MATRICES (REPERE ',A6,') '
     &      'AFFECTEES AUX ELEMENTS DISCRETS ',
     &      '(TYPE ',A,'), OCCURENCE ',I4)
         ENDIF
C
C ---    "GROUP_MA" = TOUTES LES MAILLES DE TOUS LES GROUPES DE MAILLES
         IF (NG.GT.0) THEN
            IV = 1
            DO 36 I = 1,NCARAC
               CALL AFFDIS(NDIM,IREP,ETA,CAR(I),VAL,JDC,JDV,
     &                     IVR,IV,KMA,NCMP,L,
     &                     JDCINF,JDVINF,ISYM,IFM)
               DO 38 II = 1 , NG
                  CALL NOCART(CARTDI, 2,ZK8(JDLS+II-1),' ',0,' ',0,
     &                        ' ',7)
                  CALL NOCART(CART(L),2,ZK8(JDLS+II-1),' ',0,' ',0,
     &                        ' ',NCMP)
38             CONTINUE
36          CONTINUE
         ENDIF
C
C ---   "MAILLE" = TOUTES LES MAILLES  DE LA LISTE DE MAILLES
         IF (NM.GT.0) THEN
            IV = 1
            DO 40 I = 1,NCARAC
               CALL AFFDIS(NDIM,IREP,ETA,CAR(I),VAL,JDC,JDV,
     &                     IVR,IV,KMA,NCMP,L,
     &                     JDCINF,JDVINF,ISYM,IFM)
               CALL NOCART(CARTDI, 3,' ','NOM',NM,ZK8(JDLS),0,' ',7)
               CALL NOCART(CART(L),3,' ','NOM',NM,ZK8(JDLS),0,' ',
     &                     NCMP)
40          CONTINUE
         ENDIF
C
C ---    SI DES MAILLES TARDIVES EXISTENT POUR CE MODELE :
         IF (IXNW.NE.0) THEN
C ---       "GROUP_NO" = TOUTES LES MAILLES TARDIVES DE
C                    LA LISTE DE GROUPES DE NOEUDS
            IF (NJ.GT.0) THEN
               DO 42 I = 1 , NJ
                  CALL JEVEUO(JEXNOM(MLGGNO,ZK8(JDLS+I-1)),'L',JDGN)
                  CALL JELIRA(JEXNOM(MLGGNO,ZK8(JDLS+I-1)),'LONUTI',
     &                                                  NBNOGR,K1BID)
                  CALL CRLINU('NUM', MLGNNO, NBNOGR, ZI(JDGN), K8B,
     &                         NBMTRD, ZI(JDNW), ZI(JDDI), KK )
                  IF (KK.GT.0) THEN
                     IV = 1
                     DO 44 II = 1,NCARAC
                        CALL AFFDIS(NDIM,IREP,ETA,CAR(II),VAL,JDC,JDV,
     &                              IVR,IV,KMA,NCMP,L,
     &                              JDCINF,JDVINF,ISYM,IFM)
                        CALL NOCART(CARTDI, -3,' ','NUM',KK,' ',
     &                              ZI(JDDI),LIGMO,7)
                        CALL NOCART(CART(L),-3,' ','NUM',KK,' ',
     &                              ZI(JDDI),LIGMO,NCMP)
 44                  CONTINUE
                  ENDIF
 42            CONTINUE
            ENDIF
C ---       "NOEUD" = TOUS LES NOEUDS TARDIFS DE LA LISTE DE NOEUDS
            IF (NN.GT.0) THEN
               CALL CRLINU('NOM', MLGNNO, NN, IBID, ZK8(JDLS),
     &                      NBMTRD, ZI(JDNW), ZI(JDDI), KK )
               IF (KK.GT.0) THEN
                  IV = 1
                  DO 46 I = 1,NCARAC
                     CALL AFFDIS(NDIM,IREP,ETA,CAR(I),VAL,JDC,JDV,
     &                           IVR,IV,KMA,NCMP,L,
     &                           JDCINF,JDVINF,ISYM,IFM)
                     CALL NOCART(CARTDI, -3,' ','NUM',KK,' ',ZI(JDDI),
     &                           LIGMO,7)
                     CALL NOCART(CART(L),-3,' ','NUM',KK,' ',ZI(JDDI),
     &                           LIGMO,NCMP)
 46               CONTINUE
               ENDIF
            ENDIF
         ENDIF
 30   CONTINUE
C
      IF (IXNW.NE.0) CALL JEDETR(TMPDIS)
      CALL JEDETR('&&TMPDISCRET')
      CALL GETFAC('RIGI_PARASOL',NBORP)
      CALL GETFAC('RIGI_MISS_3D',NBORM)
      IF (NBORP.EQ.0.AND.NBORM.EQ.0) THEN
         DO 240 I = 1 , 3
            CALL JEDETR(TMPND(I))
            CALL JEDETR(TMPVD(I))
240      CONTINUE
         CALL JEDETR(TMCINF)
         CALL JEDETR(TMVINF)
      ENDIF
C
      CALL JEDEMA()
      END
