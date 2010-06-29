      SUBROUTINE ACEARP(NOMA,NOMO,LMAX,NOEMAF,NBOCC,IVR,IFM)
      IMPLICIT          NONE
      INTEGER           IFM,LMAX,NOEMAF,NBOCC,IVR(*)
      CHARACTER*8       NOMA,NOMO
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 28/06/2010   AUTEUR FLEJOU J-L.FLEJOU 
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
C ----------------------------------------------------------------------
C     AFFE_CARA_ELEM
C     AFFECTATION DES CARACTERISTIQUES POUR LES ELEMENTS DISCRET
C ----------------------------------------------------------------------
C IN  : NOMA   : NOM DU MAILLAGE
C IN  : NOMO   : NOM DU MODELE
C IN  : LMAX   : NOMBRE MAX DE MAILLE OU GROUPE DE MAILLE
C IN  : NBOCC  : NOMBRE D'OCCURRENCES DU MOT CLE DISCRET
C IN  : IVR    : TABLEAU DES INDICES DE VERIFICATION
C ----------------------------------------------------------------------
C TOLE  CRP_20
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
      INTEGER      NBCAR,NBVAL,NRD
      PARAMETER    ( NBCAR = 100 , NBVAL = 12 , NRD = 2 )
      INTEGER      JDC(3), JDV(3), IBID,NIV,IR,IUNITE
      INTEGER      JDCINF,  JDVINF
      INTEGER      I,IAMTO,IER,II,IN,INBN,INO,INOE,IOC,IREP,IREPN,IREPV
      INTEGER      IRGNO,IRGTO,ISYM,ITBMP,ITBNO,ITROU,IUNIFI,IV
      INTEGER      IXCI,IXCKMA,IXNW,J,JD,JDDI,JDLS,JDNW,JJ,JN,K,KK
      INTEGER      L,LDGM,LDNM,LOKM,LOREP,NBMTRD,NBNMA
      INTEGER      NBNO,NBNOEU,NBORM,NC,NCAR,NCARAC,NCMP
      INTEGER      NDIM,NG,NGP,NMA,NREP,NUMNOE,NVAL

      REAL*8       VAL(NBVAL), ETA, VALE(6),RIROT(3)
      CHARACTER*1  KMA(3), K1BID
      CHARACTER*8  NOMNOE, NOGP, NOMMAI, K8BID, NOMU, CAR(NBCAR)
      CHARACTER*16 REP, REPDIS(NRD), CONCEP, CMD
      CHARACTER*19 CART(3), LIGMO, CARTDI
      CHARACTER*19 VREPXV, VREPXN
      CHARACTER*24 TMPND(3), TMPVD(3)
      CHARACTER*24 TMPDIS, MLGNNO, MLGNMA, TMCINF, TMVINF, MODNEM


      LOGICAL      TRANSL,TRAROT,EURPLX,LBID
      DATA REPDIS  /'GLOBAL          ','LOCAL           '/
      DATA KMA     /'K','M','A'/
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL GETRES(NOMU,CONCEP,CMD)
      TMPDIS = NOMU//'.DISCRET'
      MLGNNO = NOMA//'.NOMNOE'
      MLGNMA = NOMA//'.NOMMAI'
      LIGMO  = NOMO//'.MODELE    '
      MODNEM = NOMO//'.MODELE    .NEMA'
      CALL JEEXIN(MODNEM,IXNW)
      NBMTRD = 0
      IF (IXNW.NE.0) THEN
         CALL JELIRA(MODNEM,'NMAXOC',NBMTRD,K1BID)
         CALL JEVEUO(MODNEM,'L',JDNW)
         CALL WKVECT(TMPDIS,'V V I',NBMTRD,JDDI)
      ENDIF
      CALL WKVECT('&&TMPDISCRET','V V K8',LMAX,JDLS)
      CALL WKVECT('&&TMPTABNO','V V K8',LMAX,ITBNO)
      CALL WKVECT('&&TMPRIGNO','V V R',6*LMAX,IRGNO)
      CALL WKVECT('&&TMPRIGTO','V V R',6*NOEMAF,IRGTO)
      CALL WKVECT('&&TMPAMOTO','V V R',6*NOEMAF,IAMTO)
      CALL WKVECT('&&TMPTABMP','V V K8',LMAX,ITBMP)
C     POUR EUROPLEXUS
C     SI EUROPLEXUS ALORS TOUTES LES OCCURRENCES DE RIGI_PARASOL DOIVENT
C     AVOIR EUROPLEXUS='OUI'. TEST SUR LA 1ERE OCCURENCE DU MOT CLEF,
C     PUIS DANS LA BOUCLE SUR LES OCCURRENCES QUE L'OPTION NE CHANGE PAS
      EURPLX = .FALSE.
      CALL GETVTX('RIGI_PARASOL','EUROPLEXUS',1,1,1,K8BID,IBID)
      IF ( IBID .NE. 0 ) THEN
         EURPLX = ( K8BID(1:3) .EQ. 'OUI' )
      ENDIF
      IF ( EURPLX ) THEN
         VREPXV = NOMU//'.CARRIGXV'
         VREPXN = NOMU//'.CARRIGXN'
C        LES STRUCTURES SONT UTILISEES SEULEMENT EN PYTHON
         CALL WKVECT(VREPXV,'G V R', 6*LMAX,IREPV)
         CALL WKVECT(VREPXN,'G V K8',  LMAX,IREPN)
      ENDIF
C
      IFM = IUNIFI('MESSAGE')
C
C --- RECUPERATION DE LA DIMENSION DU MAILLAGE
      NDIM = 3
C      CALL DISMOI('F','Z_CST',NOMO,'MODELE',IBID,K8BID,IER)
C      IF ( K8BID(1:3) .EQ. 'OUI' )  NDIM = 2

      CALL DISMOI('F','DIM_GEOM',NOMO,'MODELE',IBID,K8BID,IER)
      IF (IBID.GE.1000) IBID = IBID - 1000
      IF (IBID.GE.100) THEN
         IBID = IBID - 100
         NDIM=1
      ENDIF
      IF (IBID.GE.20) THEN
         IBID = IBID - 20
         NDIM=2
      ENDIF
      IF (IBID.EQ.3) THEN
         NDIM=3
      ENDIF
C
C --- CONSTRUCTION DES CARTES ET ALLOCATION
      CARTDI = NOMU//'.CARDINFO'
      TMCINF = CARTDI//'.NCMP'
      TMVINF = CARTDI//'.VALV'
C     SI LA CARTE N'EXISTE PAS ON LA CREE
      CALL JEEXIN(TMCINF,IXCI)
      IF (IXCI.EQ.0) CALL ALCART('G',CARTDI,NOMA,'CINFDI')
C
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
      DO 220 I = 1 , 3
         CART(I)  = NOMU//'.CARDISC'//KMA(I)
         TMPND(I) = CART(I)//'.NCMP'
         TMPVD(I) = CART(I)//'.VALV'
C        SI LES CARTES N'EXISTENT PAS ON LES CREES
         CALL JEEXIN(TMPND(I),IXCKMA)
         IF (IXCKMA .EQ. 0) THEN
            CALL ALCART('G',CART(I),NOMA,'CADIS'//KMA(I))
         ENDIF
         CALL JEVEUO(TMPND(I),'E',JDC(I))
         CALL JEVEUO(TMPVD(I),'E',JDV(I))
220   CONTINUE
C

C     RECUPERATION DU NIVEAU D'IMPRESSION
C     -----------------------------------
      CALL INFNIV(IBID,NIV)
      IR = 0
C --- BOUCLE SUR LES OCCURRENCES DE DISCRET
      DO 30 IOC = 1 , NBOCC
         ETA = 0.0D0
C        PAR DEFAUT ON EST DANS LE REPERE GLOBAL, MATRICES SYMETRIQUES
         IREP = 1
         ISYM = 1
         REP = REPDIS(1)
C
         CALL GETVEM(NOMA,'GROUP_MA','RIGI_PARASOL','GROUP_MA',
     &               IOC,1,LMAX,ZK8(JDLS),NG)
         CALL GETVTX('RIGI_PARASOL','CARA'    ,IOC,1,NBCAR,CAR,NCAR)
         CALL GETVR8('RIGI_PARASOL','VALE'    ,IOC,1,NBVAL,VAL,NVAL)
         CALL GETVTX('RIGI_PARASOL','REPERE'  ,IOC,1,1,REP,NREP)
         CALL GETVTX('RIGI_PARASOL','GROUP_MA_POI1',IOC,1,1,NOGP,NGP)
         IF ( NGP .EQ. 0 ) THEN
            CALL GETVTX('RIGI_PARASOL','GROUP_MA_SEG2',IOC,1,1,NOGP,NGP)
         ENDIF
         IF ( NREP .NE. 0) THEN
            DO 32 I = 1 , NRD
               IF (REP.EQ.REPDIS(I)) IREP = I
 32         CONTINUE
         ENDIF
C        POUR EUROPLEXUS
         LBID = .FALSE.
         CALL GETVTX('RIGI_PARASOL','EUROPLEXUS',1,1,1,K8BID,IBID)
         IF ( IBID .NE. 0 ) THEN
            LBID = ( K8BID(1:3) .EQ. 'OUI' )
         ENDIF
         IF ( LBID .NEQV. EURPLX ) THEN
            CALL U2MESI('F','MODELISA9_93',1,IOC)
         ENDIF
C        UNITE POUR IMPRIMER LES VALEUR DES DISCRETS
         CALL GETVIS('RIGI_PARASOL','UNITE',IOC,1,1,IBID,IER)
         IUNITE = -1
         IF ( IER .NE. 0 ) THEN
            IUNITE = IBID
         ENDIF
C
         IF (NCAR.GT.0) NCARAC = NCAR
         IF ( IUNITE .GT. 0) THEN
            WRITE(IUNITE,1000) REP,IOC
         ENDIF
C
C ---    "GROUP_MA" = TOUTES LES MAILLES DE TOUS LES GROUPES DE MAILLES
         IF (NG.LE.0) GOTO 30

         II = 0
         DO 34 NC = 1,NCARAC
            IF ( (NC.EQ.2) .AND. (CAR(1)(1:1).EQ.CAR(2)(1:1)) )
     &         CALL U2MESS('F','MODELISA_16')
C           DISCRETS SEULEMENT EN TRANSLATION
            TRANSL = (CAR(NC)(1:7) .EQ. 'K_T_D_N').OR.
     &               (CAR(NC)(1:7) .EQ. 'K_T_D_L').OR.
     &               (CAR(NC)(1:7) .EQ. 'A_T_D_N').OR.
     &               (CAR(NC)(1:7) .EQ. 'A_T_D_L')
C           DISCRETS EN TRANSLATION ET ROTATION
            TRAROT = (CAR(NC)(1:8) .EQ. 'K_TR_D_N').OR.
     &               (CAR(NC)(1:8) .EQ. 'K_TR_D_L').OR.
     &               (CAR(NC)(1:8) .EQ. 'A_TR_D_N').OR.
     &               (CAR(NC)(1:8) .EQ. 'A_TR_D_L')
C
            IF (TRANSL .EQV. TRAROT) THEN
               CALL U2MESK('F','MODELISA_17',1,CAR(NC))
            ENDIF
C
            IF ( TRANSL ) THEN
               DO 132 J = 1,3
                  VALE(J) = VAL(II+J)
132            CONTINUE
               CALL RAIREP(NOMA,IOC,CAR(NC),VALE,NG,ZK8(JDLS),NBNO,
     &               ZK8(ITBNO),ZR(IRGNO),ZR(IRGTO),ZR(IAMTO),RIROT)
               II = II + 3
            ENDIF
C
            IF ( TRAROT ) THEN
               DO 131 J = 1,6
                  VALE(J) = VAL(II+J)
131            CONTINUE
               CALL RAIREP(NOMA,IOC,CAR(NC),VALE,NG,ZK8(JDLS),NBNO,
     &               ZK8(ITBNO),ZR(IRGNO),ZR(IRGTO),ZR(IAMTO),RIROT)
               II = II + 6
            ENDIF
C
            IF (IXNW.NE.0.AND.NGP.EQ.0) THEN
               DO 39 I = 1,NBNO
                  ITROU = 0
                  DO 100 K = 1 , NBMTRD
                     NUMNOE = ZI(JDNW+K*2-2)
                     CALL JENUNO(JEXNUM(MLGNNO,NUMNOE),NOMNOE)
                     IF (ZK8(ITBNO+I-1).EQ.NOMNOE) THEN
                        ITROU = 1
                        GOTO 101
                     ENDIF
 100              CONTINUE
 101              CONTINUE
                  IF (ITROU.EQ.0) THEN
                     CALL U2MESK('F','MODELISA_18',1,ZK8(ITBNO+I-1))
                  ENDIF
 39            CONTINUE
            ELSEIF (IXNW.EQ.0.AND.NGP.EQ.0) THEN
               CALL U2MESS('F','MODELISA_19')
            ENDIF
            IF (NGP.NE.0) THEN
               NBNOEU = 0
               LOKM   = 0
               IF ( TRANSL ) LOKM = 7
               IF ( TRAROT ) LOKM = 8
               IF ( CAR(NC)(LOKM:LOKM) .EQ. 'N' ) NBNOEU = 1
               IF ( CAR(NC)(LOKM:LOKM) .EQ. 'L' ) NBNOEU = 2
               CALL ASSERT( (NBNOEU.GT.0).AND.(LOKM.GT.0) )

               CALL JELIRA(JEXNOM(NOMA//'.GROUPEMA',NOGP),'LONMAX',
     &                     NMA,K8BID)
               CALL JEVEUO(JEXNOM(NOMA//'.GROUPEMA',NOGP),'L',LDGM)
               DO 22 IN = 0,NMA-1
C                 RECUPERE LE NOMBRE DE NOEUD DE LA MAILLE
                  CALL JELIRA(JEXNUM(NOMA//'.CONNEX',ZI(LDGM+IN)),
     &                        'LONMAX',NBNMA,K8BID)
                  CALL JEVEUO(JEXNUM(NOMA//'.CONNEX',ZI(LDGM+IN)),
     &                        'L',LDNM)
                  CALL JENUNO(JEXNUM(MLGNMA,ZI(LDGM+IN)),NOMMAI)
C                 BOUCLE SUR LE NB DE NOEUD DE LA MAILLE
                  IF ( NBNMA .NE. NBNOEU) THEN
                     CALL U2MESK('F','MODELISA_20',1,NOMMAI)
                  ENDIF
                  DO 25 INBN = 1 , NBNMA
                     INOE = ZI(LDNM+INBN-1)
                     CALL JENUNO(JEXNUM(MLGNNO,INOE),NOMNOE)
                     DO 24 INO = 1, NBNO
                        IF (ZK8(ITBNO+INO-1) .EQ. NOMNOE) THEN
                           ZK8(ITBMP+INO-1) = NOMMAI
                           GOTO 22
                        ENDIF
 24                  CONTINUE
 25               CONTINUE
C                 SI ON PASSE ICI AUCUN DES NOEUDS DU DISCRET APPARTIENT
C                 A LA SURFACE, ET CE N'EST PAS NORMAL
                  WRITE(IFM,*)'GROUP_MA :',(' '//ZK8(JDLS+II-1),II=1,NG)
                  CALL U2MESK('F','MODELISA_21',1,NOMNOE)
 22            CONTINUE
C              PREPARATION DES IMPRESSIONS DANS LE FICHIER MESSAGE
C               IFR = IUNIFI('RESULTAT')
               IF ( IREP .EQ. 1) THEN
                  LOREP  = 6
               ELSE
                  LOREP  = 5
               ENDIF
               IF ( IUNITE .GT. 0 ) THEN
                  IF ( TRANSL ) THEN
                     WRITE(IUNITE,1005) CAR(NC)(1:LOKM)
                  ELSE
                     WRITE(IUNITE,1006) CAR(NC)(1:LOKM),
     &                               RIROT(1),RIROT(2),RIROT(3)
                  ENDIF
               ENDIF
C
               IF ( IUNITE .GT. 0 ) THEN
                  DO 27 I = 1,NBNO
                     IV = 1
                     JD = ITBMP + I - 1
                     JN = ITBNO + I - 1
                     IF ( NBNOEU .EQ. 1 ) THEN
                        IF ( TRANSL ) THEN
                           WRITE(IUNITE,1010) 'NOEUD',ZK8(JN),
     &                           CAR(NC)(1:LOKM),
     &                           (ZR(IRGNO+6*I-6+JJ),JJ=0,2),
     &                           REPDIS(IREP)(1:LOREP)
                        ELSE
                           WRITE(IUNITE,1011) 'NOEUD',ZK8(JN),
     &                           CAR(NC)(1:LOKM),
     &                           (ZR(IRGNO+6*I-6+JJ),JJ=0,5),
     &                           REPDIS(IREP)(1:LOREP)
                        ENDIF
                     ELSE
                        IF ( TRANSL ) THEN
                           WRITE(IUNITE,1010) 'MAILLE',ZK8(JD),
     &                           CAR(NC)(1:LOKM),
     &                           (ZR(IRGNO+6*I-6+JJ),JJ=0,2),
     &                           REPDIS(IREP)(1:LOREP)
                        ELSE
                           WRITE(IUNITE,1011) 'MAILLE',ZK8(JD),
     &                           CAR(NC)(1:LOKM),
     &                           (ZR(IRGNO+6*I-6+JJ),JJ=0,5),
     &                           REPDIS(IREP)(1:LOREP)
                        ENDIF
                     ENDIF
 27               CONTINUE
               ENDIF

               DO 28 I = 1,NBNO
                  IV = 1
                  JD = ITBMP + I - 1
                  JN = ITBNO + I - 1
C                 POUR EUROPLEXUS PREPARATION DE L'ATTRIBUT PYTHON
                  IF ( EURPLX ) THEN
                     IF ( NBNOEU .EQ. 1 ) THEN
                        IF (TRANSL) THEN
                           DO 666 JJ=0,2
                              ZR(IREPV+6*IR+JJ)=ZR(IRGNO+6*I-6+JJ)
                              ZR(IREPV+6*IR+3+JJ)=0.D0
666                        CONTINUE
                        ELSE
                           DO 667 JJ=0,5
                              ZR(IREPV+6*IR+JJ)=ZR(IRGNO+6*I-6+JJ)
667                        CONTINUE
                        ENDIF
                        ZK8(IREPN+IR) = ZK8(JN)
                        IR = IR + 1
                     ELSE
                        CALL U2MESK('A','MODELISA9_96',1,ZK8(JD))
                     ENDIF
                  ENDIF
C
                  CALL AFFDIS(NDIM,IREP,ETA,CAR(NC),ZR(IRGNO+6*I-6),
     &                        JDC,JDV,IVR,IV,KMA,NCMP,L,
     &                        JDCINF,JDVINF,ISYM,IFM)
                  CALL NOCART(CARTDI, 3,' ','NOM',1,ZK8(JD),0,' ',7)
                  CALL NOCART(CART(L),3,' ','NOM',1,ZK8(JD),0,' ',NCMP)
 28            CONTINUE
            ELSE
               LOKM   = 0
               IF ( TRANSL ) LOKM = 7
               IF ( TRAROT ) LOKM = 8
               IF ( IREP .EQ. 1) THEN
                  LOREP  = 6
               ELSE
                  LOREP  = 5
               ENDIF
               IF ( IUNITE .GT. 0 ) THEN
                  IF ( TRANSL ) THEN
                     WRITE(IUNITE,1005) CAR(NC)(1:LOKM)
                  ELSE
                     WRITE(IUNITE,1006) CAR(NC)(1:LOKM),
     &                               RIROT(1),RIROT(2),RIROT(3)
                  ENDIF
                  DO 35 I = 1,NBNO
                     IV = 1
                     JD = ITBNO + I - 1
                     IF ( TRANSL ) THEN
                        WRITE(IUNITE,1010) 'NOEUD',ZK8(JD),
     &                        CAR(NC)(1:LOKM),
     &                        (ZR(IRGNO+6*I-6+JJ),JJ=0,2),
     &                        REPDIS(IREP)(1:LOREP)
                     ELSE
                        WRITE(IUNITE,1011) 'NOEUD',ZK8(JD),
     &                        CAR(NC)(1:LOKM),
     &                        (ZR(IRGNO+6*I-6+JJ),JJ=0,5),
     &                        REPDIS(IREP)(1:LOREP)
                     ENDIF
 35               CONTINUE
               ENDIF
               DO 36 I = 1,NBNO
                  IV = 1
                  JD = ITBNO + I - 1
                  CALL CRLINU('NOM', MLGNNO, 1, IBID, ZK8(JD),
     &                        NBMTRD, ZI(JDNW), ZI(JDDI), KK )
                  CALL AFFDIS(NDIM,IREP,ETA,CAR(NC),ZR(IRGNO+6*I-6),
     &                        JDC,JDV,IVR,IV,KMA,NCMP,L,
     &                        JDCINF,JDVINF,ISYM,IFM)
                  CALL NOCART(CARTDI, -3,' ','NUM',KK,' ',ZI(JDDI),
     &                        LIGMO,7)
                  CALL NOCART(CART(L),-3,' ','NUM',KK,' ',ZI(JDDI),
     &                        LIGMO,NCMP)
 36            CONTINUE
            ENDIF
 34      CONTINUE
 30   CONTINUE

      IF (IXNW.NE.0) CALL JEDETR(TMPDIS)
      CALL JEDETR('&&TMPDISCRET')
      CALL JEDETR('&&TMPTABNO')
      CALL JEDETR('&&TMPRIGNO')
      CALL JEDETR('&&TMPRIGTO')
      CALL JEDETR('&&TMPAMOTO')
      CALL JEDETR('&&TMPTABMP')
      CALL GETFAC('RIGI_MISS_3D',NBORM)
      IF (NBORM.EQ.0) THEN
         DO 240 I = 1 , 3
            CALL JEDETR(TMPND(I))
            CALL JEDETR(TMPVD(I))
240      CONTINUE
         CALL JEDETR(TMCINF)
         CALL JEDETR(TMVINF)
      ENDIF
C
      CALL JEDEMA()

 1000 FORMAT(/,
     &    ' <DISCRET> MATRICES AFFECTEES AUX ELEMENTS DISCRET ',
     &    '(REPERE ',A6,'), OCCURRENCE ',I4)
 1005 FORMAT(/,' PAS DE REPARTITION EN ROTATION POUR DES ',A,/)
 1006 FORMAT(/,' RAIDEURS DE ROTATION A REPARTIR POUR DES ',A,/
     &        ,'  KRX: ',1PE12.5,' KRY: ',1PE12.5,' KRZ: ',1PE12.5,/)
 1010 FORMAT(' _F(',A,'=''',A8,''', CARA=''',A,''',',/,
     &       '    VALE=(',3(1X,1PE12.5,','),'),',/,
     &       '    REPERE=''',A,'''),')

 1011 FORMAT(' _F(',A,'=''',A8,''', CARA=''',A,''',',/,
     &       '    VALE=(',3(1X,1PE12.5,','),/,
     &       '          ',3(1X,1PE12.5,','),'),',/,
     &       '    REPERE=''',A,'''),')

      END
