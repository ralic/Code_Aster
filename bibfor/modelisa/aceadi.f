      SUBROUTINE ACEADI(NOMA,NOMO,LMAX,NOCADI,NMTGDI,NBOCC,IVR,IFM)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER           LMAX,NOCADI(*),NMTGDI(*),NBOCC,IVR(*),IFM
      CHARACTER*8       NOMA,NOMO
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 03/08/2001   AUTEUR CIBHHLV L.VIVAN 
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
C IN  : NOCADI : NOMBRE
C IN  : NMTGDI : NOMBRE
C IN  : NBOCC  : NOMBRE D'OCCURENCES DU MOT CLE DISCRET
C IN  : IVR    : TABLEAU DES INDICES DE VERIFICATION
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
      PARAMETER    ( NBCAR = 100 , NBVAL = 1000 , NRD = 2 )
      INTEGER      JDC(3), JDV(3)
      REAL*8       VAL(NBVAL), ETA
      CHARACTER*1  KMA(3)
      CHARACTER*6  KI
      CHARACTER*8  K8B, NOMU, CAR(NBCAR)
      CHARACTER*16 SEC, REP, TOU, REPDIS(NRD), CONCEP, CMD
      CHARACTER*19 CARTDK, CARTDM, CARTDA, CART(3), LIGMO
      CHARACTER*24 TMPNDM, TMPVDM, TMPNDA, TMPVDA, TMPNDK, TMPVDK
      CHARACTER*24 TMPDIS, MLGGNO, MLGNNO
      CHARACTER*24 MODNEM
      CHARACTER*1 K1BID
      DATA REPDIS  /'GLOBAL          ','LOCAL           '/
      DATA KMA     /'K','M','A'/
C     ------------------------------------------------------------------
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
          CALL UTMESS('F','ACEADI','ON INTERDIT D''AVOIR SUR UN '
     +              //'MAILLAGE 2D DES ELEMENTS DISCRETS 2D ET 3D .')
      ENDIF
C
C --- ON INTERDIT SUR UN MAILLAGE 3D D'AVOIR DES ELEMENTS DISCRETS
C --- 2D
      IF (I2D.EQ.1.AND.NDIM1.EQ.3) THEN
          CALL UTMESS('F','ACEADI','ON INTERDIT D''AVOIR SUR UN '
     +              //'MAILLAGE 3D DES ELEMENTS DISCRETS 2D .')
      ENDIF
C
C --- DIMENSION DU PROBLEME
      IF (I3D.EQ.1) THEN
        NDIM = 3
      ELSEIF (I2D.EQ.1) THEN
        NDIM = 2
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
      CARTDM = NOMU//'.CARDISCM'
      CARTDA = NOMU//'.CARDISCA'
      CARTDK = NOMU//'.CARDISCK'
      CART(1) = CARTDK
      CART(2) = CARTDM
      CART(3) = CARTDA
      TMPNDM = CARTDM//'.NCMP'
      TMPVDM = CARTDM//'.VALV'
      TMPNDA = CARTDA//'.NCMP'
      TMPVDA = CARTDA//'.VALV'
      TMPNDK = CARTDK//'.NCMP'
      TMPVDK = CARTDK//'.VALV'
C
      DO 10 I = 1 , 3
         NOCADI(I) = NOCADI(I) + 2
 10   CONTINUE
C
      CALL ALCART('G',CART(1),NOMA,'CADISK',NOCADI(1),NMTGDI(1))
      CALL ALCART('G',CART(2),NOMA,'CADISM',NOCADI(2),NMTGDI(2))
      CALL ALCART('G',CART(3),NOMA,'CADISA',NOCADI(3),NMTGDI(3))
      CALL JEVEUO(TMPNDK,'E',JDC(1))
      CALL JEVEUO(TMPVDK,'E',JDV(1))
      CALL JEVEUO(TMPNDM,'E',JDC(2))
      CALL JEVEUO(TMPVDM,'E',JDV(2))
      CALL JEVEUO(TMPNDA,'E',JDC(3))
      CALL JEVEUO(TMPVDA,'E',JDV(3))
C
C --- AFFECTATION SYSTEMATIQUE DE VALEURS NULLES DANS LES CARTES
C        CARTDK,CARTDM,CARTDA POUR TOUTES LES MAILLES MAILLAGE+TARDIVES
C         AFIN DE POUVOIR CALCULER LES MATRICES K,M,A DANS TOUS LES CAS
      DO 20 I = 1 , 3
         DO 22 J = 1 , 78
            CALL CODENT(J,'G',KI)
            ZR(JDV(I)+J-1)  = 0.D0
            ZK8(JDC(I)+J-1) = KMA(I)//KI
 22      CONTINUE
         ZK8(JDC(I)+78)  = 'REP'//KMA(I)//'    '
         ZR (JDV(I)+78)  = 1.D0
         IF (I.EQ.1) THEN
            ZK8(JDC(I)+79)  = 'ETA     '
            ZR (JDV(I)+79)  = 0.D0
            CALL NOCART(CART(I),1,' ',' ',0,' ',0,' ',80)
            IF (IXNW.NE.0) THEN
               CALL NOCART(CART(I),-1,' ',' ',0,' ',0,LIGMO,80)
            ENDIF
         ELSE
            CALL NOCART(CART(I),1,' ',' ',0,' ',0,' ',79)
            IF (IXNW.NE.0) THEN
               CALL NOCART(CART(I),-1,' ',' ',0,' ',0,LIGMO,79)
            ENDIF
         ENDIF
 20   CONTINUE
C
C --- BOUCLE SUR LES OCCURENCES DE DISCRET
      DO 30 IOC = 1 , NBOCC
         ETA = 0.0D0
         DO 31 I = 1 , NBVAL
            VAL(I) = 0.0D0
 31      CONTINUE
         CALL GETVEM(NOMA,'GROUP_MA','DISCRET','GROUP_MA',
     +                                     IOC,1,LMAX,ZK8(JDLS),NG)
         CALL GETVEM(NOMA,'MAILLE'  ,'DISCRET','MAILLE',
     +                                     IOC,1,LMAX,ZK8(JDLS),NM)
         CALL GETVEM(NOMA,'GROUP_NO','DISCRET','GROUP_NO',
     +                                     IOC,1,LMAX,ZK8(JDLS),NJ)
         CALL GETVEM(NOMA,'NOEUD'   ,'DISCRET','NOEUD',
     +                                     IOC,1,LMAX,ZK8(JDLS),NN)
         CALL GETVTX('DISCRET','CARA'     ,IOC,1,NBCAR,CAR      ,NCAR)
         CALL GETVR8('DISCRET','VALE'     ,IOC,1,NBVAL,VAL      ,NVAL)
         CALL GETVTX('DISCRET','REPERE'   ,IOC,1,1    ,REP      ,NREP)
         CALL GETVR8('DISCRET','AMOR_HYST',IOC,1,1    ,ETA      ,NETA)
         IF (IOC.EQ.1 .AND. NREP.EQ.0) REP = REPDIS(1)
         DO 32 I = 1 , NRD
            IF (REP.EQ.REPDIS(I)) IREP = I
 32      CONTINUE
         IF (NCAR.GT.0) NCARAC = NCAR
         IF ( IVR(3) .EQ. 1 ) THEN
            WRITE(IFM,1000) REP,IOC
 1000       FORMAT(/,3X,
     +             '<DISCRET> MATRICES AFFECTEES AUX ELEMENTS DISCRET ',
     +                                '(REPERE ',A6,'), OCCURENCE ',I4)
         ENDIF
C
C ---    "GROUP_MA" = TOUTES LES MAILLES DE TOUS LES GROUPES DE MAILLES
         IF (NG.GT.0) THEN
           IV = 1
            DO 36 I = 1,NCARAC
              CALL AFFDIS(NDIM,IREP,ETA,CAR(I),VAL,JDC,JDV,IVR,IV,KMA,
     +                    NCMP,L,IFM)
               DO 38 II = 1 , NG
              CALL NOCART(CART(L),2,ZK8(JDLS+II-1),' ',0,' ',0,' ',NCMP)
 38            CONTINUE
 36         CONTINUE
         ENDIF
C
C ---   "MAILLE" = TOUTES LES MAILLES  DE LA LISTE DE MAILLES
         IF (NM.GT.0) THEN
            IV = 1
            DO 40 I = 1,NCARAC
              CALL AFFDIS(NDIM,IREP,ETA,CAR(I),VAL,JDC,JDV,IVR,IV,KMA,
     +                    NCMP,L,IFM)
               CALL NOCART(CART(L),3,' ','NOM',NM,ZK8(JDLS),0,' ',NCMP)
 40         CONTINUE
         ENDIF
C
C ---    SI DES MAILLES TARDIVES EXISTENT POUR CE MODELE :
         IF (IXNW.NE.0) THEN
C ---       "GROUP_NO" = TOUTES LES MAILLES TARDIVES  DE LA LISTE
C                                                  DE GROUPES DE NOEUDS
            IF (NJ.GT.0) THEN
               DO 42 I = 1 , NJ
                  CALL JEVEUO(JEXNOM(MLGGNO,ZK8(JDLS+I-1)),'L',JDGN)
                  CALL JELIRA(JEXNOM(MLGGNO,ZK8(JDLS+I-1)),'LONMAX',
     +                                                  NBNOGR,K1BID)
                  CALL CRLINU ( 'NUM', MLGNNO, NBNOGR, ZI(JDGN), K8B,
     +                           NBMTRD, ZI(JDNW), ZI(JDDI), KK )
                  IF (KK.GT.0) THEN
                     IV = 1
                     DO 44 II = 1,NCARAC
                        CALL AFFDIS(NDIM,IREP,ETA,CAR(II),VAL,JDC,JDV,
     +                              IVR,IV,KMA,NCMP,L,IFM)
                       CALL NOCART(CART(L),-3,' ','NUM',KK,' ',ZI(JDDI),
     +                                                       LIGMO,NCMP)
 44                  CONTINUE
                  ENDIF
 42            CONTINUE
            ENDIF
C ---       "NOEUD" = TOUTES LES MAILLES TARDIVES  DE LA LISTE DE NOEUDS
            IF (NN.GT.0) THEN
               CALL CRLINU ( 'NOM', MLGNNO, NN, IBID, ZK8(JDLS),
     +                        NBMTRD, ZI(JDNW), ZI(JDDI), KK )
               IF (KK.GT.0) THEN
                  IV = 1
                  DO 46 I = 1,NCARAC
                     CALL AFFDIS(NDIM,IREP,ETA,CAR(I),VAL,JDC,JDV,
     +                           IVR,IV,KMA,NCMP,L,IFM)
                     CALL NOCART(CART(L),-3,' ','NUM',KK,' ',ZI(JDDI),
     +                                                       LIGMO,NCMP)
 46               CONTINUE
               ENDIF
            ENDIF
         ENDIF
 30   CONTINUE
C
      IF (IXNW.NE.0) CALL JEDETR(TMPDIS)
      CALL JEDETR('&&TMPDISCRET')
      CALL GETFAC('RIGI_PARASOL',NBORP)
      IF (NBORP.EQ.0) THEN
         CALL JEDETR(TMPNDK)
         CALL JEDETR(TMPVDK)
         CALL JEDETR(TMPNDM)
         CALL JEDETR(TMPVDM)
         CALL JEDETR(TMPNDA)
         CALL JEDETR(TMPVDA)
      ENDIF
C
      CALL JEDEMA()
      END
