      SUBROUTINE ACEACO ( NOMU,NOMA,LMAX,NBCAGR,NBOCC)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER             LMAX,NBOCC,NBCAGR
      CHARACTER*8         NOMU,NOMA
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 23/05/2006   AUTEUR CIBHHPD L.SALMONA 
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
C     AFFECTATION DES CARACTERISTIQUES POUR L'ELEMENT COQUE
C ----------------------------------------------------------------------
C IN  : NOMU   : NOM UTILISATEUR DE LA COMMANDE
C IN  : NOMA   : NOM DU MAILLAGE
C IN  : LMAX   : LONGUEUR
C IN  : NOCACO : NOMBRE
C IN  : NMTGCO : NOMBRE
C IN  : NBOCC  : NOMBRE D'OCCURENCES DU MOT CLE COQUE
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
      CHARACTER*32     JEXNUM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      REAL*8       ANG(2), CRB, EPA, KAPPA, CORREC, RIGI, EXCENT
      CHARACTER*8  INERT, KORREC
      CHARACTER*19 CARTCO
      CHARACTER*24 TMPNCO, TMPVCO
C     ------------------------------------------------------------------
C
C --- CONSTRUCTION DES CARTES ET ALLOCATION
      CALL JEMARQ()
      CARTCO = NOMU//'.CARCOQUE'
      TMPNCO = CARTCO//'.NCMP'
      TMPVCO = CARTCO//'.VALV'
C
      CALL ALCART('G',CARTCO,NOMA,'CACOQU')
      CALL JEVEUO(TMPNCO,'E',JDCC)
      CALL JEVEUO(TMPVCO,'E',JDVC)
C
      CALL WKVECT('&&TMPCOQUE','V V K8',LMAX,JDLS)
C
      ZK8(JDCC)   = 'EP'
      ZK8(JDCC+1) = 'ALPHA'
      ZK8(JDCC+2) = 'BETA'
      ZK8(JDCC+3) = 'KAPPA'
      ZK8(JDCC+4) = 'C_METR'
      ZK8(JDCC+5) = 'CTOR'
      ZK8(JDCC+6) = 'EXCENT'
      ZK8(JDCC+7) = 'INERTIE'
C
C --- LECTURE DES VALEURS ET AFFECTATION DANS LA CARTE CARTCO
      DO 10 IOC = 1 , NBOCC
         ANG(1) = 0.D0
         ANG(2) = 0.D0
         CORREC = 0.D0
         KAPPA  = 0.D0
         EXCENT = 0.D0
         XINER  = 0.D0
         INERT = 'NON'
         CALL GETVEM(NOMA,'GROUP_MA','COQUE','GROUP_MA',
     +                                IOC,1,LMAX,ZK8(JDLS),NG)
         CALL GETVEM(NOMA,'MAILLE',  'COQUE','MAILLE',
     +                                IOC,1,LMAX,ZK8(JDLS),NM)
         CALL GETVR8('COQUE','EPAIS'        ,IOC,1,1   ,EPA      ,NV  )
         CALL GETVR8('COQUE','ANGL_REP'     ,IOC,1,2   ,ANG(1)   ,NA  )
         CALL GETVR8('COQUE','A_CIS'        ,IOC,1,1   ,KAPPA    ,NK  )
         CALL GETVTX('COQUE','MODI_METRIQUE',IOC,1,1   ,KORREC   ,NCO )
         CALL GETVR8('COQUE','COEF_RIGI_DRZ',IOC,1,1   ,RIGI     ,NCR )
         CALL GETVR8('COQUE','EXCENTREMENT' ,IOC,1,1   ,EXCENT   ,NEX )
         CALL GETVTX('COQUE','INER_ROTA'    ,IOC,1,1   ,INERT    ,NIN )
         ZR(JDVC)   = EPA
         ZR(JDVC+1) = ANG(1)
         ZR(JDVC+2) = -ANG(2)
         ZR(JDVC+3) = KAPPA
         IF (KORREC.EQ.'OUI') CORREC=1.D0
         ZR(JDVC+4) = CORREC
         ZR(JDVC+5) = RIGI
         ZR(JDVC+6) = EXCENT
         IF ( NEX.NE.0 .AND. NIN.EQ.0)  INERT = 'OUI'
         IF (INERT.EQ.'OUI') XINER=1.D0
         ZR(JDVC+7) = XINER
C
C ---    "GROUP_MA" = TOUTES LES MAILLES DE LA LISTE DE GROUPES MAILLES
         IF (NG.GT.0) THEN
            DO 20 I = 1 , NG
               CALL NOCART(CARTCO,2,ZK8(JDLS+I-1),' ',0,' ',0,' ',8)
 20         CONTINUE
         ENDIF
C
C ---    "MAILLE" = TOUTES LES MAILLES DE LA LISTE DE MAILLES
C
         IF (NM.GT.0) THEN
            CALL NOCART(CARTCO,3,' ','NOM',NM,ZK8(JDLS),0,' ',8)
         ENDIF
C
 10   CONTINUE
C
      CALL JEDETR('&&TMPCOQUE')
      IF ( NBCAGR .EQ. 0 ) THEN
         CALL JEDETR(TMPNCO)
         CALL JEDETR(TMPVCO)
      ENDIF
C
      CALL JEDEMA()
      END
