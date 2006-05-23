      SUBROUTINE ACEAMA(NOMU,NOMA,LMAX,NBOCC)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER           LMAX,NBOCC
      CHARACTER*8       NOMU,NOMA
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
      REAL*8       ANG(3), ORIG(3)
      CHARACTER*16 TOU
      CHARACTER*19 CARTMA
      CHARACTER*24 TMPNMA, TMPVMA
C     ------------------------------------------------------------------
C
C --- CONSTRUCTION DES CARTES ET ALLOCATION
      CALL JEMARQ()
      CARTMA = NOMU//'.CARMASSI'
      TMPNMA = CARTMA//'.NCMP'
      TMPVMA = CARTMA//'.VALV'
C
      CALL ALCART('G',CARTMA,NOMA,'CAMASS')
      CALL JEVEUO(TMPNMA,'E',JDCC)
      CALL JEVEUO(TMPVMA,'E',JDVC)
C
      CALL WKVECT('&&TMPMASSIF','V V K8',LMAX,JDLS)
C
C     STOCKAGE DE VALEURS NULLES SUR TOUT LE MAILLAGE
C
      ZK8(JDCC) = 'C'
      ZK8(JDCC+1) = 'ALPHA'
      ZK8(JDCC+2) = 'BETA'
      ZK8(JDCC+3) = 'KAPPA'
      ZK8(JDCC+4) = 'X'
      ZK8(JDCC+5) = 'Y'
      ZK8(JDCC+6) = 'Z'
C
      ZR(JDVC  ) = 1.D0
      ZR(JDVC+1) = 0.D0
      ZR(JDVC+2) = 0.D0
      ZR(JDVC+3) = 0.D0
      ZR(JDVC+4) = 0.D0
      ZR(JDVC+5) = 0.D0
      ZR(JDVC+6) = 0.D0
C
      CALL NOCART ( CARTMA, 1, ' ', 'NOM', 0, ' ', 0, ' ', 7 )
C
C --- LECTURE DES VALEURS ET AFFECTATION DANS LA CARTE CARTMA
      DO 10 IOC = 1 , NBOCC
         ANG(1)  = 0.D0
         ANG(2)  = 0.D0
         ANG(3)  = 0.D0
         ORIG(1) = 0.D0
         ORIG(2) = 0.D0
         ORIG(3) = 0.D0
         CALL GETVEM(NOMA,'GROUP_MA','MASSIF','GROUP_MA',
     +                                IOC,1,LMAX,ZK8(JDLS),NG)
         CALL GETVEM(NOMA,'MAILLE'  ,'MASSIF','MAILLE',
     +                                IOC,1,LMAX,ZK8(JDLS),NM)
         CALL GETVR8('MASSIF','ANGL_REP'    ,IOC,1,3   ,ANG(1)   ,NREP)
         CALL GETVR8('MASSIF','ANGL_AXE'    ,IOC,1,2   ,ANG(1)   ,NAXE)
         CALL GETVR8('MASSIF','ORIG_AXE'    ,IOC,1,3   ,ORIG(1)  ,NORIG)
C
         IF (NREP.NE.0) THEN
             ZR(JDVC  ) = 1.D0
             ZR(JDVC+1) = ANG(1)
             ZR(JDVC+2) = ANG(2)
             ZR(JDVC+3) = ANG(3)
             ZR(JDVC+4) = 0.D0
             ZR(JDVC+5) = 0.D0
             ZR(JDVC+6) = 0.D0
           ELSE
             ZR(JDVC  ) = -1.D0
             ZR(JDVC+1) = ANG(1)
             ZR(JDVC+2) = ANG(2)
             ZR(JDVC+3) = ANG(2)
             ZR(JDVC+4) = ORIG(1)
             ZR(JDVC+5) = ORIG(2)
             ZR(JDVC+6) = ORIG(3)
         ENDIF
C
C ---    "GROUP_MA" = TOUTES LES MAILLES DE LA LISTE DE GROUPES MAILLES
         IF (NG.GT.0) THEN
            DO 20 I = 1 , NG
               CALL NOCART(CARTMA,2,ZK8(JDLS+I-1),' ',0,' ',0,' ',7)
 20         CONTINUE
         ENDIF
C
C ---    "MAILLE" = TOUTES LES MAILLES DE LA LISTE DE MAILLES
C
         IF (NM.GT.0) THEN
            CALL NOCART(CARTMA,3,' ','NOM',NM,ZK8(JDLS),0,' ',7)
         ENDIF
C
 10   CONTINUE
C
      CALL JEDETR('&&TMPMASSIF')
      CALL JEDETR(TMPNMA)
      CALL JEDETR(TMPVMA)
C
      CALL JEDEMA()
      END
