      SUBROUTINE ACEAGB(NOMU, NOMA, LMAX, LOCAMB, NBOCC)
      IMPLICIT   NONE
      INCLUDE 'jeveux.h'
C
      CHARACTER*32   JEXNUM,JEXNOM
      INTEGER        LMAX, NBOCC
      LOGICAL        LOCAMB
      CHARACTER*8    NOMU, NOMA
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 21/01/2013   AUTEUR FLEJOU J-L.FLEJOU 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
C                          AFFE_CARA_ELEM
C
C     AFFECTATION DES CARACTERISTIQUES POUR LE MOT CLE "GRILLE"
C
C ----------------------------------------------------------------------
C  IN
C     NOMU   : NOM UTILISATEUR DE LA COMMANDE
C     NOMA   : NOM DU MAILLAGE
C     LMAX   : LONGUEUR
C     LOCAMB : SI ELEMENTS MEMBRANE DANS LA MODELISATION
C     NBOCC  : NOMBRE D'OCCURENCES DU MOT CLE GRILLE
C ----------------------------------------------------------------------
      INTEGER        JDCC,JDVC,JDLS,IOC,NG,NM,N1,N2,N3,N4,N5,JDLS2
      INTEGER        I, AXYZM, NBMAT, IER, NBMA, N1F, N3F, IRET
      INTEGER        IMA,NBNO, JNUMA, ADRM, NUMA, JGRMA, IGR,NBMAT0
      INTEGER        NOE1,NOE2,NOE3,IARG,JDCCF,JDVCF
      REAL*8         ANG(2), SL, EZ, CTR, R8RDDG, AXEY(3), XNORM, EPSI
      REAL*8         AXEX(3), VN1N2(3), VN1N3(3), VECNOR(3)
      CHARACTER*8    K8B, SLF, EZF
      CHARACTER*19   CARTGR, CARTCF
      CHARACTER*24   TMPNGR,TMPVGR,NOMAGR,NOMAMA,CONNEX,TMPNCF,TMPVCF
      CHARACTER*32   KJEXN
      LOGICAL        LCARTF
C     ------------------------------------------------------------------
      CALL JEMARQ()
C
      CALL JEVEUO ( NOMA//'.COORDO    .VALE', 'L', AXYZM )
      CALL DISMOI('F','NB_MA_MAILLA',NOMA,'MAILLAGE',NBMAT0,K8B,IER)
      CALL WKVECT ('&&ACEAGB.NUME_MA','V V I',NBMAT0,JNUMA )
      NOMAGR = NOMA//'.GROUPEMA'
      NOMAMA = NOMA//'.NOMMAI'
      CONNEX = NOMA//'.CONNEX'
C
C --- CONSTRUCTION DES CARTES ET ALLOCATION
      CARTGR = NOMU//'.CARCOQUE'
C     SI LA CARTE DE REELS N'EXISTE PAS
      CALL EXISD('CARTE',CARTGR,IRET)
      IF ( IRET.EQ.0 ) THEN
         CALL ALCART('G',CARTGR,NOMA,'CACOQU')
      ENDIF
      TMPNGR = CARTGR//'.NCMP'
      TMPVGR = CARTGR//'.VALV'
      CALL JEVEUO(TMPNGR,'E',JDCC)
      CALL JEVEUO(TMPVGR,'E',JDVC)
      EPSI = 1.0D-6
C     LES NOMS DES GRANDEURS REELLES
      ZK8(JDCC  ) = 'SECT_L'
      ZK8(JDCC+1) = 'ALPHA'
      ZK8(JDCC+2) = 'BETA'
      ZK8(JDCC+3) = 'DIST_N'
      ZK8(JDCC+4) = 'CTOR'
C
C     CARTE POUR LES FONCTIONS
      CARTCF = NOMU//'.CARCOQUF'
      CALL EXISD('CARTE',CARTCF,IRET)
      LCARTF = .FALSE.
      IF ( IRET.EQ.0 ) THEN
C ------ DOIT-ON CREER LA CARTE DE FONCTION
         DO 100 IOC = 1 , NBOCC
            CALL GETVID('GRILLE','SECTION_FO',     IOC,IARG,1,SLF,N1F)
            CALL GETVID('GRILLE','EXCENTREMENT_FO',IOC,IARG,1,EZF,N3F)
            IF ( N1F+N3F.NE.0 ) THEN
               LCARTF = .TRUE.
               GOTO 110
            ENDIF
100      CONTINUE
110      CONTINUE
C
C        CARTE POUR LES NOMS DES FONCTIONS
         IF ( LCARTF ) THEN
            CALL ALCART('V',CARTCF,NOMA,'CACOQUF')
         ENDIF
      ELSE
         LCARTF = .TRUE.
      ENDIF
C     SI LA CARTE EXISTE
      IF ( LCARTF ) THEN
         TMPNCF = CARTCF//'.NCMP'
         TMPVCF = CARTCF//'.VALV'
         CALL JEVEUO(TMPNCF,'E',JDCCF)
         CALL JEVEUO(TMPVCF,'E',JDVCF)
C        LES NOMS DES FONCTIONS
         ZK8(JDCCF)  = 'SECT_L'
         ZK8(JDCCF+1)= 'DIST_N'
      ENDIF
C
      CALL WKVECT('&&TMPGRILLE' ,'V V K24',LMAX,JDLS)
      CALL WKVECT('&&TMPGRILLE2','V V K8', LMAX,JDLS2)
C
C --- LECTURE DES VALEURS ET AFFECTATION DANS : CARTGR, CARTCF
      DO 10 IOC = 1 , NBOCC
         ANG(1) = 0.0D0
         ANG(2) = 0.0D0
         SL     = 0.0D0
         EZ     = 0.0D0
         CTR    = 1.D-10
C
         CALL GETVEM(NOMA,'GROUP_MA','GRILLE','GROUP_MA',
     &               IOC,IARG,LMAX,ZK24(JDLS),NG)
         CALL GETVEM(NOMA,'MAILLE','GRILLE','MAILLE',
     &               IOC,IARG,LMAX,ZK8(JDLS2),NM)
C
         CALL GETVR8('GRILLE','SECTION'        ,IOC,IARG,1,SL  ,N1 )
         CALL GETVID('GRILLE','SECTION_FO'     ,IOC,IARG,1,SLF ,N1F)
         CALL GETVR8('GRILLE','ANGL_REP'       ,IOC,IARG,2,ANG ,N2 )
         CALL GETVR8('GRILLE','EXCENTREMENT'   ,IOC,IARG,1,EZ  ,N3 )
         CALL GETVID('GRILLE','EXCENTREMENT_FO',IOC,IARG,1,EZF ,N3F)
         CALL GETVR8('GRILLE','COEF_RIGI_DRZ'  ,IOC,IARG,1,CTR ,N4 )
         CALL GETVR8('GRILLE','AXE'            ,IOC,IARG,3,AXEY,N5 )
C
         ZR(JDVC  ) = SL
         ZR(JDVC+1) = ANG(1)
         ZR(JDVC+2) = ANG(2)
         ZR(JDVC+3) = EZ
         ZR(JDVC+4) = CTR
C
         IF ( LCARTF ) THEN
            ZK8(JDVCF)   = '&&ACEAGB'
            ZK8(JDVCF+1) = '&&ACEAGB'
         ENDIF
         IF (N1F.NE.0) THEN
            ZR(JDVC  ) = 0.0D0
            IF ( LCARTF ) ZK8(JDVCF) = SLF
         ENDIF
         IF (N3F.NE.0) THEN
            ZR(JDVC+3) = 0.0D0
            IF ( LCARTF ) ZK8(JDVCF+1) = EZF
         ENDIF
C
         IF ( N5 .EQ. 0 ) THEN
C ---       "GROUP_MA" = TOUTES LES MAILLES DE LA LISTE
            IF (NG.GT.0) THEN
               DO 20 I = 1 , NG
                  CALL NOCART(CARTGR,2,ZK24(JDLS+I-1),' ',0,' ',
     &                        0,' ',5)
20             CONTINUE
               IF ( LCARTF ) THEN
                  DO 25 I = 1 , NG
                     CALL NOCART(CARTCF,2,ZK24(JDLS+I-1),' ',0,' ',
     &                           0,' ',2)
25                CONTINUE
               ENDIF
            ENDIF
C ---       "MAILLE" = TOUTES LES MAILLES DE LA LISTE DE MAILLES
            IF (NM.GT.0) THEN
               CALL NOCART(CARTGR,3,' ','NOM',NM,ZK8(JDLS2),0,' ',5)
               IF ( LCARTF ) THEN
                  CALL NOCART(CARTCF,3,' ','NOM',NM,ZK8(JDLS2),0,' ',2)
               ENDIF
            ENDIF
         ELSE
            IF (NG.GT.0) THEN
               NBMAT = 0
               NUMA = -1
               DO 120 IGR = 0 , NG-1
                  KJEXN = JEXNOM(NOMAGR,ZK24(JDLS+IGR))
                  CALL JELIRA(KJEXN,'LONMAX',NBMA,K8B)
                  CALL JEVEUO(KJEXN,'L',JGRMA)
                  NBMAT = NBMAT + NBMA
                  DO 122 IMA = 0 , NBMA-1
                     NUMA = NUMA + 1
                     ZI(JNUMA+NUMA) = ZI(JGRMA+IMA)
122               CONTINUE
120            CONTINUE
            ELSE
               NBMAT = NM
               DO 130 IMA = 0 , NM-1
                  KJEXN = JEXNOM(NOMAMA,ZK8(JDLS2+IMA))
                  CALL JENONU(KJEXN,ZI(JNUMA+IMA))
130            CONTINUE
            ENDIF
C
            CALL NORMEV(AXEY,XNORM)
            IF ( XNORM .LT. EPSI ) THEN
               CALL U2MESS('F','MODELISA_10')
            ENDIF
            DO 200 IMA = 1 , NBMAT
               NUMA  = ZI(JNUMA+IMA-1)
               KJEXN = JEXNUM(CONNEX,NUMA)
               CALL JELIRA(KJEXN,'LONMAX',NBNO,K8B)
               CALL JEVEUO(KJEXN,'L',ADRM)
C              CALCUL DE LA NORMALE : VECTEUR Z LOCAL
               NOE1 = ZI(ADRM+1-1)
               NOE2 = ZI(ADRM+2-1)
               NOE3 = ZI(ADRM+3-1)
               DO 202 I = 1 , 3
                  VN1N2(I)= ZR(AXYZM+3*(NOE2-1)+I-1)
     &                     -ZR(AXYZM+3*(NOE1-1)+I-1)
                  VN1N3(I)= ZR(AXYZM+3*(NOE3-1)+I-1)
     &                     -ZR(AXYZM+3*(NOE1-1)+I-1)
202            CONTINUE
               VECNOR(1) = VN1N2(2)*VN1N3(3) - VN1N2(3)*VN1N3(2)
               VECNOR(2) = VN1N2(3)*VN1N3(1) - VN1N2(1)*VN1N3(3)
               VECNOR(3) = VN1N2(1)*VN1N3(2) - VN1N2(2)*VN1N3(1)
               CALL NORMEV(VECNOR,XNORM)
C              CALCUL DE LA DIRECTION DES ARMATURES : XLOCAL
               AXEX(1) = AXEY(2)*VECNOR(3) - AXEY(3)*VECNOR(2)
               AXEX(2) = AXEY(3)*VECNOR(1) - AXEY(1)*VECNOR(3)
               AXEX(3) = AXEY(1)*VECNOR(2) - AXEY(2)*VECNOR(1)
               CALL NORMEV(AXEX,XNORM)
               IF ( XNORM .LT. EPSI ) THEN
                  CALL U2MESS('F','MODELISA_11')
               ENDIF
               CALL ANGVX ( AXEX, ANG(1), ANG(2) )
C
               ZR(JDVC+1) = ANG(1) * R8RDDG()
               ZR(JDVC+2) = ANG(2) * R8RDDG()
               CALL NOCART(CARTGR,3,' ','NUM',1,K8B,NUMA,' ',5)
               IF ( LCARTF ) THEN
                  CALL NOCART(CARTCF,3,' ','NUM',1,K8B,NUMA,' ',2)
               ENDIF
200         CONTINUE
         ENDIF
C
10    CONTINUE
C
      CALL JEDETR('&&ACEAGB.NUME_MA' )
      CALL JEDETR('&&TMPGRILLE')
      CALL JEDETR('&&TMPGRILLE2')
C     SI PAS MEMBRANE
      IF (.NOT.LOCAMB) THEN
         CALL JEDETR(TMPNGR)
         CALL JEDETR(TMPVGR)
         IF ( LCARTF ) THEN
            CALL JEDETR(TMPNCF)
            CALL JEDETR(TMPVCF)
         ENDIF
      ENDIF
C
      CALL JEDEMA()
      END
