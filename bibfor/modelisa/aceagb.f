      SUBROUTINE ACEAGB ( NOMU, NOMA, LMAX, LOCACO, LOCAMB, NBOCC )
      IMPLICIT   NONE
      INCLUDE 'jeveux.h'
      INTEGER             LMAX, NBOCC
      LOGICAL             LOCACO, LOCAMB
      CHARACTER*8         NOMU, NOMA
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 04/09/2012   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     AFFECTATION DES CARACTERISTIQUES POUR LE MOT CLE "GRILLE"
C ----------------------------------------------------------------------
C IN  : NOMU   : NOM UTILISATEUR DE LA COMMANDE
C IN  : NOMA   : NOM DU MAILLAGE
C IN  : LMAX   : LONGUEUR
C IN  : NBOCC  : NOMBRE D'OCCURENCES DU MOT CLE GRILLE
      INTEGER      JDCC, JDVC, JDLS, IOC,  NG, NM, N1, N2, N3, N4, N5,
     &             I, AXYZM, NUNOE, NBMAT, IER, NBMA,
     &             IMA, NBNO, INO, JNUMA, ADRM, NUMA, JGRMA, IGR,NBMAT0
      REAL*8       ANG(2), SL,EZ,CTR, ORIG(3), Z, R8RDDG,
     &             AXEZ(3), XNORM, EPSI, AXER(3), PSCAL, AXET(3), X, Y
      CHARACTER*8  K8B
      CHARACTER*19 CARTGR
      CHARACTER*24 TMPNGR, TMPVGR, NOMAGR, NOMAMA, CONNEX
      INTEGER      IARG
C     ------------------------------------------------------------------
      CALL JEMARQ( )
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
      TMPNGR = CARTGR//'.NCMP'
      TMPVGR = CARTGR//'.VALV'
C
      IF (.NOT.LOCACO) THEN
         CALL ALCART('G',CARTGR,NOMA,'CACOQU')
      ENDIF

      CALL JEVEUO(TMPNGR,'E',JDCC)
      CALL JEVEUO(TMPVGR,'E',JDVC)
      EPSI = 1.0D-6
C
      CALL WKVECT('&&TMPGRILLE','V V K8',LMAX,JDLS)
C
      ZK8(JDCC  ) = 'SECT_L'
      ZK8(JDCC+1) = 'ALPHA'
      ZK8(JDCC+2) = 'BETA'
      ZK8(JDCC+3) = 'DIST_N'
      ZK8(JDCC+4) = 'CTOR'
C
C
C --- LECTURE DES VALEURS ET AFFECTATION DANS LA CARTE CARTPF
      DO 10 IOC = 1 , NBOCC
        ANG(1) = 0.0D0
        ANG(2) = 0.0D0
        SL     = 0.0D0
        EZ     = 0.0D0
        CTR    = 1.D-10
C
        CALL GETVEM(NOMA,'GROUP_MA','GRILLE','GROUP_MA',
     &           IOC,IARG,LMAX,ZK8(JDLS),NG)
        CALL GETVEM(NOMA,'MAILLE','GRILLE','MAILLE',
     &         IOC,IARG,LMAX,ZK8(JDLS),NM)
C
        CALL GETVR8('GRILLE','SECTION'      ,IOC,IARG,1   ,SL       ,N1)
        CALL GETVR8('GRILLE','ANGL_REP'     ,IOC,IARG,2   ,ANG      ,N2)
        CALL GETVR8('GRILLE','EXCENTREMENT' ,IOC,IARG,1   ,EZ       ,N3)
        CALL GETVR8('GRILLE','COEF_RIGI_DRZ',IOC,IARG,1   ,CTR      ,N4)
        CALL GETVR8('GRILLE','ORIG_AXE'     ,IOC,IARG,0   ,ORIG     ,N5)
C
        ZR(JDVC  ) = SL
        ZR(JDVC+1) = ANG(1)
        ZR(JDVC+2) = ANG(2)
        ZR(JDVC+3) = EZ
        ZR(JDVC+4) = CTR
C
        IF ( N5 .EQ. 0 ) THEN
C
C ---     "GROUP_MA" = TOUTES LES MAILLES DE LA LISTE DE GROUPES MAILLES
           IF (NG.GT.0) THEN
              DO 20 I = 1 , NG
                 CALL NOCART(CARTGR,2,ZK8(JDLS+I-1),' ',0,' ',0,' ',5)
 20           CONTINUE
           ENDIF
C
C ---     "MAILLE" = TOUTES LES MAILLES DE LA LISTE DE MAILLES
           IF (NM.GT.0) THEN
             CALL NOCART(CARTGR,3,' ','NOM',NM,ZK8(JDLS),0,' ',5)
           ENDIF
C
        ELSE
C
           CALL GETVR8 ( 'GRILLE', 'ORIG_AXE', IOC,IARG,3 ,ORIG, N5 )
           CALL GETVR8 ( 'GRILLE', 'AXE'     , IOC,IARG,3 ,AXEZ, N5 )
C
           IF (NG.GT.0) THEN
             NBMAT = 0
             NUMA = -1
             DO 120 IGR = 0 , NG-1
             CALL JELIRA(JEXNOM(NOMAGR,ZK8(JDLS+IGR)),'LONMAX',NBMA,K8B)
               NBMAT = NBMAT + NBMA
               CALL JEVEUO(JEXNOM(NOMAGR,ZK8(JDLS+IGR)),'L',JGRMA)
               DO 122 IMA = 0 , NBMA-1
                 NUMA = NUMA + 1
                 ZI(JNUMA+NUMA) = ZI(JGRMA+IMA)
 122           CONTINUE
 120         CONTINUE
           ELSE
             DO 130 IMA = 0 , NM-1
               CALL JENONU(JEXNOM(NOMAMA,ZK8(JDLS+IMA)),ZI(JNUMA+IMA))
 130         CONTINUE
           ENDIF
C
           XNORM = 0.0D0
           DO 40 I = 1,3
              XNORM = XNORM + AXEZ(I)*AXEZ(I)
 40        CONTINUE
           IF ( XNORM .LT. EPSI ) THEN
              CALL U2MESS('F','MODELISA_10')
           ENDIF
           XNORM =  1.0D0 / SQRT( XNORM )
           DO 42 I = 1,3
              AXEZ(I) = AXEZ(I) * XNORM
 42        CONTINUE
C
           DO 200 IMA = 1 , NBMAT
             NUMA = ZI(JNUMA+IMA-1)
             CALL JELIRA (JEXNUM(CONNEX,NUMA),'LONMAX',NBNO,K8B)
             CALL JEVEUO (JEXNUM(CONNEX,NUMA),'L',ADRM)
             X = 0.D0
             Y = 0.D0
             Z = 0.D0
             DO 202 INO = 1 , NBNO
                NUNOE = ZI(ADRM+INO-1)
                X = X + ZR(AXYZM+3*(NUNOE-1)  )
                Y = Y + ZR(AXYZM+3*(NUNOE-1)+1)
                Z = Z + ZR(AXYZM+3*(NUNOE-1)+2)
 202         CONTINUE
             AXER(1) = ( X / NBNO ) - ORIG(1)
             AXER(2) = ( Y / NBNO ) - ORIG(2)
             AXER(3) = ( Z / NBNO ) - ORIG(3)
             PSCAL = AXER(1)*AXEZ(1)+AXER(2)*AXEZ(2)+AXER(3)*AXEZ(3)
             AXER(1) = AXER(1) - PSCAL*AXEZ(1)
             AXER(2) = AXER(2) - PSCAL*AXEZ(2)
             AXER(3) = AXER(3) - PSCAL*AXEZ(3)
             XNORM = 0.0D0
             DO 44 I = 1,3
               XNORM = XNORM + AXER(I)*AXER(I)
 44          CONTINUE
             IF ( XNORM .LT. EPSI ) THEN
              CALL U2MESS('F','MODELISA_11')
             ENDIF
             XNORM =  1.0D0 / SQRT( XNORM )
             DO 46 I = 1,3
               AXER(I) = AXER(I) * XNORM
 46          CONTINUE
             AXET(1) = AXEZ(2)*AXER(3) - AXEZ(3)*AXER(2)
             AXET(2) = AXEZ(3)*AXER(1) - AXEZ(1)*AXER(3)
             AXET(3) = AXEZ(1)*AXER(2) - AXEZ(2)*AXER(1)
             DO 48 I = 1,3
               XNORM = XNORM + AXET(I)*AXET(I)
 48          CONTINUE
             XNORM =  SQRT( XNORM )
             IF ( XNORM .LT. EPSI ) THEN
               CALL U2MESS('F','MODELISA9_80')
             ENDIF
             CALL ANGVX ( AXET, ANG(1), ANG(2) )
C
             ZR(JDVC+1) = ANG(1) * R8RDDG()
             ZR(JDVC+2) = ANG(2) * R8RDDG()
C
             CALL NOCART(CARTGR,3,' ','NUM',1,K8B,NUMA,' ',5)
C
 200       CONTINUE
        ENDIF
C
 10   CONTINUE
C
      CALL JEDETR ('&&ACEAGB.NUME_MA' )
      CALL JEDETR('&&TMPGRILLE')
      IF (.NOT.LOCAMB) THEN
         CALL JEDETR(TMPNGR)
         CALL JEDETR(TMPVGR)
      ENDIF
C
      CALL JEDEMA()
      END
