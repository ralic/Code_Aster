      SUBROUTINE ACEAGB ( NOMU, NOMA, LMAX, NOCAGR, NMTGGR, NBCACO,
     +                    NBOCC )
      IMPLICIT   NONE
      INTEGER             LMAX, NOCAGR, NMTGGR, NBOCC, NBCACO
      CHARACTER*8         NOMU, NOMA
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 09/04/2002   AUTEUR CIBHHLV L.VIVAN 
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
C     AFFECTATION DES CARACTERISTIQUES POUR LE MOT CLE "GRILLE"
C ----------------------------------------------------------------------
C IN  : NOMU   : NOM UTILISATEUR DE LA COMMANDE
C IN  : NOMA   : NOM DU MAILLAGE
C IN  : LMAX   : LONGUEUR
C IN  : NOCAGR : NOMBRE
C IN  : NMTGGR : NOMBRE
C IN  : NBOCC  : NOMBRE D'OCCURENCES DU MOT CLE GRILLE
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
      CHARACTER*32     JEXNOM, JEXNUM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER      JDCC, JDVC, JDLS, IOC,  NG, NM, N1, N2, N3, N4, N5,
     +             N6, N6A, N7, N8, I, AXYZM, NUNOE, NBMAT, IER, NBMA,
     +             IMA, NBNO, INO, JNUMA, ADRM, NUMA, JGRMA, IGR,NBMAT0
      REAL*8       ANG(3), PCL, PCT, SL,EZ,EZA,CTR, ORIG(3), Z, R8RDDG,
     +             AXEZ(3), XNORM, EPSI, AXER(3), PSCAL, AXET(3), X, Y
      CHARACTER*8  K8B
      CHARACTER*16 TOU
      CHARACTER*19 CARTGR
      CHARACTER*24 TMPNGR, TMPVGR, NOMAGR, NOMAMA, CONNEX
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
      IF ( NBCACO .EQ. 0 ) THEN
         CALL ALCART('G',CARTGR,NOMA,'CACOQU',NOCAGR,NMTGGR)
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
      ZK8(JDCC+3) = 'ANGL_L'
      ZK8(JDCC+4) = 'P_CENT_L'
      ZK8(JDCC+5) = 'P_CENT_T'
      ZK8(JDCC+6) = 'DIST_N'
      ZK8(JDCC+7) = 'CTOR'
C
C
C --- LECTURE DES VALEURS ET AFFECTATION DANS LA CARTE CARTPF
      DO 10 IOC = 1 , NBOCC
        ANG(1) = 0.0D0
        ANG(2) = 0.0D0
        ANG(3) = 0.0D0
        PCL    = 1.0D0
        PCT    = 1.0D0
        SL     = 0.0D0
        EZ     = 0.0D0
        CTR    = 1.D-10
C
        CALL GETVEM(NOMA,'GROUP_MA','GRILLE','GROUP_MA',
     +           IOC,1,LMAX,ZK8(JDLS),NG)
        CALL GETVEM(NOMA,'MAILLE','GRILLE','MAILLE',
     +         IOC,1,LMAX,ZK8(JDLS),NM)
C
        CALL GETVR8('GRILLE','SECTION_L'    ,IOC,1,1   ,SL       ,N1)
        CALL GETVR8('GRILLE','ANGL_REP'     ,IOC,1,2   ,ANG      ,N2)
        CALL GETVR8('GRILLE','ANGL_L'       ,IOC,1,1   ,ANG(3)   ,N3)
        CALL GETVR8('GRILLE','POUR_CENT_L'  ,IOC,1,1   ,PCL      ,N4)
        CALL GETVR8('GRILLE','POUR_CENT_T'  ,IOC,1,1   ,PCT      ,N5)
        CALL GETVR8('GRILLE','EXCENTREMENT' ,IOC,1,1   ,EZ       ,N6)
        CALL GETVR8('GRILLE','COEF_RIGI_DRZ',IOC,1,1   ,CTR      ,N7)
        CALL GETVR8('GRILLE','ORIG_AXE'     ,IOC,1,0   ,ORIG     ,N8)
C
        ZR(JDVC  ) = SL
        ZR(JDVC+1) = ANG(1)
        ZR(JDVC+2) = ANG(2)
        ZR(JDVC+3) = ANG(3)
C
        ZR(JDVC+4) = PCL
        ZR(JDVC+5) = PCT
        ZR(JDVC+6) = EZ
        ZR(JDVC+7) = CTR
C
        IF ( N8 .EQ. 0 ) THEN
C
C ---     "GROUP_MA" = TOUTES LES MAILLES DE LA LISTE DE GROUPES MAILLES
           IF (NG.GT.0) THEN
              DO 20 I = 1 , NG
                 CALL NOCART(CARTGR,2,ZK8(JDLS+I-1),' ',0,' ',0,' ',8)
 20           CONTINUE
           ENDIF
C
C ---     "MAILLE" = TOUTES LES MAILLES DE LA LISTE DE MAILLES
           IF (NM.GT.0) THEN
             CALL NOCART(CARTGR,3,' ','NOM',NM,ZK8(JDLS),0,' ',8)
           ENDIF
C
        ELSE
C
           CALL GETVR8 ( 'GRILLE', 'ORIG_AXE', IOC,1,3 ,ORIG, N8 )
           CALL GETVR8 ( 'GRILLE', 'AXE'     , IOC,1,3 ,AXEZ, N8 )
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
              CALL UTMESS('F','ACEAGB','AXE_Z NUL')
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
              CALL UTMESS('F','ACEAGB','NOEUD CONFONDU AVEC L''ORIGINE')
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
               CALL UTDEBM('F','ACEAGB','NOEUD SUR L''AXE_Z')
             ENDIF
             CALL ANGVX ( AXET, ANG(1), ANG(2) )
C
             ZR(JDVC+1) = ANG(1) * R8RDDG()
             ZR(JDVC+2) = ANG(2) * R8RDDG()
C
             CALL NOCART(CARTGR,3,' ','NUM',1,K8B,NUMA,' ',8)
C
 200       CONTINUE
        ENDIF
C
 10   CONTINUE
C
      CALL JEDETR ('&&ACEAGB.NUME_MA' )
      CALL JEDETR('&&TMPGRILLE')
      CALL JEDETR(TMPNGR)
      CALL JEDETR(TMPVGR)
C
      CALL JEDEMA()
      END
