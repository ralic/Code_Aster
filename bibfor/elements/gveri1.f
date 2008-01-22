      SUBROUTINE GVERI1 ( RESU, NOMA, MOTFAC,ILEV )
      IMPLICIT NONE
      CHARACTER*8         RESU, NOMA
      CHARACTER*(*)       MOTFAC
      INTEGER             ILEV
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 22/01/2008   AUTEUR REZETTE C.REZETTE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C TOLE CRP_20
C-----------------------------------------------------------------------
C FONCTION REALISEE:
C
C     DEFI_FOND_FISS EN 2D : TRAITEMENT DES LEVRES (MAILLES OU GROUPE
C     DE MAILLES) ET DE LA NORMALE
C     -----------------------------------------------------------------
C ENTREE:
C        RESU   : NOM DU CONCEPT RESULTAT DE L'OPERATEUR
C        NOMA   : NOM DU MAILLAGE
C        MOTFAC : MOT-CLE 'LEVRE_SUP' OU 'LEVRE_INF'
C                 OU  'NORMALE'
C     -----------------------------------------------------------------
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
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
      CHARACTER*32     JEXNOM, JEXNUM, JEXATR
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER       KK2,KK3,JADR,NBOBJ,NENT,NSOM,MM1,MM2,MM3
      INTEGER       JJJ,NDIM,NGRO,NBGM,NBEM,IER,DIM1,DIM2,DIM3
      INTEGER       IGR,NGR,INO,IRET,LL2,LL3,ITYP,DIM
      INTEGER       I,J,K,L,NA,NB,IRE1,IRE2,IN
      INTEGER       IATYMA,IBID,K2,K3
      INTEGER       JNORM,NCMP,J2,J3
      INTEGER       JSUP, NBMAS,IERA
      INTEGER       NBNOE,N1,IDCOOR,IDLINO,JNOLS,JLIMA,NBNOLS,IM,JCOORS
      INTEGER       JNOFO,KNOLS,JNOLI,KNOLI,NBNOLI,NUMORI,NUMFIN,INORMS
      INTEGER       INORMI,COMPT

      REAL*8        ZRBID,D
      REAL*8        X1,Y1,Z1,X2,Y2,Z2,DMAX,XSUP,YSUP,ZSUP,XINF,YINF,ZINF
      REAL*8        PSUP(3),PINF(3),VECNOR(3),VZ(3)
      CHARACTER*4   TYPMA,TYPM
      CHARACTER*8   K8B, MOTCLE, GROUPE, MAILLE, TYPE
      CHARACTER*24  OBJ1,OBJ2,OBJ3,TRAV
      CHARACTER*24 VALK(3),VK(2)
C     -----------------------------------------------------------------
C
      CALL JEMARQ()
      L = LEN(MOTFAC)
      IER = 0
C
      IF (MOTFAC .NE. 'NORMALE') THEN
       CALL GETVID ( ' ', 'MAILLAGE', 1,1,1, NOMA , N1 )
       CALL JEVEUO ( NOMA//'.COORDO    .VALE', 'L', IDCOOR )
       CALL DISMOI('F','NB_NO_MAILLA',NOMA,'MAILLAGE',NBNOE,K8B,IRET)

C     -----------------------------------------------------------------
C
C OBJETS DE MAILLAGE : OBJ1 A OBJ3
      CALL JEVEUO(NOMA//'.TYPMAIL','L',IATYMA)

      MOTCLE = 'MAILLE'
      GROUPE = 'GROUP_MA'
      OBJ1 = NOMA//'.GROUPEMA'
      OBJ2 = NOMA//'.NOMMAI'
      OBJ3 = NOMA//'.NOMNOE'
C
      NBGM = 0
      NBEM = 0
      CALL GETVID (MOTFAC(1:L),GROUPE,1,1,0,K8B,NGRO)
      CALL GETVID (MOTFAC(1:L),MOTCLE,1,1,0,K8B,NENT)
      NSOM = NGRO + NENT
      IF (NSOM.EQ.NGRO) THEN
         NGRO = -NGRO
         NBGM = MAX(NBGM,NGRO)
      ELSE IF (NSOM.EQ.NENT) THEN
         NENT = -NENT
         NBEM = MAX(NBEM,NENT)
      ENDIF
C
      NDIM = MAX(NBGM,NBEM)
      IF (NDIM .EQ. 0) GOTO 9999
C
C --- CAS LEVRE : ALLOCATION DES OBJETS DE TRAVAIL
C
      TRAV = '&&VERIFE.'//MOTFAC(1:L)//'               '
      CALL WKVECT(TRAV,'V V K8',NDIM,JJJ)
C
      CALL GETVID (MOTFAC(1:L),GROUPE,1,1,NDIM,ZK8(JJJ),NGR)
      DIM1 = 0
      DO 100 I=1,NGR
         CALL JEEXIN (JEXNOM(OBJ1,ZK8(JJJ+I-1)),IRET)
         IF(IRET.NE.0) THEN
            CALL JELIRA (JEXNOM(OBJ1,ZK8(JJJ+I-1)),'LONMAX',NBOBJ,K8B)
            DIM1 = DIM1 + NBOBJ
         ELSE
            IER = IER + 1
            CALL U2MESK('E','RUPTURE1_2',1,ZK8(JJJ+I-1))
         ENDIF
100   CONTINUE

      DIM2 = MAX(DIM1,NENT)
C
      IF ( MOTFAC .EQ. 'LEVRE_SUP' ) THEN
         CALL WKVECT('&&VERIFE.LEVRESUP  .MAIL','V V K8',DIM2,KK2)
         LL2 = KK2
C
      ELSEIF ( MOTFAC .EQ. 'LEVRE_INF' ) THEN
         CALL WKVECT('&&VERIFE.LEVREINF  .MAIL','V V K8',DIM2,KK3)
         LL3 = KK3
      ENDIF
C
C --- TRAITEMENT DES "GROUP_MA"
C     ---------------------------------------
C
C
      DO 110 IGR = 1, NGR
         CALL JELIRA (JEXNOM(OBJ1,ZK8(JJJ+IGR-1)),'LONMAX',NBOBJ,K8B)
         CALL JEVEUO (JEXNOM(OBJ1,ZK8(JJJ+IGR-1)),'L',JADR)
C
         DO 105 I = 1, NBOBJ
            IF (MOTFAC.EQ.'LEVRE_SUP') THEN
               CALL JENUNO(JEXNUM(OBJ2,ZI(JADR+I-1)),MAILLE)
               CALL JENONU(JEXNOM(OBJ2,MAILLE),IBID)
               ITYP=IATYMA-1+IBID
               CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ZI(ITYP)),TYPE)
               TYPMA = TYPE(1:3)
               IF (TYPMA.NE.'SEG') THEN
                  VK(1) = TYPE(1:5)
                  VK(2) = MOTFAC
                  CALL U2MESK('F','RUPTURE0_75',2,VK)
               ELSE
                  ZK8(KK2) = MAILLE
                  KK2 = KK2 + 1
               ENDIF
C
            ELSEIF(MOTFAC.EQ.'LEVRE_INF') THEN
               CALL JENUNO(JEXNUM(OBJ2,ZI(JADR+I-1)),MAILLE)
               CALL JENONU(JEXNOM(OBJ2,MAILLE),IBID)
               ITYP=IATYMA-1+IBID
               CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ZI(ITYP)),TYPE)
               TYPMA = TYPE(1:3)
               IF (TYPMA.NE.'SEG') THEN
                  VK(1) = TYPE(1:5)
                  VK(2) = MOTFAC
                  CALL U2MESK('F','RUPTURE0_75',2,VK)
               ELSE
                  ZK8(KK3) = MAILLE
                  KK3 = KK3 + 1
               ENDIF
            ENDIF
105      CONTINUE
C
110   CONTINUE
C
C --- TRAITEMENT DES "MAILLE"
C     ----------------------------------
C
      CALL GETVID (MOTFAC(1:L),MOTCLE,1,1,NDIM,ZK8(JJJ),NBOBJ)
      DO 200 INO = 1, NBOBJ
         CALL JENONU (JEXNOM(OBJ2,ZK8(JJJ+INO-1)),IRET)
         IF(IRET .EQ. 0) THEN
             VALK(1) = MOTCLE
             VALK(2) = ZK8(JJJ+INO-1)
             VALK(3) = NOMA
             CALL U2MESK('E','MODELISA2_96', 3 ,VALK)
            IER = IER + 1
         ENDIF
 200  CONTINUE
C
      IF ( MOTFAC .EQ. 'LEVRE_SUP' ) THEN
         DO 230 INO = 1, NBOBJ
            CALL JENONU(JEXNOM(OBJ2,ZK8(JJJ+INO-1)),IBID)
            ITYP=IATYMA-1+IBID
            CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ZI(ITYP)),TYPE)
            TYPMA = TYPE(1:3)
            IF (TYPMA.NE.'SEG') THEN
               VK(1) = TYPE(1:5)
               VK(2) = MOTFAC
               CALL U2MESK('F','RUPTURE0_75',2,VK)
            ELSE
               ZK8(KK2) = ZK8(JJJ + INO - 1)
               KK2 = KK2 + 1
            ENDIF
 230     CONTINUE
C
      ELSEIF ( MOTFAC .EQ. 'LEVRE_INF') THEN
         DO 240 INO = 1, NBOBJ
            CALL JENONU(JEXNOM(OBJ2,ZK8(JJJ+INO-1)),IBID)
            ITYP=IATYMA-1+IBID
            CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ZI(ITYP)),TYPE)
            TYPMA = TYPE(1:3)
            IF (TYPMA.NE.'SEG') THEN
               VK(1) = TYPE(1:5)
               VK(2) = MOTFAC
               CALL U2MESK('F','RUPTURE0_75',2,VK)
            ELSE
               ZK8(KK3) = ZK8(JJJ + INO - 1)
               KK3 = KK3 + 1
            ENDIF
 240     CONTINUE
      ENDIF
C
C --- VERIFICATION ET STOCKAGE
C
      IF ( MOTFAC .EQ. 'LEVRE_SUP' ) THEN
C              -----------------------
         CALL WKVECT(RESU//'.LEVRESUP  .MAIL','G V K8',DIM2,MM2)
         K2 = 1
         DO 600 I=1,DIM2-1
            IF ( ZK8(LL2 + I - 1).NE.'0' ) THEN
               ZK8(MM2 + K2 - 1) = ZK8(LL2 + I - 1)
               K2 = K2 + 1
               DO 605 J=I+1,DIM2
                  IF ( ZK8(LL2 + I - 1).EQ.ZK8(LL2 + J - 1) ) THEN
                     ZK8(LL2 + J - 1) = '0'
                     J2 = I
                  ENDIF
605            CONTINUE
            ENDIF
600      CONTINUE
         IF (ZK8(LL2 + DIM2 - 1).NE.'0') THEN
            ZK8(MM2 + K2 - 1) = ZK8(LL2 + DIM2 - 1)
            K2 = K2 + 1
         ENDIF
         K2 = K2 - 1
C
         IF (K2.NE.DIM2) THEN
            CALL U2MESK('E','RUPTURE0_70',1,ZK8(LL2 + J2 - 1))
            IER = IER+1
         ENDIF
C-- GROUP_MA --> GROUP_NO  ET VECTEUR NORMAL
         CALL WKVECT ( '&&GVERI1_TRAV'  , 'V V I', NBNOE, IDLINO )
         CALL WKVECT ( '&&GVERI1_NOEU_NORM_SUP', 'V V I', NBNOE, JNOLS)
         CALL WKVECT ( '&&GVERI1_MAILLE_LEV_SUP', 'V V I', DIM2, JLIMA)
         DO 601 IM = 1 , DIM2
            CALL JENONU(JEXNOM(NOMA//'.NOMMAI',ZK8(MM2+IM-1)),
     &                   ZI(JLIMA+IM-1) )
 601     CONTINUE
         CALL GMGNRE ( NOMA, NBNOE, ZI(IDLINO), ZI(JLIMA), DIM2,
     &                           ZI(JNOLS), NBNOLS, 'TOUS' )

         IF (ILEV.EQ.0) THEN
           CALL JEVEUO ( RESU//'.FOND      .NOEU', 'L', JNOFO )
         ELSE
           CALL JEVEUO ( RESU//'.FOND_SUP  .NOEU', 'L', JNOFO )
         ENDIF
         CALL JENONU(JEXNOM(OBJ3,ZK8(JNOFO)), NUMORI )
         X1 = ZR(IDCOOR-1+3*(NUMORI-1)+1)
         Y1 = ZR(IDCOOR-1+3*(NUMORI-1)+2)
         Z1 = ZR(IDCOOR-1+3*(NUMORI-1)+3)
         DMAX = 0.D0
         XSUP = 0.D0
         YSUP = 0.D0
         ZSUP = 0.D0
         COMPT = 0
         DO 602 IN = 1 , NBNOLS
           INO = JNOLS+IN-1
           IF ( ZI(INO) .EQ. NUMORI ) THEN
             COMPT = COMPT + 1
             GOTO 602
           ENDIF
           X2 = ZR(IDCOOR-1+3*(ZI(INO)-1)+1)
           Y2 = ZR(IDCOOR-1+3*(ZI(INO)-1)+2)
           Z2 = ZR(IDCOOR-1+3*(ZI(INO)-1)+3)
           D = SQRT( (X2-X1)**2 + (Y2-Y1)**2 + (Z2-Z1)**2 )
           XSUP = XSUP + X2
           YSUP = YSUP + Y2
           ZSUP = ZSUP + Z2
           IF  (D .GT. DMAX) THEN
             DMAX = D
             NUMFIN = ZI(INO)
           ENDIF
602      CONTINUE

         IF(COMPT .EQ. 0)  THEN
            CALL U2MESK('F','RUPTURE0_76',1,ZK8(JNOFO))
         ENDIF
         CALL OREINO ( NOMA, ZI(JNOLS), NBNOLS, NUMORI ,
     &          NUMFIN,ZR(IDCOOR),'RELATIF',0.1D0,IERA,IRET)
         CALL WKVECT(RESU//'.SUPNORM   .NOEU','G V K8',20,KNOLS)
         DO 603 IN = 1 , MIN(NBNOLS,20)
           CALL JENUNO(JEXNUM(OBJ3,ZI(JNOLS+IN-1)),
     &                          ZK8(KNOLS+IN-1))
603      CONTINUE
C CALCUL DE LA NORMALE
         CALL WKVECT ( '&&GVERI1_NORMAS'  , 'G V R8', 3, INORMS )
         PSUP(1) = X1 - XSUP/(NBNOLS-1)
         PSUP(2) = Y1 - YSUP/(NBNOLS-1)
         PSUP(3) = Z1 - ZSUP/(NBNOLS-1)
         VZ(1) = 0.D0
         VZ(2) = 0.D0
         VZ(3) = 1.D0
         CALL PROVEC ( VZ,PSUP,  ZR(INORMS) )
         CALL NORMEV(ZR(INORMS),ZRBID)

         CALL JEDETR ( '&&GVERI1_TRAV' )
         CALL JEDETR ( '&&GVERI1_NOEU_NORM_SUP' )
         CALL JEDETR ( '&&GVERI1_MAILLE_LEV_SUP' )
C
      ELSEIF ( MOTFAC .EQ. 'LEVRE_INF' ) THEN
C              -----------------------
         CALL WKVECT(RESU//'.LEVREINF  .MAIL','G V K8',DIM2,MM3)
         K3 = 1
         DO 700 I=1,DIM2-1
            IF ( ZK8(LL3 + I - 1).NE.'0' ) THEN
               ZK8(MM3 + K3 - 1) = ZK8(LL3 + I - 1)
               K3 = K3 + 1
               DO 705 J=I+1,DIM2
                  IF ( ZK8(LL3 + I - 1).EQ.ZK8(LL3 + J - 1) ) THEN
                     ZK8(LL3 + J - 1) = '0'
                     J3 = I
                  ENDIF
705            CONTINUE
            ENDIF
700      CONTINUE
         IF (ZK8(LL3 + DIM2 - 1).NE.'0') THEN
            ZK8(MM3 + K3 - 1) = ZK8(LL3 + DIM2 - 1)
            K3 = K3 + 1
         ENDIF
         K3 = K3 - 1
C
         IF (K3.NE.DIM2) THEN
            CALL U2MESK('E','RUPTURE0_71',1,ZK8(LL3 + J3 - 1))
            IER = IER+1
         ENDIF

C COMPARAISON LEVRE SUP / LEVRE INF
         CALL JEVEUO ( RESU//'.LEVRESUP  .MAIL', 'L', JSUP )
         CALL JELIRA ( RESU//'.LEVRESUP  .MAIL', 'LONMAX',NBMAS,K8B)
         DO 710 I = 1,NBMAS
           DO 715 J = 1,DIM2
            IF (ZK8(JSUP+I-1) .EQ. ZK8(MM3+J-1) ) THEN
              CALL U2MESK('F','RUPTURE0_73',1,ZK8(JSUP+I-1))
            END IF
715        CONTINUE
710      CONTINUE
C-- GROUP_MA --> GROUP_NO  ET VECTEUR NORMAL
         CALL WKVECT ( '&&GVERI1_TRAV'  , 'V V I', NBNOE, IDLINO )
         CALL WKVECT ( '&&GVERI1_NOEU_NORM_INF', 'V V I', NBNOE, JNOLI )
         CALL WKVECT ( '&&GVERI1_MAILLE_LEV_INF', 'V V I', DIM2, JLIMA )
         DO 721 IM = 1 , DIM2
            CALL JENONU(JEXNOM(NOMA//'.NOMMAI',ZK8(MM3+IM-1)),
     &                                                 ZI(JLIMA+IM-1) )
721      CONTINUE
         CALL GMGNRE ( NOMA, NBNOE, ZI(IDLINO), ZI(JLIMA), DIM2,
     &                           ZI(JNOLI), NBNOLI, 'TOUS' )

         IF (ILEV.EQ.0) THEN
           CALL JEVEUO ( RESU//'.FOND      .NOEU', 'L', JNOFO )
         ELSE
           CALL JEVEUO ( RESU//'.FOND_INF  .NOEU', 'L', JNOFO )
         ENDIF
         CALL JENONU(JEXNOM(OBJ3,ZK8(JNOFO)), NUMORI )
         X1 = ZR(IDCOOR-1+3*(NUMORI-1)+1)
         Y1 = ZR(IDCOOR-1+3*(NUMORI-1)+2)
         Z1 = ZR(IDCOOR-1+3*(NUMORI-1)+3)
         DMAX = 0.D0
         XINF = 0.D0
         YINF = 0.D0
         ZINF = 0.D0
         COMPT = 0
         DO 722 IN = 1 , NBNOLI
           INO = JNOLI+IN-1
           IF ( ZI(INO) .EQ. NUMORI ) THEN
             COMPT = COMPT + 1
             GOTO 722
           ENDIF
           X2 = ZR(IDCOOR-1+3*(ZI(INO)-1)+1)
           Y2 = ZR(IDCOOR-1+3*(ZI(INO)-1)+2)
           Z2 = ZR(IDCOOR-1+3*(ZI(INO)-1)+3)
           D = SQRT( (X2-X1)**2 + (Y2-Y1)**2 + (Z2-Z1)**2 )
           XINF = XINF + X2
           YINF = YINF + Y2
           ZINF = ZINF + Z2
           IF  (D .GT. DMAX) THEN
             DMAX = D
             NUMFIN = ZI(INO)
           ENDIF
722      CONTINUE

         IF(COMPT .EQ. 0)  THEN
            CALL U2MESK('F','RUPTURE0_77',1,ZK8(JNOFO))
         ENDIF
         CALL OREINO ( NOMA, ZI(JNOLI), NBNOLI, NUMORI ,
     &          NUMFIN,ZR(IDCOOR),'RELATIF',0.1D0,IERA,IRET)
         CALL WKVECT(RESU//'.INFNORM   .NOEU','G V K8',20,KNOLI)
         DO 723 IN = 1 , MIN(NBNOLI,20)
            CALL JENUNO(JEXNUM(OBJ3,ZI(JNOLI+IN-1)),
     &                          ZK8(KNOLI+IN-1))
723      CONTINUE
C CALCUL DE LA NORMALE
         CALL WKVECT ( '&&GVERI1_NORMAI'  , 'G V R8', 3, INORMI )
         PINF(1) = X1 - XINF/(NBNOLI-1)
         PINF(2) = Y1 - YINF/(NBNOLI-1)
         PINF(3) = Z1 - ZINF/(NBNOLI-1)
         VZ(1) = 0.D0
         VZ(2) = 0.D0
         VZ(3) = 1.D0
         CALL PROVEC ( VZ,PINF, ZR(INORMI)  )
         CALL NORMEV(ZR(INORMI),ZRBID)

         CALL JEDETR ( '&&GVERI1_TRAV' )
         CALL JEDETR ( '&&GVERI1_NOEU_NORM_INF' )
         CALL JEDETR ( '&&GVERI1_MAILLE_LEV_INF' )
      ENDIF
C
C --- CAS 'NORMALE' : SAUVEGARDE VECTEUR
C
      ELSEIF ( MOTFAC .EQ. 'NORMALE' ) THEN
C          ---------------------
         CALL WKVECT(RESU//'.NORMALE','G V R8',3,JNORM)
         CALL GETVR8 (' ','NORMALE',1,1,0,ZRBID,NCMP)
         IF(NCMP.EQ.0) THEN
           CALL JEEXIN('&&GVERI1_NORMAS',IRE1)
           CALL JEEXIN('&&GVERI1_NORMAI',IRE2)
           CALL ASSERT(IRE1.NE.0)
           CALL JEVEUO('&&GVERI1_NORMAS','L',INORMS)
           IF(IRE2.NE.0)THEN
             CALL JEVEUO('&&GVERI1_NORMAI','L',INORMI)
             ZR(JNORM) =   (ZR(INORMS)+ZR(INORMI))/2.D0
             ZR(JNORM+1) = (ZR(INORMS+1)+ZR(INORMI+1))/2.D0
             ZR(JNORM+2) = (ZR(INORMS+2)+ZR(INORMI+2))/2.D0
             CALL JEDETR ('&&GVERI1_NORMAI' )
           ELSE
             ZR(JNORM)   = ZR(INORMS)
             ZR(JNORM+1) = ZR(INORMS+1)
             ZR(JNORM+2) = ZR(INORMS+2)
           ENDIF
           CALL JEDETR ( '&&GVERI1_NORMAS' )
         ELSE
           NCMP = -NCMP
           CALL GETVR8 (' ','NORMALE',1,1,3,ZR(JNORM),NCMP)
         ENDIF
         GO TO 9999
      ENDIF
C
C --- DESTRUCTION DES OBJETS DE TRAVAIL
C
C
      IF(MOTFAC.EQ.'LEVRE_SUP') THEN
         CALL JEDETR (TRAV)
         CALL JEDETR('&&VERIFE.LEVRESUP  .MAIL')
      ELSEIF(MOTFAC.EQ.'LEVRE_INF') THEN
         CALL JEDETR (TRAV)
         CALL JEDETR('&&VERIFE.LEVREINF  .MAIL')
      ENDIF
C
9999  CONTINUE
      CALL JEDEMA()
      END
