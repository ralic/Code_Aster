      SUBROUTINE GNORMF ( NBNOFO,NOMA,RESU,ILEV,TYPFON )
      IMPLICIT   NONE
      CHARACTER*8         NOMA,RESU
      CHARACTER*16        TYPFON
      INTEGER              NBNOFO,ILEV
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 13/12/2006   AUTEUR PELLET J.PELLET 
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
C
C     OPERATEUR DEFI_FOND_FISS : RECUPERTAION DES NOEUDS DES LEVRES
C                               SUR DES DIRECTIONS NORMALES AU FOND
C     ------------------------------------------------------------------

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
      CHARACTER*32     JEXNOM, JEXNUM
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
      INTEGER       NBMA,JLIMA,IM,N1,IDCOOR,NBNOE,IRET,JSUP,JNOLS,KNOLS
      INTEGER       IDLINO,NBNOLS,JCOORS,IN,NBNOFT,INF,IFM,NIV,JNOFO
      INTEGER       NUMORI,NO1, NO2, NO3,JINTS,NBTRLS,IERA,JTGOR,JTGEX
      INTEGER       NUMFIN,ISYM,KNOLI,JINF,JNOLI,NBNOLI,JCOORI,JINTI
      INTEGER       NBTRLI,INO,INOS,NBS,NUMUN,JTS,JTI,NBI,INOI
      INTEGER       NUMTMP,JNOFOS
      REAL*8        X0(3),X1, X2, Y1, Y2, Z1, Z2,D,VECNOR(3),VP(3),DMIN
      REAL*8        TGOR(3),D1,D2,TGEX(3),DMAX,PREC,PRECO,PS,VECTAN(3)
      REAL*8        PRECN
      CHARACTER*8   K8B,CRITN
      CHARACTER*24  MSUP,MINF,FONNOE,NOMNOE,FONTOR,FONTEX
      LOGICAL        EXTGOR,EXTGEX

C DEB-------------------------------------------------------------------

      CALL JEMARQ ( )
      CRITN = 'RELATIF'
      CALL INFNIV ( IFM , NIV )

C     ------------------------------------------------------------------
C                        LE MAILLAGE, LE FOND
C     ------------------------------------------------------------------

      CALL GETVID ( ' ', 'MAILLAGE', 1,1,1, NOMA , N1 )

      CALL GETVR8 ( ' ', 'PREC_NORM', 1,1,1, PRECN, N1 )

      CALL JEVEUO ( NOMA//'.COORDO    .VALE', 'L', IDCOOR )
      CALL DISMOI('F','NB_NO_MAILLA',NOMA,'MAILLAGE',NBNOE,K8B,IRET)
      NOMNOE = NOMA//'.NOMNOE'

      IF (ILEV.EQ.0) THEN
        FONNOE =RESU//'.FOND      .NOEU'
        CALL JEVEUO ( FONNOE, 'L', JNOFO )
      ELSE
        FONNOE =RESU//'.FOND_INF  .NOEU'
        CALL JEVEUO ( FONNOE, 'L', JNOFO )
        FONNOE =RESU//'.FOND_SUP  .NOEU'
        CALL JEVEUO ( FONNOE, 'L', JNOFOS )
      ENDIF


C     ------------------------------------------------------------------
C                  VECTEUR RESULTAT
C     ------------------------------------------------------------------
      CALL WKVECT(RESU//'.SUPNORM   .NOEU','G V K8',NBNOFO*20,KNOLS)

      CALL JEEXIN(RESU//'.LEVREINF  .MAIL',ISYM)
      IF(ISYM.NE.0) THEN
        CALL WKVECT(RESU//'.INFNORM   .NOEU','G V K8',NBNOFO*20,KNOLI)
      ENDIF

C     ------------------------------------------------------------------
C                   TANGENTES AUX EXTREMITES
C     ------------------------------------------------------------------
      FONTOR = RESU//'.DTAN_ORIGINE'
      CALL JEEXIN ( FONTOR, IRET )
      IF (IRET.EQ.0) THEN
         EXTGOR = .FALSE.
      ELSE
         EXTGOR = .TRUE.
         CALL JEVEUO ( FONTOR, 'L', JTGOR )
         TGOR(1) = ZR(JTGOR)
         TGOR(2) = ZR(JTGOR+1)
         TGOR(3) = ZR(JTGOR+2)
      ENDIF

      FONTEX = RESU//'.DTAN_EXTREMITE'
      CALL JEEXIN ( FONTEX, IRET )
      IF (IRET.EQ.0) THEN
         EXTGEX = .FALSE.
      ELSE
         EXTGEX = .TRUE.
         CALL JEVEUO ( FONTEX, 'L', JTGEX )
         TGEX(1) = ZR(JTGEX)
         TGEX(2) = ZR(JTGEX+1)
         TGEX(3) = ZR(JTGEX+2)
      ENDIF
C
C ------ CAS DU FOND_FERME: LE PREMIER ET LE DERNIER NOEUD SONT
C                           IDENTIQUES
      IF ( TYPFON .EQ. 'FOND_FERME') THEN
         NBNOFT = NBNOFO - 1
      ELSE
         NBNOFT = NBNOFO
      ENDIF

C     ------------------------------------------------------------------
C         GROUP_MA LEVRE_SUP --> GROUP_NO LEVRE_SUP
C     ------------------------------------------------------------------
      MSUP = RESU//'.LEVRESUP  .MAIL'
      CALL JEVEUO ( MSUP, 'L', JSUP )
      CALL JELIRA ( MSUP, 'LONMAX', NBMA, K8B )

      CALL WKVECT ( '&&GNORMF_TRAV'  , 'V V I', NBNOE, IDLINO )
      CALL WKVECT ( '&&GNORMF_NOEU_NORM_SUP', 'V V I', NBNOE, JNOLS  )

      CALL WKVECT ( '&&GNORMF_MAILLE_LEV_SUP', 'V V I', NBMA, JLIMA )
      DO 10 IM = 1 , NBMA
         CALL JENONU(JEXNOM(NOMA//'.NOMMAI',ZK8(JSUP+IM-1)),
     &                                                 ZI(JLIMA+IM-1) )
 10   CONTINUE
      CALL GMGNRE ( NOMA, NBNOE, ZI(IDLINO), ZI(JLIMA), NBMA,
     &                           ZI(JNOLS), NBNOLS, 'TOUS' )

      CALL WKVECT ('&&PKFOND_COOR_LEV_SUP', 'V V R', 3*NBNOLS, JCOORS )
      DO 11 IN = 1 , NBNOLS
         ZR(JCOORS-1+3*(IN-1)+1) = ZR(IDCOOR-1+3*(ZI(JNOLS+IN-1)-1)+1)
         ZR(JCOORS-1+3*(IN-1)+2) = ZR(IDCOOR-1+3*(ZI(JNOLS+IN-1)-1)+2)
         ZR(JCOORS-1+3*(IN-1)+3) = ZR(IDCOOR-1+3*(ZI(JNOLS+IN-1)-1)+3)
 11   CONTINUE

      CALL JEDETR ( '&&GNORMF_TRAV' )

C     ------------------------------------------------------------------
C         GROUP_MA LEVRE_INF --> GROUP_NO LEVRE_INF
C     ------------------------------------------------------------------
      IF(ISYM.NE.0) THEN
        MINF = RESU//'.LEVREINF  .MAIL'
        CALL JEVEUO ( MINF, 'L',JINF )
        CALL JELIRA ( MINF, 'LONMAX', NBMA, K8B )

        CALL WKVECT ( '&&GNORMF_TRAV'  , 'V V I', NBNOE, IDLINO )
        CALL WKVECT ( '&&GNORMF_NOEU_NORM_INF', 'V V I', NBNOE,JNOLI)
        CALL WKVECT ( '&&PKFOND_MAILLE_LEV_INF', 'V V I', NBMA,JLIMA)
        DO 20 IM = 1 , NBMA
         CALL JENONU(JEXNOM(NOMA//'.NOMMAI',ZK8(JINF+IM-1)),
     &                                                 ZI(JLIMA+IM-1) )
 20     CONTINUE
        CALL GMGNRE ( NOMA, NBNOE, ZI(IDLINO), ZI(JLIMA), NBMA,
     &                           ZI(JNOLI), NBNOLI, 'TOUS' )

        CALL WKVECT ('&&PKFOND_COOR_LEV_INF', 'V V R', 3*NBNOLI,JCOORI)
        DO 21 IN = 1 , NBNOLI
          ZR(JCOORI-1+3*(IN-1)+1) = ZR(IDCOOR-1+3*(ZI(JNOLI+IN-1)-1)+1)
          ZR(JCOORI-1+3*(IN-1)+2) = ZR(IDCOOR-1+3*(ZI(JNOLI+IN-1)-1)+2)
          ZR(JCOORI-1+3*(IN-1)+3) = ZR(IDCOOR-1+3*(ZI(JNOLI+IN-1)-1)+3)
 21     CONTINUE
        CALL JEDETR ( '&&GNORMF_TRAV' )
      ENDIF

C     ------------------------------------------------------------------
C        BOUCLE SUR LES NOEUDS DU FOND DE FISSURE
C     ------------------------------------------------------------------

      DO 200 INF = 1 , NBNOFT
         CALL JENONU(JEXNOM(NOMNOE,ZK8(JNOFO+INF-1)), NUMORI )
C
C ------ DETERMINATION DU PLAN PASSANT PAR UN NOEUD N_I DU FOND DE
C        FISSURE ET UN VECTEUR NORMAL

         X0(1) =  ZR(IDCOOR-1+3*(NUMORI-1)+1)
         X0(2) =  ZR(IDCOOR-1+3*(NUMORI-1)+2)
         X0(3) =  ZR(IDCOOR-1+3*(NUMORI-1)+3)

         IF ( INF .EQ. 1 ) THEN
            IF ( EXTGOR ) THEN
               CALL JENONU(JEXNOM(NOMNOE,ZK8(JNOFO+INF-1)), IN )
               X1 = ZR(IDCOOR-1+3*(IN-1)+1)
               Y1 = ZR(IDCOOR-1+3*(IN-1)+2)
               Z1 = ZR(IDCOOR-1+3*(IN-1)+3)
               CALL JENONU(JEXNOM(NOMNOE,ZK8(JNOFO+INF)), IN )
               X2 = ZR(IDCOOR-1+3*(IN-1)+1)
               Y2 = ZR(IDCOOR-1+3*(IN-1)+2)
               Z2 = ZR(IDCOOR-1+3*(IN-1)+3)
               D = SQRT( (X2-X1)**2 + (Y2-Y1)**2 + (Z2-Z1)**2 )
               VECNOR(1) = (X2-X1) / D
               VECNOR(2) = (Y2-Y1) / D
               VECNOR(3) = (Z2-Z1) / D
               CALL PROVEC ( VECNOR, TGOR, VP )
               CALL PROVEC ( VP, TGOR, VECNOR )
            ELSE
               CALL JENONU(JEXNOM(NOMNOE,ZK8(JNOFO+INF-1)), IN )
               X2 = ZR(IDCOOR-1+3*(IN-1)+1)
               Y2 = ZR(IDCOOR-1+3*(IN-1)+2)
               Z2 = ZR(IDCOOR-1+3*(IN-1)+3)
               CALL JENONU(JEXNOM(NOMNOE,ZK8(JNOFO+INF)), IN )
               X1 = ZR(IDCOOR-1+3*(IN-1)+1)
               Y1 = ZR(IDCOOR-1+3*(IN-1)+2)
               Z1 = ZR(IDCOOR-1+3*(IN-1)+3)
               D = SQRT( (X2-X1)**2 + (Y2-Y1)**2 + (Z2-Z1)**2 )
               IF ( TYPFON .EQ. 'FOND_FISS') THEN
                 D1 = SQRT( (X2-X1)**2 + (Y2-Y1)**2 + (Z2-Z1)**2 )
                 VECNOR(1) = (X2-X1) / D1
                 VECNOR(2) = (Y2-Y1) / D1
                 VECNOR(3) = (Z2-Z1) / D1
               ELSE
                 CALL JENONU(JEXNOM(NOMNOE,ZK8(JNOFO+NBNOFO-2)), NO1 )
                 CALL JENONU(JEXNOM(NOMNOE,ZK8(JNOFO+INF-1)), NO2 )
                 CALL JENONU(JEXNOM(NOMNOE,ZK8(JNOFO+INF  )), NO3 )
                 CALL PKFON1 ( ZR(IDCOOR), VECNOR, NO1, NO2, NO3 )
                 CALL JENONU(JEXNOM(NOMNOE,ZK8(JNOFO+NBNOFO-2)), IN )
                 X1 = ZR(IDCOOR-1+3*(IN-1)+1)
                 Y1 = ZR(IDCOOR-1+3*(IN-1)+2)
                 Z1 = ZR(IDCOOR-1+3*(IN-1)+3)
                 D1 = SQRT( (X2-X1)**2 + (Y2-Y1)**2 + (Z2-Z1)**2 )
                 D  = MIN(D,D1)
               ENDIF
            ENDIF
         ELSEIF ( INF .EQ. NBNOFO ) THEN
            IF ( EXTGEX ) THEN
               CALL JENONU(JEXNOM(NOMNOE,ZK8(JNOFO+INF-2)), IN )
               X1 = ZR(IDCOOR-1+3*(IN-1)+1)
               Y1 = ZR(IDCOOR-1+3*(IN-1)+2)
               Z1 = ZR(IDCOOR-1+3*(IN-1)+3)
               CALL JENONU(JEXNOM(NOMNOE,ZK8(JNOFO+INF-1)), IN )
               X2 = ZR(IDCOOR-1+3*(IN-1)+1)
               Y2 = ZR(IDCOOR-1+3*(IN-1)+2)
               Z2 = ZR(IDCOOR-1+3*(IN-1)+3)
               D = SQRT( (X2-X1)**2 + (Y2-Y1)**2 + (Z2-Z1)**2 )
               VECNOR(1) = (X2-X1) / D
               VECNOR(2) = (Y2-Y1) / D
               VECNOR(3) = (Z2-Z1) / D
               CALL PROVEC ( VECNOR, TGEX, VP )
               CALL PROVEC ( VP, TGEX, VECNOR )
            ELSE
               CALL JENONU(JEXNOM(NOMNOE,ZK8(JNOFO+INF-2)), IN )
               X2 = ZR(IDCOOR-1+3*(IN-1)+1)
               Y2 = ZR(IDCOOR-1+3*(IN-1)+2)
               Z2 = ZR(IDCOOR-1+3*(IN-1)+3)
               CALL JENONU(JEXNOM(NOMNOE,ZK8(JNOFO+INF-1)), IN )
               X1 = ZR(IDCOOR-1+3*(IN-1)+1)
               Y1 = ZR(IDCOOR-1+3*(IN-1)+2)
               Z1 = ZR(IDCOOR-1+3*(IN-1)+3)
               D = SQRT( (X2-X1)**2 + (Y2-Y1)**2 + (Z2-Z1)**2 )
               IF ( TYPFON .EQ. 'FOND_FISS') THEN
                 D1 = SQRT( (X2-X1)**2 + (Y2-Y1)**2 + (Z2-Z1)**2 )
                 VECNOR(1) = (X2-X1) / D1
                 VECNOR(2) = (Y2-Y1) / D1
                 VECNOR(3) = (Z2-Z1) / D1
               ELSE
                 CALL JENONU(JEXNOM(NOMNOE,ZK8(JNOFO+INF-2)), NO1 )
                 CALL JENONU(JEXNOM(NOMNOE,ZK8(JNOFO+INF-1)), NO2 )
                 CALL JENONU(JEXNOM(NOMNOE,ZK8(JNOFO+1-1  )), NO3 )
                 CALL PKFON1 ( ZR(IDCOOR), VECNOR, NO1, NO2, NO3 )
                 CALL JENONU(JEXNOM(NOMNOE,ZK8(JNOFO+NBNOFO-2)), IN )
                 X1 = ZR(IDCOOR-1+3*(IN-1)+1)
                 Y1 = ZR(IDCOOR-1+3*(IN-1)+2)
                 Z1 = ZR(IDCOOR-1+3*(IN-1)+3)
                 D1 = SQRT( (X2-X1)**2 + (Y2-Y1)**2 + (Z2-Z1)**2 )
                 D  = MIN(D,D1)
               ENDIF
            ENDIF
         ELSE
            CALL JENONU(JEXNOM(NOMNOE,ZK8(JNOFO+INF-2)), NO1 )
            CALL JENONU(JEXNOM(NOMNOE,ZK8(JNOFO+INF-1)), NO2 )
            CALL JENONU(JEXNOM(NOMNOE,ZK8(JNOFO+INF  )), NO3 )
            CALL PKFON1 ( ZR(IDCOOR), VECNOR, NO1, NO2, NO3 )
            CALL JENONU(JEXNOM(NOMNOE,ZK8(JNOFO+INF-2)), IN )
            X1 = ZR(IDCOOR-1+3*(IN-1)+1)
            Y1 = ZR(IDCOOR-1+3*(IN-1)+2)
            Z1 = ZR(IDCOOR-1+3*(IN-1)+3)
            CALL JENONU(JEXNOM(NOMNOE,ZK8(JNOFO+INF-1)), IN )
            X2 = ZR(IDCOOR-1+3*(IN-1)+1)
            Y2 = ZR(IDCOOR-1+3*(IN-1)+2)
            Z2 = ZR(IDCOOR-1+3*(IN-1)+3)
            D1 = SQRT( (X2-X1)**2 + (Y2-Y1)**2 + (Z2-Z1)**2 )
            CALL JENONU(JEXNOM(NOMNOE,ZK8(JNOFO+INF)), IN )
            X1 = ZR(IDCOOR-1+3*(IN-1)+1)
            Y1 = ZR(IDCOOR-1+3*(IN-1)+2)
            Z1 = ZR(IDCOOR-1+3*(IN-1)+3)
            D2 = SQRT( (X2-X1)**2 + (Y2-Y1)**2 + (Z2-Z1)**2 )
            D  = MIN(D1,D2)
         ENDIF

         PREC = D * PRECN

C ------ CALCUL DE L'INTERSECTION DU PLAN AVEC LES NOEUDS DES MAILLES
C        DEFINISSANT LA LEVRE INFERIEURE - SAUVEGARDE

         IF(ISYM.NE.0) THEN
           CALL WKVECT ( '&&PKFOND_INTERS_INF', 'V V I', NBNOE, JINTI )
           CALL CGNOP0 ( NBNOLI, ZR(JCOORI), X0, VECNOR, PREC, NBTRLI,
     &                 ZI(JINTI))
           IF ( NBTRLI .LE. 2 ) THEN
             CALL JEDETR ( '&&PKFOND_INTERS_INF' )
             GOTO 200
           ENDIF
           CALL WKVECT ( '&&PKFOND_TRAV_INF', 'V V I', NBTRLI, JTI )

C ---- ORDRE DES NOEUDS
           NUMFIN = NUMORI
           DMAX = 0.D0
           DMIN = 100.D0
           NBI = 1
           INOI = 1
           ZI(JTI+INOI-1) = NUMORI
           X1 = ZR(IDCOOR-1+3*(NUMORI-1)+1)
           Y1 = ZR(IDCOOR-1+3*(NUMORI-1)+2)
           Z1 = ZR(IDCOOR-1+3*(NUMORI-1)+3)
C
C identification noeuds sur bon cote de la levre (cas fond ferme)
C a partir du noeud le plus proche du fond
           DO 310 IN = 1 , NBTRLI
             INO = JNOLI+ZI(JINTI+IN-1)-1
             IF ( ZI(INO) .EQ. NUMORI ) GOTO 310
             X2 = ZR(IDCOOR-1+3*(ZI(INO)-1)+1)
             Y2 = ZR(IDCOOR-1+3*(ZI(INO)-1)+2)
             Z2 = ZR(IDCOOR-1+3*(ZI(INO)-1)+3)
             D = SQRT( (X2-X1)**2 + (Y2-Y1)**2 + (Z2-Z1)**2 )
             IF  (D .LT. DMIN) THEN
               DMIN = D
               NUMUN = ZI(INO)
             ENDIF
310        CONTINUE

           VECTAN(1) = ZR(IDCOOR-1+3*(NUMUN-1)+1)-X1
           VECTAN(2) = ZR(IDCOOR-1+3*(NUMUN-1)+2)-Y1
           VECTAN(3) = ZR(IDCOOR-1+3*(NUMUN-1)+3)-Z1

           DO 320 IN = 1 , NBTRLI
             INO = JNOLI+ZI(JINTI+IN-1)-1
             IF ( ZI(INO) .EQ. NUMORI ) GOTO 320
             X2 = ZR(IDCOOR-1+3*(ZI(INO)-1)+1)
             Y2 = ZR(IDCOOR-1+3*(ZI(INO)-1)+2)
             Z2 = ZR(IDCOOR-1+3*(ZI(INO)-1)+3)
             D = SQRT( (X2-X1)**2 + (Y2-Y1)**2 + (Z2-Z1)**2 )
             PS=(X2-X1)*VECTAN(1)+(Y2-Y1)*VECTAN(2)+(Z2-Z1)*VECTAN(3)
             IF ( PS .GE. 0.D0 ) THEN
               NBI = NBI+1
               INOI = INOI+1
               ZI(JTI+INOI-1) = ZI(INO)
               IF  (D .GT. DMAX) THEN
                 DMAX = D
                 NUMFIN = ZI(INO)
               ENDIF
             ENDIF
320        CONTINUE

           PRECO = PREC*10
           CALL OREINO ( NOMA, ZI(JTI), NBI, NUMORI,
     &             NUMFIN,ZR(IDCOOR),CRITN,PRECO,IERA,IRET)

           DO 330 IN = 1 , MIN(NBI,20)
             CALL JENUNO(JEXNUM(NOMNOE,ZI(JTI+IN-1)),
     &                          ZK8(KNOLI+20*(INF-1)+IN-1))
 330       CONTINUE

           CALL JEDETR ( '&&PKFOND_INTERS_INF' )
           CALL JEDETR ( '&&PKFOND_TRAV_INF' )
         ENDIF

C ------ CALCUL DE L'INTERSECTION DU PLAN AVEC LES NOEUDS DES MAILLES
C        DEFINISSANT LA LEVRE SUPERIEURE - SAUVEGARDE

         CALL WKVECT ( '&&PKFOND_INTERS_SUP', 'V V I', NBNOE, JINTS )
         CALL CGNOP0 ( NBNOLS, ZR(JCOORS), X0, VECNOR, PREC, NBTRLS,
     &                 ZI(JINTS))
         IF ( NBTRLS .LE. 2 ) THEN
           CALL JEDETR ( '&&PKFOND_INTERS_SUP' )
           GOTO 200
         ENDIF
         CALL WKVECT ( '&&PKFOND_TRAV_SUP', 'V V I', NBTRLS, JTS )

C ---- ORDRE DES NOEUDS
         IF (ILEV.NE.0) THEN
           CALL JENONU(JEXNOM(NOMNOE,ZK8(JNOFOS+INF-1)), NUMORI )
         ENDIF
         NUMFIN = NUMORI
         DMAX = 0.D0
         DMIN = 100.D0
         NBS = 1
         INOS = 1
         ZI(JTS+INOS-1) = NUMORI
         X1 = ZR(IDCOOR-1+3*(NUMORI-1)+1)
         Y1 = ZR(IDCOOR-1+3*(NUMORI-1)+2)
         Z1 = ZR(IDCOOR-1+3*(NUMORI-1)+3)
C
C identification noeuds sur bon cote de la levre (cas fond ferme)
C a partir du noeud le plus proche du fond
         DO 210 IN = 1 , NBTRLS
           INO = JNOLS+ZI(JINTS+IN-1)-1
           IF ( ZI(INO) .EQ. NUMORI ) GOTO 210
           X2 = ZR(IDCOOR-1+3*(ZI(INO)-1)+1)
           Y2 = ZR(IDCOOR-1+3*(ZI(INO)-1)+2)
           Z2 = ZR(IDCOOR-1+3*(ZI(INO)-1)+3)
           D = SQRT( (X2-X1)**2 + (Y2-Y1)**2 + (Z2-Z1)**2 )
           IF  (D .LT. DMIN) THEN
               DMIN = D
               NUMUN = ZI(INO)
           ENDIF
210      CONTINUE

         VECTAN(1) = ZR(IDCOOR-1+3*(NUMUN-1)+1)-X1
         VECTAN(2) = ZR(IDCOOR-1+3*(NUMUN-1)+2)-Y1
         VECTAN(3) = ZR(IDCOOR-1+3*(NUMUN-1)+3)-Z1

         DO 220 IN = 1 , NBTRLS
           INO = JNOLS+ZI(JINTS+IN-1)-1
           IF ( ZI(INO) .EQ. NUMORI ) GOTO 220
           X2 = ZR(IDCOOR-1+3*(ZI(INO)-1)+1)
           Y2 = ZR(IDCOOR-1+3*(ZI(INO)-1)+2)
           Z2 = ZR(IDCOOR-1+3*(ZI(INO)-1)+3)
           D = SQRT( (X2-X1)**2 + (Y2-Y1)**2 + (Z2-Z1)**2 )
           PS=(X2-X1)*VECTAN(1)+(Y2-Y1)*VECTAN(2)+(Z2-Z1)*VECTAN(3)
           IF ( PS .GE. 0.D0 ) THEN
             NBS = NBS+1
             INOS = INOS+1
             ZI(JTS+INOS-1) = ZI(INO)
             IF  (D .GT. DMAX) THEN
               DMAX = D
               NUMFIN = ZI(INO)
             ENDIF
           ENDIF
220      CONTINUE
         PRECO = PREC*10
         CALL OREINO ( NOMA, ZI(JTS), NBS, NUMORI,
     &             NUMFIN,ZR(IDCOOR),CRITN,PRECO,IERA,IRET)

         DO 230 IN = 1 , MIN(NBS,20)
           CALL JENUNO(JEXNUM(NOMNOE,ZI(JTS+IN-1)),
     &                          ZK8(KNOLS+20*(INF-1)+IN-1))
230      CONTINUE

         CALL JEDETR ( '&&PKFOND_INTERS_SUP' )
         CALL JEDETR ( '&&PKFOND_TRAV_SUP' )


 200  CONTINUE

      CALL JEDEMA()
      END
