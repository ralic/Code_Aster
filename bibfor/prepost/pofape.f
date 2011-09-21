      SUBROUTINE POFAPE
      IMPLICIT   NONE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 21/09/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     -----------------------------------------------------------------
C     COMMANDE POST_FATIGUE
C              CHARGEMENT PERIODIQUE
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
C     ------------------------------------------------------------------
      INTEGER       N1, N2, N3, N4, N5, N6, NBF, NBPTOT, NBPTS, I, NBC,
     &              IBID, IORDO, IFONC1, IFONC, ILIGN, NBPAR, NBPAPF, J
      REAL*8        RBID, PHMAX, CISSIO, SPHERE, PCORR, VAL(2), VMAX,
     &              VMIN, DOMAGE, RCRIT
      COMPLEX*16    CBID
      LOGICAL       LHAIGH, LKE
      INTEGER ICODRE(2), ICODWO, ICODBA, ICODHS
      CHARACTER*8   K8B, NOMTEN(6), NOMRES(2), NOMMAT, KDOMM, NOMPAR,
     &              CARA, RESULT
      CHARACTER*16  NOMCMD, PHENO, PHENOM, CRITER
      CHARACTER*24  FVALE(6)
C     --- POST_FATI_MULT -----------------------------------------------
      PARAMETER    ( NBPAPF = 8  )
      CHARACTER*3   TYPPPF(NBPAPF)
      CHARACTER*16  NOMPPF(NBPAPF)
      INTEGER      IARG
      DATA  NOMPPF / 'CRITERE' , 'VALE_CRITERE' , 'PRES_HYDRO_MAX' ,
     &               'AMPLI_CISSION' , 'RAYON_SPHERE'   ,
     &               'VALE_MIN' , 'VALE_MAX' , 'DOMMAGE'        /
      DATA  TYPPPF /  'K16' , 'R' , 'R' , 'R' , 'R' , 'R' , 'R' , 'R' /
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
      LKE    = .FALSE.
      LHAIGH = .FALSE.
      NBC    = 1
C
      CALL GETRES ( RESULT, K8B, NOMCMD )
C
C     --- RECUPERATION DE LA FONCTION CHARGEMENT ---
C
      CALL GETVID ( 'HISTOIRE', 'SIGM_XX',1,IARG,1, NOMTEN(1), N1 )
      CALL GETVID ( 'HISTOIRE', 'SIGM_YY',1,IARG,1, NOMTEN(2), N2 )
      CALL GETVID ( 'HISTOIRE', 'SIGM_ZZ',1,IARG,1, NOMTEN(3), N3 )
      CALL GETVID ( 'HISTOIRE', 'SIGM_XY',1,IARG,1, NOMTEN(4), N4 )
      CALL GETVID ( 'HISTOIRE', 'SIGM_XZ',1,IARG,1, NOMTEN(5), N5 )
      CALL GETVID ( 'HISTOIRE', 'SIGM_YZ',1,IARG,1, NOMTEN(6), N6 )
      NBF = N1 + N2 + N3 + N4 + N5 + N6
C
C     --- CHARGEMENT PERIODIQUE ---
C
      FVALE(1) = NOMTEN(1)//'           .VALE'
      CALL JELIRA ( FVALE(1), 'LONMAX', NBPTS, K8B )
      NBPTOT = NBPTS
      DO 20 I = 2 , NBF
         FVALE(I) = NOMTEN(I)//'           .VALE'
         CALL JELIRA ( FVALE(I), 'LONMAX', NBPTS, K8B )
         IF ( NBPTS .NE. NBPTOT ) CALL U2MESS('F','FATIGUE1_21')
  20  CONTINUE
      CALL WKVECT ( '&&POFAPE.ORDO','V V R',NBPTOT/2*NBF, IORDO )
      CALL JEVEUO ( FVALE(1), 'L', IFONC1 )
      DO 30 I = 2 , NBF
         CALL JEVEUO ( FVALE(I), 'L', IFONC )
         DO 35 J = 1 , NBPTOT/2
            IF(ZR(IFONC+J-1).NE.ZR(IFONC1+J-1)) THEN
               CALL U2MESS('F','FATIGUE1_21')
            ENDIF
            ZR(IORDO+(J-1)*NBF+I-1) = ZR(IFONC+NBPTOT/2+J-1)
  35     CONTINUE
  30  CONTINUE
      NBPTOT = NBPTOT / 2
      DO 40 J = 1 , NBPTOT
         ZR(IORDO+(J-1)*NBF) = ZR(IFONC1+NBPTOT+J-1)
  40  CONTINUE
C
C     --- CREATION DE LA TABLE ---
C
      CALL TBCRSD ( RESULT , 'G')
      CALL TBAJPA ( RESULT, NBPAPF, NOMPPF, TYPPPF )
C
      NOMMAT = ' '
      CALL GETVID ( ' ', 'MATER', 1,IARG,1, NOMMAT, N1 )
C
C     --- DETERMINATION DES CRITERES---
C
      CRITER = ' '
      CALL GETVTX ( ' ', 'CRITERE', 1,IARG,1, CRITER, N1 )
C
      CALL TBAJLI (RESULT, 1,NOMPPF(1), IBID,RBID,CBID,CRITER, 0 )
      CALL TBNULI (RESULT, 1,NOMPPF(1), IBID,RBID,CBID,CRITER,
     &             RBID, K8B, ILIGN )
      IF ( ILIGN .LE. 0 ) ILIGN = 0
C
      NOMRES(1) = 'D0'
      NOMRES(2) = 'TAU0'
      NBPAR  = 0
      NOMPAR = ' '
      CALL RCVALE ( NOMMAT, 'FATIGUE ', NBPAR, NOMPAR, RBID,
     &                                  2, NOMRES, VAL, ICODRE, 2)
C
      IF ( CRITER .EQ. 'CROSSLAND' ) THEN
C          -----------------------
           CALL FMCROS ( NBF, NBPTOT, ZR(IORDO), VAL(1), VAL(2), RCRIT,
     &                   PHMAX, CISSIO )
C
           CALL TBAJLI (RESULT, 1,NOMPPF(2), IBID,RCRIT,CBID,K8B,ILIGN)
           CALL TBAJLI (RESULT, 1,NOMPPF(3), IBID,PHMAX,CBID,K8B,ILIGN)
           CALL TBAJLI (RESULT, 1,NOMPPF(4), IBID,CISSIO,CBID,K8B,ILIGN)
C
      ELSEIF ( CRITER .EQ. 'PAPADOPOULOS' ) THEN
C              --------------------------
           CALL FMPAPA ( NBF, NBPTOT, ZR(IORDO), VAL(1), VAL(2), RCRIT,
     &                   PHMAX, SPHERE )
C
           CALL TBAJLI (RESULT, 1,NOMPPF(2), IBID,RCRIT,CBID,K8B,ILIGN)
           CALL TBAJLI (RESULT, 1,NOMPPF(3), IBID,PHMAX,CBID,K8B,ILIGN)
           CALL TBAJLI (RESULT, 1,NOMPPF(5), IBID,SPHERE,CBID,K8B,ILIGN)
C
      ENDIF
C
C     --- CORRECTION POUR CALCUL DU DOMMAGE ----
C
      CALL GETVR8 ( ' ', 'COEF_CORR', 1,IARG,1, PCORR, N1 )
      IF ( N1 .NE. 0 ) THEN
         VMAX = 2.D0*(RCRIT+VAL(2))*PCORR
         VMIN = 0.D0
      ELSE
         VMAX = 2.D0*(RCRIT+VAL(2))*(VAL(1)/VAL(2))
         VMIN = 0.D0
      ENDIF
      CALL TBAJLI (RESULT, 1,NOMPPF(6), IBID,VMIN,CBID,K8B,ILIGN)
      CALL TBAJLI (RESULT, 1,NOMPPF(7), IBID,VMAX,CBID,K8B,ILIGN)
C
C     --- CALCUL DU DOMMAGE ELEMENTAIRE ---
C
      KDOMM = ' '
      CALL GETVTX ( ' ', 'DOMMAGE', 1,IARG,1, KDOMM, N1 )
C
C     --- CALCUL DU DOMMAGE ELEMENTAIRE DE WOHLER ---
C         ---------------------------------------
      IF ( KDOMM .EQ. 'WOHLER' ) THEN
         PHENO = 'FATIGUE'
         CALL RCCOME ( NOMMAT, PHENO, PHENOM, ICODRE(1) )
         IF ( ICODRE(1) .EQ. 1 ) CALL U2MESS('F','FATIGUE1_24')
         CARA = 'WOHLER'
         CALL RCPARE ( NOMMAT, PHENO, CARA, ICODWO )
         CARA = 'A_BASQUI'
         CALL RCPARE ( NOMMAT, PHENO, CARA, ICODBA )
         CARA = 'A0'
         CALL RCPARE ( NOMMAT, PHENO, CARA, ICODHS )
         IF ( ICODWO .EQ. 0 ) THEN
            CALL FGDOWH ( NOMMAT, NBC, VMIN, VMAX, LKE, RBID,
     &                    LHAIGH, RBID, DOMAGE )
         ELSEIF ( ICODBA .EQ. 0 ) THEN
            CALL FGDOBA ( NOMMAT, NBC, VMIN, VMAX, LKE, RBID,
     &                    LHAIGH, RBID, DOMAGE )
         ELSEIF ( ICODHS .EQ. 0 ) THEN
            CALL FGDOHS ( NOMMAT, NBC, VMIN, VMAX, LKE, RBID,
     &                    LHAIGH, RBID, DOMAGE )
         ENDIF
C
         CALL TBAJLI (RESULT, 1,NOMPPF(8), IBID,DOMAGE,CBID,K8B, ILIGN )
C
      ELSEIF ( KDOMM .EQ. ' ' ) THEN
      ELSE
         CALL U2MESS('F','FATIGUE1_20')
      ENDIF
C
      CALL JEDETR ( '&&POFAPE.ORDO' )
      CALL JEDEMA()
C
      END
