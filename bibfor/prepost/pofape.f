      SUBROUTINE POFAPE
      IMPLICIT   NONE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 26/03/2012   AUTEUR TRAN V-X.TRAN 
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
     &              IBID, IORDO, IFONC1, IFONC, ILIGN, NBPAR, NBPAPF, J,
     &              NVAL, PARACT (30), NBEPS,IFONC2, IFONCE, IORDOE,
     &              IFONC3, IFONCP, IORDOP
      REAL*8        RBID, PHMAX, CISSIO, SPHERE, PCORR, VAL(2), VMAX,
     &              VMIN, DOMAGE, RCRIT,VRESU(24)
      COMPLEX*16    CBID
      LOGICAL       LHAIGH, LKE, LBID
      INTEGER ICODRE(2), ICODWO, ICODBA, ICODHS
      CHARACTER*8   K8B, NOMTEN(6), NOMRES(2), KDOMM, NOMPAR,NOMMAT,
     &              CARA, RESULT, NOMPA1(20), NOMEPS(6), NOMEPP(6)
      CHARACTER*16  NOMCMD, PHENO, PHENOM,CRITER, NOMFOR, TYPCHA,FORVIE
      CHARACTER*24  FVALE(6), ETVALE(6), PTVALE(6)
C     --- POST_FATI_MULT -----------------------------------------------
      PARAMETER    ( NBPAPF = 29  )
      CHARACTER*3   TYPPPF(NBPAPF), TYPPA1(20)
      CHARACTER*16  NOMPPF(NBPAPF)
      INTEGER      IARG
      DATA  NOMPPF / 'CRITERE' , 'VALE_CRITERE' , 'PRES_HYDRO_MAX' ,
     &               'AMPLI_CISSION' , 'RAYON_SPHERE'   ,
     &               'VALE_MIN' , 'VALE_MAX' , 'DOMMAGE' ,'NBRUP',
     &               'DTAUMA', 'PHYDRM', 'NORMAX', 'NORMOY',  
     &               'EPNMAX', 'EPNMOY', 'DEPSPE', 'EPSPR1', 
     &               'SIGNM1', 'DENDIS', 'DENDIE', 'APHYDR',
     &               'MPHYDR', 'DSIGEQ', 'SIGPR1', 'EPSNM1',
     &               'INVA2S', 'DSITRE', 'DEPTRE', 'EPSPAC'   /       
     
      DATA  TYPPPF /  'K16' , 'R' , 'R' , 'R' , 'R' , 'R' , 'R' , 'R',  
     &                  'R' , 'R' , 'R' , 'R' , 'R' , 'R',  'R',
     &                  'R' , 'R' , 'R' , 'R' , 'R' , 'R',  'R',
     &                  'R' , 'R' , 'R' , 'R' , 'R' , 'R',  'R'/

C     ---------------------------------------------------------------
C     ----------------------------------------------------------------
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
CCCCCCCCCC RECUPERER LA CONTRAINTE
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


CCCCCCCCCC RECUPERER LA DEFORMATION TOTALE
      CALL GETVID ( 'HISTOIRE', 'EPS_XX',1,IARG,1, NOMEPS(1), N1 )
      CALL GETVID ( 'HISTOIRE', 'EPS_YY',1,IARG,1, NOMEPS(2), N2 )
      CALL GETVID ( 'HISTOIRE', 'EPS_ZZ',1,IARG,1, NOMEPS(3), N3 )
      CALL GETVID ( 'HISTOIRE', 'EPS_XY',1,IARG,1, NOMEPS(4), N4 )
      CALL GETVID ( 'HISTOIRE', 'EPS_XZ',1,IARG,1, NOMEPS(5), N5 )
      CALL GETVID ( 'HISTOIRE', 'EPS_YZ',1,IARG,1, NOMEPS(6), N6 )
      NBEPS = N1 + N2 + N3 + N4 + N5 + N6
 
       IF (NBEPS .EQ. 0) THEN
C          DO 46 J = 1 , NBPTOT/2
C             ZR(IORDOE+(J-1)*NBEPS) = 0.D0
C   46     CONTINUE 
         CALL WKVECT ( '&&POFAPE.ORDOE','V V R',NBPTOT*NBEPS, IORDOE )
         GO TO 45
      ENDIF    
     
C
C     --- CHARGEMENT PERIODIQUE ---
C
      ETVALE(1) = NOMEPS(1)//'           .VALE'
      CALL JELIRA ( ETVALE(1), 'LONMAX', NBPTS, K8B )

      NBPTOT = NBPTS
      DO 21 I = 2 , NBEPS
         ETVALE(I) = NOMEPS(I)//'           .VALE'
         CALL JELIRA ( ETVALE(I), 'LONMAX', NBPTS, K8B )
         IF ( NBPTS .NE. NBPTOT ) CALL U2MESS('F','FATIGUE1_21')
  21  CONTINUE
      CALL WKVECT ( '&&POFAPE.ORDOE','V V R',NBPTOT*NBEPS/2, IORDOE )
      CALL JEVEUO ( ETVALE(1), 'L', IFONC2 )
      DO 31 I = 2 , NBEPS
         CALL JEVEUO ( ETVALE(I), 'L', IFONCE )
         DO 36 J = 1 , NBPTOT/2
            IF(ZR(IFONCE+J-1).NE.ZR(IFONC2+J-1)) THEN
               CALL U2MESS('F','FATIGUE1_21')
            ENDIF
            ZR(IORDOE+(J-1)*NBEPS+I-1) = ZR(IFONCE+NBPTOT/2+J-1)
  36     CONTINUE
  31  CONTINUE
      NBPTOT = NBPTOT / 2
      DO 41 J = 1 , NBPTOT
         ZR(IORDOE+(J-1)*NBEPS) = ZR(IFONC2+NBPTOT+J-1)
  41  CONTINUE  
  
  45  CONTINUE  
C

CCCCCCCCCC RECUPERER LA DEFORMATION PLASTIQUE
      CALL GETVID ( 'HISTOIRE', 'EPSP_XX',1,IARG,1, NOMEPP(1), N1 )
      CALL GETVID ( 'HISTOIRE', 'EPSP_YY',1,IARG,1, NOMEPP(2), N2 )
      CALL GETVID ( 'HISTOIRE', 'EPSP_ZZ',1,IARG,1, NOMEPP(3), N3 )
      CALL GETVID ( 'HISTOIRE', 'EPSP_XY',1,IARG,1, NOMEPP(4), N4 )
      CALL GETVID ( 'HISTOIRE', 'EPSP_XZ',1,IARG,1, NOMEPP(5), N5 )
      CALL GETVID ( 'HISTOIRE', 'EPSP_YZ',1,IARG,1, NOMEPP(6), N6 )
      NBEPS = N1 + N2 + N3 + N4 + N5 + N6
      
      IF (NBEPS .EQ. 0) THEN
         CALL WKVECT ( '&&POFAPE.ORDOP','V V R',NBPTOT*NBEPS, IORDOP )
C          DO 47 J = 1 , NBPTOT/2
C             ZR(IORDOP+(J-1)*NBEPS) = 0.D0
C   47     CONTINUE 
  
         GO TO 48
      ENDIF    
C
      PTVALE(1) = NOMEPP(1)//'           .VALE'
      CALL JELIRA ( PTVALE(1), 'LONMAX', NBPTS, K8B )

      NBPTOT = NBPTS
      DO 22 I = 2 , NBEPS
         PTVALE(I) = NOMEPP(I)//'           .VALE'
         CALL JELIRA ( PTVALE(I), 'LONMAX', NBPTS, K8B )
         IF ( NBPTS .NE. NBPTOT ) CALL U2MESS('F','FATIGUE1_21')
  22  CONTINUE
      CALL WKVECT ( '&&POFAPE.ORDOP','V V R',NBPTOT*NBEPS/2, IORDOP )
      CALL JEVEUO ( PTVALE(1), 'L', IFONC3 )
      DO 32 I = 2 , NBEPS
         CALL JEVEUO ( PTVALE(I), 'L', IFONCP )
         DO 37 J = 1 , NBPTOT/2
            IF(ZR(IFONCP+J-1).NE.ZR(IFONC3+J-1)) THEN
               CALL U2MESS('F','FATIGUE1_21')
            ENDIF
            ZR(IORDOP+(J-1)*NBEPS+I-1) = ZR(IFONCP+NBPTOT/2+J-1)
  37     CONTINUE
  32  CONTINUE
      NBPTOT = NBPTOT / 2
      DO 42 J = 1 , NBPTOT
         ZR(IORDOP+(J-1)*NBEPS) = ZR(IFONC3+NBPTOT+J-1)
  42  CONTINUE 
  
  48  CONTINUE 
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
      
      TYPCHA = ' '
      CALL GETVTX ( ' ', 'TYPE_CHARGE', 1,IARG,1, TYPCHA, N1 )
      
      CALL GETVID(' ','FORMULE_GRDEQ',1,IARG,1,NOMFOR,NVAL)
      IF (NVAL .EQ. 0) THEN
          NOMFOR = '        '
      ENDIF
      
      CALL GETVID(' ','FORMULE_VIE',1,IARG,1,FORVIE,NVAL)
      IF (NVAL .EQ. 0) THEN
          FORVIE = '        '
      ENDIF
C
      KDOMM = ' '
      CALL GETVTX ( ' ', 'DOMMAGE', 1,IARG,1, KDOMM, N1 )  

CCCCCCCCCCCCCCCCCCC      
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
      ELSEIF ( CRITER .EQ. 'FORMULE_CRITERE' ) THEN
C              --------------------------
C ANALYSER LE CRITERE
           CALL ANACRI( CRITER,NOMFOR, TYPCHA,'OUI', PARACT, LBID)    

           CALL FMACGR(NBF, NBPTOT, ZR(IORDO), ZR(IORDOE),  
     &                  ZR(IORDOP),CRITER,NOMMAT,
     &                  NOMFOR,KDOMM,FORVIE,VRESU)
C
           DO 43 I = 1, 20
            IF (PARACT(I) .EQ. 1) THEN
               CALL TBAJLI (RESULT, 1,NOMPPF(I+9), IBID,VRESU(I),
     &                      CBID,K8B,ILIGN)
           ENDIF           
43    CONTINUE 

           CALL TBAJLI (RESULT, 1,NOMPPF(2), IBID,VRESU(21),
     &                    CBID,K8B, ILIGN )
           CALL TBAJLI (RESULT, 1,NOMPPF(9), IBID,VRESU(22),CBID,
     &                    K8B, ILIGN ) 
           CALL TBAJLI (RESULT, 1,NOMPPF(8), IBID,VRESU(23),CBID,
     &                    K8B, ILIGN )         
           
      ENDIF
      
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      IF ( CRITER .NE. 'FORMULE_CRITERE' ) THEN
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
C         --- CALCUL DU DOMMAGE ELEMENTAIRE ---

C
C         --- CALCUL DU DOMMAGE ELEMENTAIRE DE WOHLER ---
C             ---------------------------------------
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
     &                        LHAIGH, RBID, DOMAGE )
             ELSEIF ( ICODBA .EQ. 0 ) THEN
                CALL FGDOBA ( NOMMAT, NBC, VMIN, VMAX, LKE, RBID,
     &                        LHAIGH, RBID, DOMAGE )
             ELSEIF ( ICODHS .EQ. 0 ) THEN
                CALL FGDOHS ( NOMMAT, NBC, VMIN, VMAX, LKE, RBID,
     &                        LHAIGH, RBID, DOMAGE )
             ENDIF
C
             CALL TBAJLI (RESULT, 1,NOMPPF(8), IBID,DOMAGE,CBID,
     &              K8B, ILIGN )
C
          ELSEIF ( KDOMM .EQ. ' ' ) THEN
          ELSE
             CALL U2MESS('F','FATIGUE1_20')
          ENDIF
C     
      ENDIF
      
      CALL JEDETR ( '&&POFAPE.ORDO' )
      CALL JEDEMA()
C
      END
