      SUBROUTINE LCHBR2(TYPMOD,OPTION,IMATE,CRIT,SIGM,EPSM,TD,TF,
     &     TR,DEPSM,VIM,VIP,DSPDP1,DSPDP2,SIPM,SIPP,
     &     SIGP,DSIDEP,DSIDP1,DSIDP2,IRET)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 28/03/2007   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ======================================================================
C TOLE CRP_20
C TOLE CRP_21
C ======================================================================
      IMPLICIT      NONE
      INTEGER       IMATE,IRET
      REAL*8        DEPSM(6),VIM(*),VIP(*),SIGP(6),DSIDEP(6,6)
      REAL*8        SIGM(6),TD,TF,TR,DSIDP1(6),DSIDP2(6),EPSM(6)
      REAL*8        CRIT(*),SIPM,SIPP,DSPDP1,DSPDP2
      CHARACTER*8   TYPMOD(*)
      CHARACTER*16  OPTION
C ======================================================================
C --- LOI DE COMPORTEMENT DE TYPE HOEK BROWN EN CONTRAINTES TOTALES ----
C --- ELASTICITE ISOTROPE ----------------------------------------------
C --- CRITERE DE PLASTICITE DE HEOK BROWN ------------------------------
C --- ECOULEMENT PLASTIQUE DE DRUCKER PRAGER ---------------------------
C ======================================================================
C IN  OPTION  OPTION DE CALCUL (RAPH_MECA, RIGI_MECA_TANG OU FULL_MECA)
C IN  IMATE   NATURE DU MATERIAU
C IN  CRIT    CRITERES LOCAUX
C               CRIT(1) = NOMBRE D ITERATIONS MAXI A CONVERGENCE
C                         (ITER_INTE_MAXI == ITECREL)
C               CRIT(3) = VALEUR DE LA TOLERANCE DE CONVERGENCE
C                         (RESI_INTE_RELA == RESCREL)
C IN  SIGM    CHAMP DE CONTRAINTES EFFECTIVES A T-
C IN  EPSM    CHAMP DE DEFORMATIONS A T-
C IN  TD,TF,TR TEMPERATURES A T-, T+ ET DE REFERENCE
C IN  DEPSM   INCREMENT DU CHAMP DE DEFORMATION
C IN  VIM     VARIABLES INTERNES EN T-
C               1   : PARAMETRE D ECROUISSAGE
C               2   : DEFORMATION PLASTIQUE VOLUMIQUE CUMULEE
C               3   : INDICATEUR DISSIPATIF (1) OU ELASTIQUE (0)
C VAR VIP     VARIABLES INTERNES EN T+
C              IN  ESTIMATION (ITERATION PRECEDENTE)
C              OUT CALCULEES
C IN  DSPDP1  DERIVEE DE SIP PAR RAPPORT A PRE1
C IN  DSPDP2  DERIVEE DE SIP PAR RAPPORT A PRE2
C IN  SIPM    TERME DE PRESSION A T-
C IN  SIPP    TERME DE PRESSION A T+
C OUT SIGP    CONTRAINTES EFFECTIVES A T+
C OUT DSIDEP  MATRICE TANGENTE (DSIGPDEPS)
C OUT IRET    CODE RETOUR (0 = OK)
C OUT DSIDP1  DERIVEE DE SIG EFFECTIVES PAR RAPPORT A PRE1
C OUT DSIDP2  DERIVEE DE SIG EFFECTIVES PAR RAPPORT A PRE2
C ======================================================================
C     INFO   MATERF   COEFFICIENTS MATERIAUX A T+DT
C                     MATERF(*,1) = CARACTERISTIQUES ELASTIQUES
C                     MATERF(*,2) = CARACTERISTIQUES PLASTIQUES
C            NDT      NOMBRE DE COMPOSANTES TOTALES DES TENSEURS
C            NDI      NOMBRE DE COMPOSANTES DIRECTES DES TENSEURS
C            NVI      NOMBRE DE VARIABLES INTERNES
C            VP       VALEURS PROPRES DE LA MATRICE SE
C            VECP     VECTEURS PROPRES ASSOCIES A SE
C            TOLER    VALEUR TOLERANCE DE CONVERGENCE
C            ITMAX    NB ITERATIONS MAX A CONVERGENCE
C ======================================================================
      INTEGER      NDT,NDI,ITMAX,II,ITERI,ITER,JJ,NR,NBMAT
      REAL*8       GPM,GM,GP,ETAM,ETAP,AUX,SIG3,MU,K,NEUF,AUX2,AUX3
      REAL*8       MATERF(16,2),ALPHAT,SIGB(6),DDIFDG,AUX4,MATERD(16,2)
      REAL*8       DEPS(6),EPSP(6),SIGE(6),SE(6),SIGEB(6)
      REAL*8       TOLER,SEQ,I1E,SEUIL,PLAS,DG,SIGEQE,UN,SEUIL2
      REAL*8       HOOKF(6,6),DEUX,TROIS,TRACE,VI
      REAL*8       VP(3),VECP(3,3),DETADG,DGDL,DSDSIP(6),ETA,GAM
      REAL*8       INCRG,GNP,DGNP,ETANP,VH,VG,ZERO,DSIG3,GRUP,GRES
      REAL*8       PARAME(4),DERIVE(5),PI,R8PI,FMOINS,PPHI0,PPHI1,PPHI2
      CHARACTER*3  MATCST
      CHARACTER*8  MOD
      LOGICAL      RESI,RIGI,DEB
      INTEGER      JPARA,JDERIV,IFM,INV,NVI
      CHARACTER*16 PARECR,DERECR
      CHARACTER*10 CVP1,CVP2,CVP3
C ======================================================================
      PARAMETER       ( UN     =  1.0D0  )
      PARAMETER       ( DEUX   =  2.0D0  )
      PARAMETER       ( TROIS  =  3.0D0  )
      PARAMETER       ( NEUF   =  9.0D0  )
      PARAMETER       ( ZERO   =  0.0D0  )
C ======================================================================
      COMMON /TDIM/   NDT, NDI
C ======================================================================
C --- INITIALISATION DES PARAMETRES DE CONVERGENCE ---------------------
C ======================================================================
      MOD    = TYPMOD(1)
      ITMAX  = INT(CRIT(1))
      TOLER  = CRIT(3)
      PI     = R8PI()
      PI     = PI/180.D0
      NBMAT  = 16
C ======================================================================
C --- RECUPERATION DES PARAMETRES DE LA LOI ----------------------------
C ======================================================================
      CALL HBRMAT(MOD,IMATE,NBMAT,ZERO,MATERD,MATERF,MATCST,
     &   NDT,NDI,NR,NVI)
C ======================================================================
C --- INITIALISATION ---------------------------------------------------
C ======================================================================
      GM = VIM(1)
      IF (GM.LT.0.0D0) THEN
         CALL U2MESS('F','ALGORITH3_88')
      ENDIF
      IRET = 0
      ITERI = 0
      CALL LCINMA(0.0D0,HOOKF)
      CALL LCINMA(0.0D0,DSIDEP)
C =====================================================================
C --- CALCUL DES PARAMETRES D ECROUISSAGE -----------------------------
C =====================================================================
      CALL HBVAEC(GM,NBMAT,MATERF,PARAME)
      ETAM = DEUX*SIN(PARAME(4)*PI)/(TROIS+SIN(PARAME(4)*PI))
      RESI   = OPTION(1:9).EQ.'FULL_MECA' .OR.
     &         OPTION(1:9).EQ.'RAPH_MECA'
      RIGI   = OPTION(1:9).EQ.'FULL_MECA' .OR.
     &         OPTION(1:9).EQ.'RIGI_MECA'
      IF ( (OPTION(1:9).NE.'RIGI_MECA') .AND.
     &     (OPTION(1:9).NE.'FULL_MECA') .AND.
     &     (OPTION(1:9).NE.'RAPH_MECA') )  THEN
        CALL U2MESS('F','ALGORITH4_47')
      ENDIF
C =====================================================================
C --- OPERATEUR ELASTIQUE LINEAIRE ISOTROPE ---------------------------
C =====================================================================
      CALL LCOPLI('ISOTROPE',MOD,MATERF(1,1),HOOKF)
C ======================================================================
C --- RETRAIT DE LA DEFORMATION DUE A LA DILATATION THERMIQUE ----------
C ======================================================================
      CALL LCDEDI ( 'RIGI',1,1,NBMAT,  MATERD, MATERF,
     &             TD,TF,TR,DEPSM, EPSM, DEPS,   EPSP )
C =====================================================================
C --- INTEGRATION ELASTIQUE : SIGE = HOOKF EPSP + SIP -----------------
C =====================================================================
      CALL LCPRMV(HOOKF,DEPS,SIGEB)
      CALL LCSOVE(SIGEB,SIGM,SIGE)
      DO 5 II=1,NDI
        SIGE(II) = SIGE(II)+SIPP
 5    CONTINUE
      CALL LCDEVI(SIGE,SE)
C      CALL PSCAL(NDT,SE,SE,SEQ)
      CALL LCPRSC(SE,SE,SEQ)
      SIGEQE = SQRT(TROIS*SEQ/DEUX)
      I1E    = TRACE(NDI,SIGE)
C ======================================================================
C --- CALCUL DES CONTRAINTES -------------------------------------------
C ======================================================================
      IF (RESI) THEN
C ======================================================================
C --- CALCUL DU CRITERE ELASTIQUE --------------------------------------
C ======================================================================
        CALL HBRCVX(SIGE,VIM,NBMAT,MATERF,SEUIL,VP,VECP)
C ======================================================================
C --- CALCUL DE DELTA GAMMA --------------------------------------------
C ======================================================================
        IF (SEUIL.GT.TOLER) THEN
 3          CONTINUE
            PLAS = 1.0D0
            DG = 0.0D0
            CALL HBCREL(VP,GM,DG,NBMAT,MATERF,SIGEQE,I1E,
     &          ETAM,PARAME,SEUIL)
            FMOINS = SEUIL
C ======================================================================
C --------- CALCUL DE L INCREMENT DE GAMMA PAR METHODE DE NEWTON -------
C ======================================================================
C --------- INITIALISATION DES VARIABLES -------------------------------
C ======================================================================
            ITER   = 0
            INCRG  = 0.D0
            DGNP   = DG
            GNP    = GM
            ETANP  = ETAM
            CALL CALCVH(NBMAT,MATERF,ETANP,VP,SIGEQE,VH,VG)
            CALL HBDERI(GNP,NBMAT,MATERF,VG,ETANP,PARAME,DERIVE)
C ======================================================================
C --------- PREMIERE ITERATION -----------------------------------------
C ======================================================================
            CALL HBCALC(SEUIL,GNP,DGNP,NBMAT,MATERF,I1E,SIGEQE,VP,ETANP,
     &           VH,VG,PARAME,DERIVE,INCRG)
 2          CONTINUE
            GNP    = GNP + INCRG
            DGNP   = DGNP + INCRG
C ======================================================================
C -- ON OBTIENT DGAMMA_P NEGATIF : ON ESSAIE DE DECOUPER LE PAS DE TEMPS
C ======================================================================
            IF (DGNP.LT.0.D0) THEN
              CALL U2MESS('I','ALGORITH4_57')
              ITERI = 1
              GOTO 100
            ENDIF
            CALL HBVAEC(GNP,NBMAT,MATERF,PARAME)
            ETANP = DEUX*SIN(PARAME(4)*PI)/(TROIS+SIN(PARAME(4)*PI))
            CALL HBCREL(VP,GNP,DGNP,NBMAT,MATERF,SIGEQE,I1E,ETANP,
     &           PARAME,SEUIL)
C ======================================================================
C ---------- IL Y A CONVERGENCE ----------------------------------------
C ======================================================================
            IF ((ABS(SEUIL).LT.TOLER).OR.
     &              (ABS(SEUIL/FMOINS).LT.TOLER)) THEN
C ======================================================================
C --------- ON DETECTE LES SOLUTIONS NON ADMISSIBLES -------------------
C ======================================================================
               AUX = SIGEQE*(ETANP+UN)/(TROIS*MATERF(4,1))
               IF (DGNP.GT.AUX) THEN
                  CALL U2MESS('I','ALGORITH4_58')
                  ITERI = 1
                  GOTO 100
               ENDIF
               DG    = DGNP
               ITERI = 0
C ======================================================================
C --------- LE NOMBRE MAX D ITERATIONS N A PAS ETE ATTEINT -------------
C ======================================================================
            ELSE IF (ITER.LT.ITMAX) THEN
               ITER  = ITER + 1
               ITERI = 0
               CALL CALCVH(NBMAT,MATERF,ETANP,VP,SIGEQE,VH,VG)
               CALL HBDERI(GNP,NBMAT,MATERF,VG,ETANP,PARAME,DERIVE)
               CALL HBCALC(SEUIL,GNP,DGNP,NBMAT,MATERF,I1E,SIGEQE,VP,
     &           ETANP,VH,VG,PARAME,DERIVE,INCRG)
               GOTO 2
C ======================================================================
C --------- LE NOMBRE MAX D ITERATIONS A ETE ATTEINT -------------------
C ======================================================================
            ELSE
C ======================================================================
C --------- ON ESSAIE DE DECOUPER LE PAS DE TEMPS ----------------------
C ======================================================================
               CALL U2MESS('I','ALGORITH4_59')
               ITERI = 1
               GOTO 100
            ENDIF
 100        CONTINUE
            IF (ITERI.GE.1) GOTO (1),ITERI
C ======================================================================
            GP = GNP
            ETAP = ETANP
            CALL HBMAJS(DG,NBMAT,MATERF,SE,I1E,SIGEQE,ETAP,SIGP)
C ---------- IL FAUT RENVOYER LES CONTRAINTES EFFECTIVES ---------------
            DO 17 II=1,NDI
               SIGP(II) = SIGP(II)-SIPP
 17         CONTINUE
            VIP(1) = VIM(1) + DG
            VIP(2) = VIM(2) + TROIS*ETAP*DG/(ETAP+UN)
            VIP(3) = PLAS
            IF (RIGI) THEN
                MU  = MATERF(4,1)
                K   = MATERF(5,1)
                SIG3 = VP(3)*(UN - TROIS*MU*DG/(SIGEQE*(ETAP+UN))) +
     &              (I1E - NEUF*K*ETAP*DG/(ETAP+UN))/TROIS
            ENDIF
         ELSE
            PLAS = 0.0D0
            DO 30 II=1,NDT
               SIGP(II) = SIGE(II)
 30         CONTINUE
C ---------- IL FAUT RENVOYER LES CONTRAINTES EFFECTIVES ---------------
            DO 31 II=1,NDI
               SIGP(II) = SIGP(II) - SIPP
 31         CONTINUE
            GP = GM
            ETAP = ETAM
            VIP(1) = VIM(1)
            VIP(2) = VIM(2)
            VIP(3) = PLAS
         ENDIF
      ENDIF
C ======================================================================
C --- CALCUL DE LA MATRICE TANGENTE ------------------------------------
C ======================================================================
      IF (RIGI) THEN
         GRUP = MATERF(1,2)
         GRES = MATERF(2,2)
         PPHI1 = MATERF(9,2)
         PPHI2 = MATERF(15,2)
         PPHI0 = MATERF(16,2)
         IF (OPTION(1:9).EQ.'RIGI_MECA') THEN
            VI = VIM(3)
            DG = 0.0D0
            ETA = ETAM
            GAM = GM
            CALL LCHBVP(SIGE,VP,VECP)
            SIG3 = VP(3)+I1E/3.0D0
         ELSE
            VI = VIP(3)
            ETA = ETAP
            GAM = GP
         ENDIF
         IF (VI.EQ.0) THEN
             CALL LCEQMA(HOOKF,DSIDEP)
             DO 19 II=1,NDI
               DSDSIP(II) = 1.0D0
 19          CONTINUE
             DO 18 II=NDI+1,6
               DSDSIP(II) = 0.0D0
 18          CONTINUE
         ELSE
             IF (GAM.LT.GRUP) THEN
               DETADG = 6.0D0*(PPHI1-PPHI0)*PI*COS(PARAME(4)*PI) /
     &                (GRUP*(TROIS+SIN(PARAME(4)*PI))**2)
             ELSE IF (GAM.LT.GRES) THEN
               DETADG = 6.0D0*(PPHI2-PPHI1)*PI*COS(PARAME(4)*PI) /
     &                ((GRES-GRUP)*(TROIS+SIN(PARAME(4)*PI))**2)
             ELSE
                DETADG = 0.D0
             ENDIF
             DGDL   = ETA+UN
             CALL HBDERI(GAM,NBMAT,MATERF,ZERO,ETA,PARAME,DERIVE)
             CALL HBMATA(SE,DG,ETA,I1E,SIGEQE,VP,VECP,PARAME,
     &           DERIVE,SIG3,DETADG,DGDL,NBMAT,MATERF,DSIDEP)
             CALL HBDSDP(SE,DG,ETA,SIGEQE,VP,PARAME,DERIVE,
     &           NBMAT,MATERF,SIG3,DETADG,DGDL,DSDSIP)
         ENDIF
C ======================================================================
C --- ON A CALCULE LA DERIVEE DES CONTRAINTES TOTALES, ET ON RENVOIE ---
C --- CELLE DES CONTRRAINTES EFFECTIVES --------------------------------
C ======================================================================
         DO 32 II=1,NDI
           DSDSIP(II) = DSDSIP(II)-1.0D0
 32      CONTINUE
C ======================================================================
C --- CALCUL DE LA DERIVEE DES CONTRAINTES TOTALES PAR RAPPORT A P1,P2 -
C ======================================================================
         DO 33 II=1,NDT
           DSIDP1(II) = DSDSIP(II)*DSPDP1
           DSIDP2(II) = DSDSIP(II)*DSPDP2
 33      CONTINUE
      ENDIF
C ======================================================================
      IRET = 0
      GOTO 9999
 1    CONTINUE
      IRET = 1
 9999 CONTINUE
C ======================================================================
      END
