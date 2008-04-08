      SUBROUTINE NMCHAB (FAMI,KPG,KSP,NDIM,TYPMOD,IMATE,COMPOR,CRIT,
     &                   INSTAM,INSTAP,DEPS,SIGM,VIM,
     &                   OPTION,SIGP,VIP,DSIDEP,IRET)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 07/04/2008   AUTEUR PROIX J-M.PROIX 
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
C GENERAL PUBLIC LICENSE FOR MORE DKVIILS.
C
C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C RESPONSABLE JMBHH01 J.M.PROIX
C.======================================================================
      IMPLICIT NONE
C
C      NMCHAB   -- REALISE L'INTEGRATION DE LA LOI DE COMPORTEMENT
C                  'VISC_CIN1_CHAB' :
C                  INTEGRATION DU MODELE DE COMPORTEMENT ELASTOPLASTIQUE
C                  DE CHABOCHE A ECROUISSAGE CINEMATIQUE NON LINEAIRE
C                  ET ISOTROPE POUR LES ELEMENTS ISOPARAMETRIQUES
C                  2D ET 3D EN PETITES DEFORMATIONS.
C                  CE MODELE COMPORTE UNE SEULE VARIABLE CINEMATIQUE,
C                  IL PREND EN COMPTE TOUTES LES VARIATIONS DES
C                  DES COEFFICIENTS AVEC LA TEMPERATURE ET POSSEDE
C                  UN TERME D'ECROUISSAGE SUR LE TERME DE RAPPEL.
C                  CE MODELE EST INTEGRE PAR LA RESOLUTION D'UNE
C                  SEULE EQUATION SCALAIRE NON LINEAIRE EN P.
C
C   ARGUMENT        E/S  TYPE         ROLE
C    NDIM           IN    I       DIMENSION DE L'ESPACE (2 OU 3)
C    TYPMOD(*)      IN    K8      TYPMOD(1) EST LE TYPE DE MODELISATION
C                                 3D, D_PLAN, AXIS OU C_PLAN
C                                 TYPMOD(2) = 'INCO' POUR LES
C                                 MATERIAUX INCOMPRESSIBLES
C    IMATE          IN    I       ADRESSE DU MATERIAU CODE
C    COMPOR(3)      IN    K16     COMPOR(1) EST LE NOM DE LA RELATION
C                                 DE COMPORTEMENT
C                                 COMPOR(2) EST LE NOMBRE DE VARIABLES
C                                 INTERNES PAR POINT D'INTEGRATION
C                                 COMPOR(3) EST UN K16 DESIGNANT
C                                 UNE HYPOTHESE SUR LES DEFORMATIONS
C    CRIT(6)        IN    R       TABLEAU DE CRITERES LOCAUX
C                                 DE CONVERGENCE :
C                                 CRIT(1) : NOMBRE D'ITERATIONS
C                                 MAXIMUM A LA CONVERGENCE
C                                 CRIT(2) : TYPE DE JACOBIEN
C                                 A L'INSTANT T+DT
C                                  SI CRIT(2) = 0 ON A UNE FORMULATION
C                                  EN VITESSE ET LA MATRICE EST
C                                  SYMETRIQUE
C                                  SI CRIT(2) = 1 ON A UNE FORMULATION
C                                  INCREMENTALE ET LA MATRICE EST
C                                  NON SYMETRIQUE
C                                 CRIT(3) EST LA VALEUR DE TOLERANCE
C                                 DE CONVERGENCE
C                                 CRIT(4) EST LE NOMBRE D'INCREMENTS
C                                 POUR LE REDECOUPAGE LOCAL DU PAS DE
C                                 TEMPS
C                                  SI CRIT(4) = -1, 0 OU 1 , IL N'Y A
C                                  PAS DE REDECOUPAGE
C                                 CRIT(5) EST LE TYPE D'INTEGRATION
C                                 LOCALE POUR LA LOI DE COMPORTEMENT
C                                   SI CRIT(5) = 0 , L'INTEGRATION
C                                   EST EULER IMPLICITE
C                                   SI CRIT(5) = 1 , ON FAIT UNE
C    INSTAM         IN    R       INSTANT DU CALCUL PRECEDENT
C    INSTAP         IN    R       INSTANT DU CALCUL
C    DEPS(6)        IN    R       INCREMENT DE DEFORMATIONS, I.E.
C                                 IL S'AGIT DE B.DELTA_U
C    SIGM(6)        IN    R       CONTRAINTES A L'INSTANT DU CALCUL
C                                 PRECEDENT
C    VIM(2+6*NBVAR) IN    R       VARIABLES INTERNES A L'INSTANT DU
C                                 CALCUL PRECEDENT
C                                 VIM(1) = DEFORMATION PLASTIQUE
C                                          CUMULEE
C                                 VIM(2) = 1 SI LE POINT D'INTEGRATION
C                                            COURANT KVIIT 'PLASTIQUE'
C                                        = 0 SINON
C                                 VIM(3->8)  VARIABLES INTERNES ALPHA-
C    OPTION         IN   K16      OPTION DE CALCUL :
C                                   SI = 'RIGI_MECA_TANG'
C                                     CETTE OPTION SERT LORS DE LA
C                                     PREDICTION, LES VARIABLES
C                                     INTERNES NE SONT PAS CALCULEES
C                                   SI = 'FULL_MECA'
C                                     ON REACTUALISE LA MATRICE TANGENTE
C                                     A CHAQUE ITERATION ET ON MET A
C                                     JOUR LES CONTRAINTES ET LES
C                                     VARIABLES INTERNES
C                                   SI = 'RAPH_MECA'
C                                     ON NE REACTUALISE LA MATRICE PAS
C                                     TANGENTE, ON MET A JOUR LES
C                                     CONTRAINTES ET LES VARIABLES
C    SIGP(6)        OUT   R       CONTRAINTES A L'INSTANT ACTUEL
C    VIP(2+6*NBVAR) OUT   R       VARIABLES INTERNES A L'INSTANT ACTUEL
C                                 VIP(1) = DEFORMATION PLASTIQUE
C                                          CUMULEE
C                                 VIP(2) = 1 SI LE POINT D'INTEGRATION
C                                            COURANT EST 'PLASTIQUE'
C                                        = 0 SINON
C                                 VIP(3->8)  VARIABLES INTERNES ALPHA+
C    DSIDEP(6,6)    OUT   R       MATRICE DE COMPORTEMENT TANGENTE
C
C               ATTENTION LES TENSEURS ET MATRICES SONT RANGES DANS
C               L'ORDRE :  XX,YY,ZZ,SQRT(2)*XY,SQRT(2)*XZ,SQRT(2)*YZ
C               -----------------------------------------------------
C
C    IRET           OUT   I    CODE RETOUR DE  L'INTEGRATION DE LA LDC
C                              IRET=0 => PAS DE PROBLEME
C                              IRET=1 => ABSENCE DE CONVERGENCE DANS
C                                        LORS DE L'INTEGRATION DE LA
C                                        LOI VISC_CINX_CHAB
C
C -----  ARGUMENTS
          INTEGER             KPG,KSP,NDIM,IMATE,NBVAR,IRET,IRET1,IRET2
           REAL*8             CRIT(6),INSTAM,INSTAP
           REAL*8             DEPS(6),DEUXMU,DILATM,DILATP,EPSM(6)
           REAL*8             SIGM(6),VIM(*),SIGP(6),VIP(*),DSIDEP(6,6)
           CHARACTER*(*)      FAMI
           CHARACTER*8        TYPMOD(*)
           CHARACTER*16       COMPOR(3),OPTION
C -----  VARIABLES LOCALES
           REAL*8      DEPSTH(6),VALRES(10),PM,C2INF,GAMM20,M2P
           REAL*8      PLAST,DEPSMO,SIGMMO,E,NU,TROISK,EPSMMO
           REAL*8      RP,RPM,SIELEQ,SEUIL,DP,TM,TP,TREF,EPSMDV(6)
           REAL*8      COEF,SIGEDV(6),KRON(6),DEPSDV(6),EPSPM(6)
           REAL*8      SIGMDV(6),SIGPDV(6),EM,NUM,KSIP(6),QP
           REAL*8      TROIKM,DEUMUM,SIGMP(6),SIGEL(6),PP,EPSPP(6)
           REAL*8      UN,RAC2,C2P
           REAL*8      R0,RINF,B,CINF,K,W,MAT(16),C2M,GAMM2P,DE2P,DB2P
           REAL*8      DGAMAP,DCP,DRP,DMP,AP,BP,EP,DENOMI,DDENOM,H2A2
           REAL*8      L1P,L2P,L3P,DAP,DBP,DEP,MP,GAMMA0,GAMMAP,L22P
           REAL*8      ISP,IAP,IA2P,IP,H1S,H2P,H1A1,H2S,H1A2,H2A1
           REAL*8      S(6),DEPSP(6),ALFAM(6),ALFA(6),DALFA(6),RPVP
           REAL*8      CM,MU,AINF,CP,HP,SEQ,DGAM2P,DC2P,DM2P,E2P,B2P
           REAL*8      PDEV(6,6),DSIDE(6,6),ALFA2M(6),ALFA2(6),DALFA2(6)
           REAL*8      DT,KVI,VALDEN,CORR,VP,DVP,UNSKVI,KSIM(6),QM,RPVM
           REAL*8      GQ0,GQMAX,MUMEM,GQP,GQM,MATEL(4)
           INTEGER     NDIMSI,I,J,L,NITER,VISC,MEMO
           DATA        KRON/1.D0,1.D0,1.D0,0.D0,0.D0,0.D0/
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
C --- POUR L'INSTANT ON NE TRAITE PAS LE CAS DES CONTRAINTES PLANES :
C     -------------------------------------------------------------
      IF (TYPMOD(1) .EQ. 'C_PLAN') THEN
        CALL U2MESS('F','ALGORITH6_66')
      ENDIF
C
      CALL NMCHAM(FAMI,KPG,KSP,IMATE,COMPOR,
     &            MATEL,MAT,NBVAR,MEMO,VISC,COEF)

      UN     = 1.0D0
      RAC2   = SQRT(2.D0)
      EM     = MATEL(1)
      NUM    = MATEL(2)
      DEUMUM = EM/(UN+NUM)
      TROIKM = EM/(UN-2.D0*NUM)
      E      = MATEL(3)
      NU     = MATEL(4)
      DEUXMU = E/(UN+NU)
      TROISK = E/(UN-2.D0*NU)
      R0     = MAT(1)
      RINF   = MAT(2)
      B      = MAT(3)
      CINF   = MAT(4)
      K      = MAT(5)
      W      = MAT(6)
      GAMMA0 = MAT(7)
      AINF   = MAT(8)
      C2INF  = MAT(9) 
      GAMM20 = MAT(10) 
      IF (VISC.EQ.1) THEN
         VALDEN= MAT(11)
         KVI   = MAT(12)
      ELSE
         VALDEN=UN
         KVI=0.D0
      ENDIF

C
C --- INITIALISATIONS :
C     ===============
C
      NDIMSI = 2*NDIM
      DP     = 0.D0
      SEUIL  = 0.D0
      DT = INSTAP - INSTAM
      PM    = VIM(1)
      PLAST = VIM(2)
      NITER=0
      IF (MEMO.EQ.0) THEN
         RPM    = RINF + (R0-RINF)*EXP(-B*PM)
      ELSEIF (MEMO.EQ.1) THEN
         RPVM   = VIM(15) 
         RPM    = RPVM + R0
         QM     = VIM(16)
         DO 101 I=1,6
            KSIM(I) = VIM(I+16)
  101    CONTINUE
         DO 91 I = 1,NDIMSI
            EPSPM(I)=VIM(22+I)
 91      CONTINUE
      ENDIF
      CM     = CINF  * (UN + (K-UN)*EXP(-W*PM))
      C2M    = C2INF * (UN + (K-UN)*EXP(-W*PM))
C
C --- MISE AU FORMAT DES CONTRAINTES DE RAPPEL :
C     ========================================
      DO 10 I=1,3
        ALFAM(I) = VIM(I+2)
        IF (NBVAR.EQ.2) THEN
          ALFA2M(I) = VIM(I+8)
        ELSE
          ALFA2M(I)=0.D0
        ENDIF
  10  CONTINUE
      DO 20 I=4,NDIMSI
        ALFAM(I) = VIM(I+2)*RAC2
        IF (NBVAR.EQ.2) THEN
           ALFA2M(I) = VIM(I+8)*RAC2
        ELSE
          ALFA2M(I)=0.D0
        ENDIF
  20  CONTINUE
C
C --- CALCUL DE DEPSMO ET DEPSDV :
C     ==========================
      DEPSMO = 0.D0
C
C ---   DEPSTH = B*DELTA _U - DELTA_EPS_THERMIQUE :
C       -----------------------------------------
      DO 30 I=1,3
        DEPSTH(I)   = DEPS(I) - COEF
        DEPSMO      = DEPSMO  + DEPSTH(I)
 30   CONTINUE
C
      DEPSMO = DEPSMO/3.D0
C
      DO 40 I=4,NDIMSI
        DEPSTH(I) = DEPS(I)
 40   CONTINUE
C
C ---   DEPSDV = DEV DEPS = DEV (B*DELTA _U - DELTA_EPS_THERMIQUE) :
C       ----------------------------------------------------------
      DO 50 I=1,NDIMSI
        DEPSDV(I)   = DEPSTH(I) - DEPSMO * KRON(I)
 50   CONTINUE
C
C --- CALCUL DE SIGMP :
C     ===============
      SIGMMO = 0.D0
C
      DO 60 I =1,3
        SIGMMO = SIGMMO + SIGM(I)
 60   CONTINUE
C
      SIGMMO = SIGMMO/3.D0
C
C ---   SIGMP = (MU/MU-)*DEV SIGMA- + (K/K-)*1/3*TR(SIG-) :
C       -------------------------------------------------
      DO 70 I=1,NDIMSI
        SIGMP(I)= DEUXMU/DEUMUM*(SIGM(I)-SIGMMO*KRON(I)) +
     &            TROISK/TROIKM*SIGMMO*KRON(I)
70    CONTINUE

C
C --- CALCUL DE SIGMMO, SIGMDV, SIGEL, SIELEQ ET SEUIL :
C     ================================================
      SIGMMO = 0.D0
C
      DO 80 I =1,3
        SIGMMO = SIGMMO + SIGMP(I)
 80   CONTINUE
C
      SIGMMO = SIGMMO/3.D0
      SIELEQ = 0.D0
C
C ---   SIGMDV = DEV ((MU/MU-)*DEV SIGMA- + (K/K-)*1/3*TR(SIG-))
C ---   SIGEL  = DEV ((MU/MU-)*DEV SIGMA- + (K/K-)*1/3*TR(SIG-))
C ---          + 2*MU*DEV DELTA_EPS - 2/3*C*ALFA-
C ---   DEV SE = SIGEL
C ---   SIELEQ = J2(DEV SE) = SE_EQ
C ---   SEUIL   = SE_EQ - RP :
C       --------------------
      DO 90 I = 1,NDIMSI
        SIGMDV(I) = SIGMP(I)- SIGMMO*KRON(I)
        SIGEDV(I) = SIGMDV(I) + DEUXMU * DEPSDV(I)
        SIGEL(I)=SIGEDV(I)
     &           -CM*ALFAM(I)/1.5D0-C2M*ALFA2M(I)/1.5D0
        SIELEQ    = SIELEQ + SIGEL(I)*SIGEL(I)
 90   CONTINUE
      SIELEQ     = SQRT(1.5D0*SIELEQ)
      SEUIL      = SIELEQ - RPM
C     INITIALISATIONS
      DP    = 0.D0              
      IF (MEMO.EQ.1) THEN       
         QP=QM                     
         DO 104 I = 1,NDIMSI      
            KSIP(I)=KSIM(I)       
 104     CONTINUE                 
         RPVP  = RPVM             
      ENDIF                     
C
C --- CALCUL DE SIGP,SIGPDV,VIP,DP,RP :
C     ===============================
      IF ( OPTION(1:9) .EQ. 'RAPH_MECA' .OR.
     &     OPTION(1:9) .EQ. 'FULL_MECA'     ) THEN
C
C       CALCUL DE DP :
C       ------------
         IF (SEUIL.LE.0.D0) THEN
            PLAST = 0.D0
            DP    = 0.D0
         ELSE
C
C ---   DETERMINATION DE L'INCREMENT DE DEFORMATION PLASTIQUE CUMULEE
C ---   QUI EST LA SOLUTION D'UNE EQUATION NON LINEAIRE EN UTILISANT
C ---   UNE METHODE DE SECANTES :
C       =======================
            CALL NMCHDP(MAT,PM,NDIMSI,SIGEDV,NBVAR,EPSPM,ALFAM,ALFA2M,
     &                  DEUXMU,CRIT,SEUIL,VISC,MEMO,DT,RPVM,QM,KSIM,
     &                  RPVP,QP,KSIP,DP,IRET,NITER)
            PLAST = UN
         ENDIF
C
         PP     = PM + DP
         IF (MEMO.EQ.0) THEN
             RP     = RINF + (R0-RINF)*EXP(-B*PP)
         ELSEIF (MEMO.EQ.1) THEN
             RP    = RPVP + R0              
         ENDIF
         CP     = CINF * (UN + (K-UN)*EXP(-W*PP))
         C2P    = C2INF * (UN + (K-UN)*EXP(-W*PP))
         GAMMAP = GAMMA0 * (AINF + (UN-AINF)*EXP(-B*PP))
         GAMM2P = GAMM20 * (AINF + (UN-AINF)*EXP(-B*PP))
         MP     = CP/(UN+GAMMAP*DP)
         M2P     = C2P/(UN+GAMM2P*DP)
         DENOMI=RP+(1.5D0*DEUXMU+MP+M2P)*DP
         IF (VISC.EQ.1) THEN
            DENOMI = DENOMI + KVI*((DP/DT)**(UN/VALDEN))
         ENDIF
C
C ---   REACTUALISATION DE SIGEL A CAUSE DE CP :
C       --------------------------------------
         DO 100 I = 1,NDIMSI
           SIGEL(I)  = SIGEDV(I)-CP*ALFAM(I)/1.5D0
     &                          -C2P*ALFA2M(I)/1.5D0
 100     CONTINUE
C
C ---   DETERMINATION DE L'INCREMENT DES DEFORMATIONS PLASTIQUES :
C       --------------------------------------------------------
         DO 120 I = 1, NDIMSI
           DEPSP(I) =  UN/DENOMI*(  1.5D0*DP*SIGEDV(I)
     &                   - MP*DP*ALFAM(I)- M2P*DP*ALFA2M(I))
           EPSPP(I)=EPSPM(I)+DEPSP(I)
  120    CONTINUE
C
         DO 110 I = 1, NDIMSI
           IF (CINF.NE.0.D0) THEN
              DALFA(I) = (DEPSP(I)-GAMMAP*DP*ALFAM(I))/(UN+GAMMAP*DP)
           ELSE
              DALFA(I)=0.D0
           ENDIF
           IF (NBVAR.EQ.2) THEN
              IF (C2INF.NE.0.D0) THEN
             DALFA2(I) = (DEPSP(I)-GAMM2P*DP*ALFA2M(I))/(UN+GAMM2P*DP)
              ELSE
                 DALFA2(I)=0.D0
              ENDIF
           ENDIF
  110    CONTINUE
C
C ---     CALCUL DES CONTRAINTES SIGP A L'INSTANT ACTUEL :
C ---     SIGEDV =   DEV ((MU/MU-)*DEV SIGMA-  + 2*MU*DEV DELTA_EPS
C ---     SIGPDV =       (MU/MU-)*DEV SIGMA-  +
C ---                    2*MU*(DEV DELTA_EPS - DELTA_EPS_PLAST)
C ---     SIGP =  SIGPDV + 1/3*TR(SIG- + 3K*DELTA_EPS) :
C         --------------------------------------------
         DO 130 I = 1,NDIMSI
            ALFA(I)   = ALFAM(I)  + DALFA(I)
            IF (NBVAR.EQ.2) THEN
               ALFA2(I)   = ALFA2M(I)  + DALFA2(I)
            ENDIF
            SIGPDV(I) = SIGEDV(I) - DEUXMU * DEPSP(I)
            SIGP(I)   = SIGPDV(I) + (SIGMMO + TROISK*DEPSMO)*KRON(I)
 130     CONTINUE
C
      ENDIF
C
C ---- CALCUL DE LA MATRICE DE COMPORTEMENT TANGENTE COHERENTE
C ---  DSIDEP(6,6) :
C      ===========
      IF ( OPTION(1:14) .EQ. 'RIGI_MECA_TANG'.OR.
     &     OPTION(1:9)  .EQ. 'FULL_MECA'         ) THEN
         CALL NMCHAT(MATEL,MAT,NBVAR,MEMO,VISC,PLAST,SIGMDV,DEPSDV,
     &               PM,DP,NDIMSI,DT,RPVP,QP,VIM,DSIDEP)
C
      ENDIF
C
C --- MISE AU FORMAT DES CONTRAINTES DE RAPPEL :
C     ========================================
      IF (OPTION(1:9).EQ.'RAPH_MECA' .OR.
     &    OPTION(1:9).EQ.'FULL_MECA')     THEN
         VIP(1)=PP
         VIP(2)=NITER
         DO 280 I = 1, 3
           VIP(I+2) = ALFA(I)
  280    CONTINUE
         DO 290 I=4,NDIMSI
            VIP(I+2) = ALFA(I)/RAC2
  290    CONTINUE
         IF (NBVAR.EQ.2) THEN
            DO 281 I = 1, 3
               VIP(I+8) = ALFA2(I)
  281       CONTINUE
            DO 291 I=4,NDIMSI
               VIP(I+8) = ALFA2(I)/RAC2
  291       CONTINUE
         ENDIF
         IF (MEMO.EQ.1) THEN
            VIP(15)=RPVP
            VIP(16)=QP
            DO 102 I=1,6
               VIP(16+I)=KSIP(I)
  102       CONTINUE
            DO 105 I=1,6
               VIP(22+I)=EPSPP(I)
  105       CONTINUE
         ENDIF
      ENDIF
      END
