      SUBROUTINE NMCHAB (FAMI,KPG,KSP,NDIM,TYPMOD,IMATE,COMPOR,CRIT,
     &                   INSTAM,INSTAP,DEPS,SIGM,VIM,
     &                   OPTION,SIGP,VIP,DSIDEP,IRET)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 16/10/2007   AUTEUR SALMONA L.SALMONA 
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
C TOLE CRP_20
C RESPONSABLE JMBHH01 J.M.PROIX
C.======================================================================
C      IMPLICIT REAL*8 (A-H,O-Z)
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
C    EPSM(6)        IN    R       DEFORMATIONS A 'INSTANT DU CALCUL
C                                 PRECEDENT
C    DEPS(6)        IN    R       INCREMENT DE DEFORMATIONS, I.E.
C                                 IL S'AGIT DE B.DELTA_U
C    SIGM(6)        IN    R       CONTRAINTES A L'INSTANT DU CALCUL
C                                 PRECEDENT
C    VIM(2+6*NBVAR) IN    R       VARIABLES INTERNES A L'INSTANT DU
C                                 CALCUL PRECEDENT
C                                 VIM(1) = DEFORMATION PLASTIQUE
C                                          CUMULEE
C                                 VIM(2) = 1 SI LE POINT D'INTEGRATION
C                                            COURANT ETAIT 'PLASTIQUE'
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
           REAL*8             DEPS(6),DEUXMU,DILATM,DILATP
           REAL*8             SIGM(6),VIM(*),SIGP(6),VIP(*),DSIDEP(6,6)
           CHARACTER*(*)      FAMI
           CHARACTER*8        TYPMOD(*)
           CHARACTER*16       COMPOR(3),OPTION
C -----  VARIABLES LOCALES
           REAL*8      DEPSTH(6),VALRES(10),PM,C2INF,GAMM20,M2P
           REAL*8      PLAST,DEPSMO,SIGMMO,E,NU,TROISK
           REAL*8      RP,RPM,SIELEQ,SEUIL,DP,TM,TP,TREF
           REAL*8      COEF,SIGEDV(6),KRON(6),DEPSDV(6)
           REAL*8      SIGMDV(6),SIGPDV(6),EM,NUM
           REAL*8      TROIKM,DEUMUM,SIGMP(6),SIGEL(6),PP
           REAL*8      ZERO,UNDEMI,UN,DEUX,TROIS,QUATRE,SIX,RAC2,C2P
           REAL*8      R0,RINF,B,CINF,K,W,MAT(10),C2M,GAMM2P,DE2P,DB2P
           REAL*8      DGAMAP,DCP,DRP,DMP,AP,BP,EP,DENOMI,DDENOM,H2A2
           REAL*8      L1P,L2P,L3P,DAP,DBP,DEP,MP,GAMMA0,GAMMAP,L22P
           REAL*8      ISP,IAP,IA2P,IP,H1S,H2P,H1A1,H2S,H1A2,H2A1
           REAL*8      S(6),DEPSP(6),ALFAM(6),ALFA(6),DALFA(6)
           REAL*8      CM,MU,AINF,CP,HP,SEQ,DGAM2P,DC2P,DM2P,E2P,B2P
           REAL*8      PDEV(6,6),DSIDE(6,6),ALFA2M(6),ALFA2(6),DALFA2(6)
           REAL*8      DT,ETA,VALDEN,CORR,VP,DVP,UNSETA
           INTEGER     NDIMSI,I,J,L
           CHARACTER*2 BL2, FB2, CODRET(10)
           CHARACTER*8 NOMRES(10)
           LOGICAL     PLASTI
           DATA        KRON/1.D0,1.D0,1.D0,0.D0,0.D0,0.D0/
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
C --- POUR L'INSTANT ON NE TRAITE PAS LE CAS DES CONTRAINTES PLANES :
C     -------------------------------------------------------------
      IF (TYPMOD(1) .EQ. 'C_PLAN') THEN
        CALL U2MESS('F','ALGORITH6_66')
      ENDIF
C
      NBVAR=0
      IF ( COMPOR(1)(6:14) .EQ. 'CIN1_CHAB' ) THEN
         NBVAR=1
      ELSEIF ( COMPOR(1)(6:14) .EQ. 'CIN2_CHAB' ) THEN
         NBVAR=2
      ELSE
         CALL U2MESK('F','ALGORITH4_50',1,COMPOR(1))
      ENDIF
C
C --- INITIALISATIONS :
C     ===============
      ZERO   = 0.0D0
      UNDEMI = 0.5D0
      UN     = 1.0D0
      DEUX   = 2.0D0
      TROIS  = 3.0D0
      QUATRE = 4.0D0
      SIX    = 6.0D0
      RAC2   = SQRT(DEUX)
C
      NDIMSI = 2*NDIM
      DP     = ZERO
      SEUIL  = ZERO
      DT = INSTAP - INSTAM
C
      BL2    = '  '
      FB2    = 'F '
C
      PM    = VIM(1)
      PLAST = VIM(2)


      CALL VERIFT(FAMI,KPG,KSP,'T',IMATE,'ELAS',1,COEF,IRET1)
C
C --- RECUPERATION DES CARACTERISTIQUES ELASTIQUES :
C     ============================================
      NOMRES(1)='E'
      NOMRES(2)='NU'
C
C ---  CARACTERISTIQUES A L'INSTANT PRECEDENT :
C      --------------------------------------
      CALL RCVALB(FAMI,KPG,KSP,'-',IMATE,' ','ELAS',0,' ',0.D0,2,
     &              NOMRES(1),VALRES(1),CODRET(1), FB2 )
      EM     = VALRES(1)
      NUM    = VALRES(2)
      DEUMUM = EM/(UN+NUM)
      TROIKM = EM/(UN-DEUX*NUM)
C
C ---  CARACTERISTIQUES A L'INSTANT ACTUEL :
C      -----------------------------------

      CALL RCVALB(FAMI,KPG,KSP,'+',IMATE,' ','ELAS',0,' ',0.D0,2,
     &              NOMRES(1),VALRES(1),CODRET(1), FB2 )
C
       E      = VALRES(1)
       NU     = VALRES(2)
       DEUXMU = E/(UN+NU)
       MU     = DEUXMU/DEUX
       TROISK = E/(UN-DEUX*NU)
C
C --- RECUPERATION DES CARACTERISTIQUES D'ECROUISSAGE :
C     ===============================================
       NOMRES(1) = 'R_0'
       NOMRES(2) = 'R_I'
       NOMRES(3) = 'B'
       NOMRES(5) = 'K'
       NOMRES(6) = 'W'
       NOMRES(8) = 'A_I'
       IF (NBVAR.EQ.1) THEN
          NOMRES(4) = 'C_I'
          NOMRES(7) = 'G_0'
          CALL RCVALB(FAMI,KPG,KSP,'+',IMATE,' ','CIN1_CHAB',0,' ',0.D0,
     &             8,NOMRES,VALRES,CODRET,'FM')
       ELSEIF (NBVAR.EQ.2) THEN
          NOMRES(4) = 'C1_I'
          NOMRES(7) = 'G1_0'
          NOMRES(9) = 'C2_I'
          NOMRES(10) = 'G2_0'

          CALL RCVALB(FAMI,KPG,KSP,'+',IMATE,' ','CIN2_CHAB',0,' ',0.D0,
     &             10,NOMRES,VALRES,CODRET,'FM')
       ENDIF
          R0     = VALRES(1)
          RINF   = VALRES(2)
          B      = VALRES(3)
          CINF   = VALRES(4)
          K      = VALRES(5)
          W      = VALRES(6)
          GAMMA0 = VALRES(7)
          AINF   = VALRES(8)
          C2INF = 0.D0
          GAMM20 = 0.D0
       IF (NBVAR.EQ.2) THEN
          C2INF  = VALRES(9)
          GAMM20 = VALRES(10)
       ENDIF
C
       MAT(1) = R0
       MAT(2) = RINF
       MAT(3) = B
       MAT(4) = CINF
       MAT(5) = K
       MAT(6) = W
       MAT(7) = GAMMA0
       MAT(8) = AINF
      IF (NBVAR.EQ.2) THEN
       MAT(9) = C2INF
       MAT(10) = GAMM20
      ENDIF
C
      RPM    = RINF + (R0-RINF)*EXP(-B*PM)
      CM     = CINF * (UN + (K-UN)*EXP(-W*PM))
      C2M = C2INF * (UN + (K-UN)*EXP(-W*PM))
C
C --- RECUPERATION DES CARACTERISTIQUES VISQUEUSES :
C     ============================================
      NOMRES(1) = 'N'
      NOMRES(2) = 'UN_SUR_K'
      NOMRES(3) = 'UN_SUR_M'
      CALL RCVALB(FAMI,KPG,KSP,'+',IMATE,' ','LEMAITRE',0,' ',0.D0,3
     &             ,NOMRES,VALRES,CODRET,BL2)
C
      IF (CODRET(1).EQ.'OK') THEN
        VALDEN = VALRES(1)
        UNSETA = VALRES(2)
        IF (VALDEN.LE.ZERO) THEN
          CALL U2MESS('F','ALGORITH6_67')
        ENDIF
        IF (UNSETA.EQ.ZERO) THEN
          CALL U2MESS('F','ALGORITH6_68')
        ENDIF
        ETA = UN/UNSETA
        IF (VALRES(3).NE.ZERO) THEN
          CALL U2MESS('F','ALGORITH6_69')
        ENDIF
      ELSE
        VALDEN = UN
        ETA = ZERO
      ENDIF
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
      DEPSMO = ZERO
C
C ---   DEPSTH = B*DELTA _U - DELTA_EPS_THERMIQUE :
C       -----------------------------------------
      DO 30 I=1,3
        DEPSTH(I)   = DEPS(I) - COEF
        DEPSMO      = DEPSMO  + DEPSTH(I)
 30   CONTINUE
C
      DEPSMO = DEPSMO/TROIS
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
      SIGMMO = ZERO
C
      DO 60 I =1,3
        SIGMMO = SIGMMO + SIGM(I)
 60   CONTINUE
C
      SIGMMO = SIGMMO/TROIS
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
      SIGMMO = ZERO
C
      DO 80 I =1,3
        SIGMMO = SIGMMO + SIGMP(I)
 80   CONTINUE
C
      SIGMMO = SIGMMO/TROIS
C
      SIELEQ = ZERO
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
     &           -DEUX/TROIS*CM*ALFAM(I)-DEUX/TROIS*C2M*ALFA2M(I)
        SIELEQ    = SIELEQ + SIGEL(I)*SIGEL(I)
 90   CONTINUE
      SIELEQ     = SQRT(TROIS/DEUX*SIELEQ)
      SEUIL      = SIELEQ - RPM
            DP    = ZERO
C
C --- CALCUL DE SIGP,SIGPDV,VIP,DP,RP :
C     ===============================
      IF ( OPTION(1:9) .EQ. 'RAPH_MECA' .OR.
     &     OPTION(1:9) .EQ. 'FULL_MECA'     ) THEN
C
C       CALCUL DE DP :
C       ------------
         IF (SEUIL.LE.ZERO) THEN
            PLAST = ZERO
            DP    = ZERO
            RP    = RPM
         ELSE
C
C ---   DETERMINATION DE L'INCREMENT DE DEFORMATION PLASTIQUE CUMULEE
C ---   QUI EST LA SOLUTION D'UNE EQUATION NON LINEAIRE EN UTILISANT
C ---   UNE METHODE DE SECANTES :
C       =======================
            CALL NMCHDP(MAT,PM,NDIMSI,SIGEDV,NBVAR,ALFAM,ALFA2M,DEUXMU,
     &                  CRIT,SEUIL,ETA,DT,VALDEN,DP,IRET)
            PLAST = UN
         ENDIF
C
         PP     = PM + DP
         RP     = RINF + (R0-RINF)*EXP(-B*PP)
         CP     = CINF * (UN + (K-UN)*EXP(-W*PP))
         C2P    = C2INF * (UN + (K-UN)*EXP(-W*PP))
         GAMMAP = GAMMA0 * (AINF + (UN-AINF)*EXP(-B*PP))
         GAMM2P = GAMM20 * (AINF + (UN-AINF)*EXP(-B*PP))
         MP     = CP/(UN+GAMMAP*DP)
         M2P     = C2P/(UN+GAMM2P*DP)
         DENOMI=RP+(TROIS/DEUX*DEUXMU+MP+M2P)*DP
         DENOMI = DENOMI + ETA*((DP/DT)**(UN/VALDEN))
C
C ---   REACTUALISATION DE SIGEL A CAUSE DE CP :
C       --------------------------------------
         DO 100 I = 1,NDIMSI
           SIGEL(I)  = SIGEDV(I)-DEUX/TROIS*CP*ALFAM(I)
     &                          -DEUX/TROIS*C2P*ALFA2M(I)
 100     CONTINUE
C
C ---   DETERMINATION DE L'INCREMENT DE ALFA :
C       ------------------------------------
C         DO 110 I = 1, NDIMSI
C           DALFA(I) =  MP/(DENOMI*CP)*(TROIS/DEUX*DP*SIGEL(I)
C     +                             + MP*GAMMAP*DP*DP*ALFAM(I))
C     +               - MP/CP*GAMMAP*DP*ALFAM(I)
C  110    CONTINUE
C
C ---   DETERMINATION DE L'INCREMENT DES DEFORMATIONS PLASTIQUES :
C       --------------------------------------------------------
         DO 120 I = 1, NDIMSI
C           DEPSP(I) =  UN/DENOMI*(  TROIS/DEUX*DP*SIGEL(I)
C     +                            + MP*GAMMAP*DP*DP*ALFAM(I))
           DEPSP(I) =  UN/DENOMI*(  TROIS/DEUX*DP*SIGEDV(I)
     &                   - MP*DP*ALFAM(I)- M2P*DP*ALFA2M(I))
  120    CONTINUE
C
         DO 110 I = 1, NDIMSI
           IF (CINF.NE.0.D0) THEN
              DALFA(I) = (DEPSP(I)-GAMMAP*DP*ALFAM(I))/(UN+GAMMAP*DP)
           ELSE
              DALFA(I)=ZERO
           ENDIF
           IF (NBVAR.EQ.2) THEN
              IF (C2INF.NE.0.D0) THEN
             DALFA2(I) = (DEPSP(I)-GAMM2P*DP*ALFA2M(I))/(UN+GAMM2P*DP)
              ELSE
                 DALFA2(I)=ZERO
              ENDIF
           ENDIF
  110    CONTINUE
C
         PLASTI = (PLAST.GE.UNDEMI)
C
C ---     CALCUL DES CONTRAINTES SIGP A L'INSTANT ACTUEL :
C ---     SIGEDV =   DEV ((MU/MU-)*DEV SIGMA-  + 2*MU*DEV DELTA_EPS
C ---     SIGPDV =       (MU/MU-)*DEV SIGMA-  +
C ---                    2*MU*(DEV DELTA_EPS - DELTA_EPS_PLAST)
C ---     SIGP =  SIGPDV + 1/3*TR(SIG- + 3K*DELTA_EPS) :
C         --------------------------------------------
         DO 130 I = 1,NDIMSI
C            SIGEDV(I) = SIGMDV(I) + DEUXMU * DEPSDV(I)
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
C
         PLASTI=(PLAST.GE.UNDEMI)
C
         DO 140 I=1,NDIMSI
           SIGEDV(I) = SIGMDV(I) + DEUXMU * DEPSDV(I)
 140     CONTINUE
C
C ---   INITIALISATION :
C       --------------
         DO 150 I=1, NDIMSI
            DO 160 J=1, NDIMSI
               DSIDE (I,J) = ZERO
               DSIDEP(I,J) = ZERO
 160        CONTINUE
 150     CONTINUE
C
C ---   PARTIE ELASTIQUE DE LA MATRICE TANGENTE :
C       ---------------------------------------
        DO 170 I=1, NDIMSI
          DSIDEP(I,I) = DEUXMU
 170    CONTINUE
C
         DO 180 I=1, 3
            DO 190 J=1, 3
               DSIDEP(I,J) = DSIDEP(I,J) + TROISK/TROIS - DEUXMU/TROIS
 190        CONTINUE
 180     CONTINUE
C
C ---   PARTIE PLASTIQUE DE LA MATRICE TANGENTE :
C        =======================================
C ---   CALCUL DES DERIVEES PAR RAPPORT A LA DEFORMATION PLASTIQUE
C ---   CUMULEE DES CARACTERISTIQUES D'ECROUISSAGE DU MATERIAU :
C       ------------------------------------------------------
         IF (PLASTI) THEN
C
C ---      VISCOSIFICATION: SI DP=0 (RIGI_MECA_TANG), ON IMPOSE
C ---      DP = EPSILON > 0 POUR DERIVER LE TERME (DP/DT)**(1/N)
           IF (DP.EQ.ZERO) DP = CRIT(3) * CRIT(3)
C
           PP     = PM + DP
           RP     = RINF + (R0-RINF)*EXP(-B*PP)
           CP     = CINF * (UN + (K-UN)*EXP(-W*PP))
           GAMMAP = GAMMA0 * (AINF + (UN-AINF)*EXP(-B*PP))
           MP     = CP/(UN+GAMMAP*DP)
           C2P     = C2INF * (UN + (K-UN)*EXP(-W*PP))
           GAMM2P = GAMM20 * (AINF + (UN-AINF)*EXP(-B*PP))
           M2P     = C2P/(UN+GAMM2P*DP)
           VP     = ETA*((DP/DT)**(UN/VALDEN))
           CORR = DP/DT
           CORR = (CORR**(UN/VALDEN))/CORR
           DVP = ETA*CORR/(VALDEN*DT)
           DENOMI = RP + (TROIS*MU+MP+M2P)*DP + VP
           DGAMAP = -B*GAMMA0*(UN-AINF)*EXP(-B*PP)
           DCP    = -W*CINF*(K-UN)*EXP(-W*PP)
           DRP    = -B*(R0-RINF)*EXP(-B*PP)
           DMP    =  DCP/(UN+GAMMAP*DP)
     &             - CP*(DGAMAP*DP+GAMMAP)/
     &                  ((UN+GAMMAP*DP)*(UN+GAMMAP*DP))
C
           DGAM2P = -B*GAMM20*(UN-AINF)*EXP(-B*PP)
           DC2P    = -W*C2INF*(K-UN)*EXP(-W*PP)
           DM2P    =  DC2P/(UN+GAMM2P*DP)
     &             - C2P*(DGAM2P*DP+GAMM2P)/
     &                  ((UN+GAMM2P*DP)*(UN+GAMM2P*DP))
C
C           AP     =  UN - (DEUXMU+DEUX/TROIS*MP)*TROIS/DEUX*DP/DENOMI
           AP     =  (RP+VP)/DENOMI
C           EP     =  MP*GAMMAP*DP - CP
           EP     =  -MP
           E2P    = -M2P
C           BP     = -(DEUXMU + DEUX/TROIS*MP)*EP*DP/DENOMI
C     +              + DEUX/TROIS*EP
           BP     = - DEUX/TROIS*MP*(RP+VP)/DENOMI
           B2P     = - DEUX/TROIS*M2P*(RP+VP)/DENOMI

C
           DDENOM =  DRP + TROIS*MU + MP +M2P + (DMP+DM2P)*DP + DVP
           IP     =  UN/DENOMI - DDENOM*DP/(DENOMI*DENOMI)

C           DAP    = -DMP*DP/DENOMI - TROIS*(MU+MP/TROIS)*IP
           DAP    = (DRP+DVP)/DENOMI - (RP+VP)*DDENOM/DENOMI/DENOMI
C           DEP    =  DMP*GAMMAP*DP + MP*DGAMAP*DP + MP*GAMMAP -DCP
           DEP    =  -DMP
           DE2P=-DM2P

C           DBP    = -DEUX/TROIS*DMP*EP*DP/DENOMI
C     +              -(DEUXMU + DEUX/TROIS*MP) * (DEP*DP/DENOMI + EP*IP)
C     +              +DEUX/TROIS*DEP
           DBP = -DEUX/TROIS*(DMP*AP+MP*DAP)
           DB2P = -DEUX/TROIS*(DM2P*AP+M2P*DAP)
           SEQ = ZERO
           DO 200 I = 1, NDIMSI
              S(I) = AP*SIGEDV(I)+BP*ALFAM(I)+B2P*ALFA2M(I)
              SEQ  = SEQ + S(I)*S(I)
 200       CONTINUE
           SEQ = SQRT(TROIS/DEUX*SEQ)
C
           L1P = AP*AP/SEQ
           L2P = AP*BP/SEQ
           L22P = AP*B2P/SEQ
           L3P = ZERO
           DO 210 I = 1, NDIMSI
             L3P=L3P+(DAP*SIGEDV(I)+DBP*ALFAM(I)+DB2P*ALFA2M(I))*S(I)
 210       CONTINUE
           L3P =  TROIS/DEUX/SEQ*L3P
           L3P =  L3P - DRP - DVP
           HP  =  DEP*DP/DENOMI + EP*IP
           H2P  =  DE2P*DP/DENOMI + E2P*IP
           ISP = -TROIS/DEUX*IP*L1P/L3P
           IAP = -TROIS/DEUX*IP*L2P/L3P
           IA2P = -TROIS/DEUX*IP*L22P/L3P
           H1S = -TROIS/DEUX*HP*L1P/L3P
           H1A1 = -TROIS/DEUX*HP*L2P/L3P
           H2S = -TROIS/DEUX*H2P*L1P/L3P
           H1A2 = -TROIS/DEUX*HP*L22P/L3P
           H2A1 = -TROIS/DEUX*H2P*L2P/L3P
           H2A2 = -TROIS/DEUX*H2P*L22P/L3P
C
           DO 220 I=1, NDIMSI
             DSIDE(I,I) = -SIX*MU*MU*DP/DENOMI
             DO 230 J=1, NDIMSI
               DSIDE(I,J) = DSIDE(I,J)
     &                        - SIX*MU*MU*ISP    * SIGEDV(I)*SIGEDV(J)
     &                        - QUATRE*MU*MU*H1A1* ALFAM(I)*ALFAM(J)
     &                        - QUATRE*MU*MU*H1A2* ALFA2M(I)*ALFAM(J)
     &                        - QUATRE*MU*MU*H2A1* ALFAM(I)*ALFA2M(J)
     &                        - QUATRE*MU*MU*H2A2* ALFA2M(I)*ALFA2M(J)
     &                        - SIX*MU*MU*IAP    * ALFAM(I)*SIGEDV(J)
     &                        - SIX*MU*MU*IA2P    * ALFA2M(I)*SIGEDV(J)
     &                        - QUATRE*MU*MU*H1S * SIGEDV(I)*ALFAM(J)
     &                        - QUATRE*MU*MU*H2S * SIGEDV(I)*ALFA2M(J)
 230         CONTINUE
 220       CONTINUE
C
C ---    MATRICE DE PROJECTION DEVIATORIQUE :
C        ----------------------------------
           DO 240 I = 1, 6
           DO 240 J = 1, 6
             PDEV(I,J) = ZERO
 240       CONTINUE
C
           PDEV(1,1) =  DEUX/TROIS
           PDEV(2,2) =  DEUX/TROIS
           PDEV(3,3) =  DEUX/TROIS
           PDEV(4,4) =  UN
           PDEV(5,5) =  UN
           PDEV(6,6) =  UN
           PDEV(1,2) = -UN/TROIS
           PDEV(1,3) = -UN/TROIS
           PDEV(2,3) = -UN/TROIS
           PDEV(2,1) = -UN/TROIS
           PDEV(3,1) = -UN/TROIS
           PDEV(3,2) = -UN/TROIS
C
           DO 250 I=1, NDIMSI
             DO 260 J=1, NDIMSI
               DO 270 L=1, NDIMSI
                 DSIDEP(I,J) = DSIDEP(I,J) + DSIDE(I,L)*PDEV(L,J)
 270           CONTINUE
 260         CONTINUE
 250       CONTINUE
        ENDIF
      ENDIF
C
C --- MISE AU FORMAT DES CONTRAINTES DE RAPPEL :
C     ========================================
      IF (OPTION(1:9).EQ.'RAPH_MECA' .OR.
     &    OPTION(1:9).EQ.'FULL_MECA')     THEN
         VIP(1)=PP
         VIP(2)=PLAST
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

      END IF
C


      END
