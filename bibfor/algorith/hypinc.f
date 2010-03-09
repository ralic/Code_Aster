      SUBROUTINE HYPINC(FAMI  ,KPG   ,KSP   ,POUM  ,NDIM  ,
     &                  TYPMOD,IMATE ,COMPOR,CRIT  ,OPTION,
     &                  EPSM  ,DEPS  ,SIGM  ,SIGP  ,DSIDEP,
     &                  CODRET)
C     
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 09/03/2010   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 2005 UCBL LYON1 - T. BARANGER     WWW.CODE-ASTER.ORG
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT NONE
      INTEGER       KPG,KSP,NDIM
      INTEGER       IMATE
      CHARACTER*16  COMPOR(*)
      CHARACTER*16  OPTION
      CHARACTER*(*) FAMI,POUM
      REAL*8        CRIT(*)
      REAL*8        EPSM(6),DEPS(6)
      REAL*8        SIGM(6),SIGP(6)
      REAL*8        DSIDEP(6,6)
      CHARACTER*8   TYPMOD(*)
      INTEGER       CODRET
C
C ----------------------------------------------------------------------
C
C     LOI DE COMPORTEMENT HYPERELASTIQUE DE SIGNORINI
C
C     C10 (I1-3) + C01 (I2-3)+ C20 (I1-3)^2 + K/2(J-1)²
C
C     POUR LES ELEMENTS ISOPARAMETRIQUES 3D, CP, et DP
C
C     CONTRAINTES ET MATRICE TANGENTE EN COMP_INCR
C
C ----------------------------------------------------------------------
C
C
C IN  NDIM   : DIMENSION DE L'ESPACE
C IN  TYPMOD : TYPE DE MODELISATION
C IN  CRIT   : CRITERES DE CONVERGENCE LOCAUX
C                             (1) = NB ITERATIONS MAXI A CONVERGENCE
C                                   (ITER_INTE_MAXI == ITECREL)
C                             (2) = TYPE DE JACOBIEN A T+DT
C                                   (TYPE_MATR_COMP == MACOMP)
C                                   0 = EN VITESSE     >SYMETRIQUE
C                                   1 = EN INCREMENTAL >NON-SYMETRIQUE
C                             (3) = VALEUR TOLERANCE DE CONVERGENCE
C                                    (RESI_INTE_RELA == RESCREL)
C IN  OPTION : OPTION DEMANDEE : RIGI_MECA_TANG -> SIG    DSIDEP
C                                FULL_MECA      -> SIG VI DSIDEP
C                                RAPH_MECA      -> SIG VI
C                                RUPTURE        -> SIG VI ENERGI
C IN  IMATE  : ADRESSE DU MATERIAU CODE
C IN  COMPOR  : COMPORTEMENT  (1) = TYPE DE RELATION COMPORTEMENT
C                             (2) = NB VARIABLES INTERNES / PG
C                             (3) = HYPOTHESE SUR LES DEFORMATIONS
C                             (4) = COMP_ELAS (OU COMP_INCR)
C IN  FAMI   : FAMILLE DE POINTS DE GAUSS
C IN  KPG    : NUMERO DU POINT DE GAUSS
C IN  KSP    : NUMERO DU SOUS-POINT DE GAUSS
C IN  EPSM   : DEFORMATION A L'INSTANT MOINS
C               (SI C_PLAN EPS(3) EST EN FAIT CALCULE)
C IN  POUM   : '-' POUR VARIABLES DE COMMANDE
C IN  DEPS   : INCREMENT DE DEFORMATION
C IN  SIGM   : CONTRAINTES A L'INSTANT MOINS
C OUT SIGP   : CONTRAINTES LAGRANGIENNES CALCULEES
C OUT DSIDEP : MATRICE DE RIGIDITE TANGENTE
C OUT CODRET : CODE RETOUR ERREUR INTEGRATION (1 SI PROBLEME, 0 SINON)
C
C ----------------------------------------------------------------------
C
      INTEGER     I,J,L,M
      REAL*8      C11,C22,C12,C33,C13,C23
      REAL*8      EPS(6)
      REAL*8      CVOL(6,6),CISO(6,6)
      REAL*8      SVOL(6),SISO(6)
      REAL*8      C10,C01,C20,K
      INTEGER     NITMAX
      REAL*8      EPSI
C
C ----------------------------------------------------------------------
C
C --- INITIALISATIONS
C
      CALL MATINI(6,6,0.D0,DSIDEP)
      NITMAX = INT(CRIT(1))
      EPSI   = CRIT(3)
      CALL ASSERT(COMPOR(4).EQ.'COMP_INCR')
C
C --- LECTURE DES CARACTERISTIQUES MECANIQUES
C
      IF ((COMPOR(1)(1:10).EQ. 'ELAS_HYPER')) THEN
        CALL HYPMAT(FAMI  ,KPG   ,KSP   ,POUM  ,IMATE ,
     &              C10   ,C01   ,C20   ,K     )
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF
C
C --- A PRIORI ON A CONVERGE
C
      CODRET = 0
C
C --- CALCUL TENSEUR DEFORMATION CAUCHY-GREEN DROIT
C
      EPS(1)=DEPS(1)+EPSM(1)
      EPS(2)=DEPS(2)+EPSM(2)
      EPS(3)=DEPS(3)+EPSM(3)
      EPS(4)=DEPS(4)+EPSM(4)
      EPS(5)=DEPS(5)+EPSM(5)
      EPS(6)=DEPS(6)+EPSM(6)
C
C --- CALCUL CONTRAINTES ET MATRICE TANGENTE
C
      IF (TYPMOD(1) .EQ. '3D'.OR.TYPMOD(1) .EQ. '3D_SI') THEN
C --- CALCUL DES ELONGATIONS
        C11 = 2.D0*EPS(1)+1.D0
        C12 = 2.D0*EPS(4)
        C22 = 2.D0*EPS(2)+1.D0
        C33 = 2.D0*EPS(3)+1.D0
        C13 = 2.D0*EPS(5)
        C23 = 2.D0*EPS(6)
C --- CALCUL DES CONTRAINTES ISOTROPIQUES
        CALL HYP3CI(C11   ,C22   ,C33   ,C12   ,C13   ,
     &              C23   ,C10   ,C01   ,C20   ,SISO  ,
     &              CODRET)
        IF (CODRET.EQ.1) THEN
          GOTO 99
        END IF
C --- CALCUL DES CONTRAINTES VOLUMIQUES   
        CALL HYP3CV(C11   ,C22   ,C33   ,C12   ,C13   ,
     &              C23   ,K     ,SVOL  ,CODRET)
        IF (CODRET.EQ.1) THEN
          GOTO 99
        END IF
C --- CALCUL DE LA MATRICE TANGENTE (PARTIE ISOTROPIQUE)
        CALL HYP3DI(C11   ,C22   ,C33   ,C12   ,C13   ,
     &              C23   ,C10   ,C01   ,C20   ,CISO  ,
     &              CODRET)
        IF (CODRET.EQ.1) THEN
          GOTO 99
        END IF
C --- CALCUL DE LA MATRICE TANGENTE (PARTIE VOLUMIQUE)        
        CALL HYP3DV(C11   ,C22   ,C33   ,C12   ,C13   ,
     &              C23   ,K     ,CVOL  ,CODRET)
        IF (CODRET.EQ.1) THEN
          GOTO 99
        END IF
C --- ASSEMBLAGE VOLUMIQUE/ISOTROPIQUE
        DO 40 I=1,6
          SIGP(I) = SISO(I)+SVOL(I)
          DO 30 J=1,6
            DSIDEP(I,J) = CISO(I,J)+CVOL(I,J)
 30       CONTINUE
 40     CONTINUE
      ELSE IF (TYPMOD(1)(1:6) .EQ. 'C_PLAN') THEN
C --- CALCUL DES ELONGATIONS
        C11 = 2.D0*EPS(1)+1.D0
        C12 = 2.D0*EPS(4)
        C22 = 2*EPS(2)+1.D0
        C33 = 1.D0
C --- CALCUL DES CONTRAINTES
        CALL HYPCPC(C11   ,C22   ,C33   ,C12   ,K     ,
     &              C10   ,C01   ,C20   ,NITMAX,EPSI  ,
     &              SIGP  ,CODRET)
        IF (CODRET.EQ.1) THEN
          GOTO 99
        END IF
C --- CALCUL DE LA MATRICE TANGENTE
        CALL HYPCPD(C11   ,C22   ,C33   ,C12   ,K     ,
     &              C10   ,C01   ,C20   ,DSIDEP,CODRET)
        IF (CODRET.EQ.1) THEN
          GOTO 99
        END IF        
        DO 130 M=1,2*NDIM
          IF (M.EQ.3) GOTO 130
          DO 140 L=1,2*NDIM
            IF (L.EQ.3) GO TO 140
              DSIDEP(M,L )= DSIDEP(M,L) -
     &                    1.D0/DSIDEP(3,3)*DSIDEP(M,3)*DSIDEP(3,L)
 140      CONTINUE
 130    CONTINUE
      ELSE IF (TYPMOD(1)(1:6).EQ.'D_PLAN') THEN
C --- CALCUL DES ELONGATIONS
        C11 = 2.D0*EPS(1)+1.D0
        C12 = 2.D0*EPS(4)
        C22 = 2.D0*EPS(2)+1.D0
C --- CALCUL DES CONTRAINTES
        CALL HYPDPC(C11   ,C22   ,C12   ,K     ,C10   ,
     &              C01   ,C20   ,SIGP  ,CODRET) 
        IF (CODRET.EQ.1) THEN
          GOTO 99
        END IF
C --- CALCUL DE LA MATRICE TANGENTE
        CALL HYPDPD(C11   ,C22   ,C12   ,K     ,C10   ,
     &              C01   ,C20   ,DSIDEP,CODRET)
        IF (CODRET.EQ.1) THEN
          GOTO 99
        END IF
      ELSE
        CALL U2MESK('F','ELASHYPER_97',1,TYPMOD(1))
      ENDIF
 99   CONTINUE
      END
