        SUBROUTINE NMHUJ (TYPMOD,  IMAT, COMP, CRIT,
     &   INSTAM, INSTAP, TEMPM, TEMPF, TREF, EPSD,
     &   DEPS, SIGD, VIND, OPT, SIGF, VINF, DSDE, IRET)
        IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 22/05/2007   AUTEUR KHAM M.KHAM 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
C       ================================================================
C       INTEGRATION DE LA LOI DE COMPORTEMENT ELASTO PLASTIQUE DE HUJEUX
C       AVEC    . 9 VARIABLES INTERNES
C               . 4 FONCTIONS SEUIL ELASTIQUE
C
C       INTEGRATION DES CONTRAINTES           = SIG(T+DT)
C       INTEGRATION DES VARIABLES INTERNES    = VIN(T+DT)
C       ET CALCUL DU JACOBIEN ASSOCIE         = DS/DE(T+DT) OU DS/DE(T)
C       ================================================================
C       IN      TYPMOD  TYPE DE MODELISATION
C               IMAT    ADRESSE DU MATERIAU CODE
C               COMP    COMPORTEMENT DE L ELEMENT
C                       COMP(1) = RELATION DE COMPORTEMENT (CHABOCHE...)
C                       COMP(2) = NB DE VARIABLES INTERNES
C                       COMP(3) = TYPE DE DEFORMATION (PETIT,JAUMANN...)
C               CRIT    CRITERES  LOCAUX
C                       CRIT(1) = NOMBRE D ITERATIONS MAXI A CONVERGENCE
C                                 (ITER_INTE_MAXI == ITECREL)
C                       CRIT(2) = TYPE DE JACOBIEN A T+DT
C                                 (TYPE_MATR_COMP == MACOMP)
C                                 0 = EN VITESSE     > SYMETRIQUE
C                                 1 = EN INCREMENTAL > NON-SYMETRIQUE
C                       CRIT(3) = VALEUR DE LA TOLERANCE DE CONVERGENCE
C                                 (RESI_INTE_RELA == RESCREL)
C                       CRIT(5) = NOMBRE D'INCREMENTS POUR LE
C                                 REDECOUPAGE LOCAL DU PAS DE TEMPS
C                                 (RESI_INTE_PAS == ITEDEC )
C                                 0 = PAS DE REDECOUPAGE
C                                 N = NOMBRE DE PALIERS
C               INSTAM  INSTANT T
C               INSTAP  INSTANT T+DT
C               TEMPM   TEMPERATURE A T
C               TEMPF   TEMPERATURE A T+DT
C               TREF    TEMPERATURE DE REFERENCE
C               EPSD    DEFORMATION TOTALE A T
C               DEPS    INCREMENT DE DEFORMATION TOTALE
C               SIGD    CONTRAINTE A T
C               VIND    VARIABLES INTERNES A T    + INDICATEUR ETAT T
C               OPT     OPTION DE CALCUL A FAIRE
C                               'RIGI_MECA_TANG'> DSDE(T)
C                               'FULL_MECA'     > DSDE(T+DT) , SIG(T+DT)
C                               'RAPH_MECA'     > SIG(T+DT)
C       OUT     SIGF    CONTRAINTE A T+DT
C               VINF    VARIABLES INTERNES A T+DT + INDICATEUR ETAT T+DT
C               DSDE    MATRICE DE COMPORTEMENT TANGENT A T+DT OU T
C               IRET    CODE RETOUR DE  L'INTEGRATION DE LA LOI CJS
C                              IRET=0 => PAS DE PROBLEME
C                              IRET=1 => ECHEC 
C       ----------------------------------------------------------------
C       INFO    MATERD        (*,1) = CARACTERISTIQUES ELASTIQUES A T
C                             (*,2) = CARACTERISTIQUES PLASTIQUES A T
C               MATERF        (*,1) = CARACTERISTIQUES ELASTIQUES A T+DT
C                             (*,2) = CARACTERISTIQUES PLASTIQUES A T+DT
C               NDT             NB DE COMPOSANTE TOTALES DES TENSEURS
C                                       = 6  3D
C                                       = 4  AXIS  C_PLAN  D_PLAN
C                                       = 1  1D
C               NDI             NB DE COMPOSANTE DIRECTES DES TENSEURS
C               NVI             NB DE VARIABLES INTERNES
C       ----------------------------------------------------------------
C       ROUTINE LC....UTILITAIRES POUR INTEGRATION LOI DE COMPORTEMENT
C       ----------------------------------------------------------------
C       ORDRE DES TENSEURS      3D      XX YY ZZ XY XZ YZ
C                               DP      XX YY ZZ XY
C                               AX      RR ZZ TT RZ
C                               1D      XX YY ZZ
C       ----------------------------------------------------------------
C       ATTENTION
C       SI OPT = 'RIGI_MECA_TANG' NE PAS TOUCHER AUX VARIABLES SIGF,VINF
C       QUI N ONT PAS DE PLACE MEMOIRE ALLOUEE
C
C       SIG EPS DEPS  ONT DEJA LEURS COMPOSANTES DE CISAILLEMENT
C       MULTIPLIES PAR RACINE DE 2 > PRISE EN COMPTE DES DOUBLES
C       PRODUITS TENSORIELS ET CONSERVATION DE LA SYMETRIE
C
C       ----------------------------------------------------------------
        INTEGER       IMAT, NDT, NDI, NVI, IRET
        INTEGER       NITER, I, NDEC, J, K
        REAL*8        CRIT(*), TOLE, VIND(*), VINF(*)
        REAL*8        INSTAM, INSTAP, TEMPM, TEMPF, TREF
        REAL*8        EPSD(6), DEPS(6), EPSF(6)
        REAL*8        SIGD(6), SIGF(6), DSDE(6,6)
        REAL*8        SEUILI, SEUILD(3), PREF
        CHARACTER*6   MECAND, MECANF
        CHARACTER*7   ETATD, ETATF
        CHARACTER*8   MOD, TYPMOD(*)
        CHARACTER*16  COMP(*), OPT
        REAL*8        EPSCON
        REAL*8        DEPSTH(6), EPSDTH(6), ALPHAF, ALPHAM
        REAL*8        MATERF(20,2), I1D, D13, DSQR, ZERO
        INTEGER       UMESS, IUNIFI
        LOGICAL       DEBUG
C       ----------------------------------------------------------------
        COMMON /TDIM/   NDT, NDI
        COMMON /MESHUJ/ DEBUG
C       ----------------------------------------------------------------

        PARAMETER (TOLE = 1.D-6)
        DATA       D13  / 0.33333333334D0 /
        DATA       DSQR / 1.41421356237D0 /
        DATA       ZERO / 0.0D0 /


C --- DEBUG = .TRUE. : MODE AFFICHAGE ENRICHI
        DEBUG = .FALSE.
C        DEBUG = .TRUE.


C        UMESS = IUNIFI('MESSAGE')
        MOD   = TYPMOD(1)


C ---> RECUPERATION COEF DE LA LOI HUJEUX
C      (INDEPENDANTS DE LA TEMPERATURE)
C      NB DE CMP DIRECTES/CISAILLEMENT
C      NB VARIABLES INTERNES
        CALL HUJMAT (MOD, IMAT, TEMPF, MATERF, NDT, NDI, NVI)
        PREF = MATERF(8,2)


C ---> COEF DE DILATATION LE MEME A TPLUS ET TMOINS
        ALPHAF = MATERF(3,1)
        ALPHAM = MATERF(3,1)

        NITER  = 0
        NDEC   = 0
        EPSCON = 0

        DO 5 I = NDI+1, NDT
          SIGD(I) = SIGD(I) /DSQR
   5      CONTINUE

        I1D = ZERO
        DO 10 I = 1, NDI
          I1D = I1D + D13*SIGD(I)
   10     CONTINUE


C       CALCUL DE DEPSTH ET EPSDTH
C       --------------------------
        DO 20 I = 1, NDI
          DEPSTH(I) = DEPS(I) -
     &      (ALPHAF*(TEMPF-TREF) - ALPHAM*(TEMPM-TREF))
          EPSDTH(I) = EPSD(I) - ALPHAM*(TEMPM-TREF)
  20      CONTINUE
  
        DO 21 I = NDI+1, NDT
          DEPSTH(I) = DEPS(I)
          EPSDTH(I) = EPSD(I)
  21      CONTINUE
  
        IF (NDT .LT. 6) THEN
          DO 22 I = NDT+1, 6
            DEPSTH(I) = ZERO
            EPSDTH(I) = ZERO
  22        CONTINUE
        ENDIF


C ---> INITIALISATION SEUIL DEVIATOIRE SI NUL
       IF (VIND(1) .EQ. ZERO) THEN
          IF (MATERF(16, 2) .EQ. ZERO) THEN
            VIND(1) = 1.D-3
          ELSE
            VIND(1) = MATERF(13,2)
          ENDIF
       ENDIF
       IF (VIND(2) .EQ. ZERO) THEN
          IF (MATERF(16, 2) .EQ. ZERO) THEN
            VIND(2) = 1.D-3
          ELSE
            VIND(2) = MATERF(13,2)
          ENDIF
       ENDIF
       IF (VIND(3) .EQ. ZERO) THEN
          IF (MATERF(16, 2) .EQ. ZERO) THEN
            VIND(3) = 1.D-3
          ELSE
            VIND(3) = MATERF(13,2)
          ENDIF
       ENDIF
       IF (VIND(4) .EQ. ZERO) THEN
          IF (MATERF(17, 2) .EQ. ZERO) THEN
            VIND(4) = 1.D-3
          ELSE
            VIND(4) = MATERF(14,2)
          ENDIF
       ENDIF
         
       IF (OPT(1:14) .NE. 'RIGI_MECA_TANG') THEN
          CALL LCEQVN (9, VIND, VINF)
       ENDIF


C ---> ETAT ELASTIQUE OU PLASTIQUE A T
        IF (VIND(6) .EQ. ZERO .OR.
     &      VIND(7) .EQ. ZERO .OR.
     &      VIND(8) .EQ. ZERO .OR.
     &      VIND(9) .EQ. ZERO) THEN
          ETATD = 'ELASTIC'
        ELSE
          ETATD = 'PLASTIC'
        ENDIF


C       -------------------------------------------------------------
C       OPTIONS 'FULL_MECA' ET 'RAPH_MECA' = CALCUL DE SIG(T+DT)
C       -------------------------------------------------------------
        IF (OPT .EQ. 'RAPH_MECA' .OR. OPT .EQ. 'FULL_MECA') THEN
        

C ---> INTEGRATION ELASTIQUE SUR DT
        CALL HUJELA (MOD, CRIT, MATERF, DEPSTH, SIGD, SIGF, IRET)
        IF (IRET .EQ. 1) GOTO 9999


C ---> PREDICTION ETAT ELASTIQUE A T+DT :
C       FI(SIG(T+DT),VIN(T)) = 0 ?     SEUIL MECANISME ISOTROPE
C       FD(SIG(T+DT),VIN(T)) = 0 ?     SEUIL MECANISME DEVIATOIRE
        CALL HUJCRI (MATERF, SIGF, VIND, SEUILI)

        DO 25 K = 1, 3
          CALL HUJCRD (K, MATERF, SIGF, VIND, SEUILD(K))
 25       CONTINUE

        IF ((-SEUILI/PREF) .GT. TOLE .OR.
     &      SEUILD(1) .GT. TOLE    .OR.
     &      SEUILD(2) .GT. TOLE    .OR. 
     &      SEUILD(3) .GT. TOLE)   THEN


C ---> PREDICTION INCORRECTE ==> INTEGRATION ELASTO-PLASTIQUE SUR DT
             ETATF = 'PLASTIC'
             CALL  HUJPLA (MOD, CRIT, MATERF, SEUILI, SEUILD, NVI,
     &             EPSDTH, DEPSTH, SIGD, VIND, SIGF, VINF, MECANF,
     &             NITER, NDEC, EPSCON, IRET)

             IF (IRET .EQ. 1) GOTO 9999

        ELSE


C ---> PREDICTION CORRECTE > INTEGRATION ELASTIQUE FAITE
          ETATF = 'ELASTIC'          
        ENDIF

        ENDIF


C   --------------------------------------------------------
C   OPTIONS 'FULL_MECA' ET 'RIGI_MECA_TANG' = CALCUL DE DSDE
C   --------------------------------------------------------
C   CALCUL ELASTIQUE ET EVALUATION DE DSDE A (T)
C   POUR 'RIGI_MECA_TANG' ET POUR 'FULL_MECA'
C   --------------------------------------------------------
        IF (OPT .EQ. 'RIGI_MECA_TANG') THEN

          CALL LCINMA (ZERO, DSDE)

C REMARQUE: CALCUL DE DSDE A T AVEC MATERF CAR PARAMETRES HUJEUX
C --------  INDEPENDANTS DE LA TEMPERATURE


C ---> CALCUL MATRICE DE RIGIDITE ELASTIQUE
          IF (ETATD .EQ. 'ELASTIC') THEN
           CALL HUJTEL (MOD, MATERF, SIGD, DSDE)
          ENDIF


C ---> CALCUL MATRICE TANGENTE DU PROBLEME CONTINU
          IF (ETATD .EQ. 'PLASTIC') THEN
           CALL HUJTID (MOD, MATERF, SIGD, VIND, DSDE)
          ENDIF

        ELSEIF (OPT .EQ. 'FULL_MECA') THEN

          CALL LCINMA (ZERO, DSDE)


C ---> CALCUL MATRICE DE RIGIDITE ELASTIQUE
          IF (ETATF .EQ. 'ELASTIC')  THEN
           CALL HUJTEL (MOD, MATERF, SIGF, DSDE)
          ENDIF


C ---> CALCUL MATRICE TANGENTE DU PROBLEME CONTINU
          IF (ETATF .EQ. 'PLASTIC') THEN
           CALL HUJTID (MOD, MATERF, SIGF, VINF, DSDE)
          ENDIF

        ENDIF


C ---- VARIABLES INTERNES POUR SORTIES
C         IF ( (OPT .EQ. 'FULL_MECA') .OR.
C      &       (OPT .EQ. 'RAPH_MECA') ) THEN
C            CALL CJSINP(  MATERF, EPSDTH, DEPSTH, SIGF,
C      &                           VINF, NITER, NVI, 
C      &                           NDEC, EPSCON)
C         ENDIF
        
        DO 50 I = NDI+1, NDT
          SIGF(I) = DSQR*SIGF(I)
 50       CONTINUE

9999    CONTINUE

        END
