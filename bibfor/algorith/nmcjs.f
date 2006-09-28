        SUBROUTINE NMCJS(  TYPMOD,  IMAT, COMP, CRIT,
     &                      INSTAM, INSTAP, TEMPM, TEMPF, TREF, EPSD,
     &                      DEPS, SIGD, VIND, OPT, SIGF, VINF, DSDE,
     &                      IRET)
        IMPLICIT NONE
C       ================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
C       ================================================================
C       INTEGRATION DE LA LOI DE COMPORTEMENT ELASTO PLASTIQUE CJS
C               AVEC    . N VARIABLES INTERNES
C                       . 2 FONCTIONS SEUIL ELASTIQUE
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
        INTEGER         IMAT  , NDT   , NDI  , NVI, IRET
C
        REAL*8          CRIT(*)
        REAL*8          VIND(*),    VINF(*)
        REAL*8          INSTAM,     INSTAP,   TEMPM,  TEMPF  , TREF
        REAL*8          EPSD(6),    DEPS(6),  EPSF(6)
        REAL*8          SIGD(6),    SIGF(6)
C
        REAL*8          SEUILI,     SEUILD,Q0,RINIT,PA,QINIT
C
        REAL*8          DSDE(6,6)
C
        REAL*8          MATERF(14,2)
C
C
        CHARACTER*4     NIVCJS
        CHARACTER*6     MECAND, MECANF
        CHARACTER*7     ETATD, ETATF
        CHARACTER*8     MOD,   TYPMOD(*)
        CHARACTER*16    COMP(*), OPT
        INTEGER         NITER,I,NDEC
        REAL*8          EPSCON
        REAL*8          DEPSTH(6),EPSDTH(6),ALPHAF,ALPHAM
C
        REAL*8 I1D
C
        INTEGER   UMESS,IUNIFI
C       ----------------------------------------------------------------
        COMMON /TDIM/   NDT  , NDI
C       ----------------------------------------------------------------
C



        UMESS = IUNIFI('MESSAGE')
        MOD = TYPMOD(1)
C
C --    RECUPERATION COEF DE LA LOI CJS (INDEPENDANTS DE LA TEMPERATURE)
C                    NB DE CMP DIRECTES/CISAILLEMENT
C                    NB VARIABLES INTERNES
C                    NIVEAU DE LA LOI CJS: CJS1, CJS2 OU CJS3
        CALL CJSMAT ( MOD, IMAT, TEMPF, MATERF, NDT, NDI, NVI, NIVCJS )
        PA = MATERF(12,2)
        QINIT = MATERF(13,2)
C
C  COEF DE DILATATION LE MEME A TPLUS ET TMOINS
C
        ALPHAF = MATERF(3,1)
        ALPHAM = MATERF(3,1)
C
        NITER = 0
        NDEC = 0
        EPSCON = 0
C
        I1D = 0.D0
        DO 10 I = 1 , NDI
         I1D = I1D + SIGD(I)
   10   CONTINUE
C
C     --  CALCUL DE DEPSTH ET EPSDTH
C     --------------------------------
        DO 20 I=1,NDI
          DEPSTH(I)   = DEPS(I) -
     &           (ALPHAF*(TEMPF-TREF) - ALPHAM*(TEMPM-TREF))
          EPSDTH(I)   = EPSD(I) -ALPHAM*(TEMPM-TREF)
  20    CONTINUE
        DO 21 I=NDI+1,NDT
          DEPSTH(I) = DEPS(I)
          EPSDTH(I) = EPSD(I)
  21    CONTINUE
        IF (NDT.LT.6) THEN
         DO 22 I=NDT+1,6
          DEPSTH(I) = 0.D0
          EPSDTH(I) = 0.D0
  22     CONTINUE
        ENDIF
C
C  TESTER QUE VIND DE NVI EST 1 2 OU 3
C
        IF ( (VIND(NVI).NE.0.D0).AND.
     &       (VIND(NVI).NE.1.D0).AND.
     &       (VIND(NVI).NE.2.D0).AND.
     &       (VIND(NVI).NE.3.D0)) THEN
          WRITE(UMESS,*) ' INDICATEUR DE PLASTICITE ERRONE : ',VIND(NVI)
          CALL U2MESS('F','ALGORITH6_80')
         ENDIF


C --    BLOCAGE DES VARIABLES INTERNES EN FONCTION DU NIVEAU DE LA LOI
C       CJS CHOISI, ET ON PREND DES VALEURS INITIALES DE Q ET R PETITES
C       MAIS NON NULLES

        IF(NIVCJS .EQ. 'CJS1' ) THEN
           VIND(1) = 1.D25 * MATERF(12,2)
           VIND(2) = MATERF(2,2)
        ENDIF

        IF(NIVCJS .EQ. 'CJS3' ) THEN
           VIND(2) = MATERF(2,2)
        ENDIF
C
C  SI SEUIL ISOTROPE = 0 SEUIL ISOTROPE = PRESSION HYDRO
C
C

        IF( VIND(1) .EQ. 0.D0 ) THEN
         IF( I1D .LT. 0.D0 ) THEN
           Q0= (1.D-3*PA+I1D+QINIT)/3.D0
         ELSE
           Q0= (1.D-3*PA+QINIT)/3.D0
         ENDIF

         VIND(1)= Q0
         IF(OPT(1:14).NE.'RIGI_MECA_TANG') THEN
          VINF(1) = Q0
         ENDIF
        ENDIF
C
C INITIALISATION SEUIL DEVIATOIRE SI NUL
C
        IF( VIND(2) .EQ. 0.D0 ) THEN
         IF(MATERF(14,2).EQ.0.D0) THEN
          RINIT=1.D-3
         ELSE
          RINIT = MATERF(14,2)
         ENDIF
         VIND(2)= RINIT
         IF(OPT(1:14).NE.'RIGI_MECA_TANG') THEN
          VINF(2) = RINIT
         ENDIF
        ENDIF
C

C --    ETAT ELASTIC OU PLASTIC A T

        IF( VIND(NVI) .EQ. 0.D0 ) THEN
         ETATD = 'ELASTIC'
        ENDIF

        IF( VIND(NVI) .EQ. 1.D0 ) THEN
           ETATD = 'PLASTIC'
           MECAND = 'ISOTRO'
        ENDIF

        IF( VIND(NVI) .EQ. 2.D0 ) THEN
           ETATD = 'PLASTIC'
           MECAND = 'DEVIAT'
        ENDIF

        IF( VIND(NVI) .EQ. 3.D0 ) THEN
           ETATD = 'PLASTIC'
           MECAND = 'ISODEV'
        ENDIF


C       ----------------------------------------------------------------
C       OPTIONS 'FULL_MECA' ET 'RAPH_MECA' = CALCUL DE SIG(T+DT)
C       ----------------------------------------------------------------
C
        IF ( OPT .EQ. 'RAPH_MECA' .OR. OPT .EQ. 'FULL_MECA' ) THEN



C
C --    INTEGRATION ELASTIQUE SUR DT
C


        CALL CJSELA ( MOD, CRIT, MATERF, DEPSTH, SIGD, SIGF,
     &                NVI, VIND, VINF, IRET)
        IF (IRET.EQ.1) GOTO 9999

C
C --    PREDICTION ETAT ELASTIQUE A T+DT :
C       FI(SIG(T+DT),VIN(T)) = 0 ?     SEUIL MECANISME ISOTROPE
C       FD(SIG(T+DT),VIN(T)) = 0 ?     SEUIL MECANISME DEVIATOIRE
C

        CALL CJSSMI ( MATERF, SIGF, VIND, SEUILI )
        CALL CJSSMD ( MATERF, SIGF, VIND, SEUILD )


C
          IF ( (SEUILI .GT. 0.D0) .OR. (SEUILD .GT. 0.D0) ) THEN
C
C --      PREDICTION INCORRECTE > INTEGRATION ELASTO-PLASTIQUE SUR DT
C
          ETATF = 'PLASTIC'

          CALL  CJSPLA ( MOD, CRIT, MATERF, SEUILI, SEUILD, NVI,
     &                   EPSDTH, DEPSTH, SIGD, VIND, SIGF, VINF, MECANF,
     &                   NIVCJS ,NITER,
     &                   NDEC,EPSCON,IRET)
          IF (IRET.EQ.1) GOTO 9999
          ELSE
C
C --      PREDICTION CORRECTE > INTEGRATION ELASTIQUE FAITE
C
          ETATF = 'ELASTIC'
          ENDIF

        ENDIF


C       ----------------------------------------------------------------
C       OPTIONS 'FULL_MECA' ET 'RIGI_MECA_TANG' = CALCUL DE DSDE
C       ----------------------------------------------------------------
C       CALCUL ELASTIQUE ET EVALUATION DE DSDE A (T)
C       POUR 'RIGI_MECA_TANG' ET POUR 'FULL_MECA'
C       ----------------------------------------------------------------

        IF ( OPT .EQ. 'RIGI_MECA_TANG' ) THEN

          CALL LCINMA ( 0.D0, DSDE )



C REMARQUE: CALCUL DE DSDE A T AVEC MATERF CAR PARAMETRES CJS
C           INDEPENDANTS DE LA TEMPERATURE



C --      CALCUL MATRICE DE RIGIDITE ELASTIQUE
          IF(ETATD .EQ. 'ELASTIC') THEN
           CALL CJSTEL(MOD,MATERF,SIGD,DSDE)
          ENDIF

C --      CALCUL MATRICE TANGENTE DU PROBLEME CONTINU
          IF(ETATD .EQ. 'PLASTIC') THEN

C          MECANISME ISOTROPE SEUL
           IF(MECAND .EQ. 'ISOTRO') THEN
             CALL CJSTIS( MOD, MATERF, SIGD, VIND, DSDE )
           ENDIF

C          MECANISME DEVIATOIRE SEUL
           IF(MECAND .EQ. 'DEVIAT') THEN
             CALL CJSTDE( MOD, MATERF, NVI, EPSDTH, SIGD, VIND, DSDE )
           ENDIF

C          MECANISMES ISOTROPE ET DEVIATOIRE
           IF(MECAND .EQ. 'ISODEV') THEN
             CALL CJSTID( MOD, MATERF, NVI, EPSDTH, SIGD, VIND, DSDE )
           ENDIF


          ENDIF

        ENDIF


        IF ( OPT .EQ. 'FULL_MECA' ) THEN

          CALL LCINMA ( 0.D0, DSDE )



C --      CALCUL MATRICE DE RIGIDITE ELASTIQUE
          IF(ETATF .EQ. 'ELASTIC')  THEN
              CALL CJSTEL(MOD,MATERF,SIGF,DSDE)
          ENDIF


C --      CALCUL MATRICE TANGENTE DU PROBLEME CONTINU
          IF(ETATF .EQ. 'PLASTIC') THEN

C          MECANISME ISOTROPE SEUL
           IF(MECANF .EQ. 'ISOTRO') THEN
             CALL CJSTIS( MOD, MATERF, SIGF, VINF, DSDE )
           ENDIF

C          MECANISME DEVIATOIRE SEUL
           IF(MECANF .EQ. 'DEVIAT') THEN
             CALL LCSOVE(EPSDTH,DEPSTH,EPSF)
             CALL CJSTDE( MOD, MATERF, NVI, EPSF, SIGF, VINF, DSDE )
           ENDIF

C          MECANISMES ISOTROPE ET DEVIATOIRE
           IF(MECANF .EQ. 'ISODEV') THEN
             CALL LCSOVE(EPSDTH,DEPSTH,EPSF)
             CALL CJSTID( MOD, MATERF, NVI, EPSF, SIGF, VINF, DSDE )
           ENDIF


          ENDIF

        ENDIF

C  VARIABLES INTERNES POUR SORTIES

        IF ( (OPT .EQ. 'FULL_MECA').OR.
     &       (OPT .EQ. 'RAPH_MECA') ) THEN
           CALL CJSINP(  MATERF, EPSDTH,DEPSTH,SIGF,
     &                      VINF,NITER,NVI,NIVCJS,
     &                       NDEC,EPSCON)
        ENDIF

9999    CONTINUE
        END
