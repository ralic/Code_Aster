        SUBROUTINE NMHUJ (TYPMOD,  IMAT, COMP, CRIT,
     &   INSTAM, INSTAP, TEMPM, TEMPF, TREF, EPSD,
     &   DEPS, SIGD, VIND, OPT, SIGF, VINF, DSDE, IRET)
        IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 06/11/2007   AUTEUR KHAM M.KHAM 
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
C       AVEC    . 32 VARIABLES INTERNES
C               . 4 FONCTIONS SEUIL ELASTIQUE DEDOUBLEES AVEC CYCLIQUE
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
        INTEGER       NITER, I, J, K, INC
        REAL*8        CRIT(*), VIND(*), VINF(*)
        REAL*8        INSTAM, INSTAP, TEMPM, TEMPF, TREF
        REAL*8        EPSD(6), DEPS(6), EPSF(6), DEPS0(6)
        REAL*8        SIGD(6), SIGF(6), DSDE(6,6)
        REAL*8        SEUILI, SEUILD(3)
        REAL*8        D, PISO, DEPSR(6), DEPSQ(6)
        REAL*8        PC0, RATIO(6), SIGD0(6), HILL, DSIG(6) 
        CHARACTER*6   MECAND, MECANF
        CHARACTER*7   ETATD, ETATF
        CHARACTER*8   MOD, TYPMOD(*)
        CHARACTER*16  COMP(*), OPT
        REAL*8        EPSCON
        REAL*8        DEPSTH(6), EPSDTH(6), ALPHAF, ALPHAM
        REAL*8        EPSTHE,EPSTHM
        REAL*8        MATERF(22,2), I1D, D13, ZERO, UN
        INTEGER       UMESS, IUNIFI, IISNAN
        LOGICAL       DEBUG, CONV
C       ----------------------------------------------------------------
        COMMON /TDIM/   NDT, NDI
        COMMON /MESHUJ/ DEBUG
C       ----------------------------------------------------------------
        DATA       D13  / 0.33333333334D0 /
        DATA       ZERO / 0.0D0 /
        DATA       UN   / 1.0D0 /


C --- DEBUG = .TRUE. : MODE AFFICHAGE ENRICHI
C        DEBUG = .FALSE.


C        UMESS = IUNIFI('MESSAGE')
        MOD   = TYPMOD(1)

C        WRITE(6,'(A)')'***********************************'
C        WRITE(6,'(A,8(1X,E16.9))')'VIND =',(VIND(I),I=1,8)
C        WRITE(6,'(A,2(1X,E16.9))')'VIND =',(VIND(I),I=21,22)

C ---> RECUPERATION COEF DE LA LOI HUJEUX
C      (INDEPENDANTS DE LA TEMPERATURE)
C      NB DE CMP DIRECTES/CISAILLEMENT
C      NB VARIABLES INTERNES
        CALL HUJMAT (MOD, IMAT, TEMPF, MATERF, NDT, NDI, NVI)

C ---> COEF DE DILATATION LE MEME A TPLUS ET TMOINS
        ALPHAF = MATERF(3,1)
        ALPHAM = MATERF(3,1)

        NITER  = 0
        EPSCON = 0

        I1D = ZERO
        DO 10 I = 1, NDI
          I1D = I1D + D13*SIGD(I)
   10     CONTINUE


C       CALCUL DE DEPSTH ET EPSDTH
C       --------------------------
        IF (((IISNAN(TEMPM).GT.0).OR.(IISNAN(TREF).GT.0)).
     &     AND.(MATERF(3,1).NE.0.D0)) THEN
          CALL U2MESS('F','CALCULEL_15')     
        ELSEIF (MATERF(3,1).EQ.0.D0) THEN
          EPSTHE = 0.D0
          EPSTHM = 0.D0
        ELSE
          EPSTHE = ALPHAF*(TEMPF-TREF) - ALPHAM*(TEMPM-TREF)
          EPSTHM = ALPHAM*(TEMPM-TREF)
        ENDIF
        DO 20 I = 1, NDI
          DEPSTH(I) = DEPS(I) - EPSTHE
          EPSDTH(I) = EPSD(I) - EPSTHM
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
          IF (MATERF(13, 2) .EQ. ZERO) THEN
            VIND(1) = 1.D-3
          ELSE
            VIND(1) = MATERF(13,2)
          ENDIF
       ENDIF
       IF (VIND(2) .EQ. ZERO) THEN
          IF (MATERF(13, 2) .EQ. ZERO) THEN
            VIND(2) = 1.D-3
          ELSE
            VIND(2) = MATERF(13,2)
          ENDIF
       ENDIF
       IF (VIND(3) .EQ. ZERO) THEN
          IF (MATERF(13, 2) .EQ. ZERO) THEN
            VIND(3) = 1.D-3
          ELSE
            VIND(3) = MATERF(13,2)
          ENDIF
       ENDIF
       IF (VIND(4) .EQ. ZERO) THEN
          IF (MATERF(14, 2) .EQ. ZERO) THEN
            VIND(4) = 1.D-3
          ELSE
            VIND(4) = MATERF(14,2)
          ENDIF
          CALL HUJCRI(MATERF, SIGD, VIND, SEUILI)   
          IF (SEUILI .GT. ZERO) THEN
            PISO    = (SIGD(1)+SIGD(2)+SIGD(3))/3
            D        = MATERF(3,2)
            PC0      = MATERF(7,2)
            VIND(4)  = PISO/(D*PC0)      
            VIND(27) = UN  
          ENDIF     
       ENDIF
Caf 14/05/07 Debut
C ---> INITIALISATION SEUIL CYCLIQUE SI NUL
       IF (VIND(5) .EQ. ZERO) THEN
          IF (MATERF(18, 2) .EQ. ZERO) THEN
            VIND(5) = 1.D-3
          ELSE
            VIND(5) = MATERF(18,2)
          ENDIF
       ENDIF
       IF (VIND(6) .EQ. ZERO) THEN
          IF (MATERF(18, 2) .EQ. ZERO) THEN
            VIND(6) = 1.D-3
          ELSE
            VIND(6) = MATERF(18,2)
          ENDIF
       ENDIF
       IF (VIND(7) .EQ. ZERO) THEN
          IF (MATERF(18, 2) .EQ. ZERO) THEN
            VIND(7) = 1.D-3
          ELSE
            VIND(7) = MATERF(18,2)
          ENDIF
       ENDIF
       IF (VIND(8) .EQ. ZERO) THEN
          IF (MATERF(19, 2) .EQ. ZERO) THEN
            VIND(8) = 1.D-3
          ELSE
            VIND(8) = MATERF(19,2)
          ENDIF
       ENDIF
Caf 14/05/07 Fin
         
       IF (OPT(1:14) .NE. 'RIGI_MECA_TANG') THEN
          CALL LCEQVN (32, VIND, VINF)
       ENDIF

Caf 30/04/07 debut
C ---> ETAT ELASTIQUE OU PLASTIQUE A T
C        IF (VIND(6) .EQ. ZERO .OR.
C     &      VIND(7) .EQ. ZERO .OR.
C     &      VIND(8) .EQ. ZERO .OR.
C     &      VIND(9) .EQ. ZERO) THEN
        IF (((VIND(24) .EQ. ZERO) .OR.
     &     (VIND(24) .EQ. -UN .AND. VIND(28) .EQ. ZERO)) .AND.  
     &     ((VIND(25) .EQ. ZERO) .OR.
     &     (VIND(25) .EQ. -UN .AND. VIND(29) .EQ. ZERO)) .AND. 
     &     ((VIND(26) .EQ. ZERO) .OR.
     &     (VIND(26) .EQ. -UN .AND. VIND(30) .EQ. ZERO)) .AND.
     &     ((VIND(27) .EQ. ZERO).OR.
     &     (VIND(27) .EQ. -UN .AND. VIND(31) .EQ. ZERO))) THEN
          ETATD = 'ELASTIC'
        ELSE
          ETATD = 'PLASTIC'
        ENDIF
        
Caf 30/04/07 fin

C       -------------------------------------------------------------
C       OPTIONS 'FULL_MECA' ET 'RAPH_MECA' = CALCUL DE SIG(T+DT)
C       -------------------------------------------------------------
        IF (OPT .EQ. 'RAPH_MECA' .OR. OPT .EQ. 'FULL_MECA') THEN

C ---> INTEGRATION ELASTIQUE SUR DT
          DO 45 I = 1, NDT
            DEPSQ(I) = ZERO 
  45      CONTINUE
C -----------------------------------------------
C ---> INCREMENT TOTAL DE DEFORMATION A APPLIQUER
C -----------------------------------------------
C - ENREGISTREMENT DE L'ETAT DE CONTRAINTES A T
          CALL LCEQVE(SIGD,SIGD0)
C - ENREGISTREMENT DE L'INCREMENT TOTAL DEPS0     
          CALL LCEQVE(DEPSTH,DEPS0)
C - INITIALISATION DES DEFORMATIONS RESTANTES
          CALL LCEQVE(DEPSTH,DEPSR)
C -----------------------------------------------------
C ---> PREDICTION VIA TENSEUR ELASTIQUE DES CONTRAINTES
C -----------------------------------------------------
C          INC = 0        
 100      CONTINUE
C        WRITE(6,*)'#####################################'
C         INC = INC + 1
C         IF(INC.GT.100)THEN
C           IRET = 1
C           GOTO 9999
C         ENDIF  
          CALL HUJELA (MOD, CRIT, MATERF, DEPSR, SIGD, SIGF, 
     &               EPSDTH, IRET)
          IF (IRET .EQ. 1) GOTO 9999
                
C ----------------------------------------------------
C ---> CONTROLE DE L EVOLUTION DE LA PRESSION ISOTROPE
C ----------------------------------------------------     
          CALL HUJDP(MOD, DEPSR, SIGD, SIGF, MATERF, VIND)
          IF(IRET.EQ.1) GOTO 9999       
C ----------------------------------------
C ---> DEFORMATION CUMULEE DEPUIS L'ENTREE
C ----------------------------------------
          DO 50 I = 1, NDT
            DEPSQ(I) = DEPSQ(I) + DEPSR(I)
 50       CONTINUE
C ---------------------------------------------
C CALCUL DE L'ETAT DE CONTRAINTES CORRESPONDANT
C ---------------------------------------------
C          WRITE(6,'(A,10(1X,E16.9))')'VIND0 =',(VIND(I),I=1,31)
C          WRITE(6,'(A,6(1X,E16.9))')'SIGD =',(SIGD(I),I=1,6)
C         WRITE(6,'(A,6(1X,E16.9))')'DEPS =',(DEPSR(I),I=1,6)
          CALL HUJRES(MOD, CRIT, MATERF, NVI, EPSDTH, DEPSR,
     &               SIGD, VIND, SIGF, VINF, NITER, 
     &               EPSCON, IRET, ETATF)
C         WRITE(6,'(A,6(1X,E16.9))')'SIGF =',(SIGF(I),I=1,6)
C         WRITE(6,'(A,10(1X,E16.9))')'VINF =',(VINF(I),I=1,31)
C         WRITE(6,*)'R4 =',VINF(4)
          IF (IRET.EQ.1) GOTO 9999
C -------------------------
C ---> DEFORMATION RESTANTE
C -------------------------
          DO 55 I = 1, NDT
            DEPSR(I) = DEPS0(I) - DEPSQ(I)
 55       CONTINUE        
C -------------------------------------------
C - CONTROLE DES DEFORMATIONS DEJA APPLIQUEES
C -------------------------------------------   
          CONV = .TRUE.
          DO 56 I = 1, NDT
C           RATIO(I) = ABS(DEPSR(I)/DEPS0(I))
            IF (DEPSR(I).NE.ZERO) CONV = .FALSE.            
 56       CONTINUE
          IF(.NOT.CONV)THEN
            CALL LCEQVE(SIGF,SIGD)
            CALL LCEQVN(NVI,VINF,VIND)
            GOTO 100
          ENDIF
C --- CALCUL DU CRITERE DE HILL: DSIG*DEPS       
          HILL = ZERO
          DO 57 I = 1, NDT
            DSIG(I) = SIGF(I) - SIGD0(I)
            HILL    = HILL + DSIG(I)*DEPS0(I)
 57       CONTINUE 
          VINF(32) = HILL
        ENDIF
        
Caf 07/05/07 FIN

C       ----------------------------------------------------------------
C       OPTIONS 'FULL_MECA' ET 'RIGI_MECA_TANG' = CALCUL DE DSDE
C       ----------------------------------------------------------------
C       CALCUL ELASTIQUE ET EVALUATION DE DSDE A (T)
C       POUR 'RIGI_MECA_TANG' ET POUR 'FULL_MECA'
C       ----------------------------------------------------------------
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
           CALL HUJTID (MOD, MATERF, SIGD, VIND, DSDE, DEPSTH, IRET)
           IF(IRET.EQ.1)GOTO 9999
          ENDIF

        ELSEIF (OPT .EQ. 'FULL_MECA') THEN

          CALL LCINMA (ZERO, DSDE)


C ---> CALCUL MATRICE DE RIGIDITE ELASTIQUE
          IF (ETATF .EQ. 'ELASTIC')  THEN
           CALL HUJTEL (MOD, MATERF, SIGF, DSDE)
          ENDIF


C ---> CALCUL MATRICE TANGENTE DU PROBLEME CONTINU
          IF (ETATF .EQ. 'PLASTIC') THEN
C         WRITE(6,'(A,6(1X,E12.5))')'SIGF =',(SIGF(I),I=1,6)
C         WRITE(6,'(A,32(1X,E12.5))')'VINF - TID =',(VINF(I),I=1,32)
           CALL HUJTID (MOD, MATERF, SIGF, VINF, DSDE, DEPSTH, IRET)
           IF(IRET.EQ.1)GOTO 9999
          ENDIF

        ENDIF


C ---- VARIABLES INTERNES POUR SORTIES
C         IF ( (OPT .EQ. 'FULL_MECA') .OR.
C      &       (OPT .EQ. 'RAPH_MECA') ) THEN
C            CALL CJSINP(  MATERF, EPSDTH, DEPSTH, SIGF,
C      &                           VINF, NITER, NVI, 
C      &                           NDEC, EPSCON)
C         ENDIF
        
9999    CONTINUE
C        IF(IRET .EQ. 1)WRITE(6,'(A,I3,A)')'**** IRET =',IRET,' ****'   
C        WRITE(6,'(A,8(1X,E16.9))')'VINF =',(VINF(I),I=1,8)
C        WRITE(6,'(A,2(1X,E16.9))')'VINF =',(VINF(I),I=21,22)
        END
