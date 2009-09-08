      SUBROUTINE LC0088(FAMI,KPG,KSP,NDIM,IMATE,COMPOR,CRIT,INSTAM,
     &            INSTAP,EPSM,DEPS,SIGM,VIM,OPTION,ANGMAS,SIGP,
     &             VIP,TAMPON,TYPMOD,ICOMP,NVI,DSIDEP,CODRET)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF calculel  DATE 08/09/2009   AUTEUR SFAYOLLE S.FAYOLLE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2009  EDF R&D                  WWW.CODE-ASTER.ORG
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
C TOLE CRP_21

C VARIABLES ENTREE SORTIE NECESSAIRES A LA ROUTINE
      IMPLICIT NONE
      CHARACTER*8     TYPMOD(*)
      CHARACTER*16    OPTION
      INTEGER         NDIM, IMATE
      REAL*8          INSTAM, INSTAP, EPSM(6), DEPS(6), VIM(4)
      REAL*8          SIGP(6), VIP(4), DSIDEP(6,6)
      INTEGER         CODRET

C VARIABLES ENTREE SORTIE INUTILES POUR CETTE ROUTINE
      REAL*8          CRIT(*), ANGMAS(3)
      REAL*8          SIGM(6)
      CHARACTER*16    COMPOR(*)
      INTEGER         NVI,KSP,ICOMP,KPG
      REAL*8          TAMPON(*)
      CHARACTER*(*)   FAMI

C ----------------------------------------------------------------------
C     LOI DE COMPORTEMENT ELASTIQUE FRAGILE (SANS REGULARISATION)
C           INTEGRATION PAR LE SCHEMA IMPL-EX
C
C
C       ARGUMENTS UTILES A CETTE ROUTINE
C
C IN  NDIM    : DIMENSION DE L'ESPACE
C IN  TYPMOD  : TYPE DE MODELISATION
C IN  IMATE   : NATURE DU MATERIAU
C IN  EPSM    : DEFORMATION EN T-
C IN  DEPS    : INCREMENT DE DEFORMATION
C IN  VIM     : VARIABLES INTERNES EN T-
C IN  INSTAM  : INSTANT T-
C IN  INSTAP  : INSTANT T+DT
C IN  OPTION  : OPTION DEMANDEE
C                 RIGI_MECA_IMPLEX -> SIGP DSIDEP
C                 RAPH_MECA        -> SIGP        VIP
C                 FULL_MECA        ->  INTERDIT
C                 RIGI_MECA_ELAS   ->  INTERDIT
C OUT SIGP    : CONTRAINTE EN T+
C OUT VIP     : VARIABLES INTERNES
C                 1   -> VALEUR DE L'ENDOMMAGEMENT
C                 2   -> ELASTIQUE (0), DISSIPATIF (1) OU CASSE (2)
C                 3   -> INCREMENT DE L'ENDOMMAGEMENT
C                 4   -> INCREMENT DE L'INSTANT (T- - T-- necessaire)
C OUT DSIDEP  : MATRICE TANGENTE
C OUT CODRET  : INDICATEUR DE REDECOUPAGE DU PAS DE TEMPS
C
C
C         ARGUMENTS INUTILES A CETTE ROUTINE QUE L'ON PEUT DEMANDER
C
C       IN      KPG,KSP NUMERO DU (SOUS)POINT DE GAUSS
C               COMPOR  COMPORTEMENT DE L ELEMENT
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
C                                 (ITER_INTE_PAS == ITEDEC)
C                                 0 = PAS DE REDECOUPAGE
C                                 N = NOMBRE DE PALIERS
C               TAMPON  TABLEAUX DES ELEMENTS GEOMETRIQUES SPECIFIQUES
C                       AUX LOIS DE COMPORTEMENT (DIMENSION MAXIMALE
C                       FIXEE EN DUR)
C               SIGM    CONTRAINTE A T
C ======================================================================
      LOGICAL     CPLAN, TANG, RAPH

      INTEGER     NDIMSI, K, L, ETAT

      REAL*8      EPS(6), TREPS, COPLAN, SIGEL(6)
      REAL*8      KRON(6), VALRES(3), DDOT, DMAX, FMIN
      REAL*8      FD, D, DM, DD, ENER, DT, DDDT
      REAL*8      E, NU, LAMBDA, DEUXMU, GAMMA, SY, WY

      CHARACTER*2 IDRET(3)
      CHARACTER*8 NOMRES(3)

      PARAMETER  (DMAX = 1.D0, FMIN = 1.D-5)
      DATA  KRON/1.D0,1.D0,1.D0,0.D0,0.D0,0.D0/

C ======================================================================
C                            INITIALISATION
C ======================================================================

C -- OPTION ET MODELISATION

      RAPH   = OPTION .EQ. 'RAPH_MECA'
      TANG   = OPTION .EQ. 'RIGI_MECA_IMPLEX'
      CPLAN  = TYPMOD(1) .EQ. 'C_PLAN  '
      NDIMSI = 2 * NDIM
      CODRET = 0

C -- LECTURE DES CARACTERISTIQUES ELASTIQUES

      NOMRES(1) = 'E'
      NOMRES(2) = 'NU'
      CALL RCVALA(IMATE,' ','ELAS',0,' ',0.D0,2,
     &              NOMRES,VALRES,IDRET, 'FM')

      E     = VALRES(1)
      NU    = VALRES(2)
      LAMBDA = E * NU / (1.D0+NU) / (1.D0 - 2.D0*NU)
      DEUXMU = E/(1.D0+NU)


C -- LECTURE DES CARACTERISTIQUES D'ENDOMMAGEMENT

      NOMRES(1) = 'SY'
      NOMRES(2) = 'D_SIGM_EPSI'
      CALL RCVALA(IMATE,' ','ECRO_LINE',0,' ',0.D0,2,
     &            NOMRES,VALRES,IDRET,'FM')
      SY = VALRES(1)
      GAMMA  = - VALRES(2)/E
      WY  = SY**2 / (2*E)


C -- DEFORMATIONS

      CALL DCOPY(NDIMSI, EPSM,1, EPS,1)
      IF (RAPH) CALL DAXPY(NDIMSI, 1.D0, DEPS,1, EPS,1)

C -- EXTRACTION DES VARIABLES
      DM   = VIM(1)
      ETAT = NINT(VIM(2))
      DDDT = VIM(3)
      DT  = INSTAP-INSTAM

C ======================================================================
C     CAS RIGI_MECA_TANG : EXTRAPOLATION VARIABLES INTERNES ET MATRICE
C ======================================================================

      IF (TANG) THEN
       IF (ETAT.NE.2) THEN
C -- DISTINCTION DES CAS : SI AVANT PAS ENDO, EXTRAPOL SANS ENDO
          IF (DDDT .EQ. 0.D0) THEN
           DD = 0.D0
           D = DM + DD
           ETAT = 0.D0
          ELSE
            DD = DDDT * DT
            D  = DM + DD
            ETAT = 1
            IF (D .GT. DMAX) THEN
             D = DMAX
             ETAT = 2
            END IF
          END IF
        ELSE
C -- SI LE PTG EST CASSE, PAS D'INCREMENT
          IF (DDDT .EQ. 0.D0) THEN
            DD = 0.D0
            D = DM + DD
            ETAT = 2
          ELSE
            DD = 0.D0
            D  = DM + DD
            ETAT = 2
          END IF
        END IF

C -- CALCUL DES CONTRAINTES EXTRAPOLEES

        IF (CPLAN) THEN
          COPLAN  = - NU/(1.D0-NU)
          EPS(3)  = COPLAN * (EPS(1)+EPS(2))
        END IF

        TREPS = EPS(1)+EPS(2)+EPS(3)
        DO 60 K=1,NDIMSI
          SIGEL(K) = LAMBDA*TREPS*KRON(K) + DEUXMU*EPS(K)
 60     CONTINUE

        DO 320 K=1,NDIMSI
          SIGP(K) = (1.D0-D) * SIGEL(K)
 320     CONTINUE

C -- MATRICE TANGENTE

C -- CONTRIBUTION ELASTIQUE

        CALL R8INIR(36, 0.D0, DSIDEP, 1)
        FD = 1-D
        FD = MAX(FMIN, FD)
        DO 100 K = 1,3
          DO 110 L = 1,3
            DSIDEP(K,L) = FD*LAMBDA
 110      CONTINUE
 100    CONTINUE

        DO 120 K = 1,NDIMSI
          DSIDEP(K,K) = DSIDEP(K,K) + FD*DEUXMU
 120    CONTINUE

C -- CORRECTION CONTRAINTES PLANES

        IF (CPLAN) THEN
          DO 300 K=1,NDIMSI
            IF (K.EQ.3) GO TO 300
            DO 310 L=1,NDIMSI
              IF (L.EQ.3) GO TO 310
              DSIDEP(K,L)=DSIDEP(K,L)
     &        - 1.D0/DSIDEP(3,3)*DSIDEP(K,3)*DSIDEP(3,L)
 310        CONTINUE
 300      CONTINUE
        END IF
C ======================================================================
C     CAS RAPH_MECA : CALCUL VARIABLES INTERNES ET CONTRAINTES A T+
C ======================================================================
      ELSEIF (RAPH) THEN
C ======================================================================
C                         CONTRAINTES ELASTIQUES
C ======================================================================

C -- SI CONTRAINTES PLANES

        IF (CPLAN) THEN
          COPLAN  = - NU/(1.D0-NU)
          EPS(3)  = COPLAN * (EPS(1)+EPS(2))
        END IF

C -- CALCUL DES CONTRAINTES ELASTIQUES

        TREPS = EPS(1)+EPS(2)+EPS(3)
        DO 160 K=1,NDIMSI
          SIGEL(K) = LAMBDA*TREPS*KRON(K) + DEUXMU*EPS(K)
 160    CONTINUE
        ENER = 0.5D0 * DDOT(NDIMSI,EPS,1,SIGEL,1)

C ======================================================================
C                 INTEGRATION DE LA LOI DE COMPORTEMENT
C ======================================================================

C -- POINT DEJA SATURE

        IF (ETAT.EQ.2) THEN
          D = DM

C -- CALCUL DE L'ETAT D'ENDOMMAGEMENT
        ELSE
          IF (ENER .LE. WY*((1.D0+GAMMA)/(1.D0+GAMMA-DM))**2) THEN
            D = DM
            ETAT = 0
          ELSE
            ETAT = 1
            D = MAX(DM, (1.D0+GAMMA)*(1.D0-SQRT(WY/ENER)))
            IF (D.GT.DMAX) THEN
              D = DMAX
              ETAT = 2
            END IF
          END IF
        END IF
        DD=D-DM

C -- CALCUL DES CONTRAINTES

        DO 30 K=1,NDIMSI
          SIGP(K) = (1.D0-D) * SIGEL(K)
 30     CONTINUE

C -- STOCKAGE DES VARIABLES INTERNES

        VIP(1) = D
        VIP(2) = ETAT
        VIP(3) = (D-DM)/(INSTAP-INSTAM)
      ELSE
          CALL ASSERT(.FALSE.)
      END IF

 999  CONTINUE
      END
