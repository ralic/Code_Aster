         SUBROUTINE NMVPRK ( NDIM,  TYPMOD, IMAT,  COMP,  CRIT,
     1                       TIMED, TIMEF, TEMPD, TEMPF, TREF,
     2                       EPSDT, DEPST, SIGD,  VIND,  OPT,
     3                       SIGF,  VINF,  DSDE)
        IMPLICIT REAL*8 (A-H,O-Z)
C       ================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 27/03/2002   AUTEUR VABHHTS J.PELLET 
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
C       INTEGRATION DE LOIS DE COMPORTEMENT ELASTO-VISCOPLASTIQUE
C       PAR UNE METHODE DE RUNGE KUTTA
C               AVEC    .N VARIABLES INTERNES
C
C       INTEGRATION DES CONTRAINTES           = SIG(T+DT)
C       INTEGRATION DES VARIABLES INTERNES    = VIN(T+DT)
C
C       CETTE METHODE NE FOURNIT PAS DE MATRICE TANGENTE POUR LA
C       RESOLUTION GLOBALE.
C       ON FOURNIT DONC LA MATRICE DSDE OBTENUE EN ELASTICITE
C       ================================================================
C     ---------------------------ATTENTION----------------------------
C     IMPLANTATION D UNE LOI DE COMPORTEMENT VISCOPLASTIQUE COUPLEE
C     A DE L ENDOMMAGEMENT ISOTROPE (MODELE DE CHABOCHE) VENDOCHAB
C     ---------------------------ATTENTION----------------------------
C
C       ARGUMENTS
C
C       IN      NDIM   DIMENSION DE L ESPACE (3D=3,2D=2,1D=1)
C               TYPMOD TYPE DE MODELISATION
C               IMAT   ADRESSE DU MATERIAU CODE
C               COMP   COMPORTEMENT DE L ELEMENT
C                      COMP(1) = RELATION DE COMPORTEMENT (CHABOCHE...)
C                      COMP(2) = NB DE VARIABLES INTERNES
C                      COMP(3) = TYPE DE DEFORMATION (PETIT,JAUMANN...)
C               OPT    OPTION DE CALCUL A FAIRE
C                              'RIGI_MECA_TANG'> DSDE(T)
C               CRIT   CRITERES  LOCAUX
C                      CRIT(1) = NOMBRE D ITERATIONS MAXI A CONVERGENCE
C                                (ITER_INTE_MAXI == ITECREL)
C                      CRIT(3) = CRITERE DE PRECISION POUR L INTEGRATION
C                                PAR LA METHODE DE RUNGE KUTTA
C                      CRIT(6) = TYPE D INTEGRATION LOCALE POUR
C                                LA LOI DE COMPORTEMENT
C                                (RESO_LOCA  == INTLOC)
C                                0 = IMPLICITE
C                                1 = RUNGE_KUTTA_2
C               TIMED   INSTANT T
C               TIMEF   INSTANT T+DT
C               TEMPD   TEMPERATURE A T
C               TEMPF   TEMPERATURE A T+DT
C               TREF    TEMPERATURE DE REFERENCE
C               EPSDT   DEFORMATION TOTALE A T
C               DEPST   INCREMENT DE DEFORMATION TOTALE
C               SIGD    CONTRAINTE A T
C               VIND    VARIABLES INTERNES A T    + INDICATEUR ETAT T
C       OUT     SIGF    CONTRAINTE A T+DT
C               VINF    VARIABLES INTERNES A T+DT + INDICATEUR ETAT T+DT
C               DSDE    MATRICE DE COMPORTEMENT TANGENT ELASTIQUE
C       ----------------------------------------------------------------
C       INFO    MATERD        (*,1) = CARACTERISTIQUES ELASTIQUES A T
C                             (*,2) = CARACTERISTIQUES PLASTIQUES A T
C               MATERF        (*,1) = CARACTERISTIQUES ELASTIQUES A T+DT
C                             (*,2) = CARACTERISTIQUES PLASTIQUES A T+DT
C               MATCST        'OUI' SI MATERIAU CST ENTRE T ET T+DT
C                             'NAP' SI LE PARAMETRE K_D EST UNE NAPPE
C                             'NON' SINON
C               NDT            NB DE COMPOSANTES TOTALES DES TENSEURS
C                                      = 6  3D
C                                      = 4  AXIS  C_PLAN  D_PLAN
C               NDI            NB DE COMPOSANTES DIRECTES DES TENSEURS
C               NVI            NB DE VARIABLES INTERNES
C               NR             NB EQUATIONS SYSTEME INTEGRE A RESOUDRE
C       ----------------------------------------------------------------
C       ROUTINE LC....UTILITAIRES POUR INTEGRATION LOI DE COMPORTEMENT
C       ----------------------------------------------------------------
C       ORDRE DES TENSEURS      3D      XX YY ZZ XY XZ YZ
C                               DP      XX YY ZZ XY
C                               AX      RR ZZ TT RZ
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
        INTEGER         IMAT,   NDIM,   NDT, NDI, NR, NVI
C
        INTEGER         I
C
        INTEGER         ITMAX,  ICOMP
        INTEGER         NMAT,   IOPTIO, IDNR, NBMAT
        REAL*8          TOLER,  YMFS
        REAL*8          CRIT(*)
        REAL*8          VIND(*),     VINF(*)
        REAL*8          TIMED,       TIMEF,     TEMPD,    TEMPF  , TREF
        REAL*8          DEPS(6),     EPSF(6)
        REAL*8          EPSDT(6),    DEPST(6)
        LOGICAL         BZ
C
        REAL*8            ENDOC, MAXDOM
C
        REAL*8          SIGD(6),     SIGF(6)
C
        REAL*8          DSDE(6,6)
C
        PARAMETER     ( MAXDOM = 0.99D0 )
C
        PARAMETER       ( NMAT = 50     )
C
        REAL*8          MATERD(NMAT,2) , MATERF(NMAT,2)
C
        CHARACTER*7     ETATD  ,     ETATF
        CHARACTER*8     NBITER,      MOD,      TYPMA,  TYPMOD(*)
        CHARACTER*16    COMP(*),     OPT,      LOI
C
        CHARACTER*3     MATCST
C
        CHARACTER*11    METING
        CHARACTER*16    CHAI
C
        REAL*8 COTHE(3),DCOTHE(3)
        REAL*8 COEFF(NMAT),DCOEFF(NMAT)
        REAL*8 SIGI(6),EPSD(6),DETOT(6)
        REAL*8 NU
        REAL*8          HYDRD , HYDRF , SECHD , SECHF
C
        COMMON /TDIM/   NDT,    NDI
        COMMON /OPTI/   IOPTIO, IDNR
        COMMON /METI/   METING
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      CHARACTER*32       JEXNUM , JEXNOM , JEXR8 , JEXATR
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
C --    INITIALISATION DES PARAMETRES DE CONVERGENCE ET ITERATIONS
C
      TOLER    = CRIT(3)
      METING = 'RUNGE_KUTTA'
      LOI  = COMP(1)
      MOD  = TYPMOD(1)
      HYDRD = 0.D0
      HYDRF = 0.D0
      SECHD = 0.D0
      SECHF = 0.D0
C
C     YMFS EST UTILISE LORS DU CALCUL D ERREUR COMME MINIMUM DE
C     CHAQUE COMPOSANTE DE VINT. L IDEAL SERAIT DE RENTRER CE
C     PARAMETRE EN DONNEE POUR CHAQUE VARIABLE INTERNE
C
C --    DEBUT AIGUILLAGE SUIVANT LOI  --
      IF (LOI(1:9).EQ.'VENDOCHAB') THEN
        YMFS     = 0.0001D0
C -- AU LIEU DE 1.0D-8 DANS LA VERSION PRECEDENTE
      ELSE
        YMFS     = 0.0001D0
      ENDIF
C
C --    FIN AIGUILLAGE SUIVANT LOI  --
C
C --    RECUPERATION COEF(TEMP(T))) LOI ELASTO-PLASTIQUE A T ET/OU T+DT
C                    NB DE CMP DIRECTES/CISAILLEMENT + NB VAR. INTERNES
C
        CALL LCMATE ( LOI, MOD, IMAT, NMAT, TEMPD, TEMPF, HYDRD, HYDRF,
     1                SECHD, SECHF, TIMED, TIMEF, TYPMA, BZ, MATERD,
     2                MATERF, MATCST, NDT , NDI , NR, NVI, VIND )
C
        CALL GETVTX('NEWTON','MATRICE',1,1,1,CHAI,NCHAI)
        IF (CHAI(1:8).EQ.'TANGENTE') THEN
          CALL UTMESS('F','NMVPRK','STOP, MATRICE TANGENTE
     1                 NON ACCESSIBLE, CHOISIR MATRICE ELASTIQUE')
        END IF
        IF (OPT.EQ.'RIGI_MECA_TANG') THEN
          CALL LCINMA(0.D0,DSDE)
          CALL LCOPLI('ISOTROPE',MOD,MATERD(1,1),DSDE)
C --    DEBUT AIGUILLAGE SUIVANT LOI  --
          IF (LOI(1:9).EQ.'VENDOCHAB') THEN
          CALL UTMESS('F','NMVPRK','STOP, RIGI_MECA_TANG NON
     &                DISPONIBLE')
          ENDIF
C --    FIN   AIGUILLAGE SUIVANT LOI  --
          GOTO 9999
        END IF
C
        TPERD=TEMPD
        DTPER=TEMPF-TEMPD
        TPEREF=TREF
C
        DO 1 ICP=1,6
          DETOT(ICP)=DEPST(ICP)
          EPSD(ICP)=EPSDT(ICP)
    1   CONTINUE
        DTIME=TIMEF-TIMED
C
C --    INITIALISATION DES VARIABLES INTERNES A T
C
      DO 10 I=1,3
        COTHE(I)=MATERD(I,1)
        DCOTHE(I)=-COTHE(I)+MATERF(I,1)
   10 CONTINUE
C
      DO 11 I=1,NMAT
        COEFF(I)=MATERD(I,2)
        DCOEFF(I)=-COEFF(I)+MATERF(I,2)
   11 CONTINUE
C
C --    DEBUT AIGUILLAGE SUIVANT LOI  --
C     ROUTINE DE DECROISSANCE DES CONTRAINTES QUAND D>MAXDOM
C
      IF (LOI(1:9).EQ.'VENDOCHAB') THEN
C
        IF (VIND(9).GE.MAXDOM) THEN
C
          IF (VIND(9).EQ.1.0D0) THEN
            DO 4 ICP=1,2*NDIM
              SIGF(ICP)=SIGD(ICP)*(0.01D0)
    4     CONTINUE
          MATERD(1,1)=0.01D0*MATERD(1,1)
          CALL LCOPLI('ISOTROPE',MOD,MATERD(1,1),DSDE)
          ELSE
          DO 5 ICP=1,2*NDIM
             SIGF(ICP)=SIGD(ICP)*(0.1D0)
    5     CONTINUE
            ENDOC=(1.0D0-MAX(MAXDOM,VIND(9)))*0.1D0
            MATERD(1,1)=ENDOC*MATERD(1,1)
            CALL LCOPLI('ISOTROPE',MOD,MATERD(1,1),DSDE)
            MATERD(1,1)=MATERD(1,1)/ENDOC
          ENDIF
          DO 6 ICP=1,NVI
             VINF(ICP)=VIND(ICP)
    6     CONTINUE
          VINF(9)=1.0D0
          GOTO 9999
        ENDIF
      ENDIF
C --    FIN   AIGUILLAGE SUIVANT LOI  --
      DO 13 ICP=1,NVI
        VINF(ICP)=VIND(ICP)
   13 CONTINUE
C --    DEBUT AIGUILLAGE SUIVANT LOI  --
C       INITIALISATION PARTICULIERE SUIVANT LA LOI
C       ICI-> INITIALISATION DE VINF(8) A UNE VALEUR NON NULLE
C       POUR EVITER LES 1/0 DANS RKDVEC
      IF (LOI(1:9).EQ.'VENDOCHAB') THEN
        IF (VINF(8).LE.(1.0D-8)) THEN
          VINF(8)=1.0D-8
        ENDIF
      ENDIF
C --    FIN   AIGUILLAGE SUIVANT LOI  --
C
        CALL GERPAS(LOI,MOD,IMAT,MATCST,
     &              NVI,NMAT,VINF,DTIME,TOLER,YMFS,COTHE,
     &              COEFF,DCOTHE,DCOEFF,E,NU,ALPHA,
     &              SIGI,EPSD,DETOT,TPERD,DTPER,TPEREF,BZ,X)
C
C --    CALCUL DES CONTRAINTES
C
C --    DEBUT AIGUILLAGE SUIVANT LOI  --
C --    CALCUL DES CONTRAINTES SUIVANT QUE LE MATERIAU EST
C --    ENDOMMAGE OU PAS
      IF (LOI(1:9).EQ.'VENDOCHAB') THEN
        E=E*(1.0D0-VINF(9))
        CALL CALSIG(VINF,MOD,E,NU,ALPHA,X,DTIME,EPSD,DETOT,
     &              TPERD,DTPER,TPEREF,SIGI)
      ELSE
        CALL CALSIG(VINF,MOD,E,NU,ALPHA,X,DTIME,EPSD,DETOT,
     &              TPERD,DTPER,TPEREF,SIGI)
      ENDIF
C --    FIN   AIGUILLAGE SUIVANT LOI  --
        DO 14 ICP=1,2*NDIM
          SIGF(ICP)=SIGI(ICP)
   14   CONTINUE
C --    DEBUT AIGUILLAGE SUIVANT LOI  --
C --    CALCUL DE DSDE SUIVANT QUE LE MATERIAU EST
C --    ENDOMMAGE OU PAS
          CALL LCINMA(0.D0,DSDE)
      IF (LOI(1:9).EQ.'VENDOCHAB') THEN
        ENDOC=(1.0D0-VINF(9))
        MATERF(1,1)=MATERF(1,1)*ENDOC
        CALL LCOPLI('ISOTROPE',MOD,MATERF(1,1),DSDE)
        MATERF(1,1)=MATERF(1,1)/ENDOC
      ELSE
        CALL LCOPLI('ISOTROPE',MOD,MATERF(1,1),DSDE)
      ENDIF
C --    FIN   AIGUILLAGE SUIVANT LOI  --
C
 9999   CONTINUE
        END
