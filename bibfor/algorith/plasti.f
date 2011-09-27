         SUBROUTINE PLASTI ( FAMI,KPG,KSP,TYPMOD,IMAT,COMP,CRIT,TIMED,
     &                       TIMEF, TEMPD, TEMPF, TREF,EPSDT,DEPST,
     &                       SIGD,VIND,OPT,ANGMAS,SIGF,VINF,DSDE,ICOMP,
     &                       NVI,TAMPON,IRTETI)
        IMPLICIT NONE
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/09/2011   AUTEUR PROIX J-M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE PROIX J.M.PROIX
C TOLE CRP_21
C ======================================================================
C       INTEGRATION DE LOIS DE COMPORTEMENT ELASTO PLASTIQUE ET VISCO
C       PLASTIQUE PAR UNE MATHODE DE NEWTON (DISCRETISATION IMPLICITE)
C
C       INTEGRATION DES CONTRAINTES           = SIG(T+DT)
C       INTEGRATION DES VARIABLES INTERNES    = VIN(T+DT)
C       ET CALCUL DU JACOBIEN ASSOCIE         = DS/DE(T+DT) OU DS/DE(T)
C
C       EN CAS DE NON-CONVERGENCE LOCALE ON EFFECTUE UN REDECOUPAGE DU
C       PAS DE TEMPS, SOIT LOCAL AU POINT DE GAUSS, SOIT GLOBAL
C       ================================================================
C       ARGUMENTS
C
C       IN      FAMI    FAMILLE DE POINT DE GAUSS (RIGI,MASS,...)
C               KPG,KSP NUMERO DU (SOUS)POINT DE GAUSS
C               TYPMOD  TYPE DE MODELISATION
C               IMAT    ADRESSE DU MATERIAU CODE
C               COMP    COMPORTEMENT DE L ELEMENT
C                       COMP(1) = RELATION DE COMPORTEMENT (ROUSSELIER.)
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
C               TIMED   INSTANT T
C               TIMEF   INSTANT T+DT
C               TEMPD   TEMPERATURE A T
C               TEMPF   TEMPERATURE A T+DT
C               TREF    TEMPERATURE DE REFERENCE
C ATTENTION LES PARAMETRES DE TEMPERATURES NE SONT PAS PRIS EN COMPTE EN
C MECANIQUE PURE (ON LES RECUPERE VIA LES VARIABLES DE COMMANDES). CES
C ARGUMENTS NE SONT UTILISE QU EN THM
C               EPSDT   DEFORMATION TOTALE A T
C               DEPST   INCREMENT DE DEFORMATION TOTALE
C               SIGD    CONTRAINTE A T
C               VIND    VARIABLES INTERNES A T    + INDICATEUR ETAT T
C               OPT     OPTION DE CALCUL A FAIRE
C                               'RIGI_MECA_TANG'> DSDE(T)
C                               'FULL_MECA'     > DSDE(T+DT) , SIG(T+DT)
C                               'RAPH_MECA'     > SIG(T+DT)
C               ANGMAS  : LES TROIS ANGLES DU MOT_CLEF MASSIF
C                         (AFFE_CARA_ELEM),
C               + UN REEL QUI VAUT 0 SI NAUTIQUIES OU 2 SI EULER
C               + LES 3 ANGLES D'EULER
C       OUT     SIGF    CONTRAINTE A T+DT
C               VINF    VARIABLES INTERNES A T+DT + INDICATEUR ETAT T+DT
C               DSDE    MATRICE DE COMPORTEMENT TANGENT A T+DT OU T
C               ICOMP   COMPTEUR POUR LE REDECOUPAGE DU PAS DE TEMPS
C               NVI             NB DE VARIABLES INTERNES
C               IRTETI = 1 CONTROLE DU REDECOUPAGE DU PAS DE TEMPS
C       ================================================================
C       ROUTINES D INTEGRATION IMPLICITE :
C
C       PLASTI  ALGORITHME D INTEGRATION ELASTO PLASTIQUE
C       LCMATE  RECUPERATION DES DONNEES MATERIAU CONSTANTES SUR DT
C               + DIMENSION DES TENSEURS / VARIABLES INTERNES
C       LCDEDI  DEFORMATION THERMIQUE
C       LCDEHY  DEFORMATION HYDRIQUE
C       LCELAS  INTEGRATION ELASTIQUE
C       LCCNVX  SEUIL ELASTIQUE
C       LCPLAS  INTEGRATION ELASTO_PLASTIQUE PAR NEWTON CONTENANT :
C            LCPLNL  INTEGRATION EP = SYSTEME IMPLICITE NON LINEAIRE
C            LCINIT  SOLUTION ESSAI INITIALE POUR NEWTON
C            LCRESI  RESIDU R(Y) AVEC Y= (DSIG DVIN DEPS3)
C            LCJACB  MATRICE JACOBIENNE DU SYSTEME NON LINEAIRE
C            LCCONV  CONVERGENCE DE LA SOLUTION DU SNL
C       LCJPLA  TERMES MATRICE DE COMPORTEMENT TANGENT EL_PLAS VITESSE
C       LCJPLC  TERMES MATRICE DE COMPORTEMENT TANGENT EL_PLAS COHERENT
C       LCJELA  TERMES DE LA MATRICE DE COMPORTEMENT TANGENT ELASTIQUE
C
C       ----------------------------------------------------------------
C       POINTS D ENTREES           ROUTINES  A ECRIRE POUR INTEGRATION
C       A MODIFIER
C       ----------------------------------------------------------------
C                                  VISCOCHAB XXXXXX
C
C       (LCMATE)                   CVMMAT    XXXMAT
C       (LCCNVX)                   CVMCVX    XXXCVX
C       (LCPLAS) (LCPLNL) (LCINIT) CVMINI    XXXINI (LCINVN par defaut)
C                         (LCRESI) CVMRES    XXXRES
C                         (LCJACB) CVMJAC    XXXJAC
C                         (LCCONV) CVMCVG    XXXCVG (LCCTRL par defaut)
C       (LCJELA)                   LCOPLI    XXXJEL (LCOPLI par defaut)
C       (LCJPLA)                   CVMJPL    XXXJPL
C       (LCJPLC)                   -         XXXJPC
C
C       ----------------------------------------------------------------
C       ROUTINE LC....UTILITAIRES POUR INTEGRATION LOI DE COMPORTEMENT
C       ----------------------------------------------------------------
C       ROUTINES UTILITAIRES DE CALCUL MATRICIEL(6,6) - VECTORIEL (6)
C       COMMON /TDIM/ NDT,NDI    A INCLURE OBLIGATOIREMENT

C       LCPRTE  PRODUIT TENSORIEL DE VECTEURS
C       LCPRSC  PRODUIT SCALAIRE  DE VECTEURS
C       LCPRSM  PRODUIT SCALAIRE * MATRICE
C       LCPTMV  PRODUIT MATRICE TRANSPOSEE * VECTEUR
C       LCPRMV  PRODUIT MATRICE  * VECTEUR
C       LCPRMM  PRODUIT MATRICE  * MATRICE
C       LCDIMA  DIFFERENCE DE MATRICES
C       LCSOMA  SOMME DE MATRICES
C       LCTRMA  TRANSPOSEE DE MATRICE
C       LCEQMA  EGALITE DE MATRICES
C       LCINMA  INITIALISATION DE MATRICE A UNE VALEUR
C       LCPRSV  PRODUIT SCALAIRE * VECTEUR
C       LCSOVE  SOMME DE VECTEUR
C       LCNRVE  NORME DE VECTEUR
C       LCDIVE  DIFFERENCE DE VECTEURS
C       LCINVE  INITIALISATION DE VECTEUR
C       LCEQVE  EGALITE DE VECTEURS
C
C       ----------------------------------------------------------------
C       ROUTINES UTILITAIRES DE CALCUL MATRICIEL(N,N) - VECTORIEL (N)
C
C       LCPRSN  PRODUIT SCALAIRE DE VECTEURS
C       LCNRVN  NORME DE VECTEUR
C       LCEQVN  EGALITE DE VECTEURS
C       LCPSVN  PRODUIT SCALAIRE * VECTEUR
C       LCSOVN  SOMME DE VECTEURS
C       LCINVN  INITIALISATION DE VECTEUR
C       LCDIVN  DIFFERENCE DE VECTEURS
C       LCICMA  INCLUSION DUNE PORTION DE MATRICE DANS UNE AUTRE MATRICE
C
C       ----------------------------------------------------------------
C       ROUTINES UTILITAIRES D INTEGRATION D UN MODELE DE COMPORTEMENT
C
C       LCDEVI  PARTIE DEVIATORIQUE D UN TENSEUR
C       LCHYDR  PARTIE SPHERIQUE    D UN TENSEUR
C       LCIV2S  SECOND INVARIANT DU TENSEUR CONTRAINTE
C       LCIV2E  SECOND INVARIANT DU TENSEUR DEFORMATION
C       LCNRTS  'NORME' DU TENSEUR CONTRAINTE
C       LCNRTE  'NORME' DU TENSEUR DEFORMATION
C       LCOPLI  OPERATEUR ELASTIQUE LINEAIRE
C       LCELIN  INTEGRATION  ELASTIQUE LINEAIRE ISOTROPE
C       LCVERR  CALCUL DU VECTEUR D'ERREUR RELATIVE, ABSOLU, NORMEE...
C
C       ----------------------------------------------------------------
C       INFO    MATERD        (*,1) = CARACTERISTIQUES ELASTIQUES A T
C                             (*,2) = CARACTERISTIQUES PLASTIQUES A T
C               MATERF        (*,1) = CARACTERISTIQUES ELASTIQUES A T+DT
C                             (*,2) = CARACTERISTIQUES PLASTIQUES A T+DT
C               MATCST          'OUI' SI MATERIAU CST ENTRE T ET T+DT
C                               'NON' SINON
C               NDT             NB DE COMPOSANTE TOTALES DES TENSEURS
C                                       = 6  3D
C                                       = 4  AXIS  C_PLAN  D_PLAN
C                                       = 1  1D
C               NDI             NB DE COMPOSANTE DIRECTES DES TENSEURS
C               NR              NB EQUATION SYSTEME INTEGRE A RESOUDRE
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
        INTEGER         IMAT , NDT   , NDI   , NR  , NVI
        INTEGER         ITMAX, ICOMP , KPG, KSP
        INTEGER         NMAT, CODRET
        INTEGER         IRTET, IRTETI, K, L, IRET
        REAL*8          TOLER
        REAL*8          EPSI
        CHARACTER*(*)   FAMI
C
        PARAMETER       ( EPSI = 1.D-15 )
        PARAMETER       ( NMAT = 90     )
C
        REAL*8          CRIT(*)
        REAL*8          VIND(*),     VINF(*)
        REAL*8          TIMED,       TIMEF,     TEMPD,    TEMPF  , TREF
        REAL*8          EPSD(9),     DEPS(9)
        REAL*8          EPSDT(9),    DEPST(9)
        REAL*8          SIGD(6),     SIGF(6)
C
        REAL*8          SEUIL, THETA, DT, DEVG(6), DEVGII
C
        REAL*8          VP(3),VECP(3,3),TAMPON(*)
C
        REAL*8          DSDE(6,*),  PGL(3,3), ANGMAS(3)
C
        REAL*8          MATERD(NMAT,2) , MATERF(NMAT,2)
        CHARACTER*7     ETATD  ,     ETATF
        CHARACTER*8     MOD    ,     TYPMA,   TYPMOD(*)
        CHARACTER*16    COMP(*),     OPT,        LOI
        CHARACTER*3     MATCST

C     POUR LCMATE (MONOCRISTAL) DIMENSIONS MAX
C        NSG=NOMBRE DE SYSTEMES DE GLISSEMENT MAXIMUM
C        NFS=NOMBRE DE FAMILLES DE SYSTEMES DE GLISSEMENT MAXIMUM
C        LE SYSTEME LOCAL A RESOUDRE A POUR DIMENSION MAXIMUM NRMAX=
      INTEGER       NBCOMM(NMAT,3),NSG,NFS,NRM
      PARAMETER     ( NSG=30)
      PARAMETER     ( NFS=5)
      PARAMETER     ( NRM=NFS*NSG+6)
      REAL*8        TOUTMS(NFS,NSG,6),HSR(NFS,NSG,NSG)
      REAL*8        DRDY(NRM*NRM)
      CHARACTER*16  CPMONO(5*NMAT+1)
      
C       ----------------------------------------------------------------
        COMMON /TDIM/   NDT  , NDI
C       ----------------------------------------------------------------
C
C --    INITIALISATION DES PARAMETRES DE CONVERGENCE ET ITERATIONS
C
      IRTETI   = 0
      IRTET    = 0
      ITMAX    = INT(CRIT(1))
      TOLER    =     CRIT(3)
      THETA    =     CRIT(4)
      LOI      = COMP(1)
      MOD      = TYPMOD(1)
      DT       = TIMEF - TIMED
C
C --  OPTION SUPPRIMEE CAR TYPMA EST IMPOSE SUIVANT QUE L'ON EST EN
C --  PLASTCITE OU VISCOPLASTICITE. TYPMA EST DEFINI DANS LCMATE
C --  POUR LES MODELE VISCO-PLASTIQUES A LA VALEUR 'COHERENT'.
C      IF ( INT(CRIT(2)) .EQ. 0 ) THEN
C        TYPMA = 'VITESSE '
C      ELSE
C        TYPMA = 'COHERENT'
C      ENDIF
C
      TYPMA = 'VITESSE '
C
      IF ( ITMAX .LE. 0 )ITMAX = -ITMAX
C
C --  RECUPERATION COEF(TEMP(T))) LOI ELASTO-PLASTIQUE A T ET/OU T+DT
C     NB DE CMP DIRECTES/CISAILLEMENT + NB VAR. INTERNES
C
      CALL LCMATE( FAMI,KPG,KSP,COMP,MOD,IMAT,NMAT,TEMPD,TEMPF,0,
     &             TYPMA,HSR,MATERD,MATERF,MATCST,NBCOMM,CPMONO,
     &     ANGMAS,PGL,ITMAX,TOLER,NDT,NDI,NR,NVI,VIND,NFS,NSG,TOUTMS)
      IF (LOI(1:11).EQ.'MONOCRISTAL') THEN
         IF(MOD.NE.'3D') THEN
            SIGD(5)=0.D0
            SIGD(6)=0.D0
            EPSDT(5)=0.D0
            EPSDT(6)=0.D0
            DEPST(5)=0.D0
            DEPST(6)=0.D0
         ENDIF
      ENDIF

      IF ((COMP(3).EQ.'SIMO_MIEHE').AND.(COMP(1).EQ.'MONOCRISTAL')) THEN
C        GDEF_MONO : PAS DE DEFORM. THERMIQUE
         CALL DCOPY(9,DEPST,1,DEPS,1)
         CALL DCOPY(9,EPSDT,1,EPSD,1)
      
      ELSE
C --     RETRAIT INCREMENT DE DEFORMATION DUE A LA DILATATION THERMIQUE
         CALL LCDEDI(FAMI,KPG,KSP, NMAT,  MATERD, MATERF,
     &            TEMPD, TEMPF,TREF,DEPST, EPSDT, DEPS, EPSD )
C
C --     RETRAIT ENDOGENNE ET RETRAIT DE DESSICCATION
         CALL LCDEHY(FAMI, KPG, KSP, NMAT, MATERD, MATERF,
     &            DEPS, EPSD )
      ENDIF
C
C --    SEUIL A T > ETAT ELASTIQUE OU PLASTIQUE A T
      IF  ( ABS(VIND (NVI)) .LE. EPSI ) THEN
         ETATD = 'ELASTIC'
      ELSE
         ETATD = 'PLASTIC'
      ENDIF
C
C --> REDECOUPAGE IMPOSE
      IF ( ICOMP .EQ. -1 .AND. OPT .NE. 'RIGI_MECA_TANG') THEN
         IRTETI = 0
         GOTO 9999
      ENDIF
C
C     ----------------------------------------------------------------
C     OPTIONS 'FULL_MECA' ET 'RAPH_MECA' = CALCUL DE SIG(T+DT)
C     ----------------------------------------------------------------
C
      IF ( OPT .EQ. 'RAPH_MECA' .OR. OPT .EQ. 'FULL_MECA' ) THEN
      
      IF ((COMP(3).EQ.'SIMO_MIEHE').AND.(COMP(1).EQ.'MONOCRISTAL')) THEN
C          GDEF_MONO : PAS DE SEUIL CAR C'EST PLUS COMPLIQUE
           SEUIL=1.D0
           
         ELSE
         
C --        INTEGRATION ELASTIQUE SUR DT
            CALL LCELAS(LOI, MOD , NMAT, MATERD, MATERF, MATCST,
     &               NVI, DEPS, SIGD, VIND,   SIGF,   VINF,
     &               THETA)

C --        PREDICTION ETAT ELASTIQUE A T+DT : F(SIG(T+DT),VIN(T)) = 0 ?
            CALL LCCNVX(FAMI, KPG, KSP, LOI, IMAT, NMAT, MATERF,
     &               SIGF, VIND, NBCOMM, CPMONO, PGL, NVI,
     &               VP,VECP, HSR,NFS,NSG, TOUTMS, TIMED,TIMEF, SEUIL)
         ENDIF
C
         IF ( SEUIL .GE. 0.D0 ) THEN
C --        PREDICTION INCORRECTE > INTEGRATION ELASTO-PLASTIQUE SUR DT
            ETATF = 'PLASTIC'
C
            CALL LCPLAS(FAMI,KPG,KSP,LOI,TOLER,ITMAX,MOD,IMAT,NMAT,
     &                  MATERD,MATERF, NR, NVI,
     &                  TIMED,TIMEF, DEPS, EPSD, SIGD ,VIND, SIGF,
     &                 VINF,COMP,NBCOMM, CPMONO, PGL,NFS,NSG,TOUTMS,HSR,
     &                  ICOMP, IRTET, THETA,VP,VECP,SEUIL, DEVG,
     &                  DEVGII,DRDY,TAMPON,CRIT)
C
            IF ( IRTET.GT.0 ) GOTO (1,2), IRTET
         ELSE
C --        PREDICTION CORRECTE > INTEGRATION ELASTIQUE FAITE
            ETATF = 'ELASTIC'
C ---       MISE A JOUR DE VINF EN FONCTION DE LA LOI
C           ET POST-TRAITEMENTS POUR DES LOIS PARTICULIERES
            CALL LCELPL(LOI,NMAT,MATERF,NVI,VIND,VINF)
         ENDIF
C
         IF ( LOI(1:6) .EQ. 'LAIGLE') THEN
            CALL LGLDCM( NMAT, MATERF, SIGF, VINF )
         ENDIF
C
C --     CONTRAINTES PLANES
         IF ( MOD(1:6) .EQ. 'C_PLAN' ) SIGF(3) = 0.D0
C
      ENDIF
C
C       ----------------------------------------------------------------
C       OPTIONS 'FULL_MECA' ET 'RIGI_MECA_TANG' = CALCUL DE DSDE
C       ----------------------------------------------------------------
C       EVALUATION DU JACOBIEN DSDE A (T+DT) POUR 'FULL_MECA'
C       ET CALCUL ELASTIQUE    ET   A (T)    POUR 'RIGI_MECA_TANG'
C       ----------------------------------------------------------------
C
      IF ( OPT .EQ. 'RIGI_MECA_TANG' .OR. OPT .EQ. 'FULL_MECA' ) THEN
         IF ( OPT .EQ. 'RIGI_MECA_TANG' ) THEN
            IF (ETATD.EQ.'ELASTIC'.OR.LOI.EQ.'LAIGLE') THEN
               CALL LCJELA ( LOI  , MOD , NMAT, MATERD, VIND, DSDE)
            ELSEIF ( ETATD .EQ. 'PLASTIC') THEN
C   ------>    ELASTOPLASTICITE ==> TYPMA = 'VITESSE '
C   ------>    VISCOPLASTICITE  ==> TYPMA = 'COHERENT '==> ELASTIQUE
               IF( TYPMA .EQ. 'COHERENT' ) THEN
                  IF (LOI(1:11).EQ.'MONOCRISTAL') THEN
                     CALL LCJPLC(LOI,MOD,NMAT,MATERF,
     &                           TIMED,TIMEF,COMP,NBCOMM,CPMONO,
     &                          PGL,NFS,NSG,TOUTMS,HSR,NR,NVI,EPSD,DEPS,
     &                           ITMAX,TOLER,SIGD,VIND,SIGD,VIND,
     &                           DSDE,DRDY,OPT,IRET)
                     IF (IRET.NE.0) GOTO 1
                  ELSE
                     CALL LCJELA(LOI,MOD,NMAT,MATERD,VIND,DSDE)
                  ENDIF
               ELSEIF ( TYPMA .EQ. 'VITESSE ' ) THEN
C ---             HOEK-BROWN : CALCUL DES VALEURS ET VECTEURS PROPRES
C ---                          DU DEVIATEUR ELASTIQUE
                  IF ((LOI(1:10).EQ.'HOEK_BROWN').OR.
     &                (LOI(1:14).EQ.'HOEK_BROWN_EFF')) THEN
                     CALL LCHBVP(SIGD,VP,VECP)
                  ENDIF
                  CALL LCJPLA(FAMI,KPG,KSP,LOI,MOD,NR,IMAT,NMAT,MATERD,
     &                        NVI,DEPS,SIGD,VIND,DSDE,SIGD,VIND,VP,VECP,
     &                        THETA,DT,DEVG,DEVGII,CODRET)
                  IF (CODRET.EQ.2) GOTO 2
               ENDIF
            ENDIF
C
         ELSEIF ( OPT .EQ . 'FULL_MECA' ) THEN
            IF  ( ETATF .EQ. 'ELASTIC' ) THEN
               CALL LCJELA ( LOI  , MOD , NMAT, MATERF, VINF, DSDE)
            ELSEIF ( ETATF .EQ. 'PLASTIC' ) THEN
C   ------>    ELASTOPLASTICITE ==>  TYPMA = 'VITESSE '
C   ------>    VISCOPLASTICITE  ==>  TYPMA = 'COHERENT '
               IF     ( TYPMA .EQ. 'COHERENT' ) THEN
                  CALL LCJPLC(LOI,MOD,NMAT,MATERF,
     &                        TIMED,TIMEF,COMP,NBCOMM,CPMONO,
     &                        PGL,NFS,NSG,TOUTMS,HSR,NR,NVI,EPSD,DEPS,
     &                        ITMAX,TOLER,SIGF,VINF,SIGD,VIND,
     &                        DSDE,DRDY,OPT,IRET)
                  IF (IRET.NE.0) GOTO 1
               ELSEIF ( TYPMA .EQ. 'VITESSE ' ) THEN
                  CALL LCJPLA(FAMI,KPG,KSP,LOI,MOD,NR,IMAT,NMAT,MATERD,
     &                        NVI,DEPS,SIGF,VINF,DSDE,SIGD,VIND,VP,VECP,
     &                        THETA,DT,DEVG, DEVGII,CODRET)
                  IF (CODRET.EQ.2) GOTO 2
               ENDIF
            ENDIF
C
         ENDIF
C
C -      MODIFICATION EN CONTRAINTE PLANES POUR TENIR COMPTE DE
C        SIG3=0 ET DE LA CONSERVATION DE L'ENERGIE
         IF ( MOD(1:6).EQ.'C_PLAN' )THEN
            DO 136 K=1,NDT
               IF (K.EQ.3) GO TO 136
               DO 137 L=1,NDT
                  IF (L.EQ.3) GO TO 137
                  DSDE(K,L) = DSDE(K,L)
     &                      - 1.D0/DSDE(3,3)*DSDE(K,3)*DSDE(3,L)
 137           CONTINUE
 136        CONTINUE
         ENDIF
      ENDIF
C
C       ----------------------------------------------------------------
C
      IRTETI = 0
      GOTO 9999
1      CONTINUE
      IRTETI = 1
      GOTO 9999
C
2      CONTINUE
      IRTETI = 2
      GOTO 9999
C
9999   CONTINUE

      END
