         SUBROUTINE PLASTI ( TYPMOD, IMAT,  COMP,  CRIT, TIMED,
     1                       TIMEF, TEMPD, TEMPF, TREF, HYDRD, HYDRF,
     2                       SECHD, SECHF, SREF, EPSDT, DEPST, SIGD, 
     3                       VIND,OPT,SIGF, VINF, DSDE, ICOMP, NVI, 
     4                       IRTETI)
        IMPLICIT REAL*8 (A-H,O-Z)
C       ================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 16/06/2004   AUTEUR JMBHH01 J.M.PROIX 
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
C TOLE CRP_21
C       ================================================================
C       INTEGRATION DE LOIS DE COMPORTEMENT ELASTO PLASTIQUE ET VISCO
C       PLASTIQUE
C               AVEC    . N VARIABLES INTERNES
C                       . UNE FONCTION SEUIL ELASTIQUE
C
C       INTEGRATION DES CONTRAINTES           = SIG(T+DT)
C       INTEGRATION DES VARIABLES INTERNES    = VIN(T+DT)
C       ET CALCUL DU JACOBIEN ASSOCIE         = DS/DE(T+DT) OU DS/DE(T)
C
C       EN CAS DE NON-CONVERGENCE LOCALE ON EFFECTUE UN REDECOUPAGE DU
C       PAS DE TEMPS, L ORDRE D EXECUTION ETANT REMONTE EN ARGUMENT
C       DANS REDECE, APPELE PAR NMCOMP AVANT PLASTI
C       ================================================================
C       ROUTINES D INTEGRATION IMPLICITE DUN MODELE ELASTO PLASTIQUE
C
C       PLASTI  ALGORITHME D INTEGRATION ELASTO PLASTIQUE
C       LCTYMO  TYPE DE MODELISATION
C       LCMATE  RECUPERATION DES DONNEES MATERIAU CONSTANTES SUR DT
C               + DIMENSION DES TENSEURS / VARIABLES INTERNES
C       LCDEDI  RETRAIT DE L INCREMENT DE DEFORMATION THERMIQUE
C       LCCNVX  SEUIL ELASTIQUE
C       LCPLAS  INTEGRATION ELASTO_PLASTIQUE
C       LCELAS  INTEGRATION ELASTIQUE
C       LCPLNL  INTEGRATION EP = SYSTEME IMPLICITE NON LINEAIRE
C       LCPLLI  INTEGRATION EP = SYSTEME IMPLICITE LINEAIRE
C       LCINIT  SOLUTION ESSAI INITIALE POUR LA RESOLUTION PAR NEWTON
C       LCRESI  TERMES DU SYSTEME NON LINEAIRE (SNL)EN (DSIG DVIN DEPS3)
C       LCPL2M  TERMES DU SECOND MEMBRE DU SYSTEME  LINEAIRE
C       LCJACB  TERMES DE LA MATRICE JACOBIENNE DU SYSTEME NON LINEAIRE
C       LCPLMA  TERMES DE LA MATRICE  DU SYSTEME  LINEAIRE
C       LCCONV  CONVERGENCE DE LA SOLUTION DU SNL ET CONFORMITE
C       LCJPLA  TERMES MATRICE DE COMPORTEMENT TANGENT EL_PLAS VITESSE
C       LCJPLC  TERMES MATRICE DE COMPORTEMENT TANGENT EL_PLAS COHERENT
C       LCJELA  TERMES DE LA MATRICE DE COMPORTEMENT TANGENT ELASTIQUE
C
C       ----------------------------------------------------------------
C       POINTS D ENTREES           ROUTINES  A ECRIRE POUR INTEGRATION
C       A MODIFIER
C                                  ROUSSELIER   CHABOCHE        XXXXXX
C
C       (LCMATE)                   RSLMAT       CHBMAT          XXXMAT
C       (LCCNVX)                   RSLCVX       CHBCVX          XXXCVX
C       (LCELAS)                   RSLLIN      <LCELIN>        XXXELA
C                                                              ,<LCELIN>
C       (LCPLAS) (LCPLNL) (LCINIT) LCROUS       CHBINI          XXXINI
C                         (LCRESI) LCROUS       CHBRES          XXXRES
C                         (LCJACB) LCROUS       CHBJAC          XXXJAC
C                         (LCCONV) LCROUS       CHBCVG          /XXXCVG/
C                                                              ,<LCCTRL>
C               ,(LCPLLI) (LCPL2M) -            -               XXXP2M
C                         (LCPLMA) -            -               XXXPMA
C       (LCJELA)                   <LCOPLI>     <LCOPLI>        XXXJEL
C                                                              ,<LCOPLI>
C       (LCJPLA)                   RSLJPL       CHBJPL          XXXJPL
C       (LCJPLC)                   -            -               XXXJPC
C
C
C       () = ROUTINES A MODIFIER ( POINT D'AIGUILLAGE )
C       // = ROUTINES A ECRIRE FACULTATIVES
C       <> = ROUTINES UTILITAIRES
C       ,  = OU
C       ----------------------------------------------------------------
C       ROUTINES UTILITAIRES DE CALCUL MATRICIEL(6,6) - VECTORIEL (6)
C       COMMON /TDIM/ NDT,NDI    A INCLURE OBLIGATOIREMENT
C       () = DEBUG
C
C       ( LCIMMA  IMPRESSION MATRICE )
C       ( LCIMVE  IMPRESSION VECTEUR )
C       ( LCIMSC  IMPRESSION SCALAIRE )
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
C       ( LCIMMN  IMPRESSION DE MATRICE )
C       ( LCIMVN  IMPRESSION DE VECTEUR )
C       LCPRSN  PRODUIT SCALAIRE DE VECTEURS
C       LCNRVN  NORME DE VECTEUR
C       LCEQVN  EGALITE DE VECTEURS
C       LCPSVN  PRODUIT SCALAIRE * VECTEUR
C       LCSOVN  SOMME DE VECTEURS
C       LCINVN  INITIALISATION DE VECTEUR
C       LCDIVN  DIFFERENCE DE VECTEURS
C       LCICMA  INCLUSION DUNE PORTION DE MATRICE DANS UNE AUTRE MATRICE
C       LCSOLV  SOLVEUR LU POUR MATRICES CARRES PLEINES
C
C       ----------------------------------------------------------------
C       ROUTINES UTILITAIRES D INTEGRATION D UN MODELE DE COMPORTEMENT
C
C       LCDEVI  PARTIE DEVIATORIQUE D UN TENSEUR
C       LCHYDR  PARTIE SPHERIQUE    D UN TENSEUR
C       LCVS    DERIVEE DE LA CONTRAINTE VON MISES / CONTRAINTE
C       LCVVSS  DERIVEE SECONDE DE LA CONTRAINTE VON MISES / CONTRAINTE
C       LCIV2S  SECOND INVARIANT DU TENSEUR CONTRAINTE
C       LCIV2E  SECOND INVARIANT DU TENSEUR DEFORMATION
C       LCNRTS  'NORME' DU TENSEUR CONTRAINTE
C       LCNRTE  'NORME' DU TENSEUR DEFORMATION
C       LCOPLI  OPERATEUR ELASTIQUE LINEAIRE
C       LCELIN  INTEGRATION  ELASTIQUE LINEAIRE ISOTROPE
C       LCVERR  CALCUL DU VECTEUR D'ERREUR RELATIVE, ABSOLU, NORMEE...
C
C       ================================================================
C       ARGUMENTS
C
C       IN      TYPMOD  TYPE DE MODELISATION
C               IMAT    ADRESSE DU MATERIAU CODE
C               COMP    COMPORTEMENT DE L ELEMENT
C                       COMP(1) = RELATION DE COMPORTEMENT (CHABOCHE...)
C                       COMP(2) = NB DE VARIABLES INTERNES
C                       COMP(3) = TYPE DE DEFORMATION (PETIT,JAUMANN...)
C               OPT     OPTION DE CALCUL A FAIRE
C                               'RIGI_MECA_TANG'> DSDE(T)
C                               'FULL_MECA'     > DSDE(T+DT) , SIG(T+DT)
C                               'RAPH_MECA'     > SIG(T+DT)
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
C               HYDRD   HYDRATATION A L'INSTANT PRECEDENT
C               HYDRF   HYDRATATION A L'INSTANT DU CALCUL
C               SECHD   SECHAGE A L'INSTANT PRECEDENT
C               SECHF   SECHAGE A L'INSTANT DU CALCUL
C               SREF    SECHAGE DE REFERENCE
C               EPSDT   DEFORMATION TOTALE A T
C               DEPST   INCREMENT DE DEFORMATION TOTALE
C               SIGD    CONTRAINTE A T
C               VIND    VARIABLES INTERNES A T    + INDICATEUR ETAT T
C               ICOMP   COMPTEUR POUR LE REDECOUPAGE DU PAS DE TEMPS
C       OUT     SIGF    CONTRAINTE A T+DT
C               VINF    VARIABLES INTERNES A T+DT + INDICATEUR ETAT T+DT
C               DSDE    MATRICE DE COMPORTEMENT TANGENT A T+DT OU T
C               IRTETI = 1 CONTROLE DU REDECOUPAGE DU PAS DE TEMPS
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
C               NVI             NB DE VARIABLES INTERNES
C               NR              NB EQUATION SYSTEME INTEGRE A RESOUDRE
C               BZ              VARIABLE LOGIQUE UTILISEE POUR POLY_CFC
C               JFIS1           INDICATEUR DE FISSURATION POUR NADAI_B
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
        INTEGER         IMAT , NDT   , NDI   , NR  , NVI
        INTEGER         ITMAX, ICOMP  , JFIS1
        INTEGER         NMAT , IOPTIO , IDNR
        INTEGER         IRTET, IRTETI, K, L
        REAL*8          TOLER
        REAL*8          EPSI
        LOGICAL         BZ
C
        PARAMETER       ( EPSI = 1.D-15 )
        PARAMETER       ( NMAT = 90     )
        INTEGER         NBCOMM(NMAT,3)
C
        REAL*8          CRIT(*)
        REAL*8          VIND(*),     VINF(*)
        REAL*8          TIMED,       TIMEF,     TEMPD,    TEMPF  , TREF
        REAL*8          HYDRD , HYDRF , SECHD , SECHF, SREF
        REAL*8          EPSD(6),     DEPS(6)
        REAL*8          EPSDT(6),    DEPST(6)
        REAL*8          SIGD(6),     SIGF(6)
C
        REAL*8          SEUIL, THETA, DT, DEVG(6), DEVGII
C
        REAL*8          DSDE(6,6),  PGL(3,3)
C
        REAL*8          MATERD(NMAT,2) , MATERF(NMAT,2)
C
        CHARACTER*7     ETATD  ,     ETATF
        CHARACTER*8     MOD    ,     TYPMA,   TYPMOD(*)
        CHARACTER*16    COMP(*),     OPT,        LOI, CPMONO(5*NMAT+1)
        CHARACTER*3     MATCST
C       ----------------------------------------------------------------
        COMMON /TDIM/   NDT  , NDI
C       ----------------------------------------------------------------
C
C --    INITIALISATION DES PARAMETRES DE CONVERGENCE ET ITERATIONS
C
        JFIS1 = 0
        IRTETI = 0
        ITMAX    = INT(CRIT(1))
        TOLER    =     CRIT(3)
        THETA    =     CRIT(4)
        LOI      = COMP(1)
        MOD      = TYPMOD(1)
        DT = TIMEF - TIMED
C
C --    OPTION SUPPRIMEE CAR TYPMA EST IMPOSE SUIVANT QUE L'ON EST EN
C --    PLASTCITE OU VISCOPLASTICITE. TYPMA EST DEFINI DANS LCMATE
C --    POUR LES MODELE VISCO-PLASTIQUES A LA VALEUR 'COHERENT'.
C          IF ( INT(CRIT(2)) .EQ. 0 ) THEN
C          TYPMA = 'VITESSE '
C          ELSE
C          TYPMA = 'COHERENT'
C          ENDIF
C
          TYPMA = 'VITESSE '
C
          IF ( ITMAX .LE. 0 )ITMAX = -ITMAX
C
C --    RECUPERATION COEF(TEMP(T))) LOI ELASTO-PLASTIQUE A T ET/OU T+DT
C                    NB DE CMP DIRECTES/CISAILLEMENT + NB VAR. INTERNES
C
        CALL LCMATE (COMP, MOD, IMAT, NMAT, TEMPD, TEMPF, HYDRD, HYDRF,
     1                SECHD, SECHF, TYPMA, BZ, MATERD,MATERF, MATCST,
     3                NBCOMM, CPMONO, PGL,
     2                NDT , NDI , NR, NVI, VIND)
C
C --    RETRAIT INCREMENT DE DEFORMATION DUE A LA DILATATION THERMIQUE
C
        CALL LCDEDI ( NMAT,  MATERD, MATERF, TEMPD, TEMPF, TREF,
     &                DEPST, EPSDT, DEPS,   EPSD )
C
C               CALL LCIMVE ( 'DEPST = ', DEPST )
C               CALL LCIMVE ( 'EPSDT = ', EPSDT )
C               CALL LCIMVE ( 'DEPS = ' , DEPS )
C               CALL LCIMVE ( 'EPSD = ' , EPSD )
C
C --    RETRAIT ENDOGENNE ET RETRAIT DE DESSICCATION
C
        CALL LCDEHY ( NMAT,  MATERD, MATERF, HYDRD,  HYDRF,
     &                SECHD, SECHF, SREF, DEPS,   EPSD )
C
C --    SEUIL A T > ETAT ELASTIQUE OU PLASTIQUE A T
C
              IF( LOI(1:7) .EQ. 'NADAI_B' ) THEN
C
C ------------- ETAT DU BETON A T POUR NADAI_B
C
                CALL INSETA (ETATD,VIND,NVI)
              ELSE
                IF  ( ABS(VIND (NVI)) .LE. EPSI ) THEN
                ETATD = 'ELASTIC'
                ELSE
                ETATD = 'PLASTIC'
                ENDIF
              ENDIF
C
C
C --> REDECOUPAGE IMPOSE
        IF ( ICOMP .EQ. -1 .AND. OPT .NE. 'RIGI_MECA_TANG') THEN
            IRTETI = 0
            GOTO 9999
        ENDIF
C
C       ----------------------------------------------------------------
C       OPTIONS 'FULL_MECA' ET 'RAPH_MECA' = CALCUL DE SIG(T+DT)
C       ----------------------------------------------------------------
C
        IF ( OPT .EQ. 'RAPH_MECA' .OR. OPT .EQ. 'FULL_MECA' ) THEN
C
C --    INTEGRATION ELASTIQUE SUR DT
C
        CALL LCELAS ( LOI  ,MOD , NMAT, MATERD, MATERF, MATCST,
     1                NVI,  DEPS,
     2                SIGD ,VIND,  SIGF,  VINF, THETA )
C
C --    TEST DE FISSURATION POUR NADAI_B , SI LE BETON EST FISSURE
C       LE TRAITEMENT EST EFFECTUE DANS INSTEF
C
       IF ( LOI(1:7) .EQ. 'NADAI_B') THEN
          CALL INSTEF ( NMAT, MATERF, SIGD, SIGF, VIND, VINF,
     1    ETATF, EPSD, DEPS, DSDE, JFIS1, NVI, MOD )
C
         IF ( JFIS1 .EQ. 1 ) GOTO 10
        ENDIF
C
C --    PREDICTION ETAT ELASTIQUE A T+DT : F(SIG(T+DT),VIN(T)) = 0 ?
C
        CALL LCCNVX ( LOI, IMAT, NMAT, MATERF, TEMPF, SIGF, VIND,
     &                COMP, NBCOMM, CPMONO, PGL, NR, NVI, SEUIL )
C
          IF ( SEUIL .GE. 0.D0 ) THEN
C
C --      PREDICTION INCORRECTE > INTEGRATION ELASTO-PLASTIQUE SUR DT
C
          ETATF = 'PLASTIC'
C
          CALL LCPLAS ( LOI, TOLER, ITMAX, MOD, IMAT, NMAT, MATERD,
     1                  MATERF, MATCST, NR, NVI, TEMPD, TEMPF, TIMED,
     2                  TIMEF, DEPS,   EPSD,  SIGD ,VIND, SIGF, VINF,
     3                COMP,NBCOMM, CPMONO, PGL,
     3                  ICOMP, IRTET, THETA, SEUIL, DEVG, DEVGII)
C
          IF ( IRTET.GT.0 ) GOTO (1), IRTET
          ELSE
C
C --      PREDICTION CORRECTE > INTEGRATION ELASTIQUE FAITE
C
          ETATF = 'ELASTIC'
          ENDIF
C
C --    TEST DE FISSURATION POUR NADAI_B , SI LE BETON EST FISSURE
C       LE TRAITEMENT EST EFFECTUE DANS INSTEF
C
       IF ( LOI(1:7) .EQ. 'NADAI_B') THEN
          CALL INSTEF ( NMAT, MATERF, SIGD, SIGF, VIND, VINF,
     1    ETATF, EPSD, DEPS, DSDE, JFIS1, NVI, MOD )
       ELSE IF ( LOI(1:6) .EQ. 'LAIGLE') THEN
          CALL LGLDCM( NMAT, MATERF, SIGF, VINF )
       ENDIF
   10  CONTINUE
C
C --    CONTRAINTES PLANES
C
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
C
        IF ( LOI(1:7) .NE. 'NADAI_B' .OR. JFIS1 .EQ. 0 ) THEN
        CALL LCINMA ( 0.D0 , DSDE )
        ENDIF
C
          IF ( OPT .EQ. 'RIGI_MECA_TANG' ) THEN
          IF (ETATD.EQ.'ELASTIC'.OR.JFIS1.EQ.1.OR.LOI.EQ.'LAIGLE') THEN
            CALL LCJELA ( LOI  , MOD , NMAT, MATERD, VIND, DSDE)
            ELSEIF ( ETATD .EQ. 'PLASTIC' .AND. JFIS1 .EQ. 0 ) THEN
C   ------> ELASTOPLASTICITE ==> TYPMA = 'VITESSE '
C   ------> VISCOPLASTICITE  ==> TYPMA = 'COHERENT '==> CALCUL ELASTIQUE
                IF     ( TYPMA .EQ. 'COHERENT' ) THEN
                CALL LCJELA ( LOI  , MOD , NMAT, MATERD,  VIND, DSDE)
                ELSEIF ( TYPMA .EQ. 'VITESSE ' ) THEN
               CALL LCJPLA ( LOI  , MOD ,  IMAT,  NMAT, MATERD, NVI,
     2              TEMPD, DEPS, SIGD ,  VIND, DSDE, VIND,
     3              THETA, DT, DEVG, DEVGII)
                ENDIF
            ENDIF
C
          ELSEIF ( OPT .EQ . 'FULL_MECA' ) THEN
                IF  ( ETATF .EQ. 'ELASTIC' .OR. JFIS1 .EQ. 1 ) THEN
            CALL LCJELA ( LOI  , MOD , NMAT, MATERF, VINF, DSDE)
            ELSEIF ( ETATF .EQ. 'PLASTIC' .AND. JFIS1 .EQ. 0 ) THEN
C   ------> ELASTOPLASTICITE ==>  TYPMA = 'VITESSE '
C   ------> VISCOPLASTICITE  ==>  TYPMA = 'COHERENT '
                IF     ( TYPMA .EQ. 'COHERENT' ) THEN

                CALL LCJPLC ( LOI  , MOD ,  NMAT, MATERF,
     &            TIMED, TIMEF, COMP,NBCOMM, CPMONO, PGL,NR,NVI,
     &                  SIGF,VINF,SIGD,VIND, 
     &                   DSDE )

                ELSEIF ( TYPMA .EQ. 'VITESSE ' ) THEN
               CALL LCJPLA ( LOI  , MOD ,  IMAT,  NMAT, MATERD, NVI,
     2              TEMPD, DEPS, SIGF ,  VINF, DSDE, VIND,
     3              THETA, DT, DEVG, DEVGII)
                ENDIF
            ENDIF
C
          ENDIF
C
C -      MODIFICATION EN CONTRAINTE PLANES POUR TENIR COMPTE DE
C        SIG3=0 ET DE LA CONSERVATION DE L'ENERGIE
C
         IF ( MOD(1:6).EQ.'C_PLAN' )THEN
          DO 136 K=1,NDT
            IF (K.EQ.3) GO TO 136
            DO 137 L=1,NDT
              IF (L.EQ.3) GO TO 137
              DSDE(K,L)=DSDE(K,L)
     +          - 1.D0/DSDE(3,3)*DSDE(K,3)*DSDE(3,L)
 137        CONTINUE
 136      CONTINUE
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
 9999   CONTINUE
        END
