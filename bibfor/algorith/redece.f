        SUBROUTINE REDECE ( NDIM, TYPMOD, IMAT, COMP, CRIT,
     1                      TIMED,TIMEF, TEMPD,TEMPF,TREF,HYDRD,
     &                      HYDRF,SECHD,SECHF,SREF,EPSDT,DEPST,SIGD,
     2                      VIND, OPT,ELGEOM,SIGF,VINF,DSDE)
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
C       - SI CRIT(5) = -N  EN CAS DE NON-CONVERGENCE LOCALE ON EFFECTUE
C                          UN REDECOUPAGE DU PAS DE TEMPS EN N PALIERS
C                          L ORDRE D EXECUTION ETANT REMONTE EN ARGUMENT
C                          DANS REDECE, APPELE PAR NMCOMP AVANT PLASTI
C       - SI CRIT(5) = -1,0,1  PAS DE REDECOUPAGE DU PAS DE TEMPS
C       - SI CRIT(5) = +N  ON EFFECTUE UN REDECOUPAGE DU PAS DE TEMPS
C                          EN N PALIERS A CHAQUE APPEL DE REDECE
C                          LE PREMIER APPEL DE PLASTI SERT A
C                          L'INITIALISATION DE NVI
C       SI APRES REDECOUPAGE ON ABOUTIT A UN CAS DE NON CONVERGENCE, ON
C       REDECOUPE A NOUVEAU LE PAS DE TEMPS, EN 2*N PALIERS
C       ================================================================
C
C       PLASTI  ALGORITHME D INTEGRATION ELASTO PLASTIQUE
C       LCXXXX  ROUTINES UTILITAIRES (VOIR LE DETAIL DANS PLASTI)
C       ================================================================
C       ARGUMENTS
C
C       IN      NDIM    DIMENSION DE L ESPACE (3D=3,2D=2,1D=1)
C               TYPMOD  TYPE DE MODELISATION
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
C                                 (ITER_INTE_PAS == ITEDEC)
C                                 0 = PAS DE REDECOUPAGE
C                                 N = NOMBRE DE PALIERS
C               ELGEOM  TABLEAUX DES ELEMENTS GEOMETRIQUES SPECIFIQUES
C                       AUX LOIS DE COMPORTEMENT (DIMENSION MAXIMALE
C                       FIXEE EN DUR)
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
C    ATTENTION  VIND    VARIABLES INTERNES A T MODIFIEES SI REDECOUPAGE
C       OUT     SIGF    CONTRAINTE A T+DT
C               VINF    VARIABLES INTERNES A T+DT + INDICATEUR ETAT T+DT
C               DSDE    MATRICE DE COMPORTEMENT TANGENT A T+DT OU T
C       ----------------------------------------------------------------
        INTEGER         IMAT , NDIM   , NDT   , NDI  , NVI
C
        REAL*8          CRIT(*)
        REAL*8          TIMED,     TIMEF,    TEMPD,   TEMPF  , TREF
        REAL*8          HYDRD , HYDRF , SECHD , SECHF , SREF, ELGEOM(*)
        REAL*8          EPSDT(6),  DEPST(6)
        REAL*8          SIGD(6),   SIGF(6)
        REAL*8          VIND(*),   VINF(*)
        REAL*8          DSDE(6,6)
C
        CHARACTER*16    COMP(*),     OPT
        CHARACTER*8     TYPMOD(*)
C
C       ----------------------------------------------------------------
C       VARIABLES LOCALES POUR LE REDECOUPAGE DU PAS DE TEMPS
C               TD      INSTANT T
C               TF      INSTANT T+DT
C               TEMD    TEMPERATURE A T
C               TEMF    TEMPERATURE A T+DT
C               HYDD   HYDRATATION A T
C               HYDF   HYDRATATION A T+DT
C               SECD   SECHAGE A T
C               SECF   SECHAGE A T+DT
C               EPS     DEFORMATION TOTALE A T
C               DEPS    INCREMENT DE DEFORMATION TOTALE
C               SD      CONTRAINTE A T
C               VD      VARIABLES INTERNES A T    + INDICATEUR ETAT T
C               DSDELO MATRICE DE COMPORTEMENT TANGENT A T+DT OU T
C               NPAL            NOMBRE DE PALIER POUR LE REDECOUPAGE
C               ICOMP           COMPTEUR POUR LE REDECOUPAGE DU PAS DE
C                                    TEMPS
C               RETURN1 EN CAS DE NON CONVERGENCE LOCALE
C       ----------------------------------------------------------------
C
        INTEGER         ICOMP,        NPAL,      IPAL
        INTEGER         NMOD ,        IRTET,     K
        REAL*8          EPS(6),       DEPS(6),   SD(6)
        REAL*8          DSDELO(6,6)
        REAL*8          TD,           TF,        DELTAT
        REAL*8          TEMD,         TEMF,      DETEMP
        REAL*8          HYDD,         HYDF,      DEHYDR
        REAL*8          SECD,         SECF,      DESECH
        CHARACTER*132   RAISON
C       ----------------------------------------------------------------
        COMMON /TDIM/   NDT  , NDI
C       ----------------------------------------------------------------
        IPAL  =  INT(CRIT(5))
C
C IPAL = 0  1 OU -1 -> PAS DE REDECOUPAGE DU PAS DE TEMPS
C
        IF ( IPAL .EQ. 0 .OR. IPAL .EQ. 1 .OR. IPAL .EQ. -1 ) THEN
           IPAL  = 0
           NPAL  = 0
           ICOMP = 2
C
C IPAL < -1 -> REDECOUPAGE DU PAS DE TEMPS EN CAS DE NON CONVERGENCE
C
        ELSEIF ( IPAL .LT. -1 ) THEN
           NPAL  = -IPAL
           ICOMP = 0
C
C IPAL > 1 -> REDECOUPAGE IMPOSE DU PAS DE TEMPS
C
        ELSEIF ( IPAL .GT.  1 ) THEN
           NPAL  = IPAL
           ICOMP = -1
        ENDIF
C
        IF ( COMP(1)(1:15) .EQ. 'BETON_DOUBLE_DP') THEN
        CALL PLASBE ( TYPMOD, IMAT,  COMP,  CRIT,
     1                TEMPD, TEMPF, TREF,
     2                HYDRD, HYDRF, SECHD, SECHF, SREF,
     3                EPSDT, DEPST, SIGD,  VIND,  OPT, ELGEOM,
     4                SIGF,  VINF,  DSDE,  ICOMP, NVI,  IRTET)
        ELSE
        CALL PLASTI ( TYPMOD, IMAT,  COMP,  CRIT,
     1                TIMED, TIMEF, TEMPD, TEMPF, TREF,
     2                HYDRD, HYDRF, SECHD, SECHF, SREF,
     3                EPSDT, DEPST, SIGD,  VIND,  OPT,
     4                SIGF,  VINF,  DSDE,  ICOMP, NVI,  IRTET)
        ENDIF
        IF ( IRTET.GT.0 ) GOTO (1), IRTET
C
C -->   IPAL > 0 --> REDECOUPAGE IMPOSE DU PAS DE TEMPS
C -->   REDECOUPAGE IMPOSE ==>  RETURN DANS PLASTI APRES RECHERCHE
C       DES CARACTERISTIQUES DU MATERIAU A (T) ET (T+DT)
        IF ( IPAL  .LE.   0 ) GOTO 9999
        IF ( ICOMP .EQ.  -1 ) ICOMP = 0
C
        IF ( OPT .EQ. 'RIGI_MECA_TANG' ) GOTO 9999
C
C
C --    CAS DE NON CONVERGENCE LOCALE / REDECOUPAGE DU PAS DE TEMPS
C
 1      CONTINUE
C
        IF ( NPAL .EQ. 0 ) THEN
            CALL UTEXCP(23,'REDECE',
     1          'REDECOUPAGE DEMANDE APRES NON CONVERGENCE '
     2    //'LOCAL : MODIFIER ITER_INTE_PAS DANS L''OPTION CONVERGENCE')
        ENDIF
C
        IF ( ICOMP .GT. 3 ) THEN
            CALL UTEXCP(23,'REDECE','REDECOUPAGE EXCESSIF DU PAS DE'
     1       //' TEMPS INTERNE : REDUISEZ VOTRE PAS DE TEMPS OU'
     3       //' AUGMENTER ABS(ITER_INTE_PAS)')
        ENDIF
C
        IF ( ICOMP .GE. 1 ) NPAL = 2 * NPAL
        ICOMP = ICOMP + 1
C
        DO 124 K=1,NPAL
C --       INITIALISATION DES VARIABLES POUR LE REDECOUPAGE DU PAS
           IF ( K .EQ. 1 ) THEN
                TD = TIMED
                DELTAT = (TIMEF - TIMED) / NPAL
                TF = TD + DELTAT
                TEMD = TEMPD
                DETEMP = (TEMPF - TEMPD) / NPAL
                TEMF = TEMD + DETEMP
                DEHYDR = (HYDRF - HYDRD)  / NPAL
                HYDD = HYDRD
                HYDF = HYDD + DEHYDR
                DESECH = (SECHF - SECHD)  / NPAL
                SECD = SECHD
                SECF = SECD + DESECH
                IF ( OPT .EQ. 'RIGI_MECA_TANG'
     1                  .OR. OPT .EQ. 'FULL_MECA' )
     1                  CALL LCINMA ( 0.D0 , DSDE )
                CALL LCEQVE ( EPSDT     , EPS            )
                CALL LCEQVE ( DEPST     , DEPS           )
                CALL LCPRSV ( 1.D0/NPAL , DEPS    , DEPS )
                CALL LCEQVN ( NDT       , SIGD    , SD   )
C
C --        REACTUALISATION DES VARIABLES POUR L INCREMENT SUIVANT
            ELSE IF ( K .GT. 1 ) THEN
                TD = TF
                TF = TF + DELTAT
                TEMD = TEMF
                TEMF = TEMF + DETEMP
                HYDD = HYDF
                HYDF = HYDF + DEHYDR
                SECD = SECF
                SECF = SECF + DESECH
                CALL LCSOVE ( EPS     , DEPS    , EPS  )
                IF ( OPT .NE. 'RIGI_MECA_TANG' ) THEN
                    CALL LCEQVN ( NDT     , SIGF    , SD   )
                    CALL LCEQVN ( NVI     , VINF    , VIND   )
                ENDIF
            ENDIF
C
C
            IF ( COMP(1)(1:15) .EQ. 'BETON_DOUBLE_DP') THEN
              CALL PLASBE ( TYPMOD,  IMAT,   COMP,   CRIT,
     1                    TEMD,   TEMF,   TREF,  HYDD, HYDF,
     2                    SECD, SECF, SREF, EPS,   DEPS,
     3                    SD,  VIND,   OPT, ELGEOM, SIGF, VINF,
     4                    DSDELO,  ICOMP,   NVI,  IRTET)
            ELSE
              CALL PLASTI ( TYPMOD,  IMAT,   COMP,   CRIT,  TD,
     1                    TF,    TEMD,   TEMF,   TREF,  HYDD, HYDF,
     2                    SECD, SECF, SREF,  EPS,   DEPS,
     2                    SD,    VIND,     OPT,    SIGF,   VINF,
     3                    DSDELO,  ICOMP,   NVI,  IRTET)
            ENDIF
            IF ( IRTET.GT.0 ) GOTO (1), IRTET
C
            IF ( OPT .EQ. 'RIGI_MECA_TANG'
     1               .OR. OPT .EQ. 'FULL_MECA' ) THEN
                CALL LCPRSM ( 1.D0/NPAL , DSDELO , DSDELO   )
                CALL LCSOMA ( DSDE    , DSDELO , DSDE      )
            ENDIF
C
 124    CONTINUE
C
C
 9999   CONTINUE
        END
