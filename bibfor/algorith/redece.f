        SUBROUTINE REDECE ( FAMI,KPG,KSP,NDIM,TYPMOD,IMATE,COMPOR,CRIT,
     &                      TIMED,TIMEF,CP,NUMLC,TEMPD,TEMPF,TREF,
     &                      EPSDT,DEPST,SIGD,VIND, OPT,TAMPON,ANGMAS,
     &                      SIGF,VINF,DSDE,RETCOM)
        IMPLICIT NONE
C       ================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
C TOLE CRP_21
C RESPONSABLE JMBHH01 J.M.PROIX
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
C                          DANS REDECE, APPELE PAR NMCOMPOR AVANT PLASTI
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
C       IN      FAMI    FAMILLE DE POINT DE GAUSS (RIGI,MASS,...)
C       IN      KPG,KSP NUMERO DU (SOUS)POINT DE GAUSS
C       IN      NDIM    DIMENSION DE L ESPACE (3D=3,2D=2,1D=1)
C               TYPMOD  TYPE DE MODELISATION
C               IMATE    ADRESSE DU MATERIAU CODE
C               COMPOR    COMPORTEMENT DE L ELEMENT
C                     COMPOR(1) = RELATION DE COMPORTEMENT (CHABOCHE...)
C                     COMPOR(2) = NB DE VARIABLES INTERNES
C                     COMPOR(3) = TYPE DE DEFORMATION (PETIT,JAUMANN...)
C               OPT     OPTION DE CALCUL A FAIRE
C                               'RIGI_MECA_TANG'> DSDE(T)
C                               'FULL_MECA'     > DSDE(T+DT) , SIG(T+DT)
C                               'RAPH_MECA'     > SIG(T+DT)
C               CRIT    CRITERES  LOCAUX
C                       CRIT(1) = NOMBRE D ITERATIONS MAXI A CONVERGENCE
C                                 (ITER_INTE_MAXI == ITECREL)
C                       CRIT(2) = TYPE DE JACOBIEN A T+DT
C                                 (TYPE_MATR_COMPOR == MACOMPOR)
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
C               TIMED   INSTANT T
C               TIMEF   INSTANT T+DT
C               EPSDT   DEFORMATION TOTALE A T
C               DEPST   INCREMENT DE DEFORMATION TOTALE
C               SIGD    CONTRAINTE A T
C               VIND    VARIABLES INTERNES A T    + INDICATEUR ETAT T
C    ATTENTION  VIND    VARIABLES INTERNES A T MODIFIEES SI REDECOUPAGE
C       OUT     SIGF    CONTRAINTE A T+DT
C               VINF    VARIABLES INTERNES A T+DT + INDICATEUR ETAT T+DT
C               DSDE    MATRICE DE COMPORTEMENT TANGENT A T+DT OU T
C       ----------------------------------------------------------------
        INTEGER         IMATE,NDIM,NDT,NDI,NVI,KPG,KSP,NUMLC
C
        REAL*8          CRIT(*), ANGMAS(*)
        REAL*8          TIMED,     TIMEF,    TEMPD,   TEMPF  , TREF
        REAL*8          TAMPON(*)
        REAL*8          EPSDT(*),  DEPST(*)
        REAL*8          SIGD(*),   SIGF(*)
        REAL*8          VIND(*),   VINF(*)
        REAL*8          DSDE(6,*)
C
        CHARACTER*16    COMPOR(*),     OPT
        CHARACTER*8     TYPMOD(*)
        CHARACTER*(*)   FAMI
        LOGICAL         CP
C
C       ----------------------------------------------------------------
C       VARIABLES LOCALES POUR LE REDECOUPAGE DU PAS DE TEMPS
C               TD      INSTANT T
C               TF      INSTANT T+DT
C               TEMD    TEMPERATURE A T
C               TEMF    TEMPERATURE A T+DT
C               EPS     DEFORMATION TOTALE A T
C               DEPS    INCREMENT DE DEFORMATION TOTALE
C               SD      CONTRAINTE A T
C               VD      VARIABLES INTERNES A T    + INDICATEUR ETAT T
C               DSDELO MATRICE DE COMPORTEMENT TANGENT A T+DT OU T
C               NPAL            NOMBRE DE PALIER POUR LE REDECOUPAGE
C               ICOMP           COMPORTEUR POUR LE REDECOUPAGE DU PAS DE
C                                    TEMPS
C               RETURN1 EN CAS DE NON CONVERGENCE LOCALE
C       ----------------------------------------------------------------
C
        INTEGER         ICOMP,        NPAL,      IPAL
        INTEGER         IRTET,     K
        INTEGER         RETCOM
        REAL*8          EPS(6),       DEPS(6),   SD(6)
        REAL*8          DSDELO(6,6)
        REAL*8          DELTAT,TD,TF
C       ----------------------------------------------------------------
C       COMMONS POUR VARIABLES DE COMMANDE : CAII17 ET CARR01
        INTEGER NFPGMX
        PARAMETER (NFPGMX=10)
        INTEGER NFPG,JFPGL,DECALA(NFPGMX),KM,KP,KR,IREDEC
        COMMON /CAII17/NFPG,JFPGL,DECALA,KM,KP,KR,IREDEC
        REAL*8 TIMED1,TIMEF1,TD1,TF1
        COMMON /CARR01/TIMED1,TIMEF1,TD1,TF1
C       ----------------------------------------------------------------
C       ----------------------------------------------------------------
        COMMON /TDIM/   NDT  , NDI
C       ----------------------------------------------------------------
C       ----------------------------------------------------------------
C       -- POUR LES VARIABLES DE COMMANDE :
        IREDEC=1
        TIMED1=TIMED
        TIMEF1=TIMEF
        TD1=TIMED
        TF1=TIMEF


        IPAL  =  INT(CRIT(5))
        RETCOM=0
        IRTET=0
C       CORRECTION JMP : POURQUOI REDECOUPER POUR RIGI_MECA_TANG ?
        IF (OPT.EQ.'RIGI_MECA_TANG') IPAL=0

        READ (COMPOR(2),'(I16)') NVI
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
        CALL LC0000 ( FAMI,KPG,KSP, NDIM,TYPMOD, IMATE,COMPOR,CRIT,
     &                TIMED, TIMEF, EPSDT,DEPST, SIGD, VIND,
     &                OPT, TAMPON, ANGMAS, CP,NUMLC,TEMPD,TEMPF,TREF,
     &                SIGF, VINF, DSDE, ICOMP, NVI, IRTET)

        IF ( IRTET.GT.0 ) GOTO (1,2), IRTET
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
           GOTO 2
        ELSE
           IF ((TYPMOD(2).EQ.'GRADEPSI').OR.
     &         (TYPMOD(2).EQ.'GRADVARI')) THEN
              CALL U2MESK('A','COMPOR2_10',1,TYPMOD(2))
           ENDIF
        ENDIF
C
        IF ( ICOMP .GT. 3 ) THEN
           CALL U2MESS('A','ALGORITH10_35')
           GOTO 2
        ENDIF
C
        IF ( ICOMP .GE. 1 ) NPAL = 2 * NPAL
        ICOMP = ICOMP + 1
C
        DO 124 K=1,NPAL
C --       INITIALISATION DES VARIABLES POUR LE REDECOUPAGE DU PAS
           IF ( K .EQ. 1 ) THEN
                TD = TIMED
                TD1 = TD
                DELTAT = (TIMEF - TIMED) / NPAL
                TF = TD + DELTAT
                TF1=TF
                IF ( OPT .EQ. 'RIGI_MECA_TANG'
     &                  .OR. OPT .EQ. 'FULL_MECA' )
     &                  CALL LCINMA ( 0.D0 , DSDE )
                CALL LCEQVE ( EPSDT     , EPS            )
                CALL LCEQVE ( DEPST     , DEPS           )
                CALL LCPRSV ( 1.D0/NPAL , DEPS    , DEPS )
                CALL LCEQVN ( NDT       , SIGD    , SD   )
C
C --        REACTUALISATION DES VARIABLES POUR L INCREMENT SUIVANT
            ELSE IF ( K .GT. 1 ) THEN
                TD = TF
                TD1=TD
                TF = TF + DELTAT
                TF1=TF
               CALL LCSOVE ( EPS     , DEPS    , EPS  )
                IF ( OPT .NE. 'RIGI_MECA_TANG' ) THEN
                    CALL LCEQVN ( NDT     , SIGF    , SD   )
                    CALL LCEQVN ( NVI     , VINF    , VIND   )
                ENDIF
            ENDIF
C
C
            CALL LC0000 ( FAMI,KPG,KSP,NDIM,TYPMOD,IMATE,COMPOR,CRIT,
     &                TD, TF, EPS,DEPS, SD, VIND,
     &                OPT, TAMPON, ANGMAS, CP,NUMLC,TEMPD,TEMPF,TREF,
     &                SIGF, VINF, DSDELO, ICOMP, NVI, IRTET)

            IF ( IRTET.GT.0 ) GOTO (1,2), IRTET
C
            IF ( OPT .EQ. 'RIGI_MECA_TANG'
     &               .OR. OPT .EQ. 'FULL_MECA' ) THEN
                CALL LCPRSM ( 1.D0/NPAL , DSDELO , DSDELO   )
                CALL LCSOMA ( DSDE    , DSDELO , DSDE      )
            ENDIF
C
 124    CONTINUE
        GOTO 9999
C
   2    CONTINUE
        RETCOM = 1
        GO TO 9999
C
 9999   CONTINUE
        END
