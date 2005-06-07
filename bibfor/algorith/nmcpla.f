        SUBROUTINE NMCPLA ( NDIM, TYPMOD, IMAT, COMP, CRIT,
     1                      TIMED,TIMEF, TEMPD,TEMPF,TREF,HYDRD,
     &                      HYDRF,SECHD,SECHF,SREF,EPSDT,DEPST,SIGD,
     2                      VIND, OPT,ELGEOM,SIGF,VINF,DSDE)
        IMPLICIT NONE
C       ================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 06/08/2004   AUTEUR JMBHH01 J.M.PROIX 
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
C       INTEGRATION DU COUPLAGE FLUAGE/FISSURATION, C'EST A DIRE LE
C       COUPLAGE D'UNE LOI DE COMPORTEMENT DE TYPE FLUAGE GRANGER
C       ET D'UNE LOI DE  COMPORTEMENT ELASTO PLASTIQUE
C               AVEC    . N VARIABLES INTERNES
C                       . UNE FONCTION SEUIL ELASTIQUE
C
C       INTEGRATION DES CONTRAINTES           = SIG(T+DT)
C       INTEGRATION DES VARIABLES INTERNES    = VIN(T+DT) (CUMUL DES
C              VARIABLES INTERNES DES DEUX LOIS)
C       ET CALCUL DU JACOBIEN ASSOCIE         = DS/DE(T+DT) OU DS/DE(T)
C
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
C       OUT     SIGF    CONTRAINTE A T+DT
C               VINF    VARIABLES INTERNES A T+DT + INDICATEUR ETAT T+DT
C               DSDE    MATRICE DE COMPORTEMENT TANGENT A T+DT OU T
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER  ZI
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
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C       ----------------------------------------------------------------
        INTEGER         IMAT , NDIM
C
        REAL*8          CRIT(*)
        REAL*8          TIMED,     TIMEF,    TEMPD,   TEMPF  , TREF
        REAL*8          HYDRD, HYDRF, SECHD, SECHF, SREF, ELGEOM(*)
        REAL*8          EPSDT(6),  DEPST(6)
        REAL*8          SIGD(6),   SIGF(6)
        REAL*8          VIND(*),   VINF(*)
C
        REAL*8          DSDE(6,6)
C
        CHARACTER*16    COMP(*),     OPT
        CHARACTER*8     TYPMOD(*)
C       ----------------------------------------------------------------
C       VARIABLES LOCALES
        INTEGER         NDT    , NDI   , NVI1, IBID, IBID2, IBID3
        INTEGER         NDT2   , NDI2  , NVI2, NN, I, NMAT
        CHARACTER*2     BL2, FB2, CERR(5)
        CHARACTER*8     MOD    ,MOD3D  , MODCP,   NOMC(5), NOMPAR(3)
        CHARACTER*16    OPTFLU, CMP1(3), CMP2(3), CMP3(3), CVERI
        REAL*8          RBID, NU, ANGMAS(3)
        REAL*8          EPSFL(6), EPSFLD(6), EPSFLF(6), DEPSFL(6)
        REAL*8          DEPS(6), KOOH(6,6), VALPAD(3), VALPAF(3)
        REAL*8          MATERD(5) , MATERF(5), DEPST2(6), DEPSEL(6)
        REAL*8          EPSICV, TOLER, NDSIG, NSIGF, HOOK(6,6)
        REAL*8          NDDEPS, NDEPST, DDEPST(6), DSIGF(6)
        REAL*8          EPSELD(6), EPSELF(6)
C
        INTEGER         IRTET,     K
        INTEGER         ITER ,        ITEMAX
        REAL*8          EPS(6),       SIGF2(6)
        REAL*8          TD,           TF,        DELTAT
        REAL*8          TEMD,         TEMF,      DETEMP
        REAL*8          HYDD,         HYDF,      DEHYDR
        REAL*8          SECD,         SECF,      DESECH
        REAL*8          TMPDMX,       TMPFMX,    EPSTH
        REAL*8          ALPHAD, ALPHAF, BENDOD, BENDOF, KDESSD, KDESSF
C
        INTEGER         IADZI, IAZK24
        CHARACTER*4     CITER
        CHARACTER*8     NOMAIL
        CHARACTER*10    DCV
C       ----------------------------------------------------------------
        COMMON /TDIM/   NDT  , NDI
C       ----------------------------------------------------------------
C
C --- DECODAGE DES COMPORTEMENTS ET VERIFICATIONS
C
      MOD3D   = '3D'
      MODCP   = 'C_PLAN'
      CMP1(1) = COMP(8)
      CMP2(1) = COMP(9)
      CMP3(1) = COMP(10)
      MOD     = TYPMOD(1)
C
      IF (CMP3(1)(1:8).NE.'        ') THEN
          CALL UTMESS('F','NMCPLA_1',
     &        'COUPLAGE FLUAGE/FISSURATION : IL FAUT DEFINIR DEUX '
     &        // 'LOIS DE COMPORTEMENT EXACTEMENT. ')
      ENDIF
C
      IF (CMP1(1)(1:10) .NE. 'GRANGER_FP' ) THEN
         CALL UTMESS('F','NMCPLA_2',
     &       'COUPLAGE FLUAGE/FISSURATION : LA PREMIERE LOI DOIT '
     &       // 'ETRE UNE LOI DE FLUAGE DE TYPE GRANGER_FP OU '
     &       // 'GRANGER_FP_V.')
      ENDIF
C
C     TABLEAU DES VARIABLES INTERNES DIMENSIONNE AUX MAX I.E. 3D
      CALL GRANVI ( MOD3D , IBID , IBID2 , NVI1 )
      WRITE (CMP1(2),'(I16)') NVI1
      CMP1(3) = COMP(3)
C
C     DIMENSION DES TENSEURS
      CALL GRANVI ( MOD , NDT , NDI , IBID )
C
      NN = NVI1 + 1
      IF (CMP2(1)(1:5) .EQ. 'ELAS '            .OR.
     &    CMP2(1)(1:9) .EQ. 'VMIS_ISOT'        .OR.
     &    CMP2(1)(1:14).EQ. 'VMIS_ISOT_LINE' ) THEN
          IF ( TYPMOD(2)(1:7) .EQ. 'MEGRDKT') THEN
              CALL UTMESS('F','NMCPLA_3',
     &        ' COMPORTEMENT INATTENDU : '// CMP2(1))
          ELSE
            IF (CMP2(1)(1:5) .EQ. 'ELAS ')          NVI2 = 1
            IF (CMP2(1)(1:9) .EQ. 'VMIS_ISOT')      NVI2 = 2
            IF (CMP2(1)(1:14).EQ. 'VMIS_ISOT_LINE') NVI2 = 2
         ENDIF
C
      ELSEIF (CMP2(1)(1:8).EQ. 'ROUSS_PR' .OR.
     &        CMP2(1)(1:8) .EQ. 'CHABOCHE'   .OR.
     &        CMP2(1)(1:4) .EQ. 'OHNO'       .OR.
     &        CMP2(1)(1:5) .EQ. 'LMARC'      .OR.
     &        CMP2(1)(1:15).EQ. 'BETON_DOUBLE_DP'.OR.
     &        CMP2(1)(1:7) .EQ. 'NADAI_B'        ) THEN
C
              IF (CMP2(1)(1:8).EQ. 'ROUSS_PR')
     &            CALL RSLNVI ( MOD3D , IBID , IBID2 , IBID3 , NVI2 )
              IF (CMP2(1)(1:8) .EQ. 'CHABOCHE' )
     &            CALL CHBNVI ( MOD3D , IBID , IBID2 , IBID3 , NVI2 )
              IF (CMP2(1)(1:4) .EQ. 'OHNO')
     &            CALL ONONVI ( MOD3D , IBID , IBID2 , IBID3 , NVI2 )
              IF (CMP2(1)(1:5) .EQ. 'LMARC')
     &            CALL LMANVI ( MOD3D , IBID , IBID2 , IBID3 , NVI2 )
              IF (CMP2(1)(1:7) .EQ. 'NADAI_B')
     &            CALL INSNVI ( MODCP , IBID , IBID2 , IBID3 , NVI2 )
              IF (CMP2(1)(1:15).EQ. 'BETON_DOUBLE_DP')
     &            CALL BETNVI ( MOD3D , IBID , IBID2 , IBID3 , NVI2 )
C
      ELSE
         CALL UTMESS('F','NMCPLA_4','LOI DE COMPORTEMENT NON '
     &      // 'AUTORISEE DANS LE COUPLAGE FLUAGE/FISSURATION')
      ENDIF
C
      WRITE (CMP2(2),'(I16)') NVI2
      CMP2(3) = COMP(3)
      WRITE (CVERI,'(I16)') (NVI1 + NVI2)
C
      IF (CVERI(1:16).NE.COMP(2)(1:16)) THEN
         CALL UTMESS('F','NMCPLA_5',
     &        'COUPLAGE FLUAGE/FISSURATION : NOMBRE TOTAL DE '
     &     // 'VARIABLES INTERNES INCOHERENT <--> ERREUR DE '
     &     // 'PROGRAMMATION. ')
              ENDIF
C
C --- TEMPERATURE MAXIMALE AU COURS DE L'HISTORIQUE DE CHARGEMENT
C
      TMPDMX = TEMPD
      TMPFMX = TEMPF
      IF (CMP2(1)(1:15).EQ. 'BETON_DOUBLE_DP' ) THEN
         IF (TMPDMX.LT.VIND(NVI1+3)) TMPDMX = VIND(NVI1+3)
         IF (TMPFMX.LT.VIND(NVI1+3)) TMPFMX = VIND(NVI1+3)
      ENDIF
C
C --- CRITERE DE CONVERGENCE
C
      ITEMAX = INT(CRIT(1))
      TOLER  =     CRIT(3)
C
C --- OPTION DE CALCUL POUR LA LOI DE FLUAGE
C
      IF ( OPT .EQ. 'RAPH_MECA' .OR. OPT .EQ. 'FULL_MECA' ) THEN
         OPTFLU = 'RAPH_MECA'
      ELSE
         OPTFLU = '                '
      ENDIF
C
C     --------------------------------
C --- DEBUT DES ITERATIONS DU COUPLAGE
C     --------------------------------
      ITER = 1
      DO 10 K=1,6
         DEPST2(K) = DEPST(K)
 10   CONTINUE
C
 20   CONTINUE
C
C --- RESOLUTION LOI DE FLUAGE
C
      IF ( OPTFLU .EQ. 'RAPH_MECA' ) THEN
         CALL NMGRAN (NDIM,   TYPMOD,   IMAT,    CMP1,   CRIT,
     1                TIMED,  TIMEF,    TEMPD,   TEMPF,  TREF,
     2                HYDRD,  HYDRF,    SECHD,   SECHF,  SREF, TMPDMX,
     3                TMPFMX, DEPST2,   SIGD,    VIND(1),OPT,
     4                SIGF2,  VINF(1),  DSDE )
C
C ---    CALCUL DE L'INCREMENT DE LA DEFORMATION DE FLUAGE
C
         NOMC(1)   = 'E       '
         NOMC(2)   = 'NU      '
         NOMC(3)   = 'ALPHA   '
         NOMC(4)   = 'B_ENDOGE'
         NOMC(5)   = 'K_DESSIC'
         NOMPAR(1) = 'TEMP'
         NOMPAR(2) = 'HYDR'
         NOMPAR(3) = 'SECH'
         VALPAD(1) = TMPDMX
         VALPAD(2) = HYDRD
         VALPAD(3) = SECHD
         VALPAF(1) = TMPFMX
         VALPAF(2) = HYDRF
         VALPAF(3) = SECHF
C
C -      RECUPERATION MATERIAU A TEMPD (T)
C
         FB2 = 'F '
         CALL RCVALA(IMAT,' ',   'ELAS',     3,       NOMPAR,VALPAD, 1,
     1                NOMC(2), MATERD(2), CERR(1), FB2 )
C
C -      RECUPERATION MATERIAU A TEMPF (T+DT)
C
         CALL RCVALA(IMAT,' ',   'ELAS',     3,       NOMPAR,VALPAF, 1,
     1                NOMC(2), MATERF(2), CERR(1), FB2 )
C
         MATERD(1) = 1.D0
         MATERF(1) = 1.D0
C
         DO 40 K=1,NDT
            EPSFL(K) = VIND(8*NDT+K)
            DO 30 I=1,8
               EPSFL(K) = EPSFL(K) - VIND((I-1) * NDT+K)
  30        CONTINUE
  40     CONTINUE
C
         CALL LCOPIL ( 'ISOTROPE' , TYPMOD , MATERD , KOOH )
         CALL LCPRMV ( KOOH, EPSFL  , EPSFLD )
C
         DO 60 K=1,NDT
            EPSFL(K) = VINF(8*NDT+K)
            DO 50 I=1,8
               EPSFL(K) = EPSFL(K) - VINF((I-1) * NDT+K)
  50        CONTINUE
  60     CONTINUE
C
         CALL LCOPIL ( 'ISOTROPE' , TYPMOD , MATERF , KOOH )
         CALL LCPRMV ( KOOH, EPSFL  , EPSFLF )
C
         DO 70 K=1,NDT
            DEPSFL(K) = EPSFLF(K) - EPSFLD(K)
  70     CONTINUE
      ENDIF
C
C --- RETRAIT DE LA DEFORMATION DE FLUAGE A LA DEFORMATION TOTALE
C
      IF ( OPTFLU .EQ. 'RAPH_MECA' ) THEN
         DO 80 K=1,NDT
            DEPS(K) = DEPST(K) - DEPSFL(K)
  80     CONTINUE
      ELSE
         DO 90 K=1,NDT
            DEPS(K) = DEPST(K)
  90     CONTINUE
      ENDIF
C
C
C --- RESOLUTION LOI DE PLASTICITE FISSURATION
C
      IF (CMP2(1)(1:5) .EQ. 'ELAS '            .OR.
     &    CMP2(1)(1:9) .EQ. 'VMIS_ISOT'        .OR.
     &    CMP2(1)(1:14).EQ. 'VMIS_ISOT_LINE' ) THEN
C
          CALL NMISOT (NDIM,  TYPMOD,    IMAT,     CMP2,  CRIT,
     1                 TIMED, TIMEF,     TEMPD,    TEMPF, TREF,
     2                 HYDRD, HYDRF,     SECHD,    SECHF, SREF,
     3                 DEPS , SIGD,      VIND(NN), OPT,
     4                 SIGF,  VINF(NN),  DSDE,     RBID,  RBID)
C
      ELSEIF (CMP2(1)(1:8).EQ. 'ROUSS_PR' .OR.
     &        CMP2(1)(1:8) .EQ. 'CHABOCHE'   .OR.
     &        CMP2(1)(1:4) .EQ. 'OHNO'       .OR.
     &        CMP2(1)(1:5) .EQ. 'LMARC'      .OR.
     &        CMP2(1)(1:15).EQ. 'BETON_DOUBLE_DP'.OR.
     &        CMP2(1)(1:7) .EQ. 'NADAI_B'        ) THEN
C
              CALL REDECE (NDIM,  TYPMOD,   IMAT,     CMP2,  CRIT,
     1                     TIMED, TIMEF,    TEMPD,    TEMPF, TREF,
     2                     HYDRD, HYDRF,    SECHD,    SECHF, SREF,
     3                     EPSDT, DEPS , SIGD,     VIND(NN), OPT,   
     4                     ELGEOM, ANGMAS, SIGF,  VINF(NN), DSDE)
      ELSE
         CALL UTMESS('F','NMCPLA_6','LOI DE COMPORTEMENT NON '
     &      // 'AUTORISEE DANS LE COUPLAGE FLUAGE/FISSURATION')
      ENDIF
C
      IF (OPTFLU.EQ.'RAPH_MECA') THEN
C
C -      RECUPERATION MATERIAU A TEMPD (T)
C
         FB2 = 'F '
         CALL RCVALA(IMAT,' ',   'ELAS',     3,       NOMPAR,VALPAD, 5,
     1                NOMC(1), MATERD(1), CERR(1), FB2 )
C
         IF ( CERR(3) .NE. 'OK' ) MATERD(3) = 0.D0
         IF ( CERR(4) .NE. 'OK' ) MATERD(4) = 0.D0
         IF ( CERR(5) .NE. 'OK' ) MATERD(5) = 0.D0
C
C -      RECUPERATION MATERIAU A TEMPF (T+DT)
C
         CALL RCVALA(IMAT,' ',   'ELAS',     3,       NOMPAR,VALPAF, 5,
     1                NOMC(1), MATERF(1), CERR(1), FB2 )
C
         IF ( CERR(3) .NE. 'OK' ) MATERF(3) = 0.D0
         IF ( CERR(4) .NE. 'OK' ) MATERF(4) = 0.D0
         IF ( CERR(5) .NE. 'OK' ) MATERF(5) = 0.D0
C
C --- CALCUL DE L'INCREMENT DE DEFORMATION ELASTIQUE
C --- + RETRAIT ENDOGENNE + RETRAIT DESSICCATION + RETRAIT THERMIQUE
C
         CALL LCOPIL ( 'ISOTROPE' , TYPMOD , MATERD , KOOH )
         CALL LCPRMV ( KOOH, SIGD  , EPSELD )
         CALL LCOPIL ( 'ISOTROPE' , TYPMOD , MATERF , KOOH )
         CALL LCPRMV ( KOOH, SIGF  , EPSELF )
C
         DO 100 K=1,NDT
             DEPSEL(K) = EPSELF(K) - EPSELD(K)
 100     CONTINUE
C
         ALPHAD = MATERD(3)
         ALPHAF = MATERF(3)
         BENDOD = MATERD(4)
         BENDOF = MATERF(4)
         KDESSD = MATERD(5)
         KDESSF = MATERF(5)
         EPSTH = ALPHAF*(TEMPF-TREF)  - ALPHAD*(TEMPD-TREF)
     &         - BENDOF*HYDRF        + BENDOD*HYDRD
     &         - KDESSF*(SREF-SECHF) + KDESSD*(SREF-SECHD)
         DO 110 K=1,3
            DEPSEL(K) = DEPSEL(K) + EPSTH
 110     CONTINUE
         IF(MOD(1:6).EQ.'C_PLAN')THEN
            NU = MATERF(2)
            DEPSEL(3)=-NU / (1.D0-NU) * (DEPSEL(1)+DEPSEL(2))
     &            +(1.D0+NU) / (1.D0-NU) * EPSTH
         ENDIF
C
C
C ---    CALCUL DE L'INCREMENT DE DEFORMATION EN ENTREE DU CALCUL
C ---    DE FLUAGE POR L'ITERATION SUIVANTE
C
         DO 120 K=1,NDT
            DEPST2(K) = DEPST2(K) + DEPSEL(K) + DEPSFL(K) - DEPST2(K)
 120     CONTINUE
C
C ---    CRITERE DE CONVERGENCE - NORME DE SIGF2 - SIGF
C
         DO 150 K=1,NDT
            DSIGF(K) = SIGF2(K) - SIGF(K)
 150     CONTINUE
C
         NDSIG = 0.D0
         NSIGF = 0.D0
         DO 160 K=1,NDT
            NDSIG = NDSIG + DSIGF(K) * DSIGF(K)
            NSIGF = NSIGF  + SIGF(K) * SIGF(K)
 160     CONTINUE
C
         IF (NSIGF.GT.TOLER*TOLER) THEN
            EPSICV = (NDSIG/NSIGF) ** 0.5D0
         ELSE
            EPSICV = NDSIG ** 0.5D0
         ENDIF
C
         IF (EPSICV.GT.TOLER) THEN
            IF (ITER.LT.ITEMAX) THEN
               ITER = ITER + 1
               GOTO 20
            ELSE
               CALL TECAEL ( IADZI, IAZK24 )
               NOMAIL = ZK24(IAZK24-1+3)(1:8)
               CALL CODREE(ABS(EPSICV),'E',DCV)
               CALL CODENT(ITER,'G',CITER)
               CALL UTMESS('F','NMCPLA_7',
     &           'COUPLAGE FLUAGE/FISSURATION : NOMBRE D ITERATIONS '
     &           //' MAX (' // CITER // ') DEPASSE - NON CONVERGENCE '
     &           // 'DU COUPLAGE A LA MAILLE '// NOMAIL
     &           // ' RESIDU RELATIF : ' // DCV )
            ENDIF
         ENDIF
      ENDIF
C     ------------------
C --- FIN DES ITERATIONS
C     ------------------
C
 9999 CONTINUE
      END
