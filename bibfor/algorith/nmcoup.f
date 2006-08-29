        SUBROUTINE NMCOUP(FAMI,KPG,KSP,NDIM,TYPMOD,IMAT,COMP,LCPDB,CRIT,
     1                      TIMED,TIMEF, TEMPD,TEMPF,TREF,EPSDT,DEPST,
     2                      SIGD,VIND,OPT,ELGEOM,SIGF,VINF,DSDE,IRET)
        IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 28/08/2006   AUTEUR CIBHHPD L.SALMONA 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE JMBHH01 J.M.PROIX
C TOLE CRP_21
C       ----------------------------------------------------------------
        INTEGER         IMAT , NDIM,KPG,KSP,IRET
C
        REAL*8          CRIT(*)
        REAL*8          TIMED,     TIMEF,    TEMPD,   TEMPF  , TREF
        REAL*8          ELGEOM(*)
        REAL*8          EPSDT(6),  DEPST(6)
        REAL*8          SIGD(6),   SIGF(6)
        REAL*8          VIND(*),   VINF(*)
C
        REAL*8          DSDE(6,6)
        LOGICAL         LCPDB
C
        CHARACTER*16    COMP(*),     OPT
        CHARACTER*(*) FAMI
        CHARACTER*8     TYPMOD(*)
C       ----------------------------------------------------------------
C
C       AIGUILLAGE DES LOIS DE COMPORTEMENT COUPLES
C
C       ================================================================
C       ARGUMENTS
C
C       IN      KPG,KSP  NUMERO DU (SOUS)POINT DE GAUSS
C               NDIM    DIMENSION DE L ESPACE (3D=3,2D=2,1D=1)
C               TYPMOD  TYPE DE MODELISATION
C               IMAT    ADRESSE DU MATERIAU CODE
C               COMP    COMPORTEMENT DE L ELEMENT
C                       COMP(1) = RELATION DE COMPORTEMENT 
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
C               EPSDT   DEFORMATION TOTALE A T
C               DEPST   INCREMENT DE DEFORMATION TOTALE
C               SIGD    CONTRAINTE A T
C               VIND    VARIABLES INTERNES A T    + INDICATEUR ETAT T
C       OUT     SIGF    CONTRAINTE A T+DT
C               VINF    VARIABLES INTERNES A T+DT + INDICATEUR ETAT T+DT
C               DSDE    MATRICE DE COMPORTEMENT TANGENT A T+DT OU T
C               IRET    CODE RETOUR DE L'INTEGRATION INTEGRATION DU
C                       COUPLAGE FLUAGE/FISSURATION
C                              IRET=0 => PAS DE PROBLEME
C                              IRET=1 => ABSENCE DE CONVERGENCE
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

      CHARACTER*16 OPTION(2),CMP1,CMP2,CMP3

      CMP1(1:16) = COMP(8)
      CMP2(1:16) = COMP(9)
      CMP3(1:16) = COMP(10)
      OPTION(1)(1:16) = OPT
      IF (CMP3(1:8).NE.'        ') THEN
          CALL UTMESS('F','NMCOUP_1',
     &        'COUPLAGE FLUAGE/FISSURATION : IL FAUT DEFINIR DEUX '
     &        // 'LOIS DE COMPORTEMENT EXACTEMENT. ')
      ENDIF


      IF (CMP1(1:10).EQ.'GRANGER_FP') THEN

         IF (CMP2(1:5) .EQ. 'ELAS '            .OR.
     &       CMP2(1:9) .EQ. 'VMIS_ISOT'        .OR.
     &       CMP2(1:14).EQ. 'VMIS_ISOT_LINE'   .OR.
     &       CMP2(1:8) .EQ. 'ROUSS_PR'         .OR.
     &       CMP2(1:5) .EQ. 'LMARC'            .OR.
     &       CMP2(1:15).EQ. 'BETON_DOUBLE_DP'  .OR.
     &       CMP2(1:7) .EQ. 'NADAI_B'              ) THEN

           CALL NMCPLA (FAMI,KPG,KSP,NDIM,TYPMOD,IMAT,COMP,CRIT,
     1                      TIMED,TIMEF, TEMPD,TEMPF,TREF,
     2                      EPSDT,DEPST,SIGD,VIND,OPT,ELGEOM,
     3                      SIGF,VINF,DSDE,IRET)
           IF(IRET.EQ.1) GOTO 9999
         ELSE IF (CMP2(1:10) .EQ. 'ENDO_ISOT_BETON' .OR.
     &            CMP2(1:6)  .EQ. 'MAZARS') THEN
           OPTION(2)(1:16) = CMP2(1:16)
C          CALL NMGRAN ( NDIM, TYPMOD, IMAT, COMP, CRIT,
C     1                  TIMED,TIMEF, TEMPD,TEMPF,TREF,HYDRD,
C     &                  HYDRF,SECHD,SECHF,SREF,TEMPD,TEMPF,EPSDT,
C     2                  DEPST,SIGD, VIND, OPTION,ELGEOM,SIGF,VINF,DSDE)
           CALL UTMESS('F','NMCOUP_2','GRANGER ET ENDO_ISOT_BETON'
     &      // 'OU MAZARS NON ENCORE DEVELOPPE')

         ELSE
           CALL UTMESS('F','NMCOUP_3','LOI DE COMPORTEMENT NON '
     &      // 'AUTORISEE DANS LE COUPLAGE FLUAGE/FISSURATION')
         ENDIF

      ELSE IF (CMP1(1:13).EQ.'BETON_UMLV_FP') THEN


        IF (CMP2(1:15) .EQ. 'ENDO_ISOT_BETON' .OR.
     &      CMP2(1:6)  .EQ. 'MAZARS') THEN

          IF (CMP2(1:6).EQ.'MAZARS') THEN
            CALL UTMESS('F','NMCOUP_2','UMLV_FP ET '
     &      // 'MAZARS NON ENCORE DEVELOPPE')
          ELSE
            OPTION(2)(1:16) = CMP2(1:16)
            IF ((TYPMOD(1).EQ.'C_PLAN').AND.(.NOT.LCPDB)) THEN
              CALL UTMESS('F','NMCOUP','PAS DE C_PLAN POUR EIB '//
     &                     'UTILISER C_PLAN_DEBORST')
            ENDIF
            CALL LCUMFP ( FAMI,KPG,KSP,NDIM, TYPMOD, IMAT, COMP,
     1                      TIMED,TIMEF,TEMPD,TEMPF,TREF,
     2                      EPSDT,DEPST,SIGD, VIND, OPTION,SIGF,
     3                      VINF,DSDE)
          ENDIF
        ELSE
          CALL UTMESS('F','NMCOUP_5','LOI DE COMPORTEMENT NON '
     &     // 'AUTORISEE DANS LE COUPLAGE FLUAGE/FISSURATION')
        ENDIF

      ELSE
        CALL UTMESS('F','NMCOUP_6','LOI DE FLUAGE NON '
     &   // 'AUTORISEE DANS LE COUPLAGE FLUAGE/FISSURATION')
      ENDIF

9999  CONTINUE
      END
