      SUBROUTINE NMCOMP (NDIM,TYPMOD,IMATE,COMPOR,CRIT,
     &                   INSTAM,INSTAP,TM,TP,TREF,HYDRM,HYDRP,SECHM,
     &                   SECHP,SREF,EPSM,DEPS,SIGM,VIM,
     &                   OPTION,DEFAM,DEFAP,NZ,PHASM, PHASP,ELGEOM,
     &                   SIGP,VIP,DSIDEP,CODRET,CORRM,CORRP)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 17/05/2004   AUTEUR ROMEO R.FERNANDES 
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
C RESPONSABLE PBADEL P.BADEL
C TOLE CRP_20
C TOLE CRP_21
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER            NDIM,IMATE,NZ,CODRET
      CHARACTER*8        TYPMOD(*)
      CHARACTER*16       COMPOR(*), OPTION
      REAL*8             CRIT(*), INSTAM, INSTAP, TM, TP, TREF, SREF
      REAL*8             HYDRM, HYDRP, SECHM, SECHP ,PHASM(NZ),PHASP(NZ)
      REAL*8             EPSM(*), DEPS(*), DSIDEP(*)
      REAL*8             SIGM(*), VIM(*), SIGP(*), VIP(*)
      REAL*8             ELGEOM(*),CORRM,CORRP
      REAL*8             DEFAP(*), DEFAM(*)
C ----------------------------------------------------------------------
C     INTEGRATION DES LOIS DE COMPORTEMENT NON LINEAIRE POUR LES
C     ELEMENTS ISOPARAMETRIQUES EN PETITES DEFORMATIONS
C
C IN  NDIM    : DIMENSION DE L'ESPACE
C               3 : 3D , 2 : D_PLAN ,AXIS OU  C_PLAN
C     TYPMOD  : TYPE DE MODELISATION
C     IMATE   : ADRESSE DU MATERIAU CODE
C     COMPOR  : COMPORTEMENT :  (1) = TYPE DE RELATION COMPORTEMENT
C                               (2) = NB VARIABLES INTERNES / PG
C                               (3) = HYPOTHESE SUR LES DEFORMATIONS
C     CRIT    : CRITERES DE CONVERGENCE LOCAUX
C                               (1) = NB ITERATIONS MAXI A CONVERGENCE
C                                     (ITER_INTE_MAXI == ITECREL)
C                               (2) = TYPE DE JACOBIEN A T+DT
C                                     (TYPE_MATR_COMP == MACOMP)
C                                     0 = EN VITESSE     >SYMETRIQUE
C                                     1 = EN INCREMENTAL >NON-SYMETRIQUE
C                               (3) = VALEUR TOLERANCE DE CONVERGENCE
C                                     (RESI_INTE_RELA == RESCREL)
C                               (5) = NOMBRE D'INCREMENTS POUR LE
C                                     REDECOUPAGE LOCAL DU PAS DE TEMPS
C                                     (ITER_INTE_PAS  == ITEDEC)
C                                     -1,0,1 = PAS DE REDECOUPAGE
C                                     N = NOMBRE DE PALIERS
C                               (6) = TYPE D INTEGRATION LOCAL POUR
C                                     LA LOI DE COMPORTEMENT
C                                     (RESO_INTE == INTLOC)
C                                     0 = IMPLICITE
C                                     1 = RUNGE_KUTTA
C     INSTAM  : INSTANT DU CALCUL PRECEDENT
C     INSTAP  : INSTANT DU CALCUL
C     TREF    : TEMPERATURE DE REFERENCE POUR LES CONTRAINTES THERMIQUES
C     HYDRM   : HYDRATATION A L'INSTANT PRECEDENT
C     HYDRP   : HYDRATATION A L'INSTANT DU CALCUL
C     SECHM   : SECHAGE A L'INSTANT PRECEDENT
C     SECHP   : SECHAGE A L'INSTANT DU CALCUL
C     SREF    : SECHAGE DE REFERENCE
C     EPSM    : DEFORMATIONS A L'INSTANT DU CALCUL PRECEDENT
C     DEPS    : INCREMENT DE DEFORMATION TOTALE :
C                DEPS(T) = DEPS(MECANIQUE(T)) + DEPS(DILATATION(T))
C     SIGM    : CONTRAINTES A L'INSTANT DU CALCUL PRECEDENT
C     VIM     : VARIABLES INTERNES A L'INSTANT DU CALCUL PRECEDENT
C     OPTION  : OPTION DEMANDEE : RIGI_MECA_TANG , FULL_MECA , RAPH_MECA
C     DEFAM   : DEFORMATIONS ANELASTIQUES A L'INSTANT PRECEDENT
C     DEFAP   : DEFORMATIONS ANELASTIQUES A L'INSTANT DU CALCUL
C VAR VIP     : VARIABLES INTERNES
C                IN  : ESTIMATION (ITERATION PRECEDENTE OU LAG. AUGM.)
C                OUT : EN T+
C OUT SIGP    : CONTRAINTES A L'INSTANT ACTUEL
C     DSIDEP  : MATRICE CARREE
C
C PRECISIONS :
C  1/ SI DEFORMATION = PETIT OU PETIT_REAC
C
C      EPSM(6), DEPS(6)  SONT LES DEFORMATIONS
C
C      LES TENSEURS ET MATRICES SONT RANGES DANS L'ORDRE :
C         XX YY ZZ SQRT(2)*XY SQRT(2)*XZ SQRT(2)*YZ
C
C  2/ SI DEFORMATION = SIMO_MIEHE
C
C      EPSM(3,3) ET DEPS(3,3) MESURENT LE GRADIENT DE LA TRANSFORMATION
C      EN T- ET ENTRE T- ET T+
C
C      LES CONTRAINTES (DE CAUCHY) SONT RANGEES DANS L'ORDRE
C         XX YY ZZ XY XZ YZ
C
C      LA MATRICE TANGENTE EST DE LA FORME
C         DSIDEP(6, 3,3)  = DSIG(IJ) / DF(K,L)
C ----------------------------------------------------------------------
C
      REAL*8 R8BID
      CHARACTER*16 OPTIO2
      LOGICAL CP
      INTEGER CPL
      CODRET = 0
C      CONTRAINTES PLANES
      CALL NMCPL1(COMPOR,TYPMOD,OPTION,VIP,DEPS,OPTIO2,CPL,NVV)
      CP=(CPL.NE.0)
      
      
C ----------------------------------------------------------------------
C                        CAS DES LOIS GRAD_VARI
C ----------------------------------------------------------------------

      IF (TYPMOD(2) .EQ. 'GRADVARI') THEN
      
        IF (INT(CRIT(6)) .NE. 0)  CALL UTMESS('F','NMCOMP',
     &        'INTEGRATION EXPLICITE DU COMPORTEMENT NON PROGRAMMEE')

C -- ENDOMMAGEMENT FRAGILE (COURBE DE TRACTION BI-LINEAIRE)

        IF (COMPOR(1) .EQ. 'ENDO_FRAGILE') THEN
          CALL LCFRGV (NDIM  , TYPMOD, OPTION, IMATE , EPSM  , 
     &                 DEPS  , VIM   , R8BID  , R8BID , R8BID  ,
     &                 R8BID, R8BID , R8BID  , SIGP   , VIP   ,
     &                 DSIDEP)
            GOTO 9000
        END IF

C -- RUPTURE FRAGILE : LOI DE GRIFFITH

        IF (COMPOR(1) .EQ. 'RUPT_FRAG') THEN
          CALL LCRUPT (NDIM, TYPMOD, IMATE, TP, TREF, EPSM, DEPS,
     &                 VIM, OPTION, SIGP, VIP, DSIDEP)
          GOTO 9000
        END IF

C -- PLASTICITE ISOTROPE A GRADIENT

        IF ( COMPOR(1) .EQ. 'VMIS_ISOT_LINE'
     & .OR. COMPOR(1) .EQ. 'VMIS_ISOT_TRAC') THEN
          CALL LCPLGR (COMPOR, NDIM, OPTION, IMATE, CRIT,TREF, TM, TP, 
     &                 EPSM, DEPS,SIGM, VIM, 1.D0, R8BID, R8BID,
     &                 VIP, R8BID, R8BID, R8BID, SIGP)
          GOTO 9000
        END IF

C -- RUPTURE DUCTILE : LOI DE ROUSSELIER

        IF (COMPOR(1) .EQ. 'ROUSSELIER') THEN
          IF (COMPOR(3).NE.'SIMO_MIEHE') CALL UTMESS('F','NMCOMP',
     &        'ROUSSELIER A GRADIENT SANS SIMO MIEHE NON PROGRAMMEE')
          CALL LCRONL (NDIM,IMATE,OPTION,COMPOR,TM,TP,TREF,EPSM,DEPS,
     &                 VIM,VIP,SIGP,DSIDEP)
          GOTO 9000
        END IF
        
      END IF
          

C ----------------------------------------------------------------------
C                        CAS DES LOIS GRAD_EPSI 
C ----------------------------------------------------------------------

      IF (TYPMOD(2) .EQ. 'GRADEPSI') THEN
      
        IF (INT(CRIT(6)) .NE. 0)  CALL UTMESS('F','NMCOMP',
     &        'INTEGRATION EXPLICITE DU COMPORTEMENT NON PROGRAMMEE')
          
C -- ENDOMMAGEMENT FRAGILE (COURBE DE TRACTION BI-LINEAIRE)

        IF (COMPOR(1) .EQ. 'ENDO_FRAGILE') THEN
          CALL LCFRGE(NDIM, TYPMOD, IMATE, EPSM, DEPS,
     &                   VIM, OPTION, SIGP, VIP,  DSIDEP)
          GOTO 9000
        END IF

C -- RUPTURE BETON

        IF ( COMPOR(1) .EQ. 'ENDO_ISOT_BETON' ) THEN
          CALL LCDSBE(NDIM, TYPMOD, IMATE, COMPOR, EPSM, DEPS,
     &                   VIM, OPTION, SIGP, VIP,  DSIDEP)
          GOTO 9000
        ENDIF

C -- DRUCKER - PRAGER

        IF (COMPOR(1) .EQ. 'DRUCKER_PRAGER') THEN
          CALL LCDPNL(TYPMOD,NDIM,OPTION,IMATE,SIGM,EPSM,
     &                  TM,TP,TREF,DEPS,VIM,VIP,SIGP,DSIDEP,CODRET)
          GOTO 9000 
        END IF

C -- MAZARS

        IF (COMPOR(1) .EQ. 'MAZARS') THEN
          CALL LCMZGE(NDIM, TYPMOD, IMATE, EPSM, DEPS,
     &                   VIM,TM,TP,TREF,HYDRM,HYDRP,SECHM,SECHP,SREF,
     &                   OPTION, SIGP, VIP,  DSIDEP)
          GOTO 9000
        END IF
        

      END IF
          
          
      IF (COMPOR(3).EQ.'SIMO_MIEHE') THEN
        IF (INT(CRIT(6)) .NE. 0) CALL UTMESS('F','NMCOMP',
     &      'INTEGRATION EXPLICITE IMPOSSIBLE')
        IF ( COMPOR(1) .EQ. 'ELAS            ' .OR.
     &       COMPOR(1) .EQ. 'VMIS_ISOT_LINE  ' .OR.
     &       COMPOR(1) .EQ. 'VMIS_ISOT_TRAC  ' ) THEN
          CALL LCGDPI(NDIM,IMATE,COMPOR,TM,TP,TREF,
     &               EPSM,DEPS,SIGM,VIM,OPTION,SIGP,VIP,DSIDEP,CODRET)
        ELSE IF (COMPOR(1) .EQ. 'ROUSSELIER') THEN
          CALL LCROLO (NDIM,IMATE,OPTION,COMPOR,CRIT,TM,TP,TREF,
     &                 EPSM,DEPS,VIM,VIP,SIGP,DSIDEP,CODRET)
        ELSE IF (COMPOR(1).EQ. 'META_P_IL       '.OR.
     &           COMPOR(1).EQ. 'META_P_IL_PT    '.OR.
     &           COMPOR(1).EQ. 'META_P_IL_RE    '.OR.
     &           COMPOR(1).EQ. 'META_P_IL_PT_RE '.OR.
     &           COMPOR(1).EQ. 'META_V_IL       '.OR.
     &           COMPOR(1).EQ. 'META_V_IL_PT    '.OR.
     &           COMPOR(1).EQ. 'META_V_IL_RE    '.OR.
     &           COMPOR(1).EQ. 'META_V_IL_PT_RE '.OR.
     &           COMPOR(1).EQ. 'META_P_INL      '.OR.
     &           COMPOR(1).EQ. 'META_P_INL_PT   '.OR.
     &           COMPOR(1).EQ. 'META_P_INL_RE   '.OR.
     &           COMPOR(1).EQ. 'META_P_INL_PT_RE'.OR.
     &           COMPOR(1).EQ. 'META_V_INL      '.OR.
     &           COMPOR(1).EQ. 'META_V_INL_PT   '.OR.
     &           COMPOR(1).EQ. 'META_V_INL_RE   '.OR.
     &           COMPOR(1).EQ. 'META_V_INL_PT_RE'  ) THEN
          IF (COMPOR(8)(1:5) .EQ. 'ACIER') THEN
             IF (NZ .NE. 7) CALL UTMESS ('F','NMCOMP','NOMBRE DE
     &       PHASE INCORRECT')
             CALL LCGDPM(NDIM,IMATE,COMPOR,CRIT,
     &                   INSTAM,INSTAP,TM,TP,TREF,
     &                   EPSM,DEPS,SIGM,VIM,
     &                   PHASM,PHASP,OPTION,SIGP,VIP,DSIDEP,CODRET)
           ELSEIF (COMPOR(8)(1:4) .EQ. 'ZIRC') THEN
             IF (NZ .NE. 3) CALL UTMESS ('F','NMCOMP','NOMBRE DE
     &       PHASE INCORRECT')
             CALL NZGDZI(NDIM,IMATE,COMPOR,CRIT,
     &                   INSTAM,INSTAP,TM,TP,TREF,
     &                   EPSM,DEPS,SIGM,VIM,
     &                   PHASM,PHASP,OPTION,SIGP,VIP,DSIDEP,CODRET)
           ELSE
                CALL UTMESS ('F','NMCOMP','ERREUR DE'
     &       // 'PROGRAMMATION 1')
           ENDIF
        ELSE
          CALL UTMESS('F','NMCOMP_1','LOI DE COMPORTEMENT INEXISTANTE')
        END IF
C PETITES DEFORMATIONS
      ELSE
        IF ( COMPOR(1)(1:5) .EQ. 'ELAS '            .OR.
     &       COMPOR(1)(1:9) .EQ. 'VMIS_ISOT'        .OR.
     &       COMPOR(1)(1:14).EQ. 'VMIS_ISOT_LINE' ) THEN
          IF ( TYPMOD(2)(1:7) .EQ. 'MEGRDKT') THEN
            CALL UTMESS('F','GRILLE_01',
     &             ' COMPORTEMENT INATTENDU : '//COMPOR(1))
          ENDIF
          IF ( INT(CRIT(6)) .EQ. 0 ) THEN
            CALL NMISOT ( NDIM,  TYPMOD, IMATE, COMPOR,CRIT,
     &                    INSTAM,INSTAP,TM,TP,TREF,HYDRM,HYDRP,SECHM,
     &                    SECHP,SREF,DEPS,SIGM,VIM,OPTION,SIGP,VIP,
     &                    DSIDEP,RBID,RBID)
          ELSE
            CALL UTMESS('F','NMCOMP_1','INTEGRATION EXPLICITE DU
     &      COMPORTEMENT NON PROGRAMMEE')
          ENDIF
        ELSE IF (COMPOR(1).EQ. 'ENDO_FRAGILE') THEN
          CALL LCFRLO(NDIM, TYPMOD, IMATE, EPSM, DEPS,
     &                   VIM, OPTION, SIGP, VIP,  DSIDEP)
        ELSEIF ( COMPOR(1) .EQ. 'ENDO_ISOT_BETON' ) THEN
          IF ( INT(CRIT(6)) .NE. 0 )  THEN
              CALL UTMESS('F','NMCOMP_1',
     &          'INTEGRATION EXPLICITE DU COMPORTEMENT NON PROGRAMMEE')
            ELSE
              CALL LCLDSB(NDIM, TYPMOD, IMATE, COMPOR, EPSM, DEPS,
     &                   VIM,TM,TP,TREF,OPTION, SIGP, VIP,  DSIDEP)
            ENDIF
        ELSE IF ( COMPOR(1)(1:6) .EQ. 'MAZARS' ) THEN
          IF ( INT(CRIT(6)) .NE. 0 )  THEN
              CALL UTMESS('F','NMCOMP_1',
     &          'INTEGRATION EXPLICITE DU COMPORTEMENT NON PROGRAMMEE')
            ELSE
            CALL LCMAZA(NDIM, TYPMOD, IMATE, COMPOR, EPSM, DEPS,
     &                   VIM,TM,TP,TREF,HYDRM,HYDRP,SECHM,SECHP,SREF,
     &                   OPTION, SIGP, VIP,  DSIDEP)
            ENDIF
        ELSE IF ( COMPOR(1) .EQ. 'DRUCKER_PRAGER' ) THEN
          CALL LCDRPR(TYPMOD,OPTION,IMATE,SIGM,TM,TP,TREF,
     &                            DEPS,VIM,VIP,SIGP,DSIDEP,CODRET)
        ELSE IF ( COMPOR(1)(1:10) .EQ. 'BARENBLATT' ) THEN
          CALL LCBARE(IMATE, OPTION, EPSM, SIGP, DSIDEP, VIM, VIP)
        ELSE IF (COMPOR(1).EQ. 'META_P_IL       '.OR.
     &           COMPOR(1).EQ. 'META_P_IL_PT    '.OR.
     &           COMPOR(1).EQ. 'META_P_IL_RE    '.OR.
     &           COMPOR(1).EQ. 'META_P_IL_PT_RE '.OR.
     &           COMPOR(1).EQ. 'META_V_IL       '.OR.
     &           COMPOR(1).EQ. 'META_V_IL_PT    '.OR.
     &           COMPOR(1).EQ. 'META_V_IL_RE    '.OR.
     &           COMPOR(1).EQ. 'META_V_IL_PT_RE '.OR.
     &           COMPOR(1).EQ. 'META_P_INL      '.OR.
     &           COMPOR(1).EQ. 'META_P_INL_PT   '.OR.
     &           COMPOR(1).EQ. 'META_P_INL_RE   '.OR.
     &           COMPOR(1).EQ. 'META_P_INL_PT_RE'.OR.
     &           COMPOR(1).EQ. 'META_V_INL      '.OR.
     &           COMPOR(1).EQ. 'META_V_INL_PT   '.OR.
     &           COMPOR(1).EQ. 'META_V_INL_RE   '.OR.
     &           COMPOR(1).EQ. 'META_V_INL_PT_RE'  ) THEN
            IF (COMPOR(8)(1:5) .EQ. 'ACIER') THEN
             IF (NZ .NE. 7) CALL UTMESS ('F','NMCOMP','NOMBRE DE
     &       PHASE INCORRECT')
             CALL NZISFW ( NDIM,  IMATE, COMPOR,CRIT,
     &                     INSTAM,INSTAP, TM,TP, TREF, EPSM,
     &                     DEPS,  SIGM,   VIM, PHASM ,PHASP,
     &                     OPTION,SIGP, VIP, DSIDEP)
            ELSEIF (COMPOR(8)(1:4) .EQ. 'ZIRC') THEN
             IF (NZ .NE. 3) CALL UTMESS ('F','NMCOMP','NOMBRE DE
     &       PHASE INCORRECT')
             CALL NZEDGA ( NDIM,  IMATE, COMPOR,CRIT,
     &                     INSTAM,INSTAP, TM,TP, TREF, EPSM,
     &                     DEPS,  SIGM,   VIM, PHASM ,PHASP,
     &                     OPTION,SIGP, VIP, DSIDEP)
            ELSE
                CALL UTMESS ('F','NMCOMP','ERREUR DE'
     &       // 'PROGRAMMATION 2')
            ENDIF
        ELSE IF (COMPOR(1).EQ.'META_P_CL       '.OR.
     &           COMPOR(1).EQ. 'META_P_CL_PT    '.OR.
     &           COMPOR(1).EQ. 'META_P_CL_RE    '.OR.
     &           COMPOR(1).EQ. 'META_P_CL_PT_RE '.OR.
     &           COMPOR(1).EQ. 'META_V_CL       '.OR.
     &           COMPOR(1).EQ. 'META_V_CL_PT    '.OR.
     &           COMPOR(1).EQ. 'META_V_CL_RE    '.OR.
     &           COMPOR(1).EQ. 'META_V_CL_PT_RE ' ) THEN
            IF (COMPOR(8)(1:5) .EQ. 'ACIER') THEN
             IF (NZ .NE. 7) CALL UTMESS ('F','NMCOMP','NOMBRE DE
     &       PHASE INCORRECT')
             CALL NZCIFW ( NDIM,  IMATE, COMPOR,CRIT,
     &                     INSTAM,INSTAP, TM,TP, TREF, EPSM,
     &                     DEPS,  SIGM,   VIM, PHASM ,PHASP,
     &                     OPTION,SIGP, VIP, DSIDEP)

            ELSEIF (COMPOR(8)(1:4) .EQ. 'ZIRC') THEN
             IF (NZ .NE. 3) CALL UTMESS ('F','NMCOMP','NOMBRE DE
     &       PHASE INCORRECT')
             CALL NZCIZI ( NDIM,  IMATE, COMPOR,CRIT,
     &                     INSTAM,INSTAP, TM,TP, TREF, EPSM,
     &                     DEPS,  SIGM,   VIM, PHASM ,PHASP,
     &                     OPTION,SIGP, VIP, DSIDEP)
            ELSE
                CALL UTMESS ('F','NMCOMP','ERREUR DE'
     &       // 'PROGRAMMATION 2')
            ENDIF
        ELSE IF ( COMPOR(1)(1:11).EQ. 'NORTON_HOFF') THEN
          CALL NMHOFF(NDIM,IMATE,INSTAP,EPSM,DEPS,OPTION,SIGP,DSIDEP)

        ELSEIF ( COMPOR(1)(1:9) .EQ. 'VMIS_ECMI') THEN
          IF ( INT(CRIT(6)) .EQ. 0 ) THEN
            CALL NMECMI ( NDIM,  TYPMOD, IMATE, COMPOR,CRIT,
     &                    INSTAM,INSTAP,TM,TP,TREF,
     &                   DEPS,SIGM,VIM,OPTION,SIGP,VIP,DSIDEP)
          ELSE
            CALL UTMESS('F','NMCOMP_1','INTEGRATION EXPLICITE DU
     &      COMPORTEMENT NON PROGRAMMEE')
          ENDIF
        ELSE IF ( COMPOR(1)(1:14).EQ. 'VMIS_CINE_LINE') THEN
          IF ( INT(CRIT(6)) .EQ. 0 ) THEN
            IF (TYPMOD(1).EQ.'C_PLAN') THEN
               CALL UTMESS('F','NMCOMP','PAS DE C_PLAN POUR VMIS_CINE'//
     &                     'UTILISER C_PLAN_DEBORST')
            ELSE
              CALL NMCINE ( NDIM,  IMATE, COMPOR,CRIT,
     &                    INSTAM,INSTAP,TM,    TP,    TREF, EPSM,
     &                    DEPS,  SIGM,  VIM,   OPTION,SIGP, VIP, DSIDEP)
            ENDIF
          ELSE
            CALL UTMESS('F','NMCOMP_1','INTEGRATION EXPLICITE DU
     &      COMPORTEMENT NON PROGRAMMEE')
          ENDIF
        ELSE IF (( COMPOR(1)(1:14).EQ. 'VISC_CIN1_CHAB') .OR.
     &           ( COMPOR(1)(1:14).EQ. 'VISC_CIN2_CHAB')) THEN
          IF ( INT(CRIT(6)) .EQ. 0 ) THEN
            IF (TYPMOD(1).EQ.'C_PLAN') THEN
               CALL UTMESS('F','NMCOMP','PAS DE C_PLAN POUR VMIS_CIN1'//
     &                     'UTILISER C_PLAN_DEBORST')
            ELSE
               CALL NMCHAB ( NDIM,  TYPMOD, IMATE, COMPOR, CRIT,
     &                    INSTAM, INSTAP, TM,    TP,    TREF,
     &                    DEPS,  SIGM,  VIM,  OPTION, SIGP, VIP, DSIDEP)
            ENDIF
          ELSE
            CALL UTMESS('F','NMCOMP_1','INTEGRATION EXPLICITE DU
     &      COMPORTEMENT NON PROGRAMMEE')
          ENDIF
C
        ELSE IF (COMPOR(1)(1:11).EQ.'VISC_TAHERI') THEN
          IF ( INT(CRIT(6)) .NE. 0 )  THEN
            CALL UTMESS('F','NMCOMP_1',
     &        'INTEGRATION EXPLICITE DU COMPORTEMENT NON PROGRAMMEE')
          ELSE IF ((TYPMOD(1).EQ.'C_PLAN').AND.(.NOT.CP)) THEN
            CALL UTMESS('F','NMCOMP_2','PAS DE CONTRAINTES PLANES')
          ELSE
            CALL NMTAHE(NDIM,IMATE,COMPOR,CRIT,
     &                     INSTAM,INSTAP,TM,TP,TREF,EPSM,DEPS,SIGM,VIM,
     &                     OPTION,SIGP,VIP,DSIDEP)
          END IF
        ELSE IF ( COMPOR(1)(1:8) .EQ. 'ROUSS_PR'   .OR.
     &            COMPOR(1)(1:10).EQ. 'ROUSS_VISC' .OR.
     &            COMPOR(1)(1:8) .EQ. 'CHABOCHE'   .OR.
     &            COMPOR(1)(1:8) .EQ. 'BENALLAL'   .OR.
     &            COMPOR(1)(1:6) .EQ. 'BURLET'     .OR.
     &            COMPOR(1)(1:4) .EQ. 'OHNO'       .OR.
     &            COMPOR(1)(1:6) .EQ. 'TAHERI'     .OR.
     &            COMPOR(1)(1:5) .EQ. 'LMARC'      .OR.
     &            COMPOR(1)(1:15).EQ. 'BETON_DOUBLE_DP'.OR.
     &            COMPOR(1)(1:7) .EQ. 'NADAI_B'        ) THEN
          IF ( INT(CRIT(6)) .EQ. 0 ) THEN
            CALL REDECE ( NDIM,  TYPMOD,  IMATE,COMPOR,CRIT,
     &           INSTAM, INSTAP, TM,   TP,    TREF,
     &           HYDRM, HYDRP, SECHM, SECHP, SREF, EPSM, DEPS,
     &           SIGM, VIM,OPTION, ELGEOM,SIGP, VIP, DSIDEP)
          ELSE
            CALL UTMESS('F','NMCOMP_1','INTEGRATION EXPLICITE DU
     &      COMPORTEMENT NON PROGRAMMEE')
          ENDIF
        ELSE IF ( COMPOR(1)(1:9) .EQ. 'VISCOCHAB'  ) THEN
          IF ( INT(CRIT(6)) .EQ. 0 ) THEN
            CALL REDECE ( NDIM,  TYPMOD,  IMATE,COMPOR,CRIT,
     &                  INSTAM,INSTAP, TM,   TP,    TREF,
     &                  HYDRM, HYDRP,SECHM, SECHP, SREF, EPSM, DEPS,
     &                  SIGM, VIM,OPTION,ELGEOM,SIGP, VIP, DSIDEP)
          ELSE
            CALL NMVPRK ( NDIM,  TYPMOD,  IMATE,COMPOR,CRIT,
     &                  INSTAM,INSTAP, TM,   TP,    TREF, EPSM,
     &                  DEPS,  SIGM,   VIM,  OPTION,SIGP, VIP, DSIDEP)
          ENDIF
        ELSE IF ( COMPOR(1)(1:8) .EQ. 'POLY_CFC' ) THEN
          IF ( INT(CRIT(6)) .EQ. 0 ) THEN
            CALL UTMESS('F','NMCOMP_1','INTEGRATION DU COMPORTEMENT
     &      POLY_CFC UNIQUEMENT EXPLICITE')
          ELSE
            CALL NMVPRK ( NDIM,  TYPMOD,  IMATE, COMPOR, CRIT,
     &                    INSTAM,INSTAP, TM,    TP,     TREF,
     &                    EPSM,  DEPS,   SIGM,  VIM,    OPTION,
     &                    SIGP,  VIP,    DSIDEP)
          ENDIF
        ELSE IF ( COMPOR(1)(1:9) .EQ. 'VENDOCHAB'  ) THEN
          IF ( INT(CRIT(6)) .EQ. 0 ) THEN
C-- INTEGRATION IMPLICITE: METHODE D'EULER
            CALL NMVEEI (NDIM, TYPMOD, IMATE, COMPOR, CRIT,
     &                  INSTAM, INSTAP, TM, TP, TREF, EPSM,
     &                  DEPS, SIGM, VIM, OPTION, SIGP, VIP, DSIDEP)
          ELSE
            CALL NMVPRK ( NDIM,  TYPMOD,  IMATE,COMPOR,CRIT,
     &                  INSTAM,INSTAP, TM,   TP,    TREF, EPSM,
     &                  DEPS,  SIGM,   VIM,  OPTION,SIGP, VIP, DSIDEP)
          ENDIF
        ELSEIF ( COMPOR(1)(1:10) .EQ. 'ASSE_COMBU' .OR.
     &           COMPOR(1)(1:10) .EQ. 'ZIRC_CYRA2' .OR.
     &           COMPOR(1)(1:9)  .EQ. 'ZIRC_EPRI'  .OR.
     &           COMPOR(1)(1:10) .EQ. 'VISC_IRRA_' ) THEN
          IF ( INT(CRIT(6)) .EQ. 0 ) THEN
            CALL NMVPIR ( NDIM,  IMATE, COMPOR,CRIT,TYPMOD,
     &                  INSTAM,INSTAP,TM,    TP,    TREF,
     &                  DEPS,  SIGM,  VIM,   OPTION, DEFAM, DEFAP,
     &                  SIGP, VIP, DSIDEP )
          ELSE
            CALL UTMESS('F','NMCOMP_1','INTEGRATION EXPLICITE DU
     &      COMPORTEMENT NON PROGRAMMEE')
          ENDIF
        ELSEIF ( COMPOR(1)(1:8)  .EQ. 'LEMAITRE') THEN
          IF ( INT(CRIT(6)) .EQ. 0 ) THEN
            CALL NMVPLE ( NDIM,  IMATE, COMPOR,CRIT,TYPMOD,
     &                  INSTAM,INSTAP,TM,    TP,    TREF, 
     &                  DEPS,  SIGM,  VIM,   OPTION, DEFAM, DEFAP,
     &                  SIGP, VIP, DSIDEP )
          ELSE
            CALL UTMESS('F','NMCOMP_1','INTEGRATION EXPLICITE DU
     &      COMPORTEMENT NON PROGRAMMEE')
          ENDIF   
        ELSEIF ( COMPOR(1)(1:3) .EQ. 'CJS' ) THEN
          IF ( INT(CRIT(6)) .NE. 0 )  THEN
              CALL UTMESS('F','NMCOMP_1',
     &          'INTEGRATION EXPLICITE DU COMPORTEMENT NON PROGRAMMEE')
          ELSE IF ((TYPMOD(1).EQ.'C_PLAN').AND.(.NOT.CP)) THEN
              CALL UTMESS('F','NMCOMP_2','PAS DE CONTRAINTES PLANES')
            ELSE
              CALL NMCJS(  TYPMOD,  IMATE, COMPOR, CRIT,
     &                     INSTAM, INSTAP, TM, TP, TREF, EPSM,
     &                     DEPS, SIGM, VIM, OPTION, SIGP, VIP, DSIDEP)
            ENDIF
        ELSEIF ( COMPOR(1)(1:9) .EQ. 'CAM_CLAY ') THEN  
        IF ( INT(CRIT(6)) .NE. 0 )  THEN
              CALL UTMESS('F','NMCOMP_1',
     &          'INTEGRATION EXPLICITE DU COMPORTEMENT NON PROGRAMMEE')
          ELSE
            CALL NMCCAM (NDIM,  TYPMOD, IMATE, COMPOR,CRIT,INSTAM,
     &                   INSTAP,TM,TP,TREF,DEPS,SIGM,VIM,
     &                   OPTION,SIGP,VIP,DSIDEP)
          ENDIF            
        ELSEIF ( COMPOR(1)(1:6) .EQ. 'LAIGLE' ) THEN
          IF ( INT(CRIT(6)) .NE. 0 )  THEN
              CALL UTMESS('F','NMCOMP_1',
     &          'INTEGRATION EXPLICITE DU COMPORTEMENT NON PROGRAMMEE')
          ELSE
            CALL REDECE ( NDIM,  TYPMOD,  IMATE, COMPOR, CRIT,
     &                    INSTAM, INSTAP, TM,   TP,    TREF,
     &                    HYDRM, HYDRP, SECHM, SECHP, SREF, EPSM, DEPS,
     &                    SIGM, VIM, OPTION, ELGEOM, SIGP, VIP, DSIDEP)
          ENDIF
        ELSEIF ( COMPOR(1)(1:10) .EQ. 'GRANGER_FP' ) THEN
          IF ( INT(CRIT(6)) .NE. 0 )  THEN
              CALL UTMESS('F','NMCOMP_1',
     &          'INTEGRATION EXPLICITE DU COMPORTEMENT NON PROGRAMMEE')
          ELSE
            CALL NMGRAN (NDIM,  TYPMOD, IMATE, COMPOR,CRIT,INSTAM,
     &                   INSTAP,TM,TP,TREF,HYDRM,HYDRP,SECHM,SECHP,SREF,
     &                   TM,TP,DEPS,SIGM,VIM,OPTION,SIGP,VIP,DSIDEP)
          ENDIF
C -- FLUAGE PROPRE UMLV
        ELSEIF ( COMPOR(1)(1:13) .EQ. 'BETON_UMLV_FP' ) THEN
          IF ( INT(CRIT(6)) .NE. 0 )  THEN
              CALL UTMESS('F','NMCOMP_1',
     &          'INTEGRATION EXPLICITE DU COMPORTEMENT NON PROGRAMMEE')
          ELSE
            CALL LCUMFP (NDIM,TYPMOD,IMATE,COMPOR,INSTAM,INSTAP,
     &                   SECHM,SECHP,EPSM,
     &                   DEPS,SIGM,VIM,OPTION,SIGP,VIP,DSIDEP)
          END IF
C----LOI D'ACIER CORRODE
        ELSEIF ( COMPOR(1)(1:10) .EQ. 'CORR_ACIER') THEN
          IF ( INT(CRIT(6)) .NE. 0 )  THEN              
              CALL UTMESS('F','NMCOMP_1',
     &          'INTEGRATION EXPLICITE DU COMPORTEMENT NON PROGRAMMEE')
          ELSE  
          CALL NM3DCO(NDIM,OPTION,IMATE,TM,TP,E,SIGM,
     &           EPSM,DEPS,VIM,DEFAM,DEFAP,SIGP,VIP,DSIDEP,
     &            CORRM,CORRP)
        END IF                 
C -- COMPORTEMENT VIDE
        ELSEIF ( COMPOR(1)(1:4) .EQ. 'SANS' ) THEN
           CALL LCSANS (NDIM,OPTION,SIGP,DSIDEP)
C
        ELSEIF ( COMPOR(1)(1:10) .EQ. 'BAZANT_FD' ) THEN
          IF ( INT(CRIT(6)) .NE. 0 )  THEN
              CALL UTMESS('F','NMCOMP_1',
     &          'INTEGRATION EXPLICITE DU COMPORTEMENT NON PROGRAMMEE')
          ELSE IF (TYPMOD(1).EQ.'C_PLAN') THEN
           CALL UTMESS('F','NMCOMP','PAS DE C_PLAN POUR BAZANT_FD  '//
     &                     'UTILISER C_PLAN_DEBORST')
            ELSE
              CALL  NMGDES (NDIM,TYPMOD,IMATE,COMPOR,CRIT,
     &                   INSTAM,INSTAP,TM,TP,TREF,HYDRM,HYDRP,
     &                   SECHM,SECHP,SREF,TM,TP,DEPS,SIGM,VIM,
     &                   OPTION,SIGP,VIP,DSIDEP)
          ENDIF
        ELSEIF ( COMPOR(1)(1:7) .EQ. 'KIT_DDI' ) THEN
          IF ( INT(CRIT(6)) .NE. 0 )  THEN
              CALL UTMESS('F','NMCOMP_1',
     &          'INTEGRATION EXPLICITE DU COMPORTEMENT NON PROGRAMMEE')
          ELSE
            CALL NMCOUP ( NDIM,  TYPMOD,  IMATE,COMPOR,CP,CRIT,
     &           INSTAM, INSTAP, TM,   TP,    TREF,
     &           HYDRM, HYDRP, SECHM, SECHP,SREF, EPSM, DEPS,
     &           SIGM, VIM,OPTION, ELGEOM,SIGP, VIP, DSIDEP)
          ENDIF
        
CCC    MONOCRISTAL      
          
        ELSEIF ( COMPOR(1)(1:8) .EQ. 'MONOCRIS' ) THEN
          IF ( INT(CRIT(6)) .EQ. 0 ) THEN
            CALL REDECE ( NDIM,  TYPMOD,  IMATE,COMPOR,CRIT,
     &                  INSTAM,INSTAP, TM,   TP,    TREF,
     &                  HYDRM, HYDRP,SECHM, SECHP, SREF,EPSM, DEPS,
     &                  SIGM, VIM,OPTION,ELGEOM,SIGP, VIP, DSIDEP)
          ELSE
            CALL NMVPRK ( NDIM,  TYPMOD,  IMATE,COMPOR,CRIT,
     &                  INSTAM,INSTAP, TM,   TP,    TREF, EPSM,
     &                  DEPS,  SIGM,   VIM,  OPTION,SIGP, VIP, DSIDEP)
          ENDIF
          
CCC    FIN MONOCRISTAL      
          
        ELSE
          CALL UTMESS('F','NMCOMP_1','LOI DE COMPORTEMENT INEXISTANTE')
        ENDIF
      END IF
      
      
C      CONTRAINTES PLANES METHODE DE BORST
 9000 CONTINUE
      IF (CP) CALL NMCPL2(COMPOR,TYPMOD,OPTION,OPTIO2,CPL,NVV,CRIT,DEPS,
     &                    DSIDEP,NDIM,SIGP,VIP,CODRET)
      END
