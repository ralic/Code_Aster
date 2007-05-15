      SUBROUTINE NMCOMP (FAMI,KPG,KSP,NDIM,TYPMOD,IMATE,COMPOR,CRIT,
     &                   INSTAM,INSTAP,
     &                   EPSM,DEPS,
     &                   SIGM,VIM,
     &                   OPTION,ANGMAS,TAMPON,
     &                   SIGP,VIP,DSIDEP,CODRET)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C ======================================================================
C MODIF ALGORITH  DATE 15/05/2007   AUTEUR GENIAUT S.GENIAUT 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
C TOLE CRP_20
C TOLE CRP_21
      IMPLICIT NONE
      INTEGER            KPG,KSP,NDIM,IMATE,CODRET
      CHARACTER*8        TYPMOD(*)
      CHARACTER*(*) FAMI
      CHARACTER*16       COMPOR(*), OPTION,PHENOM
      REAL*8             CRIT(*), INSTAM, INSTAP
      REAL*8             EPSM(*), DEPS(*), DSIDEP(*)
      REAL*8             SIGM(*), VIM(*), SIGP(*), VIP(*)
      REAL*8             TAMPON(*)
      REAL*8             ANGMAS(*)
C ----------------------------------------------------------------------
C     INTEGRATION DES LOIS DE COMPORTEMENT NON LINEAIRE POUR LES
C     ELEMENTS ISOPARAMETRIQUES EN PETITES DEFORMATIONS
C
C IN  KPG,KSP  : NUMERO DU (SOUS)POINT DE GAUSS
C     NDIM    : DIMENSION DE L'ESPACE
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
C     EPSM    : DEFORMATIONS A L'INSTANT DU CALCUL PRECEDENT
C     DEPS    : INCREMENT DE DEFORMATION TOTALE :
C                DEPS(T) = DEPS(MECANIQUE(T)) + DEPS(DILATATION(T))
C     SIGM    : CONTRAINTES A L'INSTANT DU CALCUL PRECEDENT
C     VIM     : VARIABLES INTERNES A L'INSTANT DU CALCUL PRECEDENT
C     OPTION  : OPTION DEMANDEE : RIGI_MECA_TANG , FULL_MECA , RAPH_MECA
C     ANGMAS  : LES TROIS ANGLES DU MOT_CLEF MASSIF (AFFE_CARA_ELEM),
C               + UN REEL QUI VAUT 0 SI NAUTIQUIES OU 2 SI EULER
C               + LES 3 ANGLES D'EULER
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
C  INPUT
C   VIM        VARIABLES INTERNES EN T-
C   VIP        VARIABLES INTERNES EN T+ DE L'ITERATION PRECEDENTE
C   EPSM(3,3)    GRADIENT DE LA TRANSFORMATION EN T-
C   DEPS(3,3)    GRADIENT DE LA TRANSFORMATION DE T- A T+
C
C OUTPUT SI RESI (RAPH_MECA, FULL_MECA_*)
C   VIP      VARIABLES INTERNES EN T+
C   SIGP(6)  CONTRAINTE DE KIRCHHOFF EN T+ RANGES DANS L'ORDRE
C         XX YY ZZ SQRT(2)*XY SQRT(2)*XZ SQRT(2)*YZ
C
C OUTPUT SI RIGI (RIGI_MECA_*, FULL_MECA_*)
C   DSIDEP(6,3,3) MATRICE TANGENTE D(TAU)/D(FD) * (FD)T
C                 (AVEC LES RACINES DE 2)
C ----------------------------------------------------------------------
C
C    POUR LES UTILITAIRES DE CALCUL TENSORIEL
      INTEGER NDT,NDI
      COMMON /TDIM/ NDT,NDI

      REAL*8 R8BID,R8VIDE,TP,TM,TREF
      CHARACTER*16 OPTIO2
      CHARACTER*2 K2BID
      LOGICAL CP
      INTEGER CPL,NVV,IRET

      CODRET = 0
      R8BID=R8VIDE()


C      CONTRAINTES PLANES
      CALL NMCPL1(COMPOR,TYPMOD,OPTION,VIP,DEPS,OPTIO2,CPL,NVV)
      CP=(CPL.NE.0)

C    DIMENSIONNEMENT POUR LE CALCUL TENSORIEL
      NDT = 2*NDIM
      NDI = NDIM

C ----------------------------------------------------------------------
C                        CAS DES LOIS GRAD_VARI
C ----------------------------------------------------------------------

      IF (TYPMOD(2) .EQ. 'GRADVARI') THEN

        IF (INT(CRIT(6)) .NE. 0)  CALL U2MESS('F','ALGORITH6_82')

C -- ENDOMMAGEMENT FRAGILE (COURBE DE TRACTION BI-LINEAIRE)

        IF (COMPOR(1) .EQ. 'ENDO_FRAGILE') THEN
          CALL LCFRGV (NDIM  , TYPMOD, OPTION, IMATE , EPSM  , 
     &                 DEPS  , VIM   , TAMPON, SIGP  , VIP   ,
     &                 DSIDEP)
            GOTO 9000
        END IF

C -- ENDOMMAGEMENT FRAGILE ISOTROPE UNILATERAL: ENDO_ISOT_BETON

        IF ( COMPOR(1) .EQ. 'ENDO_ISOT_BETON') THEN
          CALL LCEIGV (FAMI,KPG,KSP,NDIM,TYPMOD,IMATE,COMPOR,
     &                 EPSM,DEPS,VIM,TAMPON,
     &                 OPTION,SIGP,VIP,DSIDEP)
          GOTO 9000
        END IF


C -- PLASTICITE ISOTROPE A GRADIENT

        IF ( COMPOR(1) .EQ. 'VMIS_ISOT_LINE'
     & .OR. COMPOR(1) .EQ. 'VMIS_ISOT_TRAC') THEN
          CALL LCVMGV (FAMI,KPG,KSP,NDIM,TYPMOD,IMATE,COMPOR,CRIT,
     &                 INSTAM,INSTAP,DEPS,SIGM,VIM,TAMPON,
     &                 OPTION,SIGP,VIP,DSIDEP,R8BID,R8BID,CODRET)
          GOTO 9000
        END IF



      END IF


C ----------------------------------------------------------------------
C                        CAS DES LOIS GRAD_EPSI
C ----------------------------------------------------------------------

      IF (TYPMOD(2) .EQ. 'GRADEPSI') THEN

        IF (INT(CRIT(6)) .NE. 0)  CALL U2MESS('F','ALGORITH6_82')

C -- ENDOMMAGEMENT FRAGILE (COURBE DE TRACTION BI-LINEAIRE)

        IF (COMPOR(1) .EQ. 'ENDO_FRAGILE') THEN
          CALL LCFRGE(NDIM, TYPMOD, IMATE, EPSM, DEPS,
     &                VIM, OPTION, SIGP, VIP,  DSIDEP,TAMPON)
          GOTO 9000
        END IF

C -- RUPTURE BETON

        IF ( COMPOR(1) .EQ. 'ENDO_ISOT_BETON' ) THEN
          CALL LCDSBE(NDIM, TYPMOD, IMATE, COMPOR, EPSM, DEPS,
     &                VIM, OPTION, SIGP, VIP,  DSIDEP,TAMPON)
          GOTO 9000
        ENDIF

        IF ( COMPOR(1) .EQ. 'ENDO_ORTH_BETON' ) THEN
          CALL LCEOBG(NDIM, TYPMOD, IMATE, CRIT, EPSM, DEPS,
     &                VIM, OPTION, SIGP, VIP,  DSIDEP, TAMPON,
     &                CODRET)
          GOTO 9000
        ENDIF

C -- DRUCKER - PRAGER

        IF (COMPOR(1) .EQ. 'DRUCKER_PRAGER') THEN
          CALL LCDPNL(FAMI,KPG,KSP,TYPMOD,NDIM,OPTION,IMATE,SIGM,
     &                EPSM,DEPS,VIM,VIP,SIGP,DSIDEP,TAMPON,
     &                CODRET)
          GOTO 9000
        END IF

C -- MAZARS

        IF (COMPOR(1) .EQ. 'MAZARS') THEN
          CALL LCMZGE(FAMI,KPG,KSP,NDIM, TYPMOD, IMATE, EPSM,
     &                DEPS,VIM,OPTION, SIGP, VIP,
     &                DSIDEP, TAMPON)
          GOTO 9000
        END IF


      END IF

C----------------------------------------------------------------------
C                 CAS DES LOIS COHESIVES (POUR L'ELEMENT DE JOINT)
C----------------------------------------------------------------------

      IF (TYPMOD(2) .EQ. 'ELEMJOIN') THEN

        IF ( COMPOR(1) .EQ. 'CZM_EXP_REG' ) THEN

          CALL LCEJEX(FAMI,KPG,KSP,NDIM,IMATE,OPTION,EPSM,DEPS,
     &                SIGP, DSIDEP, VIM, VIP)
          GOTO 9000

        ELSE IF ( COMPOR(1) .EQ. 'CZM_LIN_REG' ) THEN

          CALL LCEJLI(FAMI,KPG,KSP,NDIM,IMATE,OPTION,EPSM,DEPS,
     &                SIGP, DSIDEP, VIM, VIP)
          GOTO 9000

        ELSE IF ( COMPOR(1) .EQ. 'JOINT_BA' ) THEN

          CALL LCJOBA(NDIM, TYPMOD, IMATE, CRIT, EPSM, DEPS,
     &                 VIM, OPTION, SIGP, VIP,  DSIDEP, CODRET)
          GOTO 9000

        ELSE

          CALL U2MESK('F','ALGORITH6_84',1,COMPOR(1))

        ENDIF

      ENDIF

C----------------------------------------------------------------------


      IF (COMPOR(3).EQ.'SIMO_MIEHE') THEN
        IF (INT(CRIT(6)) .NE. 0) CALL U2MESS('F','ALGORITH6_85')
        IF  ( COMPOR(1) .EQ. 'VMIS_ISOT_LINE  ' .OR.
     &       COMPOR(1) .EQ. 'VMIS_ISOT_TRAC  ' .OR.
     &       COMPOR(1) .EQ. 'VISC_ISOT_LINE  ' .OR.
     &       COMPOR(1) .EQ. 'VISC_ISOT_TRAC  ' ) THEN
          CALL LCPIVM(FAMI,KPG,KSP,IMATE,COMPOR,CRIT,INSTAM,INSTAP,
     &               EPSM,DEPS,VIM,OPTION,SIGP,VIP,DSIDEP,CODRET)
        ELSE IF (COMPOR(1).EQ.'ELAS            ') THEN
           CALL U2MESS('F','COMPOR1_15')
        ELSE IF (COMPOR(1) .EQ. 'ROUSSELIER') THEN
          CALL LCROLO (FAMI,KPG,KSP,IMATE,OPTION,CRIT,
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
             CALL LCGDPM(FAMI,KPG,KSP,NDIM,IMATE,COMPOR,CRIT,
     &                   INSTAM,INSTAP,
     &                   EPSM,DEPS,SIGM,VIM,OPTION,SIGP,VIP,
     &                   DSIDEP,CODRET)
           ELSEIF (COMPOR(8)(1:4) .EQ. 'ZIRC') THEN
             CALL NZGDZI(FAMI,KPG,KSP,NDIM,IMATE,COMPOR,CRIT,
     &                   INSTAM,INSTAP,
     &                   EPSM,DEPS,SIGM,VIM,OPTION,SIGP,VIP,
     &                   DSIDEP,CODRET)
           ELSE
                CALL U2MESS('F','ALGORITH6_86')
           ENDIF
           CALL POSTSM(OPTION,EPSM,DEPS,SIGM,SIGP,DSIDEP)
        ELSE
          CALL U2MESS('F','ALGORITH6_87')
        END IF
C PETITES DEFORMATIONS
      ELSE
        IF ( COMPOR(1)(1:5) .EQ. 'ELAS '            .OR.
     &       COMPOR(1)(1:9) .EQ. 'VMIS_ISOT'        .OR.
     &       COMPOR(1)(1:14).EQ. 'VMIS_ISOT_LINE' ) THEN
          IF ( TYPMOD(2)(1:7) .EQ. 'MEGRDKT') THEN
            CALL U2MESK('F','ALGORITH4_50',1,COMPOR(1))
          ENDIF
          IF ( INT(CRIT(6)) .EQ. 0 ) THEN
            CALL RCCOMA(IMATE,'ELAS',PHENOM,K2BID)
            IF (PHENOM.EQ.'ELAS') THEN
              CALL NMISOT (FAMI,KPG,KSP,NDIM,TYPMOD,IMATE,COMPOR,CRIT,
     &                      INSTAM,INSTAP,DEPS,SIGM,VIM,
     &                      OPTION,SIGP,VIP,DSIDEP,R8BID,R8BID,CODRET)
            ELSE IF (PHENOM(1:8).EQ.'ELAS_ORT'.OR.
     &              PHENOM(1:8).EQ.'ELAS_IST') THEN
              CALL NMORTH(FAMI,KPG,KSP,1,NDIM,PHENOM,TYPMOD,IMATE,
     &                    EPSM,DEPS,SIGM,OPTION,ANGMAS,SIGP,VIP,
     &                    DSIDEP)
            ELSE
              CALL U2MESS('F','ALGORITH6_88')
            ENDIF
          ELSE
            CALL U2MESS('F','ALGORITH6_82')
          ENDIF
        ELSE IF (COMPOR(1).EQ. 'ENDO_FRAGILE') THEN
          CALL LCFRLO(NDIM, TYPMOD, IMATE, EPSM, DEPS,
     &                   VIM, OPTION, SIGP, VIP,  DSIDEP)
        ELSEIF ( COMPOR(1) .EQ. 'ENDO_ISOT_BETON' ) THEN
          IF ( INT(CRIT(6)) .NE. 0 )  THEN
              CALL U2MESS('F','ALGORITH6_82')
            ELSE
C APPEL DE RCVARC POUR LA RECUPERATION DE LA TEMPERATURE
C RAISON: CETTE ROUTINE EST APPELEE EN THM AUSSI... (CALCME)
              CALL RCVARC('F','TEMP','-',FAMI,KPG,KSP,TM,IRET)
              CALL RCVARC('F','TEMP','+',FAMI,KPG,KSP,TP,IRET)
              CALL RCVARC('F','TEMP','REF',FAMI,KPG,KSP,TREF,IRET)
              CALL LCLDSB(FAMI,KPG,KSP,NDIM, TYPMOD,IMATE,COMPOR,EPSM,
     &                   DEPS,VIM,TM,TP,TREF,OPTION,SIGP,VIP,DSIDEP)
            ENDIF
        ELSE IF ( COMPOR(1) .EQ. 'ENDO_ORTH_BETON' ) THEN
          IF ( INT(CRIT(6)) .NE. 0 )  THEN
              CALL U2MESS('F','ALGORITH6_82')
            ELSE
              CALL LCEOBL (NDIM, TYPMOD, IMATE, CRIT, EPSM, DEPS,
     &                   VIM, OPTION, SIGP, VIP,  DSIDEP,CODRET)
            ENDIF
        ELSE IF ( COMPOR(1)(1:6) .EQ. 'MAZARS' ) THEN
          IF ( INT(CRIT(6)) .NE. 0 )  THEN
              CALL U2MESS('F','ALGORITH6_82')
          ELSE
C APPEL DE RCVARC POUR LA RECUPERATION DE LA TEMPERATURE
C RAISON: CETTE ROUTINE EST APPELEE EN THM AUSSI... (CALCME)
            CALL RCVARC('F','TEMP','-',FAMI,KPG,KSP,TM,IRET)
            CALL RCVARC('F','TEMP','+',FAMI,KPG,KSP,TP,IRET)
            CALL RCVARC('F','TEMP','REF',FAMI,KPG,KSP,TREF,IRET)
            CALL LCMAZA(FAMI,KPG,KSP,NDIM,TYPMOD,IMATE,COMPOR,EPSM,
     &                  DEPS,VIM,TM,TP,TREF,OPTION,SIGP,VIP,DSIDEP)
          ENDIF
        ELSEIF ( COMPOR(1) .EQ. 'BETON_REGLE_PR' ) THEN
          IF ( INT(CRIT(6)) .NE. 0 )  THEN
              CALL U2MESS('F','ALGORITH6_82')
            ELSE
              CALL LCLBR1(FAMI,KPG,KSP,NDIM, TYPMOD, IMATE, COMPOR,
     &                   EPSM,DEPS,VIM,OPTION, SIGP, VIP,  DSIDEP)
            ENDIF
        ELSE IF ( COMPOR(1) .EQ. 'DRUCKER_PRAGER' ) THEN
C APPEL DE RCVARC POUR LA RECUPERATION DE LA TEMPERATURE
C RAISON: CETTE ROUTINE EST APPELEE EN THM AUSSI... (CALCME)
          CALL RCVARC('F','TEMP','-',FAMI,KPG,KSP,TM,IRET)
          CALL RCVARC('F','TEMP','+',FAMI,KPG,KSP,TP,IRET)
          CALL RCVARC('F','TEMP','REF',FAMI,KPG,KSP,TREF,IRET)
          CALL LCDRPR(TYPMOD,OPTION,IMATE,SIGM,TM,TP,TREF,
     &                            DEPS,VIM,VIP,SIGP,DSIDEP,CODRET)
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
             CALL NZISFW ( FAMI,KPG,KSP,NDIM,IMATE,COMPOR,CRIT,
     &                     INSTAM,INSTAP, EPSM,
     &                     DEPS,SIGM,VIM,OPTION,SIGP,
     &                     VIP,DSIDEP,CODRET)
            ELSEIF (COMPOR(8)(1:4) .EQ. 'ZIRC') THEN
             CALL NZEDGA ( FAMI,KPG,KSP,NDIM,IMATE,COMPOR,CRIT,
     &                     INSTAM,INSTAP, EPSM,
     &                     DEPS,SIGM,VIM,OPTION,SIGP,
     &                     VIP,DSIDEP,CODRET)
            ELSE
                CALL U2MESS('F','ALGORITH6_89')
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
             CALL NZCIFW ( FAMI,KPG,KSP,NDIM,IMATE,COMPOR,CRIT,
     &                     INSTAM,INSTAP, EPSM,
     &                     DEPS,  SIGM,   VIM,OPTION,SIGP,
     &                     VIP,DSIDEP,CODRET)

            ELSEIF (COMPOR(8)(1:4) .EQ. 'ZIRC') THEN
             CALL NZCIZI ( FAMI,KPG,KSP,NDIM,IMATE,COMPOR,CRIT,
     &                     INSTAM,INSTAP, EPSM,
     &                     DEPS,SIGM,VIM,OPTION,SIGP,
     &                     VIP,DSIDEP,CODRET)
            ELSE
                CALL U2MESS('F','ALGORITH6_89')
            ENDIF
        ELSE IF ( COMPOR(1)(1:11).EQ. 'NORTON_HOFF') THEN
          CALL NMHOFF(NDIM,IMATE,INSTAP,EPSM,DEPS,OPTION,SIGP,DSIDEP)

        ELSEIF ( COMPOR(1)(1:9) .EQ. 'VMIS_ECMI') THEN
          IF ( INT(CRIT(6)) .EQ. 0 ) THEN
            CALL NMECMI (FAMI,KPG,KSP, NDIM,  TYPMOD, IMATE, COMPOR,
     &                   CRIT, INSTAM,INSTAP,
     &                   DEPS,SIGM,VIM,OPTION,SIGP,VIP,DSIDEP,
     &                   CODRET)
          ELSE
            CALL U2MESS('F','ALGORITH6_82')
          ENDIF
        ELSE IF ( COMPOR(1)(1:14).EQ. 'VMIS_CINE_LINE') THEN
          IF ( INT(CRIT(6)) .EQ. 0 ) THEN
            IF (TYPMOD(1).EQ.'C_PLAN') THEN
               CALL U2MESS('F','ALGORITH6_90')
            ELSE
              CALL NMCINE (FAMI,KPG,KSP, NDIM,  IMATE, COMPOR,CRIT,
     &                    INSTAM,INSTAP,EPSM,
     &                    DEPS,  SIGM,  VIM,   OPTION,SIGP, VIP, DSIDEP)
            ENDIF
          ELSE
            CALL U2MESS('F','ALGORITH6_82')
          ENDIF
        ELSE IF (( COMPOR(1)(1:14).EQ. 'VMIS_CIN1_CHAB') .OR.
     &           ( COMPOR(1)(1:14).EQ. 'VMIS_CIN2_CHAB') .OR.
     &           ( COMPOR(1)(1:14).EQ. 'VISC_CIN1_CHAB') .OR.
     &           ( COMPOR(1)(1:14).EQ. 'VISC_CIN2_CHAB')) THEN
          IF ( INT(CRIT(6)) .EQ. 0 ) THEN
            IF (TYPMOD(1).EQ.'C_PLAN') THEN
               CALL U2MESS('F','ALGORITH6_91')
            ELSE
               CALL NMCHAB (FAMI,KPG,KSP, NDIM,  TYPMOD, IMATE, COMPOR,
     &                    CRIT,INSTAM, INSTAP,
     &                    DEPS,  SIGM,  VIM,  OPTION, SIGP, VIP, DSIDEP,
     &                    CODRET)
            ENDIF
          ELSE
            CALL U2MESS('F','ALGORITH6_82')
          ENDIF
C
        ELSE IF (COMPOR(1)(1:11).EQ.'VISC_TAHERI') THEN
          IF ( INT(CRIT(6)) .NE. 0 )  THEN
            CALL U2MESS('F','ALGORITH6_82')
          ELSE IF ((TYPMOD(1).EQ.'C_PLAN').AND.(.NOT.CP)) THEN
            CALL U2MESS('F','ALGORITH6_92')
          ELSE
            CALL NMTAHE(FAMI,KPG,KSP,NDIM,IMATE,COMPOR,CRIT,
     &                     INSTAM,INSTAP,EPSM,DEPS,SIGM,VIM,
     &                     OPTION,SIGP,VIP,DSIDEP,CODRET)
          END IF
        ELSE IF ( COMPOR(1)(1:8) .EQ. 'ROUSS_PR'   .OR.
     &            COMPOR(1)(1:10).EQ. 'ROUSS_VISC' .OR.
     &            COMPOR(1)(1:5) .EQ. 'LMARC'      .OR.
     &            COMPOR(1)(1:15).EQ. 'BETON_DOUBLE_DP'.OR.
     &            COMPOR(1)(1:7 ).EQ. 'IRRAD3M'    .OR.
     &            COMPOR(1)(1:7) .EQ. 'NADAI_B'        ) THEN
          IF ( INT(CRIT(6)) .EQ. 0 ) THEN
            CALL REDECE ( FAMI,KPG,KSP,NDIM,TYPMOD,IMATE,COMPOR,CRIT,
     &           INSTAM,INSTAP,R8BID,R8BID,R8BID,EPSM,DEPS,
     &           SIGM,VIM,OPTION,TAMPON,ANGMAS,SIGP,VIP,DSIDEP,CODRET)
          ELSE
            CALL U2MESS('F','ALGORITH6_82')
          ENDIF
        ELSE IF ( COMPOR(1)(1:9) .EQ. 'VISCOCHAB'  ) THEN
          IF ( INT(CRIT(6)) .EQ. 0 ) THEN
            CALL REDECE ( FAMI,KPG,KSP,NDIM,TYPMOD,IMATE,COMPOR,CRIT,
     &           INSTAM,INSTAP,R8BID,R8BID,R8BID,EPSM,DEPS,
     &           SIGM,VIM,OPTION,TAMPON,ANGMAS,SIGP,VIP,DSIDEP,CODRET)
          ELSE
            CALL NMVPRK (FAMI,KPG,KSP,NDIM,TYPMOD,IMATE,COMPOR,CRIT,
     &                  INSTAM,INSTAP, EPSM,
     &          DEPS,  SIGM,   VIM,  OPTION,ANGMAS,SIGP, VIP, DSIDEP)
          ENDIF
        ELSE IF ( COMPOR(1)(1:8) .EQ. 'POLY_CFC' ) THEN
          IF ( INT(CRIT(6)) .EQ. 0 ) THEN
            CALL U2MESS('F','ALGORITH6_93')
          ELSE
            CALL NMVPRK (FAMI,KPG,KSP,NDIM,TYPMOD,IMATE,COMPOR,CRIT,
     &                    INSTAM,INSTAP,
     &                    EPSM,  DEPS,   SIGM,  VIM,    OPTION,
     &                    ANGMAS, SIGP,  VIP,    DSIDEP)
          ENDIF
        ELSE IF ( COMPOR(1)(1:9) .EQ. 'VENDOCHAB'  ) THEN
          IF ( INT(CRIT(6)) .EQ. 0 ) THEN
C-- INTEGRATION IMPLICITE: METHODE D'EULER
            CALL NMVEEI (FAMI,KPG,KSP,NDIM,TYPMOD,IMATE,COMPOR,CRIT,
     &                  INSTAM, INSTAP, EPSM,
     &                  DEPS, SIGM, VIM, OPTION, SIGP, VIP, DSIDEP,
     &                  CODRET)
          ELSE
            CALL NMVPRK (FAMI,KPG,KSP, NDIM,TYPMOD,IMATE,COMPOR,CRIT,
     &                  INSTAM,INSTAP, EPSM,
     &         DEPS,  SIGM,   VIM,  OPTION,ANGMAS,SIGP, VIP, DSIDEP)
          ENDIF
        ELSEIF ( COMPOR(1)(1:13) .EQ. 'LEMAITRE_IRRA' .OR.
     &           COMPOR(1)(1:10) .EQ. 'LEMA_SEUIL' .OR.
     &           COMPOR(1)(1:10) .EQ. 'GRAN_IRRA_' .OR.
     &           COMPOR(1)(1:10) .EQ. 'ZIRC_CYRA2' .OR.
     &           COMPOR(1)(1:9)  .EQ. 'ZIRC_EPRI'  .OR.
     &           COMPOR(1)(1:10) .EQ. 'VISC_IRRA_' ) THEN
          IF ( INT(CRIT(6)) .EQ. 0 ) THEN
            CALL NMVPIR (FAMI,KPG,KSP,NDIM,TYPMOD,IMATE,COMPOR,CRIT,
     &                  INSTAM,INSTAP,
     &                  DEPS,
     &                  SIGM,VIM,
     &                  OPTION,
     &                  ANGMAS,
     &                  SIGP, VIP, DSIDEP, CODRET )
          ELSE
            CALL U2MESS('F','ALGORITH6_82')
          ENDIF
        ELSEIF ( COMPOR(1)(1:8)  .EQ. 'LEMAITRE') THEN
          IF ( INT(CRIT(6)) .EQ. 0 ) THEN
            CALL NMVPLE (FAMI,KPG,KSP,NDIM,IMATE,COMPOR,CRIT,TYPMOD,
     &                  INSTAM,INSTAP,
     &                  DEPS,SIGM,VIM,OPTION,
     &                  SIGP, VIP, DSIDEP , CODRET)
          ELSE
            CALL U2MESS('F','ALGORITH6_82')
          ENDIF
        ELSEIF ( COMPOR(1)(1:12)  .EQ. 'GATT_MONERIE') THEN
          IF ( INT(CRIT(6)) .EQ. 0 ) THEN
            CALL NMVPGM ( FAMI,KPG,KSP,NDIM,IMATE,COMPOR,CRIT,TYPMOD,
     &                  INSTAM,INSTAP,
     &                  DEPS,SIGM,VIM,OPTION,
     &                  SIGP, VIP, DSIDEP, CODRET )
          ELSE
            CALL U2MESS('F','ALGORITH6_82')
          ENDIF
        ELSEIF ( COMPOR(1)(1:3) .EQ. 'CJS' ) THEN
          IF ( INT(CRIT(6)) .NE. 0 )  THEN
              CALL U2MESS('F','ALGORITH6_82')
          ELSE IF ((TYPMOD(1).EQ.'C_PLAN').AND.(.NOT.CP)) THEN
              CALL U2MESS('F','ALGORITH6_92')
            ELSE
C APPEL DE RCVARC POUR LA RECUPERATION DE LA TEMPERATURE
C RAISON: CETTE ROUTINE EST APPELEE EN THM AUSSI... (CALCME)
              CALL RCVARC('F','TEMP','-',FAMI,KPG,KSP,TM,IRET)
              CALL RCVARC('F','TEMP','+',FAMI,KPG,KSP,TP,IRET)
              CALL RCVARC('F','TEMP','REF',FAMI,KPG,KSP,TREF,IRET)

              CALL NMCJS(  TYPMOD,  IMATE, COMPOR, CRIT,
     &                     INSTAM, INSTAP, TM, TP, TREF, EPSM,
     &                     DEPS, SIGM, VIM, OPTION, SIGP, VIP, DSIDEP,
     &                     CODRET)
            ENDIF
C --- hujeux
          ELSEIF ( COMPOR(1)(1:6) .EQ. 'HUJEUX' ) THEN
            IF ( INT(CRIT(6)) .NE. 0 )  THEN
              CALL U2MESS('F','ALGORITH6_82')
            ELSE
C APPEL DE RCVARC POUR LE CALCUL DE LA TEMPERATURE
C RAISON: CETTE ROUTINE EST APPELEE EN THM AUSSI... (CALCME)
              CALL RCVARC('F','TEMP','-',FAMI,KPG,KSP,TM,IRET)
              CALL RCVARC('F','TEMP','+',FAMI,KPG,KSP,TP,IRET)
              CALL RCVARC('F','TEMP','REF',FAMI,KPG,KSP,TREF,IRET)
              CALL NMHUJ (TYPMOD,  IMATE, COMPOR, CRIT,
     &                    INSTAM, INSTAP, TM,TP,TREF,EPSM,
     &                    DEPS, SIGM, VIM, OPTION, SIGP, VIP, DSIDEP,
     &                    CODRET)
            ENDIF

        ELSEIF ( COMPOR(1)(1:9) .EQ. 'CAM_CLAY ') THEN
        IF ( INT(CRIT(6)) .NE. 0 )  THEN
              CALL U2MESS('F','ALGORITH6_82')
          ELSE
C APPEL DE RCVARC POUR LE CALCUL DE LA TEMPERATURE
C RAISON: CETTE ROUTINE EST APPELEE EN THM AUSSI... (CALCME)
            CALL RCVARC('F','TEMP','-',FAMI,KPG,KSP,TM,IRET)
            CALL RCVARC('F','TEMP','+',FAMI,KPG,KSP,TP,IRET)
            CALL RCVARC('F','TEMP','REF',FAMI,KPG,KSP,TREF,IRET)

            CALL NMCCAM (NDIM,  TYPMOD, IMATE, COMPOR,
     &                   CRIT,INSTAM,INSTAP,TM,TP,TREF,DEPS,SIGM,VIM,
     &                   OPTION,SIGP,VIP,DSIDEP,CODRET)
          ENDIF
        ELSEIF ( COMPOR(1)(1:6) .EQ. 'LAIGLE' ) THEN
          IF ( INT(CRIT(6)) .NE. 0 )  THEN
              CALL U2MESS('F','ALGORITH6_82')
          ELSE
C APPEL DE RCVARC POUR LE CALCUL DE LA TEMPERATURE
C RAISON: CETTE ROUTINE EST APPELEE EN THM AUSSI... (CALCME)
            CALL RCVARC('F','TEMP','-',FAMI,KPG,KSP,TM,IRET)
            CALL RCVARC('F','TEMP','+',FAMI,KPG,KSP,TP,IRET)
            CALL RCVARC('F','TEMP','REF',FAMI,KPG,KSP,TREF,IRET)
            CALL REDECE ( FAMI,KPG,KSP,NDIM,TYPMOD,IMATE,COMPOR,CRIT,
     &                    INSTAM,INSTAP,TM,TP,TREF,EPSM,DEPS,SIGM,VIM,
     &                    OPTION, TAMPON,ANGMAS,SIGP,VIP,DSIDEP,CODRET)
          ENDIF
        ELSEIF ( COMPOR(1)(1:10) .EQ. 'HOEK_BROWN' ) THEN
          IF ( INT(CRIT(6)) .NE. 0 )  THEN
              CALL U2MESS('F','ALGORITH6_82')
          ELSE
C APPEL DE RCVARC POUR LE CALCUL DE LA TEMPERATURE
C RAISON: CETTE ROUTINE EST APPELEE EN THM AUSSI... (CALCME)
            CALL RCVARC('F','TEMP','-',FAMI,KPG,KSP,TM,IRET)
            CALL RCVARC('F','TEMP','+',FAMI,KPG,KSP,TP,IRET)
            CALL RCVARC('F','TEMP','REF',FAMI,KPG,KSP,TREF,IRET)
            CALL REDECE ( FAMI,KPG,KSP,NDIM,TYPMOD,IMATE,COMPOR,CRIT,
     &                    INSTAM,INSTAP,TM,TP,TREF,EPSM,DEPS,SIGM,VIM,
     &                    OPTION,TAMPON,ANGMAS,SIGP,VIP,DSIDEP,CODRET)
          ENDIF
        ELSEIF ( COMPOR(1)(1:10) .EQ. 'GRANGER_FP' ) THEN
          IF ( INT(CRIT(6)) .NE. 0 )  THEN
              CALL U2MESS('F','ALGORITH6_82')
          ELSE
C APPEL DE RCVARC POUR LE CALCUL DE LA TEMPERATURE
C RAISON: CETTE ROUTINE EST APPELEE PAR NMCPLA AVEC COMME
C TEMPERATURE LES VALEURS MIN ET MAX... IL FAUT DONC LAISSER
C L ARGUMENT
            CALL RCVARC('F','TEMP','-',FAMI,KPG,KSP,TM,IRET)
            CALL RCVARC('F','TEMP','+',FAMI,KPG,KSP,TP,IRET)
            CALL RCVARC('F','TEMP','REF',FAMI,KPG,KSP,TREF,IRET)
            CALL NMGRAN (FAMI,KPG,KSP,NDIM,TYPMOD,IMATE,COMPOR,CRIT,
     &                   INSTAM,INSTAP,TM,TP,DEPS,SIGM,VIM,
     &                   OPTION,SIGP,VIP,DSIDEP)
          ENDIF
C -- FLUAGE PROPRE UMLV
        ELSEIF ( COMPOR(1)(1:13) .EQ. 'BETON_UMLV_FP' ) THEN
          IF ( INT(CRIT(6)) .NE. 0 )  THEN
              CALL U2MESS('F','ALGORITH6_82')
          ELSE
            CALL LCUMFP (FAMI,KPG,KSP,NDIM,TYPMOD,IMATE,COMPOR,INSTAM,
     &                   INSTAP,EPSM,DEPS,SIGM,VIM,OPTION,
     &                   SIGP,VIP,DSIDEP)
          END IF
C----LOI D'ACIER CORRODE
        ELSEIF ( COMPOR(1)(1:10) .EQ. 'CORR_ACIER') THEN
          IF ( INT(CRIT(6)) .NE. 0 )  THEN
              CALL U2MESS('F','ALGORITH6_82')
          ELSE
          CALL NM3DCO(FAMI,KPG,KSP,NDIM,OPTION,IMATE,SIGM,
     &           EPSM,DEPS,VIM,SIGP,VIP,DSIDEP,CRIT,CODRET)
        END IF
C -- COMPORTEMENT VIDE
        ELSEIF ( COMPOR(1)(1:4) .EQ. 'SANS' ) THEN
           CALL LCSANS (NDIM,OPTION,SIGP,DSIDEP)
C
        ELSEIF ( COMPOR(1)(1:10) .EQ. 'BAZANT_FD' ) THEN
          IF ( INT(CRIT(6)) .NE. 0 )  THEN
              CALL U2MESS('F','ALGORITH6_82')
          ELSE IF (TYPMOD(1).EQ.'C_PLAN') THEN
           CALL U2MESS('F','ALGORITH6_94')
            ELSE
              CALL  NMGDES (FAMI,KPG,KSP,NDIM,TYPMOD,IMATE,
     &                   COMPOR,CRIT,INSTAM,INSTAP,
     &                   DEPS,SIGM,VIM,OPTION,SIGP,VIP,DSIDEP)
          ENDIF
        ELSEIF ( COMPOR(1)(1:7) .EQ. 'KIT_DDI' ) THEN
          IF ( INT(CRIT(6)) .NE. 0 )  THEN
              CALL U2MESS('F','ALGORITH6_82')
          ELSE
            CALL NMCOUP (FAMI,KPG,KSP,NDIM,TYPMOD,IMATE,COMPOR,CP,CRIT,
     &           INSTAM, INSTAP, EPSM, DEPS,SIGM, VIM,OPTION,
     &           TAMPON,SIGP, VIP, DSIDEP,CODRET)
          ENDIF

CCC    MONOCRISTAL
        ELSEIF ( COMPOR(1)(1:8) .EQ. 'MONOCRIS' ) THEN
          IF ( INT(CRIT(6)) .EQ. 0 ) THEN
            CALL REDECE ( FAMI,KPG,KSP,NDIM,TYPMOD,IMATE,COMPOR,CRIT,
     &                  INSTAM,INSTAP,R8BID, R8BID, R8BID,
     &                  EPSM, DEPS,SIGM, VIM,OPTION,TAMPON,ANGMAS,
     &                  SIGP, VIP, DSIDEP,CODRET)
          ELSE
            CALL NMVPRK (FAMI,KPG,KSP,NDIM,TYPMOD,IMATE,COMPOR,CRIT,
     &                  INSTAM,INSTAP, EPSM,
     &       DEPS,  SIGM,   VIM,  OPTION,ANGMAS,SIGP, VIP, DSIDEP)
          ENDIF
CCC    FIN MONOCRISTAL

CCC    POLYCRISTAL
        ELSEIF ( COMPOR(1)(1:8) .EQ. 'POLYCRIS' ) THEN
          IF ( INT(CRIT(6)) .EQ. 0 ) THEN
              CALL U2MESS('F','ALGORITH6_95')
          ELSE
            CALL NMVPRK (FAMI,KPG,KSP,NDIM,TYPMOD,IMATE,COMPOR,CRIT,
     &                  INSTAM,INSTAP, EPSM,
     &       DEPS,  SIGM,   VIM,  OPTION,ANGMAS,SIGP, VIP, DSIDEP)
          ENDIF
CCC    FIN POLYCRISTAL

CCC    ZMAT
        ELSEIF ( COMPOR(1)(1:4) .EQ. 'ZMAT' ) THEN
            CALL NMZMAT( FAMI, KPG, KSP, NDIM, TYPMOD, COMPOR, CRIT,
     &                   INSTAM, INSTAP, EPSM, DEPS, SIGM,
     &                   VIM, OPTION, ANGMAS, SIGP, VIP, DSIDEP, CODRET)
CCC    FIN ZMAT


CCC    LOI HYPERELASTIQUE POUR METHODE SIGNORINI

        ELSEIF (COMPOR(1)(1:10).EQ. 'ELAS_HYPER') THEN
            IF (COMPOR(3).NE.'GREEN') THEN
              CALL U2MESS('F','ALGORITH6_96')
            ENDIF
            CALL HYPINC(FAMI,KPG,KSP,'-',NDIM,TYPMOD,IMATE,COMPOR,
     &                  CRIT,OPTION,EPSM,DEPS,SIGM,
     &                  SIGP,DSIDEP,CODRET)

CCC    FIN LOI HYPERELASTIQUE POUR METHODE SIGNORINI

        ELSE
          CALL U2MESS('F','ALGORITH6_87')
        ENDIF
      END IF


 9000 CONTINUE

C      CONTRAINTES PLANES METHODE DE BORST
      IF (CP) THEN
         IF (CODRET.NE.1) THEN
            CALL NMCPL2(COMPOR,TYPMOD,OPTION,OPTIO2,CPL,NVV,CRIT,
     &                    DEPS,DSIDEP,NDIM,SIGP,VIP,CODRET)
         ELSE
            OPTION=OPTIO2
         ENDIF
      ENDIF

      END
