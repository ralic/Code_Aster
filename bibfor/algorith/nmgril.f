      SUBROUTINE NMGRIL (IMATE, TYPMOD, COMPOR, OPTION,
     &                   EPSM, DEPS,
     &                    ANGMAS,
     &                   SIGM, VIM,
     &                   TM, TP, TREF,
     &                   SIGP, VIP, DSIDEP,CODRET )
C TOLE CRP_6
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 15/06/2004   AUTEUR MABBAS M.ABBAS 
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
C ----------------------------------------------------------------------
C
      IMPLICIT NONE
C ----------------------------------------------------------------------
C          REALISE LES LOIS ELASTIQUE ET ELASTOPLASTIQUE
C          ORTHOTROPES (DECOUPLEES) POUR L'ELEMENT GRILLE
C
C
C IN  IMATE   : ADRESSE DU MATERIAU CODE
C IN  TYPMOD(2): TYPE DE MODELISATION
C IN  TYPMOD(1): TYPE DE FORMULATION (MEGRDKT)
C IN  COMPOR  : COMPORTEMENT :  (1) = TYPE DE RELATION COMPORTEMENT
C                               (2) = NB VARIABLES INTERNES / PG
C IN  OPTION  : OPTION DEMANDEE : RIGI_MECA_TANG , FULL_MECA , RAPH_MECA
C IN  EPSM    : DEFORMATIONS A L'INSTANT DU CALCUL PRECEDENT
C IN  DEPS    : INCREMENT DE DEFORMATIONS
C               SI C_PLAN DEPS(3) EST EN FAIT INCONNU (ICI:0)
C                 =>  ATTENTION LA PLACE DE DEPS(3) EST ALORS UTILISEE.
C IN  ANGMAS  : ANGLE DU MOT_CLEF 'MASSIF' DE AFFE_CARA_ELEM
C               (ORIENTATION DU REPERE POUR ORTHOTROPIE ET GRANDISSEMENT
C                SOUS IRRADIATION)
C IN  SIGM    : CONTRAINTES A L'INSTANT DU CALCUL PRECEDENT
C IN  VIM     : VARIABLES INTERNES A L'INSTANT DU CALCUL PRECEDENT
C IN   TM     : TEMPERATURE L'INSTANT DU CALCUL PRECEDENT
C IN   TP     : TEMPERATURE A L'INSTANT DU
C IN  TREF    : TEMPERATURE DE REFERENCE
C OUT SIGP    : CONTRAINTES A L'INSTANT ACTUEL
C OUT VIP     : VARIABLES INTERNES A L'INSTANT ACTUEL
C OUT DSIDEP  : MATRICE CARREE (INUTILISE POUR RAPH_MECA)
C
C               ATTENTION LES TENSEURS ET MATRICES SONT RANGES DANS
C               L'ORDRE :  XX,YY,ZZ,SQRT(2)*XY,SQRT(2)*XZ,SQRT(2)*YZ
C
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C
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
C     ------------------------------------------------------------------
C     CALCUL DES MATRICES DE RIGIDITE POUR UN MATERIAU ORTHOTROPE
C     ------------------------------------------------------------------
C
C     ARGUMENTS
C
      INTEGER       IMATE,CODRET
      CHARACTER*8   TYPMOD(*)
      CHARACTER*16  COMPOR(*),OPTION
      REAL*8        EPSM(6),DEPS(6),SIGM(6),VIM(*)
      REAL*8        TM,TP,TREF,ANGMAS(3)
      REAL*8        SIGP(6),VIP(*),DSIDEP(6,6)
C
C     VARIABLES LOCALES
C
C     ALPH    : COEF DE DILATATION AU TEMPS PLUS
C     E       : YOUNG
C     ET      : MODULE ECROUISSAGE
C     SIGY    : LIMITE ELASTICITE INITIALE POU ISOT ET CINE
C     DSDEP1  : MODULE TANGENT DIRECTION 1 INSTANT PLUS
C     DSDEM1  : MODULE TANGENT DIRECTION 1 INSTANT MOINS
C     DSDE1   : MODULE TANGENT UTILISE SELON OPTION DIR 1
C     DSDEP2  : MODULE TANGENT DIRECTION 2 INSTANT PLUS
C     DSDEM2  : MODULE TANGENT DIRECTION 2 INSTANT MOINS
C
C     SIGL    : CONTRAINTES TEMPS PLUS REPERE LOCAL
C     DEPSL   : DEF TOTALES TEMPS PLUS - TEMPS MOINS  REPERE LOCAL
C     DEPSG   : DEF TOTALES TEMPS PLUS - TEMPS MOINS REPERE GLOBAL
C
C
      INTEGER       JGEOM,JCOQU,I,J
      INTEGER       NVARPI
      PARAMETER    ( NVARPI=8)
      INTEGER      NCSTPM
      PARAMETER    (NCSTPM=13)
      REAL*8        CSTPM(NCSTPM)
      REAL*8        DX , DY , DZ , S , C , NORM, RAC2
      REAL*8        PJDX , PJDY
      REAL*8        ALPHA ,BETA, R8DGRD, R8PREM, PI, PHI
      REAL*8        ALPH,E,ET,SIGY
      REAL*8        DSDEP1, DSDEP2,DSDEM1, DSDEM2,DSDE1
      REAL*8        SIGMG(3),SIGML(3)
      REAL*8        DH(3,3),SIGL(3),DEPSG(3),DEPSL(3),EPSMG(3),EPSML(3)
      REAL*8        PGL(3,3),ROT(3,3), XAB1(3,3)
      REAL*8        EM,EP,ALPHAM,ALPHAP
      CHARACTER*2  CODRES
      LOGICAL       CINE,ISOT,PINTO,COM1D,VECTEU
C
C     ------------------------------------------------------------------
      RAC2=SQRT(2.D0)
      ISOT = .FALSE.
      CINE = .FALSE.
      PINTO = .FALSE.
      COM1D=.FALSE.
      CODRET=0
C
      IF ( COMPOR(1)(1:16) .EQ. 'GRILLE_ISOT_LINE') THEN
          ISOT = .TRUE.
      ELSE IF ( COMPOR(1)(1:16) .EQ. 'GRILLE_CINE_LINE') THEN
          CINE = .TRUE.
      ELSE IF ( COMPOR(1)(1:16) .EQ. 'GRILLE_PINTO_MEN') THEN
          PINTO = .TRUE.
      ELSE
          COM1D=.TRUE.
          IF ((COMPOR(5)(1:7).NE.'DEBORST').AND.
     &        (COMPOR(1)(1:4).NE.'SANS')) THEN
                CALL UTMESS('F','BARRES','UTILISER ALGO_1D="DEBORST"'//
     &              ' SOUS COMP_INCR POUR LE COMPORTEMENT '//COMPOR(1))
          ENDIF
      ENDIF
C
C
C     -- 1 RECUPERATION DES CARACTERISTIQUES
C     ---------------------------------------
C
      CALL JEVECH ('PGEOMER' , 'L' , JGEOM)
      CALL DXTPGL ( ZR(JGEOM) , PGL )
      CALL JEVECH('PCACOQU','L',JCOQU)
      ALPHA = ZR(JCOQU+1) * R8DGRD()
      BETA  = ZR(JCOQU+2) * R8DGRD()
      DX = COS(BETA)*COS(ALPHA)
      DY = COS(BETA)*SIN(ALPHA)
      DZ = SIN(BETA)
      NORM = SQRT (DX*DX + DY*DY + DZ*DZ)
      DX = DX/NORM
      DY = DY/NORM
      DZ = DZ/NORM
C     ------------------------------------------------
      PJDX = DX*PGL(1,1) + DY*PGL(1,2) + DZ*PGL(1,3)
      PJDY = DX*PGL(2,1) + DY*PGL(2,2) + DZ*PGL(2,3)
      NORM = SQRT (PJDX*PJDX + PJDY*PJDY)
C     ------------------------------------------------
      IF ( NORM .LE. R8PREM() ) THEN
            CALL UTMESS('A','NMGRIL','L''AXE DE REFERENCE EST NORMAL A'
     &      //' UN ELEMENT DE PLAQUE. VOUS NE POURREZ CALCULER LES '
     &      //' LES CONTRAINTES.')
      ENDIF
C     ------------------------------------------------
C
      CALL GRIROT ( ALPHA , BETA ,PGL , ROT, C, S )
C
C     -- INITIALISATIONS
C     ---------------------------------------
      CALL R8INIR(9,0.D0,DH,1)
      CALL R8INIR(36,0.D0,DSIDEP,1)
      CALL R8INIR(3,0.D0,DEPSG ,1)
      CALL R8INIR(3,0.D0,DEPSL ,1)
      CALL R8INIR(3,0.D0,SIGL ,1)
      CALL R8INIR(4,0.D0,VIP  ,1)
C
C
C     DEFORMATIONS PLUS REPERE GLOBAL
C
      DO 1 I=1,2
         DEPSG(I) = DEPS(I)
         EPSMG(I) = EPSM(I)
         SIGMG(I)= SIGM(I)
  1   CONTINUE
      EPSMG(3)= EPSM(4)*RAC2
      DEPSG(3)= DEPS(4)*RAC2
      SIGMG(3)= SIGM(4)
C
      PI =  4.D0*ATAN(1.D0)
      PHI= 0.D0
      IF(ABS(C).GT.1.D-14) PHI= (ATAN(S/C)*180.D0/PI)-90.D0
C
C     CONTRAINTES MOINS REPERE LOCAL
C
      CALL INSCRF(SIGMG,PHI,SIGML)
C
      SIGML(2) = 0.D0
C
C     DEFORMATIONS REPERE LOCAL
C
      CALL INSDRF(EPSMG,PHI,EPSML)
      CALL INSDRF(DEPSG,PHI,DEPSL)
C
C     CALCUL DES CONTRAINTES PLUS DANS REPERE LOCAL
C     ET VARIABLES INTERNES
C

      CALL NMCO1D(IMATE,COMPOR,OPTION,
     &            EPSML(1),DEPSL(1),
     &            ANGMAS,
     &            SIGML(1),VIM(1),
     &            TM,TP,TREF,
     &            SIGL(1),VIP(1),DSDE1,CODRET)
      

        CALL NMMABA (IMATE,COMPOR,E,ALPH,ET,SIGY,
     &             NCSTPM,CSTPM)
        IF (COM1D) THEN
              SIGL(2) = 0.D0
        ELSE IF (CINE.OR.ISOT) THEN
              SIGL(2) = 0.D0
              VIP(3)  = 0.D0
              VIP(4)  = 0.D0
        ELSE IF (PINTO) THEN
              SIGL(2) = 0.D0
              DO 2 I = 1 , NVARPI
                  VIP(NVARPI+I)  = 0.D0
2             CONTINUE
        ENDIF

C
      SIGL(2) = 0.D0
      CALL R8INIR(6,0.D0,SIGP,1)
      CALL INSCRG ( SIGL , PHI , SIGP)
      SIGP(4)= SIGP(3)*RAC2
      SIGP(3)= 0.D0
C
      IF ( (OPTION(1:14) .EQ. 'RIGI_MECA_TANG').OR.
     >     ( OPTION(1:9)  .EQ. 'FULL_MECA' )) THEN
          DH(1,1) = DSDE1
          DH(2,2) = 0.D0
          DH(3,3) = 1.D-7*DSDE1
          CALL UTBTAB ('ZERO', 3 , 3 , DH , ROT , XAB1 , DH )
C
          DO 3 I=1,2
              DO 4 J=1,2
                  DSIDEP(I,J) = DH(I,J)
  4           CONTINUE
              DSIDEP(4,I) = DH(3,I)*RAC2
              DSIDEP(I,4) = DH(I,3)*RAC2
  3        CONTINUE
           DSIDEP(4,4) = DH(3,3)*2.D0
           DSIDEP(3,3) = 1.D-7*DSDE1
      ENDIF
      
      END
