      SUBROUTINE NM1DCO(FAMI,KPG,KSP,OPTION,IMATE,MATERI,E,SIGM,
     &                  EPSM,DEPS,VIM,SIGP,VIP,DSDE,CRILDC,CODRET)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 02/05/2011   AUTEUR DELMAS J.DELMAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.

C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.

C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C ----------------------------------------------------------------------

      IMPLICIT NONE
C ----------------------------------------------------------------------
C          PLASTICITE VON MISES ISOTROPE BILINEAIRE MONODIM
C          ON PEUT AVOIR T0 DIFF TREF


C IN  T        : TEMPERATURE PLUS
C IN  TM       : TEMPERATURE MOINS
C IN  E        : MODULE D EG
C IN  ET       : PENTE D ECROUISSAGE
C IN  ALPH     : COEF DILAT THERMIQUE
C IN  SY       : LIMITE D ELASTICITE INITIALE

C IN  SIGM    : CONTRAINTE AU TEMPS MOINS
C               UTILISE UNIQUEMENT POUR EVALUER DSDEM
C IN  DEPS    : DEFORMATION  TOTALE PLUS - DEFORMATION TOTALE MOINS
C IN  EPSPM   : DEFORMATION  PLASTIQUE MOINS
C IN  PM      : DEFORMATION  PLASTIQUE CUMULEE MOINS

C OUT SIGP     : CONTRAINTES PLUS
C OUT EPSP    : DEFORMATION  PLASTIQUE PLUS
C OUT P       : DEFORMATION  PLASTIQUE CUMULEE PLUS
C OUT DSDE    : DSIG/DEPS
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      REAL*8 SIGM,DEPS,PM,VIM(*),VIP(*),EPSPM,CORRM
      REAL*8 SIGP,DSDE,RESI,CRILDC(*)
      CHARACTER*16 OPTION
      CHARACTER*(*) FAMI,MATERI
      INTEGER IMATE,CODRET,KPG,KSP
C     ------------------------------------------------------------------
C     VARIABLES LOCALES
C     ------------------------------------------------------------------
      REAL*8 EPSM
      INTEGER CODRES
      REAL*8 E,SY,DC,V,K,M
      REAL*8 EPSILF,EPSD,EPSC,D,P,EPSP,ECR,FPLAS
      REAL*8 DFDS,DFPDS,DFDECR,DIFECR,LAMBP,FD,VAR1
      REAL*8 VAR2,VAR3,RV,FINI,FDINI,FPLAS2
      LOGICAL DCONV,PCONV,MELAS
      INTEGER ITER,ITEMAX,I,J,IBID
      PM = VIM(1)
      EPSPM = VIM(1)
      D  = VIM(2)
      CODRET=0

C --- CARACTERISTIQUES ECROUISSAGE LINEAIRE
      CALL RCVALB(FAMI,KPG,KSP,'+',IMATE,MATERI,'CORR_ACIER',0,' ',
     &            0.D0,1,'D_CORR',DC,CODRES,1)
      CALL RCVALB(FAMI,KPG,KSP,'+',IMATE,MATERI,'CORR_ACIER',0,' ',
     &            0.D0,1,'ECRO_K',K,CODRES,1)
      CALL RCVALB(FAMI,KPG,KSP,'+',IMATE,MATERI,'CORR_ACIER',0,' ',
     &            0.D0,1,'ECRO_M',M,CODRES,1)
      CALL RCVALB(FAMI,KPG,KSP,'+',IMATE,MATERI,'CORR_ACIER',0,' ',
     &            0.D0,1,'SY',SY,CODRES,1)
      CALL RCVALB(FAMI,KPG,KSP,'+',IMATE,MATERI,'ELAS',0,' ',0.D0,
     &            1,'NU',V,CODRES,1)

C --- PARAMETRES DE CONVERGENCE
      RESI = CRILDC(3)
      ITEMAX = NINT(CRILDC(1))

        CALL RCVARC('F','CORR','-',FAMI,KPG,KSP,CORRM,IBID)
        IF (CORRM .LE. 15.D0)  THEN
         EPSC = 2.345D-1-(1.11D-2*CORRM)
        ELSE
        EPSC = 5.1D-2-(6.D-4*CORRM)
        END IF
C       END IF
C
C    DEFORMATION PLASTIQUE DE DEBUT D'ENDOMMAGMENT
      EPSD = 0.8D0*EPSC
C    RV LE PARAMETRE QUI DEPEND DU TAUX DE TRIAXIALITE
      VAR1 = 1.D0+V
      VAR2 = 1.D0-(2.D0*V)
      VAR3 = ((1.D0/3.D0)**2.D0)
      RV = (((2.D0/3.D0)*VAR1)+(3.D0*VAR2*VAR3))
      EPSILF = EPSM+DEPS
      EPSP = EPSPM
      P = PM
      SIGP=SIGM
      DCONV=.FALSE.
      MELAS=(OPTION.EQ.'RIGI_MECA_ELAS').OR.
     &      (OPTION.EQ.'FULL_MECA_ELAS')
      IF (OPTION.EQ.'FULL_MECA' .OR. OPTION.EQ.'RAPH_MECA') THEN

      ITER = 0
      DO 30 I = 1,ITEMAX
      IF (.NOT. DCONV) THEN
        ITER = ITER+1

C    *******ELASTICITE********************
         SIGP = E*(EPSILF-EPSP)
CJMP         SIGP =(1.D0-D)* E*(EPSILF-EPSP)

         ECR = K*(P**(1.D0/M))
         FINI = ((ABS(SIGP)/(1.D0-D))-ECR-SY)
         FPLAS = FINI
         IF (FINI .LE. 0.D0) THEN
          VIP(3) = 0.D0
          DCONV = .TRUE.
        ELSE
          VIP(3) = 1.D0
          PCONV = .FALSE.

C    ******PLASTICITE**********************
         DO 40 J = 1,ITEMAX
         IF (.NOT. PCONV)  THEN
            DFDS = (1.D0/(1.D0-D))
            DFPDS = (1.D0/(1.D0-D))
            DFDECR = -1.D0
            DIFECR = ((K/M)*((SIGP/((1.D0-D)*K))-(SY/K))**(1.D0-M))
            LAMBP = (FPLAS/((DFDS*E*DFPDS)-(DFDECR*DIFECR)))
            EPSP = EPSP+LAMBP*DFPDS
            P = P+(LAMBP/(1.D0-D))
            SIGP = SIGP-((E*LAMBP)/(1.D0-D))
             ECR = K*(P**(1.D0/M))
            FPLAS = ((ABS(SIGP)/(1.D0-D))-ECR-SY)
            PCONV = ((ABS(FPLAS/FINI) .LE. RESI)
     &                .OR. (LAMBP.LE.RESI))
          ELSE
             GOTO 141
         END IF
   40    CONTINUE
  141    CONTINUE
          IF(J .GE. ITEMAX) THEN
             CALL U2MESS('I','MODELISA5_40')
             CODRET=1
             GOTO 9999
          ENDIF
        ENDIF
C
        END IF

C    *****ENDOMMAGEMENT*********************
        FD = EPSP-EPSD
        IF (FD .LE. 0.D0) THEN
          DCONV = .TRUE.

          GOTO 142

        ELSE
         D = (DC*((RV*EPSP)-EPSD))/(EPSC-EPSD)
         FPLAS2 = ((ABS(SIGP)/(1.D0-D))-ECR-SY)
         IF (ITER .EQ. 1) THEN
            FDINI = FPLAS2
         ELSE
           FD = FPLAS2
           DCONV = (ABS(FD/FDINI) .LE. RESI)
           IF (DCONV) GOTO 142
         END IF
        END IF
        IF (D .GT. 0.99D0)THEN
            DCONV = .TRUE.
            SIGP = 0.D0
            GOTO 142
        ENDIF
   30   CONTINUE
  142   CONTINUE
        IF(I .GE. ITEMAX) THEN
             CALL U2MESS('I','MODELISA5_41')
             CODRET=1
             GOTO 9999
          ENDIF
        ENDIF

      VIP(1) = P
      VIP(2) = D
C      IF ((OPTION.EQ.'RIGI_MECA_TANG').OR.
C     &    (OPTION.EQ.'FULL_MECA')) THEN
        IF (VIM(3).LT.0.5D0) THEN
          DSDE = E
        ELSE
          IF (VIM(2).GT.0.D0) THEN
          IF (OPTION.EQ.'RIGI_MECA_TANG') THEN
             P = VIM(1)
             D = VIM(2)
             DSDE = (((1.D0-D)*(K*(1.D0/M)*(P**((1.D0/M)-1.D0))))/
     &   (1.D0+((1.D0-D)*((K*(1.D0/M))/E)*(P**((1.D0/M)-1.D0)))))
            ELSE
             DSDE = ((1.D0-D)*K*(1.D0/M)*(P**((1.D0/M)-1.D0)))
            ENDIF
          ELSE
             IF (OPTION.EQ.'RIGI_MECA_TANG') THEN
               P = VIM(1)
               DSDE = ((K*(1.D0/M)*(P**((1.D0/M)-1.D0)))/
     &    (1.D0+(((K*(1.D0/M))/E)*(P**((1.D0/M)-1.D0)))))
             ELSE
              DSDE = (K*(1.D0/M)*(P**((1.D0/M)-1.D0)))
             END IF
          END IF
        END IF
C     CAS RIGI_MECA_ELAS ET FULL_MECA_ELAS AVEC ENDOMMAGEMENT
C      END IF
      IF(MELAS)  DSDE=(1.D0-D)*DSDE

 9999 CONTINUE
      END
