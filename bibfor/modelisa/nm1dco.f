      SUBROUTINE NM1DCO(OPTION,IMATE,TM,TP,E,SIGM,EPSM,DEPS,VIM,
     &                  SIGP,VIP,DSDE,CORRM,CORRP)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 25/03/2004   AUTEUR OUGLOVA A.OUGLOVA 
C TOLE CRP_20
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C IN  CORRM   : CORROSION A L'INSTANT MOINS
C IN  CORRP   : CORROSION A L'INSTANT PLUS

C OUT SIGP     : CONTRAINTES PLUS
C OUT EPSP    : DEFORMATION  PLASTIQUE PLUS
C OUT P       : DEFORMATION  PLASTIQUE CUMULEE PLUS
C OUT DSDE    : DSIG/DEPS
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      REAL*8 TP,TM,EM,EP,ET,ALPHAM,ALPHAP,TREF
      REAL*8 SIGM,DEPS,PM,VIM(*),VIP(*),RESU,EPSPM,CORRM,CORRP
      REAL*8 SIGP,DSDE,RBID,RESI
      CHARACTER*16 OPTION
      INTEGER IMATE,IRET
C     ------------------------------------------------------------------
C     VARIABLES LOCALES
C     ------------------------------------------------------------------
      REAL*8 RPRIM,RM,SIGE,VALPAR,VALRES,DEPSTH,AIRERP,DUM
      REAL*8 SIELEQ,RP,DP,NU,EPSM
      INTEGER JPROLM,JVALEM,NBVALM,NBVALP,NBPAR,JPROLP,JVALEP
      CHARACTER*2 FB2,CODRES
      CHARACTER*8 NOMPAR,NOMRES,NOMECL(2),TYPE
      REAL*8  EPSIL0,D0,EPSP0,DEPSIL,P0,SIG0,
     1        ECR0,ECRF,DF,PF,EPSPF,SIGF,E,SY,DC,V,K,M,SF
C     DECLARATION DES TYPES DES VARIABLES:      
      REAL*8 EPSILF,EPSD,EPSC,D,P,EPSP,ECR,FPLAS,
     1    DFDS,DFPDS,DFDECR,DIFECR,LAMBP,FD,VAR1,TC
      REAL*8 VAR2,VAR3,RV,FINI,FDINI,FPLAS2,EPSMAX
      LOGICAL DCONV,PCONV
      INTEGER ITER,ITEMAX,ICHAR,NCHAR,I,J
      FB2 = 'FM'
      NBPAR = 1
      NOMPAR = 'TEMP'
      PM = VIM(1)
      EPSPM = VIM(1)
      D  = VIM(2)
 
C --- CARACTERISTIQUES ECROUISSAGE LINEAIRE
      VALPAR = TP
      CALL RCVALA(IMATE,'CORR_ACIER',NBPAR,NOMPAR,VALPAR,1,'D_CORR',
     &              DC,CODRES,FB2)
      CALL RCVALA(IMATE,'CORR_ACIER',NBPAR,NOMPAR,VALPAR,1,'ECRO_K',
     &              K,CODRES,FB2)
      CALL RCVALA(IMATE,'CORR_ACIER',NBPAR,NOMPAR,VALPAR,1,'ECRO_M',
     &              M,CODRES,FB2)
      CALL RCVALA(IMATE,'CORR_ACIER',NBPAR,NOMPAR,VALPAR,1,'SY',
     &              SY,CODRES,FB2)
      CALL RCVALA(IMATE,'ELAS',NBPAR,NOMPAR,VALPAR,1,'NU',
     &              V,CODRES,FB2)
     
C --- PARAMETRES DE CONVERGENCE
      CALL GETVR8('CONVERGENCE','RESI_INTE_RELA',1,1,1,RESI,IRET)
      CALL GETVIS('CONVERGENCE','ITER_INTE_MAXI',1,1,1,ITEMAX,IRET)

        TC = CORRM
        IF (TC .LE. 15.D0)  THEN
         EPSC = 2.345D-1-(1.11D-2*TC)
        ELSE 
        EPSC = 5.1D-2-(6.D-4*TC)
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
      IF (OPTION.EQ.'FULL_MECA' .OR. OPTION.EQ.'RAPH_MECA') THEN

      ITER = 0
      DO 30 I = 1,ITEMAX 
      IF (.NOT. DCONV) THEN   
        ITER = ITER+1
        
C    *******ELASTICITE********************
         SIGP = E*(EPSILF-EPSP)
         ECR = K*(P**(1.D0/M))
         FINI = ((SIGP/(1.D0-D))-ECR-SY)
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
            FPLAS = ((SIGP/(1.D0-D))-ECR-SY)
            PCONV = ((ABS(FPLAS/FINI) .LE. RESI)
     &                .OR. (LAMBP.LE.RESI))
          END IF
          GOTO 141
   40   CONTINUE
        IF(J .GE. ITEMAX)
     &    CALL UTMESS('F','NM1DCO','ABSENCE DE CONVERGENCE')
        ENDIF
C
  141  CONTINUE 
        END IF
        
C    *****ENDOMMAGEMENT*********************
        FD = EPSP-EPSD
        IF (FD .LE. 0.D0) THEN
          DCONV = .TRUE.
        ELSE
         D = (DC*((RV*EPSP)-EPSD))/(EPSC-EPSD)
         FPLAS2 = ((SIGP/(1.D0-D))-ECR-SY)
         IF (ITER .EQ. 1) THEN
            FDINI = FPLAS2
         ELSE
           FD = FPLAS2
           DCONV = ((ABS(FD/FDINI) .LE. RESI)
     1             .OR. (ITER .LE. ITEMAX))
           IF (ITER .EQ. ITEMAX) THEN
             CALL UTMESS('F','NM1DCO','ABSENCE DE CONVERGENCE')
           END IF
         END IF
       END IF
       IF (D .GT. 0.99D0)THEN
         DCONV = .TRUE.
         SIGP = 0.D0
       ENDIF
       GOTO 142
   30   CONTINUE
        IF(I .GE. ITEMAX)
     &    CALL UTMESS('F','NM1DCO','ABSENCE DE CONVERGENCE')
        ENDIF

  142  CONTINUE
      VIP(1) = P
      VIP(2) = D
      IF ((OPTION.EQ.'RIGI_MECA_TANG').OR.
     &    (OPTION.EQ.'FULL_MECA')) THEN
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

       END IF
      
      END
