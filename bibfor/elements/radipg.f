      SUBROUTINE RADIPG(SIG1,SIG2,NPG,NBSIG,RADIA,COSANG,
     &                  IND,COMPOR,IMATE,NVI,VARI1,VARI2)
      IMPLICIT NONE
      INTEGER NPG,NBSIG,IND,NVI,IMATE
      REAL*8 SIG1(*),SIG2(*),RADIA(*),COSANG(*)
      REAL*8 VARI1(*),VARI2(*)
      CHARACTER*16 COMPOR   
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 10/09/2012   AUTEUR PROIX J-M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C
C     BUT:
C       CALCUL DE L'INDICATEUR LOCAL DE PERTE DE RADIALITE RADIA
C       I = 1- ABS(SIG1:DSIGMA)/(NORME(SIG1)*NORME(DSIGMA)
C
C     ARGUMENTS:
C     ----------
C
C      ENTREE :
C-------------
C IN   SIG1     : CONTRAINTES INSTANT -
C IN   SIG2     : CONTRAINTES INSTANT +
C IN   NPG      : NOMBRE DE POINT DE GAUSS
C IN   NBSIG    : NOMBRE DE CMP DE CONTR
C IN   IND      : 0 : CALCUL DE RADI_V, 1 : CALCUL DE ERR_RADI
C IN   COMPOR   : COMPORTEMENT
C IN   IMATE    : ADRESSE MATERIAU CODE
C IN   NVI      : NOMBRE DE VARIABLES INTERNES
C IN   VARI1    : VARIABLES INTERNES INSTANT -
C IN   VARI2    : VARIABLES INTERNES INSTANT +
C
C      SORTIE :
C-------------
C OUT  RADIA    : INDICATEUR DE PERTE DE RADIALITE
C OUT  COSANG   : COSINUS DE L'ANGLE
C
C ......................................................................
C
      INTEGER MXCMEL
      PARAMETER (MXCMEL=162)

      INTEGER I,K,IGAU,ICINE,NBVAR,MEMO,VISC,IRADI

      REAL*8 DSIGMA(MXCMEL),ZERO,DEUX,S1DSIG,NORM,DNORM,NORSIG,MATEL(20)
      REAL*8 ZERNOR,R8PREM,TENSM(6),TENSP(6),INDM,INDP,XM(6),XP(6)
      REAL*8 COEF,CINF,C2INF,MAT(50)
C
C ----------------------------------------------------------------------
C
      ZERO = 0.0D0
      DEUX = 2.0D0
      ZERNOR = 10.0D0*R8PREM()
      
      IF (IND.EQ.0) THEN

C ----    CALCUL DE DSIGMA = SIG2 - SIG1 :
C         ----------------------------------
        K = 0
        DO 10 IGAU = 1,NPG
          DO 20 I = 1,NBSIG
            K = K + 1
            DSIGMA(K) = SIG2(K) - SIG1(K)

  20     CONTINUE
  10   CONTINUE

C ----    CALCUL DE L'INDICATEUR LOCAL DE PERTE DE RADIALITE
C ----    AUX POINTS D'INTEGRATION :
C         ------------------------
        DO 50 IGAU = 1,NPG

C ----       CALCUL DU PRODUIT SIG1:(SIG2-SIG1) :
C            ----------------------------------------
          S1DSIG = ZERO
          DO 30 I = 1,3
            S1DSIG = S1DSIG + SIG1(I+ (IGAU-1)*NBSIG)*
     &               DSIGMA(I+ (IGAU-1)*NBSIG)
  30      CONTINUE

          DO 40 I = 4,NBSIG
            S1DSIG = S1DSIG + DEUX*SIG1(I+ (IGAU-1)*NBSIG)*
     &               DSIGMA(I+ (IGAU-1)*NBSIG)
  40      CONTINUE

C ----       CALCUL DU SECOND INVARIANT DES TENSEURS DES CONTRAINTES :
C            -------------------------------------------------------
          NORM = NORSIG(SIG1(1+ (IGAU-1)*NBSIG),NBSIG)
          DNORM = NORSIG(DSIGMA(1+ (IGAU-1)*NBSIG),NBSIG)

C ----       DANS LE CAS OU NORME(SIG1) = 0  OU NORME(DSIGMA) = 0 :
C ----       ON MET L'INDICATEUR A 0 :
C            -----------------------
          IF (NORM.LE.ZERNOR .OR. DNORM.LE.ZERNOR) THEN
            RADIA(IGAU)  = ZERO
            COSANG(IGAU) = ZERO
          ELSE IF (DNORM.LE.1.0D4*R8PREM()*NORM) THEN
            RADIA(IGAU)  = ZERO
            COSANG(IGAU) = ZERO
          ELSE
            RADIA(IGAU) = 1.D0 - ABS(S1DSIG)/NORM/DNORM
            COSANG(IGAU) = S1DSIG/NORM/DNORM
          END IF
  50    CONTINUE
C
      ELSEIF (IND.EQ.1) THEN

         DO 51 IGAU = 1,NPG
         
            IRADI=0
            CALL DCOPY(NBSIG,SIG1(1+(IGAU-1)*NBSIG),1,TENSM,1)
            CALL DCOPY(NBSIG,SIG2(1+(IGAU-1)*NBSIG),1,TENSP,1)
            CALL DSCAL(NBSIG-3,SQRT(2.D0),TENSM(4),1)
            CALL DSCAL(NBSIG-3,SQRT(2.D0),TENSP(4),1)
            
C           ISOTROPE : LA NORMALE NE DEPEND QUE DE SIG
            IF ( (COMPOR.EQ.'VMIS_ISOT_TRAC')
     &       .OR.(COMPOR.EQ.'VMIS_ISOT_LINE')
     &       .OR.(COMPOR.EQ.'VMIS_ISOT_PUIS')) THEN
               INDM=VARI1((IGAU-1)*NVI+2)
               INDP=VARI2((IGAU-1)*NVI+2)
               ICINE=0
               IRADI=1
              
C           CINEMATIQUE : LA NORMALE DEPEND DE SIG ET X
            ELSEIF  ((COMPOR.EQ.'VMIS_ECMI_TRAC')
     &           .OR.(COMPOR.EQ.'VMIS_ECMI_LINE')) THEN
               CALL DCOPY(NBSIG,VARI1((IGAU-1)*NVI+3),1,XM,1)
               CALL DCOPY(NBSIG,VARI2((IGAU-1)*NVI+3),1,XP,1)
               INDM=VARI1((IGAU-1)*NVI+2)
               INDP=VARI2((IGAU-1)*NVI+2)
               ICINE=1
               IRADI=1
               CALL DSCAL(NBSIG-3,SQRT(2.D0),XM(4),1)
               CALL DSCAL(NBSIG-3,SQRT(2.D0),XP(4),1)

            ELSEIF((COMPOR.EQ.'VMIS_CINE_LINE')
     &          ) THEN
               CALL DCOPY(NBSIG,VARI1((IGAU-1)*NVI+1),1,XM,1)
               CALL DCOPY(NBSIG,VARI2((IGAU-1)*NVI+1),1,XP,1)
               INDM=VARI1((IGAU-1)*NVI+7)
               INDP=VARI2((IGAU-1)*NVI+7)
               ICINE=1
               IRADI=1
               CALL DSCAL(NBSIG-3,SQRT(2.D0),XM(4),1)
               CALL DSCAL(NBSIG-3,SQRT(2.D0),XP(4),1)

            ELSEIF((COMPOR.EQ.'VMIS_CIN1_CHAB')
     &        .OR. (COMPOR.EQ.'VISC_CIN1_CHAB')
     &        .OR. (COMPOR.EQ.'VMIS_CIN2_CHAB')
     &        .OR. (COMPOR.EQ.'VMIS_CIN2_MEMO')
     &        .OR. (COMPOR.EQ.'VISC_CIN2_CHAB')
     &        .OR. (COMPOR.EQ.'VISC_CIN2_MEMO')) THEN
               CALL NMCHAM('RIGI',IGAU,1,IMATE,COMPOR,
     &                MATEL,MAT,NBVAR,MEMO,VISC,COEF)
C              approximation : on supose C constant
               CINF   = MAT(4)/1.5D0
               INDM=VARI1((IGAU-1)*NVI+2)
               INDP=VARI2((IGAU-1)*NVI+2)
               CALL DCOPY(NBSIG,VARI1((IGAU-1)*NVI+3),1,XM,1)
               CALL DCOPY(NBSIG,VARI2((IGAU-1)*NVI+3),1,XP,1)
               CALL DSCAL(NBSIG,CINF,XM,1)
               CALL DSCAL(NBSIG,CINF,XP,1)
               IF (NBVAR.EQ.2) THEN
                 C2INF  = MAT(9)/1.5D0
                 CALL DAXPY(NBSIG,C2INF,VARI1((IGAU-1)*NVI+9),1,XM,1)
                 CALL DAXPY(NBSIG,C2INF,VARI2((IGAU-1)*NVI+9),1,XP,1)
               ENDIF
               ICINE=1
               IRADI=1
               CALL DSCAL(NBSIG-3,SQRT(2.D0),XM(4),1)
               CALL DSCAL(NBSIG-3,SQRT(2.D0),XP(4),1)

               
            ENDIF
            
C           CALCUL EFFECTUE UNIQUEMENT SI LE COMPORTEMENT LE PERMET
            IF (IRADI.EQ.1) THEN
               CALL RADIAL(NBSIG,TENSM,TENSP,INDM,INDP,ICINE,XM,XP,
     &                  RADIA(IGAU))
               COSANG(IGAU)=SQRT(ABS(1.D0-RADIA(IGAU)*RADIA(IGAU)))
            ELSE
               RADIA(IGAU)=0.D0
               COSANG(IGAU)=0.D0
            ENDIF
            
  51     CONTINUE

      ENDIF
      END
