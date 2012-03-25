      REAL*8 FUNCTION LCESRF(AM,WEPS,R,V,PREC,ITEMAX,IRET) 
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/03/2012   AUTEUR PROIX J-M.PROIX 
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
      IMPLICIT NONE      
      INTEGER ITEMAX,IRET
      REAL*8  WEPS,R,V,AM,PREC
C ----------------------------------------------------------------------
C   ENDO_SCALAIRE:       RESOLUTION DE -DRDA(A)*WEPS = V + R*A
C ----------------------------------------------------------------------
C  IN  AM      VALEUR DE L'ENDOMMAGEMENT EN T- (BORNE INF)
C  IN  WEPS    ENERGIE DE DEFORMATION ELASTIQUE
C  IN  R       PARAMETRE D'AUGMENTATION
C  IN  V       CSTE DU SECOND MEMBRE (K-PHI EN PRATIQUE)
C  IN  PREC    PRECISION : A ET A+PREC ENCADRENT LA SOLUTION
C  IN  ITEMAX  NOMBRE D'ITERATIONS MAXI AUTORISE
C  OUT IRET    CONVERGENCE (0=OK, 1=PB)
C  OUT ITER    NOMBRE D'ITERATIONS (POUR INFO)
C ----------------------------------------------------------------------
      INTEGER ITER
      REAL*8  AMIN,AMAX,AI,P2,P1P,P0,AN,CN,LN,SENS,PENTE,DA
      REAL*8  CAPP,AEST,ACVG,CCVG,LCVG
      REAL*8  LCESVF,UN
C ----------------------------------------------------------------------
      REAL*8 PK,PM,PP
      COMMON /LCES/ PK,PM,PP
C ----------------------------------------------------------------------

C    INITIALISATION
      IRET = 0
      UN   = 1.D0

C    INTERVALLE DE RECHERCHE
      AMIN = MAX(AM,-V/R)
      AMAX = MIN(UN,(PM*WEPS-V)/R)

C    POINT D'INFLEXION
      IF (PM-2*PP-3 .GE.0) THEN
        AI = -1
      ELSE
        P2  = (1+PM*PP)*(1+2*PP)
        P1P = 1+PM*PP
        P0  = PM-2*PP-3
        AI  = (-P1P+SQRT(P1P**2-P0*P2))/P2
      END IF

C    INITIALISATION NEWTON
      AN   = MAX(AMIN,AI)
      AN   = MIN(AMAX,AN)
      CN   = -WEPS*LCESVF(1,AN)
      LN   = V+R*AN
      SENS = SIGN(UN,CN-LN)
      
C    OPTIMISATION DE L'ESTIMATION INITIALE
      IF (SENS.GE.0) THEN
        CAPP  = PM*(1+PP)*(2+PM*(1+PP))
        P2    = R*CAPP + (2*PP+1)*PM*WEPS
        P1P   = (R+V*CAPP)/2 -PP*PM*WEPS
        P0    =  V - PM*WEPS
        AEST  =  (-P1P+SQRT(P1P**2-P0*P2))/P2
        IF (AEST.GT.AN ) THEN
          AN = AEST
          CN = -WEPS*LCESVF(1,AN)
          LN = V+R*AN
        END IF
      END IF
  
C    METHODE DE NEWTON
      DO 10 ITER = 1,ITEMAX
        PENTE = WEPS*LCESVF(2,AN)
        DA = (CN-LN)/(R+PENTE)
        AN = AN+DA

        IF (ABS(DA).LT.PREC) THEN
          ACVG = AN+SENS*PREC
          CCVG = -WEPS*LCESVF(1,ACVG)
          LCVG = V+R*ACVG
          IF (SENS*(LCVG-CCVG) .GE. 0) THEN
            LCESRF = AN
            GOTO 9999
          END IF
        END IF
  
        CN = -WEPS*LCESVF(1,AN)
        LN = V+R*AN
 10   CONTINUE
      IRET = 1

 9999 CONTINUE
      END
