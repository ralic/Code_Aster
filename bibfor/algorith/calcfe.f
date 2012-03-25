      SUBROUTINE CALCFE(NR,NDT,NVI,VIND,DF,GAMSNS,FE,FP,IRET)
      IMPLICIT NONE
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
C       ----------------------------------------------------------------
C       MONOCRISTAL : CALCUL DE Fe et Fp, F=Fe.Fp
C       IN  NR     :  DIMENSION DECLAREE DRDY
C           NDT    :  NOMBRE DE COMPOSANTES DE SIGMA (6)
C           NVI    :  NOMBRE DE VARIABLES INTERNES
C           VIND   :  VARIABLES INTERNES A L'INSTANT PRECEDENT
C                     contiennent Fe(t), Fp(t)
C           DF     :  Increment de Gradient de deformation 
C           GAMSNS :  Somme de dGamma.Ms*Ns
C       OUT FE     :  Gradient de tranformation elastique
C           FP     :  Gradient de tranformation plastique
C
      REAL*8 FE(3,3),DF(3,3),GAMSNS(3,3),DFFE(3,3),DFPM(3,3),FEM(3,3)
      REAL*8 VIND(*),ID(3,3),DET,COEF,DFP(3,3),EXPO,FP(3,3)
      REAL*8 FPM(3,3),DFPMAX,DFPMIN,DET2,R8PREM
      INTEGER NR, NDT, IRET, IOPT, I, NVI
      DATA ID/1.D0,0.D0,0.D0, 0.D0,1.D0,0.D0, 0.D0,0.D0,1.D0/
C     ----------------------------------------------------------------
      
      IRET=0
      IOPT=2

      CALL DCOPY(9,VIND(NVI-3-18+10),1,FEM,1)
      CALL DCOPY(9,VIND(NVI-3-18+1 ),1,FPM,1)      
      CALL DAXPY(9,1.D0,ID,1,FEM,1)
      CALL DAXPY(9,1.D0,ID,1,FPM,1)
      
      CALL PMAT(3,DF,FEM,DFFE)
      
      CALL DCOPY(9,GAMSNS,1,DFP,1)
      
      IF (IOPT.EQ.1) THEN
      
C        suivant ANNAND 1996 

         CALL DAXPY(9,1.D0,ID,1,DFP,1)
         
C        TEST ANALOGUE A SIMO_MIEHE NMGPFI
         DFPMAX=0.D0
         DFPMIN=100.D0
         DO 10 I=1,3
            IF (DFP(I,I).GT.DFPMAX) DFPMAX=DFP(I,I)
            IF (DFP(I,I).LT.DFPMIN) DFPMIN=DFP(I,I)
 10      CONTINUE
         IF ((DFPMAX.GT.1.D3).OR.(DFPMIN.LT.1.D-3)) THEN
           IRET=1
           GOTO 9999
         ENDIF

         CALL LCDETF(3,DFP,DET)
         
         IF (DET.GT.R8PREM()) THEN
            EXPO=-1.D0/3.D0
            COEF=DET**EXPO
            CALL DSCAL(9,COEF,DFP,1)
         ELSE
            IRET=1
            GOTO 9999
         ENDIF 
         
         CALL MATINV('S',3,DFP,DFPM,DET2)

      ELSEIF (IOPT.EQ.2) THEN
      
C        linearisation directe de exp(-dgamma.ms x ns) 

         CALL DCOPY(9,DFP,1,DFPM,1)
         CALL DSCAL(9,-1.D0,DFPM,1)
         CALL DAXPY(9,1.D0,ID,1,DFPM,1)
         
         DFPMAX=0.D0
         DFPMIN=100.D0
         DO 20 I=1,3
            IF (DFPM(I,I).GT.DFPMAX) DFPMAX=DFPM(I,I)
            IF (DFPM(I,I).LT.DFPMIN) DFPMIN=DFPM(I,I)
 20      CONTINUE
         IF ((DFPMAX.GT.1.D3).OR.(DFPMIN.LT.1.D-3)) THEN
           IRET=1
           GOTO 9999
         ENDIF

         CALL LCDETF(3,DFPM,DET)
         
         IF (DET.GT.R8PREM()) THEN
            EXPO=1.D0/3.D0
            COEF=DET**EXPO
            CALL DSCAL(9,COEF,DFPM,1)
         ELSE
            IRET=1
            GOTO 9999
         ENDIF 
         
         CALL MATINV('S',3,DFPM,DFP,DET2)

         
      ELSEIF (IOPT.EQ.3) THEN
      
C suivant DE SOUZA-NIETO
      
C         DFPMAX=0.D0
C         DFPMIN=100.D0
C         DO 30 I=1,3
C            IF (DFP(I,I).GT.DFPMAX) DFPMAX=DFP(I,I)
C            IF (DFP(I,I).LT.DFPMIN) DFPMIN=DFP(I,I)
C 30      CONTINUE
C         IF ((ABS(DFPMAX).GT.10.D0).OR.(ABS(DFPMIN).GT.10.D0)) THEN
C           IRET=1
C           GOTO 9999
C         ENDIF
C         CALL DSCAL(9,-1.0D0,DFP,1)
C         CALL EXPMAP(DFPM,NOCONV,DFP)
C         IF (NOCONV) THEN
C            IRET=1
C            GOTO 9999
C         ENDIF
C         CALL MATINV('S',3,DFPM,DFP,DET2)
         
      ENDIF
      
      CALL PMAT(3,DFFE,DFPM,FE)
      
C post traitement

      CALL PMAT(3,DFP,FPM,FP)
      
 9999 CONTINUE      
 
      END
