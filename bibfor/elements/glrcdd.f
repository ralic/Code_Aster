      SUBROUTINE GLRCDD(ZIMAT,MAXMP,MINMP,MATR,EP,SURFGP,Q,SIG,
     &                  EPST,DEPS,DSIG,ECR,DELAS,DSIDEP,NORMM,NORMN)
        IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 06/05/2008   AUTEUR MARKOVIC D.MARKOVIC 
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
      REAL*8   MATR(*),MAXMP(*),MINMP(*),NORMM,NORMN
      INTEGER  ZIMAT
      REAL*8   EP, SURFGP, Q(2,2), SIG(*), DEPS(*), EPST(*)
      REAL*8   DSIG(*),T1VE(9)
      REAL*8   ECR(*),DSIDEP(6,*),DELAS(6,*)

      REAL*8   D66(6,6),C1(6,6),C2(6,6),DCLOC(6),DCGLO(6)        

      REAL*8   TQ(2,2),
     &      DEPSLO(6),      DEPSOR(6),            DPSPOR(6),
     &      DFLOC(6),        DFORTH(6),
     &      NBACOR(6),   EP2S6,            DDISS,
     &      CURLOC(3),      CURORT(3),
     &     ALPHA, BETA, GAMMA, DMAX1, DMAX2, K1, K2, E, NU, QP1,QP2,
     &     MF1,MF2,DAM1,DAM2,CUVCUP(3)

      INTEGER     I, J, TEST,II
      REAL*8      DSSP,PQT(3,3),RQ(3,3),RPARA(5)

C     ------------------------------------------------------------
      SAVE  D66, C1, C2,ALPHA, BETA, GAMMA, DMAX1, DMAX2, K1, K2
      
        CALL R8INIR(6*6,0.0D0,D66,1)
        D66(1,1) = MATR(2)
        D66(1,2) = MATR(3)
        D66(2,1) = MATR(3)
        D66(2,2) = MATR(4)
        D66(3,3) = MATR(5)

        E   = MATR(6)      
        NU  = MATR(7)     
        MF1 = MATR(8)    
        MF2 = MATR(9)    
        QP1 = MATR(10)   
        QP2 = MATR(11)   
C-------- material parameters stored for damage computation
        ALPHA = NU*E*EP**3/24/(1-NU**2)   
        BETA = E*EP**3/24.0D0/(1.0D0 + NU)        
        GAMMA = MATR(12)

        IF (MF1  .GT.  0.D0) THEN
          DMAX1 = (1.D0-QP1)/(QP1-GAMMA)
          DMAX2 = (1.D0-QP2)/(QP2-GAMMA)
          K1 = (1.D0-GAMMA)/4*MF1**2/(ALPHA+BETA)
          K2 = (1.D0-GAMMA)/4*MF2**2/(ALPHA+BETA)
        ELSE    
          DMAX1 = (1.D0-QP2)/(QP2-GAMMA)
          DMAX2 = (1.D0-QP1)/(QP1-GAMMA)
          K1 = (1.D0-GAMMA)/4*MF2**2/(ALPHA+BETA)
          K2 = (1.D0-GAMMA)/4*MF1**2/(ALPHA+BETA)
        ENDIF
        
        CALL R8INIR(6*6,0.0D0,C1,1)
        CALL R8INIR(6*6,0.0D0,C2,1)
        DO 10, I = 1,6
           C1(I,I) = MATR(15+I)
           C2(I,I) = MATR(21+I)
 10     CONTINUE

      DO 70, J = 1,2
        DO 60, I = 1,2
          TQ(I,J) = Q(J,I)
 60   CONTINUE
 70   CONTINUE

C      -----------------------------------------------
C      strain in the orthotropic axes
C      -----------------------------------------------
      DO 80, J = 1,6
        DEPSLO(J) = DEPS(J) 
 80   CONTINUE
C   ATTENTION: DEPS(3)    = 2*EPS_XY
C   ATTENTION: DEPS(6)    = 2*KAPPA_XY
      DEPSLO(3) = DEPS(3) / 2.D0          
      DEPSLO(6) = DEPS(6) / 2.D0          

      CALL MULTSY(TQ,DEPSLO,Q,DEPSOR)
      CALL MULTSY(TQ,DEPSLO(4),Q,DEPSOR(4))

      DEPSOR(3)= 2.D0*DEPSOR(3)       
      DEPSOR(6) = 2.D0* DEPSOR(6)     

      DO 100, I = 1,3
        CURLOC(I) = EPST(I+3) 
 100  CONTINUE       
      CURLOC(3) = EPST(6) / 2.D0         

      CALL  MULTSY(TQ,CURLOC,Q,CURORT) 
      CURORT(3) = 2.D0* CURORT(3)   

C *----------------------------------------------------------------
C *     bending : elastoplasticity with kinematic softening
C *----------------------------------------------------------------

      EP2S6 = EP*EP / 6.D0
      DO 110, J = 1,6      
        NBACOR(J) = ECR(J+13)      
 110  CONTINUE
C       NBACOR(1:3) = ep * multsym(tq,sig(1),q)
C       NBACOR(4:6) = ecr(17:19)    ! internal variable
C 
C      NBACOR  = | n - backn | 
C                     | m - backm |  
C 
      DAM1 = ECR(8) * DMAX1
      DAM2 = ECR(9) * DMAX2
      DO 120, J = 1,3      
        CUVCUP(J) = CURORT(J) - ECR(J+3)
 120  CONTINUE

      RPARA(1) = ALPHA
      RPARA(2) = BETA
      RPARA(3) = GAMMA
      RPARA(4) = K1
      RPARA(5) = K2
      CALL GLRCAD (ZIMAT,MAXMP,MINMP,DELAS,RPARA,DMAX1,
     &           DMAX2,DAM1,DAM2,CUVCUP, C1,C2,NBACOR, DEPSOR, DPSPOR,
     &           DFORTH, DDISS,DSIDEP,NORMM,NORMN)

      T1VE(1) = Q(1,1)*Q(1,1)
      T1VE(4) = Q(2,1)*Q(2,1)
      T1VE(7) = Q(1,1)*Q(2,1)
      T1VE(2) =  T1VE(4)
      T1VE(5) =  T1VE(1)
      T1VE(8) = -T1VE(7)
      T1VE(3) = -T1VE(7) - T1VE(7)
      T1VE(6) =  T1VE(7) + T1VE(7)
      T1VE(9) =  T1VE(1) - T1VE(4)
        
      CALL ORTLOC(DSIDEP,0,0,T1VE)
      CALL ORTLOC(DSIDEP,3,3,T1VE)
      CALL ORTLOC(DSIDEP,0,3,T1VE)
      
      DO 127 I =1,3
        DO 125 J = 4,6
           DSIDEP(J,I)=DSIDEP(I,J)
 125    CONTINUE
 127  CONTINUE      
C     Forces increment
C      DFORTH=matmul(D66,DEPSOR - depsp_orth)
C
C     Transfer in the local axes
      CALL MULTSY(Q,DFORTH,TQ,DFLOC)
      CALL MULTSY(Q,DFORTH(4),TQ,DFLOC(4))

      DO 130, J = 1,3
        DSIG(J) = DFLOC(J)  / EP 
        DSIG(J+3) = DFLOC(J+3)  / EP2S6 
 130  CONTINUE
      DO 140, J = 1,6
        ECR(J) = ECR(J) + DPSPOR(J)
 140  CONTINUE

C     Energy dissipated during plasticity
      ECR(7) = ECR(7) + SURFGP*DDISS

C     Energy dissipated during damage
       ECR(10) = ECR(10) + SURFGP*( K1*(DAM1-ECR(8)*DMAX1)
     &                            +K2*(DAM2-ECR(9)*DMAX2)) 

      ECR(8) = DAM1 / DMAX1
      ECR(9) = DAM2 / DMAX2
      DO 150, J = 1,6
        ECR(J+13) = NBACOR(J)
 150  CONTINUE
 
      END 
