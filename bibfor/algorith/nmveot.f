       SUBROUTINE NMVEOT(DRBDB, DRBDP, DRPDB, DRPDP,
     &                   DRBDE, DRPDE, DSGDE, DSGDB, DSGDP,
     &                   NP, NB, NR, DSIDEP)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 25/07/2001   AUTEUR RATEAU G.RATEAU 
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
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER            NP, NB, NR
       REAL*8            DRBDB(NB,NB), DRBDP(NB,NP)
       REAL*8            DRPDP(NP,NP), DRPDB(NP,NB)
      REAL*8             DRBDE(NB,NB), DRPDE(NP,NB)
      REAL*8             DSGDB(NB,NB), DSGDP(NB,NP), DSGDE(NB,NB)
      REAL*8             DSIDEP(NB,NB)
C ----------------------------------------------------------------------
C     INTEGRATION DE LA LOI DE COMPORTEMENT VISCO PLASTIQUE DE
C     CHABOCHE AVEC ENDOMAGEMENT
C     METHODE ITERATIVE D'EULER IMPLICITE
C
C     CALCUL DE L'OPERATEUR TANGENT DSIDEP(6,6)
C ----------------------------------------------------------------------
      INTEGER     NMOD
      PARAMETER  (NMOD = 25)
      INTEGER     I, J, K 
      REAL*8      A(6,6), B(6,6), R(NMOD,NMOD), DPDE(2,6), DBDE(6,6)
      REAL*8      DRDY(NMOD,NMOD), MUN,ZERO
      PARAMETER  (MUN = -1.D0)
      LOGICAL    FAUX

      FAUX = .FALSE.
      ZERO = 0.D0
C
C ----------------------------------------------------------------------
C-- 1.1. INITIALISATION DE L OPERATEUR LINEAIRE DU SYSTEME 
C                     DRDY = ( DRBDB, DRBDP )
C                            ( DRPDB, DRPDP )
C
      CALL R8INIR ( NMOD*NMOD,0.D0,DRDY,1)
      CALL LCICMA ( DRBDB,NB,NB,NB,NB,1,1,DRDY,NMOD,NMOD,1,1)
      CALL LCICMA ( DRBDP,NB,NP,NB,NP,1,1,DRDY,NMOD,NMOD,1,NB+1)
      CALL LCICMA ( DRPDB,NP,NB,NP,NB,1,1,DRDY,NMOD,NMOD,NB+1,1)
      CALL LCICMA ( DRPDP,NP,NP,NP,NP,1,1,DRDY,NMOD,NMOD,NB+1,NB+1)
C-- 1.2. INITIALISATION R = ( -DRBDE , -DRPDE )
C
      CALL R8INIR ( NMOD*NMOD,0.D0,R,1)
      CALL LCICMA ( DRBDE,NB,NB,NB,NB,1,1,R,NMOD,NMOD,1,1)
      CALL LCICMA ( DRPDE,NP,NB,NP,NB,1,1,R,NMOD,NMOD,NB+1,1)
      DO 00121 I = 1 , NMOD
       DO 00121 J = 1 , NMOD
        R(I,J) = MUN * R(I,J)
00121 CONTINUE
C
C-- 2. CALCUL DE DBDE ET DPDE
      CALL MGAUSS (DRDY , R , NMOD , NR , NB, ZERO, FAUX)
      CALL LCICMA (R, NMOD, NMOD, NB, NB, 1, 1, DBDE, NB, NB, 1, 1)
      CALL LCICMA (R, NMOD, NMOD, NP, NB, NB+1, 1, DPDE, NP, NB, 1, 1)
C
C-- 3. CALCUL DE L'OPERATEUR
C-- 3.1. INITIALISATION
      CALL R8INIR ( NB*NB,0.D0,A,1)
      CALL R8INIR ( NB*NB,0.D0,B,1)
C-- 3.2. CALCUL
      DO 00300 I= 1 , NB
      DO 00300 J= 1 , NB
         DO 00300 K= 1 , NB
               A(I,J) = A(I,J) + DSGDB(I,K) * DBDE(K,J)
00300 CONTINUE
C
      DO 00305 I= 1 , NB
      DO 00305 J= 1 , NB
         DO 00305 K= 1 , NP
               B(I,J) = B(I,J) + DSGDP(I,K) * DPDE(K,J)
00305 CONTINUE
C
      DO 00310 I= 1, NB
        DO 00320 J= 1, NB
          DSIDEP(I,J) = A(I,J) + B(I,J) + DSGDE(I,J)
00320   CONTINUE
00310 CONTINUE
C
      END
