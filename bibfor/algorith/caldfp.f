      SUBROUTINE CALDFP(MSNS,GAMSNS,DFPMDG,IRET)
      IMPLICIT NONE
      
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 11/07/2011   AUTEUR PROIX J-M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE PROIX J-M.PROIX
C     ----------------------------------------------------------------
C     
C     MONOCRISTAL : calcul des derivees de Fe en GDEF      
C     IN  MSNS   : MS * NS
C         GAMSNS : somme des dGamma.Ms*Ns
C     OUT DFPMDG : dFp/dGamma_S
C         IRET   :  CODE RETOUR

C    Pour un seul systeme on calcule DF.FEn.d(Fp-1)/dGammaS 

      INTEGER IRET, IOPT, I, J, K, L
      REAL*8 MSNS(3,3),GAMSNS(3,3),DDETDG
      REAL*8 ID(3,3),COEF,EXPO,DDOT
      REAL*8 DET2,R8PREM,DFPMDG(3,3)
      REAL*8 DFPDG(3,3),DFPMDF(3,3,3,3),AMAX,AMIN,BMAX,BMIN
      REAL*8 A(3,3),AM(3,3),AMT(3,3),DETA,COEF2
      REAL*8 B(3,3),BM(3,3),BMT(3,3),DETB
      DATA ID/1.D0,0.D0,0.D0, 0.D0,1.D0,0.D0, 0.D0,0.D0,1.D0/
C     ----------------------------------------------------------------

      IRET=0
      IOPT=2

      IF (IOPT.EQ.1) THEN
      
C        calcul de dFp/dGamma suivant ANNAND 1996 

         CALL DCOPY(9,GAMSNS,1,A,1)
      
         CALL DAXPY(9,1.D0,ID,1,A,1)
         
C        TEST ANALOGUE A SIMO_MIEHE NMGPFI
         AMAX=0.D0
         AMIN=100.D0
         DO 10 I=1,3
            IF (A(I,I).GT.AMAX) AMAX=A(I,I)
            IF (A(I,I).LT.AMIN) AMIN=A(I,I)
 10      CONTINUE
         IF ((AMAX.GT.1.D3).OR.(AMIN.LT.1.D-3)) THEN
           IRET=1
           GOTO 9999
         ENDIF

         CALL LCDETF(3,A,DETA)
         
         IF (DETA.GT.R8PREM()) THEN
            EXPO=-1.D0/3.D0
            COEF=DETA**EXPO
         ELSE
            IRET=1
            GOTO 9999
         ENDIF 
         
         CALL MATINV('S',3,A,AM,DET2)
         CALL LCTR2M(3,AM,AMT)
         
         DDETDG = DDOT(9,AMT,1,MSNS,1)
         
         CALL DSCAL(9,DDETDG,A,1)
         
         CALL DCOPY(9,A,1,DFPDG,1)

         CALL DSCAL(9,-1.D0/3.D0,DFPDG,1)
         
         CALL DAXPY(9,1.D0,MSNS,1,DFPDG,1)
         
         CALL DSCAL(9,COEF,DFPDG,1)
         
C calcul de dFp-1
         CALL R8INIR ( 81, 0.D0 , DFPMDF, 1 )
         DO 100 I=1,3
         DO 100 J=1,3
         DO 100 K=1,3
         DO 100 L=1,3
            DFPMDF(I,J,K,L)=DFPMDF(I,J,K,L)+AM(I,K)*AMT(J,L)
 100     CONTINUE
         COEF2= -DETA**(2.D0/3.D0)
         
         CALL DSCAL(81,COEF2,DFPMDF,1)
         
         CALL R8INIR ( 9, 0.D0 , DFPMDG, 1 )
         DO 200 I=1,3
         DO 200 J=1,3
         DO 200 K=1,3
         DO 200 L=1,3
            DFPMDG(I,J)=DFPMDG(I,J)+DFPMDF(I,J,K,L)*DFPDG(K,L)
 200     CONTINUE
                  
         
      ELSEIF (IOPT.EQ.2) THEN
      
C        calcul de dFp/dGamma par linearisation directe 
C        de exp(-dgamma.ms x ns) 

         CALL DCOPY(9,GAMSNS,1,B,1)
         CALL DSCAL(9,-1.D0,B,1)
         CALL DAXPY(9,1.D0,ID,1,B,1)
         
         BMAX=0.D0
         BMIN=100.D0
         DO 20 I=1,3
            IF (B(I,I).GT.BMAX) BMAX=B(I,I)
            IF (B(I,I).LT.BMIN) BMIN=B(I,I)
 20      CONTINUE
         IF ((BMAX.GT.1.D3).OR.(BMIN.LT.1.D-3)) THEN
           IRET=1
           GOTO 9999
         ENDIF

         CALL LCDETF(3,B,DETB)
         
         IF (DETB.GT.R8PREM()) THEN
            EXPO=1.D0/3.D0
            COEF=DETB**EXPO
         ELSE
            IRET=1
            GOTO 9999
         ENDIF 
                  
         CALL MATINV('S',3,B,BM,DET2)
         
         CALL LCTR2M(3,BM,BMT)

         DDETDG = DDOT(9,BMT,1,MSNS,1)
         
         CALL DSCAL(9,DDETDG,B,1)
         
         CALL DCOPY(9,B,1,DFPMDG,1)
         
         CALL DSCAL(9,1.D0/3.D0,DFPMDG,1)
         
         CALL DAXPY(9,1.D0,MSNS,1,DFPMDG,1)
         
         CALL DSCAL(9,-COEF,DFPMDG,1)

         
      ELSEIF (IOPT.EQ.3) THEN
      
C suivant DE SOUZA-NIETO
          CALL ASSERT(.FALSE.)
      
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
C         CALL DEXPMAP(DFPMDG,NOCONV,DFP)
         
      ELSE
          CALL ASSERT(.FALSE.)
      
      ENDIF
      
      
 9999 CONTINUE      
      END
