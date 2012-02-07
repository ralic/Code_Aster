      SUBROUTINE LCMMKG(ZINV,VIND,VINF,NMAT,MATERF,MOD,NR,DSDE)
      IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 06/02/2012   AUTEUR PROIX J-M.PROIX 
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
      REAL*8 FEP(3,3),FEM(3,3),VINF(*),VIND(*),DSDE(6,3,3),DET
      REAL*8 DFEDF(3,3,3,3),FPP(3,3),FPPINV(3,3),ID(3,3)
      REAL*8 DR1DF(3,3,3,3),DR1DF6(6,3,3),ZINV(6,6),DSDF(6,3,3)
      REAL*8 FET(3,3),FETFE(3,3),EEL(6),HOOKE(6,6),S6(6),S(3,3)
      REAL*8 DTAUDF(3,3,3,3),MATERF(*),SFET(3,3)
      INTEGER NR, NDT, NS, NDI,I,J,K,L,M,N,NMAT,IND(3,3)
      COMMON /TDIM/ NDT,NDI
      CHARACTER*8     MOD
C     ------------------------------------------------------------------
      DATA ID/1.D0,0.D0,0.D0, 0.D0,1.D0,0.D0, 0.D0,0.D0,1.D0/
      
      DO 10 I=1,3
         IND(I,I)=I
   10 CONTINUE
      IND(1,2)=4
      IND(2,1)=4
      IND(1,3)=5
      IND(3,1)=5
      IND(2,3)=6
      IND(3,2)=6
       
      NS=NR-NDT
      CALL DCOPY(9,VIND(6+3*NS+10),1,FEM,1)
      CALL DAXPY(9,1.D0,ID,1,FEM,1)
      CALL DCOPY(9,VINF(6+3*NS+10),1,FEP,1)
      CALL DAXPY(9,1.D0,ID,1,FEP,1)
      
      CALL DCOPY(9,VINF(6+3*NS+1),1,FPP,1)
      CALL DAXPY(9,1.D0,ID,1,FPP,1)
      CALL MATINV('S',3,FPP,FPPINV,DET)

C CALCUL DE DFE/DF

      CALL R8INIR(81,0.D0,DFEDF,1)
      DO 1 I=1,3
      DO 1 J=1,3
      DO 1 K=1,3
      DO 1 L=1,3
      DO 1 M=1,3         
         DFEDF(I,J,K,L)=DFEDF(I,J,K,L)+ID(I,K)*FEM(L,M)*FPPINV(M,J)
    1 CONTINUE

C CALCUL DE DR1/DF
      CALL R8INIR(81,0.D0,DR1DF,1)
      DO 3 I=1,3
      DO 3 J=1,3
      DO 3 K=1,3
      DO 3 L=1,3
      DO 3 M=1,3         
         DR1DF(I,J,K,L)=DR1DF(I,J,K,L)
     &  +DFEDF(M,I,K,L)*FEP(M,J)   
    3 CONTINUE
      DO 31 I=1,3
      DO 31 J=1,3
      DO 31 K=1,3
      DO 31 L=1,3
      DO 31 M=1,3         
         DR1DF(I,J,K,L)=DR1DF(I,J,K,L)
     &  +FEP(M,I)*DFEDF(M,J,K,L)        
   31 CONTINUE
      CALL DSCAL(81,-0.5D0,DR1DF,1)

C CALCUL DE DS/DF EN UTILISANT LES SYMETRIES
      DO 4 I=1,3
      DO 4 J=1,3
      DO 4 K=1,3
      DO 4 L=1,3
         DR1DF6(IND(I,J),K,L)=DR1DF(I,J,K,L)
    4 CONTINUE

      CALL R8INIR(54,0.D0,DSDF,1)
      DO 6 I=1,6
      DO 6 J=1,3
      DO 6 K=1,3
      DO 6 L=1,6
        DSDF(I,J,K)=DSDF(I,J,K)-ZINV(I,L)*DR1DF6(L,J,K)
    6 CONTINUE

C RECALCUL DU PK2 S
      IF (MATERF(NMAT).EQ.0) THEN
         CALL LCOPLI  ( 'ISOTROPE' , MOD , MATERF(1) , HOOKE )
      ELSEIF (MATERF(NMAT).EQ.1) THEN
         CALL LCOPLI  ( 'ORTHOTRO' , MOD , MATERF(1) , HOOKE )
      ENDIF
      CALL LCTR2M(3, FEP , FET )
      CALL PMAT(3,FET,FEP,FETFE)
      CALL DAXPY(9,-1.D0,ID,1,FETFE,1)
      CALL DSCAL(9,0.5D0,FETFE,1)
      
C      CONTRAINTES PK2
      CALL TNSVEC(3,3,FETFE, EEL,1.D0)
      CALL LCPRMV ( HOOKE,   EEL  , S6)
      CALL TNSVEC(6,3,S, S6,1.D0)

C CALCUL DE DTAU/DF EN UTILISANT LES SYMETRIES
      CALL R8INIR(81,0.D0,DTAUDF,1)
      CALL PMAT(3,S,FET,SFET)
      DO 71 I=1,3
      DO 71 J=1,3
      DO 71 K=1,3
      DO 71 L=1,3
      DO 71 M=1,3         
         DTAUDF(I,J,K,L)=DTAUDF(I,J,K,L)+DFEDF(I,M,K,L)*SFET(M,J)
  71  CONTINUE
  
      DO 72 I=1,3
      DO 72 J=1,3
      DO 72 K=1,3
      DO 72 L=1,3
      DO 72 M=1,3         
         DTAUDF(I,J,K,L)=DTAUDF(I,J,K,L)+SFET(M,I)*DFEDF(J,M,K,L)
  72  CONTINUE
  
      DO 73 I=1,3
      DO 73 J=1,3
      DO 73 K=1,3
      DO 73 L=1,3
      DO 73 M=1,3         
      DO 73 N=1,3         
         DTAUDF(I,J,K,L)=DTAUDF(I,J,K,L)+
     &   FEP(I,M)*DSDF(IND(M,N),K,L)*FEP(J,N)
  73  CONTINUE
  
      DO 8 I=1,3
      DO 8 J=1,3
      DO 8 K=1,3
      DO 8 L=1,3
         DSDE(IND(I,J),K,L)=DTAUDF(I,J,K,L)
    8 CONTINUE

C LES RACINE(2) ATTENDUES PAR NMCOMP  !!!         
      CALL DSCAL(9,SQRT(2.D0),DSDE(4,1,1),6)
      CALL DSCAL(9,SQRT(2.D0),DSDE(5,1,1),6)
      CALL DSCAL(9,SQRT(2.D0),DSDE(6,1,1),6)
      
      END
