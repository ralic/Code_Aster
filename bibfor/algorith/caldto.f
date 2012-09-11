      SUBROUTINE CALDTO(S6,FKOOH,MSNS,DTODS)
      IMPLICIT NONE
      
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 10/09/2012   AUTEUR PROIX J-M.PROIX 
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
C RESPONSABLE PROIX J-M.PROIX
C     ----------------------------------------------------------------
C     
C     MONOCRISTAL : calcul de la derivee de Tau en GDEF : dTau/dS_ab   
C     IN  S6    :  CONTRAINTES NOTATION VOIGT
C     IN  FKOOH :  INVERSE TENSEUR HOOKE
C     IN  MSNS  :  MS * NS
C     OUT DTODS :  dTau/dS

      INTEGER I,J,K,L,IND(3,3),A,B,M,N
      REAL*8 S6(6),FKOOH(6,6),MSNS(3,3),DTODS(3,3),HMS6(6),HMS(3,3)
      REAL*8 S(3,3),T1(3,3),T16(6),T1B(3,3),T1B6(6),L4(3,3,3,3)
      REAL*8 MUS(3,3),L4S(3,3)
      DATA IND/1,4,5,4,2,6,5,6,3/
C     ----------------------------------------------------------------
C     CONSTUCTION DU TENSEUR INVERSE DE HOOKE D'ORDRE 4

      DO 1 I=1,3
      DO 1 J=1,3
      DO 1 K=1,3
      DO 1 L=1,3
         L4(I,J,K,L)=FKOOH(IND(I,J),IND(K,L))
 1    CONTINUE
      
      CALL TNSVEC(6,3,S, S6, 1.D0)
      CALL DCOPY(9,MSNS,1,DTODS,1)
      
      CALL R8INIR ( 9, 0.D0 , MUS, 1 )

C     CALCUL DU TERME  2(Lambda**-1)*mu*S 
      DO 2 M=1,3
      DO 2 N=1,3
      DO 2 K=1,3
         MUS(M,N)=MUS(M,N)+MSNS(M,K)*S(K,N)
 2    CONTINUE

      DO 3 A=1,3
      DO 3 B=1,3
      DO 3 M=1,3
      DO 3 N=1,3
         DTODS(A,B)=DTODS(A,B)+2.D0*L4(A,B,M,N)*MUS(M,N)
 3    CONTINUE
      
C     CALCUL DU TERME  2(LAMBDA**-1*S)*MU 
      CALL R8INIR ( 9, 0.D0 , L4S, 1 )
      DO 4 A=1,3
      DO 4 K=1,3
      DO 4 M=1,3
      DO 4 N=1,3
         L4S(A,K)=L4S(A,K)+L4(A,K,M,N)*MUS(M,N)
 4    CONTINUE

      DO 5 A=1,3
      DO 5 B=1,3
      DO 5 K=1,3
         DTODS(A,B)=DTODS(A,B)+2.D0*L4S(A,K)*MUS(K,B)
 5    CONTINUE

      END
