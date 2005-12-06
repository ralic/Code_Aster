      SUBROUTINE DFBDE (DIM,B,E,DEUXMU,LAMBDA,DSIDEP)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 04/10/2004   AUTEUR GODARD V.GODARD 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
      INTEGER           DIM
      REAL*8                 B(6),E(6),DEUXMU,LAMBDA
      REAL*8                 DSIDEP(6,6)
C ----------------------------------------------------------------------
C     LOI DE COMPORTEMENT ENDO_ORTH_BETON
C     CALCUL DE LA DERIVEE DE LA FORCE THERMODYNAMIQUE (ENDO TRACTION)
C     PAR RAPPORT A LA DEFORMATION:DFB/DEPS
C
C     FB=-LAMBDA.TR(EB).H(TR(EB))-MU/2*((BE+EB)_+*E + E*(BE+EB)_+)
C        +ECROB*(I-B)
C     IN  DIM      : DIMENSION 3(3D) OU 2(2D)
C     IN  E        : DEFORMATION
C     IN  B        : TENSEUR D ENDOMMAGEMENT DE TRACTION
C     IN  LAMBDA   : /
C     IN  DEUXMU   : / COEFFICIENTS DE LAME
C     OUT DSIDEP      : DFB/DEPS
C ----------------------------------------------------------------------

      INTEGER I,J,K,L,T(3,3)
      INTEGER P,Q,R,S,IK,PQ,RS
      REAL*8  RTEMP2
      REAL*8  RTEMP3,RTEMP4,RAC2,KRON(3,3),DBEDE(6,6),MTEMP(6,6)
      REAL*8  BEEB(6),BEEBP(6),VECBEB(3,3),VALBEB(3)
      REAL*8  F1B(6,6),F2B(6,6)
      REAL*8  A(6,6),TREB,C(6,6),TREC

      T(1,1)=1
      T(1,2)=4
      T(1,3)=5
      T(2,1)=4
      T(2,2)=2
      T(2,3)=6
      T(3,1)=5
      T(3,2)=6
      T(3,3)=3


      KRON(1,1)=1.D0
      KRON(1,2)=0.D0
      KRON(1,3)=0.D0
      KRON(2,1)=0.D0
      KRON(2,2)=1.D0
      KRON(2,3)=0.D0
      KRON(3,1)=0.D0
      KRON(3,2)=0.D0
      KRON(3,3)=1.D0
      
      RAC2=SQRT(2.D0)

      CALL R8INIR(36,0.D0,DBEDE,1)
      CALL R8INIR(36,0.D0,C,1)
      CALL R8INIR(36,0.D0,DSIDEP,1)
      CALL R8INIR(36,0.D0,A,1)
      CALL R8INIR(6,0.D0,BEEB,1)
      CALL R8INIR(36,0.D0,F1B,1)
      CALL R8INIR(36,0.D0,F2B,1)

C-----CALCUL DE D(BE+EB)/DE------------------------------------
      RTEMP3 = 0.D0
      RTEMP4 = 0.D0
      DO 20 I=1,DIM
        DO 21 J=I,DIM
          DO 22 K=1,DIM
            DO 23 L=1,DIM
              IF (I.EQ.J) THEN
                RTEMP3=1.D0
              ELSE
                RTEMP3=RAC2
              ENDIF
              IF (K.EQ.L) THEN
                RTEMP4=1.D0
              ELSE
                RTEMP4=1.D0/RAC2
              ENDIF
              DBEDE(T(I,J),T(K,L))=DBEDE(T(I,J),T(K,L))+(KRON(K,I)*
     &         B(T(L,J))+KRON(J,L)*B(T(I,K)))*RTEMP3*RTEMP4
23          CONTINUE
            BEEB(T(I,J))=BEEB(T(I,J))+
     &              B(T(I,K))*E(T(K,J))+E(T(I,K))*B(T(K,J))            
22        CONTINUE
21      CONTINUE
20    CONTINUE


        CALL DIAGO3(BEEB,VECBEB,VALBEB)
        DO 822 I=1,3
          IF (VALBEB(I).LT.0.D0) THEN
            VALBEB(I)=0.D0
          ENDIF
 822    CONTINUE

        CALL R8INIR(6,0.D0,BEEBP,1)

        DO 823 I=1,3
          DO 824 J=I,3
            DO 825 K=1,3
         BEEBP(T(I,J))=BEEBP(T(I,J))+VECBEB(I,K)*VALBEB(K)*VECBEB(J,K)
 825        CONTINUE
 824      CONTINUE
 823    CONTINUE




C----------------------------------------------------------------------
C
C-----CALCUL DE F2B----------------------------------------------------


      CALL DFPDF(6,BEEB,MTEMP)
      
      DO 240 IK=1,6
        DO 241 PQ=1,6
          DO 242 RS=1,6
            A(IK,PQ)=A(IK,PQ)+MTEMP(IK,RS)*DBEDE(RS,PQ)
242       CONTINUE
241     CONTINUE
240   CONTINUE

      RTEMP2 = 0.D0
      RTEMP3 = 0.D0
      RTEMP4 = 0.D0
      DO 250 I=1,3
        DO 251 J=I,3
          DO 252 K=1,3
            DO 253 L=1,6
              IF (I.EQ.J) THEN
                RTEMP2=1.D0
              ELSE
                RTEMP2=RAC2
              ENDIF
              IF (K.EQ.J) THEN
                RTEMP3=1.D0
              ELSE
                RTEMP3=1.D0/RAC2
              ENDIF
              IF (K.EQ.I) THEN
                RTEMP4=1.D0
              ELSE
                RTEMP4=1.D0/RAC2
              ENDIF              
              F2B(T(I,J),L)=F2B(T(I,J),L)+(A(T(I,K),L)*E(T(K,J))*
     &                    RTEMP4+E(T(I,K))*A(T(K,J),L)*RTEMP3)*RTEMP2
253         CONTINUE
252       CONTINUE
251     CONTINUE
250   CONTINUE


      RTEMP2 = 0.D0
      RTEMP3 = 0.D0
      DO 260 I=1,3
        DO 261 J=I,3
            DO 262 P=1,3
              DO 263 Q=1,3 
                IF (I.EQ.J) THEN
                  RTEMP2=1.D0
                ELSE
                  RTEMP2=RAC2
                ENDIF
                IF (P.EQ.Q) THEN
                  RTEMP3=1.D0
                ELSE
                  RTEMP3=1.D0/RAC2
                ENDIF
      F2B(T(I,J),T(P,Q))=F2B(T(I,J),T(P,Q))
     &                   +(KRON(J,Q)*BEEBP(T(I,P))
     &                   +KRON(I,P)*BEEBP(T(J,Q)))
     &                   *RTEMP2*RTEMP3              
263         CONTINUE
262       CONTINUE
261     CONTINUE
260   CONTINUE

C-----------------------------------------------------------------------
C
C-----CALCUL DE F1B-----------------------------------------------------

      TREB=(BEEB(1)+BEEB(2)+BEEB(3))/2.D0
      
      IF (TREB.LT.0) THEN
         TREC=0.D0
      ELSE 
         TREC=1.D0
      ENDIF   
         

      RTEMP2 = 0.D0
      RTEMP3 = 0.D0
      DO 120 I=1,3
        DO 121 J=I,3
          DO 122 P=1,3
            DO 123 Q=1,3
              IF (I.EQ.J) THEN
              RTEMP2=1.D0
              ELSE
              RTEMP2=RAC2
              ENDIF
              IF (P.EQ.Q) THEN
              RTEMP3=1.D0
              ELSE
              RTEMP3=1.D0/RAC2
              ENDIF
         F1B(T(I,J),T(P,Q))=F1B(T(I,J),T(P,Q))+TREC*B(T(P,Q))
     &                     *E(T(I,J))*RTEMP2*RTEMP3
123            CONTINUE
122          CONTINUE
121        CONTINUE
120   CONTINUE


      RTEMP2 = 0.D0
      RTEMP3 = 0.D0
      DO 160 I=1,3
        DO 161 J=I,3
          DO 162 P=1,3
              DO 163 Q=1,3
              IF (I.EQ.J) THEN
              RTEMP2=1.D0
              ELSE
              RTEMP2=RAC2
              ENDIF
              IF (P.EQ.Q) THEN
              RTEMP3=1.D0
              ELSE
              RTEMP3=1.D0/RAC2
              ENDIF
          F1B(T(I,J),T(P,Q))= F1B(T(I,J),T(P,Q))+TREC*TREB
     &                        *KRON(I,P)*KRON(J,Q)*RTEMP2*RTEMP3
163            CONTINUE
162          CONTINUE
161        CONTINUE
160   CONTINUE
      

C----CALCUL DE FB-------------------------------------------------------

      DO 900 I=1,6
        DO 901 J=1,6
          DSIDEP(I,J)=-LAMBDA*F1B(I,J)-DEUXMU/4.D0*F2B(I,J)
901     CONTINUE
900   CONTINUE      
      
      END
