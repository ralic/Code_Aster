      SUBROUTINE DFBDB (DIM,B,E,DEUXMU,LAMBDA,ECROB,DSIDEP)

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
      INTEGER            DIM
      REAL*8             B(6),E(6),DEUXMU,LAMBDA,DSIDEP(6,6),ECROB
C ----------------------------------------------------------------------
C     LOI DE COMPORTEMENT ENDO_ORTH_BETON
C     CALCUL DE LA DERIVEE DE LA FORCE THERMODYNAMIQUE (ENDO TRACTION)
C     PAR RAPPORT A L ENDOMMAGEMENT DE TRACTION:DFB/DB
C
C     FB=-LAMBDA.TR(EB).H(TR(EB))-MU/2*((BE+EB)_+*E + E*(BE+EB)_+)
C        +ECROB*(I-B)
C
C     IN  DIM      : DIMENSION 3(3D) OU 2(2D)
C     IN  E        : DEFORMATION
C     IN  B        : TENSEUR D ENDOMMAGEMENT DE TRACTION
C     IN  LAMBDA   : /
C     IN  DEUXMU   : / COEFFICIENTS DE LAME
C     IN  ECROB    : PARAMETRE D ECROUISSAGE DE TRACTION
C     OUT DSIDEP   : DFB/DB
C ----------------------------------------------------------------------

      INTEGER I,J,K,L,T(3,3),NDIM
      REAL*8  RTEMP2
      REAL*8  RTEMP3,RTEMP4,RAC2,KRON(3,3),DBEDB(6,6),C(6),MTEMP(6,6)
      REAL*8  A(6,6),TREB

      
           
      
      IF (DIM.EQ.3) THEN
      T(1,1)=1
      T(1,2)=4
      T(1,3)=5
      T(2,1)=4
      T(2,2)=2
      T(2,3)=6
      T(3,1)=5
      T(3,2)=6
      T(3,3)=3
      NDIM=6
      ELSEIF (DIM.EQ.2) THEN
      T(1,1)=1
      T(1,2)=3
      T(2,2)=2
      T(2,1)=3
      NDIM=3
      ENDIF

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
      CALL R8INIR(36,0.D0,DBEDB,1)
      CALL R8INIR(6,0.D0,C,1)
      CALL R8INIR(36,0.D0,DSIDEP,1)
      CALL R8INIR(36,0.D0,A,1)
      

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
              DBEDB(T(I,J),T(K,L))=DBEDB(T(I,J),T(K,L))+(KRON(K,I)*
     &         E(T(L,J))+KRON(J,L)*E(T(I,K)))*RTEMP3*RTEMP4
23          CONTINUE
            C(T(I,J))=C(T(I,J))+
     &              B(T(I,K))*E(T(K,J))+E(T(I,K))*B(T(K,J))            
22        CONTINUE
21      CONTINUE
20    CONTINUE

      CALL DFPDF(6,C,MTEMP)
      
      DO 40 I=1,NDIM
        DO 41 J=1,NDIM
          DO 42 K=1,NDIM
            A(I,J)=A(I,J)+MTEMP(I,K)*DBEDB(K,J)
42        CONTINUE
41      CONTINUE
40    CONTINUE


      DO 50 I=1,DIM
        DO 51 J=I,DIM
          DO 52 K=1,DIM
            DO 53 L=1,NDIM
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
              DSIDEP(T(I,J),L)=DSIDEP(T(I,J),L)+(A(T(I,K),L)*E(T(K,J))*
     &                    RTEMP4+E(T(I,K))*A(T(K,J),L)*RTEMP3)*RTEMP2
53          CONTINUE
52        CONTINUE
51      CONTINUE
50    CONTINUE


      DO 70 I=DIM+1,NDIM
        E(I)=RAC2*E(I)
70    CONTINUE

      IF (DIM.EQ.3) THEN
      TREB=(C(1)+C(2)+C(3))
      ELSEIF (DIM.EQ.2) THEN
      TREB=(C(1)+C(2))
      ENDIF


      IF (TREB.LT.0.D0) THEN
        TREB=0.D0
      ELSE
        TREB=1.D0
      ENDIF
      DO 60 I=1,NDIM
        DO 61 J=1,NDIM
          DSIDEP(I,J)=-DEUXMU/4.D0*DSIDEP(I,J)-LAMBDA*TREB*E(I)*E(J)
61      CONTINUE
60    CONTINUE

      DO 71 I=DIM+1,NDIM
        E(I)=E(I)/RAC2
71    CONTINUE
      
      
CCC ON RAJOUTE LE TERME QUI VIENT DE L ECROUISSAGE

      DO 80 I=1,NDIM
          DSIDEP(I,I)=DSIDEP(I,I)-ECROB
80    CONTINUE

      
      
      END
