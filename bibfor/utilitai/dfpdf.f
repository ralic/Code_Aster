      SUBROUTINE DFPDF (DIM,F,DSIDEP)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 04/10/2004   AUTEUR GODARD V.GODARD 
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
      REAL*8             F(DIM),DSIDEP(DIM,DIM)
C ----------------------------------------------------------------------
C    CALCUL DE LA DERIVEE DE LA PARTIE POSITIVE D UN TENSEUR SYMETRIQUE
C    PAR RAPPORT A CE TENSEUR
C
C    DEFINITION:
C    F SYMETRIQUE DONC DIAGONALISABLE:
C                      L1 0  0
C             F=  MT.  0  L2 0   .M
C                      0  0  L3
C
C    LA PARTIE POSITIVE S ECRIT (H FONCTION DE HEAVISIDE):
C                        H(L1)  0       0
C             FP=  MT.   0      H(L2)  0       .M
C                        0      0      H(L3)
C
C    ON CHERCHE DFP/DF
C
C    TENSEURS ENTRES SOUS FORME VECTORIELLE:
C    MATRICE (3x3)---> VECTEUR(6) ORDRE (XX YY ZZ XY XZ YZ)
C    MATRICE (2x2)---> VECTEUR(3) ORDRE (XX YY XY)
C    MATRICE (1x1)---> VECTEUR(1)
C
C    IN DIM   : DIMENSION DU VECTEUR (6 POUR 3D, 3 POUR 2D, 1 POUR 1D)
C    IN F     : VECTEUR (=TENSEUR)
C    OUT      : DFP/DF
C
C ----------------------------------------------------------------------
      INTEGER I,J,K,L,M,N,T(3,3),T2(2,2)
      REAL*8  RTEMP,RTEMP2,DSPDEP(3,3),EPSP(3),VECP(3,3),RIGMIN,SIGP(3)
      REAL*8  DSPDEB(2,2),EPSP2(2),VECP2(2,2),SIGP2(2)
      REAL*8  RTEMP3,RTEMP4,RAC2
      LOGICAL MTG
      PARAMETER  (RIGMIN = 1.D-6)




      T(1,1)=1
      T(1,2)=4
      T(1,3)=5
      T(2,1)=4
      T(2,2)=2
      T(2,3)=6
      T(3,1)=5
      T(3,2)=6
      T(3,3)=3


      T2(1,1)=1
      T2(1,2)=3
      T2(2,1)=3
      T2(2,2)=2

      RAC2=SQRT(2.D0)


      CALL R8INIR(DIM*DIM, 0.D0, DSIDEP, 1)



      IF (DIM.EQ.6) THEN

      CALL DIAGO3(F,VECP,EPSP)
      MTG = .TRUE.
      RTEMP=ABS(EPSP(1))
           
      IF (ABS(EPSP(2)).GT.RTEMP) RTEMP=ABS(EPSP(2))
      IF (ABS(EPSP(3)).GT.RTEMP) RTEMP=ABS(EPSP(3))
          DO 500 I=1,2
            DO 501 J=(I+1),3
          IF (ABS(EPSP(I)-EPSP(J)).LT.1D-12) THEN
           EPSP(I)=EPSP(I)+RIGMIN*RTEMP
            EPSP(J)=EPSP(J)-RIGMIN*RTEMP
          ENDIF
501     CONTINUE
500   CONTINUE
      DO 600 I=1,3
        IF (EPSP(I).GT.0.D0) THEN
          SIGP(I)=EPSP(I)
        ELSE
          SIGP(I)=0.D0
        ENDIF
600   CONTINUE
      MTG = .TRUE.
      CALL R8INIR(9, 0.D0, DSPDEP, 1)
      DO 120 K = 1,3
        IF (EPSP(K).GT.0.D0) THEN
          DSPDEP(K,K)=1.D0
        ELSE
          DSPDEP(K,K)=0.D0
        ENDIF
 120  CONTINUE
      DO 20 I=1,3
        DO 21 J=I,3
          DO 22 K=1,3
            DO 23 L=1,3
              DO 24 M=1,3
                DO 25 N=1,3
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
        DSIDEP(T(I,J),T(K,L))=DSIDEP(T(I,J),T(K,L))+VECP(K,M)*
     &        VECP(I,N)*VECP(J,N)*VECP(L,M)*DSPDEP(N,M)*RTEMP3*RTEMP4
        RTEMP=ABS(EPSP(M)-EPSP(N))
                   IF ((M.NE.N)) THEN
                     IF ((RTEMP.GT.1.D-12)) THEN
       RTEMP2=(VECP(K,M)*VECP(L,N))/(EPSP(N)-EPSP(M))
       RTEMP2=RTEMP2*VECP(I,M)*VECP(J,N)*SIGP(N)*RTEMP3*RTEMP4
       DSIDEP(T(I,J),T(K,L))=DSIDEP(T(I,J),T(K,L))+RTEMP2
       RTEMP2=(VECP(K,N)*VECP(L,M))/(EPSP(N)-EPSP(M))
       RTEMP2=RTEMP2*VECP(J,M)*VECP(I,N)*SIGP(N)*RTEMP3*RTEMP4
       DSIDEP(T(I,J),T(K,L))=DSIDEP(T(I,J),T(K,L))+RTEMP2
                      ELSE
                        MTG= .FALSE.
                      ENDIF
                    ENDIF
25                CONTINUE
24              CONTINUE
23            CONTINUE
22          CONTINUE
21        CONTINUE
20      CONTINUE
C       GOTO 999
       IF (.NOT.MTG) THEN
         DO 70 K=1,6
           DO 71 L=1,6
             DSIDEP(K,L)=0.D0
71         CONTINUE
70       CONTINUE
         DSIDEP(1,1)=1.D0
         DSIDEP(2,2)=1.D0
         DSIDEP(3,3)=1.D0
         DSIDEP(4,4)=1.D0
         DSIDEP(5,5)=1.D0
         DSIDEP(6,6)=1.D0
       ENDIF

       ELSEIF (DIM.EQ.3) THEN


      
      CALL DIAGO2(F,VECP2,EPSP2)
      MTG = .TRUE.
      RTEMP=ABS(EPSP2(1))
           
      IF (ABS(EPSP2(2)).GT.RTEMP) RTEMP=ABS(EPSP2(2))
          IF (ABS(EPSP2(1)-EPSP2(2)).LT.1D-12) THEN
           EPSP2(1)=EPSP2(1)+RIGMIN*RTEMP
           EPSP2(2)=EPSP2(2)-RIGMIN*RTEMP
          ENDIF

      DO 700 I=1,2
        IF (EPSP2(I).GT.0.D0) THEN
          SIGP2(I)=EPSP2(I)
        ELSE
          SIGP2(I)=0.D0
        ENDIF
700   CONTINUE
      MTG = .TRUE.
      CALL R8INIR(4, 0.D0, DSPDEB, 1)

      DO 7200 K = 1,2
        IF (EPSP2(K).GT.0.D0) THEN
          DSPDEB(K,K)=1.D0
        ELSE
          DSPDEB(K,K)=0.D0
        ENDIF
 7200  CONTINUE
      DO 720 I=1,2
        DO 721 J=I,2
          DO 722 K=1,2
            DO 723 L=1,2
              DO 724 M=1,2
                DO 725 N=1,2
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
        DSIDEP(T2(I,J),T2(K,L))=DSIDEP(T2(I,J),T2(K,L))+VECP2(K,M)*
     &   VECP2(I,N)*VECP2(J,N)*VECP2(L,M)*DSPDEB(N,M)*RTEMP3*RTEMP4
        RTEMP=ABS(EPSP2(M)-EPSP2(N))
                   IF ((M.NE.N)) THEN
                     IF ((RTEMP.GT.1.D-12)) THEN
       RTEMP2=(VECP2(K,M)*VECP2(L,N))/(EPSP2(N)-EPSP2(M))
       RTEMP2=RTEMP2*VECP2(I,M)*VECP2(J,N)*SIGP2(N)*RTEMP3*RTEMP4
       DSIDEP(T2(I,J),T2(K,L))=DSIDEP(T2(I,J),T2(K,L))+RTEMP2
       RTEMP2=(VECP2(K,N)*VECP2(L,M))/(EPSP2(N)-EPSP2(M))
       RTEMP2=RTEMP2*VECP2(J,M)*VECP2(I,N)*SIGP2(N)*RTEMP3*RTEMP4
       DSIDEP(T2(I,J),T2(K,L))=DSIDEP(T2(I,J),T2(K,L))+RTEMP2
                      ELSE
                        MTG= .FALSE.
                      ENDIF
                    ENDIF
725               CONTINUE
724             CONTINUE
723           CONTINUE
722         CONTINUE
721       CONTINUE
720     CONTINUE
       IF (.NOT.MTG) THEN
         DSIDEP(1,1)=1.D0
         DSIDEP(2,2)=1.D0
         DSIDEP(3,3)=1.D0
       ENDIF


       ELSEIF (DIM.EQ.1) THEN
       
         IF (F(1).GT.0.D0) THEN 
           DSIDEP(1,1)=1.D0
         ELSE
           DSIDEP(1,1)=0.D0
         ENDIF



       ENDIF


      END
