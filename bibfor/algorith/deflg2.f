      SUBROUTINE DEFLG2(GN,LAMB,LOGL,PES,FETA,XI,ME)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 07/11/2011   AUTEUR PROIX J-M.PROIX 
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
C ----------------------------------------------------------------------
      IMPLICIT NONE
C     CALCUL DES DES TERMES NECESSAIRES
C     AU POST TRAITEMENT DES CONTRAINTES 
C     SUIVANT ARTICLE MIEHE APEL LAMBRECHT CMAME 2002
C ----------------------------------------------------------------------
C     IN   GN    directions propres du tenseur F
C     IN   LAMB  valeurs propres du tenseur F
C     IN   LOGL  log des valeurs propres du tenseur F
C     OUT  PES   tenseur P symetrise, pour le passage de T a PK2  
C     OUT  FETA  utilitaires pour DEFLG3 : f_i=-2/lambda_i**2 puis eta 
C     OUT  XI    utilitaires pour DEFLG3 : xi_ij
C     OUT  ME    utilitaires pour DEFLG3 : tenseur M d'ordre 4
C ----------------------------------------------------------------------
      REAL*8  GN(3,3),R8MIEM, LAMB(3),LOGL(3)
      REAL*8  DI(3),THETA(3,3)
      REAL*8  PE(3,3,3,3),ME(3,3,3,3),XI(3,3),FETA(4),PES(6,6)
      INTEGER NBVEC,I,ICAS,J,K,L,A,B
C ----------------------------------------------------------------------

      NBVEC = 3
      
      CALL R8INIR(4,0.D0,FETA,1)
      DO 14 I=1,3
         DI(I)=1.D0/LAMB(I)
         FETA(I)=-2.D0/LAMB(I)/LAMB(I)
 14   CONTINUE
      
      IF (ABS(LAMB(1)-LAMB(2)).LT.R8MIEM()) THEN
         IF (ABS(LAMB(1)-LAMB(3)).LT.R8MIEM()) THEN
            ICAS=123
         ELSE
            ICAS=12
         ENDIF
      ELSE
        IF (ABS(LAMB(1)-LAMB(3)).LT.R8MIEM()) THEN
           ICAS=13
        ELSEIF (ABS(LAMB(2)-LAMB(3)).LT.R8MIEM()) THEN
           ICAS=23
        ELSE
           ICAS=1
        ENDIF
      ENDIF   
      
      CALL R8INIR(9,0.D0,THETA,1)
      CALL R8INIR(9,0.D0,XI,1)
      IF (ICAS.EQ.1) THEN
         THETA(1,2)=(LOGL(1)-LOGL(2))/(LAMB(1)-LAMB(2))
         THETA(2,1)=(LOGL(2)-LOGL(1))/(LAMB(2)-LAMB(1))
         THETA(3,2)=(LOGL(3)-LOGL(2))/(LAMB(3)-LAMB(2))
         THETA(2,3)=(LOGL(2)-LOGL(3))/(LAMB(2)-LAMB(3))
         THETA(1,3)=(LOGL(1)-LOGL(3))/(LAMB(1)-LAMB(3))
         THETA(3,1)=(LOGL(3)-LOGL(1))/(LAMB(3)-LAMB(1))
         XI(1,2)=  (THETA(1,2) - 0.5D0*DI(2)) / (LAMB(1)-LAMB(2))
         XI(2,1)=  (THETA(2,1) - 0.5D0*DI(1)) / (LAMB(2)-LAMB(1))
         XI(3,2)=  (THETA(3,2) - 0.5D0*DI(2)) / (LAMB(3)-LAMB(2))
         XI(2,3)=  (THETA(2,3) - 0.5D0*DI(3)) / (LAMB(2)-LAMB(3))
         XI(1,3)=  (THETA(1,3) - 0.5D0*DI(3)) / (LAMB(1)-LAMB(3))
         XI(3,1)=  (THETA(3,1) - 0.5D0*DI(1)) / (LAMB(3)-LAMB(1))
         DO 15 I=1,3
            DO 16 J=1,3
               DO 17 K=1,3
                  IF ((J.NE.I).AND.(J.NE.K).AND.(K.NE.I)) THEN
                     FETA(4)=FETA(4)+ 
     &               LOGL(I)*0.5D0/(LAMB(I)-LAMB(J))/(LAMB(I)-LAMB(K))
                  ENDIF
 17            CONTINUE
 16         CONTINUE
 15      CONTINUE
      ELSEIF (ICAS.EQ.12) THEN
         THETA(1,2)=DI(1)/2.D0
         THETA(2,1)=DI(1)/2.D0
         THETA(1,3)=(LOGL(1)-LOGL(3))/(LAMB(1)-LAMB(3))
         THETA(3,1)=(LOGL(3)-LOGL(1))/(LAMB(3)-LAMB(1))
         THETA(3,2)=(LOGL(3)-LOGL(2))/(LAMB(3)-LAMB(2))
         THETA(2,3)=(LOGL(2)-LOGL(3))/(LAMB(2)-LAMB(3))
         XI(1,2)=  FETA(1)/8.D0
         XI(2,1)=  FETA(1)/8.D0
         XI(3,2)=  (THETA(3,2) - 0.5D0*DI(2)) / (LAMB(3)-LAMB(2))
         XI(2,3)=  (THETA(2,3) - 0.5D0*DI(3)) / (LAMB(2)-LAMB(3))
         XI(1,3)=  (THETA(1,3) - 0.5D0*DI(3)) / (LAMB(1)-LAMB(3))
         XI(3,1)=  (THETA(3,1) - 0.5D0*DI(1)) / (LAMB(3)-LAMB(1))
         FETA(4)= XI(3,1)
      ELSEIF (ICAS.EQ.13) THEN
         THETA(1,2)=(LOGL(1)-LOGL(2))/(LAMB(1)-LAMB(2))
         THETA(2,1)=(LOGL(2)-LOGL(1))/(LAMB(2)-LAMB(1))
         THETA(1,3)=DI(1)/2.D0
         THETA(3,1)=DI(1)/2.D0
         THETA(3,2)=(LOGL(3)-LOGL(2))/(LAMB(3)-LAMB(2))
         THETA(2,3)=(LOGL(2)-LOGL(3))/(LAMB(2)-LAMB(3))
         XI(1,2)=  (THETA(1,2) - 0.5D0*DI(2)) / (LAMB(1)-LAMB(2))
         XI(2,1)=  (THETA(2,1) - 0.5D0*DI(1)) / (LAMB(2)-LAMB(1))
         XI(3,2)=  (THETA(3,2) - 0.5D0*DI(2)) / (LAMB(3)-LAMB(2))
         XI(2,3)=  (THETA(2,3) - 0.5D0*DI(3)) / (LAMB(2)-LAMB(3))
         XI(1,3)=  FETA(1)/8.D0
         XI(3,1)=  FETA(1)/8.D0
         FETA(4)= XI(1,2)
      ELSEIF (ICAS.EQ.23) THEN
         THETA(1,2)=(LOGL(1)-LOGL(2))/(LAMB(1)-LAMB(2))
         THETA(2,1)=(LOGL(2)-LOGL(1))/(LAMB(2)-LAMB(1))
         THETA(1,3)=(LOGL(1)-LOGL(3))/(LAMB(1)-LAMB(3))
         THETA(3,1)=(LOGL(3)-LOGL(1))/(LAMB(3)-LAMB(1))
         THETA(3,2)=DI(1)/2.D0
         THETA(2,3)=DI(1)/2.D0
         XI(1,2)=  (THETA(1,2) - 0.5D0*DI(2)) / (LAMB(1)-LAMB(2))
         XI(2,1)=  (THETA(2,1) - 0.5D0*DI(1)) / (LAMB(2)-LAMB(1))
         XI(3,2)=  FETA(2)/8.D0
         XI(2,3)=  FETA(2)/8.D0
         XI(1,3)=  (THETA(1,3) - 0.5D0*DI(3)) / (LAMB(1)-LAMB(3))
         XI(3,1)=  (THETA(3,1) - 0.5D0*DI(1)) / (LAMB(3)-LAMB(1))
         FETA(4)= XI(2,3)
      ELSEIF (ICAS.EQ.123) THEN
         THETA(1,2)=DI(1)/2.D0
         THETA(2,1)=DI(1)/2.D0
         THETA(1,3)=DI(1)/2.D0
         THETA(3,1)=DI(1)/2.D0
         THETA(3,2)=DI(1)/2.D0
         THETA(2,3)=DI(1)/2.D0
         XI(1,2)=  FETA(2)/8.D0
         XI(2,1)=  FETA(2)/8.D0
         XI(3,2)=  FETA(2)/8.D0
         XI(2,3)=  FETA(2)/8.D0
         XI(1,3)=  FETA(2)/8.D0
         XI(3,1)=  FETA(2)/8.D0
         FETA(4)=  FETA(2)/8.D0
      ENDIF
      
      
      CALL R8INIR(81,0.D0,ME,1)
      
C     calcul de M_E (Lagrangien) pour chaque direction propre
      DO 25 I=1,NBVEC
         DO 26 J=1,NBVEC
            DO 27 A=1,3
               DO 28 B=1,3      
                  ME(A,B,I,J)=GN(A,I)*GN(B,J)+GN(A,J)*GN(B,I)
 28            CONTINUE
 27         CONTINUE
 26      CONTINUE
 25   CONTINUE
 
      CALL R8INIR(81,0.D0,PE,1)
      
      DO 29 I=1,3
         DO 30 K=1,3
            DO 31 L=1,3      
               DO 32 A=1,3
                  DO 33 B=1,3      
                     PE(K,L,A,B)=PE(K,L,A,B)+
     &                           DI(I)*GN(K,I)*GN(L,I)*ME(A,B,I,I)/2.D0
 33               CONTINUE
 32            CONTINUE
 31         CONTINUE
 30      CONTINUE
 29   CONTINUE
      DO 34 I=1,3
         DO 35 J=1,3
            IF (I.NE.J) THEN
               DO 36 K=1,3
                  DO 37 L=1,3
                     DO 38 A=1,3
                        DO 39 B=1,3
                           PE(K,L,A,B)=PE(K,L,A,B)+
     &                           THETA(I,J)*GN(K,I)*GN(L,J)*ME(A,B,I,J)
 39                     CONTINUE
 38                  CONTINUE
 37               CONTINUE
 36            CONTINUE
            ENDIF
 35      CONTINUE
 34   CONTINUE
 
      CALL SYMT46(PE,PES)
      
      END
