      SUBROUTINE DEFLOG(NDIM,F,EPSL,PE,GN,FETA,XI,ME, ICALC, T, TL )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 10/01/2011   AUTEUR PROIX J-M.PROIX 
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
C     CALCUL DES DEFORMATIONS LOGARITHMIQUES ET DES TERMES NECESSAIRES
C     AU POST TRAITEMENT DES CONTRAINTES ET A LA RIGIDITE TANGENTE
C     SUIVANT ARTICLE MIEHE APEL LAMBRECHT CMAME 2002
C ----------------------------------------------------------------------
C     IN    F gradient de la transformation calcule sur config initiale
C           ICALC : TYPE DE CALCUL
C    SI ICALC = 0 
C     OUT    EPSL  deforamtion logarithmique (si ICALC=0)
C            PE, GN, FETA, XI, ME
C    SI ICALC = 1 
C     IN     T     TENSEUR CONTRAINTE  (XX YY ZZ XY XZ YZ)
C            PE, GN, FETA, XI, ME
C     OUT    TL  
C ----------------------------------------------------------------------
      REAL*8  TR(6),GN(3,3),EPSL33(3,3),T(6),TL(3,3,3,3),TR2(3)
      REAL*8  FT(3,3), DI(3),THETA(3,3),DZETA(3,3),T33(3,3),GN2(2,2)
      REAL*8  F(3,3),EPSL(6),R8MIEM, LAMB(3),LOGL(3),PN(3,3)
      REAL*8  PE(3,3,3,3),ME(3,3,3,3),F33(3,3),XI(3,3),FETA(4)
      INTEGER NBVEC,I,ICAS,J,K,L,A,B,C,D,ICALC,NDIM3,CONFIG,NDIM
C ----------------------------------------------------------------------

      NBVEC = 3
      NDIM3 = 3
C     CONFIG=0 : LAGRANGIEN, CONFIG=1 : EULERIEN (NON IMPLEMENTE)
      CONFIG=0

C     LE CALCUL DES VALEURS PROPRES N'A PAS ENCORE ETE FAIT  
      CALL LCTR2M(3,F,FT)
      CALL PMAT(3,FT,F,F33)
C --- VALEURS PRINCIPALES = VECTEURS PROPRES
C  VECP : DIM1=I=COMPOSANTE DIM2=J=NUM VECTEUR ASSOCIE A LAMBP(J)
      CALL TNSVEC(3,NDIM3,F33,TR,1.D0)
      
      IF(NDIM.EQ.3) THEN
      
C         CALL DIAGO3(TR,GN,LAMB)
C     --------------------------------
C     pour gagner du temps
C     --------------------------------
C --- MATRICE TR = (XX XY XZ YY YZ ZZ) (POUR DIAGP3)
         TR(1) = F33(1,1)
         TR(2) = F33(1,2)
         TR(3) = F33(1,3)
         TR(4) = F33(2,2)
         TR(5) = F33(2,3)
         TR(6) = F33(3,3)
         CALL DIAGP3(TR,GN,LAMB) 

      ELSEIF(NDIM.EQ.2) THEN
      
         TR2(1)=TR(1)
         TR2(2)=TR(2)
         TR2(3)=TR(4)
         CALL DIAGO2(TR2,GN2,LAMB)
         LAMB(3)=TR(3)
         CALL R8INIR(9,0.D0,GN,1)
         DO 1 I=1,2
         DO 1 J=1,2
            GN(I,J)=GN2(I,J)
 1       CONTINUE
         GN(3,3)=1.D0
         
      ENDIF

     
      DO 10 I=1,NBVEC
         LOGL(I)=LOG(LAMB(I))*0.5D0
 10   CONTINUE
      
C   EPSL = DEFORMATION LOGARITHMIQUE
      CALL R8INIR(9,0.D0,EPSL33,1)
      CALL R8INIR(6,0.D0,EPSL,1)
      DO 11 I=1,3
         DO 12 J=1,3
            DO 13 K=1,NBVEC
C              Calcul de EPSL dans le repere general
               EPSL33(I,J)=EPSL33(I,J)+LOGL(K)*GN(I,K)*GN(J,K)
 13         CONTINUE
 12      CONTINUE
 11   CONTINUE
      CALL TNSVEC(3,NDIM3,EPSL33,EPSL,SQRT(2.D0))
      
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
      IF (CONFIG.EQ.1) THEN
         CALL R8INIR(9,0.D0,PN,1)
C        calcul de n=F.N pour chaque direction propre
         DO 18 K=1,NBVEC
            DO 19 I=1,3
               DO 20 J=1,3
                  PN(I,K)=PN(I,K)+F(I,J)*GN(J,K)
 20            CONTINUE
 19         CONTINUE
 18      CONTINUE
C        calcul de M_E (Eulerien) pour chaque direction propre
         DO 21 I=1,NBVEC
            DO 22 J=1,NBVEC
               DO 23 A=1,3
                  DO 24 B=1,3      
                     ME(A,B,I,J)=PN(A,I)*PN(B,J)+PN(A,J)*PN(B,I)
 24               CONTINUE
 23            CONTINUE
 22         CONTINUE
 21      CONTINUE
      ELSE
C        calcul de M_E (Lagrangien) pour chaque direction propre
         DO 25 I=1,NBVEC
            DO 26 J=1,NBVEC
               DO 27 A=1,3
                  DO 28 B=1,3      
                     ME(A,B,I,J)=GN(A,I)*GN(B,J)+GN(A,J)*GN(B,I)
 28               CONTINUE
 27            CONTINUE
 26         CONTINUE
 25      CONTINUE
 
      ENDIF
      
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


      IF (ICALC.EQ.1) THEN
C     CALCUL DU TERME T.L

         CALL R8INIR(81,0.D0,TL,1)
         CALL R8INIR(9,0.D0,DZETA,1)
         CALL TNSVEC(6,3,T33,T,1.D0)
         DO 40 I=1,3
            DO 41 J=1,3
               DO 42 A=1,3
                  DO 43 B=1,3
                     DZETA(I,J)=DZETA(I,J)+T33(A,B)*GN(A,I)*GN(B,J)
 43               CONTINUE
 42            CONTINUE
 41         CONTINUE
 40      CONTINUE

         DO 44 I=1,3
            DO 45 A=1,3
               DO 46 B=1,3
                  DO 47 C=1,3
                     DO 48 D=1,3
                        TL(A,B,C,D)=TL(A,B,C,D)+
     &           0.25D0*FETA(I)*DZETA(I,I)*ME(A,B,I,I)*ME(C,D,I,I)
 48                  CONTINUE
 47               CONTINUE
 46            CONTINUE
 45         CONTINUE
 44      CONTINUE

         DO 49 I=1,3
            DO 50 J=1,3
               DO 51 K=1,3
                  DO 52 A=1,3
                     DO 53 B=1,3
                        DO 54 C=1,3
                           DO 55 D=1,3
                           IF ((J.NE.I).AND.(J.NE.K).AND.(K.NE.I)) THEN
                              TL(A,B,C,D)=TL(A,B,C,D)+
     &                  2.D0*FETA(4)*DZETA(I,J)*ME(A,B,I,K)*ME(C,D,J,K)
                           ENDIF
 55                        CONTINUE
 54                     CONTINUE
 53                  CONTINUE
 52               CONTINUE
 51            CONTINUE
 50         CONTINUE
 49      CONTINUE

         DO 56 I=1,3
            DO 57 J=1,3
               DO 58 A=1,3
                  DO 59 B=1,3
                     DO 60 C=1,3
                        DO 61 D=1,3
                           IF (J.NE.I) THEN
                              TL(A,B,C,D)=TL(A,B,C,D)+
     &                  2.D0*XI(I,J)*DZETA(I,J)*ME(A,B,I,J)*ME(C,D,J,J)
                           ENDIF
 61                     CONTINUE
 60                  CONTINUE
 59               CONTINUE
 58            CONTINUE
 57         CONTINUE
 56      CONTINUE

         DO 62 I=1,3
            DO 63 J=1,3
               DO 64 A=1,3
                  DO 65 B=1,3
                     DO 66 C=1,3
                        DO 67 D=1,3
                           IF (J.NE.I) THEN
                              TL(A,B,C,D)=TL(A,B,C,D)+
     &                  2.D0*XI(I,J)*DZETA(I,J)*ME(A,B,J,J)*ME(C,D,I,J)
                           ENDIF
 67                     CONTINUE
 66                  CONTINUE
 65               CONTINUE
 64            CONTINUE
 63         CONTINUE
 62      CONTINUE

         DO 68 I=1,3
            DO 69 J=1,3
               DO 70 A=1,3
                  DO 71 B=1,3
                     DO 72 C=1,3
                        DO 73 D=1,3
                           IF (J.NE.I) THEN
                              TL(A,B,C,D)=TL(A,B,C,D)+
     &                  2.D0*XI(I,J)*DZETA(J,J)*ME(A,B,I,J)*ME(C,D,I,J)
                           ENDIF
 73                     CONTINUE
 72                  CONTINUE
 71               CONTINUE
 70            CONTINUE
 69         CONTINUE
 68      CONTINUE

      ENDIF

      END
