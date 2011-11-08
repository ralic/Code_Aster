      SUBROUTINE DEFLG3(GN,FETA,XI,ME,T,TL)
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
C     CALCUL DES DEFORMATIONS LOGARITHMIQUES ET DES TERMES NECESSAIRES
C     AU POST TRAITEMENT DES CONTRAINTES ET A LA RIGIDITE TANGENTE
C     SUIVANT ARTICLE MIEHE APEL LAMBRECHT CMAME 2002
C ----------------------------------------------------------------------
C     IN GN    directions propres du tenseur F
C     IN FETA  utilitaires issus de DEFLG2  f_i=-2/lambda_i**2 puis eta
C     IN XI    utilitaires issus de DEFLG2  xi_ij
C     IN ME    utilitaires issus de DEFLG2  tenseur M d'ordre 4
C     IN T     tenseur des contraintesissu de NMCOMP (avec sqrt(2))
C     OUT TL   tenseur d'ordre 4 T:L
C ----------------------------------------------------------------------
      REAL*8  GN(3,3),T(6),TL(3,3,3,3)
      REAL*8  DZETA(3,3),T33(3,3),ME(3,3,3,3),XI(3,3),FETA(4)
      INTEGER I,J,K,A,B,C,D
C ----------------------------------------------------------------------

C     CALCUL DU TERME T.L

      CALL R8INIR(81,0.D0,TL,1)
      CALL R8INIR(9,0.D0,DZETA,1)
      CALL TNSVEC(6,3,T33,T,1.D0/SQRT(2.D0))

C     A,B sont les composantes, J,I sont les modes propres      
      DO 40 I=1,3
         DO 41 J=1,3
            DO 42 A=1,3
               DO 43 B=1,3
                  DZETA(I,J)=DZETA(I,J)+T33(A,B)*GN(A,I)*GN(B,J)
 43            CONTINUE
 42         CONTINUE
 41      CONTINUE
 40   CONTINUE

      DO 44 I=1,3
         DO 45 A=1,3
            DO 46 B=1,3
               DO 47 C=1,3
                  DO 48 D=1,3
                     TL(A,B,C,D)=TL(A,B,C,D)+
     &        0.25D0*FETA(I)*DZETA(I,I)*ME(A,B,I,I)*ME(C,D,I,I)
 48               CONTINUE
 47            CONTINUE
 46         CONTINUE
 45      CONTINUE
 44   CONTINUE

      DO 49 I=1,3
         DO 50 J=1,3
            DO 51 K=1,3
               DO 52 A=1,3
                  DO 53 B=1,3
                     DO 54 C=1,3
                        DO 55 D=1,3
                        IF ((J.NE.I).AND.(J.NE.K).AND.(K.NE.I)) THEN
                           TL(A,B,C,D)=TL(A,B,C,D)+
     &               2.D0*FETA(4)*DZETA(I,J)*ME(A,B,I,K)*ME(C,D,J,K)
                        ENDIF
 55                     CONTINUE
 54                  CONTINUE
 53               CONTINUE
 52            CONTINUE
 51         CONTINUE
 50      CONTINUE
 49   CONTINUE

      DO 56 I=1,3
         DO 57 J=1,3
            DO 58 A=1,3
               DO 59 B=1,3
                  DO 60 C=1,3
                     DO 61 D=1,3
                        IF (J.NE.I) THEN
                           TL(A,B,C,D)=TL(A,B,C,D)+
     &               2.D0*XI(I,J)*DZETA(I,J)*ME(A,B,I,J)*ME(C,D,J,J)
                        ENDIF
 61                  CONTINUE
 60               CONTINUE
 59            CONTINUE
 58         CONTINUE
 57      CONTINUE
 56   CONTINUE

      DO 62 I=1,3
         DO 63 J=1,3
            DO 64 A=1,3
               DO 65 B=1,3
                  DO 66 C=1,3
                     DO 67 D=1,3
                        IF (J.NE.I) THEN
                           TL(A,B,C,D)=TL(A,B,C,D)+
     &               2.D0*XI(I,J)*DZETA(I,J)*ME(A,B,J,J)*ME(C,D,I,J)
                        ENDIF
 67                  CONTINUE
 66               CONTINUE
 65            CONTINUE
 64         CONTINUE
 63      CONTINUE
 62   CONTINUE

      DO 68 I=1,3
         DO 69 J=1,3
            DO 70 A=1,3
               DO 71 B=1,3
                  DO 72 C=1,3
                     DO 73 D=1,3
                        IF (J.NE.I) THEN
                           TL(A,B,C,D)=TL(A,B,C,D)+
     &               2.D0*XI(I,J)*DZETA(J,J)*ME(A,B,I,J)*ME(C,D,I,J)
                        ENDIF
 73                  CONTINUE
 72               CONTINUE
 71            CONTINUE
 70         CONTINUE
 69      CONTINUE
 68   CONTINUE

      END
