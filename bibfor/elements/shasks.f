      SUBROUTINE SHASKS(S,B,K)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 S(6),B(3,8),K(8,8)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 29/09/2003   AUTEUR JMBHH01 J.M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C--------------------------------------------------------
C ELEMENT SHB8-PS A.COMBESCURE, S.BAGUET INSA LYON 2003 /
C-------------------------------------------------------
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C CALCUL ET ASSEMBLE LA MATRICE 8*8 K_SIGMA ---> K
C RQ: ELLE EST DIAGONALE PAR BLOCS DE 8*8, EGAUX
C B : DERIVEES DES FONCTIONS DE FORMES
C S : CONTRAINTES
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DO 20 J = 1,8
        DO 10 I = 1,8
          K(I,J) = 0.D0
   10   CONTINUE
   20 CONTINUE

      DO 40 J = 1,8
        DO 30 I = 1,8
          K(I,J) = S(1)*B(1,I)*B(1,J) + S(2)*B(2,I)*B(2,J) +
     &             S(3)*B(3,I)*B(3,J) + S(4)*
     &             (B(1,I)*B(2,J)+B(2,I)*B(1,J)) +
     &             S(6)* (B(1,I)*B(3,J)+B(3,I)*B(1,J)) +
     &             S(5)* (B(3,I)*B(2,J)+B(2,I)*B(3,J))
   30   CONTINUE
   40 CONTINUE

      END
