      SUBROUTINE BPTOBG (M,N,P)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 27/06/2000   AUTEUR CIBHHBC B.CIREE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C ======================================================================
      IMPLICIT NONE
      REAL*8             M(6),N(6),P(3,3)
C ----------------------------------------------------------------------
C CHANGEMENT DE BASE DE LA BASE PROPRE A LA BASE INITIALE DONT LES
C    VECTEURS PROPRES SONT RANGES DANS P POUR DES MATRICES SYMETRIQUES
C
C IN  M       : MATRICE DANS BP
C IN  P       : MATRICE DE PASSAGE B->BP
C OUT N       : MATRICE M DANS B
C ----------------------------------------------------------------------
      INTEGER I,J,T(3,3)
      REAL*8  TEMP
      N(1)=0.D0
      N(2)=0.D0
      N(3)=0.D0
      N(4)=0.D0
      N(5)=0.D0
      N(6)=0.D0
      T(1,1)=1
      T(1,2)=4
      T(1,3)=5
      T(2,1)=4
      T(2,2)=2
      T(2,3)=6
      T(3,1)=5
      T(3,2)=6
      T(3,3)=3
      DO 10 I=1,3
        DO 11 J=1,3
          TEMP=M(T(I,J))
          N(1)=N(1)+P(1,I)*P(1,J)*TEMP
          N(2)=N(2)+P(2,I)*P(2,J)*TEMP
          N(3)=N(3)+P(3,I)*P(3,J)*TEMP
          N(4)=N(4)+P(1,I)*P(2,J)*TEMP
          N(5)=N(5)+P(1,I)*P(3,J)*TEMP
          N(6)=N(6)+P(2,I)*P(3,J)*TEMP
11      CONTINUE
10    CONTINUE
      END
