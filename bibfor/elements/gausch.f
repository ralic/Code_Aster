      SUBROUTINE GAUSCH(NPGXYZ,XPG,YPG,ZPG,HPG)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 15/04/96   AUTEUR GJBHHHH H.HADDAR 
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
C
C......................................................................C
C......................................................................C
C                                                                      C
C BUT: CALCUL DES POIDS ET POINTS DE GAUSS POUR DES POLYNOMES          C
C      NON HOMOGENE A TROIS VARIABLES                                  C
C                                                                      C
C ENTREES  ---> NPGXZ(I)    : NOMBRE DE POINTS DE GAUSS DANS           C
C                             LA DIRECTION "I"                         C
C                                                                      C
C SORTIES  <--- XPG,YPG,ZPG : COORDONNEES DES POINTS DE GAUSS          C
C          <--- HPG         : POIDS DES POINTS DE GAUSS                C
C                                                                      C
C......................................................................C
C......................................................................C
C
      IMPLICIT     NONE
      INTEGER      NPGXYZ(3)
      REAL*8       XPG(1),YPG(1),ZPG(1),HPG(1)
C----------------------------------------------------------------------
      INTEGER      NPARI,I,J,K,NPI
      REAL*8       A(4),H(4),COORD(3,4),HPGXYZ(3,4)
C----------------------------------------------------------------------
C
      DO 10 I = 1, 3
C
         IF (NPGXYZ(I).EQ.2) THEN
C
           NPARI = 2
           A(1) = -1.D0/(SQRT(3.D0))
           A(2) = -A(1)
           H(1) = 1.D00
           H(2) = 1.D00
C
         ELSE IF (NPGXYZ(I).EQ.3) THEN
C
           NPARI = 3
           A(1) = -SQRT(3.D0/5.D0)
           A(2) =  0.D00
           A(3) = -A(1)
           H(1) =  5.D0/9.D0
           H(2) =  8.D0/9.D0
           H(3) =  H(1)
C
         ELSE IF (NPGXYZ(I).EQ.4) THEN
C
           NPARI = 4
           A(1) = -SQRT((3.D0+2.D0*SQRT(6.D0/5.D0))/7.D0)
           A(2) = -SQRT((3.D0-2.D0*SQRT(6.D0/5.D0))/7.D0)
           A(3) = -A(2)
           A(4) = -A(1)
           H(1) = .5D0-1.D0/(6.D0*SQRT(6.D0/5.D0))
           H(2) = .5D0+1.D0/(6.D0*SQRT(6.D0/5.D0))
           H(3) = H(2)
           H(4) = H(1)
C
        ENDIF
C
          DO 20 J = 1, NPARI
            COORD(I,J) = A(J)
            HPGXYZ(I,J) = H(J)
 20      CONTINUE
 10   CONTINUE
      NPI = 0
      DO 30 I = 1, NPGXYZ(1)
        DO 30 J = 1, NPGXYZ(2)
           DO 30 K = 1,NPGXYZ(3)
              NPI = NPI + 1
              XPG( NPI ) = COORD(1,I)
              YPG( NPI ) = COORD(2,J)
              ZPG( NPI ) = COORD(3,K)
              HPG( NPI ) = HPGXYZ(1,I)*HPGXYZ(2,J)*HPGXYZ(3,K)
 30   CONTINUE
C
C------------------------------------------------------------
      END
