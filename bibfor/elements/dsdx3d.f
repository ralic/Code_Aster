      SUBROUTINE DSDX3D(LOOP,B,U,DEPS,D,NBN)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 12/08/2008   AUTEUR DESROCHES X.DESROCHES 
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

C      CALCUL DES DERIVEES DUX/DX DVX/DY ETC....     H.  BUNG   FEV 83
C          (ELEMENTS CUB6 )

C ENTREES :
C       B     :  MATRICE (B)
C      U     :  TABLEAU DES VALEURS NODALES
C    LOOP    =  2 ON CALCULE LES DEFORMATIONS JUSQU'AU 2E ORDRE
C               SUR LA CONFIGURATION (N+1)
C
C SORTIES :
C      D       : D(1)=DU1/DX1 ;  D(2)=DU2/DX1 ; D(3)=DU3/DX1 ;
C              : D(4)=DU1/DX2 ;  D(5)=DU2/DX2 ; D(6)=DU3/DX2 ;
C              : D(7)=DU1/DX3 ;  D(8)=DU2/DX3 ; D(9)=DU3/DX3
C      DEPS  : TENSEUR DE DEFORMATION ASSOCIE A   <U>
C               DEPS(1)=D(1)     ; DEPS(2)=D(5)    ; DEPS(3)=D(9)
C               DEPS(4)=D(2)+D(4); DEPS(5)=D(6)+D(8); DEPS(6)=D(3)+D(7)
C
C      AVEC EVENTUELLEMENT LES TERMES DU 2EME  ORDRE


      IMPLICIT NONE

      INTEGER NBN,LOOP
      REAL *8 B(3,NBN),U(3,NBN),D(9),DEPS(6)
      REAL *8 B1I,B2I,B3I,U1I,U2I,U3I
      REAL *8 D1,D2,D3,D4,D5,D6,D7,D8,D9


      INTEGER I


      DO 10 I=1,9
        D(I) = 0.D0
10    CONTINUE

      D1 = 0.D0
      D2 = 0.D0
      D3 = 0.D0
      D4 = 0.D0
      D5 = 0.D0
      D6 = 0.D0
      D7 = 0.D0
      D8 = 0.D0
      D9 = 0.D0

      DO  20 I=1,NBN
        B1I = B(1,I)
        B2I = B(2,I)
        B3I = B(3,I)
        U1I = U(1,I)
        U2I = U(2,I)
        U3I = U(3,I)
        D1 = D1 + B1I*U1I
        D2 = D2 + B1I*U2I
        D3 = D3 + B1I*U3I
        D4 = D4 + B2I*U1I
        D5 = D5 + B2I*U2I
        D6 = D6 + B2I*U3I
        D7 = D7 + B3I*U1I
        D8 = D8 + B3I*U2I
        D9 = D9 + B3I*U3I

20    CONTINUE

      D(1) = D1
      D(2) = D2
      D(3) = D3
      D(4) = D4
      D(5) = D5
      D(6) = D6
      D(7) = D7
      D(8) = D8
      D(9) = D9

      DEPS(1) = D1
      DEPS(2) = D5
      DEPS(3) = D9
      DEPS(4) = D2 + D4
      DEPS(5) = D6 + D8
      DEPS(6) = D3 + D7

      IF(LOOP.EQ.2) THEN
        DEPS(1) = DEPS(1) - 0.5D0*(D1*D1 + D2*D2 + D3*D3)
        DEPS(2) = DEPS(2) - 0.5D0*(D4*D4 + D5*D5 + D6*D6)
        DEPS(3) = DEPS(3) - 0.5D0*(D7*D7 + D8*D8 + D9*D9)

        DEPS(4) = DEPS(4) - D1*D4 - D2*D5 - D3*D6
        DEPS(5) = DEPS(5) - D4*D7 - D5*D8 - D6*D9
        DEPS(6) = DEPS(6) - D1*D7 - D2*D8 - D3*D9
      ENDIF

      END
