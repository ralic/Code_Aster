      SUBROUTINE DEPSH6(LOOP,BLOC,UELOC,DEPS,D)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 12/08/2008   AUTEUR DESROCHES X.DESROCHES 
C ======================================================================
C COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
C
C      CALCUL DES DERIVEES DUX/DX DVX/DY ETC....     H.  BUNG   FEV 83
C          (ELEMENTS CUB6 )
C
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
C
      IMPLICIT NONE
      INTEGER LOOP,I
      REAL *8 BLOC(6,18),UELOC(3,6)
      REAL *8 D(9),DEPS(6),UTEMP(18)
C
      CALL R8INIR(9,0.D0,D,1)
      CALL R8INIR(18,0.D0,UTEMP,1)
      DO  20 I=1,6
         UTEMP(I)    = UELOC(1,I)
         UTEMP(I+6)  = UELOC(2,I)
         UTEMP(I+12) = UELOC(3,I)
20    CONTINUE
C
      CALL R8INIR(6,0.D0,DEPS,1)
      DO  30 I=1,18
         DEPS(1) = DEPS(1) + BLOC(1,I)*UTEMP(I)
         DEPS(2) = DEPS(2) + BLOC(2,I)*UTEMP(I)
         DEPS(3) = DEPS(3) + BLOC(3,I)*UTEMP(I)
         DEPS(4) = DEPS(4) + BLOC(4,I)*UTEMP(I)
         DEPS(5) = DEPS(5) + BLOC(5,I)*UTEMP(I)
         DEPS(6) = DEPS(6) + BLOC(6,I)*UTEMP(I)
30    CONTINUE

      D(1) = DEPS(1)
      D(2) = DEPS(4)/0.5D0
      D(3) = DEPS(6)/0.5D0
      D(4) = DEPS(4)/0.5D0
      D(5) = DEPS(2)
      D(6) = DEPS(5)/0.5D0
      D(7) = DEPS(6)/0.5D0
      D(8) = DEPS(5)/0.5D0
      D(9) = DEPS(3)

      IF(LOOP.EQ.2) THEN
        DEPS(1) = DEPS(1) - 0.5D0*(D(1)*D(1) + D(2)*D(2) + D(3)*D(3))
        DEPS(2) = DEPS(2) - 0.5D0*(D(4)*D(4) + D(5)*D(5) + D(6)*D(6))
        DEPS(3) = DEPS(3) - 0.5D0*(D(7)*D(7) + D(8)*D(8) + D(9)*D(9))

        DEPS(4) = DEPS(4) - D(1)*D(4) - D(2)*D(5) - D(3)*D(6)
        DEPS(5) = DEPS(5) - D(4)*D(7) - D(5)*D(8) - D(6)*D(9)
        DEPS(6) = DEPS(6) - D(1)*D(7) - D(2)*D(8) - D(3)*D(9)
      ENDIF

      END
