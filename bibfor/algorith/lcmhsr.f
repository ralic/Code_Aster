      SUBROUTINE LCMHSR (NECOUL,NECRIS,NBSYS, NBCOEF, COEFH, HSR)
      IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/09/2011   AUTEUR PROIX J-M.PROIX 
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
C RESPONSABLE PROIX J-M.PROIX
C     ----------------------------------------------------------------
C     MONOCRISTAL : CALCUL DE LA MATRICE D'INTERACTION HSR
C     ----------------------------------------------------------------
C     IN  NOMFAM :  NOM DE LA FAMILLE DE GLISSEMENT
C         NBSYS  :  NOMBRE DE SYSTEMES DE GLISSEMENT
C         NBCOEF  :  NOMBRE DE COEFFICIENTS
C         COEFH  :  COEFFICIENTS H1 A H6
C     OUT HSR    :  MATRICE D'INTERACTION
C     ----------------------------------------------------------------
      INTEGER NBCOEF,NBSYS,IR,IS,I,J
      REAL*8  COEFH(6),HSR(30,30),H
      REAL*8  A1(3,3),A2(3,3),A3(3,3),A4(3,3)
      CHARACTER*16 NECRIS,NECOUL
C     ----------------------------------------------------------------
C

      IF (NBCOEF.EQ.1) THEN
          H=COEFH(1)
C  MATRICE D INTERACTION (NBSYS*NBSYS): 1 SUR LA DIAGONALE, H AILLEURS
          DO 507 IS = 1, NBSYS
             DO 508 IR = 1, NBSYS
                   HSR(IS,IR) = H
  508        CONTINUE
  507     CONTINUE
          DO 509 IS = 1, NBSYS
                   HSR(IS,IS) = 1.D0
  509     CONTINUE

      ELSEIF (NECRIS(1:9).EQ.'MONO_DD_C') THEN

C  MATRICE D INTERACTION (12*12): 5 COEFFICIENTS DD_CFC
C  DEFINITION SELON G.MONET
          CALL LCMHDD(NECOUL,NECRIS,NBSYS,NBCOEF,COEFH,HSR)

      ELSEIF (NBCOEF.EQ.4) THEN

C  MATRICE D INTERACTION (24*24): 4 COEFFICIENTS (BCC24)

          IF (NBSYS.NE.24) CALL U2MESS('F','COMPOR1_23')


          CALL R8INIR ( 3*3, COEFH(1) , A1, 1 )
          CALL R8INIR ( 3*3, COEFH(2) , A2, 1 )
          CALL R8INIR ( 3*3, COEFH(3) , A3, 1 )
          CALL R8INIR ( 3*3, COEFH(4) , A4, 1 )

C         DEFINITION DE LA MATRICE D INTERACTION BCC24

          CALL LCICMA(A1,3,3,3,3,1,1,HSR,30,30,16,1)
          CALL LCICMA(A1,3,3,3,3,1,1,HSR,30,30,19,1)
          CALL LCICMA(A1,3,3,3,3,1,1,HSR,30,30,22,1)
          CALL LCICMA(A1,3,3,3,3,1,1,HSR,30,30,13,4)
          CALL LCICMA(A1,3,3,3,3,1,1,HSR,30,30,19,4)
          CALL LCICMA(A1,3,3,3,3,1,1,HSR,30,30,22,4)
          CALL LCICMA(A1,3,3,3,3,1,1,HSR,30,30,13,7)
          CALL LCICMA(A1,3,3,3,3,1,1,HSR,30,30,16,7)
          CALL LCICMA(A1,3,3,3,3,1,1,HSR,30,30,22,7)
          CALL LCICMA(A1,3,3,3,3,1,1,HSR,30,30,13,10)
          CALL LCICMA(A1,3,3,3,3,1,1,HSR,30,30,16,10)
          CALL LCICMA(A1,3,3,3,3,1,1,HSR,30,30,19,10)


          CALL LCICMA(A2,3,3,3,3,1,1,HSR,30,30,1,1)
          CALL LCICMA(A2,3,3,3,3,1,1,HSR,30,30,4,4)
          CALL LCICMA(A2,3,3,3,3,1,1,HSR,30,30,7,7)
          CALL LCICMA(A2,3,3,3,3,1,1,HSR,30,30,10,10)
          CALL LCICMA(A2,3,3,3,3,1,1,HSR,30,30,13,1)
          CALL LCICMA(A2,3,3,3,3,1,1,HSR,30,30,16,4)
          CALL LCICMA(A2,3,3,3,3,1,1,HSR,30,30,19,7)
          CALL LCICMA(A2,3,3,3,3,1,1,HSR,30,30,22,10)

          CALL LCICMA(A3,3,3,3,3,1,1,HSR,30,30,16,13)
          CALL LCICMA(A3,3,3,3,3,1,1,HSR,30,30,19,13)
          CALL LCICMA(A3,3,3,3,3,1,1,HSR,30,30,22,13)
          CALL LCICMA(A3,3,3,3,3,1,1,HSR,30,30,19,16)
          CALL LCICMA(A3,3,3,3,3,1,1,HSR,30,30,22,16)
          CALL LCICMA(A3,3,3,3,3,1,1,HSR,30,30,22,19)

          CALL LCICMA(A4,3,3,3,3,1,1,HSR,30,30,13,13)
          CALL LCICMA(A4,3,3,3,3,1,1,HSR,30,30,16,16)
          CALL LCICMA(A4,3,3,3,3,1,1,HSR,30,30,19,19)
          CALL LCICMA(A4,3,3,3,3,1,1,HSR,30,30,22,22)

          CALL LCICMA(A1,3,3,3,3,1,1,HSR,30,30,4,1)
          HSR(4,1)=COEFH(2)
          CALL LCICMA(A1,3,3,3,3,1,1,HSR,30,30,7,1)
          HSR(7,2)=COEFH(2)
          CALL LCICMA(A1,3,3,3,3,1,1,HSR,30,30,10,1)
          HSR(10,3)=COEFH(2)
          CALL LCICMA(A1,3,3,3,3,1,1,HSR,30,30,7,4)
          HSR(8,6)=COEFH(2)
          CALL LCICMA(A1,3,3,3,3,1,1,HSR,30,30,10,4)
          HSR(12,5)=COEFH(2)
          CALL LCICMA(A1,3,3,3,3,1,1,HSR,30,30,10,7)
          HSR(11,9)=COEFH(2)

      ELSEIF (NBCOEF.EQ.6) THEN

C  MATRICE D INTERACTION (12*12): 6 COEFFICIENTS CF ZMAT

          IF (NBSYS.NE.12) CALL U2MESS('F','COMPOR1_24')

          DO 10 I=1,12
             HSR(I,I)=COEFH(1)
  10      CONTINUE
          HSR(2,1)=COEFH(2)
          HSR(3,1)=COEFH(2)
          HSR(3,2)=COEFH(2)
          HSR(4,1)=COEFH(4)
          HSR(4,2)=COEFH(5)
          HSR(4,3)=COEFH(5)
          HSR(5,1)=COEFH(5)
          HSR(5,2)=COEFH(3)
          HSR(5,3)=COEFH(6)
          HSR(5,4)=COEFH(2)
          HSR(6,1)=COEFH(5)
          HSR(6,2)=COEFH(6)
          HSR(6,3)=COEFH(3)
          HSR(6,4)=COEFH(2)
          HSR(6,5)=COEFH(2)
          HSR(7,1)=COEFH(5)
          HSR(7,2)=COEFH(4)
          HSR(7,3)=COEFH(5)
          HSR(7,4)=COEFH(6)
          HSR(7,5)=COEFH(3)
          HSR(7,6)=COEFH(5)
          HSR(8,1)=COEFH(6)
          HSR(8,2)=COEFH(5)
          HSR(8,3)=COEFH(3)
          HSR(8,4)=COEFH(5)
          HSR(8,5)=COEFH(5)
          HSR(8,6)=COEFH(4)
          HSR(8,7)=COEFH(2)
          HSR(9,1)=COEFH(3)
          HSR(9,2)=COEFH(5)
          HSR(9,3)=COEFH(6)
          HSR(9,4)=COEFH(3)
          HSR(9,5)=COEFH(6)
          HSR(9,6)=COEFH(5)
          HSR(9,7)=COEFH(2)
          HSR(9,8)=COEFH(2)
          HSR(10,1)=COEFH(5)
          HSR(10,2)=COEFH(5)
          HSR(10,3)=COEFH(4)
          HSR(10,4)=COEFH(6)
          HSR(10,5)=COEFH(5)
          HSR(10,6)=COEFH(3)
          HSR(10,7)=COEFH(6)
          HSR(10,8)=COEFH(3)
          HSR(10,9)=COEFH(5)
          HSR(11,1)=COEFH(3)
          HSR(11,2)=COEFH(6)
          HSR(11,3)=COEFH(5)
          HSR(11,4)=COEFH(3)
          HSR(11,5)=COEFH(5)
          HSR(11,6)=COEFH(6)
          HSR(11,7)=COEFH(5)
          HSR(11,8)=COEFH(5)
          HSR(11,9)=COEFH(4)
          HSR(11,10)=COEFH(2)
          HSR(12,1)=COEFH(6)
          HSR(12,2)=COEFH(3)
          HSR(12,3)=COEFH(5)
          HSR(12,4)=COEFH(5)
          HSR(12,5)=COEFH(4)
          HSR(12,6)=COEFH(5)
          HSR(12,7)=COEFH(3)
          HSR(12,8)=COEFH(6)
          HSR(12,9)=COEFH(5)
          HSR(12,10)=COEFH(2)
          HSR(12,11)=COEFH(2)

      ELSE
          CALL U2MESS('F','COMPOR1_25')
      ENDIF

      DO 1 I=1,NBSYS
      DO 1 J=1,I
         HSR(J,I)=HSR(I,J)
 1    CONTINUE

      END
