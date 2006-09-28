      SUBROUTINE SHVROT(RR,X,NN)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
      IMPLICIT NONE
      REAL *8 RR(3,3)
      INTEGER NN
      REAL *8 X(24)
C ---   VARIABLES LOCALES
      REAL *8 AA(24),R(3,3)
      INTEGER I,J
      IF(NN.EQ.2) THEN
        DO 10 I=1,3
           DO 20 J=1,3
              R(I,J)= RR(J,I)
20           CONTINUE
10        CONTINUE
      ELSEIF(NN.EQ.1) THEN
        DO 30 I=1,3
          DO 40 J=1,3
              R(I,J)= RR(I,J)
40          CONTINUE
30        CONTINUE
      ELSE
        CALL U2MESS('F','ELEMENTS2_26')
      ENDIF

      AA(1) = R(1,1)*X(1) + R(1,2)*X(2) + R(1,3)*X(3)
      AA(2) = R(2,1)*X(1) + R(2,2)*X(2) + R(2,3)*X(3)
      AA(3) = R(3,1)*X(1) + R(3,2)*X(2) + R(3,3)*X(3)

      AA(4) = R(1,1)*X(4) + R(1,2)*X(5) + R(1,3)*X(6)
      AA(5) = R(2,1)*X(4) + R(2,2)*X(5) + R(2,3)*X(6)
      AA(6) = R(3,1)*X(4) + R(3,2)*X(5) + R(3,3)*X(6)

      AA(7) = R(1,1)*X(7) + R(1,2)*X(8) + R(1,3)*X(9)
      AA(8) = R(2,1)*X(7) + R(2,2)*X(8) + R(2,3)*X(9)
      AA(9) = R(3,1)*X(7) + R(3,2)*X(8) + R(3,3)*X(9)

      AA(10) = R(1,1)*X(10) + R(1,2)*X(11) + R(1,3)*X(12)
      AA(11) = R(2,1)*X(10) + R(2,2)*X(11) + R(2,3)*X(12)
      AA(12) = R(3,1)*X(10) + R(3,2)*X(11) + R(3,3)*X(12)

      AA(13) = R(1,1)*X(13) + R(1,2)*X(14) + R(1,3)*X(15)
      AA(14) = R(2,1)*X(13) + R(2,2)*X(14) + R(2,3)*X(15)
      AA(15) = R(3,1)*X(13) + R(3,2)*X(14) + R(3,3)*X(15)

      AA(16) = R(1,1)*X(16) + R(1,2)*X(17) + R(1,3)*X(18)
      AA(17) = R(2,1)*X(16) + R(2,2)*X(17) + R(2,3)*X(18)
      AA(18) = R(3,1)*X(16) + R(3,2)*X(17) + R(3,3)*X(18)

      AA(19) = R(1,1)*X(19) + R(1,2)*X(20) + R(1,3)*X(21)
      AA(20) = R(2,1)*X(19) + R(2,2)*X(20) + R(2,3)*X(21)
      AA(21) = R(3,1)*X(19) + R(3,2)*X(20) + R(3,3)*X(21)

      AA(22) = R(1,1)*X(22) + R(1,2)*X(23) + R(1,3)*X(24)
      AA(23) = R(2,1)*X(22) + R(2,2)*X(23) + R(2,3)*X(24)
      AA(24) = R(3,1)*X(22) + R(3,2)*X(23) + R(3,3)*X(24)

      DO 50 I=1,24
          X(I)= AA(I)
50      CONTINUE


      END
