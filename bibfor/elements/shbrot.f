      SUBROUTINE SHBROT(X,RR)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
C--------------------------------------------------------
C ELEMENT SHB8-PS A.COMBESCURE, S.BAGUET INSA LYON 2003 /
C-------------------------------------------------------
      IMPLICIT NONE
C ---   VARIABLES GLOBALES
      REAL *8 X(24)
      REAL *8 RR(3,3)
C ---   VARIABLES LOCALES
      REAL *8 AUX,AA
C      DATA VB/-1.D0, 1.D0, 1.D0,-1.D0,-1.D0, 1.D0, 1.D0,-1.D0,
C     &        -1.D0,-1.D0, 1.D0, 1.D0,-1.D0,-1.D0, 1.D0, 1.D0,
C     &        -1.D0,-1.D0,-1.D0,-1.D0, 1.D0, 1.D0, 1.D0, 1.D0/
      RR(1,1)= - X(1)  +  X(4)  +  X(7)  -   X(10)
     &                  - X(13) +  X(16) +  X(19) -   X(22)
      RR(1,2)= - X(2)  +  X(5)  +  X(8)  -   X(11)
     &                  - X(14) +  X(17) +  X(20) -   X(23)
      RR(1,3)= - X(3)  +  X(6)  +  X(9)  -   X(12)
     &                  - X(15) +  X(18) +  X(21) -   X(24)
C
      RR(2,1)= - X(1)  -  X(4)  +  X(7)  +   X(10)
     &                  - X(13) -  X(16) +  X(19) +  X(22)
      RR(2,2)= - X(2)  -  X(5)  +  X(8)  +   X(11)
     &                  - X(14) -  X(17) +  X(20) +  X(23)
      RR(2,3)= - X(3)  -  X(6)  +  X(9)  +   X(12)
     &                  - X(15) -  X(18) +  X(21) +  X(24)

      AUX= RR(1,1)*RR(1,1)+RR(1,2)*RR(1,2)+RR(1,3)*RR(1,3)
      AA = RR(1,1)*RR(2,1)+RR(1,2)*RR(2,2)+RR(1,3)*RR(2,3)
      AA = -(AA/AUX)

      RR(2,1)= RR(2,1) + AA*RR(1,1)
      RR(2,2)= RR(2,2) + AA*RR(1,2)
      RR(2,3)= RR(2,3) + AA*RR(1,3)

      RR(3,1)= RR(1,2)*RR(2,3) - RR(1,3)*RR(2,2)
      RR(3,2)= RR(1,3)*RR(2,1) - RR(1,1)*RR(2,3)
      RR(3,3)= RR(1,1)*RR(2,2) - RR(1,2)*RR(2,1)

      AUX = SQRT(AUX)
      RR(1,1)= RR(1,1)/AUX
      RR(1,2)= RR(1,2)/AUX
      RR(1,3)= RR(1,3)/AUX

      AUX = SQRT(RR(2,1)*RR(2,1)+RR(2,2)*RR(2,2)+RR(2,3)*RR(2,3))
      RR(2,1)= RR(2,1)/AUX
      RR(2,2)= RR(2,2)/AUX
      RR(2,3)= RR(2,3)/AUX

      AUX = SQRT(RR(3,1)*RR(3,1)+RR(3,2)*RR(3,2)+RR(3,3)*RR(3,3))
      RR(3,1)= RR(3,1)/AUX
      RR(3,2)= RR(3,2)/AUX
      RR(3,3)= RR(3,3)/AUX

      END
