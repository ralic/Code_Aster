      SUBROUTINE SHBKSI(NPINT,XXG,BKSI)
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
C         DERIVEE P/R VARIABLES CANONIQUE AUX POINTS D INTEGRATION
C          POUR LES ELEMENTS CUBES A 8 NOEUDS
C   ENTREES
C     NPINT   : NBRE DE  PTS D INTEGRATION
C     XG      : COOR. CANONIQUES DES  PTS D INTEGRATION
C   SORTIES :
C     BKSI(3,8,NPINT) : LES DERIVEES
      IMPLICIT NONE

C---    VARIABLES GLOBALES

      INTEGER NPINT
      REAL*8 XG(3,5),XXG(5)
      REAL*8 BKSI(3,8,5)

C---    VARIABLES LOCALES

      REAL*8 XP(3),XM(3),AUX
      REAL*8 UN,UNS8
      INTEGER IP,I
      UN = 1.D0
      UNS8 = 0.125D0

C ON REMPLIT LE TABLEAU XG

      DO 10 IP = 1,NPINT
        XG(1,IP) = 0
        XG(2,IP) = 0
        XG(3,IP) = XXG(IP)
   10 CONTINUE

      DO 30 IP = 1,NPINT

        DO 20 I = 1,3
          XP(I) = UN + XG(I,IP)
          XM(I) = UN - XG(I,IP)
   20   CONTINUE

        AUX = -UNS8*XM(3)
        BKSI(1,1,IP) = AUX*XM(2)
        BKSI(1,2,IP) = -BKSI(1,1,IP)
        BKSI(1,3,IP) = -AUX*XP(2)
        BKSI(1,4,IP) = -BKSI(1,3,IP)
        BKSI(2,1,IP) = AUX*XM(1)
        BKSI(2,2,IP) = AUX*XP(1)
        BKSI(2,3,IP) = -BKSI(2,2,IP)
        BKSI(2,4,IP) = -BKSI(2,1,IP)
        AUX = -UNS8*XP(3)
        BKSI(1,5,IP) = AUX*XM(2)
        BKSI(1,6,IP) = -BKSI(1,5,IP)
        BKSI(1,7,IP) = -AUX*XP(2)
        BKSI(1,8,IP) = -BKSI(1,7,IP)
        BKSI(2,5,IP) = AUX*XM(1)
        BKSI(2,6,IP) = AUX*XP(1)
        BKSI(2,7,IP) = -BKSI(2,6,IP)
        BKSI(2,8,IP) = -BKSI(2,5,IP)
        AUX = -UNS8*XM(2)
        BKSI(3,1,IP) = AUX*XM(1)
        BKSI(3,2,IP) = AUX*XP(1)
        BKSI(3,5,IP) = -BKSI(3,1,IP)
        BKSI(3,6,IP) = -BKSI(3,2,IP)
        AUX = -UNS8*XP(2)
        BKSI(3,3,IP) = AUX*XP(1)
        BKSI(3,4,IP) = AUX*XM(1)
        BKSI(3,7,IP) = -BKSI(3,3,IP)
        BKSI(3,8,IP) = -BKSI(3,4,IP)
   30 CONTINUE

      END
