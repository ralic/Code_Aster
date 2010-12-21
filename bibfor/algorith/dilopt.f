      SUBROUTINE DILOPT(DIMDEF,DIMUEL,POIDS,POIDS2,B,DRDE,MATUU)
C ======================================================================
       IMPLICIT NONE
       INTEGER  DIMDEF,DIMUEL
       REAL*8   POIDS,POIDS2,DRDE(DIMDEF,DIMDEF),B(DIMDEF,DIMUEL)
       REAL*8   MATUU(DIMUEL*DIMUEL)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 20/12/2010   AUTEUR PELLET J.PELLET 
C TOLE CRS_1404
C ======================================================================
C COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ======================================================================
C --- BUT : ASSEMBLAGE DE L OPERATEUR TANGENT POUR LA PARTIE -----------
C ---       SECOND GRADIENT POUR L ELEMENT CALCULE ---------------------
C ======================================================================
C --- VARIABLES LOCALES ------------------------------------------------
C ======================================================================
      INTEGER    I,J,KJI
      REAL*8     MATR1(DIMDEF,DIMUEL),MATRI(DIMUEL,DIMUEL)
C ======================================================================
      DO 10 I=1,DIMUEL
         DO 20 J=1,DIMDEF
            MATR1(J,I)=0.0D0
 20      CONTINUE
         DO 50 J=1,DIMUEL
            MATRI(J,I)=0.0D0
 50      CONTINUE
 10   CONTINUE
C ======================================================================
      CALL DGEMM('N','N',DIMDEF,DIMUEL,DIMDEF,1.0D0,DRDE,DIMDEF,B,
     +                                        DIMDEF,0.0D0,MATR1,DIMDEF)

      CALL DGEMM('T','N',DIMUEL,DIMUEL,DIMDEF,POIDS,B,DIMDEF,
     +                                MATR1,DIMDEF,0.0D0,MATRI,DIMUEL)
C ======================================================================
      KJI=1
      DO 30 I=1,DIMUEL
         DO 40 J=1,DIMUEL
            MATUU(KJI) = MATUU(KJI)+MATRI(I,J)
            KJI = KJI+1
 40      CONTINUE
 30   CONTINUE
C ======================================================================
      END
