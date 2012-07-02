      INTEGER FUNCTION INDIIS(LIS,IS,RANG,NBIS)
      IMPLICIT NONE
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     ARGUMENTS:
C     ----------
      INTEGER NBIS,RANG
      INTEGER IS,LIS(*)
C ----------------------------------------------------------------------
C     ENTREES:
C     LIS : LISTE DE IS OU ON DOIT CHERCHER L'ENTIER IS
C     IS  : ENTIER A CHERCHER
C     NBIS: NOMBRE D'ENTIERS DE LA LISTE
C     RANG: ON CHERCHE LE RANG-IEME ENTIER IS DANS LA LISTE.
C
C     SORTIES:
C     INDIIS : POSITION DE L'ENTIER DANS LA LISTE
C           SI L'ENTIER EST ABSENT: INDIIS=0
C
C ----------------------------------------------------------------------
C
C     VARIABLES LOCALES:
C     ------------------
      INTEGER I,J
C DEB-------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      J = 0
      DO 100 I = 1,NBIS
         IF (LIS(I).EQ.IS) THEN
            J = J + 1
            IF (J.EQ.RANG) GO TO 110
         END IF
  100 CONTINUE
      INDIIS = 0
      GO TO 120
  110 CONTINUE
      INDIIS = I
  120 CONTINUE
      END
