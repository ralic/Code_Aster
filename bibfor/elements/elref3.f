      SUBROUTINE ELREF3(ELREFZ,NDIM)
      IMPLICIT NONE
      INTEGER NDIM
      CHARACTER*(*) ELREFZ
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 26/01/2004   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.

C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.

C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================

C BUT :  RETOURNE UNE CARACTERISTIQUE DE L'ELREFE
C ----------------------------------------------------------------------
C   IN   ELREFZ : NOM DE L'ELREFE (K8)
C   OUT  NDIM   : DIMENSION DU PROBLEME
C   -------------------------------------------------------------------
      CHARACTER*8 ELREFE
C DEB ------------------------------------------------------------------

      ELREFE = ELREFZ

C     ------------------------------------------------------------------
      IF (ELREFE(1:4).EQ.'FACE') THEN
        NDIM = 2

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'MEDKTR3' .OR. ELREFE.EQ.'MEGRDKT' .OR.
     &         ELREFE.EQ.'MEDSTR3' .OR. ELREFE.EQ.'MEGRDKT' .OR.
     &         ELREFE.EQ.'MEDKQU4' .OR. ELREFE.EQ.'MEGRDKQ' .OR.
     &         ELREFE.EQ.'MEDSQU4' .OR. ELREFE.EQ.'MEQ4QU4') THEN
        NDIM = 2

C     ------------------------------------------------------------------
      ELSE IF (ELREFE(1:3).EQ.'TRI') THEN
        NDIM = 2

C     ------------------------------------------------------------------
      ELSE IF (ELREFE(1:3).EQ.'QUA') THEN
        NDIM = 2

      ELSE
        CALL ASSERT(.FALSE.)
      END IF

      END
