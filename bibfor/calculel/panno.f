      SUBROUTINE PANNO(TYPEMA,PAN  )
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 09/01/2007   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C
C
C ======================================================================
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT NONE
      CHARACTER*8  TYPEMA
      INTEGER      PAN(*)     
C      
C ----------------------------------------------------------------------
C
C CONSTRUCTION DE BOITES ENGLOBANTES POUR UN GROUPE DE MAILLES
C
C INDICES DES PANS TOUCHANT LES NOEUDS PRINCIPAUX D'UN TYPE DE MAILLE
C
C ----------------------------------------------------------------------
C
C
C IN  TYPEMA : TYPE DE MAILLE
C OUT PAN    : PANS TOUCHANT LES NOEUDS PRINCIPAUX (DIM,NSOM)
C               POUR CHAQUE NOEUD, LES NUMEROS DES PANS LE TOUCHANT
C                EN 2D: DEUX PANS (ARETES) TOUCHENT LE NOEUD
C                EN 3D: TROIS PANS (FACES) TOUCHENT LE NOEUD
C                 ( PAN1.1, PAN1.2, [PAN1.3], PAN2.1, ... )
C                   PAN*.1 PAN*.2 [PAN*.3] TOUCHENT LE
C                   NOEUD *
C
C ----------------------------------------------------------------------
C
      IF (TYPEMA(1:4).EQ.'TRIA') THEN
        PAN(1) = 3
        PAN(2) = 1
        PAN(3) = 1
        PAN(4) = 2
        PAN(5) = 2
        PAN(6) = 3
      ELSEIF (TYPEMA(1:4).EQ.'QUAD') THEN
        PAN(1) = 4
        PAN(2) = 1
        PAN(3) = 1
        PAN(4) = 2
        PAN(5) = 2
        PAN(6) = 3
        PAN(7) = 3
        PAN(8) = 4
      ELSEIF (TYPEMA(1:5).EQ.'TETRA') THEN
        PAN(1) = 1
        PAN(2) = 2
        PAN(3) = 3
        PAN(4) = 1
        PAN(5) = 2
        PAN(6) = 4
        PAN(7) = 1
        PAN(8) = 3
        PAN(9) = 4
        PAN(10) = 2
        PAN(11) = 3
        PAN(12) = 4
      ELSEIF (TYPEMA(1:5).EQ.'PENTA') THEN
        PAN(1) = 1
        PAN(2) = 2
        PAN(3) = 4
        PAN(4) = 1
        PAN(5) = 2
        PAN(6) = 3
        PAN(7) = 1
        PAN(8) = 3
        PAN(9) = 4
        PAN(10) = 5
        PAN(11) = 2
        PAN(12) = 4
        PAN(13) = 5
        PAN(14) = 2
        PAN(15) = 3
        PAN(16) = 5
        PAN(17) = 3
        PAN(18) = 4
      ELSEIF (TYPEMA(1:4).EQ.'HEXA') THEN
        PAN(1) = 1
        PAN(2) = 2
        PAN(3) = 5
        PAN(4) = 1
        PAN(5) = 2
        PAN(6) = 3
        PAN(7) = 1
        PAN(8) = 3
        PAN(9) = 4
        PAN(10) = 1
        PAN(11) = 4
        PAN(12) = 5
        PAN(13) = 2
        PAN(14) = 5
        PAN(15) = 6
        PAN(16) = 2
        PAN(17) = 3
        PAN(18) = 6
        PAN(19) = 3
        PAN(20) = 4
        PAN(21) = 6
        PAN(22) = 4
        PAN(23) = 5
        PAN(24) = 6
      ELSE
        WRITE(6,*) 'TYPEMA: ',TYPEMA
        CALL ASSERT(.FALSE.)
      ENDIF
      END
