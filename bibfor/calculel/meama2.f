      SUBROUTINE MEAMA2(MODELE,NCHAR,LCHAR,MATE,MATEL,PREFCH)
      IMPLICIT NONE

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================

C     ARGUMENTS:
C     ----------
      INCLUDE 'jeveux.h'
      CHARACTER*8 MODELE,LCHAR(*)
      CHARACTER*19 MATEL,PREFCH
      CHARACTER*(*) MATE
      INTEGER NCHAR
C ----------------------------------------------------------------------

C*    CALCUL DES MATRICES ELEMENTAIRES D'AMORTISSEMENT ACOUSTIQUE
C*           ( ISO_FACE, 'AMOR_ACOU      R/F' )

C     LES RESUELEM PRODUITS S'APPELLENT :
C           PREFCH(1:8).ME000I , I=1,NCHAR

C     ENTREES:

C     LES NOMS QUI SUIVENT SONT LES PREFIXES UTILISATEUR K8:
C        MODELE : NOM DU MODELE
C        NCHAR  : NOMBRE DE CHARGES
C        LCHAR  : LISTE DES CHARGES
C*       MATE   : CARTE DE MATERIAU CODE
C        MATEL  : NOM DU MATELE (N RESUELEM) PRODUIT
C        PREFCH : PREFIXE DES NOMS DES RESUELEM STOCKES DANS MATEL
C*

C     SORTIES:
C        MATEL  : EST REMPLI.

C ----------------------------------------------------------------------

C     FONCTIONS EXTERNES:
C     -------------------

C     VARIABLES LOCALES:
C     ------------------
C*
      LOGICAL EXIGEO
C*
      CHARACTER*8 LPAIN(4),LPAOUT(1)
      CHARACTER*16 OPTION
C*
      CHARACTER*24 CHGEOM,CHIMPE,LCHIN(3),LCHOUT(1)
      CHARACTER*24 LIGRMO

C-----------------------------------------------------------------------
      INTEGER ICHA ,ILIRES ,IRET 
C-----------------------------------------------------------------------
      CALL JEMARQ()
      CALL MEGEOM(MODELE,LCHAR(1),EXIGEO,CHGEOM)
C*
      CALL JEEXIN(MATEL//'.RERR',IRET)
      IF (IRET.GT.0) THEN
        CALL JEDETR(MATEL//'.RERR')
        CALL JEDETR(MATEL//'.RELR')
      END IF
      CALL MEMARE('V',MATEL,MODELE,MATE,' ','AMOR_ACOU')
C*
      LPAOUT(1) = 'PMATTTC'
      LCHOUT(1) = PREFCH(1:8)//'.ME000'
      ILIRES = 0
      IF (LCHAR(1).NE.'        ') THEN

        LIGRMO = MODELE//'.MODELE'
        LPAIN(1) = 'PGEOMER'
        LCHIN(1) = CHGEOM
C**
        LPAIN(3) = 'PMATERC'
        LCHIN(3) = MATE
C**
        DO 10 ICHA = 1,NCHAR
C**
          OPTION = 'AMOR_ACOU'
          LPAIN(2) = 'PIMPEDC'
C**
C**
          CHIMPE = LCHAR(ICHA)//'.CHAC.IMPED.DESC'
          CALL JEEXIN(CHIMPE,IRET)
C**
          IF (IRET.EQ.0) GO TO 10
C*
          LCHIN(2) = CHIMPE
          ILIRES = ILIRES + 1
          CALL CODENT(ILIRES,'D0',LCHOUT(1) (12:14))
          CALL CALCUL('S',OPTION,LIGRMO,3,LCHIN,LPAIN,1,LCHOUT,LPAOUT,
     &                'G','OUI')
          CALL REAJRE(MATEL,LCHOUT(1),'G')
   10   CONTINUE
      END IF
      CALL JEDEMA()
      END
