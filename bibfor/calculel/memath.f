      SUBROUTINE MEMATH(OPTION,MODELE,MATE,CARA,TIME,MATEL)
      IMPLICIT REAL*8 (A-H,O-Z)

C MODIF CALCULEL  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
      CHARACTER*8 MODELE,CARA
      CHARACTER*19 MATEL
      CHARACTER*16 OPTION
      CHARACTER*24 TIME,MATE
C ----------------------------------------------------------------------

C     CALCUL DES MATRICES ELEMENTAIRES DE MASSE_THERMIQUE
C                ( 'MASS_THER', ISO )

C     ENTREES:

C     LES NOMS QUI SUIVENT SONT LES PREFIXES UTILISATEUR K8:
C        MODELE : NOM DU MODELE
C        MATE   : CHAMP DE MATERIAUX
C        CARA   : CHAMP DE CARAC_ELEM
C        TIME   : CHAMP DE TEMPSR
C        MATEL  : NOM DU MAT_ELE(N RESUELEM) PRODUIT

C     SORTIES:
C        MATEL  : EST CALCULE

C ----------------------------------------------------------------------

C     FONCTIONS EXTERNES:
C     -------------------

C     VARIABLES LOCALES:
C     ------------------
C---------------- COMMUNS NORMALISES  JEVEUX  --------------------------
      COMMON /IVARJE/ZI(1)
      COMMON /RVARJE/ZR(1)
      COMMON /CVARJE/ZC(1)
      COMMON /LVARJE/ZL(1)
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      INTEGER ZI
      REAL*8 ZR
      COMPLEX*16 ZC
      LOGICAL ZL,EXIGEO,EXICAR
      CHARACTER*8 ZK8,LPAIN(4),LPAOUT(1),BLANC
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24,CHGEOM,CHCARA(18),LCHIN(4),LCHOUT(1)
      CHARACTER*24 LIGRMO
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80


C     -- ON VERIFIE LA PRESENCE PARFOIS NECESSAIRE DE CARA_ELEM
C        ET CHAM_MATER :
      CALL JEMARQ()
      IF (MODELE(1:1).EQ.' ') THEN
        CALL U2MESS('F','CALCULEL3_50')
      END IF

      BLANC = '        '
      CALL MEGEOM(MODELE,BLANC,EXIGEO,CHGEOM)
      CALL MECARA(CARA,EXICAR,CHCARA)

      CALL JEEXIN(MATEL//'.RERR',IRET)
      IF (IRET.GT.0) THEN
        CALL JEDETR(MATEL//'.RERR')
        CALL JEDETR(MATEL//'.RELR')
      END IF
      CALL MEMARE('G',MATEL,MODELE,MATE,CARA,'MASS_THER')

      LPAOUT(1) = 'PMATTTR'
      LCHOUT(1) = MATEL(1:8)//'.ME000'
      LPAIN(1) = 'PGEOMER'
      LCHIN(1) = CHGEOM
      LPAIN(2) = 'PMATERC'
      LCHIN(2) = MATE
      LPAIN(3) = 'PCACOQU'
      LCHIN(3) = CHCARA(7)
      LPAIN(4) = 'PTEMPSR'
      LCHIN(4) = TIME

      LIGRMO = MODELE//'.MODELE'
      ILIRES = 0
      ILIRES = ILIRES + 1
      CALL CODENT(ILIRES,'D0',LCHOUT(1) (12:14))
      CALL CALCUL('S',OPTION,LIGRMO,4,LCHIN,LPAIN,1,LCHOUT,LPAOUT,'G',
     &               'OUI')
      CALL REAJRE(MATEL,LCHOUT(1),'G')

      CALL JEDEMA()
      END
