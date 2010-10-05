      SUBROUTINE MERIRO(MODELE,CARA,NCHAR,LCHAR,MATE,EXITIM,TIME,MATEL)
      IMPLICIT REAL*8 (A-H,O-Z)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 04/10/2010   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
      CHARACTER*8 MODELE,CARA,LCHAR(*)
      CHARACTER*19 MATEL
      CHARACTER*24 MATE
      REAL*8 TIME
      LOGICAL EXITIM
      INTEGER NCHAR
C ----------------------------------------------------------------------

C     CALCUL DES MATRICES ELEMENTAIRES DE RAIDEUR CENTRIFUGE

C     ENTREES:

C     LES NOMS QUI SUIVENT SONT LES PREFIXES UTILISATEUR K8:
C        MODELE : NOM DU MODELE
C        NCHAR  : NOMBRE DE CHARGES
C        LCHAR  : LISTE DES CHARGES
C        MATE   : CHAMP DE MATERIAUX
C        EXITIM : VRAI SI L'INSTANT EST DONNE
C        TIME   : INSTANT DE CALCUL

C     SORTIES:

C        MATEL  : NOM DU MATEL (N RESUELEM) PRODUIT

C ----------------------------------------------------------------------

C     FONCTIONS EXTERNES:
C     -------------------
      CHARACTER*32 JEXNUM,JEXNOM,JEXATR

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
      CHARACTER*2 CODRET
      CHARACTER*8 ZK8,LPAIN(10),LPAOUT(1),TEMPE,REPK
      CHARACTER*16 ZK16,OPTION
      CHARACTER*19 CHVARC
      CHARACTER*24 ZK24
      CHARACTER*24 CHGEOM,CHROTA,LCHIN(10),LCHOUT(10)
      CHARACTER*24 LIGRMO
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      DATA CHVARC /'&&MERIRO.CHVARC'/

C     -- ON VERIFIE LA PRESENCE PARFOIS NECESSAIRE DE CHAM_MATER

      CALL JEMARQ()
      IF (MODELE(1:1).EQ.' ') THEN
        CALL U2MESS('F','CALCULEL3_50')
      END IF

      CALL MEGEOM(MODELE,LCHAR(1),EXIGEO,CHGEOM)

      NBRO = 0
      DO 10 ICHA = 1,NCHAR
        CALL EXISD('CHAMP_GD',LCHAR(ICHA)//'.CHME.ROTAT',IRET)
        IF (IRET.NE.0) THEN
          CHROTA = LCHAR(ICHA)//'.CHME.ROTAT.DESC'
          NBRO = NBRO + 1
        END IF
   10 CONTINUE

      IF (NBRO.NE.1) THEN
        CALL U2MESS('F','CALCULEL3_71')
      END IF

      CALL VRCINS(MODELE,MATE,CARA,TIME,CHVARC,CODRET)

      CALL JEEXIN(MATEL//'.RERR',IRET)
      IF (IRET.GT.0) THEN
        CALL JEDETR(MATEL//'.RERR')
        CALL JEDETR(MATEL//'.RELR')
      END IF
      CALL MEMARE('G',MATEL,MODELE,MATE,' ','RIGI_ROTA')
      CALL REAJRE(MATEL,' ','G')

      LPAOUT(1) = 'PMATUUR'
      LCHOUT(1) = MATEL(1:8)//'.ME001'

      IF (MODELE.NE.'       ') THEN
        LPAIN(1) = 'PGEOMER'
        LCHIN(1) = CHGEOM
        LPAIN(2) = 'PMATERC'
        LCHIN(2) = MATE
        LPAIN(3) = 'PROTATR'
        LCHIN(3) = CHROTA
        LPAIN(4) = 'PVARCPR'
        LCHIN(4) = CHVARC
        LIGRMO = MODELE//'.MODELE'
        OPTION = 'RIGI_MECA_RO'

        CALL CALCUL('S',OPTION,LIGRMO,4,LCHIN,LPAIN,1,LCHOUT,LPAOUT,'G')

        CALL REAJRE(MATEL,LCHOUT(1),'G')
      END IF

   30 CONTINUE
      CALL DETRSD('CHAMP_GD',CHVARC)
      CALL JEDEMA()
      END
