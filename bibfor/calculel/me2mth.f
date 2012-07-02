      SUBROUTINE ME2MTH(MODELZ,NCHAR,LCHAR,MATEZ,CARAZ,TIMEZ,CHTNZ,
     &                  VECELZ)
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
      CHARACTER*(*) MODELZ,CHTNZ,CARAZ,MATEZ,VECELZ,TIMEZ
      CHARACTER*(*) LCHAR(*)
      CHARACTER*8 MODELE,CARA
      CHARACTER*8 LCHARZ
      CHARACTER*19 VECEL
      CHARACTER*24 MATE,TIME
      INTEGER NCHAR
C ......................................................................
C     BUT:
C         CALCUL DE TOUS LES SECONDS MEMBRES ELEMENTAIRES PROVENANT
C         DES CHARGES_THERMIQUES

C     ENTREES:

C     LES NOMS QUI SUIVENT SONT LES PREFIXES UTILISATEUR K8:
C        MODELZ : NOM DU MODELE
C        NCHAR  : NOMBRE DE CHARGES
C        LCHAR  : LISTE DES CHARGES
C        MATEZ  : CHAM_MATER
C        CARAZ  : CARAC_ELEM
C        TIMEZ  : CHAMPS DE TEMPSR
C        CHTNZ  : CHAM_NO DE TEMPERATURE A L'INSTANT TN
C        VECELZ : NOM DU VEC_ELE (N RESUELEM) PRODUIT
C                 SI VECEL EXISTE DEJA, ON LE DETRUIT.

C     SORTIES:
C     SONT TRAITES ACTUELLEMENT LES CHAMPS:
C        LCHAR(ICHA)//'.CHTH.CIMPO     ' : TEMPERATURE IMPOSEE
C        LCHAR(ICHA)//'.CHTH.SOURE     ' : SOURCE REPARTIE
C        LCHAR(ICHA)//'.CHTH.GRAIN     ' : GRADIENT INITIAL
C        LCHAR(ICHA)//'.CHTH.FLURE     ' : FLUX NORMAL REPARTI
C        LCHAR(ICHA)//'.CHTH.FLUR2     ' : FLUX NORMAL REPARTI (VECTEUR)
C        LCHAR(ICHA)//'.CHTH.T_EXT     ' : TEMPERATURE EXTERIEURE

C ......................................................................




      LOGICAL EXIGEO,LFONC
      CHARACTER*8 LPAIN(5),LPAOUT(1),K8BID
      CHARACTER*16 OPTION
      CHARACTER*24 LCHIN(5),LCHOUT(1),LIGRMO,LIGRCH
      CHARACTER*24 CHGEOM
      INTEGER ICHA,IRET,ILIRES,IBID,IERD,JNOMO


C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      CALL JEMARQ()
      MODELE = MODELZ
      MATE = MATEZ
      CARA = CARAZ
      VECEL = VECELZ
      TIME = TIMEZ
      LCHARZ = LCHAR(1)
      CALL MEGEOM(MODELE,LCHARZ,EXIGEO,CHGEOM)

      CALL JEEXIN(VECEL//'.RERR',IRET)
      IF (IRET.GT.0) THEN
        CALL JEDETR(VECEL//'.RERR')
        CALL JEDETR(VECEL//'.RELR')
      END IF
      CALL MEMARE('G',VECEL,MODELE,' ',CARA,'CHAR_THER')

      LPAOUT(1) = 'PVECTTR'
      LCHOUT(1) = VECEL(1:8)//'.VE000'
      ILIRES = 0

C     BOUCLE SUR LES CHARGES POUR CALCULER :
C         ( CHAR_THER_TEXT_F , ISO_FACE ) SUR LE MODELE
C         ( CHAR_THER_FLUN_F , ISO_FACE ) SUR LE MODELE
C         ( CHAR_THER_SOUR_F , ISO      ) SUR LE MODELE
C         ( CHAR_THER_SOUR_F , SOURCE_NO) SUR LE LIGREL(CHARGE)
C         ( THER_DDLI_F      , CAL_TI   ) SUR LE LIGREL(CHARGE)


      IF (NCHAR.NE.0) THEN
        LPAIN(1) = 'PGEOMER'
        LCHIN(1) = CHGEOM
        LPAIN(2) = 'PTEMPSR'
        LCHIN(2) = TIME
        IF (MODELE.NE.'        ') THEN
          LIGRMO = MODELE//'.MODELE'
        ELSE
          LCHARZ = LCHAR(1)
          CALL JEVEUO(LCHARZ//'.CHTH      .NOMO','L',JNOMO)
          LIGRMO = ZK8(JNOMO)//'.MODELE'
        END IF
        DO 10 ICHA = 1,NCHAR
          LCHARZ = LCHAR(ICHA)
          CALL DISMOI('F','TYPE_CHARGE',LCHARZ,'CHARGE',IBID,K8BID,IERD)
          IF (K8BID(5:7).EQ.'_FO') THEN
            LFONC = .TRUE.
          ELSE
            LFONC = .FALSE.
          END IF

          LIGRCH = LCHARZ//'.CHTH.LIGRE      '
C  =====================================================================
C           --  ( CHAR_THER_TEXT_F , ISO_FACE ) SUR LE MODELE
          CALL EXISD('CHAMP_GD',LIGRCH(1:13)//'.COEFH',IRET)
          IF (IRET.NE.0) THEN
            IF (LFONC) THEN
              OPTION = 'CHAR_THER_TEXT_F'
              LPAIN(3) = 'PT_EXTF'
              LPAIN(4) = 'PCOEFHF'
            ELSE
              OPTION = 'CHAR_THER_TEXT_R'
              LPAIN(3) = 'PT_EXTR'
              LPAIN(4) = 'PCOEFHR'
            END IF
            LCHIN(3) = LIGRCH(1:13)//'.T_EXT     '
            LCHIN(4) = LIGRCH(1:13)//'.COEFH     '
            LPAIN(5) = 'PTEMPER'
            LCHIN(5) = CHTNZ
            ILIRES = ILIRES + 1
            CALL CODENT(ILIRES,'D0',LCHOUT(1) (12:14))
            CALL CALCUL('S',OPTION,LIGRMO,5,LCHIN,LPAIN,1,LCHOUT,LPAOUT,
     &                  'G','OUI')
            CALL REAJRE(VECEL,LCHOUT(1),'G')
          END IF
C  =====================================================================
C           --  ( CHAR_THER_FLUN_F , ISO_FACE ) SUR LE MODELE
          CALL EXISD('CHAMP_GD',LIGRCH(1:13)//'.FLURE',IRET)
          IF (IRET.NE.0) THEN
            IF (LFONC) THEN
              OPTION = 'CHAR_THER_FLUN_F'
              LPAIN(3) = 'PFLUXNF'
            ELSE
              OPTION = 'CHAR_THER_FLUN_R'
              LPAIN(3) = 'PFLUXNR'
            END IF
            LCHIN(3) = LIGRCH(1:13)//'.FLURE     '
            ILIRES = ILIRES + 1
            CALL CODENT(ILIRES,'D0',LCHOUT(1) (12:14))
            CALL CALCUL('S',OPTION,LIGRMO,3,LCHIN,LPAIN,1,LCHOUT,LPAOUT,
     &                  'G','OUI')
            CALL REAJRE(VECEL,LCHOUT(1),'G')
          END IF
C  =====================================================================
C           --  ( CHAR_THER_FLUX_  , ISO_FACE ) SUR LE MODELE
          CALL EXISD('CHAMP_GD',LIGRCH(1:13)//'.FLUR2',IRET)
          IF (IRET.NE.0) THEN
            IF (LFONC) THEN
              OPTION = 'CHAR_THER_FLUX_F'
              LPAIN(3) = 'PFLUXVF'
            ELSE
              OPTION = 'CHAR_THER_FLUX_R'
              LPAIN(3) = 'PFLUXVR'
            END IF
            LCHIN(3) = LIGRCH(1:13)//'.FLUR2     '
            ILIRES = ILIRES + 1
            CALL CODENT(ILIRES,'D0',LCHOUT(1) (12:14))
            CALL CALCUL('S',OPTION,LIGRMO,3,LCHIN,LPAIN,1,LCHOUT,LPAOUT,
     &                  'G','OUI')
            CALL REAJRE(VECEL,LCHOUT(1),'G')
          END IF
C  =====================================================================
C           --   ( CHAR_THER_SOUR_F , ISO    )  SUR LE MODELE
          CALL EXISD('CHAMP_GD',LIGRCH(1:13)//'.SOURE',IRET)
          IF (IRET.NE.0) THEN
            IF (LFONC) THEN
              OPTION = 'CHAR_THER_SOUR_F'
              LPAIN(3) = 'PSOURCF'
            ELSE
              OPTION = 'CHAR_THER_SOUR_R'
              LPAIN(3) = 'PSOURCR'
            END IF
            LCHIN(3) = LIGRCH(1:13)//'.SOURE     '
            ILIRES = ILIRES + 1
            CALL CODENT(ILIRES,'D0',LCHOUT(1) (12:14))
            CALL CALCUL('S',OPTION,LIGRMO,3,LCHIN,LPAIN,1,LCHOUT,LPAOUT,
     &                  'G','OUI')
            CALL REAJRE(VECEL,LCHOUT(1),'G')
          END IF
C  =====================================================================
C           --   ( CHAR_THER_GRAI_  ,  ISO_VOLU  ) SUR LE   MODELE
          CALL EXISD('CHAMP_GD',LIGRCH(1:13)//'.GRAIN',IRET)
          IF (IRET.NE.0) THEN
            IF (LFONC) THEN
              OPTION = 'CHAR_THER_GRAI_F'
              LPAIN(3) = 'PGRAINF'
            ELSE
              OPTION = 'CHAR_THER_GRAI_R'
              LPAIN(3) = 'PGRAINR'
            END IF
            LCHIN(3) = LIGRCH(1:13)//'.GRAIN     '
            LPAIN(4) = 'PMATERC'
            LCHIN(4) = MATE
            ILIRES = ILIRES + 1
            CALL CODENT(ILIRES,'D0',LCHOUT(1) (12:14))
            CALL CALCUL('S',OPTION,LIGRMO,4,LCHIN,LPAIN,1,LCHOUT,LPAOUT,
     &                  'G','OUI')
            CALL REAJRE(VECEL,LCHOUT(1),'G')
          END IF
C  =====================================================================
C           --   ( THER_DDLI_F    , CAL_TI   )  SUR LE LIGREL(CHARGE)
          CALL EXISD('CHAMP_GD',LIGRCH(1:13)//'.CIMPO',IRET)
          IF (IRET.NE.0) THEN
            IF (LFONC) THEN
              OPTION = 'THER_DDLI_F'
              LPAIN(3) = 'PDDLIMF'
            ELSE
              OPTION = 'THER_DDLI_R'
              LPAIN(3) = 'PDDLIMR'
            END IF
            LCHIN(3) = LIGRCH(1:13)//'.CIMPO     '
            ILIRES = ILIRES + 1
            CALL CODENT(ILIRES,'D0',LCHOUT(1) (12:14))
            CALL CALCUL('S',OPTION,LIGRCH,3,LCHIN,LPAIN,1,LCHOUT,LPAOUT,
     &                  'G','OUI')
            CALL REAJRE(VECEL,LCHOUT(1),'G')
          END IF
   10   CONTINUE
      END IF

      CALL JEDEMA()
      END
