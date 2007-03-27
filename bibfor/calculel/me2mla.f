      SUBROUTINE ME2MLA(MODELE,NCHAR,LCHAR,MATE,CARA,EXITIM,TIME,MATEL,
     &                  THETA,ALPHA)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 28/03/2007   AUTEUR PELLET J.PELLET 
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
C TOLE CRP_20
      IMPLICIT REAL*8 (A-H,O-Z)

C     ARGUMENTS:
C     ----------
      CHARACTER*8 MODELE,LCHAR(*),CARA,MATEL,KBID,LCMP(5)
      CHARACTER*24 MATE
      REAL*8 TIME
      LOGICAL EXITIM,LFONC,LBID
      INTEGER NCHAR,ICHA
C ----------------------------------------------------------------------

C     CALCUL DES SECONDS MEMBRES ELEMENTAIRES AVEC OPTION LAGRANGIENNE

C     ENTREES:

C     LES NOMS QUI SUIVENT SONT LES PREFIXES UTILISATEUR K8:
C        MODELE : NOM DU MODELE
C        NCHAR  : NOMBRE DE CHARGES
C        LCHAR  : LISTE DES CHARGES
C        MATE   : CHAMP DE MATERIAUX
C        CARA   : CHAMP DE CARAC_ELEM
C        MATEL  : NOM DU MATEL (N RESUELEM) PRODUIT
C        THETA  : CHAMP THETA
C        ALPHA  : VALEUR DE LA PROPAGATION

C        EXITIM : VRAI SI L'INSTANT EST DONNE
C        TIME   : INSTANT DE CALCUL

C     SORTIES:
C     SONT TRAITES LES CHAMPS:
C        LCHAR(ICHA)//'.CHME.CIMPO.DESC'
C        LCHAR(ICHA)//'.CHME.FORNO.DESC'
C        LCHAR(ICHA)//'.CHME.F3D3D.DESC'
C        LCHAR(ICHA)//'.CHME.FCO2D.DESC'
C        LCHAR(ICHA)//'.CHME.FCO3D.DESC'
C        LCHAR(ICHA)//'.CHME.F2D3D.DESC'
C        LCHAR(ICHA)//'.CHME.F1D3D.DESC'
C        LCHAR(ICHA)//'.CHME.F2D2D.DESC'
C        LCHAR(ICHA)//'.CHME.F1D2D.DESC'
C        LCHAR(ICHA)//'.CHME.F1D1D.DESC'
C        LCHAR(ICHA)//'.CHME.PESAN.DESC'
C        LCHAR(ICHA)//'.CHME.ROTAT.DESC'
C        LCHAR(ICHA)//'.CHME.FELEC.DESC'
C        LCHAR(ICHA)//'.CHME.FL1??.DESC'
C        LCHAR(ICHA)//'.CHME.PRESS.DESC'
C        LCHAR(ICHA)//'.CHME.EPSIN.DESC'
C        LCHAR(ICHA)//'.CHME.TEMPE.TEMP'
C        LCHAR(ICHA)//'.CHME.VNOR .DESC'

C ----------------------------------------------------------------------

C     FONCTIONS EXTERNES:
C     -------------------
      CHARACTER*32 JEXNUM,JEXNOM,JEXATR

C     VARIABLES LOCALES:
C     ------------------
C --------------- COMMUNS NORMALISES  JEVEUX  --------------------------
      COMMON /IVARJE/ZI(1)
      COMMON /RVARJE/ZR(1)
      COMMON /CVARJE/ZC(1)
      COMMON /LVARJE/ZL(1)
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      INTEGER ZI
      REAL*8 ZR,ALPHA
      COMPLEX*16 ZC,CBID
      LOGICAL ZL,EXIGEO,EXICAR,EXITHE,EXITRF
      CHARACTER*8 ZK8,LPAIN(15),LPAOUT(1),TEMPE,NOMA,EXIELE
      CHARACTER*8 REPK
      CHARACTER*16 ZK16,OPTION
      CHARACTER*19 THETA,CHVARC
      CHARACTER*24 CHGEOM,LCHIN(15),LCHOUT(1),KCMP(5)
      CHARACTER*24 LIGRMO,LIGRCH,CHTREF,CHTIME,CHLAPL,CHCARA(16),ZK24
      CHARACTER*24 CHHARM,CHALPH
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80


C     -- CALCUL DE .REFE_RESU:
      CALL JEMARQ()
      CALL MEMARE('G',MATEL,MODELE,MATE,CARA,'CHAR_MECA_LAGR')
      CHVARC='&&ME2MLA.VARC'

C     -- S'IL N' Y A PAS D'ELEMENTS CLASSIQUES, ON RESSORT:
      CALL DISMOI('F','EXI_ELEM',MODELE,'MODELE',IBID,EXIELE,IERD)


C     -- ON VERIFIE LA PRESENCE PARFOIS NECESSAIRE DE CARA_ELEM
C        ET CHAM_MATER :

      CALL MEGEOM(MODELE,LCHAR(1),EXIGEO,CHGEOM)
      CALL MECARA(CARA,EXICAR,CHCARA)

      NOMA = CHGEOM(1:8)
      CALL VRCINS(MODELE,MATE(1:8),CARA(1:8),NCHAR,LCHAR,TIME,CHVARC)
      CALL DISMOI('F','ELAS_F_TEMP',MATE,'CHAM_MATER',IBID,REPK,IERD)
      IF (REPK.EQ.'OUI') THEN
        CALL NMVCD2('TEMP',MATE,EXITHE,LBID)
        IF (.NOT.EXITHE) THEN
          CALL U2MESS('F','ALGORITH_57')
        END IF
      END IF

      IF (NCHAR.EQ.0) GO TO 30
      LONLIS = 10*NCHAR
      CALL JEEXIN(MATEL//'.LISTE_RESU',IRET)
      IF (IRET.GT.0) CALL JEDETR(MATEL//'.LISTE_RESU')
      CALL WKVECT(MATEL//'.LISTE_RESU','G V K24',LONLIS,JLIRES)

      LPAOUT(1) = 'PVECTUR'
      LCHOUT(1) = MATEL//'.VEXXX'
      ILIRES = 0
      IF (NCHAR.NE.0) THEN
        LPAIN(1) = 'PGEOMER'
        LCHIN(1) = CHGEOM
        LPAIN(2) = 'PMATERC'
        LCHIN(2) = MATE
        LPAIN(3) = 'PVARCPR'
        LCHIN(3) = CHVARC


        LIGRMO = MODELE//'.MODELE'

C        -- EN PRINCIPE, EXITIM EST TOUJOURS .TRUE.
        CHTIME = '&&ME2MLA.CH_INST_R'
        CALL MECACT('V',CHTIME,'MODELE',LIGRMO,'INST_R  ',1,'INST   ',
     &              IBID,TIME,CBID,KBID)
        LPAIN(5) = 'PTEMPSR'
        LCHIN(5) = CHTIME
        LPAIN(6) = 'PCACOQU'
        LCHIN(6) = CHCARA(7)
        LPAIN(7) = 'PCAGNPO'
        LCHIN(7) = CHCARA(6)
        LPAIN(8) = 'PCADISM'
        LCHIN(8) = CHCARA(3)
        LPAIN(9) = 'PCAORIE'
        LCHIN(9) = CHCARA(1)
        LPAIN(10) = 'PCAARPO'
        LCHIN(10) = CHCARA(9)
        LPAIN(11) = 'PCASECT'
        LCHIN(11) = CHCARA(8)
        CALL MEALPH(NOMA,ALPHA,CHALPH)
        LPAIN(12) = 'PTHETAR'
        LCHIN(12) = THETA
        LPAIN(13) = 'PALPHAR'
        LCHIN(13) = CHALPH


        DO 20 ICHA = 1,NCHAR
          CALL DISMOI('F','TYPE_CHARGE',LCHAR(ICHA),'CHARGE',IBID,KBID,
     &                IERD)
          IF (KBID(5:7).EQ.'_FO') THEN
            LFONC = .TRUE.
          ELSE
            LFONC = .FALSE.
          END IF
          LIGRCH = LCHAR(ICHA)//'.CHME.LIGRE'
C ====================================================================
          CALL EXISD('CHAMP_GD',LIGRCH(1:13)//'.CIMPO',IRET)
          IF (IRET.NE.0) THEN

            IF (LFONC) THEN
              OPTION = 'MECA_DDLI_F'
              LPAIN(4) = 'PDDLIMF'
            ELSE
              OPTION = 'MECA_DDLI_R'
              LPAIN(4) = 'PDDLIMR'
            END IF
            LCHIN(4) = LIGRCH(1:13)//'.CIMPO.DESC'
            ILIRES = ILIRES + 1
            CALL CODENT(ILIRES,'D0',LCHOUT(1) (12:14))
            CALL CALCUL('S',OPTION,LIGRCH,5,LCHIN,LPAIN,1,LCHOUT,LPAOUT,
     &                  'G')
            CALL EXISD('CHAMP_GD',LCHOUT(1) (1:19),IRET)
            IF (IRET.NE.0) THEN
              ZK24(JLIRES-1+ILIRES) = LCHOUT(1)
              CALL JEECRA(MATEL//'.LISTE_RESU','LONUTI',ILIRES,' ')
            ELSE
              ILIRES = ILIRES - 1
            END IF
          END IF
C ====================================================================
          CALL EXISD('CHAMP_GD',LIGRCH(1:13)//'.FORNO',IRET)
          IF (IRET.NE.0) THEN
            IF (LFONC) THEN
              OPTION = 'CHAR_MECA_FORC_F'
              LPAIN(4) = 'PFORNOF'
            ELSE
              OPTION = 'CHAR_MECA_FORC_R'
              LPAIN(4) = 'PFORNOR'
            END IF
            LCHIN(4) = LIGRCH(1:13)//'.FORNO.DESC'
            ILIRES = ILIRES + 1
            CALL CODENT(ILIRES,'D0',LCHOUT(1) (12:14))
            CALL CALCUL('S',OPTION,LIGRCH,5,LCHIN,LPAIN,1,LCHOUT,LPAOUT,
     &                  'G')
            CALL EXISD('CHAMP_GD',LCHOUT(1) (1:19),IRET)
            IF (IRET.NE.0) THEN
              ZK24(JLIRES-1+ILIRES) = LCHOUT(1)
              CALL JEECRA(MATEL//'.LISTE_RESU','LONUTI',ILIRES,' ')
            ELSE
              ILIRES = ILIRES - 1
            END IF
          END IF

C      -- SI LE MODELE NE CONTIENT PAS D'ELEMENTS CLASSIQUES, ON SAUTE:
C      ----------------------------------------------------------------
          IF (EXIELE(1:3).EQ.'NON') GO TO 20
C ====================================================================
          CALL EXISD('CHAMP_GD',LIGRCH(1:13)//'.F3D3D',IRET)
          IF (IRET.NE.0) THEN
            IF (LFONC) THEN
              OPTION = 'CHAR_MECA_FF3D3D'
              LPAIN(4) = 'PFF3D3D'
            ELSE
              OPTION = 'CHAR_MECA_FR3D3D'
              LPAIN(4) = 'PFR3D3D'
            END IF
            LCHIN(4) = LIGRCH(1:13)//'.F3D3D.DESC'
            ILIRES = ILIRES + 1
            CALL CODENT(ILIRES,'D0',LCHOUT(1) (12:14))
            CALL CALCUL('S',OPTION,LIGRMO,5,LCHIN,LPAIN,1,LCHOUT,LPAOUT,
     &                  'G')
            CALL EXISD('CHAMP_GD',LCHOUT(1) (1:19),IRET)
            IF (IRET.NE.0) THEN
              ZK24(JLIRES-1+ILIRES) = LCHOUT(1)
              CALL JEECRA(MATEL//'.LISTE_RESU','LONUTI',ILIRES,' ')
            ELSE
              ILIRES = ILIRES - 1
            END IF
          END IF
C ====================================================================
          CALL EXISD('CHAMP_GD',LIGRCH(1:13)//'.FCO2D',IRET)
          IF (IRET.NE.0) THEN

            IF (LFONC) THEN
              OPTION = 'CHAR_MECA_FFCO2D'
              LPAIN(4) = 'PFFCO2D'
            ELSE
              OPTION = 'CHAR_MECA_FRCO2D'
              LPAIN(4) = 'PFRCO2D'
            END IF
            LCHIN(4) = LIGRCH(1:13)//'.FCO2D.DESC'
            ILIRES = ILIRES + 1
            CALL CODENT(ILIRES,'D0',LCHOUT(1) (12:14))
            CALL CALCUL('S',OPTION,LIGRMO,5,LCHIN,LPAIN,1,LCHOUT,LPAOUT,
     &                  'G')
            CALL EXISD('CHAMP_GD',LCHOUT(1) (1:19),IRET)
            IF (IRET.NE.0) THEN
              ZK24(JLIRES-1+ILIRES) = LCHOUT(1)
              CALL JEECRA(MATEL//'.LISTE_RESU','LONUTI',ILIRES,' ')
            ELSE
              ILIRES = ILIRES - 1
            END IF
          END IF
C ====================================================================
          CALL EXISD('CHAMP_GD',LIGRCH(1:13)//'.FCO3D',IRET)
          IF (IRET.NE.0) THEN

            IF (LFONC) THEN
              OPTION = 'CHAR_MECA_FFCO3D'
              LPAIN(4) = 'PFFCO3D'
            ELSE
              OPTION = 'CHAR_MECA_FRCO3D'
              LPAIN(4) = 'PFRCO3D'
            END IF
            LCHIN(4) = LIGRCH(1:13)//'.FCO3D.DESC'
            ILIRES = ILIRES + 1
            CALL CODENT(ILIRES,'D0',LCHOUT(1) (12:14))
            CALL CALCUL('S',OPTION,LIGRMO,6,LCHIN,LPAIN,1,LCHOUT,LPAOUT,
     &                  'G')
            CALL EXISD('CHAMP_GD',LCHOUT(1) (1:19),IRET)
            IF (IRET.NE.0) THEN
              ZK24(JLIRES-1+ILIRES) = LCHOUT(1)
              CALL JEECRA(MATEL//'.LISTE_RESU','LONUTI',ILIRES,' ')
            ELSE
              ILIRES = ILIRES - 1
            END IF
          END IF
C ====================================================================
          CALL EXISD('CHAMP_GD',LIGRCH(1:13)//'.F2D3D',IRET)
          IF (IRET.NE.0) THEN

            IF (LFONC) THEN
              OPTION = 'CHAR_MECA_FF2D3D'
              LPAIN(4) = 'PFF2D3D'
            ELSE
              OPTION = 'CHAR_MECA_FR2D3D'
              LPAIN(4) = 'PFR2D3D'
            END IF
            LCHIN(4) = LIGRCH(1:13)//'.F2D3D.DESC'
            ILIRES = ILIRES + 1
            CALL CODENT(ILIRES,'D0',LCHOUT(1) (12:14))
            CALL CALCUL('S',OPTION,LIGRMO,5,LCHIN,LPAIN,1,LCHOUT,LPAOUT,
     &                  'G')
            CALL EXISD('CHAMP_GD',LCHOUT(1) (1:19),IRET)
            IF (IRET.NE.0) THEN
              ZK24(JLIRES-1+ILIRES) = LCHOUT(1)
              CALL JEECRA(MATEL//'.LISTE_RESU','LONUTI',ILIRES,' ')
            ELSE
              ILIRES = ILIRES - 1
            END IF
          END IF
C ====================================================================
          CALL EXISD('CHAMP_GD',LIGRCH(1:13)//'.F1D3D',IRET)
          IF (IRET.NE.0) THEN

            IF (LFONC) THEN
              OPTION = 'CHAR_MECA_FF1D3D'
              LPAIN(4) = 'PFF1D3D'
            ELSE
              OPTION = 'CHAR_MECA_FR1D3D'
              LPAIN(4) = 'PFR1D3D'
            END IF
            LCHIN(4) = LIGRCH(1:13)//'.F1D3D.DESC'
            ILIRES = ILIRES + 1
            CALL CODENT(ILIRES,'D0',LCHOUT(1) (12:14))
            CALL CALCUL('S',OPTION,LIGRMO,5,LCHIN,LPAIN,1,LCHOUT,LPAOUT,
     &                  'G')
            CALL EXISD('CHAMP_GD',LCHOUT(1) (1:19),IRET)
            IF (IRET.NE.0) THEN
              ZK24(JLIRES-1+ILIRES) = LCHOUT(1)
              CALL JEECRA(MATEL//'.LISTE_RESU','LONUTI',ILIRES,' ')
            ELSE
              ILIRES = ILIRES - 1
            END IF
          END IF
C ====================================================================
          CALL EXISD('CHAMP_GD',LIGRCH(1:13)//'.F2D2D',IRET)
          IF (IRET.NE.0) THEN

            IF (LFONC) THEN
              OPTION = 'CHAR_MECA_FF2D2D'
              LPAIN(4) = 'PFF2D2D'
            ELSE
              OPTION = 'CHAR_MECA_FR2D2D'
              LPAIN(4) = 'PFR2D2D'
            END IF
            LCHIN(4) = LIGRCH(1:13)//'.F2D2D.DESC'
            ILIRES = ILIRES + 1
            CALL CODENT(ILIRES,'D0',LCHOUT(1) (12:14))
            CALL CALCUL('S',OPTION,LIGRMO,5,LCHIN,LPAIN,1,LCHOUT,LPAOUT,
     &                  'G')
            CALL EXISD('CHAMP_GD',LCHOUT(1) (1:19),IRET)
            IF (IRET.NE.0) THEN
              ZK24(JLIRES-1+ILIRES) = LCHOUT(1)
              CALL JEECRA(MATEL//'.LISTE_RESU','LONUTI',ILIRES,' ')
            ELSE
              ILIRES = ILIRES - 1
            END IF
          END IF
C ====================================================================
          CALL EXISD('CHAMP_GD',LIGRCH(1:13)//'.F1D2D',IRET)
          IF (IRET.NE.0) THEN
            IF (LFONC) THEN
              OPTION = 'CHAR_MECA_FF1D2D'
              LPAIN(4) = 'PFF1D2D'
            ELSE
              OPTION = 'CHAR_MECA_FR1D2D'
              LPAIN(4) = 'PFR1D2D'
            END IF
            LCHIN(4) = LIGRCH(1:13)//'.F1D2D.DESC'
            ILIRES = ILIRES + 1
            CALL CODENT(ILIRES,'D0',LCHOUT(1) (12:14))
            CALL CALCUL('S',OPTION,LIGRMO,5,LCHIN,LPAIN,1,LCHOUT,LPAOUT,
     &                  'G')
            CALL EXISD('CHAMP_GD',LCHOUT(1) (1:19),IRET)
            IF (IRET.NE.0) THEN
              ZK24(JLIRES-1+ILIRES) = LCHOUT(1)
              CALL JEECRA(MATEL//'.LISTE_RESU','LONUTI',ILIRES,' ')
            ELSE
              ILIRES = ILIRES - 1
            END IF
          END IF
C ====================================================================
          CALL EXISD('CHAMP_GD',LIGRCH(1:13)//'.F1D1D',IRET)
          IF (IRET.NE.0) THEN
            IF (LFONC) THEN
              OPTION = 'CHAR_MECA_FF1D1D'
              LPAIN(4) = 'PFF1D1D'
            ELSE
              OPTION = 'CHAR_MECA_FR1D1D'
              LPAIN(4) = 'PFR1D1D'
            END IF
            LCHIN(4) = LIGRCH(1:13)//'.F1D1D.DESC'
            ILIRES = ILIRES + 1
            CALL CODENT(ILIRES,'D0',LCHOUT(1) (12:14))
            CALL CALCUL('S',OPTION,LIGRMO,11,LCHIN,LPAIN,1,LCHOUT,
     &                  LPAOUT,'G')
            CALL EXISD('CHAMP_GD',LCHOUT(1) (1:19),IRET)
            IF (IRET.NE.0) THEN
              ZK24(JLIRES-1+ILIRES) = LCHOUT(1)
              CALL JEECRA(MATEL//'.LISTE_RESU','LONUTI',ILIRES,' ')
            ELSE
              ILIRES = ILIRES - 1
            END IF
          END IF
C ====================================================================
          CALL EXISD('CHAMP_GD',LIGRCH(1:13)//'.PESAN',IRET)
          IF (IRET.NE.0) THEN
            CALL U2MESS('F','CALCULEL2_79')
          END IF
C ====================================================================
          CALL EXISD('CHAMP_GD',LIGRCH(1:13)//'.ROTAT',IRET)
          IF (IRET.NE.0) THEN
            OPTION = 'CHAR_MECA_ROTA_R'
            LPAIN(4) = 'PROTATR'
            LCHIN(4) = LIGRCH(1:13)//'.ROTAT.DESC'
            ILIRES = ILIRES + 1
            CALL CODENT(ILIRES,'D0',LCHOUT(1) (12:14))
            CALL CALCUL('S',OPTION,LIGRMO,6,LCHIN,LPAIN,1,LCHOUT,LPAOUT,
     &                  'G')
            CALL EXISD('CHAMP_GD',LCHOUT(1) (1:19),IRET)
            IF (IRET.NE.0) THEN
              ZK24(JLIRES-1+ILIRES) = LCHOUT(1)
              CALL JEECRA(MATEL//'.LISTE_RESU','LONUTI',ILIRES,' ')
            ELSE
              ILIRES = ILIRES - 1
            END IF
          END IF
C ====================================================================
          CALL EXISD('CHAMP_GD',LIGRCH(1:13)//'.EPSIN',IRET)
          IF (IRET.NE.0) THEN
            IF (LFONC) THEN
              OPTION = 'CHAR_MECA_EPLG_F'
              LPAIN(4) = 'PEPSINF'
            ELSE
              OPTION = 'CHAR_MECA_EPLG_R'
              LPAIN(4) = 'PEPSINR'
            END IF
            LCHIN(4) = LIGRCH(1:13)//'.EPSIN.DESC'
            ILIRES = ILIRES + 1
            CALL CODENT(ILIRES,'D0',LCHOUT(1) (12:14))
            CALL CALCUL('S',OPTION,LIGRMO,13,LCHIN,LPAIN,1,LCHOUT,
     &                  LPAOUT,'G')
            CALL EXISD('CHAMP_GD',LCHOUT(1) (1:19),IRET)
            IF (IRET.NE.0) THEN
              ZK24(JLIRES-1+ILIRES) = LCHOUT(1)
              CALL JEECRA(MATEL//'.LISTE_RESU','LONUTI',ILIRES,' ')
            ELSE
              ILIRES = ILIRES - 1
            END IF
          END IF
C ====================================================================
          CALL EXISD('CHAMP_GD',LIGRCH(1:13)//'.FELEC',IRET)
          IF (IRET.NE.0) THEN
            OPTION = 'CHAR_MECA_FRELEC'
            LPAIN(4) = 'PFRELEC'
            LCHIN(4) = LIGRCH(1:13)//'.FELEC.DESC'
            ILIRES = ILIRES + 1
            CALL CODENT(ILIRES,'D0',LCHOUT(1) (12:14))
            CALL CALCUL('S',OPTION,LIGRMO,5,LCHIN,LPAIN,1,LCHOUT,LPAOUT,
     &                  'G')
            CALL EXISD('CHAMP_GD',LCHOUT(1) (1:19),IRET)
            IF (IRET.NE.0) THEN
              ZK24(JLIRES-1+ILIRES) = LCHOUT(1)
              CALL JEECRA(MATEL//'.LISTE_RESU','LONUTI',ILIRES,' ')
            ELSE
              ILIRES = ILIRES - 1
            END IF
          END IF
C ====================================================================
          CALL JEEXIN(LIGRCH(1:13)//'.TEMPE.TEMP',IRET)
          IF (IRET.NE.0) THEN
            CALL U2MESS('F','CALCULEL2_80')
          END IF
C ====================================================================
          CALL EXISD('CHAMP_GD',LIGRCH(1:13)//'.PRESS',IRET)
          IF (IRET.NE.0) THEN
            IF (LFONC) THEN
              OPTION = 'CHAR_MECA_PRES_F'
              LPAIN(4) = 'PPRESSF'
            ELSE
              OPTION = 'CHAR_MECA_PRES_R'
              LPAIN(4) = 'PPRESSR'
            END IF
            LCHIN(4) = LIGRCH(1:13)//'.PRESS.DESC'
            ILIRES = ILIRES + 1
            CALL CODENT(ILIRES,'D0',LCHOUT(1) (12:14))
            CALL CALCUL('S',OPTION,LIGRMO,5,LCHIN,LPAIN,1,LCHOUT,LPAOUT,
     &                  'G')
            CALL EXISD('CHAMP_GD',LCHOUT(1) (1:19),IRET)
            IF (IRET.NE.0) THEN
              ZK24(JLIRES-1+ILIRES) = LCHOUT(1)
              CALL JEECRA(MATEL//'.LISTE_RESU','LONUTI',ILIRES,' ')
            ELSE
              ILIRES = ILIRES - 1
            END IF
          END IF
C ====================================================================
          CALL EXISD('CHAMP_GD',LIGRCH(1:13)//'.VNOR ',IRET)
          IF (IRET.NE.0) THEN
            IF (LFONC) THEN
              OPTION = 'CHAR_MECA_VNOR_F'
              LPAIN(4) = 'PSOURCF'
            ELSE
              OPTION = 'CHAR_MECA_VNOR'
              LPAIN(4) = 'PSOURCR'
            END IF
            LCHIN(4) = LIGRCH(1:13)//'.VNOR .DESC'
            ILIRES = ILIRES + 1
            CALL CODENT(ILIRES,'D0',LCHOUT(1) (12:14))
            CALL CALCUL('S',OPTION,LIGRMO,4,LCHIN,LPAIN,1,LCHOUT,LPAOUT,
     &                  'G')
            CALL EXISD('CHAMP_GD',LCHOUT(1) (1:19),IRET)
            IF (IRET.NE.0) THEN
              ZK24(JLIRES-1+ILIRES) = LCHOUT(1)
              CALL JEECRA(MATEL//'.LISTE_RESU','LONUTI',ILIRES,' ')
            ELSE
              ILIRES = ILIRES - 1
            END IF
          END IF
   20   CONTINUE
C ====================================================================
      END IF

   30 CONTINUE
      CALL DETRSD('CHAMP_GD',CHVARC)
      CALL JEDETC('V','&&ME2MLA.CH_INST_R',1)
      CALL JEDEMA()
      END
