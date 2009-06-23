      SUBROUTINE ME2MME(MODELZ,NCHAR,LCHAR,MATE,CARAZ,EXITIM,TIME,
     &                  MATELZ,NH,BASEZ)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 22/06/2009   AUTEUR FLEJOU J-L.FLEJOU 
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
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
C TOLE  CRP_20
      IMPLICIT REAL*8 (A-H,O-Z)

C     ARGUMENTS:
C     ----------
      CHARACTER*8 MODELE,LCHARZ,CARA,KBID,LCMP(5)
      CHARACTER*(*) MODELZ,CARAZ,MATELZ,LCHAR(*),MATE,BASEZ
      CHARACTER*19 MATEL
      REAL*8 TIME
      LOGICAL EXITIM,LFONC
      INTEGER NCHAR
C ----------------------------------------------------------------------

C     CALCUL DES SECONDS MEMBRES ELEMENTAIRES

C     ENTREES:

C     LES NOMS QUI SUIVENT SONT LES PREFIXES UTILISATEUR K8:
C        MODELZ : NOM DU MODELE
C        NCHAR  : NOMBRE DE CHARGES
C        LCHAR  : LISTE DES CHARGES
C        MATE   : CHAMP DE MATERIAUX
C        CARAZ  : CHAMP DE CARAC_ELEM
C        MATELZ : NOM DU MATEL (N RESUELEM) PRODUIT
C        NH     : NUMERO DE L'HARMONIQUE DE FOURIER
C        BASEZ  : NOM DE LA BASE

C        EXITIM : VRAI SI L'INSTANT EST DONNE
C        TIME   : INSTANT DE CALCUL

C     SORTIES:
C     SONT TRAITES LES CHARGEMENTS :
C        LCHAR(ICHA)//'.CHME.CIMPO'
C        LCHAR(ICHA)//'.CHME.FORNO'
C        LCHAR(ICHA)//'.CHME.F3D3D'
C        LCHAR(ICHA)//'.CHME.FCO2D'
C        LCHAR(ICHA)//'.CHME.FCO3D'
C        LCHAR(ICHA)//'.CHME.F2D3D'
C        LCHAR(ICHA)//'.CHME.F1D3D'
C        LCHAR(ICHA)//'.CHME.F2D2D'
C        LCHAR(ICHA)//'.CHME.F1D2D'
C        LCHAR(ICHA)//'.CHME.F1D1D'
C        LCHAR(ICHA)//'.CHME.PESAN'
C        LCHAR(ICHA)//'.CHME.ROTAT'
C        LCHAR(ICHA)//'.CHME.FELEC'
C        LCHAR(ICHA)//'.CHME.FL1??'
C        LCHAR(ICHA)//'.CHME.PRESS'
C        LCHAR(ICHA)//'.CHME.EPSIN'
C        LCHAR(ICHA)//'.CHME.TEMPE'
C        LCHAR(ICHA)//'.CHME.VNOR'
C        LCHAR(ICHA)//'.CHME.ONDE'
C        LCHAR(ICHA)//'.CHME.EVOL.CHAR'

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
      REAL*8 ZR
      COMPLEX*16 ZC,CBID
      LOGICAL ZL,EXIGEO,EXICAR,EXITHE,EXITRF
      CHARACTER*1 BASE
      CHARACTER*2 CODRET
      INTEGER NBIN
      PARAMETER (NBIN=19)
      CHARACTER*8 ZK8,LPAIN(NBIN),LPAOUT(1),TEMPE,NOMA,EXIELE,REPK
      CHARACTER*16 ZK16,OPTION
      CHARACTER*24 CHGEOM,LCHIN(NBIN),LCHOUT(1),KCMP(5)
      CHARACTER*24 LIGRMO,LIGRCH,CHTIME,CHLAPL,CHCARA(18),ZK24
      CHARACTER*24 CHHARM,CH24
      CHARACTER*19 CHVARC,CHVREF
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80

      CHARACTER*19  RESUFV(3)
      CHARACTER*24 CHARGE
      INTEGER JAD,I
      LOGICAL LTEMP,LTREF


      CALL JEMARQ()
      MODELE = MODELZ
      CARA = CARAZ
      MATEL = MATELZ
      BASE = BASEZ

      DO 10,I = 1,NBIN
        LCHIN(I) = ' '
        LPAIN(I) = ' '
   10 CONTINUE

      CHVARC='&&ME2MME.VARC'
      CHVREF='&&ME2MME.VARC.REF'


C     -- CALCUL DE .RERR:
      CALL MEMARE(BASE,MATEL,MODELE,MATE,CARA,'CHAR_MECA')

C     -- S'IL N' Y A PAS D'ELEMENTS CLASSIQUES, ON RESSORT:
      CALL DISMOI('F','EXI_ELEM',MODELE,'MODELE',IBID,EXIELE,IERD)


C     -- ON VERIFIE LA PRESENCE PARFOIS NECESSAIRE DE CARA_ELEM
C        ET CHAM_MATER :

      CALL MEGEOM(MODELE,LCHAR(1),EXIGEO,CHGEOM)
      CALL MECARA(CARA,EXICAR,CHCARA)
      NOMA = CHGEOM(1:8)
      CALL VRCINS(MODELE,MATE,CARA,TIME,CHVARC,CODRET)
      CALL VRCREF(MODELE,MATE(1:8),CARA,CHVREF(1:19))

      IF (NCHAR.EQ.0) GO TO 60
      CALL JEEXIN(MATEL//'.RELR',IRET)
      IF (IRET.GT.0) CALL JEDETR(MATEL//'.RELR')

      LPAOUT(1) = 'PVECTUR'
      LCHOUT(1) = MATEL(1:8)//'.VEXXX'
      ILIRES = 0
        LPAIN(1) = 'PGEOMER'
        LCHIN(1) = CHGEOM
        LPAIN(2) = 'PMATERC'
        LCHIN(2) = MATE
        LPAIN(3) = 'PVARCPR'
        LCHIN(3) = CHVARC

        IFLA = 0

        LIGRMO = MODELE//'.MODELE'

C        -- EN PRINCIPE, EXITIM EST TOUJOURS .TRUE.
        CHTIME = '&&ME2MME.CH_INST_R'
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

        LPAIN(17) = 'PNBSP_I'
        LCHIN(17) = CHCARA(16)


        DO 50 ICHA = 1,NCHAR
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
            CALL CALCUL('S',OPTION,LIGRCH,NBIN,LCHIN,LPAIN,1,LCHOUT,
     &                  LPAOUT,BASE)
            CALL REAJRE(MATEL,LCHOUT(1),BASE)
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
            CALL CALCUL('S',OPTION,LIGRCH,NBIN,LCHIN,LPAIN,1,LCHOUT,
     &                  LPAOUT,BASE)
            CALL REAJRE(MATEL,LCHOUT(1),BASE)
          END IF

C      -- SI LE MODELE NE CONTIENT PAS D'ELEMENTS CLASSIQUES, ON SAUTE:
C      ----------------------------------------------------------------
          IF (EXIELE(1:3).EQ.'NON') GO TO 50
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
            CALL CALCUL('S',OPTION,LIGRMO,NBIN,LCHIN,LPAIN,1,LCHOUT,
     &                  LPAOUT,BASE)
            CALL REAJRE(MATEL,LCHOUT(1),BASE)
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
            CALL CALCUL('S',OPTION,LIGRMO,NBIN,LCHIN,LPAIN,1,LCHOUT,
     &                  LPAOUT,BASE)
            CALL REAJRE(MATEL,LCHOUT(1),BASE)
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
            CALL CALCUL('S',OPTION,LIGRMO,NBIN,LCHIN,LPAIN,1,LCHOUT,
     &                  LPAOUT,BASE)
            CALL REAJRE(MATEL,LCHOUT(1),BASE)
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
            CALL CALCUL('S',OPTION,LIGRMO,NBIN,LCHIN,LPAIN,1,LCHOUT,
     &                  LPAOUT,BASE)
            CALL REAJRE(MATEL,LCHOUT(1),BASE)
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
            CALL CALCUL('S',OPTION,LIGRMO,NBIN,LCHIN,LPAIN,1,LCHOUT,
     &                  LPAOUT,BASE)
            CALL REAJRE(MATEL,LCHOUT(1),BASE)
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
            CALL CALCUL('S',OPTION,LIGRMO,NBIN,LCHIN,LPAIN,1,LCHOUT,
     &                  LPAOUT,BASE)
            CALL REAJRE(MATEL,LCHOUT(1),BASE)
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
            CALL CALCUL('S',OPTION,LIGRMO,NBIN,LCHIN,LPAIN,1,LCHOUT,
     &                  LPAOUT,BASE)
            CALL REAJRE(MATEL,LCHOUT(1),BASE)
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
            LPAIN(12) = 'PCAGEPO'
            LCHIN(12) = CHCARA(5)
            LPAIN(13) = 'PCAGNBA'
            LCHIN(13) = CHCARA(11)
            CALL CALCUL('S',OPTION,LIGRMO,NBIN,LCHIN,LPAIN,1,LCHOUT,
     &                  LPAOUT,BASE)
            CALL REAJRE(MATEL,LCHOUT(1),BASE)
          END IF
C ====================================================================
          CALL EXISD('CHAMP_GD',LIGRCH(1:13)//'.PESAN',IRET)
          IF (IRET.NE.0) THEN
            OPTION = 'CHAR_MECA_PESA_R'
            LPAIN(2) = 'PMATERC'
            LCHIN(2) = MATE
            LPAIN(4) = 'PPESANR'
            LCHIN(4) = LIGRCH(1:13)//'.PESAN.DESC'
            ILIRES = ILIRES + 1
            CALL CODENT(ILIRES,'D0',LCHOUT(1) (12:14))
            LPAIN(12) = 'PCAGEPO'
            LCHIN(12) = CHCARA(5)
            LPAIN(13) = 'PCAGNBA'
            LCHIN(13) = CHCARA(11)
            LPAIN(19) = 'PCINFDI'
            LCHIN(19) = CHCARA(15)
            CALL CALCUL('S',OPTION,LIGRMO,NBIN,LCHIN,LPAIN,1,LCHOUT,
     &                  LPAOUT,BASE)
            CALL REAJRE(MATEL,LCHOUT(1),BASE)
          END IF
C ====================================================================
          CALL EXISD('CHAMP_GD',LIGRCH(1:13)//'.ROTAT',IRET)
          IF (IRET.NE.0) THEN
            OPTION = 'CHAR_MECA_ROTA_R'
            LPAIN(2) = 'PMATERC'
            LCHIN(2) = MATE
            LPAIN(4) = 'PROTATR'
            LCHIN(4) = LIGRCH(1:13)//'.ROTAT.DESC'
            ILIRES = ILIRES + 1
            CALL CODENT(ILIRES,'D0',LCHOUT(1) (12:14))
            CALL CALCUL('S',OPTION,LIGRMO,NBIN,LCHIN,LPAIN,1,LCHOUT,
     &                  LPAOUT,BASE)
            CALL REAJRE(MATEL,LCHOUT(1),BASE)
          END IF
C ====================================================================
          CALL EXISD('CHAMP_GD',LIGRCH(1:13)//'.EPSIN',IRET)
          IF (IRET.NE.0) THEN
            LPAIN(2) = 'PMATERC'
            LCHIN(2) = MATE
            IF (LFONC) THEN
              OPTION = 'CHAR_MECA_EPSI_F'
              LPAIN(4) = 'PEPSINF'
            ELSE
              OPTION = 'CHAR_MECA_EPSI_R'
              LPAIN(4) = 'PEPSINR'
            END IF
            LCHIN(4) = LIGRCH(1:13)//'.EPSIN.DESC'
            LPAIN(12) = 'PCAMASS'
            LCHIN(12) = CHCARA(12)
            CALL MEHARM(MODELE,NH,CHHARM)
            LPAIN(13) = 'PHARMON'
            LCHIN(13) = CHHARM
            LPAIN(14) = 'PFIBRES'
            LCHIN(14) = CHCARA(17)
            LPAIN(15) = 'PCOMPOR'
            LCHIN(15) =  MATE(1:8)//'.COMPOR'
            ILIRES = ILIRES + 1
            CALL CODENT(ILIRES,'D0',LCHOUT(1) (12:14))
            CALL CALCUL('S',OPTION,LIGRMO,NBIN,LCHIN,LPAIN,1,LCHOUT,
     &                  LPAOUT,BASE)
            CALL REAJRE(MATEL,LCHOUT(1),BASE)
          END IF
C ====================================================================
          CALL EXISD('CHAMP_GD',LIGRCH(1:13)//'.FELEC',IRET)
          IF (IRET.NE.0) THEN
            OPTION = 'CHAR_MECA_FRELEC'
            LPAIN(4) = 'PFRELEC'
            LCHIN(4) = LIGRCH(1:13)//'.FELEC.DESC'
            ILIRES = ILIRES + 1
            CALL CODENT(ILIRES,'D0',LCHOUT(1) (12:14))
            CALL CALCUL('S',OPTION,LIGRMO,NBIN,LCHIN,LPAIN,1,LCHOUT,
     &                  LPAOUT,BASE)
            CALL REAJRE(MATEL,LCHOUT(1),BASE)
          END IF
C ====================================================================
C         -- LA BOUCLE 30 SERT A TRAITER LES FORCES ELECTRIQUES LAPLACE

          DO 30 J = 1,99
            LCHIN(13) (1:17) = LIGRCH(1:13)//'.FL1'
            CALL CODENT(J,'D0',LCHIN(13) (18:19))
            LCHIN(13) = LCHIN(13) (1:19)//'.DESC'
            CALL JEEXIN(LCHIN(13),IRET)
            IF (IRET.EQ.0) GO TO 40
            LPAIN(12) = 'PHARMON'
            LCHIN(12) = ' '
            LPAIN(13) = 'PLISTMA'
            IF (IFLA.EQ.0) THEN
              CHLAPL = '&&ME2MME.CH_FLAPLA'
              LCMP(1) = 'NOMAIL'
              LCMP(2) = 'NOGEOM'
              KCMP(1) = NOMA
              KCMP(2) = CHGEOM(1:19)
              CALL MECACT('V',CHLAPL,'MAILLA',NOMA,'FLAPLA  ',2,LCMP(1),
     &                    IBID,TIME,CBID,KCMP(1))
              IFLA = 1
            END IF
            OPTION = 'CHAR_MECA_FRLAPL'
            LPAIN(4) = 'PFLAPLA'
            LCHIN(4) = CHLAPL
            ILIRES = ILIRES + 1
            CALL CODENT(ILIRES,'D0',LCHOUT(1) (12:14))
            CALL CALCUL('S',OPTION,LIGRMO,NBIN,LCHIN,LPAIN,1,LCHOUT,
     &                  LPAOUT,BASE)
            CALL REAJRE(MATEL,LCHOUT(1),BASE)
   30     CONTINUE
   40     CONTINUE
C ====================================================================
          CALL JEEXIN(LIGRCH(1:13)//'.TEMPE.TEMP',IRET)
          CALL NMVCD2('TEMP',MATE,LTEMP,LTREF)
          LTEMP=LTEMP.OR.(IRET.GT.0)
          IF (LTEMP) THEN
            CALL VRCINS(MODELE,MATE,CARA,TIME,CHVARC,CODRET)
            OPTION = 'CHAR_MECA_TEMP_R'
            LPAIN(2) = 'PMATERC'
            LCHIN(2) = MATE
            LPAIN(4) = 'PVARCRR'
            LCHIN(4) = CHVREF
            LPAIN(12) = 'PCAMASS'
            LCHIN(12) = CHCARA(12)
            CALL MEHARM(MODELE,NH,CHHARM)
            LPAIN(13) = 'PHARMON'
            LCHIN(13) = CHHARM
            LPAIN(14) = 'PCAGEPO'
            LCHIN(14) = CHCARA(5)
            LPAIN(15) = 'PCAGNBA'
            LCHIN(15) = CHCARA(11)
            LPAIN(16) = ' '
            LCHIN(16) = ' '
            ILIRES = ILIRES + 1
            CALL CODENT(ILIRES,'D0',LCHOUT(1) (12:14))
            CALL CALCUL('S',OPTION,LIGRMO,NBIN,LCHIN,LPAIN,1,LCHOUT,
     &                  LPAOUT,BASE)
            CALL REAJRE(MATEL,LCHOUT(1),BASE)
            CALL DETRSD('CHAMP_GD',CHVARC)
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
            LPAIN(6) = 'PCAGEPO'
            LCHIN(6) = CHCARA(5)
            CALL CALCUL('S',OPTION,LIGRMO,NBIN,LCHIN,LPAIN,1,LCHOUT,
     &                  LPAOUT,BASE)
            CALL REAJRE(MATEL,LCHOUT(1),BASE)
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
            CALL CALCUL('S',OPTION,LIGRMO,NBIN,LCHIN,LPAIN,1,LCHOUT,
     &                  LPAOUT,BASE)
            CALL REAJRE(MATEL,LCHOUT(1),BASE)
          END IF
C ====================================================================
          CALL JEEXIN(LIGRCH(1:13)//'.VEASS',IRET)
          IF (IRET.GT.0) THEN
            ILIRES = ILIRES + 1
            CALL CODENT(ILIRES,'D0',LCHOUT(1) (12:14))
            CALL JEVEUO(LIGRCH(1:13)//'.VEASS','L',JVEASS)
            CALL COPISD('CHAMP_GD',BASE,ZK8(JVEASS),LCHOUT(1))
            CALL REAJRE(MATEL,LCHOUT(1),BASE)
          END IF
C ====================================================================
          CALL EXISD('CHAMP_GD',LIGRCH(1:13)//'.ONDE ',IRET)
          IF (IRET.NE.0) THEN
            IF (LFONC) THEN
              OPTION = 'CHAR_MECA_ONDE_F'
              LPAIN(4) = 'PONDECF'
            ELSE
              OPTION = 'CHAR_MECA_ONDE'
              LPAIN(4) = 'PONDECR'
            END IF

            LCHIN(4) = LIGRCH(1:13)//'.ONDE .DESC'
            ILIRES = ILIRES + 1
            CALL CODENT(ILIRES,'D0',LCHOUT(1) (12:14))
            CALL CALCUL('S',OPTION,LIGRMO,NBIN,LCHIN,LPAIN,1,LCHOUT,
     &                  LPAOUT,BASE)
            CALL REAJRE(MATEL,LCHOUT(1),BASE)
          END IF
C ====================================================================

C CHARGE DE TYPE EVOL_CHAR

        CHARGE = '&&ME2MME.INTERF.NMDEPR'
        CALL WKVECT(CHARGE,'V V K24',1,JAD)
        ZK24(JAD) = LCHAR(ICHA)

        RESUFV(1) = '&&ME2MME.VE001'
        RESUFV(2) = '&&ME2MME.VE002'
        RESUFV(3) = '&&ME2MME.VE003'

        CALL NMDEPR(MODELE,LIGRMO,CARA,CHARGE,1,TIME,RESUFV)

        DO 200 I = 1,3
          CALL EXISD('CHAMP_GD',RESUFV(I),IRET)
          IF (IRET.NE.0) THEN
            ILIRES = ILIRES + 1
            CALL CODENT(ILIRES,'D0',LCHOUT(1) (12:14))
            CALL REAJRE(MATEL,LCHOUT(1),BASE)
            CALL COPISD('CHAMP_GD',BASE,RESUFV(I),LCHOUT(1))
            CALL DETRSD('CHAMP_GD',RESUFV(I))
          END IF
 200    CONTINUE

        CALL JEDETR(CHARGE)

C ====================================================================
   50   CONTINUE

   60 CONTINUE
      CALL DETRSD('CHAMP_GD',CHVARC)
      CALL DETRSD('CHAMP_GD',CHVREF)

      CALL JEDEMA()
      END
