      SUBROUTINE MECALG(OPTIOZ,RESULT,MODELE,DEPLA,THETA,
     &                  MATE,NCHAR,LCHAR,SYMECH,COMPOR,INCR,
     &                  TIME,IORD,NBPRUP,NOPRUP,CHVITE,
     &                   CHACCE,LMELAS,NOMCAS,KCALC)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 12/02/2013   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
C-----------------------------------------------------------------------

C     - FONCTION REALISEE:   CALCUL DU TAUX DE RESTITUTION D'ENERGIE

C     - ARGUMENTS   :

C IN/OUT    OPTION       --> CALC_G    (G SI CHARGES REELLES)
C                        --> CALC_G_F  (G SI CHARGES FONCTIONS)
C IN    RESULT       --> NOM UTILISATEUR DU RESULTAT ET TABLE
C IN    MODELE       --> NOM DU MODELE
C IN    DEPLA        --> CHAMP DES DEPLACEMENTS
C IN    THETA        --> CHAMP THETA (DE TYPE CHAM_NO)
C IN    MATE         --> CHAMP DU MATERIAU
C IN    NCHAR        --> NOMBRE DE CHARGES
C IN    LCHAR        --> LISTE DES CHARGES
C IN    SYMECH       --> SYMETRIE DU CHARGEMENT
C IN    TIME         --> INSTANT DE CALCUL
C IN    IORD         --> NUMERO D'ORDRE DE LA SD
C IN    LMELAS       --> TRUE SI LE TYPE DE LA SD RESULTAT EST MULT_ELAS
C IN    NOMCAS       --> NOM DU CAS DE CHARGE SI LMELAS
C IN    KCALC        --> = 'NON' : ON RECUPERE LES CHAMPS DE CONTRAINTES
C                                  ET D'ENERGIE DE LA SD RESULTAT
C                        = 'OUI' :ON RECALCULE LES CHAMPS DE CONTRAINTES
C                                  ET D'ENERGIE
C----------------------------------------------------------------------
C CORPS DU PROGRAMME

      IMPLICIT NONE

C DECLARATION PARAMETRES D'APPELS
      INCLUDE 'jeveux.h'

      CHARACTER*8 MODELE,LCHAR(*),RESULT,SYMECH
      CHARACTER*8 KCALC
      CHARACTER*16 OPTIOZ,NOPRUP(*),NOMCAS
      CHARACTER*24 DEPLA,MATE,COMPOR,THETA
      CHARACTER*24 CHVITE,CHACCE
      REAL*8 TIME
      INTEGER IORD,NCHAR,NBPRUP
      LOGICAL LMELAS, INCR


C DECLARATION VARIABLES LOCALES

      CHARACTER*2 CODRET
      CHARACTER*6 NOMPRO
      PARAMETER (NOMPRO='MECALG')

      INTEGER NBMXPA
      PARAMETER (NBMXPA = 20)

      INTEGER IBID,IRET,NRES,NUMFON,LIVI(NBMXPA)
      INTEGER NCHIN,NSIG,INO1,INO2,INGA
      REAL*8  G,LIVR(NBMXPA)
      COMPLEX*16 CBID,LIVC(NBMXPA)
      LOGICAL FONC,EPSI,LXFEM
      CHARACTER*8 RESU,LPAIN(50),LPAOUT(2),K8B, RESUCO
      CHARACTER*8  FISS
      CHARACTER*16 OPTION
      CHARACTER*19 CF1D2D,CF2D3D,CHPRES,CHROTA,CHPESA,CHVOLU,CHEPSI
      CHARACTER*19 CHVREF,CHVARC
      CHARACTER*19 BASLOC,PINTTO,CNSETO,HEAVTO,LONCHA,LNNO,LTNO,PMILTO
      CHARACTER*19 PINTER,AINTER,CFACE,LONGCO,BASECO
      CHARACTER*24 LIGRMO,CHGEOM,LCHIN(50),LCHOUT(2)
      CHARACTER*24 CHTIME,CELMOD,SIGOUT
      CHARACTER*24 PAVOLU,PA1D2D,PA2D3D,PAPRES,PEPSIN
      CHARACTER*24 CHSIG,CHEPSP,CHVARI,CHSIGI,LIVK(NBMXPA)
      PARAMETER (RESUCO = '&&MECALG')
      INTEGER      IARG
      DATA CHVARC/'&&MECALG.CH_VARC_R'/
      DATA CHVREF/'&&MECALG.CHVREF'/

      CALL JEMARQ()
      CHTIME = ' '
      OPTION = OPTIOZ

C     INITIALISATIONS
      G = 0.D0
      NSIG=0
      INGA=0
      CF1D2D = '&&MECALG.1D2D'
      CF2D3D = '&&MECALG.2D3D'
      CHEPSI = '&&MECALG.EPSI'
      CHPESA = '&&MECALG.PESA'
      CHPRES = '&&MECALG.PRES'
      CHROTA = '&&MECALG.ROTA'
      CHTIME = '&&MECALG.CH_INST_R'
      CHVOLU = '&&MECALG.VOLU'
      CHSIGI = '&&MECALG.CHSIGI'
      CELMOD = '&&MECALG.CELMOD'
      SIGOUT = '&&MECALG.SIGOUT'

      CALL GETVID('THETA','FISSURE',1,IARG,1,FISS,IBID)
      LXFEM = .FALSE.
      IF (IBID.NE.0) LXFEM = .TRUE.

C- RECUPERATION DU CHAMP GEOMETRIQUE

      CALL MEGEOM(MODELE,CHGEOM)

C- RECUPERATION DU COMPORTEMENT

      IF (INCR) THEN
        CALL GETVID(' ','RESULTAT',0,IARG,1,RESU,NRES)
        CALL RSEXCH('F',RESU,'SIEF_ELGA',IORD,CHSIG,IRET)
        CALL RSEXCH('F',RESUCO,'EPSP_ELNO',IORD,CHEPSP,IRET)
        CALL RSEXCH('F',RESUCO,'VARI_ELNO',IORD,CHVARI,IRET)
      END IF

C- RECUPERATION DE L'ETAT INITIAL
      IF (INCR) THEN
          CALL GETVID('COMP_INCR','SIGM_INIT',1,IARG,1,CHSIGI,NSIG)
C- VERIFICATION DU TYPE DE CHAMP + TRANSFO, SI NECESSAIRE, EN CHAMP ELNO
          IF (NSIG.NE.0) THEN
              CALL CHPVER ('C', CHSIGI(1:19),'ELNO','SIEF_R',INO1)
              CALL CHPVER ('C', CHSIGI(1:19),'NOEU','SIEF_R',INO2)
              CALL CHPVER ('C', CHSIGI(1:19),'ELGA','SIEF_R',INGA)
              IF ((INO1.EQ.1).AND.(INO2.EQ.1).AND.(INGA.EQ.1))THEN
                 CALL U2MESS('F','RUPTURE1_12')
              ELSE IF (INGA.EQ.0) THEN
                 LIGRMO = MODELE//'.MODELE'
                 CALL ALCHML(LIGRMO,'CALC_G','PSIGINR','V',CELMOD,
     &            IRET,' ')
                 CALL CHPCHD(CHSIGI(1:19),'ELNO',CELMOD,'NON','V',
     &           SIGOUT)
                 CALL CHPVER ('C', SIGOUT(1:19),'ELNO','SIEF_R',INO1)
              END IF
          ENDIF
      ELSE
          NSIG=0
      ENDIF

C- RECUPERATION (S'ILS EXISTENT) DES CHAMP DE TEMPERATURES (T,TREF)
      K8B = '        '
      CALL VRCINS(MODELE,MATE,K8B,TIME,CHVARC,CODRET)
      CALL VRCREF(MODELE,MATE(1:8),K8B,CHVREF)


C - TRAITEMENT DES CHARGES

      CALL GCHARG(MODELE,NCHAR,LCHAR,CHVOLU,CF1D2D,CF2D3D,CHPRES,CHEPSI,
     &            CHPESA,CHROTA,FONC,EPSI,TIME,IORD)
      IF (FONC) THEN
        PAVOLU = 'PFFVOLU'
        PA1D2D = 'PFF1D2D'
        PA2D3D = 'PFF2D3D'
        PAPRES = 'PPRESSF'
        PEPSIN = 'PEPSINF'
        IF (OPTION.EQ.'CALC_DG') THEN
          OPTION = 'CALC_DG_F'
        ELSE IF (OPTION.EQ.'CALC_G') THEN
          OPTION = 'CALC_G_F'
        ELSE IF (OPTION.EQ.'CALC_G_GLOB') THEN
          OPTION = 'CALC_G_GLOB_F'
        ELSE IF (OPTION.EQ.'CALC_DG_E') THEN
          OPTION = 'CALC_DG_E_F'
        ELSE IF (OPTION.EQ.'CALC_DGG_E') THEN
          OPTION = 'CALC_DGG_E_F'
        ELSE IF (OPTION.EQ.'CALC_DG_FORC') THEN
          OPTION = 'CALC_DG_FORC_F'
        ELSE IF (OPTION.EQ.'CALC_DGG_FORC') THEN
          OPTION = 'CALC_DGG_FORC_F'
        END IF
      ELSE
        PAVOLU = 'PFRVOLU'
        PA1D2D = 'PFR1D2D'
        PA2D3D = 'PFR2D3D'
        PAPRES = 'PPRESSR'
        PEPSIN = 'PEPSINR'
      END IF

      IF  (LXFEM) THEN
C       RECUPERATION DES DONNEES XFEM (TOPOSE)
        PINTTO = MODELE//'.TOPOSE.PIN'
        CNSETO = MODELE//'.TOPOSE.CNS'
        HEAVTO = MODELE//'.TOPOSE.HEA'
        LONCHA = MODELE//'.TOPOSE.LON'
        PMILTO = MODELE//'.TOPOSE.PMI'
        LNNO   = FISS//'.LNNO'
        LTNO   = FISS//'.LTNO'
        BASLOC = FISS//'.BASLOC'

C       RECUPERATION DES DONNEES XFEM (TOPOFAC)
        PINTER = MODELE//'.TOPOFAC.OE'
        AINTER = MODELE//'.TOPOFAC.AI'
        CFACE  = MODELE//'.TOPOFAC.CF'
        LONGCO = MODELE//'.TOPOFAC.LO'
        BASECO = MODELE//'.TOPOFAC.BA'

      ENDIF

      LPAOUT(1) = 'PGTHETA'
      LCHOUT(1) = '&&'//NOMPRO//'.CH_G'
      LPAIN(1) = 'PGEOMER'
      LCHIN(1) = CHGEOM
      LPAIN(2) = 'PDEPLAR'
      LCHIN(2) = DEPLA
      LPAIN(3) = 'PTHETAR'
      LCHIN(3) = THETA
      LPAIN(4) = 'PMATERC'
      LCHIN(4) = MATE
      LPAIN(5) = 'PVARCPR'
      LCHIN(5) = CHVARC
      LPAIN(6) = 'PVARCRR'
      LCHIN(6) = CHVREF
      LPAIN(7) = PAVOLU(1:8)
      LCHIN(7) = CHVOLU
      LPAIN(8) = PA1D2D(1:8)
      LCHIN(8) = CF1D2D
      LPAIN(9) = PA2D3D(1:8)
      LCHIN(9) = CF2D3D
      LPAIN(10) = PAPRES(1:8)
      LCHIN(10) = CHPRES
      LPAIN(11) = 'PPESANR'
      LCHIN(11) = CHPESA
      LPAIN(12) = 'PROTATR'
      LCHIN(12) = CHROTA
      LPAIN(13) = PEPSIN(1:8)
      LCHIN(13) = CHEPSI
      LPAIN(14) = 'PCOMPOR'
      LCHIN(14) = COMPOR

      LIGRMO = MODELE//'.MODELE'
      NCHIN = 14

      IF (LXFEM) THEN
        LPAIN(15) = 'PCNSETO'
        LCHIN(15) = CNSETO
        LPAIN(16) = 'PHEAVTO'
        LCHIN(16) = HEAVTO
        LPAIN(17) = 'PLONCHA'
        LCHIN(17) = LONCHA
        LPAIN(18) = 'PLSN'
        LCHIN(18) = LNNO
        LPAIN(19) = 'PLST'
        LCHIN(19) = LTNO
        LPAIN(20) = 'PBASLOR'
        LCHIN(20) = BASLOC
        LPAIN(21) = 'PPINTTO'
        LCHIN(21) = PINTTO
        LPAIN(22) = 'PPMILTO'
        LCHIN(22) = PMILTO
        LPAIN(23) = 'PPINTER'
        LCHIN(23) = PINTER
        LPAIN(24) = 'PAINTER'
        LCHIN(24) = AINTER
        LPAIN(25) = 'PCFACE'
        LCHIN(25) = CFACE
        LPAIN(26) = 'PLONGCO'
        LCHIN(26) = LONGCO
        LPAIN(27) = 'PBASECO'
        LCHIN(27) = BASECO

        NCHIN = 27

      END IF

      IF ((OPTION.EQ.'CALC_G_F') .OR. (OPTION.EQ.'CALC_DG_F') .OR.
     &(OPTION.EQ.'CALC_DG_E_F').OR.(OPTION.EQ.'CALC_G_GLOB_F').OR.
     &(OPTION.EQ.'CALC_DGG_E_F').OR.(OPTION.EQ.'CALC_DGG_FORC_F').OR.
     &(OPTION.EQ.'CALC_DG_FORC_F')) THEN
        CALL MECACT('V',CHTIME,'MODELE',LIGRMO,'INST_R  ',1,'INST   ',
     &              IBID,TIME,CBID,K8B)
        LPAIN(NCHIN+1) = 'PTEMPSR'
        LCHIN(NCHIN+1) = CHTIME
        NCHIN = NCHIN + 1
      END IF

      IF (INCR) THEN
        LPAIN(NCHIN+1) = 'PCONTRR'
        LCHIN(NCHIN+1) = CHSIG
        LPAIN(NCHIN+2) = 'PDEFOPL'
        LCHIN(NCHIN+2) = CHEPSP
        LPAIN(NCHIN+3) = 'PVARIPR'
        LCHIN(NCHIN+3) = CHVARI
        NCHIN = NCHIN + 3

C       CHAMP DE CONTRAINTE INITIALE
        IF (NSIG.NE.0) THEN
          LPAIN(NCHIN+1) = 'PSIGINR'
          IF (INGA.EQ.0) THEN
             LCHIN(NCHIN+1)=SIGOUT
          ELSE
             LCHIN(NCHIN+1) = CHSIGI
          END IF
          NCHIN = NCHIN + 1
        END IF
      END IF

      IF (CHVITE.NE.' ') THEN
        LPAIN(NCHIN+1) = 'PVITESS'
        LCHIN(NCHIN+1) =  CHVITE
        LPAIN(NCHIN+2) = 'PACCELE'
        LCHIN(NCHIN+2) =  CHACCE
        NCHIN = NCHIN + 2
      END IF

      IF(KCALC.EQ.'NON')THEN
          CALL GETVID(' ','RESULTAT',0,IARG,1,RESU,IRET)
          CALL RSEXCH(' ',RESU,'SIEF_ELGA',IORD,CHSIG,IRET)
          LPAIN(NCHIN+1) = 'PCONTGR'
          LCHIN(NCHIN+1) =  CHSIG
          NCHIN = NCHIN + 1
      ENDIF


C-  SOMMATION DES G ELEMENTAIRES
      CALL CALCUL('S',OPTION,LIGRMO,NCHIN,LCHIN,LPAIN,1,LCHOUT,LPAOUT,
     &            'V','OUI')

      CALL MESOMM(LCHOUT(1),1,IBID,G,CBID,0,IBID)
      IF (SYMECH.NE.'NON') THEN
        G = 2.D0*G
      END IF

C- IMPRESSION DE G ET ECRITURE DANS LA TABLE RESULT
C
      CALL GETVIS('THETA','NUME_FOND',1,IARG,1,NUMFON,IBID)

      IF (LXFEM)   THEN
        CALL TBAJVI(RESULT,NBPRUP,'NUME_FOND',NUMFON,LIVI)
      ENDIF

      IF (LMELAS) THEN
        CALL TBAJVI(RESULT,NBPRUP,'NUME_CAS',IORD,LIVI)
        CALL TBAJVK(RESULT,NBPRUP,'NOM_CAS',NOMCAS,LIVK)
      ELSE
        CALL TBAJVI(RESULT,NBPRUP,'NUME_ORDRE',IORD,LIVI)
        CALL TBAJVR(RESULT,NBPRUP,'INST',TIME,LIVR)
      ENDIF

      CALL TBAJVR(RESULT,NBPRUP,'G',G,LIVR)
      CALL TBAJLI(RESULT,NBPRUP,NOPRUP,LIVI,LIVR,LIVC,LIVK,0)
C
      CALL DETRSD('CHAMP_GD',CF1D2D)
      CALL DETRSD('CHAMP_GD',CF2D3D)
      CALL DETRSD('CHAMP_GD',CHEPSI)
      CALL DETRSD('CHAMP_GD',CHPESA)
      CALL DETRSD('CHAMP_GD',CHPRES)
      CALL DETRSD('CHAMP_GD',CHROTA)
      CALL DETRSD('CHAMP_GD',CHTIME)
      CALL DETRSD('CHAMP_GD',CHVOLU)

      CALL JEDEMA()
      END
