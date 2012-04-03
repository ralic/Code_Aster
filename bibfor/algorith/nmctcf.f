      SUBROUTINE NMCTCF(NOMA  ,MODELE,SDIMPR,SDERRO,DEFICO,
     &                  RESOCO,VALINC,MMCVFR)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 02/04/2012   AUTEUR ABBAS M.ABBAS 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT     NONE
      CHARACTER*8  NOMA
      CHARACTER*24 MODELE
      CHARACTER*24 DEFICO,RESOCO
      CHARACTER*24 SDIMPR,SDERRO
      CHARACTER*19 VALINC(*)
      LOGICAL      MMCVFR
C
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (ALGO - BOUCLE CONTACT)
C
C SEUIL DE FROTTEMENT
C
C ----------------------------------------------------------------------
C
C
C IN  NOMA   : NOM DU MAILLAGE
C IN  MODELE : NOM DU MODELE
C IN  SDIMPR : SD AFFICHAGE
C IN  SDERRO : GESTION DES ERREURS
C IN  DEFICO : SD POUR LA DEFINITION DE CONTACT
C IN  RESOCO : SD POUR LA RESOLUTION DE CONTACT
C IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
C OUT MMCVCA : INDICATEUR DE CONVERGENCE POUR BOUCLE DU
C              FROTTEMENT
C               .TRUE. SI LA BOUCLE A CONVERGE
C
C -------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------------
C
      INTEGER ZI
      COMMON /IVARJE/ ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
C ---------------- FIN DECLARATIONS NORMALISEES JEVEUX -----------------
C
      INTEGER      IFM,NIV
      LOGICAL      CFDISL,LTFCM,LCTCC,LXFCM
      LOGICAL      LSANS,LMANU,LAUTO,LERROF
      INTEGER      CFDISI,NBREAF,MAXFRO
      REAL*8       CFDISR,EPSFRO
      INTEGER      MMITFR
      CHARACTER*19 DEPPLU,DEPLAM,DEPMOI
      CHARACTER*8  NOMO
      CHARACTER*16 CVGNOE
      REAL*8       CVGVAL
      REAL*8       R8BID
      INTEGER      IBID
      CHARACTER*16 K16BLA
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFDBG('MECANONLINE',IFM,NIV)
C
C --- AFFICHAGE
C
      IF (NIV.GE.2) THEN
        WRITE (IFM,*) '<MECANONLINE> MISE A JOUR DU SEUIL DE TRESCA'
      ENDIF
C
C --- INITIALISATIONS
C
      NOMO   = MODELE(1:8)
      K16BLA = ' '
      MMCVFR = .FALSE.
      DEPLAM = RESOCO(1:14)//'.DEPF'
      LERROF = .FALSE.
C
C --- DECOMPACTION DES VARIABLES CHAPEAUX
C
      CALL NMCHEX(VALINC,'VALINC','DEPMOI',DEPMOI)
      CALL NMCHEX(VALINC,'VALINC','DEPPLU',DEPPLU)
C
C --- INFOS BOUCLE FROTTEMENT
C
      CALL MMBOUC(RESOCO,'FROT','READ',MMITFR)
      MAXFRO  = CFDISI(DEFICO,'ITER_FROT_MAXI')
      NBREAF  = CFDISI(DEFICO,'NB_ITER_FROT'  )
      EPSFRO  = CFDISR(DEFICO,'RESI_FROT'     )
C
C --- TYPE DE CONTACT
C
      LCTCC  = CFDISL(DEFICO,'FORMUL_CONTINUE')
      LXFCM  = CFDISL(DEFICO,'FORMUL_XFEM')
      LTFCM  = CFDISL(DEFICO,'CONT_XFEM_GG')
C
      LMANU  = CFDISL(DEFICO,'REAC_FROT_MANU')
      LSANS  = CFDISL(DEFICO,'REAC_FROT_SANS')
      LAUTO  = CFDISL(DEFICO,'REAC_FROT_AUTO')
C
C --- MISE A JOUR DES SEUILS
C
      IF (LXFCM) THEN
        IF (.NOT.LTFCM) THEN
          CALL XREACL(NOMA  ,NOMO ,VALINC,RESOCO)
        ENDIF
      ELSEIF (LCTCC) THEN
        CALL MMREAS(NOMA  ,DEFICO,RESOCO,VALINC)
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF
C
C --- CONVERGENCE SEUIL FROTTEMENT
C
      CALL MMMCRI('FROT',NOMA  ,DEPMOI,DEPLAM,DEPPLU,
     &            RESOCO,EPSFRO,CVGNOE,CVGVAL,MMCVFR)
C
C --- CAS MANUEL
C
      IF (LMANU) THEN
        IF (MMITFR.EQ.NBREAF) THEN
          IF (.NOT.MMCVFR) THEN
            CALL U2MESS('A','CONTACT3_97')
          ENDIF
          MMCVFR = .TRUE.
        ELSE
          MMCVFR = .FALSE.
        ENDIF
      ENDIF
C
C --- CAS SANS
C
      IF (LSANS) THEN
        MMCVFR = .TRUE.
      ENDIF
C
C --- CAS AUTO
C
      IF (LAUTO) THEN
        IF ((.NOT.MMCVFR).AND.(MMITFR.EQ.MAXFRO)) THEN
          LERROF = .TRUE.
        ENDIF
      ENDIF
C
C --- CONVERGENCE ET ERREUR
C
      CALL NMCREL(SDERRO,'ERRE_CTCF',LERROF)
      CALL NMCREL(SDERRO,'DIVE_FIXF',.NOT.MMCVFR)
C
C --- VALEUR ET ENDROIT OU SE REALISE L'EVALUATION DE LA BOUCLE
C
      CALL IMPSDR(SDIMPR,'BOUC_NOEU',CVGNOE,R8BID ,IBID)
      CALL IMPSDR(SDIMPR,'BOUC_VALE',K16BLA,CVGVAL,IBID)
C
C --- NUMERO ITERATION DE FROTTEMENT
C
      CALL IMPSDR(SDIMPR,'BOUC_FROT',' ',0.D0,MMITFR)
C
C --- MISE A JOUR DU SEUIL DE REFERENCE
C
      IF (.NOT.MMCVFR) THEN
        CALL COPISD('CHAMP_GD','V',DEPPLU,DEPLAM)
      ENDIF
C
      CALL JEDEMA()
      END
