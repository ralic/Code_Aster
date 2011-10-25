      SUBROUTINE NMDORC(MODELZ,COMPOZ,CARCRI)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.
C
C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE <IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.
C
C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C RESPONSABLE PROIX J-M.PROIX
      IMPLICIT NONE
      CHARACTER*(*) MODELZ,COMPOZ
      CHARACTER*24  CARCRI
C ----------------------------------------------------------------------
C MODIF ALGORITH  DATE 24/10/2011   AUTEUR DELMAS J.DELMAS 
C
C     SAISIE ET VERIFICATION DES MOTS CLES COMP_INCR / COMP_ELAS
C
C IN  MODELZ  : NOM DU MODELE
C OUT COMPOZ  : CARTE DECRIVANT LE TYPE DE COMPORTEMENT
C ----------------------------------------------------------------------
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------

      INTEGER        ZI
      COMMON /IVARJE/ZI(1)
      REAL*8         ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16     ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL        ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8    ZK8
      CHARACTER*16          ZK16
      CHARACTER*24                  ZK24
      CHARACTER*32                          ZK32
      CHARACTER*80                                  ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C ----------------------------------------------------------------------
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------

      INTEGER NCMPMA,DIMAKI,NBMO1,IRET,DIMANV
C    DIMAKI = DIMENSION MAX DE LA LISTE DES RELATIONS KIT
      PARAMETER (DIMAKI=9)
C    DIMANV = DIMENSION MAX DE LA LISTE DU NOMBRE DE VAR INT EN THM
      PARAMETER (DIMANV=4)
      PARAMETER (NCMPMA=7+DIMAKI+DIMANV)
      CHARACTER*8  NOMCMP(NCMPMA),K8B
      CHARACTER*16 MOCLEF(2),K16BID,NOMCMD
      CHARACTER*19 COMPOR
      CHARACTER*24 MODELE
      LOGICAL CRILOC,MECA

      DATA NOMCMP/'RELCOM  ','NBVARI  ','DEFORM  ','INCELA  ',
     &     'C_PLAN  ','XXXX1','XXXX2','KIT1    ','KIT2    ','KIT3    ',
     &     'KIT4    ','KIT5    ','KIT6    ','KIT7    ','KIT8    ',
     &     'KIT9    ', 'NVI_C   ', 'NVI_T   ', 'NVI_H   ', 'NVI_M   '/
C     ------------------------------------------------------------------

      CALL JEMARQ()

C     initialisations
      CRILOC=.FALSE.
      MECA=.FALSE.
      MODELE = MODELZ
      COMPOR = '&&NMDORC.COMPOR'

      CALL GETRES(K8B,K16BID,NOMCMD)

C     MOCLEF= COMP_INCR / COMP_ELAS SUIVANT LES COMMANDES APPELANTES
C     CRILOC = EXISTENCE DE CRIT�RES LOCAUX DE CONVERGENCE
C     MECA=COMMANDES MECANIQUE

      IF (NOMCMD(1:13).EQ.'THER_NON_LINE') THEN
        NBMO1 = 1
        MOCLEF(1) = 'COMP_THER_NL'
      ELSE IF (NOMCMD(1:9).EQ.'LIRE_RESU') THEN
        NBMO1 = 1
        MOCLEF(1) = 'COMP_INCR'
        MECA=.TRUE.
      ELSEIF (NOMCMD(1:6) .EQ.'CALC_G') THEN
        NBMO1 = 2
        MOCLEF(1) = 'COMP_INCR'
        MOCLEF(2) = 'COMP_ELAS'
        MECA=.TRUE.
      ELSEIF ((NOMCMD(1:13).EQ.'STAT_NON_LINE').OR.
     &        (NOMCMD(1:13).EQ.'DYNA_NON_LINE').OR.
     &        (NOMCMD(1:6) .EQ.'CALCUL')      ) THEN
        NBMO1 = 2
        MOCLEF(1) = 'COMP_INCR'
        MOCLEF(2) = 'COMP_ELAS'
        MECA=.TRUE.
        CRILOC=.TRUE.
      ELSE
         CALL U2MESG('F','COMPOR1_51',1,NOMCMD,0,0,0,0.D0)
      ENDIF

C ======================================================================
C     CARTE COMPOR
      CALL NMDOCC(COMPOR,MODELE,NBMO1,MOCLEF,
     &            NOMCMP,NCMPMA,MECA,NOMCMD)
      COMPOZ = COMPOR

C ======================================================================

C     CARTE DE CRITERES LOCAUX
      IF (CRILOC) THEN
        CALL NMDOCR(CARCRI,MODELE,NBMO1,MOCLEF,IRET)
      ENDIF

C ======================================================================

      CALL JEDEMA()
      END
