      SUBROUTINE ASSCHC(BASE,MATAS,NBCHC,LCHCI,NOMNU)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*) MATAS,LCHCI(*),NOMNU
      CHARACTER*1 BASE
      CHARACTER*4 CUMU
      INTEGER NBCHC
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ASSEMBLA  DATE 04/04/2006   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ======================================================================
C-----------------------------------------------------------------------
C OBJET :
C        TRAITEMENT DES CHARGE CINEMATIQUE DANS LES MATRICE ASSEMBLEES
C
C-----------------------------------------------------------------------
C IN   BASE    K*1     : 'G','V' BASE SUR LAQUELLE EST MATAS
C VAR  MATAS   K*19    : NOM DE LA MATR_ASSE
C IN   NBCHC   I       : NOMBRE DE CHARGE CINEMATIQUES
C IN   LCHCI   K*19    : LISTE DES NOMS DES CHARGES CINEMATIQUES
C                        L'EFFET DE CES CHARGES EST CUMULE DANS MATAS
C IN   NOMNU   K*14    : NOM DE LA NUMEROTATION
C-----------------------------------------------------------------------
C     FONCTIONS JEVEUX
C-----------------------------------------------------------------------
      CHARACTER*32 JEXNUM,JEXNOM,JEXATR
C-----------------------------------------------------------------------
C     COMMUNS   JEVEUX
C-----------------------------------------------------------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C----------------------------------------------------------------------
C     VARIABLES LOCALES
C----------------------------------------------------------------------
      CHARACTER*2 TYPSTO
      CHARACTER*8 KBID,GD
      CHARACTER*14 NU
      CHARACTER*19 MAT,NOMCH
C----------------------------------------------------------------------
C                DEBUT DES INSTRUCTIONS
      CALL JEMARQ()
C----------------------------------------------------------------------
      MAT = MATAS
      NU = NOMNU
      IF (NBCHC.EQ.0) GO TO 40

      CALL JEVEUO(NU//'.NUME.NEQU','L',IDEQU)
      NEQU = ZI(IDEQU)
      CALL DISMOI('F','NOM_GD',NU,'NUME_DDL',IBID,GD,IERD)
      CALL JENONU(JEXNOM('&CATA.GD.NOMGD',GD),NUMGD)
      CALL JEVEUO(JEXNUM('&CATA.GD.DESCRIGD',NUMGD),'L',IDDES)
      NEC = ZI(IDDES+2)

      CALL JEDETR(MAT//'.CCID')
      CALL WKVECT(MAT//'.CCID',BASE//' V I ',NEQU,JCCID)


      CALL JEVEUO(JEXNUM(NU//'.NUME.PRNO',1),'L',IDPRNO)
      DO 20 ICH = 1,NBCHC
        NOMCH = LCHCI(ICH)
        CALL JEVEUO(NOMCH//'.DEFI','L',IDEFI)
        NIMP = ZI(IDEFI)
        DO 10 IMP = 1,NIMP
          INO = ZI(IDEFI+3* (IMP-1)+1)
          IDDL = ZI(IDEFI+3* (IMP-1)+2)
          IEQ = ZI(IDPRNO-1+ (NEC+2)* (INO-1)+1) + IDDL - 1
          IEXI = ZI(JCCID-1+IEQ)
          IF (IEXI.EQ.0) ZI(JCCID-1+IEQ) = -1
   10   CONTINUE
   20 CONTINUE


C --- STOCKAGE DES LIGNES A ELIMINER CCID(IEQ)=-1 A CAUSE DE CUMU
C     POUR NE PAS RESTOCKER UNE LIGNE DEJA TRAITEE
      CALL WKVECT('&&ASSCHC.ELIM','V V I ',NEQU,IDELIM)
      NELIM = 0
      DO 30 IEQ = 1,NEQU
        IF (ZI(JCCID-1+IEQ).EQ.-1) THEN
          NELIM = NELIM + 1
          ZI(IDELIM-1+IEQ) = NELIM
          ZI(JCCID-1+IEQ) = 1
        END IF

   30 CONTINUE
      CALL ASMCHC(BASE,MAT,ZI(IDELIM),NELIM)
      CALL JEDETR('&&ASSCHC.ELIM')

   40 CONTINUE
      CALL JEDEMA()
      END
