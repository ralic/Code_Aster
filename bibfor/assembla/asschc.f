      SUBROUTINE ASSCHC(MATAS,NBCHC,LCHCI,NOMNU,CUMUL)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*) MATAS,LCHCI(*),NOMNU
      CHARACTER*1 BASE
      INTEGER NBCHC
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ASSEMBLA  DATE 08/10/2007   AUTEUR PELLET J.PELLET 
C RESPONSABLE VABHHTS J.PELLET
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
C  BUT : ON NOTE LES DDLS ELIMINES PAR LES CHARGES CINEMATIQUES
C
C  REMARQUE : LE RESTE DU TRAITEMENT DES CHARGES CINEMATIQUES EST FAIT
C             AU DERNIER MOMENT (ASMCHC+CSMBGG)
C
C-----------------------------------------------------------------------
C VAR  MATAS   K*19    : NOM DE LA MATR_ASSE
C IN   NBCHC   I       : NOMBRE DE CHARGE CINEMATIQUES
C IN   LCHCI   K*19    : LISTE DES NOMS DES CHARGES CINEMATIQUES
C                        L'EFFET DE CES CHARGES EST CUMULE DANS MATAS
C IN   NOMNU   K*14    : NOM DE LA NUMEROTATION
C IN   CUMUL   K4      : 'ZERO' / 'CUMU'
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
      CHARACTER*4 CUMUL
      CHARACTER*8 KBID,GD
      CHARACTER*14 NU
      CHARACTER*19 MAT,NOMCH
C----------------------------------------------------------------------
C                DEBUT DES INSTRUCTIONS
      CALL JEMARQ()
C----------------------------------------------------------------------
      MAT = MATAS
      CALL JEVEUO(MAT//'.REFA','E',JREFA)
      NU = NOMNU
      CALL ASSERT(ZK24(JREFA-1+2).EQ.NU)
      IF (NBCHC.EQ.0) GO TO 40

      CALL JEVEUO(NU//'.NUME.NEQU','L',JNEQU)
      NEQ = ZI(JNEQU)
      CALL DISMOI('F','NOM_GD',NU,'NUME_DDL',IBID,GD,IERD)
      CALL JENONU(JEXNOM('&CATA.GD.NOMGD',GD),NUMGD)
      CALL JEVEUO(JEXNUM('&CATA.GD.DESCRIGD',NUMGD),'L',IDDES)
      NEC = ZI(IDDES+2)
      CALL JELIRA(MAT//'.REFA','CLAS',IBID,BASE)

      IF (CUMUL.EQ.'ZERO') THEN
        CALL JEDETR(MAT//'.CCID')
        CALL WKVECT(MAT//'.CCID',BASE//' V I ',NEQ+1,JCCID)
      ELSE IF (CUMUL.EQ.'CUMU') THEN
        CALL JEVEUO(MAT//'.CCID','E',JCCID)
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF


      CALL JEVEUO(JEXNUM(NU//'.NUME.PRNO',1),'L',IDPRNO)
      NELIM=0
      DO 20 ICH = 1,NBCHC
        NOMCH = LCHCI(ICH)
        CALL JEVEUO(NOMCH//'.AFCI','L',JAFCI)
        NIMP = ZI(JAFCI)
        DO 10 IMP = 1,NIMP
          INO = ZI(JAFCI+3* (IMP-1)+1)
          IDDL = ZI(JAFCI+3* (IMP-1)+2)
          IEQ = ZI(IDPRNO-1+ (NEC+2)* (INO-1)+1) + IDDL - 1
          ZI(JCCID-1+IEQ) = 1
   10   CONTINUE
   20 CONTINUE

      NELIM=0
      DO 30, IEQ=1,NEQ
        IF (ZI(JCCID-1+IEQ).EQ.1) NELIM=NELIM+1
   30 CONTINUE
      ZI(JCCID-1+NEQ+1) = NELIM
      IF (NELIM.GT.0) ZK24(JREFA-1+3)='ELIML'


   40 CONTINUE
      CALL JEDEMA()
      END
