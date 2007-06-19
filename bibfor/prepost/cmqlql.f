      SUBROUTINE CMQLQL ( MAIN, MAOUT, NBMA, LIMA)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 19/06/2007   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT NONE
      INTEGER NBMA, LIMA(NBMA)
      CHARACTER*8 MAIN,MAOUT
C ----------------------------------------------------------------------
C         TRANSFORMATION DES MAILLES QUADRATIQUES -> LINEAIRE
C-----------------------------------------------------------------------
C               SEG3          --> SEG2
C               TRIA6         --> TRIA3,
C               QUAD8,QUAD9   --> QUAD4,
C               TETRA10       --> TETRA4
C               PYRAM13       --> PYRMA5
C               PENTA15       --> PENTA6
C               HEXA20,HEXA27 --> HEWA8
C ----------------------------------------------------------------------
C IN        MAIN   K8  NOM DU MAILLAGE INITIAL
C IN/JXOUT  MAOUT  K8  NOM DU MAILLAGE TRANSFORME
C IN        NBMA    I  NOMBRE DE MAILLES A TRAITER
C IN        LIMA    I  NUMERO DES MAILLES A TRAITER
C ----------------------------------------------------------------------
C
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32 JEXNUM,JEXNOM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------

      INTEGER NBTYMA
      PARAMETER (NBTYMA=26)
      INTEGER I,JNOM,JMQ,ICOUNT,JNNMPM,JPPNM,ITYP,JNUM,
     &        NBTNM,JNMPM,JNON,NBTNMM,JNM,JDIM,IRET,TYMAQ(NBTYMA),
     &        NBTNO,NBNM,JREFE,NBNOMX,JTYP,NBTGNO
      CHARACTER*1 KBID
      CHARACTER*8 NOM,NOMAST(NBTYMA),K8B
      CHARACTER*19 COORDO
      CHARACTER*24 CONNEX,DIMEL,DIMEQ,NOMNOE,GROUPN
      CHARACTER*32 TYPMA

C     TYMAQ: TYPE DES MAILLES QUADRATIQUES (CF. CATALOGUE TYPE_MAILLE__)
C     NOMAST: NOM DES TYPES DE MAILLES     (CF. CATALOGUE TYPE_MAILLE__)
      DATA TYMAQ   /3*0,4,4*0,9,4*0,14,0,16,2*0,19,0,21,0,23,0,25,26/
      DATA NOMAST / 'POI1    ', 'SEG2    ', 'SEG22   ', 'SEG3    ',
     &              'SEG33   ', 'SEG4    ', 'TRIA3   ', 'TRIA33  ',
     &              'TRIA6   ', 'TRIA66  ', 'TRIA7   ', 'QUAD4   ',
     &              'QUAD44  ', 'QUAD8   ', 'QUAD88  ', 'QUAD9   ',
     &              'QUAD99  ', 'TETRA4  ', 'TETRA10 ', 'PENTA6  ',
     &              'PENTA15 ', 'PYRAM5  ', 'PYRAM13 ', 'HEXA8   ',
     &              'HEXA20  ', 'HEXA27  '/
C     ------------------------------------------------------------------
      CALL JEMARQ()

C     =========================================================
C     VERIFICATION QUE LE CATALOGUE DES TYPES DE MAILLE N'A PAS
C     ETE MODIFIE
C     =========================================================

      DO 10 ITYP = 1, NBTYMA
         CALL JENUNO (JEXNUM('&CATA.TM.NOMTM',ITYP),NOM)
         IF ( NOMAST(ITYP) .NE. NOM ) THEN
            CALL U2MESS('F','PREPOST_6')
         ENDIF
 10   CONTINUE
C
C     =========================================================
C     - ON VERIFIE SI DES MAILLES FOURNIES SONT QUADRATIQUES
C     - ON FILTRE EN NE RECUPERANT QUE LES MAILLES : SEG3,TRIA6,
C       QUAD8,QUAD9,TETRA10,PYRAM13,PENTA15,HEXA20,HEXA27.
C     =========================================================
C     ON RECUPERE LES NUMEROS DES MAILLES QUADRATIQUES:
      CALL WKVECT('&&CMQLQL.MAIL_QUAD','V V I',NBMA,JMQ)
      TYPMA=MAIN//'.TYPMAIL'
      CALL JEVEUO(TYPMA,'L',JTYP)
      ICOUNT=0
      DO 20 I=1,NBMA
         IF(TYMAQ(ZI(JTYP+LIMA(I)-1)).NE.0)THEN
            ICOUNT=ICOUNT+1
            ZI(JMQ+ICOUNT-1)=LIMA(I)
         ENDIF
 20   CONTINUE

C     NOMBRE DE MAILLES QUADRATIQUES :
      NBMA = ICOUNT
C
C     ===============================
C     RECUPERATION DES NOEUDS MILIEUX
C     ===============================
      DIMEL=MAIN//'.DIME'
      CALL JEVEUO(DIMEL,'L',JDIM)
      NBTNO=ZI(JDIM)
      CALL WKVECT('&&CMQLQL.NOEUD_MILIEU','V V I',NBTNO,JNM)
      CALL CMQLNM(MAIN,NBTNO,NBMA,ZI(JMQ),ZI(JNM),NBNM)
C
C     =========================================================
C     DUPLICATION DE CERTAINS OBJETS DE LA SD MAILLAGE INITIALE
C     AVANT REACTUALISATION
C     =========================================================
      CALL JEDUPO(MAIN//'.NOMMAI'  ,'G',MAOUT//'.NOMMAI'  ,.FALSE.)
      CALL JEDUPO(MAIN//'.DIME'    ,'G',MAOUT//'.DIME'    ,.FALSE.)
      CALL JEDUPO(MAIN//'.TYPMAIL' ,'G',MAOUT//'.TYPMAIL' ,.FALSE.)
C
      CALL JEEXIN(MAIN//'.GROUPEMA',IRET)
      IF(IRET.NE.0)THEN
        CALL JEDUPO(MAIN//'.GROUPEMA','G',MAOUT//'.GROUPEMA',.FALSE.)
      ENDIF
C
C     ACTUALISATION DE '.DIME'
      DIMEQ=MAOUT//'.DIME'
      CALL JEVEUO(DIMEQ,'E',JDIM)
      ZI(JDIM)=ZI(JDIM)-NBNM
C
C     ==================================================================
C     MISE A JOUR DES NOEUDS ('.COORDO','.NOMNOE','.GROUPENO')
C     ==================================================================

      CALL CMQLNO(MAIN,MAOUT,NBNM,ZI(JNM))
C
C     =============================================
C     MISE A JOUR DES MAILLES ('.CONNEX','.TYPMAI')
C     =============================================
C
      CALL CMQLMA(MAIN,MAOUT,NBMA,ZI(JMQ))
C
      CALL JEDETR('&&CMQLQL.MAIL_QUAD')
      CALL JEDETR('&&CMQLQL.NOEUD_MILIEU')
C
      CALL JEDEMA()
C
      END
