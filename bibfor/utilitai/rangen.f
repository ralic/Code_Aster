      SUBROUTINE RANGEN(NOMNU,ISST,INUMOD,IRANG)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)     NOMNU
      INTEGER                         ISST,INUMOD,IRANG
C     ------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 14/01/98   AUTEUR VABHHTS J.PELLET 
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
C     ------------------------------------------------------------
C     DONNE LE NUMERO DU NOEUD
C           NUNOE = 0 SI LE NOEUD N'EXISTE PAS
C     DONNE LE NUMERO DU DDL ASSOCIE AU NOEUD ET A SA COMPOSANTE
C           NUDDL = 0 SI LE COUPLE (NOEUD,COMPOSANTE) N'EXISTE PAS
C     ------------------------------------------------------------
C IN  NUMEGE    : K14: NOM D'UN NUME_DDL
C IN  ISST   : I  : NUMERO DE LA SOUS-STRUCTURE
C IN  INUMOD : I  : NUMERO DU MODE DYNAMIQUE OU CONTRAINT ASSOCIEE A
C                   LA SOUS-STRUCTURE
C OUT IRANG  : I  : POSITION DANS LE VECTEUR DES DDLS GENERALISES
C                   DU MODE
C     -------------------------------------------------------------
C     -- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*6      PGC, PGCANC
      COMMON  /NOMAJE/ PGC
C     ---  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C     -----------------------------------------------------------------
C     FONCTION EXTERNE
      INTEGER      NBEC
      CHARACTER*32 JEXNUM, JEXNOM
C     ----------------------------------------------------------------
      CHARACTER*8 C8BID
C     ----------------------------------------------------------------
      CALL JEMARQ()
      PGC    = 'RANGEN'
C
C
C     --- NUMERO DDL DU NOEUD NOEUD ET DE SA COMPOSANTE CMP

      CALL JENONU(JEXNOM(NOMNU(1:19)//'.LILI','&SOUSSTR'),IBID)
      CALL JEVEUO(JEXNUM(NOMNU(1:19)//'.PRNO',IBID),'L',IAPRNO)
      IDDL  = ZI(IAPRNO+2*ISST-2)
      IF (IDDL.EQ.0) GOTO 9999
      IRANG = INUMOD + IDDL - 1
C
 9999 CONTINUE
      CALL JEDEMA()
      END
