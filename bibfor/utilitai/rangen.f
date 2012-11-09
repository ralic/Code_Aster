      SUBROUTINE RANGEN(PRGENE,ISST,INUMOD,IRANG)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'

      CHARACTER*32 JEXNUM,JEXNOM
      CHARACTER*19     PRGENE
      INTEGER                         ISST,INUMOD,IRANG
C     ------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
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
C     -----------------------------------------------------------------
C     FONCTION EXTERNE
C     ----------------------------------------------------------------
C     ----------------------------------------------------------------
C-----------------------------------------------------------------------
      INTEGER IAPRNO ,IBID ,IDDL
C-----------------------------------------------------------------------
      CALL JEMARQ()
C
C
C     --- NUMERO DDL DU NOEUD NOEUD ET DE SA COMPOSANTE CMP

      CALL JENONU(JEXNOM(PRGENE//'.LILI','&SOUSSTR'),IBID)
      CALL JEVEUO(JEXNUM(PRGENE//'.PRNO',IBID),'L',IAPRNO)
      IDDL  = ZI(IAPRNO+2*ISST-2)
      IF (IDDL.EQ.0) GOTO 9999
      IRANG = INUMOD + IDDL - 1
C
 9999 CONTINUE
      CALL JEDEMA()
      END
