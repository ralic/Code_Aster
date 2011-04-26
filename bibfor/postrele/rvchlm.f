      SUBROUTINE RVCHLM(SSCH19,M2D,NOEUD,NBN,NBCMP,NBCO,NBSP,VAL)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C
      CHARACTER*19 SSCH19
      INTEGER      M2D,NOEUD(*),NBN,NBCMP,NBCO,NBSP
      REAL*8       VAL(*)
C
C**********************************************************************
C
C  OPERATION REALISEE
C  ------------------
C     OPERATION D' EXTRACTION SUR DES NOEUD D' UNE MAILLE
C
C  ARGUMENT EN ENTREE
C  ------------------
C
C     SSCH19 : NOM DU SOUS CHAMP DE GRANDEUR
C     M2D    : NUMERO DE LA MAILLE (2D)
C     NOEUD  : TABLE DES NUMEROS DE NOEUDS
C     NBN    : NOMBRE DE NOEUD A TRAITER
C     NBCMP  : NOMBRE DE CMP A EXTRAIRE
C     NBCO   : NOMBRE DE COUCHES
C     NBSP   : NOMBRE DE COUCHES
C
C  ARGUMENT EN SORTIE
C  ------------------
C
C     VAL : TABLE DES VALEURS EXTRAITES
C
C**********************************************************************
C
C  FONCTIONS EXTERNES
C  ------------------
C
      CHARACTER*32 JEXNUM
C
C  DECLARATION DES COMMUNS NORMALISES JEVEUX
C  -----------------------------------------
C
      INTEGER         ZI
      COMMON /IVARJE/ ZI(1)
      REAL*8          ZR
      COMMON /RVARJE/ ZR(1)
      COMPLEX*16      ZC
      COMMON /CVARJE/ ZC(1)
      LOGICAL         ZL
      COMMON /LVARJE/ ZL(1)
      CHARACTER*8     ZK8
      CHARACTER*16    ZK16
      CHARACTER*24    ZK24
      CHARACTER*32    ZK32
      CHARACTER*80    ZK80
      COMMON /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
C  FIN DES COMMUNS NORMALISES JEVEUX
C  ---------------------------------
C
C  VARIABLES LOCALES
C  -----------------
C
      CHARACTER*24 NVALE,NPADR,NNUMLO,NNOMA
      CHARACTER*8  MAILLA
C
      INTEGER AVALE,APADR,ANUMLO,APNCO,APNSP,NBTND
      INTEGER ADR,ACONEC,I,ND,NLOC,J,ADRM,ADRN,LCOI,LNDI,LNDO,LCOO
C
      LOGICAL TROUVE
      CHARACTER*1 K1BID
C
C==================== CORPS DE LA ROUTINE =============================
C
      CALL JEMARQ()
      NVALE  = SSCH19//'.VALE'
      NPADR  = SSCH19//'.PADR'
      NNOMA  = SSCH19//'.NOMA'
      NNUMLO = '&&RVCHLM.NUM.LOC.NOEUD '
C
      CALL JEVEUO(NNOMA,'L',ADR)
      CALL JEVEUO(NVALE,'L',AVALE)
      CALL JEVEUO(NPADR,'L',APADR)
      CALL JEVEUO(SSCH19//'.PNCO','L',APNCO)
      CALL JEVEUO(SSCH19//'.PNSP','L',APNSP)
C
      ADRM = ZI(APADR + M2D-1)
C
      MAILLA = ZK8(ADR)
C
      CALL JEVEUO(JEXNUM(MAILLA//'.CONNEX',M2D),'L',ACONEC)
      CALL JELIRA(JEXNUM(MAILLA//'.CONNEX',M2D),'LONMAX',
     +            NBTND,K1BID)
C
      LNDI = NBCMP*ZI(APNSP + M2D-1)
      LCOI = LNDI*NBTND
      LNDO = NBCMP*NBSP
      LCOO = LNDO*NBN
C
      CALL WKVECT(NNUMLO,'V V I',NBN,ANUMLO)
C
      DO 100, I = 1, NBN, 1
C
         TROUVE = .FALSE.
         ND     =  NOEUD(I)
         NLOC   =  1
C
110      CONTINUE
         IF ( .NOT. TROUVE ) THEN
C
            IF ( ZI(ACONEC + NLOC-1) .EQ. ND ) THEN
C
               TROUVE = .TRUE.
C
            ELSE
C
               NLOC = NLOC +1
C
            ENDIF
C
            GOTO 110
C
         ENDIF
C
         ZI(ANUMLO + I-1) = NLOC
C
100   CONTINUE
C
      DO 200, I = 1, NBN, 1
C
         ADRN = ADRM + (ZI(ANUMLO + I-1)-1)*LNDI
C
         DO 210, J = 1, NBCO, 1
C
            DO 211, K = 1, LNDO, 1
C
               VAL(K + (J-1)*LCOO + LNDO*(I-1)) =
     +                            ZR(AVALE + ADRN-1 + (J-1)*LCOI + K-1)
C
211         CONTINUE
C
210      CONTINUE
C
200   CONTINUE
C
      CALL JEDETR(NNUMLO)
C
      CALL JEDEMA()
      END
