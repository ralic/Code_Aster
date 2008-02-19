      SUBROUTINE CONNEC(NOMTE,NSE,NNOP2,C)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 19/02/2008   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
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
      IMPLICIT REAL*8 (A-H,O-Z)

      CHARACTER*16 NOMTE
      INTEGER NSEMAX, NNOMAX
      PARAMETER (NSEMAX = 6)
      PARAMETER (NNOMAX = 9)
      INTEGER NSE,NNOP2,C(NSEMAX, NNOMAX)

C ......................................................................
C    - FONCTION REALISEE:  INITIALISATION DES ELEMENTS ISO-P2

C    - ARGUMENTS:
C        DONNEES:    NOMTE         -->  NOM DU TYPE ELEMENT
C        SORTIES:    NSE           <--  NOMBRE DE SOUS-ELEMENTS P1
C                    NNOP2         <--  NOMBRE DE NOEUD DE L'ELEMENT P2
C                    C (NSE*NNO)   <--  CONNECTIVITE DES SOUS-ELEMENTS
C ......................................................................

C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      CHARACTER*32 JEXNUM,JEXNOM,JEXR8,JEXATR
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
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------

      INTEGER NNO,I,J,IADZI,IAZK24

      CALL TECAEL(IADZI,IAZK24)
      NNO = ZI(IADZI-1+2)

C INITIALISATION DU TABLEAU COMPLET

      NSE = 1
      NNOP2 = NNO
      DO 20 I = 1,NSEMAX
        DO 10 J = 1,NNOMAX
          C(I,J) = J
   10   CONTINUE
   20 CONTINUE

C CONNECTIVITE DES SOUS ELEMENTS (ELEMENTS ISO_P2)

      IF (NOMTE(5:7).EQ.'SL3') THEN
        NNOP2 = 3
        NSE = 2
        C(1,1) = 1
        C(1,2) = 3
        C(2,1) = C(1,2)
        C(2,2) = 2
      ELSE IF (NOMTE(5:7).EQ.'TL6') THEN
        NNOP2 = 6
        NSE = 4
        C(1,1) = 1
        C(1,2) = 4
        C(1,3) = 6
        C(2,1) = C(1,2)
        C(2,2) = 2
        C(2,3) = 5
        C(3,1) = C(1,3)
        C(3,2) = C(2,3)
        C(3,3) = 3
        C(4,1) = C(1,2)
        C(4,2) = C(2,3)
        C(4,3) = C(1,3)
      ELSE IF (NOMTE(5:7).EQ.'QL9') THEN
        NNOP2 = 9
        NSE = 4
        C(1,1) = 1
        C(1,2) = 5
        C(1,3) = 9
        C(1,4) = 8
        C(2,1) = C(1,2)
        C(2,2) = 2
        C(2,3) = 6
        C(2,4) = C(1,3)
        C(3,1) = C(1,3)
        C(3,2) = C(2,3)
        C(3,3) = 3
        C(3,4) = 7
        C(4,1) = C(1,4)
        C(4,2) = C(1,3)
        C(4,3) = C(3,4)
        C(4,4) = 4
      END IF

      END
