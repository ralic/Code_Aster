      SUBROUTINE CONNEC(NOMTE,NSE,NNOP2,C)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT NONE

      INCLUDE 'jeveux.h'
      CHARACTER*16 NOMTE
      INTEGER NSEMAX, NNOMAX
C-----------------------------------------------------------------------
      INTEGER IBID 
C-----------------------------------------------------------------------
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


      INTEGER NNO,I,J,IADZI,IAZK24
      CHARACTER*8 ALIAS8
      LOGICAL LTEATT

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

      CALL TEATTR(' ','S','ALIAS8',ALIAS8,IBID)

      IF (LTEATT(' ','LUMPE','OUI').AND.
     &          (ALIAS8(6:8).EQ.'SE3')) THEN
        NNOP2 = 3
        NSE = 2
        C(1,1) = 1
        C(1,2) = 3
        C(2,1) = C(1,2)
        C(2,2) = 2

      ELSE IF (LTEATT(' ','LUMPE','OUI').AND.
     &          (ALIAS8(6:8).EQ.'TR6')) THEN
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
      ELSE IF (LTEATT(' ','LUMPE','OUI').AND.
     &        (ALIAS8(6:8).EQ.'QU9')) THEN
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
