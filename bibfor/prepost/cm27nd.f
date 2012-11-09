      SUBROUTINE CM27ND(NBNO  , NBNOMI, NBNOHE, NBMA, LIMA, TYPEMA,
     &                  CONNEZ, PREFIX, NDINIT, NOMIPE, NOMNOE, COOR)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
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

      IMPLICIT NONE

      INCLUDE 'jeveux.h'

      CHARACTER*32 JEXNUM,JEXNOM
      INTEGER      NBNO, NBNOMI, NBNOHE, NOMIPE(8,NBNOMI), NDINIT,NBMA
      INTEGER      LIMA(*),TYPEMA(*)
      REAL*8       COOR(3,*)
      CHARACTER*8  PREFIX
      CHARACTER*24 NOMNOE,CONNEX
      CHARACTER*(*) CONNEZ


C ----------------------------------------------------------------------
C         CREATION DES NOEUDS MILIEUX (CREA_MAILLAGE HEXA20_27)
C ----------------------------------------------------------------------
C IN        NBNO    NOMBRE TOTAL DE NOEUDS DU MAILLAGE
C IN        NBNOMI  NOMBRE DE NOEUDS CREES DES FACES ( S Y AJOUTE LE
C                   NOEUD CENTRAL POUR LES HEXA20 )
C IN        NBNOHE  NOMBRE DE NOEUDS CENTRAUX
C                   ( IE NOMBRE DE MAILLE HEXA20)
C IN        NBMA    NOMBRE DE MAILLE
C IN        TYPEMA  TYPE DE MAILLES
C IN        CONNEZ  CONNECTIVITE DES MAILLES
C IN        PREFIX  PREFIXE POUR LE NOM DES NOEUDS (EX : N, NS, ...)
C IN        NDINIT  NUMERO INITIAL DES NOEUDS CREES
C IN        NOMIPE  LISTE DES PERES PAR NOEUDS CREES (NOEUDS SOMMETS)
C IN/JXVAR  NOMNOE  REPERTOIRE DE NOMS DES NOEUDS
C VAR       COOR    COORDONNEES DES NOEUDS
C ----------------------------------------------------------------------


      INTEGER NO,NO1,NO2,NO3,NO4,NO5,NO6,NO7,NO8,NO9, LGPREF,LGND, IRET
      INTEGER NO10,NO11,NO12,NO13,NO14,NO15,NO16,NO17,NO18,NO19,NO20
      INTEGER LXLGUT,IMA,TYMA,JNOMA,MA,IHEX27

      CHARACTER*8  NOMND
      CHARACTER*24 VALK
      CHARACTER*80 KNUME
C ----------------------------------------------------------------------
      CALL JEMARQ()

      CONNEX=CONNEZ

C - INSERTION DES NOUVEAUX NOEUDS

      LGPREF = LXLGUT(PREFIX)
      DO 10 NO = 1, NBNOMI+NBNOHE

C      NOM DU NOEUD CREE
        CALL CODENT(NDINIT-1+NO,'G',KNUME)
        LGND = LXLGUT(KNUME)
        IF (LGND+LGPREF.GT.8) CALL U2MESS('F','ALGELINE_16')
        NOMND = PREFIX(1:LGPREF) // KNUME

C      DECLARATION DU NOEUD CREE
        CALL JEEXIN(JEXNOM(NOMNOE,NOMND),IRET)
        IF (IRET.EQ.0) THEN
          CALL JECROC(JEXNOM(NOMNOE,NOMND))
        ELSE
          VALK = NOMND
          CALL U2MESG('F', 'ALGELINE4_5',1,VALK,0,0,0,0.D0)
        END IF

 10   CONTINUE

C - CALCUL DES COORDONNEES DES NOUVEAUX NOEUDS DES FACES
      DO 20 NO = 1, NBNOMI
        NO1 = NOMIPE(1,NO)
        NO2 = NOMIPE(2,NO)
        NO3 = NOMIPE(3,NO)
        NO4 = NOMIPE(4,NO)
        NO5 = NOMIPE(5,NO)
        NO6 = NOMIPE(6,NO)
        NO7 = NOMIPE(7,NO)
        NO8 = NOMIPE(8,NO)
        COOR(1,NO+NBNO) = -(COOR(1,NO1) + COOR(1,NO2) +
     &                      COOR(1,NO3) + COOR(1,NO4))/4.D0+
     &                     (COOR(1,NO5) + COOR(1,NO6)+
     &                      COOR(1,NO7) + COOR(1,NO8))/2.D0

        COOR(2,NO+NBNO) = -(COOR(2,NO1) + COOR(2,NO2) +
     &                      COOR(2,NO3) + COOR(2,NO4))/4.D0+
     &                     (COOR(2,NO5) + COOR(2,NO6)+
     &                      COOR(2,NO7) + COOR(2,NO8))/2.D0

        COOR(3,NO+NBNO) = -(COOR(3,NO1) + COOR(3,NO2) +
     &                      COOR(3,NO3) + COOR(3,NO4))/4.D0+
     &                     (COOR(3,NO5) + COOR(3,NO6)+
     &                      COOR(3,NO7) + COOR(3,NO8))/2.D0
 20   CONTINUE

C - CALCUL DES COORDONNEES DES NOUVEAUX NOEUDS CENTRAUX A LA MAILLE
      IHEX27=0
      DO 30 IMA = 1, NBMA
        MA=LIMA(IMA)
        TYMA = TYPEMA(MA)
        IF (TYMA.EQ.26) THEN
          CALL JEVEUO(JEXNUM(CONNEX,MA),'L',JNOMA)
          IHEX27=IHEX27+1

          NO1 = ZI(JNOMA-1+1)
          NO2 = ZI(JNOMA-1+2)
          NO3 = ZI(JNOMA-1+3)
          NO4 = ZI(JNOMA-1+4)
          NO5 = ZI(JNOMA-1+5)
          NO6 = ZI(JNOMA-1+6)
          NO7 = ZI(JNOMA-1+7)
          NO8 = ZI(JNOMA-1+8)
          NO9 = ZI(JNOMA-1+9)
          NO10 = ZI(JNOMA-1+10)
          NO11 = ZI(JNOMA-1+11)
          NO12 = ZI(JNOMA-1+12)
          NO13 = ZI(JNOMA-1+13)
          NO14 = ZI(JNOMA-1+14)
          NO15 = ZI(JNOMA-1+15)
          NO16 = ZI(JNOMA-1+16)
          NO17 = ZI(JNOMA-1+17)
          NO18 = ZI(JNOMA-1+18)
          NO19 = ZI(JNOMA-1+19)
          NO20 = ZI(JNOMA-1+20)

          COOR(1,NBNOMI+NBNO+IHEX27) = (-COOR(1,NO1) - COOR(1,NO2) -
     &                       COOR(1,NO3) - COOR(1,NO4) -
     &                       COOR(1,NO5) - COOR(1,NO6) -
     &                       COOR(1,NO7) - COOR(1,NO8) +
     &                       COOR(1,NO9) + COOR(1,NO10) +
     &                       COOR(1,NO11) + COOR(1,NO12) +
     &                       COOR(1,NO13) + COOR(1,NO14) +
     &                       COOR(1,NO15) + COOR(1,NO16) +
     &                       COOR(1,NO17) + COOR(1,NO18) +
     &                       COOR(1,NO19) + COOR(1,NO20))/4.D0

          COOR(2,NBNOMI+NBNO+IHEX27) = (-COOR(2,NO1) - COOR(2,NO2) -
     &                       COOR(2,NO3) - COOR(2,NO4) -
     &                       COOR(2,NO5) - COOR(2,NO6) -
     &                       COOR(2,NO7) - COOR(2,NO8) +
     &                       COOR(2,NO9) + COOR(2,NO10) +
     &                       COOR(2,NO11) + COOR(2,NO12) +
     &                       COOR(2,NO13) + COOR(2,NO14) +
     &                       COOR(2,NO15) + COOR(2,NO16) +
     &                       COOR(2,NO17) + COOR(2,NO18) +
     &                       COOR(2,NO19) + COOR(2,NO20))/4.D0

          COOR(3,NBNOMI+NBNO+IHEX27) = (-COOR(3,NO1) - COOR(3,NO2) -
     &                       COOR(3,NO3) - COOR(3,NO4) -
     &                       COOR(3,NO5) - COOR(3,NO6) -
     &                       COOR(3,NO7) - COOR(3,NO8) +
     &                       COOR(3,NO9) + COOR(3,NO10) +
     &                       COOR(3,NO11) + COOR(3,NO12) +
     &                       COOR(3,NO13) + COOR(3,NO14) +
     &                       COOR(3,NO15) + COOR(3,NO16) +
     &                       COOR(3,NO17) + COOR(3,NO18) +
     &                       COOR(3,NO19) + COOR(3,NO20))/4.D0
        ENDIF
 30   CONTINUE

      CALL JEDEMA()
      END
