      SUBROUTINE CM18ND(NBNO, NBNOMI, NBMA, LIMA, TYPEMA, 
     &                  PREFIX, NDINIT, NOMIPE, NOMNOE, COOR)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 14/09/2010   AUTEUR REZETTE C.REZETTE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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

      INTEGER      NBNO, NBNOMI, NOMIPE(8,NBNOMI), NDINIT,NBMA
      INTEGER      LIMA(*),TYPEMA(*)
      REAL*8       COOR(3,*)
      CHARACTER*8  PREFIX
      CHARACTER*24 NOMNOE


C ----------------------------------------------------------------------
C         CREATION DES NOEUDS MILIEUX (CREA_MAILLAGE PENTA15_18)
C ----------------------------------------------------------------------
C IN        NBNO    NOMBRE TOTAL DE NOEUDS DU MAILLAGE
C IN        NBNOMI  NOMBRE DE NOEUDS CREES DES FACES
C IN        NBMA    NOMBRE DE MAILLE
C IN        TYPEMA  TYPE DE MAILLES
C IN        PREFIX  PREFIXE POUR LE NOM DES NOEUDS (EX : N, NS, ...)
C IN        NDINIT  NUMERO INITIAL DES NOEUDS CREES
C IN        NOMIPE  LISTE DES PERES PAR NOEUDS CREES (NOEUDS SOMMETS)
C IN/JXVAR  NOMNOE  REPERTOIRE DE NOMS DES NOEUDS
C VAR       COOR    COORDONNEES DES NOEUDS
C ----------------------------------------------------------------------

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

      INTEGER NO,NO1,NO2,NO3,NO4,NO5,NO6,NO7,NO8, LGPREF,LGND, IRET
      INTEGER LXLGUT,IMA,TYMA,JNOMA,MA

      CHARACTER*8  NOMND
      CHARACTER*24 VALK
      CHARACTER*80 KNUME
C ----------------------------------------------------------------------
      CALL JEMARQ()

C - INSERTION DES NOUVEAUX NOEUDS

      LGPREF = LXLGUT(PREFIX)
      DO 10 NO = 1, NBNOMI

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

      CALL JEDEMA()
      END
