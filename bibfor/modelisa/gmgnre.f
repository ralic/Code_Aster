      SUBROUTINE GMGNRE(NOMA,NBNOTO,LITRAV,LISTMA,NBMA,LISTNO,
     &                  NBNO,SELEZ)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 14/09/2010   AUTEUR REZETTE C.REZETTE 
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
      IMPLICIT REAL*8 (A-H,O-Z)

C     ARGUMENTS:
C     ----------
      CHARACTER*8 NOMA
      CHARACTER*(*) SELEZ
      CHARACTER*16  SELEC
      INTEGER NBMA,NBNOTO,NBNO,LISTMA(*),LISTNO(*),LITRAV(*)
C ----------------------------------------------------------------------
C     BUT: REMPLIR LA LISTE DE NOEUD SOUS-JACENTE A LA LISTE DE MAILLE
C
C     IN: NOMA   : NOM DU MAILLAGE
C         NBNOTO : NOMBRE DE NOEUDS TOTAL DU MAILLAGE.
C         LISTMA : LISTE DES NUMEROS DE MAILLES A TRAITER.
C           NBMA : NOMBRE DE MAILLES DANS LA LISTE.
C         LITRAV : VECTEUR DE TRAVAIL.
C         SELEC  :  SELECTION DES NOEUDS (TOUS, SOMMET, MILIEU, CENTRE)
C
C     OUT:
C         LISTNO : LISTE DES NOEUDS TROUVES
C          NBNO  : NOMBRE DE NOEUDS TROUVE.
C ----------------------------------------------------------------------
C
C     FONCTIONS EXTERNES:
C     -------------------
C
C---------------- COMMUNS NORMALISES  JEVEUX  --------------------------
      CHARACTER*32 JEXNUM,JEXNOM,JEXATR,JEXR8
      COMMON /IVARJE/ZI(1)
      COMMON /RVARJE/ZR(1)
      COMMON /CVARJE/ZC(1)
      COMMON /LVARJE/ZL(1)
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      INTEGER ZI
      REAL*8 ZR
      COMPLEX*16 ZC
      LOGICAL ZL
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
C ---------------- FIN COMMUNS NORMALISES  JEVEUX  --------------------
C     VARIABLES LOCALES:
C     ------------------
      CHARACTER*8  K8BID,NOMMA,NOMNO, TYPM, NOTYMA(19)
      INTEGER      POSINI, POSFIN, JTYPM, SEL, NUTYMA
      INTEGER      PINI(3,19), PFIN(3,19)

      DATA NOTYMA / 'POI1'  ,
     &              'SEG2'  , 'SEG3'   ,
     &              'TRIA3' , 'TRIA6'  , 'TRIA7',
     &              'QUAD4' , 'QUAD8'  , 'QUAD9',
     &              'TETRA4', 'TETRA10',
     &              'PENTA6', 'PENTA15','PENTA18',
     &              'PYRAM5', 'PYRAM13',
     &              'HEXA8' , 'HEXA20' , 'HEXA27' /




      DATA PINI / 1, 0, 0,
     &            1, 0, 0,
     &            1, 3, 0,
     &            1, 0, 0,
     &            1, 4, 0,
     &            1, 4, 7,
     &            1, 0, 0,
     &            1, 5, 0,
     &            1, 5, 9,
     &            1, 0, 0,
     &            1, 5, 0,
     &            1, 0, 0,
     &            1, 7, 0,
     &            1, 7, 16,
     &            1, 0, 0,
     &            1, 6, 0,
     &            1, 0, 0,
     &            1, 9, 0,
     &            1, 9, 21 /

      DATA PFIN / 1, 0, 0,
     &            2, 0, 0,
     &            2, 3, 0,
     &            3, 0, 0,
     &            3, 6, 0,
     &            3, 6, 7,
     &            4, 0, 0,
     &            4, 8, 0,
     &            4, 8, 9,
     &            4, 0, 0,
     &            4, 10,0,
     &            6, 0, 0,
     &            6, 15,0,
     &            6, 15,18,
     &            5, 0, 0,
     &            5, 13,0,
     &            8, 0, 0,
     &            8, 20,0,
     &            8, 20,27 /

C
C
C     -- ON PARCOURE LA LISTE DES MAILLES ET ON COCHE LES NOEUDS
C     -- DANS LITRAV:
C
      CALL JEMARQ()
      SELEC = SELEZ
      CALL JEVEUO(NOMA // '.TYPMAIL','L', JTYPM)

      IF (SELEC .EQ. 'TOUS'  ) SEL=0
      IF (SELEC .EQ. 'SOMMET') SEL=1
      IF (SELEC .EQ. 'MILIEU') SEL=2
      IF (SELEC .EQ. 'CENTRE') SEL=3

      DO 1 I =1,NBNOTO
            LITRAV(I) =0
 1    CONTINUE

      DO 2 I   =1,NBMA
        IMA=LISTMA(I)
        CALL JEVEUO(JEXNUM(NOMA//'.CONNEX',IMA),'L',IACNEX)
        CALL JELIRA(JEXNUM(NOMA//'.CONNEX',IMA),'LONMAX',NBNOMA,K8BID)

        IF (SEL.EQ.0) THEN
          POSINI = 1
          POSFIN = NBNOMA
        ELSE
          CALL JENUNO ( JEXNUM('&CATA.TM.NOMTM', ZI(JTYPM+IMA-1)),TYPM)
                DO 10 NUTYMA = 1,18
                  IF (TYPM.EQ.NOTYMA(NUTYMA)) THEN
                   POSINI = PINI(SEL,NUTYMA)
                   POSFIN = PFIN(SEL,NUTYMA)
                   GOTO 20
                 END IF
 10       CONTINUE
          CALL U2MESK('F','MODELISA4_68',1,TYPM)
 20       CONTINUE
          IF (POSFIN .EQ. 0) GOTO 2
        END IF

        DO 3 INO = POSINI, POSFIN
          NUMNO=ZI(IACNEX-1+INO)
          LITRAV(NUMNO)= LITRAV(NUMNO) +1
 3      CONTINUE
 2    CONTINUE

C     -- ON COMPTE LES NOEUDS COCHES ET ON LES RECOPIE DANS LISTNO:

      NBNO=0
      DO 4 I   =1,NBNOTO
         IF  (LITRAV(I).GT.0) THEN
            NBNO=NBNO+1
            LISTNO(NBNO)=I
         END IF
 4    CONTINUE
C
 9999 CONTINUE
      CALL JEDEMA()
      END
