      SUBROUTINE CM27NA(MAIN, NBMA  , NBNO  , LIMA  , TYPEMA  ,
     &                  MILIEU, NOMIMA, NOMIPE, MXNOFA, NBHE20,
     &                  NBTYMA, DEFFAC )

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 24/09/2007   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
      INTEGER      NFMAX
      PARAMETER  ( NFMAX = 24 )
      INTEGER      NBMA,NBNO,LIMA(*),MXNOFA,TYPEMA(*),NBTRI
      INTEGER      MILIEU(4,NFMAX,NBNO),NOMIMA(6,NBMA),NOMIPE(4,*),
     &             NBTYMA, DEFFAC(4,0:6,NBTYMA),NOEUD(4),NBHE20,
     &             FACE,INO,NBFA
      CHARACTER*8 MAIN
      CHARACTER*24 CONNEX
C ----------------------------------------------------------------------
C                   DETERMINATION DES NOEUDS DES FACES
C ----------------------------------------------------------------------
C IN  MAIN    MAILLAGE EN ENTREE (POUR CONNECTIVITE ET )
C IN  NBMA    NOMBRE DE MAILLES A TRAITER
C IN  NBNO    NOMBRE TOTAL DE NOEUDS DU MAILLAGE
C IN  LIMA    LISTE DES MAILLES A TRAITER
C IN  TYPEMA  LISTE DES TYPES DES MAILLES
C OUT MILIEU  REFERENCE DES FACES ET NOEUD MILIEU CORRESPONDANT
C OUT NOMIMA  LISTE DES NOEUDS MILIEUX PAR MAILLE
C OUT NOMIPE  LISTE DES NOEUDS PERES PAR NOEUDS MILIEUX
C OUT MXNOFA  NOMBRE DE NOEUDS MILIEUX CREES
C OUT NBHE20  NOMBRE DE MAILLES HEXA20
C IN  NBTYMA  NOMBRE DE TYPE DE MAILLES (ACTUELLEMENT 26)
C IN  DEFFAC  DEFINITION DES FACES (VOIR LA ROUTINE CM2027)
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

      INTEGER M, A, NO, MA, NBAR, NO1, NO2, NO3,NO4,I, NOMI, JTYP, TYMA
      INTEGER     JNOMA
      CHARACTER*8 KBID, NOMNOE
      REAL*8      RBID
C ----------------------------------------------------------------------
      CALL JEMARQ()
      CONNEX = MAIN//'.CONNEX'

C --- INITIALISATION

      MXNOFA = 0
      DO 2 M = 1, NBMA
         DO 3 A = 1,6
            NOMIMA(A,M) = 0
 3       CONTINUE
 2    CONTINUE

      DO 5 NO = 1, NBNO
         DO 6 FACE = 1, NFMAX
           DO 7 INO = 1 , 4
            MILIEU(INO,FACE,NO) = 0
 7         CONTINUE
 6       CONTINUE
 5    CONTINUE

      NBHE20=0
      DO 10 M = 1, NBMA
         MA = LIMA(M)
         TYMA = TYPEMA(MA)
C   DECOMPTE DU NOMBRE DE MAILLE HEXA20
         IF (TYMA.EQ.25)  NBHE20=NBHE20+1

         CALL JEVEUO(JEXNUM(CONNEX,MA),'L',JNOMA)

C ------ PARCOURS DES FACE DE LA MAILLE COURANTE

         NBFA = DEFFAC(1,0,TYMA)
         DO 20 FACE = 1, NBFA

C --------- NOEUDS SOMMETS DE LA FACE
            NO1 = ZI(JNOMA-1 + DEFFAC(1,FACE,TYMA))
            NO2 = ZI(JNOMA-1 + DEFFAC(2,FACE,TYMA))
            NO3 = ZI(JNOMA-1 + DEFFAC(3,FACE,TYMA))
            NO4 = ZI(JNOMA-1 + DEFFAC(4,FACE,TYMA))

            NOEUD(1)=NO1
            NOEUD(2)=NO2
            NOEUD(3)=NO3
            NOEUD(4)=NO4
            NBTRI=4
            CALL UTTRII(NOEUD,NBTRI)

            CALL ASSERT(NBTRI.EQ.4)
            NO1=NOEUD(1)
            NO2=NOEUD(2)
            NO3=NOEUD(3)
            NO4=NOEUD(4)

C --------- EST-CE QUE LA FACE EST DEJA REFERENCEE

            DO 30 I = 1, NFMAX
C ------------ FACE DEJA REFERENCEE
               IF ((MILIEU(1,I,NO1) .EQ. NO2).AND.
     &             (MILIEU(2,I,NO1) .EQ. NO3).AND.
     &             (MILIEU(3,I,NO1) .EQ. NO4)) THEN
                  NOMI = MILIEU(4,I,NO1)
                  GOTO 31

C ------------ NOUVELLE FACE
               ELSE IF (MILIEU(1,I,NO1) .EQ.0) THEN
                  MXNOFA = MXNOFA + 1
                  MILIEU(1,I,NO1) = NO2
                  MILIEU(2,I,NO1) = NO3
                  MILIEU(3,I,NO1) = NO4
                  MILIEU(4,I,NO1) = MXNOFA
                  NOMI = MXNOFA
                  GOTO 31
               END IF
 30         CONTINUE
C           PLUS DE NFMAX FACES TOUCHENT NO1 ?
            CALL JENUNO(JEXNUM(MAIN//'.NOMNOE',NO1), NOMNOE)
            CALL U2MESG('F', 'MAILLAGE_11', 1, NOMNOE, 1, NFMAX, 0,RBID)
 31         CONTINUE
            NOMIMA(FACE,M)  = NOMI
            NOMIPE(1,NOMI) = NO1
            NOMIPE(2,NOMI) = NO2
            NOMIPE(3,NOMI) = NO3
            NOMIPE(4,NOMI) = NO4

 20      CONTINUE
 10   CONTINUE

      CALL JEDEMA()
      END
