      SUBROUTINE LBORD(MAIL,LIMA,NLIMA,NTM,BASE,NOMZ)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 09/10/2002   AUTEUR DURAND C.DURAND 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C                                                                       
C                                                                       
C ======================================================================
C A_UTIL
C ----------------------------------------------------------------------
C             LISTE DES MAILLES DE BORD D'UN GROUPE DE MAILLE
C ----------------------------------------------------------------------
C VARIABLES D'ENTREE
C CHARACTER*8    MAIL        : SD MAILLAGE  
C INTEGER        LIMA(NLIMA) : LISTE DE MAILLES 
C INTEGER        NLIMA       : NOMBRE DE MAILLES
C CHARACTER*8    NTM(*)      : VECTEUR NOMS TYPES DE MAILLE
C CHARACTER*1    BASE        : BASE DE CREATION DE NOM
C CHARACTER*(19) NOMZ        : NOM DE L'OBJET MAILLES DE BORD
C
C SD DE SORTIE
C NOM.BORD : LISTE DES MAILLES DE BORD DE LIMA
C            (MA1, MA2, ...) MA* INDEX DANS LIMA
C ----------------------------------------------------------------------

      IMPLICIT NONE

C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      CHARACTER*32       JEXNUM , JEXNOM , JEXR8 , JEXATR
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
C --- VARIABLES
      CHARACTER*(*) NOMZ
      CHARACTER*19  NOM
      CHARACTER*1   BASE
      CHARACTER*8   NTM(*),MAIL,TYPE
      INTEGER       LIMA(*),NLIMA,INT1(4),INT2(4),NOEPAN(30)
      INTEGER       NMA,IMA,NPAN,NINT,NN1,NN2,IINT,INO,I,J,K
      INTEGER       A0,A1,A2,B0,B1,B2,B3,B4,P0,P1,P2,P3,P4

C --- LECTURE DONNEES

      CALL JEMARQ()

      NOM = NOMZ
      NINT = 6*NLIMA

      CALL JEVEUO(MAIL//'.DIME','L',A0)
      NN1 = ZI(A0)
   
      CALL JEVEUO(MAIL//'.TYPMAIL','L',A0)
      CALL JEVEUO(MAIL//'.CONNEX','L',A1)
      CALL JEVEUO(JEXATR(MAIL//'.CONNEX','LONCUM'),'L',A2)

      CALL WKVECT('&&LBORD.LISTE','V V I',NN1,B0)
      CALL WKVECT('&&LBORD.NNOEUD','V V I',NINT,B1)
      CALL WKVECT('&&LBORD.CONNEX','V V I',4*NINT,B2)
      CALL WKVECT('&&LBORD.RESERVE','V V I',2*NINT,B3)
      CALL WKVECT('&&LBORD.MAILLE','V V L',NLIMA,B4)

C --- INITIALISATION

      DO 10 I = 1, NN1
        ZI(B0-1+I) = 0
 10   CONTINUE

      NINT = 0

C --- BOUCLE SUR LES MAILLES DE LA LISTE

      DO 20 I = 1, NLIMA

        IMA = LIMA(I)
        P0 = A1-1+ZI(A2-1+IMA)
        TYPE = NTM(ZI(A0-1+IMA))

C ----- MAILLES REDUITES

        IF (TYPE(1:3).EQ.'SEG') THEN
          CALL NOPAN('SEG2    ',NOEPAN,NPAN)
        ELSEIF (TYPE(1:4).EQ.'TRIA') THEN
          CALL NOPAN('TRIA3   ',NOEPAN,NPAN)
        ELSEIF (TYPE(1:4).EQ.'QUAD') THEN
          CALL NOPAN('QUAD4   ',NOEPAN,NPAN)
        ELSEIF (TYPE(1:5).EQ.'TETRA') THEN
          CALL NOPAN('TETRA4  ',NOEPAN,NPAN)
        ELSEIF (TYPE(1:5).EQ.'PENTA') THEN
          CALL NOPAN('PENTA6  ',NOEPAN,NPAN)
        ELSEIF (TYPE(1:4).EQ.'HEXA') THEN
          CALL NOPAN('HEXA8   ',NOEPAN,NPAN)
        ELSE
          CALL UTMESS('F','LBORD',TYPE//' INDISPONIBLE')
        ENDIF

C ----- BOUCLE SUR LES PANS DE MA

        P1 = 0
        DO 20 J = 1, NPAN

          P1 = P1 + 1
          NN1 = ABS(NOEPAN(P1))
          
C ------- CONSTRUCTION INTERFACE TRIEE

          INO = 0
          DO 30 K = 1, NN1
            P1 = P1 + 1
            INT1(K) = ZI(P0-1+NOEPAN(P1))
            IF (INT1(K).GT.INO) THEN 
              INO = INT1(K)
              P2 = K
            ENDIF
 30       CONTINUE

          DO 40 K = 1, NN1
            INT2(K) = INT1(P2)
            IF (P2.EQ.NN1) P2 = 0
            P2 = P2 + 1
 40       CONTINUE

C ------- EXISTE-T-ELLE DEJA ?

          P2 = B0-2+INO

 50       CONTINUE

          P3 = P2+1
          P2 = ZI(P3)

          IF (P2.NE.0) THEN

            IINT = ZI(P2)
            NN2 = ZI(B1-1+IINT)
            IF (NN1.NE.NN2) GOTO 50

            P4 = B2 + 4*(IINT-1) + NN1
            DO 60 K = 2, NN1
              P4 = P4 - 1
              IF (ZI(P4).NE.INT2(K)) GOTO 50
 60         CONTINUE

            ZI(B1-1+IINT) = 0
            ZI(P3) = ZI(P2+1)
            GOTO 20

          ENDIF

C ------- STOCKAGE INTERFACE

          ZI(B1+NINT) = NN1

          P4 = B2 + 4*NINT
          DO 70 K = 1, NN1
            ZI(P4) = INT2(K)
            P4 = P4 + 1
 70       CONTINUE
          
          NINT = NINT + 1

          P2 = B3
          P3 = B0-1+INO
          B3 = B3 + 2

          ZI(P2) = NINT
          ZI(P2+1) = ZI(P3)
          ZI(P3) = P2
        
 20   CONTINUE

C --- NOEUDS SUR LE BORD

      P0 = B2 - 4

      DO 80 I = 1, NINT

        P0 = P0 + 4
        NN1 = ZI(B1-1+I)

        DO 80 J = 1, NN1
          ZI(B0-1+ZI(P0+J)) = 1
 80   CONTINUE

C --- SELECTION DES MAILLES DE BORD

      NMA = 0
      DO 90 I = 1, NLIMA

        IMA = LIMA(I)
        P0 = ZI(A2-1+IMA)
        P1 = ZI(A2+IMA)-1

        DO 100 J = P0, P1
 
          IF (ZI(B0-1+ZI(A1-1+J)).NE.0) THEN
            ZL(B4-1+I) = .TRUE.
            NMA = NMA + 1
            GOTO 90
          ENDIF
          
 100    CONTINUE

        ZL(B4-1+I) = .FALSE.
        
 90   CONTINUE

C --- COPIE &&LBORD.MAILLE -> LISTE MAILLES DE BORD

      CALL WKVECT(NOM//'.BORD',BASE//' V I',NMA,P0)

      DO 110 I = 1, NLIMA
        IF (ZL(B4-1+I)) THEN
          ZI(P0) = I
          P0 = P0 + 1
        ENDIF
 110  CONTINUE
      
C --- DESALLOCATION
      
      CALL JEDETR('&&LBORD.LISTE')
      CALL JEDETR('&&LBORD.NNOEUD')
      CALL JEDETR('&&LBORD.CONNEX')
      CALL JEDETR('&&LBORD.RESERVE')
      CALL JEDETR('&&LBORD.MAILLE')

      CALL JEDEMA()

      END
