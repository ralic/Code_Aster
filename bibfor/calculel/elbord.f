      SUBROUTINE ELBORD(MAIL,CINE,LIMA,NLIMA,NTM,BASE,NOMZ)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 08/11/2004   AUTEUR DURAND C.DURAND 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
C A_UTIL
C ----------------------------------------------------------------------
C                 ELEMENTS DE BORD D'UN GROUPE DE MAILLE
C ----------------------------------------------------------------------
C VARIABLES D'ENTREE
C CHARACTER*8    MAIL        : SD MAILLAGE  
C CHARACTER*8    CINE        : CINEMATIQUE MAILLES ('SOLIDE' OU 'COQUE')
C INTEGER        LIMA(NLIMA) : LISTE DE MAILLES (INDEX GLOBAL)
C INTEGER        NLIMA       : NOMBRE DE MAILLES
C CHARACTER*8    NTM(*)      : VECTEUR NOMS TYPES DE MAILLE
C CHARACTER*1    BASE        : BASE DE CREATION DE NOM
C CHARACTER*(19) NOMZ        : NOM DE L'OBJET MAILLES DE BORD
C
C SD DE SORTIE
C NOM.BORD : LISTE DES MAILLES DE BORD DE LIMA
C            (MA1, MA2, ...) MA* INDEX DANS LIMA
C NOM.IPAN : INTERFACES PAVANT LE BORD DE LIMA
C            (MA1, IPAN1, MA2, IPAN2, ..., 0) 
C            MA* INDEX DANS LIMA (PAR ORDRE CROISSANT)
C            IPAN* INDEX DANS NOPAN (CF. NOPAN)
C                  DONNE POUR LA MAILLE REORIENTEE (CF ORIEM3)
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

C --- VARIABLES
      CHARACTER*(*) NOMZ
      CHARACTER*19  NOM
      CHARACTER*1   BASE
      CHARACTER*8   NTM(*),MAIL,TYPE,CINE
      INTEGER       LIMA(*),INT1(4),INT2(4),NOEPAN(30),CNXMA(27)
      INTEGER       NLIMA,NMA,IMA,NPAN,NINT,NN1,NN2,IINT,INO,I,J,K
      INTEGER       A0,A1,A2,A3,B0,B1,B2,B3,B4,B5,P0,P1,P2,P3,P4
 
C --- PARAMETRES
      INTEGER PANCOQ(9)
      DATA PANCOQ / 4,2, 3,4,5, 1,4,6,2 /

      NOM = NOMZ
      
C --- LECTURE DONNEES

      CALL JEMARQ()

      NINT = 6*NLIMA

      CALL JEVEUO(MAIL//'.DIME','L',A0)
      NN1 = ZI(A0)

      CALL JEVEUO(MAIL//'.TYPMAIL','L',A0)
      CALL JEVEUO(MAIL//'.CONNEX','L',A1)
      CALL JEVEUO(JEXATR(MAIL//'.CONNEX','LONCUM'),'L',A2)
      CALL JEVEUO(MAIL//'.COORDO    .VALE','L',A3)

      CALL WKVECT('&&ELBORD.LISTE','V V I',NN1,B0)
      CALL WKVECT('&&ELBORD.IPAN','V V I',2*NINT,B1)
      CALL WKVECT('&&ELBORD.NNOEUD','V V I',NINT,B2)
      CALL WKVECT('&&ELBORD.CONNEX','V V I',4*NINT,B3)
      CALL WKVECT('&&ELBORD.RESERVE','V V I',2*NINT,B4)
      CALL WKVECT('&&ELBORD.MAILLE','V V L',NLIMA,B5)

C --- INITIALISATION

      DO 10 I = 1, NN1
        ZI(B0-1+I) = 0
 10   CONTINUE

      NINT = 0

C --- BOUCLE SUR LES MAILLES DE LA LISTE

      DO 20 I = 1, NLIMA

        IMA = LIMA(I)
        TYPE = NTM(ZI(A0-1+IMA))

C ----- REORIENTATION DES MAILLES SOLIDES

        IF (CINE.EQ.'SOLIDE') THEN 

          CALL ORIEM3(IMA,TYPE,ZR(A3),ZI(A1),ZI(A2),CNXMA)

        ELSE

          P0 = ZI(A2-1+IMA)
          NN1 = ZI(A2+IMA) - P0
          DO 30 J = 1, NN1
            CNXMA(J) = ZI(A1-1+P0)
            P0 = P0 + 1
 30       CONTINUE

        ENDIF
        
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
          CALL UTMESS('F','ELBORD',TYPE//' INDISPONIBLE')
        ENDIF

C ----- BOUCLE SUR LES PANS DE MA

        P0 = 0
        DO 20 J = 1, NPAN

          P0 = P0 + 1
          NN1 = ABS(NOEPAN(P0))
          
C ------- CONSTRUCTION INTERFACE TRIEE

          INO = 0
          DO 40 K = 1, NN1
            P0 = P0 + 1
            INT1(K) = CNXMA(NOEPAN(P0))
            IF (INT1(K).GT.INO) THEN 
              INO = INT1(K)
              P1 = K
            ENDIF
 40       CONTINUE

          DO 50 K = 1, NN1
            INT2(K) = INT1(P1)
            IF (P1.EQ.NN1) P1 = 0
            P1 = P1 + 1
 50       CONTINUE

C ------- EXISTE-T-ELLE DEJA DANS LA LISTE ?

          P1 = B0-2+INO

 60       CONTINUE

          P2 = P1+1
          P1 = ZI(P2)

          IF (P1.NE.0) THEN

            IINT = ZI(P1)
            NN2 = ZI(B2-1+IINT)
            IF (NN1.NE.NN2) GOTO 60

            P3 = B3 + 4*(IINT-1) + NN1
            DO 70 K = 2, NN1
              P3 = P3 - 1
              IF (ZI(P3).NE.INT2(K)) GOTO 60
 70         CONTINUE

            ZI(B2-1+IINT) = 0
            ZI(P2) = ZI(P1+1)
            GOTO 20

          ENDIF

C ------- STOCKAGE INTERFACE

          ZI(B1+2*NINT) = I
          ZI(B1+2*NINT+1) = J

          ZI(B2+NINT) = NN1

          P2 = B3 + 4*NINT
          DO 80 K = 1, NN1
            ZI(P2) = INT2(K)
            P2 = P2 + 1
 80       CONTINUE
          
          NINT = NINT + 1
          P1 = B0-1+INO

          ZI(B4) = NINT
          ZI(B4+1) = ZI(P1)
          ZI(P1) = B4
          B4 = B4 + 2

 20   CONTINUE

C --- CAS DES COQUES

      IF (CINE.EQ.'COQUE   ') THEN 

C ----- SELECTION DES INTERFACES

        NPAN = 2*NLIMA

        DO 90 I = 1, NINT
          IF (ZI(B2-1+I).NE.0) NPAN = NPAN + 1
 90     CONTINUE

C ----- STRUCTURE .BORD

        CALL WKVECT(NOM//'.BORD',BASE//' V I',NLIMA,P0)

        DO 100 I = 1, NLIMA
          ZI(P0-1+I) = I
 100    CONTINUE

C ----- STRUCTURE .IPAN

        CALL WKVECT(NOM//'.IPAN',BASE//' V I',2*NPAN+1,P0)
      
        J = ZI(B1)

        DO 110 I = 1, NLIMA

          ZI(P0) = I
          ZI(P0+2) = I

          TYPE = NTM(ZI(A0-1+LIMA(I)))
          IF (TYPE(1:3).EQ.'SEG') THEN
            ZI(P0+1) = 1
            ZI(P0+3) = 3
            P1 = 0
          ELSEIF (TYPE(1:4).EQ.'TRIA') THEN
            ZI(P0+1) = 1
            ZI(P0+3) = 2
            P1 = 2
          ELSEIF (TYPE(1:4).EQ.'QUAD') THEN
            ZI(P0+1) = 3
            ZI(P0+3) = 5
            P1 = 5
          ENDIF

          P0 = P0 + 4

 120      CONTINUE
          IF (I.NE.J) GOTO 110
            
          IF (ZI(B2).NE.0) THEN
            ZI(P0) = J
            ZI(P0+1) = PANCOQ(P1+ZI(B1+1))
            P0 = P0 + 2 
          ENDIF

          B1 = B1 + 2
          B2 = B2 + 1
          J = ZI(B1)
          GOTO 120

 110    CONTINUE

        ZI(P0) = 0

C --- CAS DES SOLIDES

      ELSE

C ----- NOEUDS SUR LE BORD

        NPAN = 0
        P0 = B3 - 5
      
        DO 130 I = 1, NINT

          P0 = P0 + 4
          NN1 = ZI(B2-1+I)

          IF (NN1.NE.0) NPAN = NPAN + 1
        
          DO 130 J = 1, NN1
            ZI(B0-1+ZI(P0+J)) = -1
 130    CONTINUE

C ----- SELECTION DES MAILLES DE BORD

        NMA = 0
        DO 140 I = 1, NLIMA

          IMA = LIMA(I)
          P0 = ZI(A2-1+IMA)
          P1 = ZI(A2+IMA)-1

          DO 150 J = P0, P1
 
            IF (ZI(B0-1+ZI(A1-1+J)).LT.0) THEN
              ZL(B5-1+I) = .TRUE.
              NMA = NMA + 1
              GOTO 140
            ENDIF
          
 150      CONTINUE

          ZL(B5-1+I) = .FALSE.
        
 140    CONTINUE

C ----- STRUCTURE .BORD

        CALL WKVECT(NOM//'.BORD',BASE//' V I',NMA,P0)

        DO 160 I = 1, NLIMA
          IF (.NOT.ZL(B5-1+I)) GOTO 160
          ZI(P0) = I
          P0 = P0 + 1
 160    CONTINUE

C ----- STRUCTURE .IPAN

        CALL WKVECT(NOM//'.IPAN',BASE//' V I',2*NPAN+1,P0)
      
        B1 = B1 - 2
        DO 170 I = 1, NINT
          B1 = B1 + 2
          IF (ZI(B2-1+I).EQ.0) GOTO 170
          ZI(P0) = ZI(B1)
          ZI(P0+1) = ZI(B1+1)
          P0 = P0 + 2
 170    CONTINUE
   
        ZI(P0) = 0

      ENDIF
       
C --- DESALLOCATIONS
      
      CALL JEDETR('&&ELBORD.LISTE')
      CALL JEDETR('&&ELBORD.IPAN')
      CALL JEDETR('&&ELBORD.NNOEUD')
      CALL JEDETR('&&ELBORD.CONNEX')
      CALL JEDETR('&&ELBORD.RESERVE')
      CALL JEDETR('&&ELBORD.MAILLE')

      CALL JEDEMA()

      END
