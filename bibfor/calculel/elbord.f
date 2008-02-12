      SUBROUTINE ELBORD(MAIL  ,CINE  ,LIMA  ,NLIMA ,NTM   ,
     &                  BASE  ,NOM)
C     
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 12/02/2008   AUTEUR ABBAS M.ABBAS 
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT NONE
      CHARACTER*19  NOM
      INTEGER       LIMA(*)    
      CHARACTER*1   BASE
      CHARACTER*8   NTM(*),MAIL,CINE
      INTEGER       NLIMA
C      
C ----------------------------------------------------------------------
C
C ROUTINE ARLEQUIN
C
C ELEMENTS DE BORD D'UN GROUPE DE MAILLE
C
C ----------------------------------------------------------------------
C      
C
C IN  NOM    : NOM DE L'OBJET MAILLES DE BORD
C IN  DIME   : DIMENSION DE L'ESPACE
C IN  MAIL   : NOM UTILISATEUR DU MAILLAGE
C IN  CINE   : CINEMATIQUES DU GROUPE DE MAILLE
C IN  LIMA   : LISTE DE MAILLES (INDEX GLOBAL)
C IN  NLIMA  : NOMBRE DE MAILLES (LONGUEUR NLIMA)
C IN  NTM    : VECTEUR NOMS TYPES DE MAILLE
C IN  BASE   : BASE DE CREATION DE NOM
C
C SD DE SORTIE:
C NOM.BORD : LISTE DES MAILLES DE BORD DE LIMA
C            (MA1, MA2, ...) MA* INDEX DANS LIMA
C NOM.IPAN : INTERFACES PAVANT LE BORD DE LIMA
C            (MA1, IPAN1, MA2, IPAN2, ..., 0)
C            MA* INDEX DANS LIMA (PAR ORDRE CROISSANT)
C            IPAN* INDEX DANS NOPAN (CF. NOPAN)
C                  DONNE POUR LA MAILLE REORIENTEE (CF ORIEM3)
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
C
      CHARACTER*32       JEXATR
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
C      
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      CHARACTER*8   TYPEMA,TYPEML
      INTEGER       INT1(4),INT2(4),NOEPAN(30),CNXMA(27),NBPAN
      INTEGER       NMA,NUMA,NPAN,NINT,NN1,NN2,IINT,INO,I,J,K
      INTEGER       JNDIM,JTYPM,JCONX,JCUMU,JCOOR
      INTEGER       JBLIST,JBIPAN,JBNNO,JBCONX,JBRESV,JBMAIL
      INTEGER       P0,P1,P2,P3
C
      INTEGER PANCOQ(9)
      DATA PANCOQ / 4,2, 3,4,5, 1,4,6,2 /
C      
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- INITIALISATIONS
C      
      NINT = 6*NLIMA
C
C --- LECTURE DONNEES MAILLAGE
C
      CALL JEVEUO(MAIL(1:8)//'.DIME','L',JNDIM)
      NN1 = ZI(JNDIM)
      CALL JEVEUO(MAIL(1:8)//'.TYPMAIL','L',JTYPM)
      CALL JEVEUO(MAIL(1:8)//'.CONNEX','L',JCONX)
      CALL JEVEUO(JEXATR(MAIL(1:8)//'.CONNEX','LONCUM'),'L',JCUMU)
      CALL JEVEUO(MAIL(1:8)//'.COORDO    .VALE','L',JCOOR)
C
C --- ALLOCATIONS OBJETS TEMPORAIRES
C
      CALL WKVECT('&&ELBORD.LISTE','V V I',NN1,JBLIST)
      CALL WKVECT('&&ELBORD.IPAN','V V I',2*NINT,JBIPAN)
      CALL WKVECT('&&ELBORD.NNOEUD','V V I',NINT,JBNNO)
      CALL WKVECT('&&ELBORD.CONNEX','V V I',4*NINT,JBCONX)
      CALL WKVECT('&&ELBORD.RESERVE','V V I',2*NINT,JBRESV)
      CALL WKVECT('&&ELBORD.MAILLE','V V L',NLIMA,JBMAIL)
      NINT = 0
C
C --- BOUCLE SUR LES MAILLES DE LA LISTE
C
      DO 20 I = 1, NLIMA

        NUMA   = LIMA(I)
        TYPEMA = NTM(ZI(JTYPM-1+NUMA))
C
C --- REORIENTATION DES MAILLES SOLIDES
C
        IF (CINE.EQ.'SOLIDE') THEN
          CALL ORIEM3(NUMA  ,TYPEMA,ZR(JCOOR)  ,ZI(JCONX)  ,ZI(JCUMU)  ,
     &                CNXMA)
        ELSE
          P0  = ZI(JCUMU-1+NUMA)
          NN1 = ZI(JCUMU+NUMA) - P0
          DO 30 J = 1, NN1
            CNXMA(J) = ZI(JCONX-1+P0)
            P0 = P0 + 1
 30       CONTINUE
        ENDIF
C
C --- MAILLES REDUITES (LINEARISEES)
C
        IF (TYPEMA(1:3).EQ.'SEG') THEN
          TYPEML = 'SEG2    '
        ELSEIF (TYPEMA(1:4).EQ.'TRIA') THEN
          TYPEML = 'TRIA3   '
        ELSEIF (TYPEMA(1:4).EQ.'QUAD') THEN
          TYPEML = 'QUAD4   '
        ELSEIF (TYPEMA(1:5).EQ.'TETRA') THEN
          TYPEML = 'TETRA4  '
        ELSEIF (TYPEMA(1:5).EQ.'PENTA') THEN
          TYPEML = 'PENTA6  '
        ELSEIF (TYPEMA(1:4).EQ.'HEXA') THEN
          TYPEML = 'HEXA8   '
        ELSE
          WRITE(6,*) 'TYPEMA: ',TYPEMA
          CALL ASSERT(.FALSE.)
        ENDIF
C
C --- INFOS SUR MAILLES
C        
        CALL NOPAN(TYPEML,NOEPAN)
        NPAN = NBPAN(TYPEML)
C        
C --- BOUCLE SUR LES PANS DE MA
C
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
C
C --- EXISTE-T-ELLE DEJA DANS LA LISTE ?
C
          P1 = JBLIST-2+INO

 60       CONTINUE

          P2 = P1+1
          P1 = ZI(P2)

          IF (P1.NE.0) THEN
            IINT = ZI(P1)
            NN2 = ZI(JBNNO-1+IINT)
            IF (NN1.NE.NN2) GOTO 60

            P3 = JBCONX + 4*(IINT-1) + NN1
            DO 70 K = 2, NN1
              P3 = P3 - 1
              IF (ZI(P3).NE.INT2(K)) GOTO 60
 70         CONTINUE
            ZI(JBNNO-1+IINT) = 0
            ZI(P2) = ZI(P1+1)
            GOTO 20
          ENDIF
C
C --- STOCKAGE INTERFACE
C
          ZI(JBIPAN+2*NINT) = I
          ZI(JBIPAN+2*NINT+1) = J

          ZI(JBNNO+NINT) = NN1

          P2 = JBCONX + 4*NINT
          DO 80 K = 1, NN1
            ZI(P2) = INT2(K)
            P2 = P2 + 1
 80       CONTINUE

          NINT = NINT + 1
          P1 = JBLIST-1+INO

          ZI(JBRESV) = NINT
          ZI(JBRESV+1) = ZI(P1)
          ZI(P1) = JBRESV
          JBRESV = JBRESV + 2

 20   CONTINUE
C
C --- CAS DES COQUES
C
      IF (CINE.EQ.'COQUE   ') THEN

C ----- SELECTION DES INTERFACES

        NPAN = 2*NLIMA

        DO 90 I = 1, NINT
          IF (ZI(JBNNO-1+I).NE.0) NPAN = NPAN + 1
 90     CONTINUE

C ----- STRUCTURE .BORD

        CALL WKVECT(NOM(1:19)//'.BORD',BASE//' V I',NLIMA,P0)

        DO 100 I = 1, NLIMA
          ZI(P0-1+I) = I
 100    CONTINUE

C ----- STRUCTURE .IPAN

        CALL WKVECT(NOM(1:19)//'.IPAN',BASE//' V I',2*NPAN+1,P0)

        J = ZI(JBIPAN)

        DO 110 I = 1, NLIMA

          ZI(P0) = I
          ZI(P0+2) = I

          TYPEMA = NTM(ZI(JTYPM-1+LIMA(I)))
          IF (TYPEMA(1:3).EQ.'SEG') THEN
            ZI(P0+1) = 1
            ZI(P0+3) = 3
            P1 = 0
          ELSEIF (TYPEMA(1:4).EQ.'TRIA') THEN
            ZI(P0+1) = 1
            ZI(P0+3) = 2
            P1 = 2
          ELSEIF (TYPEMA(1:4).EQ.'QUAD') THEN
            ZI(P0+1) = 3
            ZI(P0+3) = 5
            P1 = 5
          ENDIF

          P0 = P0 + 4

 120      CONTINUE
          IF (I.NE.J) GOTO 110

          IF (ZI(JBNNO).NE.0) THEN
            ZI(P0) = J
            ZI(P0+1) = PANCOQ(P1+ZI(JBIPAN+1))
            P0 = P0 + 2
          ENDIF

          JBIPAN = JBIPAN + 2
          JBNNO = JBNNO + 1
          J = ZI(JBIPAN)
          GOTO 120

 110    CONTINUE

        ZI(P0) = 0

C --- CAS DES SOLIDES

      ELSE

C ----- NOEUDS SUR LE BORD

        NPAN = 0
        P0 = JBCONX - 5

        DO 130 I = 1, NINT

          P0 = P0 + 4
          NN1 = ZI(JBNNO-1+I)

          IF (NN1.NE.0) NPAN = NPAN + 1

          DO 130 J = 1, NN1
            ZI(JBLIST-1+ZI(P0+J)) = -1
 130    CONTINUE

C ----- SELECTION DES MAILLES DE BORD

        NMA = 0
        DO 140 I = 1, NLIMA

          NUMA = LIMA(I)
          P0 = ZI(JCUMU-1+NUMA)
          P1 = ZI(JCUMU+NUMA)-1

          DO 150 J = P0, P1

            IF (ZI(JBLIST-1+ZI(JCONX-1+J)).LT.0) THEN
              ZL(JBMAIL-1+I) = .TRUE.
              NMA = NMA + 1
              GOTO 140
            ENDIF

 150      CONTINUE

          ZL(JBMAIL-1+I) = .FALSE.

 140    CONTINUE
C
C --- STRUCTURE .BORD
C
        CALL WKVECT(NOM(1:19)//'.BORD',BASE//' V I',NMA,P0)

        DO 160 I = 1, NLIMA
          IF (.NOT.ZL(JBMAIL-1+I)) GOTO 160
          ZI(P0) = I
          P0 = P0 + 1
 160    CONTINUE
C
C --- STRUCTURE .IPAN
C
        CALL WKVECT(NOM(1:19)//'.IPAN',BASE//' V I',2*NPAN+1,P0)

        JBIPAN = JBIPAN - 2
        DO 170 I = 1, NINT
          JBIPAN = JBIPAN + 2
          IF (ZI(JBNNO-1+I).EQ.0) GOTO 170
          ZI(P0) = ZI(JBIPAN)
          ZI(P0+1) = ZI(JBIPAN+1)
          P0 = P0 + 2
 170    CONTINUE

        ZI(P0) = 0

      ENDIF
C
C --- DESALLOCATIONS
C
      CALL JEDETR('&&ELBORD.LISTE'  )
      CALL JEDETR('&&ELBORD.IPAN'   )
      CALL JEDETR('&&ELBORD.NNOEUD' )
      CALL JEDETR('&&ELBORD.CONNEX' )
      CALL JEDETR('&&ELBORD.RESERVE')
      CALL JEDETR('&&ELBORD.MAILLE' )
C
      CALL JEDEMA()

      END
