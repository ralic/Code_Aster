      SUBROUTINE ARLPND(DIM,MAIL,NOM1,CINE1,NOM2,CINE2,R1,
     &                  NORM,BC,NTM,APP,PND)
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
C TOLE CRP_20
C ----------------------------------------------------------------------
C              METHODE ARLEQUIN : PONDERATION DES MAILLES
C ----------------------------------------------------------------------
C VARIABLES D'ENTREE
C INTEGER       DIM             : DIMENSION DE L'ESPACE
C CHARACTER*8   MAIL            : NOM DU MAILLAGE  
C CHARACTER*10  NOM1            : NOM SD DOMAINE 1
C CHARACTER*8   CINE1           : CINEMATIQUE DOMAINE 1 (CF ARLVER)
C CHARACTER*10  NOM2            : NOM SD DOMAINE 2
C CHARACTER*8   CINE2           : CINEMATIQUE DOMAINE 2 (CF ARLVER)
C REAL*8        R1              : PONDERATION DOMAINE 1 DANS ZONE DE 
C                                 SUPERPOSITION
C CHARACTER*10  NORM            : NORMALES LISSEES COQUE (CF LISNOR)
C REAL*8        BC(2,*)         : BOITE ENGLOBANT ZONE DE SUPERPOSITION
C CHARACTER*8   NTM(*)          : VECTEUR NOMS TYPES DE MAILLE
C LOGICAL       APP(*)          : .TRUE. SI MAILLE REELLEMENT APPARIEE
C
C VARIABLES D'ENTREE/SORTIE
C REAL*8        PND(*)          : VECTEUR DE PONDERATION DES MAILLES
C
C SD D'ENTREE
C NOM1.NOM2     : SD GRAPHE D'APPARIEMENT (CF ARLAPP)
C NOM1.GROUPEMA : LISTE DES MAILLES DOMAINE 1
C NOM1.BOITE    : SD BOITES ENGLOBANTES (CF BOITE)
C NOM2.GROUPEMA : LISTE DES MAILLES DOMAINE 2
C NOM2.BOITE    : SD BOITES ENGLOBANTES (CF BOITE)
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

C --- FONCTION
      REAL*8       INTMAD,PROVE2

C --- VARIABLES
      CHARACTER*10 NOM1,NOM2,NORM
      CHARACTER*8  NTM(*),MAIL,CINE1,CINE2,TM
      INTEGER      DIM,NM1,NM2,NMS1,NMS2,NINT,NNT,NPA,NNH,NNO,NNI,NH
      INTEGER      NF,NS,NA,M1,IM1,M2,IM2,P0,P1,P2,Q0,Q1,Q2,I,J,K,N
      INTEGER      A0,A1,A2,A3,A4,B0,B1,B2,B3,B4,B5,B6,B7,E0,E1,E2
      INTEGER      C0,C1,C2,C3,C4,C5,C6,C7,D0,D1,D2,D3,D4,D5
      REAL*8       BC(2,*),PND(*),R,R1,R2,NO(81)
      LOGICAL      APP(*)

      R2 = 1.D0 - R1

C --- LECTURE DONNEES

      CALL JEMARQ()

      CALL JEVEUO(MAIL//'.COORDO    .VALE','L', A0)
      CALL JEVEUO(MAIL//'.CONNEX','L',A1)
      CALL JEVEUO(JEXATR(MAIL//'.CONNEX','LONCUM'),'L',A2)
      CALL JEVEUO(MAIL//'.TYPMAIL','L',A3)
      CALL JEVEUO(MAIL//'.DIME','L',P0)
      NNT = ZI(P0)

      CALL JEEXIN(NORM,I)
      IF (I.NE.0) THEN
        CALL JEVEUO(NORM,'L',A4)
      ELSE
        A4 = A0
      ENDIF

      CALL JEVEUO(NOM1//'.GROUPEMA','L',B0)
      CALL JELIRA(NOM1//'.GROUPEMA','LONMAX',NM1,ZK8)
      CALL JEVEUO(NOM1//'.'//NOM2,'L',B1)
      CALL JELIRA(NOM1//'.'//NOM2,'LONT',N,ZK8)
      CALL JEVEUO(JEXATR(NOM1//'.'//NOM2,'LONCUM'),'L',B2)
      CALL JEVEUO(NOM1//'.BOITE.DIME','L',B3)
      CALL JEVEUO(NOM1//'.BOITE.PAN','L',B4)

      CALL JEVEUO(NOM2//'.GROUPEMA','L',C0)
      CALL JELIRA(NOM2//'.GROUPEMA','LONMAX',NM2,ZK8)
      CALL WKVECT('&&ARLPND.MAMINV','V V I',N,C1)
      CALL WKVECT('&&ARLPND.MAMINV.LONCUM','V V I',NM2+1,C2)
      CALL JEVEUO(NOM2//'.BOITE.DIME','L',C3)
      CALL JEVEUO(NOM2//'.BOITE.PAN','L',C4)

      CALL JEVEUO('&&ARL.NH','L',P0)
      NH = ZI(P0+1)

C --- VECTEURS DE TRAVAIL

      CALL WKVECT('&&ARLPND.TMP1','V V I',NM1+1,Q1)
      CALL WKVECT('&&ARLPND.TMP2','V V I',NM2+1,Q2)
      CALL WKVECT('&&ARLPND.DICO','V V I',NNT,D0)

C --- 1.1 POINTEURS DU GRAPHE D'APPARIEMENT INVERSE

      DO 10 M2 = 1, NM2
        ZI(C2+M2) = 0
 10   CONTINUE

      P1 = ZI(B2)

      DO 20 M1 = 1, NM1

        P0 = P1
        P1 = ZI(B2+M1)

        DO 20 I = P0, P1-1

          M2 = ZI(B1-1+I)
          IF (M2.NE.0) ZI(C2+M2) = ZI(C2+M2) + 1

 20   CONTINUE

      P2 = 1
      ZI(C2) = 1
      ZI(Q2) = 1

      DO 30 M2 = 1, NM2

        P2 = P2 + ZI(C2+M2)
        ZI(C2+M2) = P2
        ZI(Q2+M2) = P2

 30   CONTINUE

C --- 1.2. SOMMETS DU GRAPHE DU GRAPHE D'APPARIEMENT INVERSE

      P1 = ZI(B2)

      DO 40 M1 = 1, NM1

        P0 = P1
        P1 = ZI(B2+M1)

        DO 40 I = P0, P1-1

          M2 = ZI(B1-1+I)
          IF (M2.EQ.0) GOTO 40

          P2 = ZI(Q2-1+M2)
          ZI(C1-1+P2) = M1
          ZI(Q2-1+M2) = P2 + 1

 40   CONTINUE

C --- 2.1. RECONSTITUTION DE LA ZONE DE SUPERPOSITION

      CALL ARLSUP(DIM,NOM1,BC,APP)
      CALL ARLSUP(DIM,NOM2,BC,APP)

C --- 2.2. GROUPES DE MAILLES TOUCHANT LA ZONE DE SUPERPOSITION

      NMS1 = 0
      NMS2 = 0

      DO 60 I = 1, NM1
        IF (APP(ZI(B0-1+I))) NMS1 = NMS1 + 1 
 60   CONTINUE
      
      DO 70 I = 1, NM2
        IF (APP(ZI(C0-1+I))) NMS2 = NMS2 + 1
 70   CONTINUE

      CALL WKVECT('&&ARLPND.RECOUVRE.1','V V I',NMS1,B5)
      CALL WKVECT('&&ARLPND.RECOUVRE.2','V V I',NMS2,C5)

      P0 = Q1
      Q0 = B5
      DO 80 I = 1, NM1
        IM1 = ZI(B0-1+I)
        IF (.NOT.APP(IM1)) GOTO 80
        PND(IM1) = R1
        ZI(P0) = IM1
        ZI(Q0) = I
        P0 = P0 + 1
        Q0 = Q0 + 1
 80   CONTINUE

      P0 = Q2
      Q0 = C5
      DO 90 I = 1, NM2
        IM2 = ZI(C0-1+I)
        IF (.NOT.APP(IM2)) GOTO 90
        PND(IM2) = R2
        ZI(P0) = IM2
        ZI(Q0) = I
        P0 = P0 + 1
        Q0 = Q0 + 1
 90   CONTINUE

C --- 2.3. ELEMENTS BORDANT LA ZONE DE SUPERPOSITION

      CALL ELBORD(MAIL,CINE1,ZI(Q1),NMS1,NTM,'V','&&ARLPND.RECOUVRE.1')
      CALL ELBORD(MAIL,CINE2,ZI(Q2),NMS2,NTM,'V','&&ARLPND.RECOUVRE.2')
 
      CALL JEVEUO('&&ARLPND.RECOUVRE.1.BORD','L',B6)
      CALL JELIRA('&&ARLPND.RECOUVRE.1.BORD','LONMAX',NMS1,ZK8)
      CALL JEVEUO('&&ARLPND.RECOUVRE.1.IPAN','L',B7)

      CALL JEVEUO('&&ARLPND.RECOUVRE.2.BORD','L',C6)
      CALL JELIRA('&&ARLPND.RECOUVRE.2.BORD','LONMAX',NMS2,ZK8)
      CALL JEVEUO('&&ARLPND.RECOUVRE.2.IPAN','L',C7)
      
C --- 2.4.1. POINTEURS DANS STRUCTURE .1.IPAN

      Q0 = 1
      P0 = B7
      ZI(Q1) = 1
      M1 = ZI(B5-1+ZI(P0)) 
      ZI(P0) = M1

      DO 100 I = 1, NM1

 110    CONTINUE
        IF (M1.EQ.I) THEN
          Q0 = Q0 + 1
          P0 = P0 + 2
          M1 = ZI(P0)
          IF (M1.EQ.0) GOTO 110
          M1 = ZI(B5-1+M1)
          ZI(P0) = M1
          GOTO 110
        ENDIF

        ZI(Q1+I) = Q0

 100  CONTINUE

C --- 2.4.2. POINTEURS DANS STRUCTURE .2.IPAN

      Q0 = 1
      P0 = C7
      ZI(Q2) = 1
      M2 = ZI(C5-1+ZI(P0))
      ZI(P0) = M2

      DO 120 I = 1, NM2

 130    CONTINUE
        IF (M2.EQ.I) THEN
          Q0 = Q0 + 1
          P0 = P0 + 2
          M2 = ZI(P0)
          IF (M2.EQ.0) GOTO 130
          M2 = ZI(C5-1+M2)
          ZI(P0) = M2
          GOTO 130
        ENDIF

        ZI(Q2+I) = Q0

 120  CONTINUE

C --- 3.1. DIMENSIONNEMENT POUR INTERSECTION MAILLE / DOMAINE

      NINT = 0

C --- 3.1.1. DIMENSIONNEMENT POUR MAILLE 1 / DOMAINE 2
  
      DO 140 I = 1, NMS1
        
        N = 0
        M1 = ZI(B5-1+ZI(B6-1+I))

        P0 = ZI(B2-1+M1)
        P1 = ZI(B2+M1)-1

        DO 150 J = P0, P1
          M2 = ZI(B1-1+J)
          IF (M2.NE.0) N = N + ZI(Q2+M2) - ZI(Q2-1+M2)
 150    CONTINUE

        IF (N.GT.NINT) NINT = N
 
 140  CONTINUE

C --- 3.1.2 DIMENSIONNEMENT POUR MAILLE 2 / DOMAINE 1

      DO 160 I = 1, NMS2
        
        N = 0
        M2 = ZI(C5-1+ZI(C6-1+I))

        P0 = ZI(C2-1+M2)
        P1 = ZI(C2+M2)-1

        DO 170 J = P0, P1
          M1 = ZI(C1-1+J)
          N = N + ZI(Q1+M1) - ZI(Q1-1+M1) 
 170    CONTINUE

        IF (N.GT.NINT) NINT = N

 160  CONTINUE

C --- 3.2 ALLOCATIONS

      NINT = 4*NINT + 4
      N = NH*NH
      I = NINT + 6
      J = NH + 1

      CALL WKVECT('&&ARLPND.INT','V V I',2*NINT,D1)
      CALL WKVECT('&&ARLPND.FS','V V I',10*NINT,D2)
      CALL WKVECT('&&ARLPND.NO','V V R',18*NINT+9,D3)
      CALL WKVECT('&&ARLPND.AS','V V I',28*NINT+12,D4)
      CALL WKVECT('&&ARLPND.FA','V V I',5*NINT,D5)

      CALL WKVECT('&&ARLPND.ZR','V V R',35*NINT*N+66*N+18*J,E0)
      CALL WKVECT('&&ARLPND.ZI','V V I',I*(50*N+22*J)+162*NINT*N,E1)
      CALL WKVECT('&&ARLPND.ZL','V V L',2*I*N,E2)

C --- 4.1. INTERSECTION MAILLES 1 / DOMAINE 2

      DO 180 I = 1, NMS1

        M1 = ZI(B5-1+ZI(B6-1+I))        
        IM1 = ZI(B0-1+M1)
        TM = NTM(ZI(A3-1+IM1))
        CALL TMACOQ(TM,DIM,J)
        CALL CONOEU(IM1,ZI(A1),ZI(A2),ZR(A0),ZR(A4),DIM,J,NO,NNO)

C ----- 4.1.1. LISTE DES FACES BORDANT DOMAINE 2

        NF = 0
        Q0 = D1
        P0 = ZI(B2-1+M1)
        P1 = ZI(B2+M1)-1

        DO 190 J = P0, P1

          M2 = ZI(B1-1+J)
          IF (M2.EQ.0) GOTO 180

          P2 = ZI(Q2-1+M2)
          N = ZI(Q2+M2) - P2
          NF = NF + N

          P2 = C7 + 2*(P2-1)

          DO 190 K = 1, N
            
            ZI(Q0) = ZI(P2)
            ZI(Q0+1) = ZI(P2+1)
            Q0 = Q0 + 2
            P2 = P2 + 2

 190    CONTINUE

        IF (NF.EQ.0) GOTO 180

C ----- 4.1.2. NOEUDS, ARETES ET FACES DE LA FRONTIERE DU DOMAINE 2

        DO 200 J = 1, NNT
          ZI(D0-1+J) = 0
 200    CONTINUE

        IF (DIM.EQ.2) THEN

          CALL NAFINT(ZI(D1),NF,ZI(C0),ZI(C3),ZR(C4),DIM,CINE2,ZI(A3),
     &         ZR(A0),ZR(A4),ZI(A1),ZI(A2),NTM,ZI(D0),ZR(D3),NNI,ZI(D4))
          NA = NF
          
        ELSE

          CALL NAFINT(ZI(D1),NF,ZI(C0),ZI(C3),ZR(C4),DIM,CINE2,ZI(A3),
     &         ZR(A0),ZR(A4),ZI(A1),ZI(A2),NTM,ZI(D0),ZR(D3),NNI,ZI(D2))
          CALL ARFACE(ZI(D2),NF,ZI(D4),ZI(D5),NA)

        ENDIF

C ----- 4.1.3 RATIO VOLUME DANS ZONE DE RECOUVREMENT / VOLUME MAILLE

C --- (PROVISOIRE) CORRECTION A FAIRE DANS ALGORITHME 3D D'INTERSECTION
        IF (DIM.EQ.2) THEN
C --- (PROVISOIRE) CORRECTION A FAIRE DANS ALGORITHME 3D D'INTERSECTION

        P0 = B4+(DIM+2)*(ZI(B3+2*M1)-1)
        R = INTMAD(DIM,NH,TM,NO,NNO,ZR(P0),ZR(D3),NNI,ZI(D4),NA,
     &             ZI(D2),ZI(D5),NF,ZR(E0),ZI(E1),ZL(E2))
        PND(IM1) = 1.D0 - R2*R

C --- (PROVISOIRE) CORRECTION A FAIRE DANS ALGORITHME 3D D'INTERSECTION
        ENDIF
C --- (PROVISOIRE) CORRECTION A FAIRE DANS ALGORITHME 3D D'INTERSECTION

 180  CONTINUE

C --- 4.2. INTERSECTION MAILLES 2 / DOMAINE 1

      DO 210 I = 1, NMS2

        M2 = ZI(C5-1+ZI(C6-1+I))
        IM2 = ZI(C0-1+M2)
        TM = NTM(ZI(A3-1+IM2))
        CALL TMACOQ(TM,DIM,J)
        CALL CONOEU(IM2,ZI(A1),ZI(A2),ZR(A0),ZR(A4),DIM,J,NO,NNO)

C ----- 4.2.1. LISTE DES FACES BORDANT DOMAINE 1

        NF = 0
        Q0 = D1
        P0 = ZI(C2-1+M2)
        P1 = ZI(C2+M2)-1

        DO 220 J = P0, P1

          M1 = ZI(C1-1+J)
          P2 = ZI(Q1-1+M1)
          N = ZI(Q1+M1) - P2
          NF = NF + N

          P2 = B7 + 2*(P2-1)

          DO 220 K = 1, N
            
            ZI(Q0) = ZI(P2)
            ZI(Q0+1) = ZI(P2+1)
            Q0 = Q0 + 2
            P2 = P2 + 2

 220    CONTINUE

        IF (NF.EQ.0) GOTO 210

C ----- 4.2.2. NOEUDS, ARETES ET FACES DE LA FRONTIERE DU DOMAINE 1

        DO 230 J = 1, NNT
          ZI(D0-1+J) = 0
 230    CONTINUE

        IF (DIM.EQ.2) THEN

          CALL NAFINT(ZI(D1),NF,ZI(B0),ZI(B3),ZR(B4),DIM,CINE1,ZI(A3),
     &         ZR(A0),ZR(A4),ZI(A1),ZI(A2),NTM,ZI(D0),ZR(D3),NNI,ZI(D4))
          NA = NF
          
        ELSE

          CALL NAFINT(ZI(D1),NF,ZI(B0),ZI(B3),ZR(B4),DIM,CINE1,ZI(A3),
     &         ZR(A0),ZR(A4),ZI(A1),ZI(A2),NTM,ZI(D0),ZR(D3),NNI,ZI(D2))
          CALL ARFACE(ZI(D2),NF,ZI(D4),ZI(D5),NA)

        ENDIF

C ----- 4.2.3 RATIO VOLUME DANS ZONE DE RECOUVREMENT / VOLUME MAILLE

C --- (PROVISOIRE) CORRECTION A FAIRE DANS ALGORITHME 3D D'INTERSECTION
        IF (DIM.EQ.2) THEN 
C --- (PROVISOIRE) CORRECTION A FAIRE DANS ALGORITHME 3D D'INTERSECTION

        P0 = C4+(DIM+2)*(ZI(C3+2*M2)-1)
        R = INTMAD(DIM,NH,TM,NO,NNO,ZR(P0),ZR(D3),NNI,ZI(D4),NA,
     &             ZI(D2),ZI(D5),NF,ZR(E0),ZI(E1),ZL(E2))
        PND(IM2) = 1.D0 - R1*R

C --- (PROVISOIRE) CORRECTION A FAIRE DANS ALGORITHME 3D D'INTERSECTION
        ENDIF
C --- (PROVISOIRE) CORRECTION A FAIRE DANS ALGORITHME 3D D'INTERSECTION

 210  CONTINUE

C --- DESALLOCATIONS

 240  CONTINUE

      CALL JEDETR('&&ARLPND.MAMINV')
      CALL JEDETR('&&ARLPND.MAMINV.LONCUM')
      CALL JEDETR('&&ARLPND.TMP1')
      CALL JEDETR('&&ARLPND.TMP2')
      CALL JEDETR('&&ARLPND.DICO')

      CALL JEDETR('&&ARLPND.RECOUVRE.1')
      CALL JEDETR('&&ARLPND.RECOUVRE.1.BORD')
      CALL JEDETR('&&ARLPND.RECOUVRE.1.IPAN')

      CALL JEDETR('&&ARLPND.RECOUVRE.2')
      CALL JEDETR('&&ARLPND.RECOUVRE.2.BORD')
      CALL JEDETR('&&ARLPND.RECOUVRE.2.IPAN')

      CALL JEDETR('&&ARLPND.INT')
      CALL JEDETR('&&ARLPND.FS')
      CALL JEDETR('&&ARLPND.NO')
      CALL JEDETR('&&ARLPND.AS')
      CALL JEDETR('&&ARLPND.FA')

      CALL JEDETR('&&ARLPND.ZR')
      CALL JEDETR('&&ARLPND.ZI')
      CALL JEDETR('&&ARLPND.ZL')

      CALL JEDEMA()

      END
