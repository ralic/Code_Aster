      SUBROUTINE CANORT(NOMA,NBMA,LISTI,LISTK,NDIM,NBNO,NBOPN,NUNO,L)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 27/09/2005   AUTEUR CIBHHPD L.SALMONA 
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
C
C     BUT: CALCULER LES NORMALES AUX NOEUDS D'UNE LISTE DE MAILLES
C                   ET LES TANGENTES
C ARGUMENTS D'ENTREE:
C      NOMA : NOM DU MAILLAGE
C      NBMA : NOMBRE DE MAILLES DU MAILLAGE DANS LA LISTE.
C              SI >0 LA LISTE EST NUMEROTEE ==> LISTI
C              SI <0 LA LISTE EST NOMMEE    ==> LISTK
C      NDIM : DIMENSION DU PROBLEME
C      NBNO : NOMBRE DE NOEUDS DANS LA LISTE DE MAILLES.
C             = NOMBRE DE MAILLES SUPPLEMENTAIRES.
C      NBOPN: NOMBRE D'OCCURENCES DE CHAQUE NOEUD DANS LA LISTE
C      NUNO : LISTE DES NUMEROS DE NOEUDS DE LA LISTE DE MAILLES
C      L    : =1 ==> CALCUL DE LA NORMALE (2D ET 3D)
C             =2 ==> CALCUL DE LA TANGENTE (2D )
C OBJETS JEVEUX CREES
C     &&CANORT.NORMALE : NORMALES MOYENNEES AUX NOEUDS (2 EN 2D,3 EN 3D)
C     &&CANORT.TANGENT : TANGENTES AUX NOEUDS (2 EN 2D)
C
C ROUTINES APPELEES:
C---------------- COMMUNS NORMALISES  JEVEUX  --------------------------
      COMMON/IVARJE/ZI(1)
      COMMON/RVARJE/ZR(1)
      COMMON/CVARJE/ZC(1)
      COMMON/LVARJE/ZL(1)
      COMMON/KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C-------------- FIN COMMUNS NORMALISES JEVEUX --------------------------
C
      INTEGER      ZI,LISTI(*),NBOPN(*),DIMCOO,NUNO(*)
      REAL*8       ZR,COOR(3,9),A,B,C,PVEC(3)
      COMPLEX*16   ZC
      LOGICAL      ZL
      CHARACTER*8  KANGL
      CHARACTER*8  ZK8,LISTK(*),NOMA,MK,NOMTYP,NOMNOE
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24,NOMOBJ, NOMOB2
      CHARACTER*32 ZK32,JEXNOM,JEXNUM
      CHARACTER*80 ZK80
      CHARACTER*1 K1BID
C
      CALL JEMARQ()
      IF (L.EQ.1) NOMOBJ = '&&CANORT.NORMALE'
      IF (L.EQ.2) NOMOBJ = '&&CANORT.TANGENT'
      CALL JEEXIN(NOMOBJ,IRET)
      IF (IRET.NE.0) CALL JEDETR(NOMOBJ)
      CALL JECREO(NOMOBJ,'V V R')
      CALL JEECRA(NOMOBJ,'LONMAX',NDIM*NBNO,' ')
      CALL JEVEUO(NOMOBJ,'E',JNORM)
C
      NOMOB2 = '&&CANORT.VECTEUR'
      CALL JEEXIN(NOMOB2,IRET)
      IF (IRET.NE.0) CALL JEDETR(NOMOB2)
      ISOM = 0
      DO 10 I = 1, NBNO
         ISOM = ISOM + NBOPN(I)
 10   CONTINUE
        
      CALL WKVECT(NOMOB2,'V V R',NDIM*ISOM,IDOBJ2)
C
      CALL JEVEUO(NOMA//'.COORDO    .VALE','L',JCOOR)
      IJ = 0
      DO 1 M=1,ABS(NBMA)
         IF (NBMA.GT.0) THEN
            MI=LISTI(M)
            CALL JEVEUO(JEXNUM(NOMA//'.CONNEX',MI),'L',JDES)
            CALL JELIRA(JEXNUM(NOMA//'.CONNEX',MI),'LONMAX',NN,K1BID)
            CALL JEVEUO(NOMA//'.TYPMAIL','L',IATYMA)
            JTYP=IATYMA-1+MI
         ELSE IF (NBMA.LT.0) THEN
            MK=LISTK(M)
            CALL JENONU(JEXNOM(NOMA//'.NOMMAI',MK),IBID)
            CALL JEVEUO(JEXNUM(NOMA//'.CONNEX',IBID),'L',JDES)
            CALL JELIRA(JEXNUM(NOMA//'.CONNEX',IBID),'LONMAX',NN,K1BID)
            CALL JENONU(JEXNOM(NOMA//'.NOMMAI',MK),IBID)
            CALL JEVEUO(NOMA//'.TYPMAIL','L',IATYMA)
            JTYP=IATYMA-1+IBID
         END IF
         ITYP=ZI(JTYP)
         CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ITYP),NOMTYP)
C
C       ATTENTION, NDIM PEUT ETRE DIFFERENT DU NOMBRE DE COMPOSANTES DE
C       LA GEOMETRIE ( CAS DU 2D (COOR2D) PLONGE DANS LE 3D)
C
         CALL JEVEUO (NOMA//'.COORDO    .DESC', 'L', JCOODE)
         DIMCOO = -ZI(JCOODE-1+2)
         IF (NDIM.EQ.2.AND.NOMTYP(1:3).EQ.'SEG') THEN
            INO1=ZI(JDES-1+1)
            INO2=ZI(JDES-1+2)
            COOR(1,1)=ZR(JCOOR-1+DIMCOO*(INO1-1)+1)
            COOR(2,1)=ZR(JCOOR-1+DIMCOO*(INO1-1)+2)
            COOR(1,2)=ZR(JCOOR-1+DIMCOO*(INO2-1)+1)
            COOR(2,2)=ZR(JCOOR-1+DIMCOO*(INO2-1)+2)
            IF (L.EQ.1) CALL CANOR2(COOR,A,B)
            IF (L.EQ.2) CALL CATAN2(COOR,A,B)
            DO 3 N=1,NBNO
               DO 4 IN=1,NN
                  INO=ZI(JDES-1+IN)
                  IF(INO.EQ.NUNO(N)) THEN
                     ZR(JNORM-1+2*(N-1)+1)=ZR(JNORM-1+2*(N-1)+1)+A
                     ZR(JNORM-1+2*(N-1)+2)=ZR(JNORM-1+2*(N-1)+2)+B
                     IJ = IJ + 1
                     ZR(IDOBJ2-1+2*(IJ-1)+1) = A
                     ZR(IDOBJ2-1+2*(IJ-1)+2) = B
                  END IF
    4          CONTINUE
    3       CONTINUE
         ELSE IF (NDIM.EQ.3.AND.NOMTYP(1:3).EQ.'SEG') THEN
            CALL UTMESS ('F','CANORT','IMPOSSIBLE DE CALCULER '//
     &                 'LA NORMALE D UN SEGMENT EN 3D')
         ELSE IF (NDIM.EQ.3.AND.NOMTYP(1:4).EQ.'TRIA') THEN
            INO1=ZI(JDES-1+1)
            INO2=ZI(JDES-1+2)
            INO3=ZI(JDES-1+3)
            COOR(1,1)=ZR(JCOOR-1+3*(INO1-1)+1)
            COOR(2,1)=ZR(JCOOR-1+3*(INO1-1)+2)
            COOR(3,1)=ZR(JCOOR-1+3*(INO1-1)+3)
            COOR(1,2)=ZR(JCOOR-1+3*(INO2-1)+1)
            COOR(2,2)=ZR(JCOOR-1+3*(INO2-1)+2)
            COOR(3,2)=ZR(JCOOR-1+3*(INO2-1)+3)
            COOR(1,3)=ZR(JCOOR-1+3*(INO3-1)+1)
            COOR(2,3)=ZR(JCOOR-1+3*(INO3-1)+2)
            COOR(3,3)=ZR(JCOOR-1+3*(INO3-1)+3)
            CALL CANOR3(COOR,A,B,C)
            DO 5 N=1,NBNO
               DO 6 IN=1,NN
                  INO=ZI(JDES-1+IN)
                  IF(INO.EQ.NUNO(N)) THEN
                     ZR(JNORM-1+3*(N-1)+1)=ZR(JNORM-1+3*(N-1)+1)+A
                     ZR(JNORM-1+3*(N-1)+2)=ZR(JNORM-1+3*(N-1)+2)+B
                     ZR(JNORM-1+3*(N-1)+3)=ZR(JNORM-1+3*(N-1)+3)+C
                     IJ = IJ + 1
                     ZR(IDOBJ2-1+3*(IJ-1)+1) = A
                     ZR(IDOBJ2-1+3*(IJ-1)+2) = B
                     ZR(IDOBJ2-1+3*(IJ-1)+3) = C
                  END IF
    6          CONTINUE
    5       CONTINUE
         ELSE IF (NDIM.EQ.3.AND.NOMTYP(1:4).EQ.'QUAD') THEN
C
C           ON TRAITE D'ABORD LES QUATRE SOMMETS DU QUADRILATERE
C
            INO1=ZI(JDES-1+1)
            INO2=ZI(JDES-1+2)
            INO3=ZI(JDES-1+3)
            INO4=ZI(JDES-1+4)
            DO 45 N=1,NBNO
               INOGRE=NUNO(N)
               IF (INOGRE.EQ.INO1) THEN
                  COOR(1,1)=ZR(JCOOR-1+3*(INO1-1)+1)
                  COOR(2,1)=ZR(JCOOR-1+3*(INO1-1)+2)
                  COOR(3,1)=ZR(JCOOR-1+3*(INO1-1)+3)
                  COOR(1,2)=ZR(JCOOR-1+3*(INO2-1)+1)
                  COOR(2,2)=ZR(JCOOR-1+3*(INO2-1)+2)
                  COOR(3,2)=ZR(JCOOR-1+3*(INO2-1)+3)
                  COOR(1,3)=ZR(JCOOR-1+3*(INO4-1)+1)
                  COOR(2,3)=ZR(JCOOR-1+3*(INO4-1)+2)
                  COOR(3,3)=ZR(JCOOR-1+3*(INO4-1)+3)
                  CALL CANOR3(COOR,A1,B1,C1)
                  ZR(JNORM-1+3*(N-1)+1)=ZR(JNORM-1+3*(N-1)+1)+A1
                  ZR(JNORM-1+3*(N-1)+2)=ZR(JNORM-1+3*(N-1)+2)+B1
                  ZR(JNORM-1+3*(N-1)+3)=ZR(JNORM-1+3*(N-1)+3)+C1
                  IJ = IJ + 1
                  ZR(IDOBJ2-1+3*(IJ-1)+1) = A1
                  ZR(IDOBJ2-1+3*(IJ-1)+2) = B1
                  ZR(IDOBJ2-1+3*(IJ-1)+3) = C1
               ELSE IF (INOGRE.EQ.INO2) THEN
                  COOR(1,1)=ZR(JCOOR-1+3*(INO2-1)+1)
                  COOR(2,1)=ZR(JCOOR-1+3*(INO2-1)+2)
                  COOR(3,1)=ZR(JCOOR-1+3*(INO2-1)+3)
                  COOR(1,2)=ZR(JCOOR-1+3*(INO3-1)+1)
                  COOR(2,2)=ZR(JCOOR-1+3*(INO3-1)+2)
                  COOR(3,2)=ZR(JCOOR-1+3*(INO3-1)+3)
                  COOR(1,3)=ZR(JCOOR-1+3*(INO1-1)+1)
                  COOR(2,3)=ZR(JCOOR-1+3*(INO1-1)+2)
                  COOR(3,3)=ZR(JCOOR-1+3*(INO1-1)+3)
                  CALL CANOR3(COOR,A2,B2,C2)
                  ZR(JNORM-1+3*(N-1)+1)=ZR(JNORM-1+3*(N-1)+1)+A2
                  ZR(JNORM-1+3*(N-1)+2)=ZR(JNORM-1+3*(N-1)+2)+B2
                  ZR(JNORM-1+3*(N-1)+3)=ZR(JNORM-1+3*(N-1)+3)+C2
                  IJ = IJ + 1
                  ZR(IDOBJ2-1+3*(IJ-1)+1) = A2
                  ZR(IDOBJ2-1+3*(IJ-1)+2) = B2
                  ZR(IDOBJ2-1+3*(IJ-1)+3) = C2
               ELSE IF (INOGRE.EQ.INO3) THEN
                  COOR(1,1)=ZR(JCOOR-1+3*(INO3-1)+1)
                  COOR(2,1)=ZR(JCOOR-1+3*(INO3-1)+2)
                  COOR(3,1)=ZR(JCOOR-1+3*(INO3-1)+3)
                  COOR(1,2)=ZR(JCOOR-1+3*(INO4-1)+1)
                  COOR(2,2)=ZR(JCOOR-1+3*(INO4-1)+2)
                  COOR(3,2)=ZR(JCOOR-1+3*(INO4-1)+3)
                  COOR(1,3)=ZR(JCOOR-1+3*(INO2-1)+1)
                  COOR(2,3)=ZR(JCOOR-1+3*(INO2-1)+2)
                  COOR(3,3)=ZR(JCOOR-1+3*(INO2-1)+3)
                  CALL CANOR3(COOR,A3,B3,C3)
                  ZR(JNORM-1+3*(N-1)+1)=ZR(JNORM-1+3*(N-1)+1)+A3
                  ZR(JNORM-1+3*(N-1)+2)=ZR(JNORM-1+3*(N-1)+2)+B3
                  ZR(JNORM-1+3*(N-1)+3)=ZR(JNORM-1+3*(N-1)+3)+C3
                  IJ = IJ + 1
                  ZR(IDOBJ2-1+3*(IJ-1)+1) = A3
                  ZR(IDOBJ2-1+3*(IJ-1)+2) = B3
                  ZR(IDOBJ2-1+3*(IJ-1)+3) = C3
               ELSE IF (INOGRE.EQ.INO4) THEN
                  COOR(1,1)=ZR(JCOOR-1+3*(INO4-1)+1)
                  COOR(2,1)=ZR(JCOOR-1+3*(INO4-1)+2)
                  COOR(3,1)=ZR(JCOOR-1+3*(INO4-1)+3)
                  COOR(1,2)=ZR(JCOOR-1+3*(INO1-1)+1)
                  COOR(2,2)=ZR(JCOOR-1+3*(INO1-1)+2)
                  COOR(3,2)=ZR(JCOOR-1+3*(INO1-1)+3)
                  COOR(1,3)=ZR(JCOOR-1+3*(INO3-1)+1)
                  COOR(2,3)=ZR(JCOOR-1+3*(INO3-1)+2)
                  COOR(3,3)=ZR(JCOOR-1+3*(INO3-1)+3)
                  CALL CANOR3(COOR,A4,B4,C4)
                  ZR(JNORM-1+3*(N-1)+1)=ZR(JNORM-1+3*(N-1)+1)+A4
                  ZR(JNORM-1+3*(N-1)+2)=ZR(JNORM-1+3*(N-1)+2)+B4
                  ZR(JNORM-1+3*(N-1)+3)=ZR(JNORM-1+3*(N-1)+3)+C4
                  IJ = IJ + 1
                  ZR(IDOBJ2-1+3*(IJ-1)+1) = A4
                  ZR(IDOBJ2-1+3*(IJ-1)+2) = B4
                  ZR(IDOBJ2-1+3*(IJ-1)+3) = C4
              END IF
   45       CONTINUE
C
C           ON TRAITE ENSUITE LES EVENTUELS NOEUDS MILIEUX DE COTES
C
            IF (NN.GT.4.AND.NN.LE.9) THEN
               INO5=ZI(JDES-1+5)
               INO6=ZI(JDES-1+6)
               INO7=ZI(JDES-1+7)
               INO8=ZI(JDES-1+8)
               IF (NN.EQ.9) THEN
                  INO9=ZI(JDES-1+9)
               ELSE
                  INO9=0
               END IF
               DO 46 N=1,NBNO
                  INOGRE=NUNO(N)
                  IF (INOGRE.EQ.INO5) THEN
                   ZR(JNORM-1+3*(N-1)+1)=ZR(JNORM-1+3*(N-1)+1)+(A1+A2)/2
                   ZR(JNORM-1+3*(N-1)+2)=ZR(JNORM-1+3*(N-1)+2)+(B1+B2)/2
                   ZR(JNORM-1+3*(N-1)+3)=ZR(JNORM-1+3*(N-1)+3)+(C1+C2)/2
                   IJ = IJ + 1
                   ZR(IDOBJ2-1+3*(IJ-1)+1) = (A1+A2)/2
                   ZR(IDOBJ2-1+3*(IJ-1)+2) = (B1+B2)/2
                   ZR(IDOBJ2-1+3*(IJ-1)+3) = (C1+C2)/2
                  ELSE IF (INOGRE.EQ.INO6) THEN
                   ZR(JNORM-1+3*(N-1)+1)=ZR(JNORM-1+3*(N-1)+1)+(A2+A3)/2
                   ZR(JNORM-1+3*(N-1)+2)=ZR(JNORM-1+3*(N-1)+2)+(B2+B3)/2
                   ZR(JNORM-1+3*(N-1)+3)=ZR(JNORM-1+3*(N-1)+3)+(C2+C3)/2
                   IJ = IJ + 1
                   ZR(IDOBJ2-1+3*(IJ-1)+1) = (A2+A3)/2
                   ZR(IDOBJ2-1+3*(IJ-1)+2) = (B2+B3)/2
                   ZR(IDOBJ2-1+3*(IJ-1)+3) = (C2+C3)/2
                  ELSE IF (INOGRE.EQ.INO7) THEN
                   ZR(JNORM-1+3*(N-1)+1)=ZR(JNORM-1+3*(N-1)+1)+(A3+A4)/2
                   ZR(JNORM-1+3*(N-1)+2)=ZR(JNORM-1+3*(N-1)+2)+(B3+B4)/2
                   ZR(JNORM-1+3*(N-1)+3)=ZR(JNORM-1+3*(N-1)+3)+(C3+C4)/2
                   IJ = IJ + 1
                   ZR(IDOBJ2-1+3*(IJ-1)+1) = (A3+A4)/2
                   ZR(IDOBJ2-1+3*(IJ-1)+2) = (B3+B4)/2
                   ZR(IDOBJ2-1+3*(IJ-1)+3) = (C3+C4)/2
                  ELSE IF (INOGRE.EQ.INO8) THEN
                   ZR(JNORM-1+3*(N-1)+1)=ZR(JNORM-1+3*(N-1)+1)+(A4+A1)/2
                   ZR(JNORM-1+3*(N-1)+2)=ZR(JNORM-1+3*(N-1)+2)+(B4+B1)/2
                   ZR(JNORM-1+3*(N-1)+3)=ZR(JNORM-1+3*(N-1)+3)+(C4+C1)/2
                   IJ = IJ + 1
                   ZR(IDOBJ2-1+3*(IJ-1)+1) = (A4+A1)/2
                   ZR(IDOBJ2-1+3*(IJ-1)+2) = (B4+B1)/2
                   ZR(IDOBJ2-1+3*(IJ-1)+3) = (C4+C1)/2
                  ELSE IF (INOGRE.EQ.INO9) THEN
                   ZR(JNORM-1+3*(N-1)+1)=ZR(JNORM-1+3*(N-1)+1)
     &                                   +(A1+A2+A3+A4)/4
                   ZR(JNORM-1+3*(N-1)+2)=ZR(JNORM-1+3*(N-1)+2)
     &                                   +(B1+B2+B3+B4)/4
                   ZR(JNORM-1+3*(N-1)+3)=ZR(JNORM-1+3*(N-1)+3)
     &                                   +(C1+C2+C3+C4)/4
                   IJ = IJ + 1
                   ZR(IDOBJ2-1+3*(IJ-1)+1) = (A1+A2+A3+A4)/4
                   ZR(IDOBJ2-1+3*(IJ-1)+2) = (B1+B2+B3+B4)/4
                   ZR(IDOBJ2-1+3*(IJ-1)+3) = (C1+C2+C3+C4)/4
                  END IF
   46          CONTINUE
            ELSE IF (NN.EQ.12) THEN
               INO5=ZI(JDES-1+5)
               INO6=ZI(JDES-1+6)
               INO7=ZI(JDES-1+7)
               INO8=ZI(JDES-1+8)
               INO9=ZI(JDES-1+9)
               IN10=ZI(JDES-1+10)
               IN11=ZI(JDES-1+11)
               DO 47 N=1,NBNO
                  INOGRE=NUNO(N)
                  IF (INOGRE.EQ.INO5.OR.INOGRE.EQ.6) THEN
                   ZR(JNORM-1+3*(N-1)+1)=ZR(JNORM-1+3*(N-1)+1)+(A1+A2)/2
                   ZR(JNORM-1+3*(N-1)+2)=ZR(JNORM-1+3*(N-1)+2)+(B1+B2)/2
                   ZR(JNORM-1+3*(N-1)+3)=ZR(JNORM-1+3*(N-1)+3)+(C1+C2)/2
                   IJ = IJ + 1
                   ZR(IDOBJ2-1+3*(IJ-1)+1) = (A1+A2)/2
                   ZR(IDOBJ2-1+3*(IJ-1)+2) = (B1+B2)/2
                   ZR(IDOBJ2-1+3*(IJ-1)+3) = (C1+C2)/2
                  ELSE IF (INOGRE.EQ.INO7.OR.INOGRE.EQ.8) THEN
                   ZR(JNORM-1+3*(N-1)+1)=ZR(JNORM-1+3*(N-1)+1)+(A2+A3)/2
                   ZR(JNORM-1+3*(N-1)+2)=ZR(JNORM-1+3*(N-1)+2)+(B2+B3)/2
                   ZR(JNORM-1+3*(N-1)+3)=ZR(JNORM-1+3*(N-1)+3)+(C2+C3)/2
                   IJ = IJ + 1
                   ZR(IDOBJ2-1+3*(IJ-1)+1) = (A2+A3)/2
                   ZR(IDOBJ2-1+3*(IJ-1)+2) = (B2+B3)/2
                   ZR(IDOBJ2-1+3*(IJ-1)+3) = (C2+C3)/2
                  ELSE IF (INOGRE.EQ.INO9.OR.INOGRE.EQ.IN10) THEN
                   ZR(JNORM-1+3*(N-1)+1)=ZR(JNORM-1+3*(N-1)+1)+(A3+A4)/2
                   ZR(JNORM-1+3*(N-1)+2)=ZR(JNORM-1+3*(N-1)+2)+(B3+B4)/2
                   ZR(JNORM-1+3*(N-1)+3)=ZR(JNORM-1+3*(N-1)+3)+(C3+C4)/2
                   IJ = IJ + 1
                   ZR(IDOBJ2-1+3*(IJ-1)+1) = (A3+A4)/2
                   ZR(IDOBJ2-1+3*(IJ-1)+2) = (B3+B4)/2
                   ZR(IDOBJ2-1+3*(IJ-1)+3) = (C3+C4)/2
                  ELSE IF (INOGRE.EQ.IN11.OR.INOGRE.EQ.12) THEN
                   ZR(JNORM-1+3*(N-1)+1)=ZR(JNORM-1+3*(N-1)+1)+(A4+A1)/2
                   ZR(JNORM-1+3*(N-1)+2)=ZR(JNORM-1+3*(N-1)+2)+(B4+B1)/2
                   ZR(JNORM-1+3*(N-1)+3)=ZR(JNORM-1+3*(N-1)+3)+(C4+C1)/2
                   IJ = IJ + 1
                   ZR(IDOBJ2-1+3*(IJ-1)+1) = (A4+A1)/2
                   ZR(IDOBJ2-1+3*(IJ-1)+2) = (B4+B1)/2
                   ZR(IDOBJ2-1+3*(IJ-1)+3) = (C4+C1)/2
                  END IF
   47          CONTINUE
            END IF
         ELSE
            CALL UTMESS('F','CANORT','NOMBRE DE NOEUDS PAS PREVU')
         END IF
    1 CONTINUE
      IJ = 0
      DO 2 N=1,NBNO
         INO = NUNO(N)
         NOCC=NBOPN(N)
         IF (NDIM.EQ.2) THEN
            ZR(JNORM-1+2*(N-1)+1)=ZR(JNORM-1+2*(N-1)+1)/NOCC
            ZR(JNORM-1+2*(N-1)+2)=ZR(JNORM-1+2*(N-1)+2)/NOCC
            VNORM =  ZR(JNORM-1+2*(N-1)+1)*ZR(JNORM-1+2*(N-1)+1)
     +             + ZR(JNORM-1+2*(N-1)+2)*ZR(JNORM-1+2*(N-1)+2) 
            VNORM = SQRT(VNORM)
            IF (VNORM.LT.1.0D-2) THEN
                  CALL JENUNO(JEXNUM(NOMA//'.NOMNOE',INO),NOMNOE)
                  CALL UTMESS('F','CANORT','LA NORME DU VECTEUR '//
     +                     'NORMAL ( OU TANGENTIEL) MOYENNE EST '
     +                   //'PRESQUE NULLE. LES FACETTES CONCOURANTES '
     +                   //' AU NOEUD '//NOMNOE// ' NE DEFINISSENT '
     +                   //'PAS UNE NORMALE MOYENNE FIABLE . '
     +                   //'IL Y A UN PROBLEME DANS '
     +                   //'LA DEFINITION DE VOS MAILLES DE BORD .')
            ENDIF
            ZR(JNORM-1+2*(N-1)+1)=ZR(JNORM-1+2*(N-1)+1)/VNORM
            ZR(JNORM-1+2*(N-1)+2)=ZR(JNORM-1+2*(N-1)+2)/VNORM
            DO 7 I = 1, NOCC
               IJ = IJ + 1
               COSVEC =  ZR(JNORM-1+2*(N-1)+1)*ZR(IDOBJ2-1+2*(IJ-1)+1)
     +                 + ZR(JNORM-1+2*(N-1)+2)*ZR(IDOBJ2-1+2*(IJ-1)+2)
               SINVEC =  ZR(JNORM-1+2*(N-1)+1)*ZR(IDOBJ2-1+2*(IJ-1)+2)
     +                 - ZR(JNORM-1+2*(N-1)+2)*ZR(IDOBJ2-1+2*(IJ-1)+1)
               ANGL = R8RDDG()*ATAN2(SINVEC,COSVEC)
               IF (ABS(ANGL).GT.10.0D0) THEN
                  CALL JENUNO(JEXNUM(NOMA//'.NOMNOE',INO),NOMNOE)
                  CALL CODREE(ABS(ANGL),'G',KANGL)
                  CALL UTMESS('A','CANORT','L''ANGLE '//
     +                       'FORME PAR LE VECTEUR NORMAL COURANT '
     +                     //'A 1 FACE ET LE VECTEUR NORMAL MOYENNE,'
     +                     //' AU NOEUD '//NOMNOE//', EST SUPERIEUR'
     +                     //' A 10 DEGRES ET VAUT '//KANGL//' DEGRES.')
               ENDIF
  7         CONTINUE
         ELSE IF (NDIM.EQ.3) THEN
            ZR(JNORM-1+3*(N-1)+1)=ZR(JNORM-1+3*(N-1)+1)/NOCC
            ZR(JNORM-1+3*(N-1)+2)=ZR(JNORM-1+3*(N-1)+2)/NOCC
            ZR(JNORM-1+3*(N-1)+3)=ZR(JNORM-1+3*(N-1)+3)/NOCC
            VNORM =  ZR(JNORM-1+3*(N-1)+1)*ZR(JNORM-1+3*(N-1)+1)
     +             + ZR(JNORM-1+3*(N-1)+2)*ZR(JNORM-1+3*(N-1)+2) 
     +             + ZR(JNORM-1+3*(N-1)+3)*ZR(JNORM-1+3*(N-1)+3) 
            VNORM = SQRT(VNORM)
            IF (VNORM.LT.1.0D-2) THEN
               CALL JENUNO(JEXNUM(NOMA//'.NOMNOE',INO),NOMNOE)
               CALL UTMESS('F','CANORT','LA NORME DU VECTEUR '//
     +                     'NORMAL MOYENNE EST PRESQUE'
     +                   //' NULLE. LES FACETTES CONCOURANTES '
     +                   //' AU NOEUD '//NOMNOE// ' NE DEFINISSENT '
     +                   //'PAS UNE NORMALE MOYENNE FIABLE . '
     +                   //'IL Y A UN PROBLEME DANS LA'
     +                   //' DEFINITION DE VOS MAILLES DE BORD .')
            ENDIF
            ZR(JNORM-1+3*(N-1)+1)=ZR(JNORM-1+3*(N-1)+1)/VNORM
            ZR(JNORM-1+3*(N-1)+2)=ZR(JNORM-1+3*(N-1)+2)/VNORM
            ZR(JNORM-1+3*(N-1)+3)=ZR(JNORM-1+3*(N-1)+3)/VNORM
            DO 8 I = 1, NOCC
               IJ = IJ + 1
               COSVEC =  ZR(JNORM-1+3*(N-1)+1)*ZR(IDOBJ2-1+3*(IJ-1)+1)
     +                 + ZR(JNORM-1+3*(N-1)+2)*ZR(IDOBJ2-1+3*(IJ-1)+2)
     +                 + ZR(JNORM-1+3*(N-1)+3)*ZR(IDOBJ2-1+3*(IJ-1)+3)
               CALL PROVEC(ZR(JNORM-1+3*(N-1)+1),
     +                     ZR(IDOBJ2-1+3*(IJ-1)+1),PVEC)
               SINVEC = PVEC(1)*PVEC(1) + PVEC(2)*PVEC(2) + 
     +                  PVEC(3)*PVEC(3)
               SINVEC = SQRT(SINVEC)
               ANGL = R8RDDG()*ATAN2(SINVEC,COSVEC)
               IF (ABS(ANGL).GT.10.0D0) THEN
                  CALL JENUNO(JEXNUM(NOMA//'.NOMNOE',INO),NOMNOE)
                  CALL CODREE(ABS(ANGL),'G',KANGL)
                  CALL UTMESS('A','CANORT','L''ANGLE '//
     +                       'FORME PAR LE VECTEUR NORMAL COURANT '
     +                     //'A 1 FACE ET LE VECTEUR NORMAL MOYENNE,'
     +                     //' AU NOEUD '//NOMNOE//', EST SUPERIEUR'
     +                     //' A 10 DEGRES ET VAUT '//KANGL//' DEGRES.')
               ENDIF
  8         CONTINUE
         END IF
    2 CONTINUE
C
      CALL JEDETR(NOMOB2)
      CALL JEDEMA()
      END
