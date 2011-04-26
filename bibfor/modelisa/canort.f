      SUBROUTINE CANORT(NOMA,NBMA,LISTI,LISTK,NDIM,NBNO,NUNO,L)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT NONE
      INTEGER NBMA,LISTI(*),NDIM,NBNO,NUNO(*),L
      CHARACTER*8 NOMA,LISTK(*)

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
C      NUNO : LISTE DES NUMEROS DE NOEUDS DE LA LISTE DE MAILLES
C      L    : =1 ==> CALCUL DE LA NORMALE (2D ET 3D)
C             =2 ==> CALCUL DE LA TANGENTE (2D )
C OBJETS JEVEUX CREES
C     &&CANORT.NORMALE : NORMALES MOYENNEES AUX NOEUDS (2 EN 2D,3 EN 3D)
C     &&CANORT.TANGENT : TANGENTES AUX NOEUDS (2 EN 2D)
C
C ROUTINES APPELEES:
C -------------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ----------------
C TOLE CRP_20

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
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------
C
      INTEGER      DIMCOO,I,IFONC,IBID,IRET,JNORM,ISOM,IN
      INTEGER      IDOBJ2,JCOOR,IATYMA,M,JCOODE,IJ,INO
      INTEGER      N,NOCC,NNO,NNOS,NBPAR,NNN
      INTEGER      IINVER,IMAIL,NUMAIL,ITYP,JDES,NN,NUMNO,LINO(9)
      REAL*8       COOR(3,9),A,B,C,PVEC(3),NORME,R8B
      COMPLEX*16   C16B
      CHARACTER*8  KANGL,K8B,KNUMAI
      CHARACTER*8  MK,NOMTYP,NOMNOE,K8BID
      CHARACTER*19 NOMT19
      CHARACTER*24 NOMOBJ,NOMOB2,CONINV,PARA
      CHARACTER*24 VALK(2)
      CHARACTER*32 JEXNOM,JEXNUM
      CHARACTER*1  K1B
      REAL*8       DFSE2(4),DFSE3(9),R8RDDG,ARMIN,PREC
      REAL*8       DFTR3(18),DFTR6(72),DFTR7(98)
      REAL*8       DFQU4(32),DFQU8(128),DFQU9(162)
      REAL*8       EKSIX,EKSIY,EKSIZ,EETAX,EETAY,EETAZ
      REAL*8       VNORM,COSVEC,SINVEC,ANGL,ATAN2

C
      CALL JEMARQ()

C     RECUPERATION DES FONCTIONS DE FORMES POUR TOUS LES
C     TYPES D ELEMENTS SUSCEPTIBLES D ETRE PRESENT

      CALL DFFNO('SE2',IBID,NNO,NNOS,DFSE2)
      CALL DFFNO('SE3',IBID,NNO,NNOS,DFSE3)
      CALL DFFNO('TR3',IBID,NNO,NNOS,DFTR3)
      CALL DFFNO('TR6',IBID,NNO,NNOS,DFTR6)
      CALL DFFNO('TR7',IBID,NNO,NNOS,DFTR7)
      CALL DFFNO('QU4',IBID,NNO,NNOS,DFQU4)
      CALL DFFNO('QU8',IBID,NNO,NNOS,DFQU8)
      CALL DFFNO('QU9',IBID,NNO,NNOS,DFQU9)
      CONINV='&&CANORT.CONINV'

      IF (L.EQ.1) NOMOBJ = '&&CANORT.NORMALE'
      IF (L.EQ.2) NOMOBJ = '&&CANORT.TANGENT'
      CALL JEEXIN(NOMOBJ,IRET)
      IF (IRET.NE.0) CALL JEDETR(NOMOBJ)
      CALL JECREO(NOMOBJ,'V V R')
      CALL JEECRA(NOMOBJ,'LONMAX',NDIM*NBNO,' ')
      CALL JEVEUO(NOMOBJ,'E',JNORM)
C
      CALL JEVEUO(NOMA//'.COORDO    .VALE','L',JCOOR)
      CALL JEVEUO(NOMA//'.TYPMAIL','L',IATYMA)

C --- RECUPERATION DE L'ARETE MINIMUM DU MAILLAGE
C
      CALL JEEXIN ( NOMA//'           .LTNT', IRET )
      IF ( IRET .NE. 0 ) THEN
         CALL LTNOTB ( NOMA , 'CARA_GEOM' , NOMT19 )
         NBPAR = 0
         PARA = 'AR_MIN                  '
         CALL TBLIVA (NOMT19, NBPAR, ' ', IBID, R8B, C16B, K8BID,
     &                K8BID, R8B , PARA, K8BID, IBID, ARMIN, C16B,
     &                K8BID, IRET )
          IF ( IRET .NE. 0 ) CALL U2MESS('F','MODELISA2_13')
         PREC = ARMIN*1.D-06
      ELSE
         PREC = 1.D-10
      ENDIF

C     TRANSFORMATION DE LA LISTE DE NOM DE MAILLES EN LISTE DE NUMERO
C     DE MAILLE ( POUR PASSAGE DANS CNCINV )
      IF (NBMA.LT.0) THEN
         DO 5 M=1,ABS(NBMA)
            MK=LISTK(M)
            CALL JENONU(JEXNOM(NOMA//'.NOMMAI',MK),LISTI(M))
    5    CONTINUE
      END IF

C     RECUPERATION DE LA CONNECTIVITE INVERSE
      CALL CNCINV(NOMA,LISTI,ABS(NBMA),'V',CONINV)

      NOMOB2 = '&&CANORT.VECTEUR'
      CALL JEEXIN(NOMOB2,IRET)
      IF (IRET.NE.0) CALL JEDETR(NOMOB2)
      ISOM = 0
      DO 1 I = 1, NBNO
         CALL JELIRA(JEXNUM(CONINV,NUNO(I)),'LONMAX',NNN,K8B)
         ISOM = ISOM + NNN
 1    CONTINUE

      CALL WKVECT(NOMOB2,'V V R',NDIM*ISOM,IDOBJ2)

      CALL JEVEUO (NOMA//'.COORDO    .DESC', 'L', JCOODE)

      IJ=0
C     BOUCLE SUR TOUS LES NOEUDS CONCERNES
      DO 10 INO=1,NBNO
         NUMNO=NUNO(INO)
         CALL JELIRA(JEXNUM(CONINV,NUMNO),'LONMAX',NNN,K8B)
         CALL JEVEUO(JEXNUM(CONINV,NUMNO),'L',IINVER)

C    BOUCLE SUR TOUTES LES MAILLES CONNECTEES AU NOEUD ACTUEL
         DO 20 IMAIL=1,NNN

C           NUMERO ABSOLUE DE LA MAILLE

            NUMAIL=LISTI(ZI(IINVER-1+IMAIL))
            ITYP=ZI(IATYMA-1+NUMAIL)
            CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ITYP),NOMTYP)
            CALL JEVEUO(JEXNUM(NOMA//'.CONNEX',NUMAIL),'L',JDES)
            CALL JELIRA(JEXNUM(NOMA//'.CONNEX',NUMAIL),'LONMAX',NN,K1B)
            IF (NDIM.EQ.2.AND.NOMTYP(1:4).EQ.'SEG2') THEN
               DIMCOO = -ZI(JCOODE-1+2)
               LINO(1)=ZI(JDES-1+1)
               LINO(2)=ZI(JDES-1+2)
               COOR(1,1)=ZR(JCOOR-1+DIMCOO*(LINO(1)-1)+1)
               COOR(2,1)=ZR(JCOOR-1+DIMCOO*(LINO(1)-1)+2)
               COOR(1,2)=ZR(JCOOR-1+DIMCOO*(LINO(2)-1)+1)
               COOR(2,2)=ZR(JCOOR-1+DIMCOO*(LINO(2)-1)+2)
               EKSIX=COOR(1,1)*DFSE2(1)+COOR(1,2)*DFSE2(2)
               EKSIY=COOR(2,1)*DFSE2(1)+COOR(2,2)*DFSE2(2)
               IF (L.EQ.2) THEN
                  NORME=SQRT(EKSIX**2+EKSIY**2)
                  IF (NORME.GT.PREC) THEN
                      A=EKSIX/NORME
                      B=EKSIY/NORME
                  ELSE
                     CALL JENUNO(JEXNUM(NOMA//'.NOMMAI',NUMAIL),KNUMAI)
                     CALL U2MESK('F','MODELISA3_23',1,KNUMAI)
                  ENDIF
               ELSEIF (L.EQ.1) THEN
                  NORME=SQRT(EKSIX**2+EKSIY**2)
                  IF (NORME.GT.PREC) THEN
                     A=EKSIY/NORME
                     B=-EKSIX/NORME
                  ELSE
                     CALL JENUNO(JEXNUM(NOMA//'.NOMMAI',NUMAIL),KNUMAI)
                     CALL U2MESK('F','MODELISA3_24',1,KNUMAI)
                  ENDIF
               ENDIF
               ZR(JNORM-1+2*(INO-1)+1)=ZR(JNORM-1+2*(INO-1)+1)
     &               +A/NNN
               ZR(JNORM-1+2*(INO-1)+2)=ZR(JNORM-1+2*(INO-1)+2)
     &               +B/NNN
               IJ=IJ+1
               ZR(IDOBJ2-1+2*(IJ-1)+1) = A
               ZR(IDOBJ2-1+2*(IJ-1)+2) = B
            ELSE IF (NDIM.EQ.2.AND.NOMTYP(1:4).EQ.'SEG3') THEN
               DO 30 I=1,NN
                  DIMCOO = -ZI(JCOODE-1+2)
                  LINO(I)=ZI(JDES-1+I)
                  COOR(1,I)=ZR(JCOOR-1+DIMCOO*(LINO(I)-1)+1)
                  COOR(2,I)=ZR(JCOOR-1+DIMCOO*(LINO(I)-1)+2)
                  COOR(3,I)=0.D0
                  IF ( NUMNO.EQ.LINO(I)) IN=I
 30            CONTINUE
               EKSIX=0.D0
               EKSIY=0.D0
C              CALCUL DU  VECTEUR TANGENT VIA LES FONCTIONS DE FORMES
               DO 35 IFONC=1,NN
                  EKSIX=EKSIX+COOR(1,IFONC)*DFSE3((IN-1)*NN+IFONC)
                  EKSIY=EKSIY+COOR(2,IFONC)*DFSE3((IN-1)*NN+IFONC)
 35            CONTINUE
C              ON S INTERESSE AU VECTEUR TANGENT
               IF (L.EQ.2) THEN
                  NORME=SQRT(EKSIX**2+EKSIY**2)
                  IF (NORME.GT.PREC) THEN
                     A=EKSIX/NORME
                     B=EKSIY/NORME
                  ELSE
                     CALL NORLIN('SE3',2,KNUMAI,COOR,DFSE2,IN,PREC,
     &                           A,B,C)
                  ENDIF

C              ON S INTERESSE AU VECTEUR NORMAL
               ELSEIF (L.EQ.1) THEN
                  NORME=SQRT(EKSIX**2+EKSIY**2)
                  IF (NORME.GT.PREC) THEN
                     A=EKSIY/NORME
                     B=-EKSIX/NORME
                  ELSE
                     CALL NORLIN('SE3',1,KNUMAI,COOR,DFSE2,IN,PREC,
     &                           A,B,C)
                  ENDIF
               ENDIF
               ZR(JNORM-1+2*(INO-1)+1)=ZR(JNORM-1+2*(INO-1)+1)
     &               +A/NNN
               ZR(JNORM-1+2*(INO-1)+2)=ZR(JNORM-1+2*(INO-1)+2)
     &               +B/NNN
               IJ=IJ+1
               ZR(IDOBJ2-1+2*(IJ-1)+1) = A
               ZR(IDOBJ2-1+2*(IJ-1)+2) = B
            ELSE IF (NDIM.EQ.3.AND.NOMTYP(1:3).EQ.'SEG') THEN
               CALL U2MESS('F','MODELISA3_25')

            ELSE IF (NDIM.EQ.3.AND.NOMTYP(1:5).EQ.'QUAD4') THEN
               DO 40 I=1,NN
                  LINO(I)=ZI(JDES-1+I)
                  COOR(1,I)=ZR(JCOOR-1+3*(LINO(I)-1)+1)
                  COOR(2,I)=ZR(JCOOR-1+3*(LINO(I)-1)+2)
                  COOR(3,I)=ZR(JCOOR-1+3*(LINO(I)-1)+3)
                  IF ( NUMNO.EQ.LINO(I)) IN=I
 40            CONTINUE
               EKSIX=0.D0
               EKSIY=0.D0
               EKSIZ=0.D0
               EETAX=0.D0
               EETAY=0.D0
               EETAZ=0.D0

C              CALCUL DES DEUX VECTEURS TANGENTS
               DO 45 IFONC=1,NN
                  EKSIX=EKSIX+COOR(1,IFONC)*DFQU4((IN-1)*NN*2+IFONC)
                  EKSIY=EKSIY+COOR(2,IFONC)*DFQU4((IN-1)*NN*2+IFONC)
                  EKSIZ=EKSIZ+COOR(3,IFONC)*DFQU4((IN-1)*NN*2+IFONC)

                  EETAX=EETAX+COOR(1,IFONC)*DFQU4((IN-1)*NN*2+NN+IFONC)
                  EETAY=EETAY+COOR(2,IFONC)*DFQU4((IN-1)*NN*2+NN+IFONC)
                  EETAZ=EETAZ+COOR(3,IFONC)*DFQU4((IN-1)*NN*2+NN+IFONC)
 45            CONTINUE

C              CALCUL DU VECTEUR NORMAL ET NORMALISATION
               A=EKSIY*EETAZ-EKSIZ*EETAY
               B=EKSIZ*EETAX-EKSIX*EETAZ
               C=EKSIX*EETAY-EKSIY*EETAX
               NORME=SQRT(A*A+B*B+C*C)
               IF (NORME.GT.PREC) THEN
                  A=A/NORME
                  B=B/NORME
                  C=C/NORME
               ELSE
                  CALL JENUNO(JEXNUM(NOMA//'.NOMMAI',NUMAIL),KNUMAI)
                  CALL U2MESK('F','MODELISA3_26',1,KNUMAI)
               ENDIF
C              ON FAIT LA MOYENNE SUR TOUTES LES MAILLES DES NORMALES
C              RELATIVES A UN NOEUD
               ZR(JNORM-1+3*(INO-1)+1)=ZR(JNORM-1+3*(INO-1)+1)
     &            +A/NNN
               ZR(JNORM-1+3*(INO-1)+2)=ZR(JNORM-1+3*(INO-1)+2)
     &            +B/NNN
               ZR(JNORM-1+3*(INO-1)+3)=ZR(JNORM-1+3*(INO-1)+3)
     &            +C/NNN
               IJ=IJ+1
C              ON STOCHE DANS L OBJET IDOBJ2 TOUTES LES NORMALES POUR
C              UNE VERIFICATION ULTERIEURE
               ZR(IDOBJ2-1+3*(IJ-1)+1) = A
               ZR(IDOBJ2-1+3*(IJ-1)+2) = B
               ZR(IDOBJ2-1+3*(IJ-1)+3) = C
            ELSE IF (NDIM.EQ.3.AND.NOMTYP(1:5).EQ.'QUAD8') THEN
               DO 50 I=1,NN
                  LINO(I)=ZI(JDES-1+I)
                  COOR(1,I)=ZR(JCOOR-1+3*(LINO(I)-1)+1)
                  COOR(2,I)=ZR(JCOOR-1+3*(LINO(I)-1)+2)
                  COOR(3,I)=ZR(JCOOR-1+3*(LINO(I)-1)+3)
                  IF ( NUMNO.EQ.LINO(I)) IN=I
 50            CONTINUE
               EKSIX=0.D0
               EKSIY=0.D0
               EKSIZ=0.D0
               EETAX=0.D0
               EETAY=0.D0
               EETAZ=0.D0
C              CALCUL DES DEUX VECTEURS TANGENTS
               DO 55 IFONC=1,NN

                  EKSIX=EKSIX+COOR(1,IFONC)*DFQU8((IN-1)*NN*2+IFONC)
                  EKSIY=EKSIY+COOR(2,IFONC)*DFQU8((IN-1)*NN*2+IFONC)
                  EKSIZ=EKSIZ+COOR(3,IFONC)*DFQU8((IN-1)*NN*2+IFONC)

                  EETAX=EETAX+COOR(1,IFONC)*DFQU8((IN-1)*NN*2+NN+IFONC)
                  EETAY=EETAY+COOR(2,IFONC)*DFQU8((IN-1)*NN*2+NN+IFONC)
                  EETAZ=EETAZ+COOR(3,IFONC)*DFQU8((IN-1)*NN*2+NN+IFONC)
 55            CONTINUE
C              CALCUL DU VECTEUR NORMAL ET NORMALISATION
               A=EKSIY*EETAZ-EKSIZ*EETAY
               B=EKSIZ*EETAX-EKSIX*EETAZ
               C=EKSIX*EETAY-EKSIY*EETAX
               NORME=SQRT(A*A+B*B+C*C)
               IF (NORME.GT.PREC) THEN
                  A=A/NORME
                  B=B/NORME
                  C=C/NORME
               ELSE
                  CALL NORLIN('QU8',0,KNUMAI,COOR,DFQU4,IN,PREC,A,B,C)
               ENDIF
C              ON FAIT LA MOYENNE SUR TOUTES LES MAILLES DES NORMALES
C              RELATIVES A UN NOEUD
               ZR(JNORM-1+3*(INO-1)+1)=ZR(JNORM-1+3*(INO-1)+1)
     &            +A/NNN
               ZR(JNORM-1+3*(INO-1)+2)=ZR(JNORM-1+3*(INO-1)+2)
     &            +B/NNN
               ZR(JNORM-1+3*(INO-1)+3)=ZR(JNORM-1+3*(INO-1)+3)
     &            +C/NNN
               IJ=IJ+1
C              ON STOCHE DANS L OBJET IDOBJ2 TOUTES LES NORMALES POUR
C              UNE VERIFICATION ULTERIEURE
               ZR(IDOBJ2-1+3*(IJ-1)+1) = A
               ZR(IDOBJ2-1+3*(IJ-1)+2) = B
               ZR(IDOBJ2-1+3*(IJ-1)+3) = C
            ELSE IF (NDIM.EQ.3.AND.NOMTYP(1:5).EQ.'QUAD9') THEN
               DO 60 I=1,NN
                  LINO(I)=ZI(JDES-1+I)
                  COOR(1,I)=ZR(JCOOR-1+3*(LINO(I)-1)+1)
                  COOR(2,I)=ZR(JCOOR-1+3*(LINO(I)-1)+2)
                  COOR(3,I)=ZR(JCOOR-1+3*(LINO(I)-1)+3)
                  IF ( NUMNO.EQ.LINO(I)) IN=I
 60            CONTINUE
               EKSIX=0.D0
               EKSIY=0.D0
               EKSIZ=0.D0
               EETAX=0.D0
               EETAY=0.D0
               EETAZ=0.D0
C              CALCUL DES DEUX VECTEURS TANGENTS
               DO 65 IFONC=1,NN
                  EKSIX=EKSIX+COOR(1,IFONC)*DFQU9((IN-1)*NN*2+IFONC)
                  EKSIY=EKSIY+COOR(2,IFONC)*DFQU9((IN-1)*NN*2+IFONC)
                  EKSIZ=EKSIZ+COOR(3,IFONC)*DFQU9((IN-1)*NN*2+IFONC)

                  EETAX=EETAX+COOR(1,IFONC)*DFQU9((IN-1)*NN*2+NN+IFONC)
                  EETAY=EETAY+COOR(2,IFONC)*DFQU9((IN-1)*NN*2+NN+IFONC)
                  EETAZ=EETAZ+COOR(3,IFONC)*DFQU9((IN-1)*NN*2+NN+IFONC)
 65            CONTINUE
C              CALCUL DU VECTEUR NORMAL ET NORMALISATION
               A=EKSIY*EETAZ-EKSIZ*EETAY
               B=EKSIZ*EETAX-EKSIX*EETAZ
               C=EKSIX*EETAY-EKSIY*EETAX
               NORME=SQRT(A*A+B*B+C*C)
               IF (NORME.GT.PREC) THEN
                  A=A/NORME
                  B=B/NORME
                  C=C/NORME
               ELSE
                  CALL JENUNO(JEXNUM(NOMA//'.NOMMAI',NUMAIL),KNUMAI)
                  CALL U2MESK('F','MODELISA3_26',1,KNUMAI)
               ENDIF
C              ON FAIT LA MOYENNE SUR TOUTES LES MAILLES DES NORMALES
C              RELATIVES A UN NOEUD
               ZR(JNORM-1+3*(INO-1)+1)=ZR(JNORM-1+3*(INO-1)+1)
     &            +A/NNN
               ZR(JNORM-1+3*(INO-1)+2)=ZR(JNORM-1+3*(INO-1)+2)
     &            +B/NNN
               ZR(JNORM-1+3*(INO-1)+3)=ZR(JNORM-1+3*(INO-1)+3)
     &            +C/NNN
               IJ=IJ+1
C              ON STOCHE DANS L OBJET IDOBJ2 TOUTES LES NORMALES POUR
C              UNE VERIFICATION ULTERIEURE
               ZR(IDOBJ2-1+3*(IJ-1)+1) = A
               ZR(IDOBJ2-1+3*(IJ-1)+2) = B
               ZR(IDOBJ2-1+3*(IJ-1)+3) = C
            ELSE IF (NDIM.EQ.3.AND.NOMTYP(1:5).EQ.'TRIA3') THEN
               DO 70 I=1,NN
                  LINO(I)=ZI(JDES-1+I)
                  COOR(1,I)=ZR(JCOOR-1+3*(LINO(I)-1)+1)
                  COOR(2,I)=ZR(JCOOR-1+3*(LINO(I)-1)+2)
                  COOR(3,I)=ZR(JCOOR-1+3*(LINO(I)-1)+3)
                  IF ( NUMNO.EQ.LINO(I)) IN=I
 70            CONTINUE
C              CALCUL DES DEUX VECTEURS TANGENTS
               EKSIX=0.D0
               EKSIY=0.D0
               EKSIZ=0.D0
               EETAX=0.D0
               EETAY=0.D0
               EETAZ=0.D0
               DO 75 IFONC=1,NN
                  EKSIX=EKSIX+COOR(1,IFONC)*DFTR3((IN-1)*NN*2+IFONC)
                  EKSIY=EKSIY+COOR(2,IFONC)*DFTR3((IN-1)*NN*2+IFONC)
                  EKSIZ=EKSIZ+COOR(3,IFONC)*DFTR3((IN-1)*NN*2+IFONC)

                  EETAX=EETAX+COOR(1,IFONC)*DFTR3((IN-1)*NN*2+NN+IFONC)
                  EETAY=EETAY+COOR(2,IFONC)*DFTR3((IN-1)*NN*2+NN+IFONC)
                  EETAZ=EETAZ+COOR(3,IFONC)*DFTR3((IN-1)*NN*2+NN+IFONC)
 75            CONTINUE
C              CALCUL DU VECTEUR NORMAL ET NORMALISATION
               A=EKSIY*EETAZ-EKSIZ*EETAY
               B=EKSIZ*EETAX-EKSIX*EETAZ
               C=EKSIX*EETAY-EKSIY*EETAX
               NORME=SQRT(A*A+B*B+C*C)
               IF (NORME.GT.PREC) THEN
                  A=A/NORME
                  B=B/NORME
                  C=C/NORME
               ELSE
                  CALL JENUNO(JEXNUM(NOMA//'.NOMMAI',NUMAIL),KNUMAI)
                  CALL U2MESK('F','MODELISA3_26',1,KNUMAI)
               ENDIF
C              ON FAIT LA MOYENNE SUR TOUTES LES MAILLES DES NORMALES
C              RELATIVES A UN NOEUD
               ZR(JNORM-1+3*(INO-1)+1)=ZR(JNORM-1+3*(INO-1)+1)
     &            +A/NNN
               ZR(JNORM-1+3*(INO-1)+2)=ZR(JNORM-1+3*(INO-1)+2)
     &            +B/NNN
               ZR(JNORM-1+3*(INO-1)+3)=ZR(JNORM-1+3*(INO-1)+3)
     &            +C/NNN
               IJ=IJ+1
C              ON STOCHE DANS L OBJET IDOBJ2 TOUTES LES NORMALES POUR
C              UNE VERIFICATION ULTERIEURE
               ZR(IDOBJ2-1+3*(IJ-1)+1) = A
               ZR(IDOBJ2-1+3*(IJ-1)+2) = B
               ZR(IDOBJ2-1+3*(IJ-1)+3) = C
            ELSE IF (NDIM.EQ.3.AND.NOMTYP(1:5).EQ.'TRIA6') THEN
               DO 90 I=1,NN
                  LINO(I)=ZI(JDES-1+I)
                  COOR(1,I)=ZR(JCOOR-1+3*(LINO(I)-1)+1)
                  COOR(2,I)=ZR(JCOOR-1+3*(LINO(I)-1)+2)
                  COOR(3,I)=ZR(JCOOR-1+3*(LINO(I)-1)+3)
                  IF ( NUMNO.EQ.LINO(I)) IN=I
 90            CONTINUE
C              CALCUL DES DEUX VECTEURS TANGENTS
               EKSIX=0.D0
               EKSIY=0.D0
               EKSIZ=0.D0
               EETAX=0.D0
               EETAY=0.D0
               EETAZ=0.D0
              DO 95 IFONC=1,NN
                  EKSIX=EKSIX+COOR(1,IFONC)*DFTR6((IN-1)*NN*2+IFONC)
                  EKSIY=EKSIY+COOR(2,IFONC)*DFTR6((IN-1)*NN*2+IFONC)
                  EKSIZ=EKSIZ+COOR(3,IFONC)*DFTR6((IN-1)*NN*2+IFONC)

                  EETAX=EETAX+COOR(1,IFONC)*DFTR6((IN-1)*NN*2+NN+IFONC)
                  EETAY=EETAY+COOR(2,IFONC)*DFTR6((IN-1)*NN*2+NN+IFONC)
                  EETAZ=EETAZ+COOR(3,IFONC)*DFTR6((IN-1)*NN*2+NN+IFONC)
 95            CONTINUE
C              CALCUL DU VECTEUR NORMAL ET NORMALISATION
               A=EKSIY*EETAZ-EKSIZ*EETAY
               B=EKSIZ*EETAX-EKSIX*EETAZ
               C=EKSIX*EETAY-EKSIY*EETAX
               NORME=SQRT(A*A+B*B+C*C)
               IF (NORME.GT.PREC) THEN
                  A=A/NORME
                  B=B/NORME
                  C=C/NORME
               ELSE
                  CALL NORLIN('TR6',0,KNUMAI,COOR,DFTR3,IN,PREC,A,B,C)
               ENDIF
C              ON FAIT LA MOYENNE SUR TOUTES LES MAILLES DES NORMALES
C              RELATIVES A UN NOEUD
               ZR(JNORM-1+3*(INO-1)+1)=ZR(JNORM-1+3*(INO-1)+1)
     &            +A/NNN
               ZR(JNORM-1+3*(INO-1)+2)=ZR(JNORM-1+3*(INO-1)+2)
     &            +B/NNN
               ZR(JNORM-1+3*(INO-1)+3)=ZR(JNORM-1+3*(INO-1)+3)
     &            +C/NNN
               IJ=IJ+1
C              ON STOCHE DANS L OBJET IDOBJ2 TOUTES LES NORMALES POUR
C              UNE VERIFICATION ULTERIEURE
               ZR(IDOBJ2-1+3*(IJ-1)+1) = A
               ZR(IDOBJ2-1+3*(IJ-1)+2) = B
               ZR(IDOBJ2-1+3*(IJ-1)+3) = C
            ELSE IF (NDIM.EQ.3.AND.NOMTYP(1:5).EQ.'TRIA7') THEN
               DO 100 I=1,NN
                  LINO(I)=ZI(JDES-1+I)
                  COOR(1,I)=ZR(JCOOR-1+3*(LINO(I)-1)+1)
                  COOR(2,I)=ZR(JCOOR-1+3*(LINO(I)-1)+2)
                  COOR(3,I)=ZR(JCOOR-1+3*(LINO(I)-1)+3)
                  IF ( NUMNO.EQ.LINO(I)) IN=I
 100           CONTINUE
C              CALCUL DES DEUX VECTEURS TANGENTS
               EKSIX=0.D0
               EKSIY=0.D0
               EKSIZ=0.D0
               EETAX=0.D0
               EETAY=0.D0
               EETAZ=0.D0
               DO 105 IFONC=1,NN
                  EKSIX=EKSIX+COOR(1,IFONC)*DFTR7((IN-1)*NN*2+IFONC)
                  EKSIY=EKSIY+COOR(2,IFONC)*DFTR7((IN-1)*NN*2+IFONC)
                  EKSIZ=EKSIZ+COOR(3,IFONC)*DFTR7((IN-1)*NN*2+IFONC)

                  EETAX=EETAX+COOR(1,IFONC)*DFTR7((IN-1)*NN*2+NN+IFONC)
                  EETAY=EETAY+COOR(2,IFONC)*DFTR7((IN-1)*NN*2+NN+IFONC)
                  EETAZ=EETAZ+COOR(3,IFONC)*DFTR7((IN-1)*NN*2+NN+IFONC)
 105           CONTINUE
C              CALCUL DU VECTEUR NORMAL ET NORMALISATION
               A=EKSIY*EETAZ-EKSIZ*EETAY
               B=EKSIZ*EETAX-EKSIX*EETAZ
               C=EKSIX*EETAY-EKSIY*EETAX
               NORME=SQRT(A*A+B*B+C*C)
               IF (NORME.GT.PREC) THEN
                  A=A/NORME
                  B=B/NORME
                  C=C/NORME
               ELSE
                  CALL JENUNO(JEXNUM(NOMA//'.NOMMAI',NUMAIL),KNUMAI)
                  CALL U2MESK('F','MODELISA3_26',1,KNUMAI)
               ENDIF
C              ON FAIT LA MOYENNE SUR TOUTES LES MAILLES DES NORMALES
C              RELATIVES A UN NOEUD
               ZR(JNORM-1+3*(INO-1)+1)=ZR(JNORM-1+3*(INO-1)+1)
     &            +A/NNN
               ZR(JNORM-1+3*(INO-1)+2)=ZR(JNORM-1+3*(INO-1)+2)
     &            +B/NNN
               ZR(JNORM-1+3*(INO-1)+3)=ZR(JNORM-1+3*(INO-1)+3)
     &            +C/NNN
               IJ=IJ+1
C              ON STOCHE DANS L OBJET IDOBJ2 TOUTES LES NORMALES POUR
C              UNE VERIFICATION ULTERIEURE
               ZR(IDOBJ2-1+3*(IJ-1)+1) = A
               ZR(IDOBJ2-1+3*(IJ-1)+2) = B
               ZR(IDOBJ2-1+3*(IJ-1)+3) = C
            ELSE
               CALL U2MESS('F','MODELISA3_27')
            ENDIF
 20      CONTINUE
 10   CONTINUE


      IJ = 0
      DO 2 N=1,NBNO
         INO = NUNO(N)
         CALL JELIRA(JEXNUM(CONINV,INO),'LONMAX',NOCC,K8B)
         IF (NDIM.EQ.2) THEN
            VNORM =  ZR(JNORM-1+2*(N-1)+1)*ZR(JNORM-1+2*(N-1)+1)
     &             + ZR(JNORM-1+2*(N-1)+2)*ZR(JNORM-1+2*(N-1)+2)
            VNORM = SQRT(VNORM)
            IF (VNORM.LT.1.0D-2) THEN
                  CALL JENUNO(JEXNUM(NOMA//'.NOMNOE',INO),NOMNOE)
                  CALL U2MESK('F','MODELISA3_28',1,NOMNOE)
            ENDIF
            ZR(JNORM-1+2*(N-1)+1)=ZR(JNORM-1+2*(N-1)+1)/VNORM
            ZR(JNORM-1+2*(N-1)+2)=ZR(JNORM-1+2*(N-1)+2)/VNORM
            DO 7 I = 1, NOCC
               IJ = IJ + 1
               COSVEC =  ZR(JNORM-1+2*(N-1)+1)*ZR(IDOBJ2-1+2*(IJ-1)+1)
     &                 + ZR(JNORM-1+2*(N-1)+2)*ZR(IDOBJ2-1+2*(IJ-1)+2)
               SINVEC =  ZR(JNORM-1+2*(N-1)+1)*ZR(IDOBJ2-1+2*(IJ-1)+2)
     &                 - ZR(JNORM-1+2*(N-1)+2)*ZR(IDOBJ2-1+2*(IJ-1)+1)
               ANGL = R8RDDG()*ATAN2(SINVEC,COSVEC)
               IF (ABS(ANGL).GT.10.0D0) THEN
                  CALL JENUNO(JEXNUM(NOMA//'.NOMNOE',INO),NOMNOE)
                  CALL CODREE(ABS(ANGL),'G',KANGL)
                   VALK(1) = NOMNOE
                   VALK(2) = KANGL
                   CALL U2MESK('A','MODELISA3_29', 2 ,VALK)
               ENDIF
  7         CONTINUE
         ELSE IF (NDIM.EQ.3) THEN
            VNORM =  ZR(JNORM-1+3*(N-1)+1)*ZR(JNORM-1+3*(N-1)+1)
     &             + ZR(JNORM-1+3*(N-1)+2)*ZR(JNORM-1+3*(N-1)+2)
     &             + ZR(JNORM-1+3*(N-1)+3)*ZR(JNORM-1+3*(N-1)+3)
            VNORM = SQRT(VNORM)
            IF (VNORM.LT.1.0D-2) THEN
               CALL JENUNO(JEXNUM(NOMA//'.NOMNOE',INO),NOMNOE)
               CALL U2MESK('F','MODELISA3_30',1,NOMNOE)
            ENDIF
            ZR(JNORM-1+3*(N-1)+1)=ZR(JNORM-1+3*(N-1)+1)/VNORM
            ZR(JNORM-1+3*(N-1)+2)=ZR(JNORM-1+3*(N-1)+2)/VNORM
            ZR(JNORM-1+3*(N-1)+3)=ZR(JNORM-1+3*(N-1)+3)/VNORM
            DO 8 I = 1, NOCC
               IJ = IJ + 1
               COSVEC =  ZR(JNORM-1+3*(N-1)+1)*ZR(IDOBJ2-1+3*(IJ-1)+1)
     &                 + ZR(JNORM-1+3*(N-1)+2)*ZR(IDOBJ2-1+3*(IJ-1)+2)
     &                 + ZR(JNORM-1+3*(N-1)+3)*ZR(IDOBJ2-1+3*(IJ-1)+3)
               CALL PROVEC(ZR(JNORM-1+3*(N-1)+1),
     &                     ZR(IDOBJ2-1+3*(IJ-1)+1),PVEC)
               SINVEC = PVEC(1)*PVEC(1) + PVEC(2)*PVEC(2) +
     &                  PVEC(3)*PVEC(3)
               SINVEC = SQRT(SINVEC)
               ANGL = R8RDDG()*ATAN2(SINVEC,COSVEC)
               IF (ABS(ANGL).GT.10.0D0) THEN
                  CALL JENUNO(JEXNUM(NOMA//'.NOMNOE',INO),NOMNOE)
                  CALL CODREE(ABS(ANGL),'G',KANGL)
                   VALK(1) = NOMNOE
                   VALK(2) = KANGL
                   CALL U2MESK('A','MODELISA3_31', 2 ,VALK)
               ENDIF
  8         CONTINUE
         END IF
    2 CONTINUE
C
      CALL JEDETR(NOMOB2)
      CALL JEDETR(CONINV)
      CALL JEDEMA()
      END
