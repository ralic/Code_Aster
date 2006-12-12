      SUBROUTINE ASMACO (MA1, MA2, MAG)
      IMPLICIT NONE
      CHARACTER*8        MA1, MA2, MAG
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 13/12/2006   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     OPERATEUR: ASSE_MAILLAGE / CAS DE L ASSEMBLAGE DE MAILLAGES
C     AVEC COLLAGE DE DEUX GROUPES DE MAILLES
C
C-----------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32     JEXNUM, JEXNOM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      CHARACTER*1  KKK
      CHARACTER*8  CGPM1, CGPM2
      CHARACTER*8  KIND, K8B
      CHARACTER*8  NOMA,NONO,NOGMA,NOGMAB,NOGNO,NOGNOB
      CHARACTER*19 COORDO,NOMT19
      CHARACTER*24 PARA,VALK(2)
      INTEGER      NBMA,NBM1,NBM2,NBNO,NBN1,NBN2,NBGMA,NBGM1,NBGM2
      INTEGER      NBNGM1,NBNGM2,NBNGM,NNO1,NNO2,IANODE,NNODIF
      INTEGER      I1,ICOMPT,INO,L1,L2,L3,I,N,NCOOR,K,IFM,NIV,J
      INTEGER      IADIM1,IADIM2,IADIME
      INTEGER      IAGMA1,IAGMA2,IAGMAX
      INTEGER      IACON1,IACON2,IACONX
      INTEGER      IAGNO1,IAGNO2,IAGNOX
      INTEGER      IATYP1,IATYP2,IATYPX
      INTEGER      NBGNO,NBGN1,NBGN2,II,JJ,IGEOMR,IADESC,IBID,IAREFE
      INTEGER      IATYMA,IACOO1,IACOO2,IAVALE,IRET,IRET1,IRET2
      INTEGER      LXLGUT,IAMAM1,IAMAM2,NBPAR
      LOGICAL      MATCH
      REAL*8       PREC1,PREC2,PREC,DIST,X1,Y1,Z1,X2,Y2,Z2,R8B,ARMIN
      COMPLEX*16   C16B
C
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFNIV(IFM,NIV)
CCC   ------------------------------------------------------------------
CCC RECUPERATION DE L'ARETE MINIMUM DES MAILLAGE
CCC   ------------------------------------------------------------------
      CALL JEEXIN ( MA1//'           .LTNT', IRET )
      IF ( IRET .NE. 0 ) THEN
         CALL LTNOTB ( MA1 , 'CARA_GEOM' , NOMT19 )
         NBPAR = 0
         PARA = 'AR_MIN                  '
         CALL TBLIVA (NOMT19, NBPAR, ' ', IBID, R8B, C16B, K8B,
     &                K8B, R8B , PARA, K8B, IBID, ARMIN, C16B,
     &                K8B, IRET )
          IF ( IRET .NE. 0 ) CALL U2MESS('F','MODELISA2_13')
         PREC1 = ARMIN*1.D-06
      ELSE
         PREC1 = 1.D-10
      ENDIF
      IF ( PREC1 .LE. 0.D0 ) CALL U2MESS('F','MODELISA2_14')
CCC   ------------------------------------------------------------------
      CALL JEEXIN ( MA2//'           .LTNT', IRET )
      IF ( IRET .NE. 0 ) THEN
         CALL LTNOTB ( MA2 , 'CARA_GEOM' , NOMT19 )
         NBPAR = 0
         PARA = 'AR_MIN                  '
         CALL TBLIVA (NOMT19, NBPAR, ' ', IBID, R8B, C16B, K8B,
     &                K8B, R8B , PARA, K8B, IBID, ARMIN, C16B,
     &                K8B, IRET )
          IF ( IRET .NE. 0 ) CALL U2MESS('F','MODELISA2_13')
         PREC2 = ARMIN*1.D-06
      ELSE
         PREC2 = 1.D-10
      ENDIF
      IF ( PREC2 .LE. 0.D0 ) CALL U2MESS('F','MODELISA2_14')
      PREC=MIN(PREC1,PREC2)
CCC   ------------------------------------------------------------------
CCC RECUPERATION DES 2 GROUP_MA A COLLER
CCC   ------------------------------------------------------------------
      CALL GETVTX('COLLAGE','GROUP_MA_1',1,1,1,CGPM1,IBID)
      CALL GETVTX('COLLAGE','GROUP_MA_2',1,1,1,CGPM2,IBID)
      CALL JEEXIN(JEXNOM(MA1//'.GROUPEMA',CGPM1),IRET1)
      IF(IRET1.EQ.0) THEN
        VALK(1) = CGPM1
        VALK(2) = MA1
        CALL U2MESK('F','MODELISA2_15', 2 ,VALK)
      ENDIF
      CALL JEEXIN(JEXNOM(MA2//'.GROUPEMA',CGPM2),IRET2)
      IF(IRET2.EQ.0) THEN
        VALK(1) = CGPM2
        VALK(2) = MA2
        CALL U2MESK('F','MODELISA2_16', 2 ,VALK)
      ENDIF
CCC   ------------------------------------------------------------------
CCC VERIFICATION QUE LES 2 GROUP_MA A COLLER ONT LE MM NOMBRE DE MAILLES
CCC   ------------------------------------------------------------------
      CALL JELIRA(JEXNOM(MA1//'.GROUPEMA',CGPM1),'LONMAX',NBNGM1,K8B)
      CALL JELIRA(JEXNOM(MA2//'.GROUPEMA',CGPM2),'LONMAX',NBNGM2,K8B)
      NBNGM=NBNGM1
      IF(NBNGM1.NE.NBNGM2) THEN
         VALK(1) = CGPM1
         VALK(2) = CGPM2
         CALL U2MESK('F','MODELISA2_17', 2 ,VALK)
      ENDIF
      CALL JEVEUO(JEXNOM(MA1//'.GROUPEMA',CGPM1),'L',IAGMA1)
      CALL JEVEUO(JEXNOM(MA2//'.GROUPEMA',CGPM2),'L',IAGMA2)
CCC   ------------------------------------------------------------------
CCC CREATION DU VECTEUR DES NOEUDS A APPARIER DANS CHACUN DES MAILLAGES
CCC VECTEUR '.NODE' :  - DE    1 A   NNO1 : NOEUDS DU MAILLAGE 2
CCC                    - DE NNO1 A 2*NNO1 : NOEUDS DU MAILLAGE 1
CCC   ------------------------------------------------------------------
      NNO1=0
      NNO2=0
      DO 1000 I=1,NBNGM
         CALL JELIRA(JEXNUM(MA1//'.CONNEX',ZI(IAGMA1+I-1)),'LONMAX',
     &               II,K8B)
         CALL JELIRA(JEXNUM(MA2//'.CONNEX',ZI(IAGMA2+I-1)),'LONMAX',
     &               JJ,K8B)
         NNO1=NNO1+II
         NNO2=NNO2+JJ
1000  CONTINUE
      IF(NNO1.NE.NNO2) CALL U2MESS('F','MODELISA2_18')
      CALL WKVECT('&&ASMACO'//'.NODE','V V I',NNO1*2  ,IANODE)
      NNO1=0
      DO 1010 I=1,NBNGM
         CALL JELIRA(JEXNUM(MA1//'.CONNEX',ZI(IAGMA1+I-1)),'LONMAX',
     &               II,K8B)
         CALL JEVEUO(JEXNUM(MA1//'.CONNEX',ZI(IAGMA1+I-1)),'L',IAGNO1)
         DO 1020 J=1,II
            NNO1=NNO1+1
            ZI(IANODE+NNO1-1)=ZI(IAGNO1+J-1)
1020     CONTINUE
1010  CONTINUE
      CALL TRI(ZI(IANODE),ZI,0,NNO1)
      NNODIF=1
      ZI(IANODE+NNO1)=ZI(IANODE)
      DO 1012 I=2,NNO1
         IF (ZI(IANODE+I-1).NE.ZI(IANODE+NNO1+NNODIF-1)) THEN
            NNODIF=NNODIF+1
            ZI(IANODE+NNO1+NNODIF-1)=ZI(IANODE+I-1)
         ENDIF
1012  CONTINUE
      NNO2=0
      CALL JEVEUO(MA1//'.COORDO    .VALE','L',IACOO1)
      CALL JEVEUO(MA2//'.COORDO    .VALE','L',IACOO2)

      DO 1050 K=1,NNODIF
         X1=ZR(IACOO1+3*(ZI(IANODE+NNO1+K-1)-1)-1+1)
         Y1=ZR(IACOO1+3*(ZI(IANODE+NNO1+K-1)-1)-1+2)
         Z1=ZR(IACOO1+3*(ZI(IANODE+NNO1+K-1)-1)-1+3)
         MATCH=.FALSE.
         DO 1030 I=1,NBNGM
           CALL JELIRA(JEXNUM(MA2//'.CONNEX',ZI(IAGMA2+I-1)),'LONMAX',
     &                 II,K8B)
           CALL JEVEUO(JEXNUM(MA2//'.CONNEX',ZI(IAGMA2+I-1)),'L',IAGNO2)
           DO 1040 J=1,II
              NNO2=NNO2+1
              X2=ZR(IACOO2+3*(ZI(IAGNO2+J-1)-1)-1+1)
              Y2=ZR(IACOO2+3*(ZI(IAGNO2+J-1)-1)-1+2)
              Z2=ZR(IACOO2+3*(ZI(IAGNO2+J-1)-1)-1+3)
              DIST=(X2-X1)**2+(Y2-Y1)**2+(Z2-Z1)**2
              IF (DIST.LE.PREC) THEN
                  MATCH=.TRUE.
                  GOTO 1060
              ENDIF
1040       CONTINUE
1030     CONTINUE
         IF(.NOT.MATCH) CALL U2MESS('F','MODELISA2_19')
1060     CONTINUE
         ZI(IANODE+K-1)=ZI(IAGNO2+J-1)
1050  CONTINUE
CCC   ------------------------------------------------------------------
CCC   --OBJET .DIME :
CCC   ------------------------------------------------------------------
      CALL JEVEUO(MA1//'.DIME','L',IADIM1)
      CALL JEVEUO(MA2//'.DIME','L',IADIM2)
      CALL WKVECT(MAG//'.DIME','G V I',6,IADIME)
CCC SOMME POUR : 1 LE NB DE NOEUDS - ON LAISSE LES NOEUDS EN DOUBLE
CCC                                  INUTILISES DANS LA CONNECTIVITE
CCC              2       DE NOEUDS LAGRANGES,
CCC              3       DE MAILLES - ON SOUSTRAIT LES MAILLES APPARIES
CCC                                   POUR LES 2 MAILLAGES
CCC              4       DE SUPER MAILLES
CCC              5       DU MAJORANT DE SUPER MAILLES
      ZI(IADIME-1+1)=ZI(IADIM1-1+1)+ZI(IADIM2-1+1)
      ZI(IADIME-1+2)=ZI(IADIM1-1+2)+ZI(IADIM2-1+2)
      ZI(IADIME-1+3)=ZI(IADIM1-1+3)+ZI(IADIM2-1+3)-2*NBNGM
      ZI(IADIME-1+4)=ZI(IADIM1-1+4)+ZI(IADIM2-1+4)
      ZI(IADIME-1+5)=ZI(IADIM1-1+5)+ZI(IADIM2-1+5)
C
      IF(ZI(IADIM1-1+6).NE.ZI(IADIM2-1+6)) CALL U2MESS('F','MODELISA2_20
     &')
C
      NCOOR=ZI(IADIM1-1+6)
      ZI(IADIME-1+6)=NCOOR
C
      NBMA=ZI(IADIME-1+3)
      NBM1=ZI(IADIM1-1+3)
      NBM2=ZI(IADIM2-1+3)
C
      NBNO=ZI(IADIME-1+1)
      NBN1=ZI(IADIM1-1+1)
      NBN2=ZI(IADIM2-1+1)
CCC   ------------------------------------------------------------------
CCC   --OBJET .NOMMAI:
CCC   ON SUPPRIME LES NBNGM MAILLES DU GROUP_MA A COLLER DANS LES 2
CCC   MAILLAGES
CCC   ------------------------------------------------------------------
      IF (NBMA.GT.0) THEN
        CALL JECREO(MAG//'.NOMMAI','G N K8')
        CALL JEECRA(MAG//'.NOMMAI','NOMMAX',NBMA,K8B)
        DO 21,I=1,NBM1-NBNGM
          CALL CODENT(I,'G',KIND)
          NOMA='M'//KIND
          CALL JECROC(JEXNOM(MAG//'.NOMMAI',NOMA))
 21     CONTINUE
        DO 22,I=1,NBM2-NBNGM
          CALL CODENT(NBM1-NBNGM+I,'G',KIND)
          NOMA='M'//KIND
          CALL JECROC(JEXNOM(MAG//'.NOMMAI',NOMA))
 22     CONTINUE
      END IF
CCC   ------------------------------------------------------------------
CCC   --OBJET .NOMNOE :
CCC   TOUS LES NOEUDS DES 2 MAILLAGES SONT CONSERVES, Y COMPRIS CEUX
CCC   DU MAILLAGE 2 REDONDANTS AVEC CEUX DU MAILLAGE 1
CCC   (AU NOMBRE DE NNODIF)
CCC   ------------------------------------------------------------------
      IF (NBNO.GT.0) THEN
        CALL JECREO(MAG//'.NOMNOE','G N K8')
        CALL JEECRA(MAG//'.NOMNOE','NOMMAX',NBNO,K8B)
        DO 23,I=1,NBN1
          CALL CODENT(I,'G',KIND)
          NONO='N'//KIND
          CALL JECROC(JEXNOM(MAG//'.NOMNOE',NONO))
 23     CONTINUE
        DO 24,I=1,NBN2
          CALL CODENT(NBN1+I,'G',KIND)
          NONO='N'//KIND
          CALL JECROC(JEXNOM(MAG//'.NOMNOE',NONO))
 24     CONTINUE
      END IF
CCC   ------------------------------------------------------------------
CCC   --OBJET .CONNEX :
CCC   ON NE RETIENT QUE LES MAILLES HORS DES 2 GROUP_MA CGPM1 ET CGPM2
CCC   POUR LES MAILLES DU MAILLAGE 2 CONTENANT DES NOEUDS DE CGPM2,
CCC   ON SUBSTITUE DANS LEUR CONNECTIVITE LES NOMS DE NOEUDS DU
CCC   MAILLAGE 1 QUI LEUR ONT ETE APPARIES
CCC   ------------------------------------------------------------------
      IF (NBMA.GT.0) THEN
        CALL JECREC(MAG//'.CONNEX','G V I','NU'
     &            ,'CONTIG','VARIABLE',NBMA)
        CALL WKVECT('&&ASMACO'//'.MAM1','V V I',NBM1*2 ,IAMAM1)
        CALL WKVECT('&&ASMACO'//'.MAM2','V V I',NBM2*2 ,IAMAM2)
        DO 31,I=1,NBM1
           ZI(IAMAM1+I-1)=I
 31     CONTINUE
        DO 32,I=1,NBM2
           ZI(IAMAM2+I-1)=I
 32     CONTINUE
        DO 33,I=1,NBNGM
           ZI(IAMAM1+ZI(IAGMA1+I-1)-1)=0
           ZI(IAMAM2+ZI(IAGMA2+I-1)-1)=0
 33     CONTINUE
        II=0
        DO 34,I=1,NBM1
           IF (ZI(IAMAM1+I-1).EQ.0) THEN
               ZI(IAMAM1+NBM1+I-1)=0
           ELSE
               II=II+1
               ZI(IAMAM1+NBM1+I-1)=II
           ENDIF
 34     CONTINUE
        II=0
        DO 35,I=1,NBM2
           IF (ZI(IAMAM2+I-1).EQ.0) THEN
               ZI(IAMAM2+NBM2+I-1)=0
           ELSE
               II=II+1
               ZI(IAMAM2+NBM2+I-1)=II
           ENDIF
 35     CONTINUE
        CALL TRI(ZI(IAMAM1),ZI,0,NBM1)
        CALL TRI(ZI(IAMAM2),ZI,0,NBM2)
        L1=0
        L2=0
        IF (NBM1.GT.0) CALL JELIRA(MA1//'.CONNEX','LONT',L1,K8B)
        IF (NBM2.GT.0) CALL JELIRA(MA2//'.CONNEX','LONT',L2,K8B)
        L3= L1+L2
        CALL JEECRA(MAG//'.CONNEX','LONT',L3,K8B)
        DO 41,I=1,NBM1-NBNGM
          CALL JEVEUO(JEXNUM(MA1//'.CONNEX',ZI(IAMAM1+NBNGM+I-1)),
     &                                     'L',IACON1)
          CALL JELIRA(JEXNUM(MA1//'.CONNEX',ZI(IAMAM1+NBNGM+I-1)),
     &                                     'LONMAX',N,K8B)
          CALL JEECRA(JEXNUM(MAG//'.CONNEX',I),'LONMAX',N,K8B)
          CALL JEVEUO(JEXNUM(MAG//'.CONNEX',I),'E',IACONX)
          DO 411,II=1,N
            ZI(IACONX-1+II)=ZI(IACON1-1+II)
 411      CONTINUE
 41     CONTINUE
        DO 42,I=1,NBM2-NBNGM
          I1= I+NBM1-NBNGM
          CALL JEVEUO(JEXNUM(MA2//'.CONNEX',ZI(IAMAM2+NBNGM+I-1)),
     &                                      'L',IACON2)
          CALL JELIRA(JEXNUM(MA2//'.CONNEX',ZI(IAMAM2+NBNGM+I-1)),
     &                                      'LONMAX',N,K8B)
          CALL JEECRA(JEXNUM(MAG//'.CONNEX',I1),'LONMAX',N,K8B)
          CALL JEVEUO(JEXNUM(MAG//'.CONNEX',I1),'E',IACONX)
          DO 421,II=1,N
            MATCH=.FALSE.
            DO 422,JJ=1,NNODIF
               IF (ZI(IACON2+II-1).EQ.ZI(IANODE+JJ-1)) THEN
                  MATCH=.TRUE.
                  GOTO 423
               ENDIF
 422        CONTINUE
 423        CONTINUE
            IF (MATCH) THEN
              ZI(IACONX+II-1)=ZI(IANODE+NNO1+JJ-1)
            ELSE
              ZI(IACONX+II-1)=ZI(IACON2+II-1)+NBN1
            ENDIF
 421      CONTINUE
 42     CONTINUE
      END IF
CCC   ------------------------------------------------------------------
CCC   --OBJET .COORDO :
CCC   ------------------------------------------------------------------
      COORDO= MAG//'.COORDO'
C
      CALL JENONU(JEXNOM('&CATA.GD.NOMGD','GEOM_R'),IGEOMR)
      CALL WKVECT(COORDO//'.DESC','G V I',3,IADESC)
      CALL JEECRA(COORDO//'.DESC','DOCU',IBID,'CHNO')
      ZI (IADESC-1+1)= IGEOMR
C     -- TOUJOURS 3 COMPOSANTES X, Y ET Z
      ZI (IADESC-1+2)= -3
C     -- 14 = 2**1 + 2**2 + 2**3
      ZI (IADESC-1+3)= 14
C
      CALL WKVECT(COORDO//'.REFE','G V K24',2,IAREFE)
      ZK24(IAREFE-1+1)= MAG
      CALL JEVEUO(MA1//'.COORDO    .VALE','L',IACOO1)
      CALL JEVEUO(MA2//'.COORDO    .VALE','L',IACOO2)
      CALL WKVECT(COORDO//'.VALE','G V R',3*NBNO,IAVALE)
C     -- COORDONNEES DES NOEUDS :
      DO 51 , INO=1, NBN1
        DO 511, K=1,3
          ZR(IAVALE-1+3*(INO-1)+K)=ZR(IACOO1-1+3*(INO-1)+K)
 511    CONTINUE
 51   CONTINUE
      DO 52 , INO=1, NBN2
        DO 521, K=1,3
          ZR(IAVALE-1+3*(NBN1+INO-1)+K)=ZR(IACOO2-1+3*(INO-1)+K)
 521    CONTINUE
 52   CONTINUE
CCC   ------------------------------------------------------------------
CCC   --OBJET .TYPMAIL :
CCC   ------------------------------------------------------------------
      IF (NBMA.GT.0) THEN
        CALL WKVECT(MAG//'.TYPMAIL','G V I',NBMA,IBID)
        DO 61,I=1,NBM1-NBNGM
          CALL JEVEUO(MA1//'.TYPMAIL','L',IATYMA)
          IATYP1=IATYMA-1+ZI(IAMAM1+NBNGM+I-1)
          CALL JEVEUO(MAG//'.TYPMAIL','E',IATYMA)
          IATYPX=IATYMA-1+I
          ZI(IATYPX)=ZI(IATYP1)
 61     CONTINUE
        DO 62,I=1,NBM2-NBNGM
          I1=I+NBM1-NBNGM
          CALL JEVEUO(MA2//'.TYPMAIL','L',IATYMA)
          IATYP2=IATYMA-1+ZI(IAMAM2+NBNGM+I-1)
          CALL JEVEUO(MAG//'.TYPMAIL','E',IATYMA)
          IATYPX=IATYMA-1+I1
          ZI(IATYPX)=ZI(IATYP2)
 62     CONTINUE
      END IF
CCC   ------------------------------------------------------------------
CCC   --OBJET .GROUPEMA:
CCC   ON RECREE TOUS LES GROUP_MA DANS LE NOUVEAU MAILLAGE, SAUF LES 2
CCC   QUI ON SERVI A REALISER LE COLLAGE. IL FAUDRAIT VERIFIER
CCC   (PAS FAIT) QU AUCUNE DES MAILLES SUPPRIMEES NE FIGURE DANS UN
CCC   AUTRE GROUP_MA QUE CELUI QUI A SERVI AU COLLAGE, ET DONC SUPPRIME
CCC   ------------------------------------------------------------------
      CALL JEEXIN(MA1//'.GROUPEMA',IRET1)
      CALL JEEXIN(MA2//'.GROUPEMA',IRET2)
      NBGM1 = 0
      NBGM2 = 0
      IF (IRET1.GT.0) CALL JELIRA(MA1//'.GROUPEMA','NUTIOC',NBGM1,K8B)
      IF (IRET2.GT.0) CALL JELIRA(MA2//'.GROUPEMA','NUTIOC',NBGM2,K8B)
      NBGMA = NBGM1 - 1 + NBGM2 - 1
      IF ( NBGMA .GT. 0 ) THEN
        CALL JECREC(MAG//'.GROUPEMA','G V I','NO',
     &                               'DISPERSE','VARIABLE',NBGMA)
        ICOMPT=0
        DO 71,I=1,NBGM1
          CALL JEVEUO(JEXNUM(MA1//'.GROUPEMA',I),'L',IAGMA1)
          CALL JELIRA(JEXNUM(MA1//'.GROUPEMA',I),'LONMAX',N,K8B)
          CALL JENUNO(JEXNUM(MA1//'.GROUPEMA',I),NOGMA)
          IF (NOGMA.NE.CGPM1) THEN
            ICOMPT=ICOMPT+1
            CALL JECROC(JEXNOM(MAG//'.GROUPEMA',NOGMA))
            CALL JEECRA(JEXNUM(MAG//'.GROUPEMA',ICOMPT),'LONMAX',N,K8B)
            CALL JEVEUO(JEXNUM(MAG//'.GROUPEMA',ICOMPT),'E',IAGMAX)
            DO 711, II=1,N
              ZI(IAGMAX-1+II)=ZI(IAMAM1+NBM1+ZI(IAGMA1-1+II)-1)
 711        CONTINUE
          ENDIF
 71     CONTINUE
        ICOMPT = 0
        DO 72,I=1,NBGM2
          CALL JEVEUO(JEXNUM(MA2//'.GROUPEMA',I),'L',IAGMA2)
          CALL JELIRA(JEXNUM(MA2//'.GROUPEMA',I),'LONMAX',N,K8B)
          CALL JENUNO(JEXNUM(MA2//'.GROUPEMA',I),NOGMA)
          IF (NOGMA.NE.CGPM2) THEN
            CALL JEEXIN(JEXNOM(MAG//'.GROUPEMA',NOGMA),IRET)
            IF (IRET.GT.0) THEN
              CALL U2MESK('A','MODELISA2_21',1,NOGMA)
              NOGMAB=NOGMA
              II = LXLGUT(NOGMAB(1:7))
              DO 724,K=II+1,7
                 NOGMAB(K:K)='_'
 724          CONTINUE
              DO 722,K=0,9
                 CALL CODENT(K,'G',KKK)
                 NOGMAB(8:8)=KKK
                 CALL JEEXIN(JEXNOM(MAG//'.GROUPEMA',NOGMAB),IRET)
                 IF (IRET.EQ.0) GOTO 723
 722          CONTINUE
 723          CONTINUE
              WRITE (IFM,*) ' LE GROUP_MA '//NOGMA//' DU MAILLAGE '
     &             //MA2//' EST RENOMME '//NOGMAB//' DANS '//MAG
              NOGMA=NOGMAB
            END IF
            ICOMPT = ICOMPT + 1
            I1 = NBGM1 -1 + ICOMPT
            CALL JECROC(JEXNOM(MAG//'.GROUPEMA',NOGMA))
            CALL JEECRA(JEXNUM(MAG//'.GROUPEMA',I1),'LONMAX',N,K8B)
            CALL JEVEUO(JEXNUM(MAG//'.GROUPEMA',I1),'E',IAGMAX)
            DO 721, II=1,N
              ZI(IAGMAX-1+II)=ZI(IAMAM2+NBM2+ZI(IAGMA2-1+II)-1)
     &                        +NBM1-NBNGM
 721        CONTINUE
          ENDIF
 72     CONTINUE
      END IF
CCC   ------------------------------------------------------------------
CCC   --OBJET .GROUPENO:
CCC   LES GROUP_NO SONT CONSERVES TELS QUELS, DANS LA MESURE OU ON NE
CCC   SUPPRIME PAS DE NOEUDS. POUR LES GROUP_NO DU MAILLAGE 2, SI DES
CCC   NOEUDS FONT PARTI DE CEUX APPARIES, ON LES SUBSTITUE PAR LEUR
CCC   HOMOLOGUE DU MAILLAGE 1, PLUTOT QUE DE LAISSER LE(S) NOEUD
CCC   DESORMAIS ORPHELIN DANS LE GROUPE.
CCC   ------------------------------------------------------------------
      CALL JEEXIN(MA1//'.GROUPENO',IRET1)
      CALL JEEXIN(MA2//'.GROUPENO',IRET2)
      NBGN1 = 0
      NBGN2 = 0
      IF (IRET1.GT.0) CALL JELIRA(MA1//'.GROUPENO','NUTIOC',
     &                            NBGN1,K8B)
      IF (IRET2.GT.0) CALL JELIRA(MA2//'.GROUPENO','NUTIOC',
     &                            NBGN2,K8B)
      NBGNO = NBGN1 + NBGN2
      IF ( NBGNO .GT. 0 ) THEN
        CALL JECREC(MAG//'.GROUPENO','G V I','NO',
     &                               'DISPERSE','VARIABLE',NBGNO)
        DO 81,I=1,NBGN1
          CALL JEVEUO(JEXNUM(MA1//'.GROUPENO',I),'L',IAGNO1)
          CALL JELIRA(JEXNUM(MA1//'.GROUPENO',I),'LONMAX',N,K8B)
          CALL JENUNO(JEXNUM(MA1//'.GROUPENO',I),NOGNO)
          CALL JECROC(JEXNOM(MAG//'.GROUPENO',NOGNO))
          CALL JEECRA(JEXNUM(MAG//'.GROUPENO',I),'LONMAX',N,K8B)
          CALL JEVEUO(JEXNUM(MAG//'.GROUPENO',I),'E',IAGNOX)
          DO 811, II=1,N
            ZI(IAGNOX-1+II)=ZI(IAGNO1-1+II)
 811      CONTINUE
 81     CONTINUE
        ICOMPT = 0
        DO 82,I=1,NBGN2
          CALL JEVEUO(JEXNUM(MA2//'.GROUPENO',I),'L',IAGNO2)
          CALL JELIRA(JEXNUM(MA2//'.GROUPENO',I),'LONMAX',N,K8B)
          CALL JENUNO(JEXNUM(MA2//'.GROUPENO',I),NOGNO)
          CALL JEEXIN(JEXNOM(MAG//'.GROUPENO',NOGNO),IRET)
          IF (IRET.GT.0) THEN
            CALL U2MESK('A','MODELISA2_22',1,NOGNO)
            NOGNOB=NOGNO
            II = LXLGUT(NOGNOB(1:7))
            DO 821,K=II+1,7
               NOGNOB(K:K)='_'
 821        CONTINUE
            DO 822,K=0,9
               CALL CODENT(K,'G',KKK)
               NOGNOB(8:8)=KKK
               CALL JEEXIN(JEXNOM(MAG//'.GROUPENO',NOGNOB),IRET)
               IF (IRET.EQ.0) GOTO 823
 822        CONTINUE
 823        CONTINUE
            WRITE (IFM,*) ' LE GROUP_NO '//NOGNO//' DU MAILLAGE '
     &           //MA2//' EST RENOMME '//NOGNOB//' DANS '//MAG
            NOGNO=NOGNOB
          END IF
          ICOMPT = ICOMPT + 1
          I1 = NBGN1 + ICOMPT
          CALL JECROC(JEXNOM(MAG//'.GROUPENO',NOGNO))
          CALL JEECRA(JEXNUM(MAG//'.GROUPENO',I1),'LONMAX',N,K8B)
          CALL JEVEUO(JEXNUM(MAG//'.GROUPENO',I1),'E',IAGNOX)
          DO 824, II=1,N
            MATCH=.FALSE.
            DO 825,JJ=1,NNODIF
               IF (ZI(IAGNO2+II-1).EQ.ZI(IANODE+JJ-1)) THEN
                  MATCH=.TRUE.
                  GOTO 826
               ENDIF
 825        CONTINUE
 826        CONTINUE
            IF (MATCH) THEN
              ZI(IAGNOX+II-1)=ZI(IANODE+NNO1+JJ-1)
            ELSE
              ZI(IAGNOX+II-1)=ZI(IAGNO2+II-1)+NBN1
            ENDIF
 824      CONTINUE
 82     CONTINUE
      END IF
C
9999  CONTINUE
      CALL JEDEMA()
      END
