      SUBROUTINE OP0105 ( IER )
      IMPLICIT REAL*8 (A-H,O-Z)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SOUSTRUC  DATE 11/03/2003   AUTEUR DURAND C.DURAND 
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
C     OPERATEUR: ASSE_MAILLAGE
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
      CHARACTER*8  KBID, MAG
      CHARACTER*8  NOMA,NONO,NOSMA,DM(2),NOGMA,NOGNO,NOMACR
      CHARACTER*16 KBI1,KBI2
      LOGICAL      LOK
      REAL*8       X,Y,Z,DREFE,DIJ
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFMAJ()
C
      CALL GETRES( MAG, KBI1, KBI2 )
      CALL GETVID(' ','MAILLAGE',1,1,2,DM,N1)
C
C     --OBJET .TITR:
C     ---------------
      CALL WKVECT(MAG//'           .TITR','G V K80',2,IBID)
      ZK80(IBID)=' MAILLAGE OBTENU PAR CONCATENATION DES MAILLAGES : '
      ZK80(IBID+1)='  '//DM(1)//' ET '//DM(2)
C
C
C     --OBJET .DIME :
C     ---------------
      CALL JEVEUO(DM(1)//'.DIME','L',IADIM1)
      CALL JEVEUO(DM(2)//'.DIME','L',IADIM2)
      CALL WKVECT(MAG//'.DIME','G V I',6,IADIME)
      DO 11,I=1,5
        ZI(IADIME-1+I)=ZI(IADIM1-1+I)+ZI(IADIM2-1+I)
 11   CONTINUE
      IF(ZI(IADIM1-1+6).NE.ZI(IADIM2-1+6)) CALL UTMESS('F','OP0105',
     +      'LES 2 MAILLAGES DOIVENT ETRE DU MEME TYPE : 2D (OU 3D).')
C
      NCOOR=ZI(IADIM1-1+6)
      ZI(IADIME-1+6)=NCOOR
C
      NBSMA=ZI(IADIME-1+4)
      NBSM1=ZI(IADIM1-1+4)
      NBSM2=ZI(IADIM2-1+4)
C
      NBMA=ZI(IADIME-1+3)
      NBM1=ZI(IADIM1-1+3)
      NBM2=ZI(IADIM2-1+3)
C
      NBNO=ZI(IADIME-1+1)
      NBN1=ZI(IADIM1-1+1)
      NBN2=ZI(IADIM2-1+1)
C
      NBL1=ZI(IADIM1-1+2)
C
C
C     --OBJET .NOMACR :
C     -----------------
      IF (NBSMA.GT.0) THEN
        CALL WKVECT(MAG//'.NOMACR','G V K8',NBSMA,IANMCR)
        IF (NBSM1.GT.0) CALL JEVEUO(DM(1)//'.NOMACR','L',IANMC1)
        IF (NBSM2.GT.0) CALL JEVEUO(DM(2)//'.NOMACR','L',IANMC2)
        DO 12,I=1,NBSM1
          ZK8(IANMCR-1+I)=ZK8(IANMC1-1+I)
 12     CONTINUE
        DO 13,I=1,NBSM2
          ZK8(IANMCR-1+NBSM1+I)=ZK8(IANMC2-1+I)
 13     CONTINUE
      END IF
C
C
C     --OBJET .DIME_2 (V):
C     -----------------
      IF (NBSMA.GT.0) THEN
        CALL WKVECT(MAG//'.DIME_2','V V I',4*NBSMA,IADIMP)
        I1NOE= 0
        I1NOL= 0
        DO 14,I=1,NBSMA
          NOMACR=ZK8(IANMCR-1+I)
          CALL JEVEUO(NOMACR//'.DESM','L',IADESM)
          NBNOE= ZI(IADESM-1+2)
          NBNOL= ZI(IADESM-1+8)+ZI(IADESM-1+9)
          ZI(IADIMP-1+4*(I-1)+1)=NBNOE
          ZI(IADIMP-1+4*(I-1)+2)=NBNOL
          ZI(IADIMP-1+4*(I-1)+3)=I1NOE
          ZI(IADIMP-1+4*(I-1)+4)=I1NOL
          I1NOE= I1NOE+NBNOE
          I1NOL= I1NOL+NBNOL
 14     CONTINUE
      END IF
C
C
C     --OBJET .PARA_R :
C     -----------------
      IF (NBSMA.GT.0) THEN
        CALL WKVECT(MAG//'.PARA_R','G V R',14*NBSMA,IAPARR)
        IF (NBSM1.GT.0) CALL JEVEUO(DM(1)//'.PARA_R','L',IAPAR1)
        IF (NBSM2.GT.0) CALL JEVEUO(DM(2)//'.PARA_R','L',IAPAR2)
        DO 16,I=1,14*NBSM1
          ZR(IAPARR-1+I)=ZR(IAPAR1-1+I)
 16     CONTINUE
        DO 17,I=1,14*NBSM2
          ZR(IAPARR-1+NBSM1+I)=ZR(IAPAR2-1+I)
 17     CONTINUE
      END IF
C
C
C     --OBJET .SUPMAIL:
C     -----------------
      IF (NBSMA.GT.0) THEN
        CALL JECREC(MAG//'.SUPMAIL','G V I','NO','DISPERSE',
     +            'VARIABLE',NBSMA)
        DO 18,I=1,NBSM1
          CALL JEVEUO(JEXNUM(DM(1)//'.SUPMAIL',I),'L',IASUP1)
          CALL JELIRA(JEXNUM(DM(1)//'.SUPMAIL',I),'LONMAX',N,KBID)
          CALL JENUNO(JEXNUM(DM(1)//'.SUPMAIL',I),NOSMA)
          CALL JECROC(JEXNOM(MAG//'.SUPMAIL',NOSMA))
          CALL JEECRA(JEXNUM(MAG//'.SUPMAIL',I),'LONMAX',N,KBID)
          CALL JEVEUO(JEXNUM(MAG//'.SUPMAIL',I),'E',IASUPM)
          DO 181,II=1,N
            IF (ZI(IASUP1-1+II).LE.NBN1) THEN
              ZI(IASUPM-1+II)=ZI(IASUP1-1+II)
            ELSE
              ZI(IASUPM-1+II)=ZI(IASUP1-1+II)+NBN2
            END IF
 181      CONTINUE
 18     CONTINUE
        DO 19,I=1,NBSM2
          I1= I+NBSM1
          CALL JEVEUO(JEXNUM(DM(2)//'.SUPMAIL',I),'L',IASUP2)
          CALL JELIRA(JEXNUM(DM(2)//'.SUPMAIL',I),'LONMAX',N,KBID)
          CALL JENUNO(JEXNUM(DM(2)//'.SUPMAIL',I),NOSMA)
          CALL JEEXIN(JEXNOM(MAG//'.SUPMAIL',NOSMA),IRET)
          IF (IRET.GT.0) CALL UTMESS('F','OP0105',
     +       'LA (SUPER)MAILLE : '//NOSMA//' EST EN DOUBLE.')
          CALL JECROC(JEXNOM(MAG//'.SUPMAIL',NOSMA))
          CALL JEECRA(JEXNUM(MAG//'.SUPMAIL',I1),'LONMAX',N,KBID)
          CALL JEVEUO(JEXNUM(MAG//'.SUPMAIL',I1),'E',IASUPM)
          DO 191,II=1,N
            IF (ZI(IASUP2-1+II).LE.NBN2) THEN
              ZI(IASUPM-1+II)=ZI(IASUP2-1+II)+NBN1
            ELSE
              ZI(IASUPM-1+II)=ZI(IASUP2-1+II)+NBN1+NBL1
            END IF
 191      CONTINUE
 19     CONTINUE
      END IF
C
C
C     --OBJET .NOMMAI:
C     ----------------
      ICO=0
      IF (NBMA.GT.0) THEN
        CALL JECREO(MAG//'.NOMMAI','G N K8')
        CALL JEECRA(MAG//'.NOMMAI','NOMMAX',NBMA,KBID)
        DO 21,I=1,NBM1
          CALL JENUNO(JEXNUM(DM(1)//'.NOMMAI',I),NOMA)
          CALL JECROC(JEXNOM(MAG//'.NOMMAI',NOMA))
          ICO=ICO+1
 21     CONTINUE
        DO 22,I=1,NBM2
          CALL JENUNO(JEXNUM(DM(2)//'.NOMMAI',I),NOMA)
          IF (ICO.EQ.0) THEN
            IRET=0
          ELSE
            CALL JENONU(JEXNOM(MAG//'.NOMMAI',NOMA),IRET)
          END IF
          IF (IRET.GT.0) CALL UTMESS('F','OP0105',
     +       'LA MAILLE : '//NOMA//' EST EN DOUBLE.')
          CALL JECROC(JEXNOM(MAG//'.NOMMAI',NOMA))
 22     CONTINUE
      END IF
C
C
C     --OBJET .CONNEX:
C     -----------------
      IF (NBMA.GT.0) THEN
        CALL JECREC(MAG//'.CONNEX','G V I','NU'
     +            ,'CONTIG','VARIABLE',NBMA)
        L1=0
        L2=0
        IF (NBM1.GT.0) CALL JELIRA(DM(1)//'.CONNEX','LONT',L1,KBID)
        IF (NBM2.GT.0) CALL JELIRA(DM(2)//'.CONNEX','LONT',L2,KBID)
        L3= L1+L2
        CALL JEECRA(MAG//'.CONNEX','LONT',L3,KBID)
        DO 25,I=1,NBM1
          CALL JEVEUO(JEXNUM(DM(1)//'.CONNEX',I),'L',IACON1)
          CALL JELIRA(JEXNUM(DM(1)//'.CONNEX',I),'LONMAX',N,KBID)
          CALL JEECRA(JEXNUM(MAG//'.CONNEX',I),'LONMAX',N,KBID)
          CALL JEVEUO(JEXNUM(MAG//'.CONNEX',I),'E',IACONX)
          DO 251,II=1,N
            ZI(IACONX-1+II)=ZI(IACON1-1+II)
 251      CONTINUE
 25     CONTINUE
        DO 26,I=1,NBM2
          I1= I+NBM1
          CALL JEVEUO(JEXNUM(DM(2)//'.CONNEX',I),'L',IACON2)
          CALL JELIRA(JEXNUM(DM(2)//'.CONNEX',I),'LONMAX',N,KBID)
          CALL JEECRA(JEXNUM(MAG//'.CONNEX',I1),'LONMAX',N,KBID)
          CALL JEVEUO(JEXNUM(MAG//'.CONNEX',I1),'E',IACONX)
          DO 261,II=1,N
            ZI(IACONX-1+II)=ZI(IACON2-1+II)+NBN1
 261      CONTINUE
 26     CONTINUE
      END IF
C
C
C     --OBJET .TYPMAIL:
C     -----------------
      IF (NBMA.GT.0) THEN
        CALL WKVECT(MAG//'.TYPMAIL','G V I',NBMA,IBID)
        DO 27,I=1,NBM1
          CALL JEVEUO(DM(1)//'.TYPMAIL','L',IATYMA)
          IATYP1=IATYMA-1+I
          CALL JEVEUO(MAG//'.TYPMAIL','E',IATYMA)
          IATYPX=IATYMA-1+I
          ZI(IATYPX)=ZI(IATYP1)
 27     CONTINUE
        DO 28,I=1,NBM2
          I1=I+NBM1
          CALL JEVEUO(DM(2)//'.TYPMAIL','L',IATYMA)
          IATYP2=IATYMA-1+I
          CALL JEVEUO(MAG//'.TYPMAIL','E',IATYMA)
          IATYPX=IATYMA-1+I1
          ZI(IATYPX)=ZI(IATYP2)
 28     CONTINUE
      END IF
C
C
C     --OBJET .GROUPEMA:
C     -----------------
      CALL JEEXIN(DM(1)//'.GROUPEMA',IRET1)
      CALL JEEXIN(DM(2)//'.GROUPEMA',IRET2)
      NBGM1 = 0
      NBGM2 = 0
      IF (IRET1.GT.0) CALL JELIRA(DM(1)//'.GROUPEMA','NUTIOC',
     +                            NBGM1,KBID)
      IF (IRET2.GT.0) CALL JELIRA(DM(2)//'.GROUPEMA','NUTIOC',
     +                            NBGM2,KBID)
      NBGMA = NBGM1 + NBGM2
      IF ( NBGMA .GT. 0 ) THEN
        CALL JECREC(MAG//'.GROUPEMA','G V I','NO',
     +                               'DISPERSE','VARIABLE',NBGMA)
        DO 31,I=1,NBGM1
          CALL JEVEUO(JEXNUM(DM(1)//'.GROUPEMA',I),'L',IAGMA1)
          CALL JELIRA(JEXNUM(DM(1)//'.GROUPEMA',I),'LONMAX',N,KBID)
          CALL JENUNO(JEXNUM(DM(1)//'.GROUPEMA',I),NOGMA)
          CALL JECROC(JEXNOM(MAG//'.GROUPEMA',NOGMA))
          CALL JEECRA(JEXNUM(MAG//'.GROUPEMA',I),'LONMAX',N,KBID)
          CALL JEVEUO(JEXNUM(MAG//'.GROUPEMA',I),'E',IAGMAX)
          DO 311, II=1,N
            ZI(IAGMAX-1+II)=ZI(IAGMA1-1+II)
 311      CONTINUE
 31     CONTINUE
        DO 32,I=1,NBGM2
          CALL JEVEUO(JEXNUM(DM(2)//'.GROUPEMA',I),'L',IAGMA2)
          CALL JELIRA(JEXNUM(DM(2)//'.GROUPEMA',I),'LONMAX',N,KBID)
          CALL JENUNO(JEXNUM(DM(2)//'.GROUPEMA',I),NOGMA)
          CALL JEEXIN(JEXNOM(MAG//'.GROUPEMA',NOGMA),IRET)
          IF (IRET.GT.0) THEN
            CALL UTMESS('A','OP0105',
     +           'LE GROUP_MA : '//NOGMA//' EST EN DOUBLE.'
     +           //' ON IGNORE LE SECOND.')
            GO TO 32
          END IF
          I1 = I + NBGM1
          CALL JECROC(JEXNOM(MAG//'.GROUPEMA',NOGMA))
          CALL JEECRA(JEXNUM(MAG//'.GROUPEMA',I1),'LONMAX',N,KBID)
          CALL JEVEUO(JEXNUM(MAG//'.GROUPEMA',I1),'E',IAGMAX)
          DO 321, II=1,N
            ZI(IAGMAX-1+II)=ZI(IAGMA2-1+II)+NBM1
 321      CONTINUE
 32     CONTINUE
      END IF
C
C
C     --OBJET .GROUPENO:
C     -----------------
      CALL JEEXIN(DM(1)//'.GROUPENO',IRET1)
      CALL JEEXIN(DM(2)//'.GROUPENO',IRET2)
      NBGN1 = 0
      NBGN2 = 0
      IF (IRET1.GT.0) CALL JELIRA(DM(1)//'.GROUPENO','NUTIOC',
     +                            NBGN1,KBID)
      IF (IRET2.GT.0) CALL JELIRA(DM(2)//'.GROUPENO','NUTIOC',
     +                            NBGN2,KBID)
      NBGNO = NBGN1 + NBGN2
      IF ( NBGNO .GT. 0 ) THEN
        CALL JECREC(MAG//'.GROUPENO','G V I','NO',
     +                               'DISPERSE','VARIABLE',NBGNO)
        DO 33,I=1,NBGN1
          CALL JEVEUO(JEXNUM(DM(1)//'.GROUPENO',I),'L',IAGNO1)
          CALL JELIRA(JEXNUM(DM(1)//'.GROUPENO',I),'LONMAX',N,KBID)
          CALL JENUNO(JEXNUM(DM(1)//'.GROUPENO',I),NOGMA)
          CALL JECROC(JEXNOM(MAG//'.GROUPENO',NOGMA))
          CALL JEECRA(JEXNUM(MAG//'.GROUPENO',I),'LONMAX',N,KBID)
          CALL JEVEUO(JEXNUM(MAG//'.GROUPENO',I),'E',IAGNOX)
          DO 331, II=1,N
            ZI(IAGNOX-1+II)=ZI(IAGNO1-1+II)
 331      CONTINUE
 33     CONTINUE
        DO 34,I=1,NBGN2
          CALL JEVEUO(JEXNUM(DM(2)//'.GROUPENO',I),'L',IAGNO2)
          CALL JELIRA(JEXNUM(DM(2)//'.GROUPENO',I),'LONMAX',N,KBID)
          CALL JENUNO(JEXNUM(DM(2)//'.GROUPENO',I),NOGNO)
          CALL JEEXIN(JEXNOM(MAG//'.GROUPENO',NOGNO),IRET)
          IF (IRET.GT.0) THEN
            CALL UTMESS('A','OP0105',
     +           'LE GROUP_NO : '//NOGNO//' EST EN DOUBLE.'
     +           //' ON IGNORE LE SECOND.')
            GO TO 34
          END IF
          I1=I+NBGN1
          CALL JECROC(JEXNOM(MAG//'.GROUPENO',NOGNO))
          CALL JEECRA(JEXNUM(MAG//'.GROUPENO',I1),'LONMAX',N,KBID)
          CALL JEVEUO(JEXNUM(MAG//'.GROUPENO',I1),'E',IAGNOX)
          DO 341, II=1,N
            ZI(IAGNOX-1+II)=ZI(IAGNO2-1+II)+NBN1
 341      CONTINUE
 34     CONTINUE
      END IF
C
C
C     --OBJET .COORDO_2 (V):
C     ----------------------
      CALL WKVECT(MAG//'.COORDO_2','V V R',3*NBNO,IACOO2)
      CALL JEVEUO(DM(1)//'.COORDO    .VALE','L',IACOOR)
      DO 41, I=1,3*NBN1
        ZR(IACOO2-1+I)=ZR(IACOOR-1+I)
 41   CONTINUE
      CALL JEVEUO(DM(2)//'.COORDO    .VALE','L',IACOOR)
      DO 42, I=1,3*NBN2
        ZR(IACOO2-1+3*NBN1+I)=ZR(IACOOR-1+I)
 42   CONTINUE
C
C
C     --OBJET .NOEUD_CONF ET .NOMNOE_2 (V):
C     -------------------------------------
      CALL WKVECT(MAG//'.NOEUD_CONF','V V I',NBNO,IANCNF)
      CALL WKVECT(MAG//'.NOMNOE_2','V V K8',NBNO,IANON2)
      DO 43, I=1,NBNO
        ZI(IANCNF-1+I)=I
 43   CONTINUE
      DO 44, I=1,NBN1
        CALL JENUNO(JEXNUM(DM(1)//'.NOMNOE',I),NONO)
        ZK8(IANON2-1+I)=NONO
 44   CONTINUE
      DO 45, I=1,NBN2
        CALL JENUNO(JEXNUM(DM(2)//'.NOMNOE',I),NONO)
        ZK8(IANON2-1+NBN1+I)=NONO
        CALL JENONU(JEXNOM(DM(1)//'.NOMNOE',NONO),ITROU)
        IF (ITROU.GT.0) THEN
          ZI(IANCNF-1+NBN1+I)=ITROU
        END IF
 45   CONTINUE
C
C
C     --ON VERIFIE QUE LES NOEUDS CONFONDUS NE SONT PAS TROP DISTANTS:
C     ----------------------------------------------------------------
      DREFE=0.0D0
      DO 51, I=1,NBNO
        X=ZR(IACOO2-1+3*(I-1)+1)-ZR(IACOO2-1+1)
        Y=ZR(IACOO2-1+3*(I-1)+2)-ZR(IACOO2-1+2)
        Z=ZR(IACOO2-1+3*(I-1)+3)-ZR(IACOO2-1+3)
        DREFE= MAX(DREFE,SQRT(X**2+Y**2+Z**2))
 51   CONTINUE
      DO 52, I=1,NBNO
        J=ZI(IANCNF-1+I)
        IF (J.NE.I) THEN
          X=ZR(IACOO2-1+3*(I-1)+1)-ZR(IACOO2-1+3*(J-1)+1)
          Y=ZR(IACOO2-1+3*(I-1)+2)-ZR(IACOO2-1+3*(J-1)+2)
          Z=ZR(IACOO2-1+3*(I-1)+3)-ZR(IACOO2-1+3*(J-1)+3)
          DIJ= SQRT(X**2+Y**2+Z**2)
          IF (DIJ.GT.1.0D-6*DREFE) CALL UTMESS('A','OP0105','LE NOEUD:'
     +        //ZK8(IANON2-1+I)//' N''A PAS LES MEMES COORDONNEES'
     +        //' DANS LES MAILLAGES: '//DM(1)//' ET '//DM(2))
        END IF
 52   CONTINUE
C
C
C     --ON "TERMINE" LE MAILLAGE:
C     ---------------------------
      CALL SSDMTE(MAG)
C
      CALL CARGEO ( MAG )
C
C
C     --"MENAGE":
C     -----------
      CALL JEDETC('V',MAG,1)
      CALL JEDETC('V','&&OP0105',1)
C
      CALL JEDEMA()
      END
