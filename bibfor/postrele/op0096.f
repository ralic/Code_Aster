      SUBROUTINE OP0096 ( IER )
      IMPLICIT   NONE
      INTEGER             IER
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 06/07/2009   AUTEUR COURTOIS M.COURTOIS 
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
C TOLE CRP_20
C     ------------------------------------------------------------------
C     OPERATEUR DE REPERAGE DANS UN MAILLAGE 3D
C     MAILLAGE 3D <=> MAILLES DE TYPE HEXA TETRA PENTA
C     REPERAGE DE SEGMENT DE DROITE PAR RAPPORT AUX HEXA TETRA ET PENTA
C     ------------------------------------------------------------------
C     ------------------------------------------------------------------
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
      CHARACTER*32     JEXNUM, JEXNOM, JEXATR
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      INTEGER      TETRA,PENTA,HEXA,I,J,N,M,L,LONG,IFM,INFO,J1,J2
      INTEGER VALI
      INTEGER      ASDS1,ASDS2,ASDS3,ASDS4,ASDS5,ASDS6,ASDS7,ASDS8
      INTEGER      ASDS9,ASDS10,ASDS11,ASDS12,ASDS13,ASDS14
      INTEGER      ATMP1,ATMP2,ATMP3,ATMP4,ATMP5,ATMP6,ATMP7,ATMP8
      INTEGER      ATMP9,ATMP10,ATMP13,ATMP14
      INTEGER      NBSGT,NBSGEL,ISGT,NBTMA,NBNMA,NBN,TETE,QUEUE,NIL
      INTEGER      ASUCC,APREC,ADESC,AXYZM,AXYZN,ACONEC,ADRVLC,ADRMC
      INTEGER      ASDS,ADESCM,AINDIR,CPSGT,NNBM,INN,IATYMA
      INTEGER      IUNIFI,K,IM1,IF1,IAO1,IAE1,IM2,IF2,IAO2
      INTEGER      JNUMA,IMA,N1,N2,NDIM, IRET, NBPAR, IBID
      REAL*8       EPSI,ZERO,SGT(6),RBI,XA,YA,ZA,XB,YB,ZB, R8B
      REAL*8       VALR(6)
      REAL*8       NORM, SGTU(6), T, ABSCO, ABSCE, PREC, ARMIN
      COMPLEX*16   C16B
      CHARACTER*1  K1BID
      CHARACTER*4  CNUM
      CHARACTER*8  K8B, SURFAC, NOMAIL, TYPM, NOMM1, NOMM2,
     &             NNMAIL(7), TYPMCL(2)
      CHARACTER*16 OPERA,TYPRES,MOTCLE(3)
      CHARACTER*19 NOMT19
      CHARACTER*24 DESCM, NSDS, SD1TMP, SD2TMP
      CHARACTER*24 VALK(2)
      CHARACTER*24 TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,TEMP6,TEMP7,TEMP8
      CHARACTER*24 TEMP9,TEMP10,TEMP13,TEMP14
      CHARACTER*24 NSDS1,NSDS2,NSDS3,NSDS4,NSDS5,NSDS6,NSDS7
      CHARACTER*24 NSDS8,NSDS9,NSDS10,NSDS11,NSDS12,NSDS13,NSDS14
      CHARACTER*24 NOMMAI, LISMAI, PARA
      LOGICAL      COUPE, FINI, SWAP, EGFAC, I3EGFA
C
C===================================================================
C
      CALL JEMARQ()
      CALL INFMAJ()
C
      IFM   =  IUNIFI('MESSAGE')
      ZERO  =  0.0D0
      NIL   = -1
      TETRA =  1
      PENTA =  2
      HEXA  =  3
      CPSGT =  0
      NDIM  = 3
C
      CALL GETRES ( SURFAC, TYPRES, OPERA )
      CALL GETVR8 ( ' ', 'PRECISION', 1,1,1, EPSI  ,N)
      CALL GETVIS ( ' ', 'INFO'     , 1,1,1, INFO  ,N)
      CALL GETVID ( ' ', 'MAILLAGE' , 1,1,1, NOMAIL,N)
      CALL GETFAC ( 'DEFI_SEGMENT' , NBSGT )
C
      CALL DISMOI('F','NB_MA_MAILLA',NOMAIL,'MAILLAGE',NBTMA,K1BID,N)
      CALL DISMOI('F','NB_NO_MAILLA',NOMAIL,'MAILLAGE',NBNMA,K1BID,N)
      CALL JEVEUO(NOMAIL//'.COORDO    .VALE', 'L', AXYZM  )
      CALL JEVEUO(NOMAIL//'.CONNEX         ', 'L', ACONEC )
      CALL JEVEUO(NOMAIL//'.TYPMAIL        ', 'L', IATYMA )
      CALL JEVEUO(JEXATR(NOMAIL//'.CONNEX','LONCUM'),'L',ADRVLC)
      NOMMAI = NOMAIL//'.NOMMAI         '
C
      CALL WKVECT(SURFAC//'.NOMA','G V K8',1,N)
      ZK8(N) = NOMAIL
C
      CALL WKVECT ('&&OP0096.MAILLE.CHP.SUCC','V V I',NBTMA,  ASUCC)
      CALL WKVECT ('&&OP0096.MAILLE.CHP.PREC','V V I',NBTMA,  APREC)
      CALL WKVECT ('&&OP0096.MAILLE.CHP.DESC','V V I',NBTMA,  ADESC)
      CALL WKVECT ('&&OP0096.NEW.COORDO.VALE','V V R',3*NBNMA,AXYZN)
C
      CALL WKVECT('&&OP0096.NSDS','V V K24',NBSGT,ASDS)
      SD1TMP = '&&OP0096.R_1D'
      SD2TMP = '&&OP0096.R_OM'
      DESCM  = '&&OP0096.PTR.DESC.TYP.MA'
      CALL I3CRDM ( DESCM )
      CALL JEVEUO ( DESCM, 'L', ADESCM )
C
C     --- TRAITEMENT DES GROUP_MA ET MAILLE ---
C
      LISMAI = '&&OP0096.NUME_MAIL'
C
      CALL GETVTX ( ' ', 'GROUP_MA', 1,1,0, K8B, N1 )
      CALL GETVTX ( ' ', 'MAILLE'  , 1,1,0, K8B, N2 )
C
      IF ( (N1+N2) .EQ. 0 ) THEN
         CALL WKVECT ( LISMAI, 'V V I', NBTMA, JNUMA )
         DO 400, I = 1, NBTMA, 1
            ZI(JNUMA+I-1) = I
 400     CONTINUE
      ELSE
         MOTCLE(1) = 'GROUP_MA'
         MOTCLE(2) = 'MAILLE'
         TYPMCL(1) = 'GROUP_MA'
         TYPMCL(2) = 'MAILLE'
         CALL RELIEM(' ',NOMAIL,'NU_MAILLE',' ',1,2,MOTCLE, TYPMCL,
     &                                                  LISMAI, NBTMA )
         CALL JEVEUO ( LISMAI, 'L', JNUMA )
      ENDIF
C
C --- RECUPERATION DE L'ARETE MINIMUM DU MAILLAGE
C
      CALL JEEXIN ( NOMAIL//'           .LTNT', IRET )
      IF ( IRET .NE. 0 ) THEN
         CALL LTNOTB ( NOMAIL , 'CARA_GEOM' , NOMT19 )
         NBPAR = 0
         PARA = 'AR_MIN                  '
         CALL TBLIVA (NOMT19, NBPAR, ' ', IBID, R8B, C16B, K8B,
     &                K8B, R8B , PARA, K8B, IBID, ARMIN, C16B,
     &                K8B, IRET )
          IF ( IRET .NE. 0 ) CALL U2MESK('F','INTEMAIL_32',1,PARA)
         PREC = ARMIN*1.D-06
      ELSE
         PREC = 1.D-10
      ENDIF
C
      DO 100, ISGT = 1, NBSGT, 1
C
         MOTCLE(1) = 'ORIGINE'
         MOTCLE(2) = 'NOEUD_ORIG'
         MOTCLE(3) = 'GROUP_NO_ORIG'
         CALL UTCONO('DEFI_SEGMENT',MOTCLE,ISGT,NOMAIL,NDIM,SGTU(1),N1)
C
         MOTCLE(1) = 'EXTREMITE'
         MOTCLE(2) = 'NOEUD_EXTR'
         MOTCLE(3) = 'GROUP_NO_EXTR'
         CALL UTCONO('DEFI_SEGMENT',MOTCLE,ISGT,NOMAIL,NDIM,SGTU(4),N1)
C
         NORM = ZERO
         K    = 0
         DO 10, J = 1, 3, 1
            IF ( ABS(SGTU(J)-SGTU(3+J)) .GT. NORM ) THEN
               NORM = ABS( SGTU(J) - SGTU(3+J) )
               K    = J
            ENDIF
 10      CONTINUE
         IF ( NORM .LE. EPSI*SGTU(K) ) THEN
            VALK(1) = 'DEFI_SEGMENT'
            CALL U2MESG('F', 'INTEMAIL_27',1,VALK,1,ISGT,1,EPSI)
         ENDIF
C
         DO 110, N = 1, NBNMA, 1
            ZR(AXYZN + 3*(N-1)+1-1) = ZERO
            ZR(AXYZN + 3*(N-1)+2-1) = ZERO
            ZR(AXYZN + 3*(N-1)+3-1) = ZERO
 110     CONTINUE
         CALL I3CHGR ( SGTU, SGTU(4), ZR(AXYZM), ZR(AXYZN), NBNMA )
         RBI = ZERO
         DO 111, N = 1, 3, 1
            RBI = RBI + (SGTU(N+3)-SGTU(N))*(SGTU(N+3)-SGTU(N))
 111     CONTINUE
         RBI = SQRT( RBI )
         SGT(1) = ZERO
         SGT(2) = ZERO
         SGT(3) = ZERO
         SGT(4) = ZERO
         SGT(5) = ZERO
         SGT(6) = RBI
         DO 112, N = 1, NBTMA, 1
            ZI(ASUCC + N-1) = 0
            ZI(APREC + N-1) = 0
            ZI(ADESC + N-1) = 0
 112     CONTINUE
         TETE  = NIL
         QUEUE = NIL
C
         DO 120, M = 1, NBTMA, 1
            IMA = ZI(JNUMA+M-1)
            CALL JEVEUO (JEXNUM(NOMAIL//'.CONNEX',IMA),'L',ADRMC)
            CALL JELIRA (JEXNUM(NOMAIL//'.CONNEX',IMA),'LONMAX',NBN,K8B)
            CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ZI(IATYMA-1+IMA)),TYPM)
            CALL JENUNO(JEXNUM(NOMMAI,IMA),NOMM1)

            IF ( TYPM(1:5) .EQ. 'TETRA' .OR.
     &           TYPM(1:5) .EQ. 'PENTA' .OR.
     &           TYPM(1:4) .EQ. 'HEXA'  ) THEN
              COUPE = .FALSE.
              CALL I3CTPV ( EPSI,ZI(ADRMC),NBN,ZR(AXYZN),SGT,COUPE)
              IF ( COUPE ) THEN
                 IF ( TYPM(1:5) .EQ. 'TETRA' ) THEN
                    CALL I3LCHI ( NIL, TETE, QUEUE, IMA, TETRA,
     &                                 ZI(ADESC), ZI(ASUCC), ZI(APREC))
                 ELSE IF ( TYPM(1:5) .EQ. 'PENTA' ) THEN
                    CALL I3LCHI ( NIL, TETE, QUEUE, IMA, PENTA,
     &                                 ZI(ADESC), ZI(ASUCC), ZI(APREC))
                 ELSE
                    CALL I3LCHI ( NIL, TETE, QUEUE, IMA, HEXA,
     &                                 ZI(ADESC), ZI(ASUCC), ZI(APREC))
                 ENDIF
              ENDIF
            ENDIF
 120     CONTINUE
C
         CALL I3IMAS ( EPSI,NIL,TETE,QUEUE,ZI(ASUCC),ZI(APREC),
     &                 ZI(ADESC),ZI(ADESCM),SGT,ZI(ACONEC),ZI(ADRVLC),
     &                 ZR(AXYZN),SD1TMP,SD2TMP,NBSGEL)
C
         TEMP1  = SD1TMP(1:13)//'.SGTEL.ORIG'
         TEMP2  = SD1TMP(1:13)//'.SGTEL.EXTR'
         TEMP3  = SD1TMP(1:13)//'.SGTEL.TYPE'
         TEMP4  = SD2TMP(1:13)//'.MAIL      '
         TEMP5  = SD2TMP(1:13)//'.FACE .ORIG'
         TEMP6  = SD2TMP(1:13)//'.FACE .EXTR'
         TEMP7  = SD2TMP(1:13)//'.CREFM.ORIG'
         TEMP8  = SD2TMP(1:13)//'.CREFM.EXTR'
         TEMP9  = SD2TMP(1:13)//'.ARETE.ORIG'
         TEMP10 = SD2TMP(1:13)//'.ARETE.EXTR'
         TEMP13 = SD2TMP(1:13)//'.CREFF.ORIG'
         TEMP14 = SD2TMP(1:13)//'.CREFF.EXTR'
         CALL JEVEUO(TEMP1 ,'L',ATMP1 )
         CALL JEVEUO(TEMP2 ,'L',ATMP2 )
         CALL JEVEUO(TEMP3 ,'L',ATMP3 )
         CALL JEVEUO(TEMP5 ,'L',ATMP5 )
         CALL JEVEUO(TEMP6 ,'L',ATMP6 )
         CALL JEVEUO(TEMP7 ,'L',ATMP7 )
         CALL JEVEUO(TEMP8 ,'L',ATMP8 )
         CALL JEVEUO(TEMP9 ,'L',ATMP9 )
         CALL JEVEUO(TEMP10,'L',ATMP10)
         CALL JEVEUO(TEMP13,'L',ATMP13)
         CALL JEVEUO(TEMP14,'L',ATMP14)
C
         CALL WKVECT('&&OP0096.VEC.IND.AUX','V V I',NBSGEL+1,AINDIR)
         N    = 0
         M    = 0
         LONG = 0
         DO 130, I = 1, NBSGEL, 1
            IF ( ZI(ATMP3 + I-1) .GT. 0 ) THEN
               N = N + 1
               M = M + 1
               CALL JELIRA(JEXNUM(TEMP4,I),'LONMAX',L,K1BID)
               LONG = LONG + L
               ZI(AINDIR + M-1) = I
            ENDIF
 130     CONTINUE
         DO 135, I = 2, N, 1
            J    = ZI(AINDIR + I-1)
            T    = ZR(ATMP1 + J-1)
            FINI = .FALSE.
            SWAP = .FALSE.
            L    = 1
 136        CONTINUE
            IF ( .NOT. FINI ) THEN
               M = ZI(AINDIR + L-1)
               IF ( ZR(ATMP1 + M-1) .LE. T ) THEN
                  L = L + 1
               ELSE
                  SWAP = .TRUE.
                  FINI = .TRUE.
               ENDIF
               FINI = ( FINI .OR. (L .EQ. I) )
               GOTO 136
            ENDIF
            IF ( SWAP ) THEN
               DO 137, M = I, L+1, -1
                  ZI(AINDIR + M-1) = ZI(AINDIR + M-2)
 137           CONTINUE
               ZI(AINDIR + L-1) = J
            ENDIF
 135     CONTINUE
C
         IF ( N .LE. 0 ) THEN
            CALL CODENT(ISGT,'G',CNUM)
            VALK(1) = CNUM
            VALK(2) = NOMAIL
            VALR (1) = SGTU(1)
            VALR (2) = SGTU(2)
            VALR (3) = SGTU(3)
            VALR (4) = SGTU(4)
            VALR (5) = SGTU(5)
            VALR (6) = SGTU(6)
            CALL U2MESG('A', 'INTEMAIL_28',2,VALK,0,0,6,VALR)
         ELSE
            CPSGT = CPSGT + 1
            CALL CODENT(CPSGT,'G',CNUM)
            NSDS        = SURFAC//'S'//CNUM
            NSDS(14:24) = ' '
            ZK24(ASDS + CPSGT-1) = NSDS
            CALL WKVECT(NSDS(1:13)//'.DESC','G V R',6,M)
            DO 105, I = 1, 6, 1
               ZR(M + I-1) = SGTU(I)
 105        CONTINUE
            NSDS1  = NSDS  (1:13)//'.SGTEL.ORIG'
            NSDS2  = NSDS  (1:13)//'.SGTEL.EXTR'
            NSDS3  = NSDS  (1:13)//'.SGTEL.TYPE'
            NSDS4  = NSDS  (1:13)//'.MAIL      '
            NSDS5  = NSDS  (1:13)//'.FACE .ORIG'
            NSDS6  = NSDS  (1:13)//'.FACE .EXTR'
            NSDS7  = NSDS  (1:13)//'.CREFM.ORIG'
            NSDS8  = NSDS  (1:13)//'.CREFM.EXTR'
            NSDS13 = NSDS  (1:13)//'.CREFF.ORIG'
            NSDS14 = NSDS  (1:13)//'.CREFF.EXTR'
            NSDS9  = NSDS  (1:13)//'.ARETE.ORIG'
            NSDS10 = NSDS  (1:13)//'.ARETE.EXTR'
            NSDS11 = NSDS  (1:13)//'.CONEX.ORIG'
            NSDS12 = NSDS  (1:13)//'.CONEX.EXTR'
            CALL WKVECT ( NSDS1 ,'G V R',  N,ASDS1 )
            CALL WKVECT ( NSDS2 ,'G V R',  N,ASDS2 )
            CALL WKVECT ( NSDS3 ,'G V I',  N,ASDS3 )
            CALL WKVECT ( NSDS5 ,'G V I',  N,ASDS5 )
            CALL WKVECT ( NSDS6 ,'G V I',  N,ASDS6 )
            CALL WKVECT ( NSDS7 ,'G V R',3*N,ASDS7 )
            CALL WKVECT ( NSDS8 ,'G V R',3*N,ASDS8 )
            CALL WKVECT ( NSDS13,'G V R',2*N,ASDS13)
            CALL WKVECT ( NSDS14,'G V R',2*N,ASDS14)
            CALL WKVECT ( NSDS9 ,'G V I',  N,ASDS9 )
            CALL WKVECT ( NSDS10,'G V I',  N,ASDS10)
            CALL JECREC ( NSDS4 ,'G V I','NU','CONTIG','VARIABLE',N)
            CALL JEECRA ( NSDS4 ,'LONT',LONG,' ')
            DO 140, I = 1, N, 1
               M = ZI(AINDIR + I-1)
               ZR(ASDS1  +      I-1) = ZR(ATMP1  + M-1)
               ZR(ASDS2  +      I-1) = ZR(ATMP2  + M-1)
               ZI(ASDS3  +      I-1) = ZI(ATMP3  + M-1)
               ZI(ASDS5  +      I-1) = ZI(ATMP5  + M-1)
               ZI(ASDS6  +      I-1) = ZI(ATMP6  + M-1)
               ZR(ASDS7  + 3*(I-1)+1-1) = ZR(ATMP7  + 3*(M-1)+1-1)
               ZR(ASDS7  + 3*(I-1)+2-1) = ZR(ATMP7  + 3*(M-1)+2-1)
               ZR(ASDS7  + 3*(I-1)+3-1) = ZR(ATMP7  + 3*(M-1)+3-1)
               ZR(ASDS8  + 3*(I-1)+1-1) = ZR(ATMP8  + 3*(M-1)+1-1)
               ZR(ASDS8  + 3*(I-1)+2-1) = ZR(ATMP8  + 3*(M-1)+2-1)
               ZR(ASDS8  + 3*(I-1)+3-1) = ZR(ATMP8  + 3*(M-1)+3-1)
               ZR(ASDS13 + 2*(I-1)+1-1) = ZR(ATMP13 + 2*(M-1)+1-1)
               ZR(ASDS13 + 2*(I-1)+2-1) = ZR(ATMP13 + 2*(M-1)+2-1)
               ZR(ASDS14 + 2*(I-1)+1-1) = ZR(ATMP14 + 2*(M-1)+1-1)
               ZR(ASDS14 + 2*(I-1)+2-1) = ZR(ATMP14 + 2*(M-1)+2-1)
               ZI(ASDS9  +      I-1) = ZI(ATMP9  + M-1)
               ZI(ASDS10 +      I-1) = ZI(ATMP10 + M-1)
               CALL JEVEUO(JEXNUM(TEMP4,M),'E',ATMP4)
               CALL JELIRA(JEXNUM(TEMP4,M),'LONMAX',L,K1BID)
               CALL JECROC(JEXNUM(NSDS4,I))
               CALL JEECRA(JEXNUM(NSDS4,I),'LONMAX',L,' ')
               CALL JEVEUO(JEXNUM(NSDS4,I),'E',ASDS4)
               DO 145, J = 1, L, 1
                  ZI(ASDS4 + J-1) = ZI(ATMP4 + J-1)
 145           CONTINUE
 140        CONTINUE
C
C           --- DETERMINATION DU CMP_CNX ---
            M = 1
            ZI(AINDIR) = 1
            I = 1
            CALL JEVEUO(JEXNUM(NSDS4,1),'L',ASDS4)
            IM1 = ZI(ASDS4)
            IF1 = ZI(ASDS6)
            IAO1 = ZI(ASDS9)
            IAE1 = ZI(ASDS10)
            ABSCE = ZR(ASDS2)
            DO 150, I = 2, N, 1
               CALL JEVEUO(JEXNUM(NSDS4,I),'L',ASDS4)
               IM2  = ZI(ASDS4)
               IF2  = ZI(ASDS5+I-1)
               IAO2 = ZI(ASDS9+I-1)
               ABSCO = ZR(ASDS1+I-1)
               EGFAC = I3EGFA( ZI(ADESC), ZI(ADESCM), ZI(ACONEC),
     &                ZI(ADRVLC), IM1, IF1, IAO1, IAE1, IM2, IF2, IAO2 )
               IF ( .NOT. EGFAC ) THEN
                  IF ( ABS(ABSCE-ABSCO) .LE. PREC )  EGFAC = .TRUE.
               ENDIF
               IF ( .NOT. EGFAC ) THEN
                  M = M + 1
                  ZI(AINDIR + M-1) = I
               ENDIF
               IAO1 = IAO2
               IAE1 = ZI(ASDS10+I-1)
               IF1  = ZI(ASDS6+I-1)
               IM1  = IM2
               ABSCE = ZR(ASDS2+I-1)
 150        CONTINUE
            ZI(AINDIR + M) = N + 1
C
            CALL WKVECT ( NSDS11,'G V I',M,ASDS11)
            CALL WKVECT ( NSDS12,'G V I',M,ASDS12)
            DO 155, I = 1, M, 1
               ZI(ASDS11 + I-1) = ZI(AINDIR + I-1)
               ZI(ASDS12 + I-1) = ZI(AINDIR + I  ) - 1
 155        CONTINUE
            CALL JEVEUO(NSDS(1:13)//'.DESC','L',I)
            XA = ZR(I + 1-1)
            YA = ZR(I + 2-1)
            ZA = ZR(I + 3-1)
            XB = ZR(I + 4-1)
            YB = ZR(I + 5-1)
            ZB = ZR(I + 6-1)
C
C           --- ABSCISSES CURVILIGNES CROISSANTES ---
C
            ABSCE = ZR(ASDS2+ZI(ASDS11)-1)
            DO 160, I = 1, M, 1
               J1 = ZI(ASDS11 +  I-1)
               IF ( I .EQ. 1 ) J1 = J1 + 1
               J2 = ZI(ASDS12 +  I-1)
               DO 162, J = J1, J2, 1
                  ABSCO = ZR(ASDS1+J-1)
                  IF ( ABS(ABSCE-ABSCO) .GT. PREC ) THEN
                     CALL JEVEUO(JEXNUM(NSDS4,J),'L',ASDS4)
                     CALL JENUNO(JEXNUM(NOMMAI,ZI(ASDS4)),NOMM1)
                     CALL JEVEUO(JEXNUM(NSDS4,J-1),'L',ASDS4)
                     CALL JENUNO(JEXNUM(NOMMAI,ZI(ASDS4)),NOMM2)
                     IF ( ABSCE .GT. ABSCO ) THEN
                        VALK (1) = NOMM2
                        VALK (2) = NOMM1
                        CALL U2MESK('A', 'INTEMAIL_29',2,VALK)
                     ELSE
                        VALK (1) = NOMM2
                        VALK (2) = NOMM1
                        CALL U2MESK('A', 'INTEMAIL_30',2,VALK)
                     ENDIF
                  ENDIF
                  ABSCE = ZR(ASDS2+J-1)
 162           CONTINUE
 160        CONTINUE
C
            IF ( INFO .GE. 2 ) THEN
              WRITE(IFM,1002) ISGT
              CALL JELIRA(NSDS1 ,'LONMAX',N,K1BID)
              CALL JELIRA(NSDS11,'LONMAX',M,K1BID)
              WRITE(IFM,1004) N
              WRITE(IFM,1006) M
              WRITE(IFM,1008) XA, YA, ZA
              WRITE(IFM,1010) XB, YB, ZB
              WRITE(IFM,1012)
              DO 200, I = 1, M, 1
                J1 = ZI(ASDS11 +  I-1)
                J2 = ZI(ASDS12 +  I-1)
                DO 210, J = J1, J2, 1
                  CALL JEVEUO(JEXNUM(NSDS4,J),'L',ASDS4)
                  CALL JELIRA(JEXNUM(NSDS4,J),'LONMAX',L,K1BID)
                  CALL JENUNO(JEXNUM(NOMMAI,ZI(ASDS4)),NOMM1)
                  IF ( L .GE. 2 ) THEN
                    NNBM = MIN( 7 , L-1 )
                    DO 212 INN = 1 , NNBM
                      CALL JENUNO(JEXNUM(NOMMAI,ZI(ASDS4+INN)),
     &                                NNMAIL(INN) )
 212                CONTINUE
                    WRITE(IFM,1018)I,NOMM1,ZI(ASDS5+J-1),ZI(ASDS9+J-1),
     &                          ZR(ASDS1+J-1), (NNMAIL(K),K = 1,NNBM,1)
                  ELSE
                    WRITE(IFM,1014) I, NOMM1, ZI(ASDS5+J-1),
     &                              ZI(ASDS9+J-1), ZR(ASDS1+J-1)
                  ENDIF
                  WRITE(IFM,1016) ZI(ASDS6+J-1), ZI(ASDS10+J-1),
     &                            ZR(ASDS2+J-1)
 210            CONTINUE
 200          CONTINUE
            ENDIF
         ENDIF
         CALL JEDETR('&&OP0096.VEC.IND.AUX')
 100  CONTINUE
C
      IF ( CPSGT .LE. 0 ) THEN
         CALL U2MESS('F','INTEMAIL_11')
      ELSE
         CALL WKVECT(SURFAC//'.NSDS','G V K24',CPSGT,ATMP1)
         DO 300, ISGT = 1, CPSGT, 1
            ZK24(ATMP1 + ISGT-1) = ZK24(ASDS + ISGT-1)
 300     CONTINUE
      ENDIF
C
      CALL I3DRDM ( DESCM )
C
      CALL JEDEMA()
C
 1002 FORMAT(/,1X,'OCCURENCE : ',I2)
 1004 FORMAT(1X,'NB_SGT_ELEM = ',I2)
 1006 FORMAT(1X,'NB_CMP_CNX  = ',I2)
 1008 FORMAT(1X,'ORIGINE   =',3(1X,1PD12.5))
 1010 FORMAT(1X,'EXTREMITE =',3(1X,1PD12.5))
 1012 FORMAT(1X,' CMP_CNX  MAILLE   FACE  ARETE    ABSC_CURV')
 1014 FORMAT(1X,I7,2X,A8,4X,I1,5X,I1,4X,1PD12.5)
 1016 FORMAT(22X           ,I1,5X,I1,4X,1PD12.5)
 1018 FORMAT(1X,I7,2X,A8,4X,I1,5X,I1,4X,1PD12.5,3X,7(1X,A8))
C
      END
