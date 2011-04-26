      SUBROUTINE OP0120()
      IMPLICIT REAL*8 (A-H,O-Z)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
C     CALCUL D'UNE MATRICE INTERSPECTRALE
C
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
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      PARAMETER   ( NBPAR = 6 )
      INTEGER       LONG, IVAL(2), IER
      REAL*8        RESU, ZERO
      COMPLEX*16    C16B
      CHARACTER*8   K8B, NOMU, TYPAR(NBPAR)
      CHARACTER*16  CONCEP, NOMCMD, NOPAR(NBPAR), KVAL(2)
      CHARACTER*19  NOMFON, NOMCOD
      CHARACTER*24  CHVALE, CHPROL, NOMOBJ
C
      DATA NOPAR / 'NOM_CHAM' , 'OPTION' , 'DIMENSION' ,
     &             'NUME_ORDRE_I' , 'NUME_ORDRE_J' ,'FONCTION_C' /
      DATA TYPAR / 'K16' , 'K16' , 'I' , 'I' , 'I' , 'K24' /
C     ------------------------------------------------------------------
C
C     --- INITIALISATION DES DIVERS ---
      CALL JEMARQ()
C
      CALL GETRES ( NOMU, CONCEP, NOMCMD )
C
      CALL GETVR8 ( ' ','INST_INIT' ,0,1,1, TINST1 ,L   )
      CALL GETVR8 ( ' ','INST_FIN'  ,0,1,1, TINST2 ,L    )
      CALL GETVIS ( ' ','NB_POIN'   ,0,1,1, NBPTS  ,L    )
      CALL GETVID ( ' ','FONCTION'  ,0,1,0, K8B    ,NFONC)
      NFONC = ABS(NFONC)
C
C    --- VERIFICATION DU NOMBRE DE POINTS ---
      PTS = LOG(DBLE(NBPTS))/LOG(2.D0)
      PTS1 = AINT(PTS)
      PTS2 = ABS(PTS1-PTS)
      PTS3 = ABS(1.D0-PTS2)
      IF (PTS2.GE.1.D-06 .AND. PTS3.GE.1.D-06) THEN
        CALL U2MESS('F','ALGORITH9_56')
      END IF
C
      CALL INFMAJ
      CALL INFNIV ( IFM , NIV )
C
      CALL TBCRSD ( NOMU, 'G' )
      CALL TBAJPA ( NOMU, NBPAR, NOPAR, TYPAR )
C
      KVAL(1) = 'DSP'
      KVAL(2) = 'TOUT'
      CALL TBAJLI ( NOMU, 3, NOPAR, NFONC, R8B, C16B, KVAL, 0 )
C
      DURANA = TINST2 - TINST1
      CALL GETVR8 ( ' ', 'DUREE_ANALYSE' , 0,1,1, DURANA, NDA )
C
      DURDEC = DURANA
      CALL GETVR8 ( ' ', 'DUREE_DECALAGE', 0,1,1, DURDEC, NDD )
C
      IF ( NDA .NE. 0 ) THEN
        BMATR = ( (TINST2-TINST1) - DURANA ) / DURDEC
        NMATR = INT( ABS(BMATR) + 1 )
      ELSE
        NMATR = 1
      ENDIF
C
      CALL WKVECT('&&OP0120.TEMP.LFON','V V K8',NFONC,LFON  )
      CALL WKVECT('&&OP0120.TEMP.VALE','V V C' ,NBPTS,LVALE )
C
      CALL GETVID ( ' ', 'FONCTION', 0,1,NFONC, ZK8(LFON), L )
C
      DT     = DURANA / NBPTS
      LONG   = NBPTS * NFONC / 2
      NFCOD  = NFONC * ( NFONC+1 ) / 2
      LONG1  = NBPTS * NFCOD
      LONG2  = NMATR * NFCOD
      NBPTS2 = NBPTS / 2
      NBPTS3 = NBPTS2 * 3
      DFREQ  = 1.D0 / DURANA
CC
      CALL WKVECT('&&OP0120.TEMP.VALC','V V C',LONG,LVALC)
      CALL WKVECT('&&OP0120.TEMP.LINT','V V R',NBPTS,LINT)
      CALL WKVECT('&&OP0120.TEMP.LSSX','V V R',LONG1,LSSX)
      CALL WKVECT('&&OP0120.TEMP.LRMS','V V R',LONG2,LRMS)
CC
      DO 20 IMATR = 1,NMATR
        DO 30 KF = 1,NFONC
          NOMFON = ZK8(LFON+KF-1)
          DO 50 IT = 1,NBPTS
            TINST = TINST1 + (IMATR-1)* (DURDEC) + (IT-1)*DT
            CALL FOINTE('F ',NOMFON,1,'INST',TINST,RESU,IER)
            ZERO = 0.D0
            ZC(LVALE+IT-1) = DCMPLX(RESU,ZERO)
   50     CONTINUE
C       --- CALCUL DE LA TRANFORMEE DE FOURIER ---
          IFFT = 1
          CALL FFT(ZC(LVALE),NBPTS,IFFT)
C       ---
          DO 60 IT = 1,NBPTS2
            LRESU1 = LVALC + (KF-1)*NBPTS2 + (IT-1)
            ZC(LRESU1) = ZC(LVALE+IT-1)*DT
   60     CONTINUE
   30   CONTINUE
        LCOMP1 = 0
        DO 70 J = 1,NFONC
          DO 80 I = 1,J
C
            CALL CALINT(I,J,ZC(LVALC),NBPTS,ZR(LINT),LONG,DURANA)
C
            DO 90 KK = 1,NBPTS2
              L1 = LINT + KK - 1
              L2 = LSSX + KK - 1 + NBPTS*LCOMP1
              ZR(L2) = ZR(L2) + ZR(L1)
              ZR(L2+NBPTS2) = ZR(L2+NBPTS2) + ZR(L1+NBPTS2)
   90       CONTINUE
            LCOMP1 = LCOMP1 + 1
   80     CONTINUE
   70   CONTINUE
        CALL RMS(IMATR,ZR(LSSX),LONG1,ZR(LRMS),LONG2,NBPTS,NFCOD,DFREQ,
     &           NFONC)
   20 CONTINUE
      DO 110 KB = 1,LONG1
        LS1 = LSSX + KB - 1
        ZR(LS1) = ZR(LS1)/DBLE(NMATR)
  110 CONTINUE
C
C     --- CREATION DES NOMS DE FONCTIONS ---
      KTABL = 1
      DO 130 J = 1,NFONC
        IVAL(2) = J
C
        DO 140 I = 1,J
          IVAL(1) = I
C
          WRITE(NOMCOD,'(A8,A3,2I4.4)') NOMU,'.FO',I,J
C
          CALL TBAJLI ( NOMU, 3, NOPAR(4),
     &                        IVAL, R8B, C16B, NOMCOD, 0 )
C
C           --- CREATION DE L'OBJET NOMCOD//'.VALE' ---
C           --- CREATION DE L'OBJET NOMCOD//'.PROL' ---
          CHVALE = NOMCOD//'.VALE'
          CALL WKVECT ( CHVALE, 'G V R', NBPTS3, LCOD )
          CHPROL = NOMCOD//'.PROL'
          CALL WKVECT ( CHPROL, 'G V K24', 6, LPROL )
          ZK24(LPROL  ) = 'FONCT_C '
          ZK24(LPROL+1) = 'LIN LIN '
          ZK24(LPROL+2) = 'FREQ    '
          ZK24(LPROL+3) = 'DSP     '
          ZK24(LPROL+4) = 'EE      '
          ZK24(LPROL+5) = NOMCOD
C
          DO 150 K = 1,NBPTS2
            L1 = LCOD + NBPTS2 + (K-1)*2
            L2 = LSSX + NBPTS* (KTABL-1) + K - 1
            ZR(LCOD+K-1) = (K-1)*DFREQ
            ZR(L1) = ZR(L2)
            ZR(L1+1) = ZR(L2+NBPTS2)
  150     CONTINUE
          KTABL = KTABL + 1
  140   CONTINUE
  130 CONTINUE
C
      IF ( NIV .GE. 1 ) THEN
         FREINI = 0.D0
         FREFIN = DFREQ* (NBPTS2-1)
         WRITE (IFM,200)
         WRITE (IFM,201) DFREQ, FREINI, FREFIN
      ENDIF
      IF ( NIV .GE. 2 ) THEN
         NOMOBJ = '&&OP0117.FONCTION'
         CALL TBEXVE ( NOMU, 'FONCTION_C', NOMOBJ, 'V', NBF1, K8B )
         IF (NFCOD.NE.NBF1) CALL U2MESS('F','MODELISA2_89')
         CALL JEVEUO ( NOMOBJ, 'L', LTABL )
         CALL INTIMP ( IFM, ZR(LRMS), ZK24(LTABL), NMATR, NFCOD )
      ENDIF
C
C
      CALL TITRE
C
 200  FORMAT ('<PAS EN FREQUENCE>  <FREQ. INITIALE>  <FREQ. FINALE>')
 201  FORMAT (4X,D11.4,4X,D11.4,4X,D11.4)
C
      CALL JEDEMA()
      END
