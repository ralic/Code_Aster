      SUBROUTINE OP0118 ( IER )
      IMPLICIT   NONE
      INTEGER             IER
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 14/05/2002   AUTEUR DURAND C.DURAND 
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
C    GENERATION D'UN VECTEUR DE FONCTIONS ALEATOIRES
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
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      INTEGER       IBID, NVAL, NPFFT, DIM, DIM2, IP2, L, NALEA, NBTIR,
     +              IFM , NIV, NBPAR, NBVAL, LVALE, LDESC, NBFREQ, K,
     +              NBFC, LONG, LN, LN2, LONV, LVALF, LVALC, LR, LV, LX,
     +              LY, LN4, KF, LFO, LPROF, LN42, IF, IFO, KT, IT,
     +              IX, IY, LNR, KK, IFFT, KV 
      INTEGER LNUOR
      PARAMETER   ( NBPAR = 2 )
      REAL*8        PUI2, PUI, PUI2D, PUI3D, FREINI, FREFIN, DFREQ, TT,
     +              DT, R8B, TINI, TFIN, DSEED
      COMPLEX*16    C16B
      LOGICAL       LVAL
      CHARACTER*8   K8B, TYPAR(2), NOMVEC
      CHARACTER*16  TYPVEC, NOMCMD, NOPAR(2)
      CHARACTER*19  NOMINF, NOMFON
C
      DATA NOPAR / 'NUME_ORDRE' , 'FONCTION' /
      DATA TYPAR / 'I'          , 'K24'      /
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
      CALL GETRES ( NOMVEC, TYPVEC, NOMCMD )
      CALL GETVIS ( ' ', 'NB_POIN'  , 0,1,0, IBID , NVAL)
      LVAL = NVAL .NE. 0
C
      IF ( LVAL ) THEN
        CALL GETVIS ( ' ', 'NB_POIN', 0,1,1, NPFFT, L )
        PUI2  = LOG(DBLE(NPFFT))/LOG(2.D0)
        PUI   = AINT( PUI2 )
        PUI2D = ABS( PUI2 - PUI )
        PUI3D = ABS( 1.D0 - PUI2D )
        IF (PUI2D.GE.1.D-06 .AND. PUI3D.GE.1.D-06) THEN
          CALL UTMESS('F',NOMCMD,
     +                   'LE "NB_POIN" DOIT ETRE UNE PUISSANCE DE 2')
        END IF
      END IF
C
      CALL INFMAJ
      CALL INFNIV ( IFM , NIV )
C 
      CALL GETVIS ( ' ', 'INIT_ALEA'     , 0,1,1, NALEA , L )
      CALL GETVIS ( ' ', 'NB_TIRAGE'     , 0,1,1, NBTIR , L )
      CALL GETVID ( ' ', 'INTE_SPEC_FACT', 0,1,1, NOMINF, L )
C 
      CALL JELIRA ( NOMINF//'.VALE' , 'LONUTI', NBVAL, K8B )
      CALL JEVEUO ( NOMINF//'.VALE' , 'L', LVALE )
      CALL JEVEUO ( NOMINF//'.DESC' , 'L', LDESC )
      CALL JEVEUO ( NOMINF//'.NUOR' , 'L', LNUOR )
      NBFREQ = ZI(LDESC)
      DIM    = ZI(LDESC+1)
      NBFC   = ZI(LDESC+2)
C                        => NBFC = (DIM*(DIM+1))/2
      LONG = NBFREQ + NBFC*2*NBFREQ
      IF ( LONG .NE. NBVAL ) THEN
         CALL UTMESS('F',NOMCMD,
     +                  'MAUVAISE DEFINITION DE L''INTERSPECTRE.')
      END IF
C
      IF ( LVAL ) THEN
         CALL GETVIS ( ' ', 'NB_POIN', 0,1,1, NPFFT, L )
      ELSE
         PUI2 = LOG(DBLE(NBFREQ))/LOG(2.D0)
         IP2  = INT(PUI2) + 1
         NPFFT = 2**IP2
         IF ( NPFFT .GT. NBFREQ ) THEN
            IP2 = IP2 - 1
            NPFFT = 2**IP2
         END IF
      END IF
C
      LN     = NPFFT
      LN2    = LN*2
      DIM2   = DIM*DIM
      LONV   = LN2*DIM
      FREINI = ZR(LVALE)
      FREFIN = ZR(LVALE+LN-1)
C     ERREUR DFREQ   = (FREQFIN - FREQINI)/(LN-1)
      DFREQ = (FREFIN-FREINI) / LN
      TT    = 1.D0 / DFREQ
C     ERREUR DT = TT/(LN2-1)
      DT    = TT / LN2
C
      CALL WKVECT ( '&&OP0118.TEMP.VALF', 'V V R', LONV, LVALF )
      CALL WKVECT ( '&&OP0118.TEMP.VALC', 'V V C', LN2 , LVALC )
      CALL WKVECT ( '&&OP0118.TEMP.VALR', 'V V C', DIM2, LR    )
      CALL WKVECT ( '&&OP0118.TEMP.VALV', 'V V C', DIM , LV    )
      CALL WKVECT ( '&&OP0118.TEMP.VALX', 'V V C', DIM , LX    )
      CALL WKVECT ( '&&OP0118.TEMP.VALY', 'V V I', DIM , LY    )
C
C     --- CREATION DE L'OBJET NOMVEC//'.TABL' ET REMPLISSAGE ---
C
      CALL TBCRSD ( NOMVEC, 'G' )
      CALL TBAJPA ( NOMVEC, NBPAR, NOPAR, TYPAR )
C
C     --- CREATION DES FONCTIONS ---
      LN4 = LN2*2*NBTIR
      DO 60 KF = 1 , DIM
         WRITE (NOMFON,'(A8,A3,I4.4)') NOMVEC, '.FO', KF
C
         CALL TBAJLI ( NOMVEC, NBPAR, NOPAR, 
     +             ZI(LNUOR-1+KF), R8B, C16B, NOMFON, 0 )
C
         CALL WKVECT ( NOMFON//'.VALE', 'G V R', LN4, LFO   )
         CALL WKVECT ( NOMFON//'.PROL', 'G V K8', 5 , LPROF )
         ZI(LY+KF-1)  = LFO
         ZK8(LPROF  ) = 'FONCTION'
         ZK8(LPROF+1) = 'LIN LIN '
         ZK8(LPROF+2) = 'INST    '
         ZK8(LPROF+3) = 'TOUTRESU'
         ZK8(LPROF+4) = 'EC      '
   60 CONTINUE
C
      LN42 = LN4/2
      TINI = 0.D0
      TFIN = ((LN2*NBTIR)-1)*DT
      DO 80 IF = 1,DIM
         IFO = ZI(LY+IF-1)
         DO 30 KT = 1,LN42
            ZR(IFO+KT-1) = DT* (KT-1)
   30    CONTINUE
   80 CONTINUE
C
      DSEED = DBLE( NALEA )
      DO 70 IT = 1,NBTIR
C
         CALL GENALE ( ZR(LVALE), ZR(LVALF), ZC(LR), ZC(LV), ZC(LX),
     +                 DIM, LONG, LONV, LN, TT, DSEED )
C
         DO 20 KF = 1,DIM
            DO 40 K = 1,LN
               IX = LVALF + (K-1) + (KF-1)*LN2
               IY = IX + LN
               ZC(LVALC+K-1) = DCMPLX(ZR(IX),ZR(IY))
               IF ( K .NE. 1 ) THEN
                  LNR = LN2 - K + 1
                  ZC(LVALC+LNR) = DCONJG(ZC(LVALC+K-1))
               ELSE
                  ZC(LVALC+LN)  = DCMPLX(0.D0,0.D0)
                  ZC(LVALC+K-1) = DCMPLX(0.D0,0.D0)
               END IF
   40       CONTINUE
            DO 998 KK = 1,LN2
               ZC(LVALC+KK-1) = ZC(LVALC+KK-1)*SQRT(LN2/DT)
  998       CONTINUE
C
            IFFT = -1
            CALL FFT ( ZC(LVALC), LN2, IFFT )
C
            IFO = ZI(LY+KF-1) + LN2* (IT-1) + LN42
            DO 50 KV = 1,LN2
               ZR(IFO+KV-1) = DBLE(ZC(LVALC+KV-1))
   50       CONTINUE
   20    CONTINUE
   70 CONTINUE
C
      CALL JEDETR ( '&&OP0118.TEMP.VALF' )
      CALL JEDETR ( '&&OP0118.TEMP.VALR' )
      CALL JEDETR ( '&&OP0118.TEMP.VALV' )
      CALL JEDETR ( '&&OP0118.TEMP.VALX' )
      CALL JEDETR ( '&&OP0118.TEMP.VALY' )
      CALL JEDETR ( '&&OP0118.TEMP.VALC' )
      CALL JEDETR ( '&&OP0118.VECT.TABL' )
C
      IF ( NIV .GE. 2 ) THEN
         WRITE (IFM,200)
         WRITE (IFM,210) DT, TINI, TFIN + DT
      END IF
C
 200  FORMAT ('<-PAS DE TEMPS->   <-TEMPS INITIAL->  <-TEMPS FINAL->')
 210  FORMAT (1P,2X,D11.4,8X,D11.4,8X,D11.4)
C
      CALL JEDEMA()
      END
