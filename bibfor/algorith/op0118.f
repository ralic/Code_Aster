      SUBROUTINE OP0118()
C ----------------------------------------------------------------------
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
C
C    GENERATION D'UN VECTEUR DE FONCTIONS ALEATOIRES
C
C     ------------------------------------------------------------------
      IMPLICIT   NONE
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
      INTEGER         NPFFT, DIM, DIM2, L, NBTIR
      INTEGER       IFM , NIV, NBPAR, NBVAL, LVALE, LDESC, NBFREQ, K
      INTEGER       NBFC, LONG, LN, LN2, LONV, LVALF, LVALC, LR, LV
      INTEGER       LX, LY, LN4, KF, LFO, LPROF, IF, IFO, KT, IT
      INTEGER       IX, IY, LNR, KK, IFFT, KV
      INTEGER       JUMP

      INTEGER      LNUOR
      PARAMETER   ( NBPAR = 2 )

      REAL*8        PUI2, PUI2D, PUI3D, FREINI, FREFIN, DFREQ, TT,
     &              DT, R8B, TINI, TFIN, DUREE
      COMPLEX*16    C16B
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
      CALL INFMAJ
      CALL INFNIV ( IFM , NIV )
C
C
      CALL GETRES ( NOMVEC, TYPVEC, NOMCMD )

C
C===============
C 2. LECTURE DES DONNEES LIEES A LA GENERATION
C===============

      CALL GETVIS ( ' ', 'NB_TIRAGE'     , 0,1,1, NBTIR , L )
      IF (L .EQ. 0) NBTIR = 0

      CALL GETVR8(' ','DUREE_TIRAGE',0,1,1,DUREE,L)
      IF (L .EQ. 0) DUREE = -1.D0

      CALL GETVIS ( ' ', 'INIT_ALEA'    , 0,1,1, JUMP , L )
      IF (L .NE. 0) CALL INIRAN(JUMP)

C===============
C 3. LECTURE DES DONNEES LIEES A L'INTERSPECTRE, DISCRETISATION
C    DE L'INTERSPECTRE (=> CHOIX DES PARAMETRES DE LA GENERATION)
C    ET FACTORISATION DE L'INTERSPECTE
C===============
      CALL GEFACT (DUREE,NOMINF)

C
C===============
C 4. RECUPERATION DE L'INTERSPECTRE FACTORISE (NOMINF)
C===============
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
         CALL U2MESS('F','ALGORITH9_55')
      END IF

C===============
C 5. PREPARATION GENERATION
C===============
      PUI2  = LOG(DBLE(NBFREQ))/LOG(2.D0)
      PUI2D = ABS( PUI2 - AINT( PUI2 ))
      PUI3D = ABS( 1.D0 - PUI2D )
      IF (PUI2D.GE.1.D-06 .AND. PUI3D.GE.1.D-06) THEN
        NPFFT  = 2**INT(LOG(DBLE(NBFREQ))/LOG(2.D0))
      ELSE
        NPFFT  = NBFREQ
      ENDIF

      LN     = NPFFT
      LN2    = LN*2
      DIM2   = DIM*DIM
      LONV   = LN2*DIM
      FREINI = ZR(LVALE)
      FREFIN = ZR(LVALE+LN-1)

      DFREQ   = (FREFIN - FREINI)/(LN-1)
      TT    = 1.D0 / DFREQ
      DT    = TT / LN2
C C'EST BIEN TT/LN2 ET NON TT/(LN2-1) CAR LA GENERATION COMMENCE
C A T=DT ET NON T=0.
      TFIN = LN2*NBTIR*DT
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
C     --- CREATION DES FONCTIONS (VIDE)---
      LN4 = LN2*NBTIR+1

      DO 60 KF = 1 , DIM
         WRITE (NOMFON,'(A8,A3,I4.4)') NOMVEC, '.FO', KF
C
         CALL TBAJLI ( NOMVEC, NBPAR, NOPAR,
     &             ZI(LNUOR-1+KF), R8B, C16B, NOMFON, 0 )
C
         CALL WKVECT ( NOMFON//'.VALE', 'G V R', LN4*2, LFO   )
         CALL WKVECT ( NOMFON//'.PROL', 'G V K24', 6 , LPROF )
         ZI(LY+KF-1)  = LFO
         ZK24(LPROF  ) = 'FONCTION'
         ZK24(LPROF+1) = 'LIN LIN '
         ZK24(LPROF+2) = 'INST    '
         ZK24(LPROF+3) = 'TOUTRESU'
         ZK24(LPROF+4) = 'EC      '
         ZK24(LPROF+5) = NOMFON
   60 CONTINUE
C
      DO 80 IF = 1,DIM
         IFO = ZI(LY+IF-1)
         DO 30 KT = 1,LN4
            ZR(IFO+KT-1) = DT* (KT-1)
   30    CONTINUE
   80 CONTINUE
C


C===============
C 5.  GENERATION DES FONCTIONS
C===============
      DO 70 IT = 1,NBTIR
C
         CALL GENALE ( ZR(LVALE), ZR(LVALF), ZC(LR), ZC(LV), ZC(LX),
     &                 DIM, LONG, LONV, LN)
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
            IFO = ZI(LY+KF-1) + LN2* (IT-1) + LN4 + 1
            IF (IT.EQ.1) ZR(IFO-1)=0.D0
C                        (VALEUR NULLE POUR T=0.)
            DO 50 KV = 1,LN2
               ZR(IFO+KV-1) = DBLE(ZC(LVALC+KV-1))
   50       CONTINUE
   20    CONTINUE
   70 CONTINUE

C===============
C 6. IMPRESSION
C===============
C
      IF ( NIV .GE. 2 ) THEN
C LA GENERATION COMMENCE A T=DT MAIS ON METS LE SIGNAL COMMENCE
C A TINI=0. AVEC UNE VALEUR NULLE
         TINI = 0.D0
         WRITE (IFM,200)
         WRITE (IFM,210) DT, TINI, TFIN , NPFFT
      END IF
C
 200  FORMAT ('<-PAS DE TEMPS->   <-TEMPS INITIAL->  <-TEMPS FINAL->
     &  <-NB PT FFT->')
 210  FORMAT (1P,2X,D11.4,8X,D11.4,8X,D11.4,8X, I6)
C
      CALL JEDEMA()
      END
