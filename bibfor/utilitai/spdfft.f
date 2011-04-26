      SUBROUTINE SPDFFT(NSENS,NOMFON,NBVIN,NOMFS,NBVOUT,METHOD,SYM,BASE)
      IMPLICIT NONE
      CHARACTER*(1)                               BASE
      INTEGER             NSENS
C     ----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C     REALISATION N.GREFFET
C     CALCUL DE LA FFT OU DE LA FFT-1 (E. BOYERE 09/06/00)
C     ----------------------------------------------------------------
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
      CHARACTER*16  METHOD,SYM
      CHARACTER*19  NOMFS, NOMFON
      COMPLEX*16    DCMPLX
      REAL*8        PAS,PASFRQ
      INTEGER       NBVAL,NBVA,NBPTS,NBPTS1,NBPTS2,LTRA,LRES,LRES1,IER
      INTEGER       NIN,NBVIN,NBVOUT,LVAR,N,LFON,I,II,VALMAX
C     ----------------------------------------------------------------
      CALL JEMARQ()
C
C     ---  NOMBRE DE POINTS ----
      NBVAL = NBVIN
      CALL JEVEUO(NOMFON,'L',NIN)
      LVAR = NIN
      IF (NSENS.EQ.1) THEN
         NBVA = NBVAL/2
      ELSEIF (NSENS.EQ.-1) THEN
         NBVA = NBVAL/3
      ENDIF
      N = 1
  100 CONTINUE
      NBPTS = 2**N
      IF (NBPTS.LT.NBVA) THEN
         N = N + 1
         GOTO 100
      ENDIF
C     Methode de prise en compte du signal :
C     -TRONCATURE : on tronque au 2**N inferieur le plus proche de NBVA
C     -PROL_ZERO : on prolonge le signal avec des zero pour aller
C                   au 2**N le plus proche superieur a NBVA
      IF ( (METHOD.EQ.'TRONCATURE') .AND. (NBPTS.NE.NBVA) ) THEN
         NBPTS = 2**(N-1)
         NBPTS1 = NBPTS
         NBPTS2 = 2*NBPTS
      ELSE
         NBPTS = 2**N
         NBPTS1 = NBVA
         NBPTS2 = NBPTS
      ENDIF
C
      LFON  = LVAR + NBVA
C     --- TRANSFORMATION PAR FOURIER
      IF (NSENS.EQ.1) THEN
C     --- SENS DIRECT
C     --- RECOPIE DES VARIABLES ---
         IF ( SYM.EQ.'NON' ) THEN
            NBPTS2 = (NBPTS/2)
         ENDIF
         CALL WKVECT('&&SPDFFT.TRAVAIL','V V C',NBPTS,LTRA)
         DO 199 I = 1,NBPTS1
            ZC(LTRA+I-1) = DCMPLX(ZR(LFON+I-1),0.D0)
  199    CONTINUE
         IF (NBPTS.GT.NBVA) THEN
            DO 1999 I = 1,(NBPTS-NBVA)
               ZC(LTRA+NBVA+I-1) =  DCMPLX(0.D0,0.D0)
 1999       CONTINUE
         ENDIF

         CALL FFT(ZC(LTRA),NBPTS,1)
         PAS = ZR(LVAR+1)-ZR(LVAR)
C         NOMFS = 'FCT_FFT'
         CALL JEEXIN(NOMFS,IER)
         IF ( IER.NE.0 ) CALL JEDETR(NOMFS)
         CALL WKVECT(NOMFS,BASE//' V C',2*NBPTS2,LRES)
         LRES1 = LRES + NBPTS2
         PASFRQ = 1.D0/((DBLE(NBPTS))*PAS)
C         NOUT = LRES
         NBVOUT = NBPTS2
         DO 198 I = 1,NBPTS2
            ZC(LRES+I-1) = DCMPLX((I-1)*PASFRQ,0.D0)
  198    CONTINUE
         DO 200 I = 1,NBPTS2
            ZC(LRES1+I-1) = ZC(LTRA+I-1)
  200    CONTINUE
      ELSEIF (NSENS.EQ.-1) THEN
C     --- SENS INVERSE
C
C        Pour cas tronque
C         NBPTS=2*NBPTS
         IF ( SYM.EQ.'NON' ) THEN
            NBPTS2 = (2*NBPTS)
         ENDIF
         CALL WKVECT('&&SPDFFT.TRAVAIL','V V C',(NBPTS2+1),LTRA)
         VALMAX = (NBPTS2/2)
         IF ( NBVA .LT. (NBPTS2/2) ) VALMAX = NBVA
         DO 201 I = 1, VALMAX
            II = (2*I)-1
            ZC(LTRA+I-1) = DCMPLX(ZR(LFON+II-1),ZR(LFON+II))
            ZC(LTRA+NBPTS2-I+1) =
     &               DCMPLX(ZR(LFON+II-1),-ZR(LFON+II))
  201    CONTINUE
         ZC(LTRA+NBPTS+1)=DCMPLX(((4.D0*ZR(LFON+II-1)-ZR(LFON+II-3)
     &      )/3.D0),0.D0)
         IF ( (NBPTS.GT.NBVA) .AND. (SYM.EQ.'NON') ) THEN
            DO 2999 I = 1,(NBPTS-NBVA)
               ZC(LTRA+NBVA+I-1) =  DCMPLX(0.D0,0.D0)
               ZC(LTRA+NBPTS2-NBVA-I+1) =  DCMPLX(0.D0,0.D0)
 2999       CONTINUE
         ENDIF

         ZC(LTRA+NBPTS+1)=DCMPLX(((4.D0*DBLE(ZC(LTRA+NBPTS))
     &   -DBLE(ZC(LTRA+NBPTS-1)) )/3.D0),0.D0)

         CALL FFT(ZC(LTRA),NBPTS2,-1)
         PAS = ZR(LVAR+1)-ZR(LVAR)
C         NOMFS = 'FCT_FFT'
         CALL JEEXIN(NOMFS,IER)
         IF ( IER.NE.0 ) CALL JEDETR(NOMFS)
         CALL WKVECT(NOMFS,BASE//' V R',2*NBPTS2,LRES)
C         NOUT = LRES
         NBVOUT = NBPTS2
         LRES1 = LRES + NBPTS2
         DO 202 I = 1,NBPTS2
            ZR(LRES+I-1) = (1.D0/(DBLE(NBPTS2)*PAS))*(I-1)
  202    CONTINUE
C         PAS2 = (1.D0/ZR(LVAR+NBVA-1))*(DBLE(NBVA)/DBLE(NBPTS2))
         DO 203 I = 1,NBPTS2
            ZR(LRES1+I-1) = DBLE(ZC(LTRA+I-1))
  203    CONTINUE
      ENDIF
C
      CALL JEDETR('&&SPDFFT.TRAVAIL')
C      CALL JEDETC('V','&&',1)
C
      CALL JEDEMA()
      END
