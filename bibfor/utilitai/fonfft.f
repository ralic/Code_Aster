      SUBROUTINE FONFFT ( NSENS, NOMFON, SORTIE, BASE )
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)       NOMFON, SORTIE
      CHARACTER*(1)                               BASE
C     ----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 20/06/2000   AUTEUR ACBHHCD G.DEVESA 
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
      CHARACTER*8   CBID, NOMRES
      CHARACTER*19  NOMFI, NOMFS
      CHARACTER*24  VALE, PROL
      REAL*8        PAS,PASFRQ
C     ----------------------------------------------------------------
      CALL JEMARQ()
      NOMFI = NOMFON
      NOMFS = SORTIE
C
C     ---  NOMBRE DE POINTS ----
      VALE = NOMFI//'.VALE'
      CALL JELIRA(VALE,'LONUTI',NBVAL,CBID)
      CALL JEVEUO(VALE,'L',LVAR)
      IF (NSENS.EQ.1) THEN
         NBVA = NBVAL/2
      ELSEIF (NSENS.EQ.-1) THEN
         NBVA = NBVAL/3
      ENDIF
      N = 1
  100 CONTINUE
      NBPTS = 2**N
      IF (NBPTS.LE.NBVA) THEN
         N = N + 1
         GOTO 100
      ENDIF
      NBPTS = 2**(N-1)
      LFON  = LVAR + NBVA
C
C     --- TRANSORMATION PAR FOURIER
      IF (NSENS.EQ.1) THEN
C     --- SENS DIRECT
C     --- RECOPIE DES VARIABLES ---
         CALL WKVECT('&&TRAVAIL','V V C',NBPTS,LTRA)
         DO 199 I = 1,NBPTS
            ZC(LTRA+I-1) = DCMPLX(ZR(LFON+I-1),0.D0)
  199    CONTINUE
         CALL FFT(ZC(LTRA),NBPTS,1)
         PAS = ZR(LVAR+1)-ZR(LVAR)
         NBPTS1 = NBPTS/2
         NBVAL1 = 3*NBPTS1
         CALL WKVECT(NOMFS//'.VALE',BASE//' V R',NBVAL1,LRES)
         LRES1 = LRES + NBPTS1
         PASFRQ = 1.D0/(NBPTS*PAS)
         DO 198 I = 1,NBPTS1
            ZR(LRES+I-1) = I*PASFRQ
  198    CONTINUE
         DO 200 I = 1,NBPTS1
            II = 2*I-1
            ZR(LRES1+II-1) = DBLE(ZC(LTRA+I-1))*PAS
            ZR(LRES1+II) = DIMAG(ZC(LTRA+I-1))*PAS
  200    CONTINUE
      ELSEIF (NSENS.EQ.-1) THEN
C     --- SENS INVERSE
         NBPTS2 = 2*NBPTS
         CALL WKVECT('&&TRAVAIL','V V C',NBPTS2,LTRA)
         DO 201 I = 1,NBPTS
            II = 2*I-1
            ZC(LTRA+I-1) = DCMPLX(ZR(LFON+II-1),ZR(LFON+II))
            ZC(LTRA+NBPTS2-I+1) = 
     &               DCMPLX(ZR(LFON+II-1),-ZR(LFON+II)) 
  201    CONTINUE
         CALL FFT(ZC(LTRA),NBPTS2,-1)
         PAS = ZR(LVAR+1)-ZR(LVAR)
         WRITE(6,*) 'PAS ',PAS
         CALL WKVECT(NOMFS//'.VALE',BASE//' V R',2*NBPTS2,LRES)
         LRES1 = LRES + NBPTS2
         DO 202 I = 1,NBPTS2
            ZR(LRES+I-1) = 1.D0/(NBPTS2*PAS)*(I-1)
  202    CONTINUE
         DO 203 I = 1,NBPTS2
            ZR(LRES1+I-1) = DBLE(ZC(LTRA+I-1))*NBPTS2*PAS
  203    CONTINUE
      ENDIF
C
C     --- AFFECTATION DU .PROL ---
      PROL = NOMFI//'.PROL'
      CALL JEVEUO(PROL,'L',LPRO)
      NOMRES = ZK8(LPRO+3)
      IF ( NOMRES(1:4) .EQ. 'DEPL' ) THEN
         NOMRES = 'VITE'
      ELSEIF ( NOMRES(1:4) .EQ. 'VITE' ) THEN
         NOMRES = 'ACCE'
      ELSE
         NOMRES      = 'TOUTRESU'
      ENDIF
      PROL = NOMFS//'.PROL'
      CALL WKVECT(PROL,'G V K8',5,LPROS)
      IF (NSENS.EQ.1) THEN
         ZK8(LPROS  ) = 'FONCT_C'
         ZK8(LPROS+2) = 'FREQ'
      ELSEIF (NSENS.EQ.-1) THEN
         ZK8(LPROS  ) = 'FONCTION'
         ZK8(LPROS+2) = 'INST'
      ENDIF
      IF (ZK8(LPRO+1)(1:3).EQ.'INT') THEN
         ZK8(LPROS+1) = 'LIN LIN '
      ELSE
         ZK8(LPROS+1) = ZK8(LPRO+1)
      ENDIF
      ZK8(LPROS+3) = NOMRES
      IF (ZK8(LPRO+4)(1:1).EQ.'I' .OR. ZK8(LPRO+4)(2:2).EQ.'I') THEN
         ZK8(LPROS+4) = 'EE      '
      ELSE
         ZK8(LPROS+4) = ZK8(LPRO+4)
      ENDIF
C
      CALL JEDETC('V','&&',1)      

      CALL JEDEMA()
      END
