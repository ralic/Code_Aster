      SUBROUTINE PREFFT(RESIN,METHOD,SYMETR,NSENS,GRAND,VECTOT,
     &    NBVA,IER)
      IMPLICIT NONE
      INTEGER NPARA,NSENS
      CHARACTER*4   GRAND
      CHARACTER*16  SYMETR,METHOD
      CHARACTER*19  RESIN,VECTOT
C     ------------------------------------------------------------------
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
C     OPERATEUR "ENVOI A FFT : CREATION DE FONCTIONS"
C     IN:
C       RESIN    : SD_RESULTAT INITIALE HARMONIQUE
C                  (VENANT DE DYNA_LINE_HARM)
C       METHOD   : METHODE POUR FFT
C       SYMETRIE : SPECTRE SYMETRIQUE OU NON
C       NSENS    : SENS FFT (1=DIRECT,-1=INVERSE)
C       GRAND    : GRANDEUR PHYSIQUE (DEPL,VITE,ACCE)
C
C     OUT:
C       NPARA : POINTEUR DU TABLEAU DE DONNEE VENANT DE LA FFT
C       NBVA  : NOMBRE DE PAS DE TEMPS
C
C
C
C
C
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER      NBORDR,JORDR,IBID,I,II
      INTEGER      IORDR,LACCE,LFON,IDDL,NBVA,NEQ
      INTEGER      LVAR,IER,LORDR,LVAL,IRET
      INTEGER      LVALE,NOUT,NBVIN,NBVOUT,LFON2,J
      REAL*8       R8B, DIMAG
      COMPLEX*16   C16B
      CHARACTER*1  K1B
      CHARACTER*4  GRANDE
      CHARACTER*8  K8B
      CHARACTER*16 SYM
      CHARACTER*19 CHDEP,KNUME,CHAM19,NOMFON,FONOUT
      CHARACTER*24 CHDEP2,TYPRES
C     ------------------------------------------------------------------
      CALL JEMARQ()
C      pour ne pas invalider NPARA
      GRANDE = GRAND
      IER = 0
C
      CALL GETTCO(RESIN,TYPRES)
      CALL JELIRA(RESIN//'.ORDR','LONUTI',NBORDR,K1B)
      CALL RSORAC(RESIN,'LONUTI',IBID,R8B,K8B,C16B,R8B,K8B,NBORDR,1,
     &              IBID)
      KNUME='KNUME'
      CALL WKVECT(KNUME,'V V I',NBORDR,JORDR)
      CALL RSORAC(RESIN,'TOUT_ORDRE',IBID,R8B,K8B,C16B,R8B,K8B,
     &              ZI(JORDR),NBORDR,IBID)
      CALL JEVEUO ( KNUME, 'L', LORDR )
C
C    Creation objet fonction
C
      NOMFON = '&&PREFFT.FON_AV'
      IF ( NSENS.EQ.1 ) THEN
         CALL WKVECT(NOMFON,'V V R',2*NBORDR,LVAR)
      ELSEIF (NSENS.EQ.-1) THEN
         CALL WKVECT(NOMFON,'V V R',3*NBORDR,LVAR)
      ENDIF
      LFON = LVAR + NBORDR
      FONOUT = '&&PREFFT.FCTFFT'
C
      IF (TYPRES .NE. 'TRAN_GENE') THEN
         CALL RSEXCH(RESIN,GRANDE,1,CHDEP,IRET)
         CALL JEVEUO(CHDEP//'.VALE','L',LVAL)
C  Nombre d'equations : NEQ
         CHDEP2 = CHDEP(1:19)//'.VALE'
         CALL JELIRA(CHDEP2,'LONMAX',NEQ,K1B)
      ELSE
         CALL JEVEUO(RESIN//'.'//GRANDE ,'L',LVAL)
         CHDEP2 = RESIN//'.'//GRANDE
         CALL JELIRA(CHDEP2,'LONMAX',NEQ,K1B)
         NEQ = NEQ / NBORDR
         CALL JEVEUO(RESIN//'.INST' ,'L',LACCE)
      ENDIF
      IDDL = 1
      II = 0
      SYM = SYMETR
      IF ( NSENS.EQ.1 ) THEN
C  Retirer 1 pour DYNA_LINE_TRAN...
C         NBORDR = NBORDR -1
C
         IF (TYPRES .NE. 'TRAN_GENE') THEN
            DO 5 IORDR = 0 , NBORDR-1
               CALL RSEXCH(RESIN,GRANDE,IORDR,CHAM19,IRET)
               CALL RSADPA(RESIN,'L',1,'INST',IORDR,0,LACCE,K8B)
               CALL JEVEUO(CHAM19//'.VALE','L',LVALE)
               ZR(LVAR+IORDR) = ZR(LACCE)
               ZR(LFON+II) = ZR(LVALE+IDDL-1)
               II = II + 1
               CALL JELIBE(CHAM19//'.VALE')
   5        CONTINUE
         ELSE
            DO 6 IORDR = 0 , NBORDR-1
               ZR(LVAR+IORDR) = ZR(LACCE+IORDR)
               ZR(LFON+II) = ZR(LVAL+IDDL-1+(NEQ*IORDR))
               II = II + 1
   6        CONTINUE
         ENDIF
C         NIN = LVAR
         NBVIN = NBORDR*2
C         CALL SPDFFT(NSENS,NIN,NBVIN,NOUT,NBVOUT,METHOD,SYM,'V')
         CALL SPDFFT(NSENS,NOMFON,NBVIN,FONOUT,NBVOUT,METHOD,SYM,'V')
         CALL JEVEUO(FONOUT,'L',NOUT)
C
C   Recup resultat FFT-1
C
         CALL JEEXIN( VECTOT , IRET )
C         IF ( IRET.EQ.0) THEN
C            CALL WKVECT(VECTOT,'V V C',(NEQ+1)*NBVOUT,NPARA)
C         ELSE
C            CALL JEVEUO(VECTOT,'L',NPARA)
C         ENDIF
         IF ( IRET.NE.0) CALL JEDETR(VECTOT)
         CALL WKVECT(VECTOT,'V V C',(NEQ+1)*NBVOUT,NPARA)
         LFON2 = NOUT + NBVOUT
         DO 15 I = 1,NBVOUT
            ZC(NPARA+(IDDL-1)*NBVOUT+I-1) = ZC(LFON2+I-1)
   15    CONTINUE
         IF (TYPRES .NE. 'TRAN_GENE') THEN
            CALL JELIBE(CHAM19//'.VALE')
            DO 10 IDDL = 2 , NEQ
               II = 0
               DO 20 IORDR = 0 , NBORDR-1
                  CALL RSEXCH(RESIN,GRANDE,IORDR,CHAM19,IRET)
                  CALL RSADPA(RESIN,'L',1,'INST',IORDR,0,LACCE,K8B)
                  CALL JEVEUO(CHAM19//'.VALE','L',LVALE)
                  ZR(LFON+II) = ZR(LVALE+IDDL-1)
                  II = II + 1
                  CALL JELIBE(CHAM19//'.VALE')
   20          CONTINUE
               SYM = SYMETR
               NBVIN = NBORDR*2
               CALL SPDFFT(NSENS,NOMFON,NBVIN,FONOUT,NBVOUT,METHOD,
     &                  SYM,'V')
               CALL JEVEUO(FONOUT,'L',NOUT)
C
C   Recup resultat FFT-1
C
               LFON2 = NOUT + NBVOUT
               DO 30 J = 1,NBVOUT
                  ZC(NPARA+(IDDL-1)*NBVOUT+J-1) = ZC(LFON2+J-1)
   30          CONTINUE
   10       CONTINUE
         ELSE
            DO 11 IDDL = 2 , NEQ
               II = 0
               DO 21 IORDR = 0 , NBORDR-1
                  ZR(LFON+II) = ZR(LVAL+IDDL-1+(NEQ*IORDR))
C                  ZR(LFON+II) = ZR(LVAL+(IDDL-1)*NBORDR+IORDR)
                  II = II + 1
   21          CONTINUE
               SYM = SYMETR
               NBVIN = NBORDR*2
               CALL SPDFFT(NSENS,NOMFON,NBVIN,FONOUT,NBVOUT,METHOD,
     &                  SYM,'V')
               CALL JEVEUO(FONOUT,'L',NOUT)
C
C   Recup resultat FFT-1
C
               LFON2 = NOUT + NBVOUT
               DO 31 J = 1,NBVOUT
                  ZC(NPARA+(IDDL-1)*NBVOUT+J-1) = ZC(LFON2+J-1)
   31          CONTINUE
   11       CONTINUE

         ENDIF
C
C On stocke les instants a la fin
C
         DO 40 I = 1,NBVOUT
            ZC(NPARA+(NEQ*NBVOUT)+I-1) = ZC(NOUT+I-1)

   40    CONTINUE
C  Sens inverse
      ELSEIF ( NSENS.EQ.-1 ) THEN
         DO 50 IORDR = 1 , NBORDR
            CALL RSEXCH(RESIN,GRANDE,IORDR,CHAM19,IRET)
            CALL RSADPA(RESIN,'L',1,'FREQ',IORDR,0,LACCE,K8B)
            CALL JEVEUO(CHAM19//'.VALE','L',LVALE)
            ZR(LVAR+IORDR-1) = ZR(LACCE)
            ZR(LFON+II) = DBLE(ZC(LVALE+IDDL-1))
            II = II + 1
            ZR(LFON+II) = DIMAG(ZC(LVALE+IDDL-1))
            II = II + 1
            CALL JELIBE(CHAM19//'.VALE')
   50    CONTINUE
         IF (ABS(ZR(LFON+II-1)).LT.((1.D-6)*ABS(ZR(LFON+II-2)))) THEN
            ZR(LFON+II-1) = 0.D0
         ENDIF
C         NIN = LVAR
         NBVIN = NBORDR*3
         CALL SPDFFT(NSENS,NOMFON,NBVIN,FONOUT,NBVOUT,METHOD,SYM,'V')
         CALL JEVEUO(FONOUT,'L',NOUT)
C
C   Recup resultat FFT-1
C
         CALL JEEXIN( VECTOT , IRET )
C         IF ( IRET.EQ.0) THEN
C            CALL WKVECT(VECTOT,'V V C',(NEQ+1)*NBVOUT,NPARA)
C         ELSE
C            CALL JEVEUO(VECTOT,'L',NPARA)
C         ENDIF
         IF ( IRET.NE.0) CALL JEDETR(VECTOT)
         CALL WKVECT(VECTOT,'V V R',(NEQ+1)*NBVOUT,NPARA)
         LFON2 = NOUT + NBVOUT
         DO 55 I = 1,NBVOUT
            ZR(NPARA+(IDDL-1)*NBVA+I-1) = ZR(LFON2+I-1)
   55    CONTINUE
         CALL JELIBE(CHAM19//'.VALE')
         DO 100 IDDL = 2 , NEQ
            II = 0
            DO 70 IORDR = 1 , NBORDR
               CALL RSEXCH(RESIN,GRANDE,IORDR,CHAM19,IRET)
               CALL RSADPA(RESIN,'L',1,'FREQ',IORDR,0,LACCE,K8B)
               CALL JEVEUO(CHAM19//'.VALE','L',LVALE)
               ZR(LFON+II) = DBLE(ZC(LVALE+IDDL-1))
               II = II + 1
               ZR(LFON+II) = DIMAG(ZC(LVALE+IDDL-1))
               II = II + 1
               CALL JELIBE(CHAM19//'.VALE')
   70       CONTINUE
            SYM = SYMETR
            NBVIN = NBORDR*3
            CALL SPDFFT(NSENS,NOMFON,NBVIN,FONOUT,NBVOUT,METHOD,
     &                  SYM,'V')
            CALL JEVEUO(FONOUT,'L',NOUT)

C
C   Recup resultat FFT-1
C
            LFON2 = NOUT + NBVOUT
            DO 80 J = 1,NBVOUT
               ZR(NPARA+(IDDL-1)*NBVOUT+J-1) = ZR(LFON2+J-1)
   80       CONTINUE
  100    CONTINUE
C
C On stocke les instants a la fin
C
         DO 400 I = 1,NBVOUT
            ZR(NPARA+(NEQ*NBVOUT)+I-1) = ZR(NOUT+I-1)
  400    CONTINUE
      ENDIF
      IF (TYPRES .NE. 'TRAN_GENE') CALL JELIBE(CHAM19//'.VALE')
C
      NBVA = NBVOUT
      CALL JEDETR( KNUME )
      CALL JEDETR( NOMFON )
      CALL JEDEMA()
      END
