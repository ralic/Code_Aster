      SUBROUTINE DLTINS ( NBGRPA, LISPAS, LIBINT, LINBPA, NPATOT, TINIT,
     &                   LISINS )
      IMPLICIT  REAL*8  (A-H,O-Z)
      CHARACTER*24  LISPAS, LIBINT, LINBPA, LISINS
      
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 21/09/2011   AUTEUR COURTOIS M.COURTOIS 
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
C     ------------------------------------------------------------------
C OUT : NBGRPA : NOMBRE DE GROUPE DE PAS
C OUT : LISPAS : OBJET DU ZR DES PAS DE CALCUL
C OUT : LIBINT : OBJET DU ZR DES BORNES DES INTERVALLES
C OUT : LINBPA : OBJET DU ZI DU NOMBRE DE PAS PAR INTERVALLE
C OUT : NPATOT : NOMBRE TOTAL DE PAS DE CALCUL
C IN  : TINIT  : TEMPS INITIAL
C IN  : LISINS : NOM DE LA LISTE DES INSTANTS DE CALCUL
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER           ZI
      COMMON / IVARJE / ZI(1)
      REAL*8            ZR    
      COMMON / RVARJE / ZR(1)
      COMPLEX*16        ZC
      COMMON / CVARJE / ZC(1)
      LOGICAL           ZL
      COMMON / LVARJE / ZL(1)
      CHARACTER*8       ZK8
      CHARACTER*16              ZK16
      CHARACTER*24                       ZK24
      CHARACTER*32                                ZK32
      CHARACTER*80                                         ZK80
      COMMON / KVARJE / ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
      REAL*8       VALR(4)
      CHARACTER*1  K1BID
      CHARACTER*8  NOMRES, DYNA, LI
      CHARACTER*16 TYPRES, NOMCMD
      INTEGER      IARG
            
C     -----------------------------------------------------------------
      CALL JEMARQ()
      CALL GETRES(NOMRES,TYPRES,NOMCMD)
C
C     --- EST-ON EN REPRISE ? ---
      CALL GETVID('ETAT_INIT','RESULTAT',1,IARG,1,DYNA,NDY)
C
C     --- DEFINITION DES INSTANTS DE CALCUL A PARTIR DE "LIST_INST" ---
C
      CALL GETVID('INCREMENT','LIST_INST',1,IARG,1,LI,N1)
      IF (N1.NE.0) THEN
         CALL JEVEUO(LI//'           .LPAS','L',JLPAS)
         CALL JEVEUO(LI//'           .NBPA','L',JNBPA)
         CALL JEVEUO(LI//'           .VALE','L',JVALE)
         CALL JEVEUO(LI//'           .BINT','L',JBINT)
         CALL JELIRA(LI//'           .VALE','LONUTI',NBINST,K1BID)
         CALL JELIRA(LI//'           .NBPA','LONUTI',NBGRPA,K1BID)
         LISPAS = LI//'           .LPAS'
         LIBINT = LI//'           .BINT'
         LINBPA = LI//'           .NBPA'
C
         LISINS = LI//'           .VALE'
         NPATOT = NBINST - 1
C         
C
C        --- SI REPRISE, IL FAUT SE RECALER ---
         IF (NDY.NE.0) THEN
C           --- DANS QUEL INTERVALLE SE SITUE LE TEMPS INITIAL ---
            DO 100 IINT = 1,NBGRPA
               IF (TINIT.LT.ZR(JBINT+IINT)) GOTO 102
 100        CONTINUE
         VALR (1) = TINIT
         VALR (2) = ZR(JBINT+NBGRPA)
            CALL U2MESG('F', 'ALGORITH12_89',0,' ',0,0,2,VALR)
 102        CONTINUE
            EPS = ZR(JLPAS+IINT-1) / 10.D0
            IF (ABS(ZR(JBINT+IINT)-TINIT).LT.EPS) IINT = IINT + 1
            NBINTN = NBGRPA - IINT + 1
C           --- ON CREE UNE NOUVELLE LISTE ---
            CALL WKVECT('&&OP0048.LI_BINT','V V R',NBINTN+1,JBIN2)
            CALL WKVECT('&&OP0048.LI_LPAS','V V R',NBINTN  ,JLPA2)
            CALL WKVECT('&&OP0048.LI_NBPA','V V I',NBINTN  ,JNBP2)
            J = 0
            DO 110 I = IINT+1,NBGRPA
               J = J + 1
               ZI(JNBP2+J) = ZI(JNBPA+I-1)
               ZR(JBIN2+J) = ZR(JBINT+I-1)
               ZR(JLPA2+J) = ZR(JLPAS+I-1)
 110        CONTINUE
            J = J + 1
            ZR(JBIN2+J) = ZR(JBINT+NBGRPA)
C           --- POUR LE PREMIER INTERVALLE ---
            ZR(JBIN2) = TINIT
            NBPD = 0
            IF (IINT.NE.1) THEN
               DO 150 I = 1,IINT-1
                  NBPD = NBPD + ZI(JNBPA+I-1)
 150           CONTINUE
            ENDIF
            IF (NBGRPA.EQ.1) THEN
               NBPF = NBINST - 1
            ELSE
               NBPF = NBPD + ZI(JNBPA+IINT-1)
            ENDIF
            EPS = ZR(JLPAS+IINT-1) / 10.D0
            DO 120 IV = NBPD,NBPF
               IF (ABS(ZR(JVALE+IV)-TINIT).LT.EPS) GOTO 122
 120        CONTINUE
         VALR (1) = TINIT
         VALR (2) = ZR(JLPAS+IINT-1)
         VALR (3) = ZR(JBINT+IINT-1)
         VALR (4) = ZR(JBINT+IINT)
            CALL U2MESG('F', 'ALGORITH12_90',0,' ',0,0,4,VALR)
 122        CONTINUE
            ZI(JNBP2) = NBPF - IV
            ZR(JLPA2) = ZR(JLPAS+IINT-1)
            LISPAS = '&&OP0048.LI_LPAS'
            LIBINT = '&&OP0048.LI_BINT'
            LINBPA = '&&OP0048.LI_NBPA'
            JNBPA = JNBP2
            JLPAS = JLPA2
            JBINT = JBIN2
            NBGRPA = NBINTN
            NPATOT = 0
            DO 130 IP = 1,NBGRPA
               NPATOT = NPATOT + ZI(JNBPA+IP-1)
 130        CONTINUE
            NBINST = NPATOT + 1
            CALL WKVECT('&&OP0048.FI_JVALE','V V R',NBINST,JVALE)
            J = 0
            ZR(JVALE) = TINIT
            DO 140 I = 1,NBGRPA
               DT = ZR(JLPAS+I-1)
               NBP = ZI(JNBPA+I-1)
               T0 = ZR(JBINT+I-1)
               DO 142 K = 1,NBP
                 J = J + 1
                 ZR(JVALE+J) = T0 + K*DT
 142           CONTINUE
 140        CONTINUE
            LISINS= '&&OP0048.FI_JVALE'
         ENDIF
C
         CALL GETVIS('INCREMENT','NUME_FIN',1,IARG,1,NUMEF,N1)
         IF ( N1 .EQ. 0 ) THEN
            CALL GETVR8('INCREMENT','INST_FIN',1,IARG,1,TFIN,N1)
            IF ( N1 .EQ. 0 ) GOTO 9999
         ELSE
            CALL JEVEUO(LI//'           .VALE','L',JVALR)
            CALL JELIRA(LI//'           .VALE','LONUTI',NBINSR,K1BID)
            IF (NUMEF.GE.NBINSR) GOTO 9999
            TFIN = ZR(JVALR+NUMEF)
         ENDIF
C
         IF (TFIN.LT.ZR(JBINT)) THEN
         VALR (1) = TFIN
         VALR (2) = ZR(JBINT)
            CALL U2MESG('F', 'ALGORITH12_91',0,' ',0,0,2,VALR)
         ELSEIF (TFIN.GE.ZR(JBINT+NBGRPA)) THEN
            GOTO 9999
         ENDIF
C        --- DANS QUEL INTERVALLE SE SITUE L'INSTANT ---
         DO 200 IINT = 1,NBGRPA
            EPS = ZR(JLPAS+IINT-1) / 10.D0
            IF (ABS(ZR(JBINT+IINT)-TFIN).LT.EPS) GOTO 202
            IF (TFIN.LT.ZR(JBINT+IINT)) GOTO 202
 200     CONTINUE
 202     CONTINUE
         NBINTN = IINT
C        --- ON CREE UNE NOUVELLE LISTE ---
         CALL WKVECT('&&OP0048.LI_BINTF','V V R',NBINTN+1,JBIN2)
         CALL WKVECT('&&OP0048.LI_LPASF','V V R',NBINTN  ,JLPA2)
         CALL WKVECT('&&OP0048.LI_NBPAF','V V I',NBINTN  ,JNBP2)
         DO 210 I = 1,IINT
            ZI(JNBP2+I-1) = ZI(JNBPA+I-1)
            ZR(JBIN2+I-1) = ZR(JBINT+I-1)
            ZR(JLPA2+I-1) = ZR(JLPAS+I-1)
 210     CONTINUE
         ZR(JBIN2+IINT) = TFIN
C        --- POUR LE DERNIER INTERVALLE ---
         NBPD = 0
         DO 220 I = 1,IINT-1
            NBPD = NBPD + ZI(JNBPA+I-1)
 220     CONTINUE
         NBPF = NBPD + ZI(JNBPA+IINT-1)
         EPS = ZR(JLPAS+IINT-1) / 10.D0
         DO 230 IV = NBPD,NBPF
            IF (ABS(ZR(JVALE+IV)-TFIN).LT.EPS) GOTO 232
 230     CONTINUE
         VALR (1) = TFIN
         VALR (2) = ZR(JLPAS+IINT-1)
         VALR (3) = ZR(JBINT+IINT-1)
         VALR (4) = ZR(JBINT+IINT)
         CALL U2MESG('F', 'ALGORITH12_92',0,' ',0,0,4,VALR)
 232     CONTINUE
         ZI(JNBP2+IINT-1) = IV - NBPD
         LISPAS = '&&OP0048.LI_LPASF'
         LIBINT = '&&OP0048.LI_BINTF'
         LINBPA = '&&OP0048.LI_NBPAF'
         JNBPA = JNBP2
         JLPAS = JLPA2
         JBINT = JBIN2
         NBGRPA = NBINTN
         NPATOT = 0
         DO 240 IP = 1,NBGRPA
            NPATOT = NPATOT + ZI(JNBPA+IP-1)
 240     CONTINUE
         NBINST = NPATOT + 1
         CALL WKVECT('&&OP0048.FI_JVALF','V V R',NBINST,JVALE)
         ZR(JVALE) = ZR(JBINT)
         J=0
         DO 250 I = 1,NBGRPA
               DT = ZR(JLPAS+I-1)
               NBP = ZI(JNBPA+I-1)
               T0 = ZR(JBINT+I-1)
               DO 252 K = 1,NBP
                 J = J + 1
                 ZR(JVALE+J) = T0 + K*DT
 252           CONTINUE
 250        CONTINUE 
        LISINS='&&OP0048.FI_JVALF'
C
         GOTO 9999
      ENDIF
C
C     --- DEFINITION DES INSTANTS DE CALCUL A PARTIR DE "PAS" ---
C
      CALL GETVR8('INCREMENT','INST_FIN',1,IARG,1,TFIN,IBID)
      CALL GETVR8('INCREMENT','PAS',1,IARG,1,DT,IBID)
      IF ( DT.EQ.0.D0 ) THEN
        CALL U2MESS('F','ALGORITH3_12')
      ENDIF
      CALL WKVECT('&&OP0048.LI_BINT','V V R',2,JBIN2)
      CALL WKVECT('&&OP0048.LI_LPAS','V V R',1,JLPA2)
      CALL WKVECT('&&OP0048.LI_NBPA','V V I',1,JNBP2)
      NPATOT = NINT((TFIN-TINIT)/DT)
      ZI(JNBP2) = NPATOT
      ZR(JBIN2) = TINIT
      ZR(JBIN2+1) = TFIN
      ZR(JLPA2) = DT
      NBGRPA=1
      LISPAS = '&&OP0048.LI_LPAS'
      LIBINT = '&&OP0048.LI_BINT'
      LINBPA = '&&OP0048.LI_NBPA'
      CALL WKVECT('&&OP0048.LI_VALE','V V R',NPATOT+1,JVAL2)
      DO 23 I = 0, NPATOT
          ZR(JVAL2+I)=TINIT+I*DT
 23   CONTINUE
      LISINS = '&&OP0048.LI_VALE'         
C
 9999 CONTINUE
      CALL JEDEMA()
      END
