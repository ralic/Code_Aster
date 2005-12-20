      SUBROUTINE DLTINS ( NBGRPA, LISPAS, LIBINT, LINBPA, NPATOT )
      IMPLICIT  REAL*8  (A-H,O-Z)
      CHARACTER*24  LISPAS, LIBINT, LINBPA
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 11/01/2005   AUTEUR CIBHHLV L.VIVAN 
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
C     ------------------------------------------------------------------
C OUT : NBGRPA : NOMBRE DE GROUPE DE PAS
C OUT : LISPAS  : OBJET DU ZR DES PAS DE CALCUL
C OUT : LIBINT  : OBJET DU ZR DES BORNES DES INTERVALLES
C OUT : LINBPA  : OBJET DU ZI DU NOMBRE DE PAS PAR INTERVALLE
C OUT : NPATOT : NOMBRE TOTAL DE PAS DE CALCUL
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
      CHARACTER*32      JEXNUM, JEXNOM
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
      CHARACTER*8   K8B, NOMRES, DYNA, LI, CRIT
      CHARACTER*16  TYPRES, NOMCMD
      CHARACTER*8   CTYPE
      COMPLEX*16    C16B
      CHARACTER*1 K1BID
C     -----------------------------------------------------------------
      CALL JEMARQ()
      CALL GETRES(NOMRES,TYPRES,NOMCMD)
C
C     --- EST-ON EN REPRISE ? ---
C
      CALL GETVID('ETAT_INIT','DYNA_TRANS',1,1,1,DYNA,NDY)
      IF ( NDY .NE. 0 ) THEN
         CALL GETVIS('ETAT_INIT','NUME_INIT' ,1,1,1,NUME,NNI)
         IF ( NNI .EQ. 0 ) THEN
            CALL GETVR8('ETAT_INIT','INST_INIT',1,1,1,TEMPS,NT)
            IF ( NT .EQ. 0 ) THEN
               CALL RSORAC(DYNA,'DERNIER',IBID,TEMPS,K8B,C16B,
     +                                         PREC,CRIT,NUME,1,NBTROU)
               IF (NBTROU.NE.1) THEN
                  CALL UTMESS('F',NOMCMD,'ON N''A PAS PU TROUVE LE '//
     +                                   'DERNIER INSTANT SAUVE.')
               ENDIF
            ELSE
               CALL GETVR8('ETAT_INIT','PRECISION',1,1,1,PREC ,NP)
               CALL GETVTX('ETAT_INIT','CRITERE'  ,1,1,1,CRIT ,NC)
               CALL RSORAC(DYNA,'INST',IBID,TEMPS,K8B,C16B,
     +                                        PREC,CRIT,NUME,1,NBTROU)
               IF (NBTROU.LT.0) THEN
                  CALL UTDEBM('F',NOMCMD,'PLUSIEURS CHAMPS '
     +                           //'CORRESPONDANT A L''ACCES DEMANDE.')
                  CALL UTIMPK('L','RESULTAT ',1,DYNA)
                  CALL UTIMPR('S',', ACCES "INST": ',1,TEMPS)
                  CALL UTIMPI('S',', NOMBRE :',1,-NBTROU)
                  CALL UTFINM()
               ELSEIF (NBTROU.EQ.0) THEN
                  CALL UTDEBM('F',NOMCMD,'PAS DE CHAMP '//
     +                             'CORRESPONDANT A UN ACCES DEMANDE.')
                  CALL UTIMPK('L','RESULTAT ',1,DYNA)
                  CALL UTIMPR('S',', ACCES "INST": ',1,TEMPS)
                  CALL UTFINM()
               ENDIF
            ENDIF
         ELSE
C           --- VERIFICATION QUE NUME EXISTE ---
            CALL RSORAC(DYNA,'LONUTI',IBID,R8B,K8B,C16B,R8B,K8B,
     +                                                 NBORDR,1,IBID)
            CALL WKVECT('&&OP0048.NUME_ORDRE','V V I',NBORDR,JORDR)
            CALL RSORAC(DYNA,'TOUT_ORDRE',IBID,R8B,K8B,C16B,R8B,K8B,
     +                                        ZI(JORDR),NBORDR,IBID)
            DO 10 I = 1,NBORDR
               IF (ZI(JORDR+I-1).EQ.NUME) GOTO 12
 10         CONTINUE
            CALL UTMESS('F',NOMCMD,'NUME_INIT: ON N''A PAS TROUVER'//
     +                  ' LE NUME_INIT DANS LE RESULTAT '//DYNA)
 12         CONTINUE
         ENDIF
C
C        --- RECUPERATION DE L'INSTANT ---
         CALL RSADPA(DYNA,'L',1,'INST',NUME,1,JADR,CTYPE)
         TEMPS = ZR(JADR)
      ENDIF
C
C     --- DEFINITION DES INSTANTS DE CALCUL A PARTIR DE "LIST_INST" ---
C
      CALL GETVID('INCREMENT','LIST_INST',1,1,1,LI,N1)
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
C        --- SI REPRISE, IL FAUT SE RECALER ---
         IF (NDY.NE.0) THEN
C           --- DANS QUEL INTERVALLE SE SITUE LE TEMPS ---
            DO 100 IINT = 1,NBGRPA
               IF (TEMPS.LT.ZR(JBINT+IINT)) GOTO 102
 100        CONTINUE
            CALL UTDEBM('F',NOMCMD,
     +         'INSTANT DE REPRISE SUPERIEUR A LA LISTE DES INSTANTS')
            CALL UTIMPR('L','   INSTANT DE REPRISE: ',1,TEMPS)
            CALL UTIMPR('L','   INSTANT MAX: ',1,ZR(JBINT+NBGRPA))
            CALL UTFINM()
 102        CONTINUE
            EPS = ZR(JLPAS+IINT-1) / 10.D0
            IF (ABS(ZR(JBINT+IINT)-TEMPS).LT.EPS) IINT = IINT + 1
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
            ZR(JBIN2) = TEMPS
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
               IF (ABS(ZR(JVALE+IV)-TEMPS).LT.EPS) GOTO 122
 120        CONTINUE
            CALL UTDEBM('F',NOMCMD,'ON N''A PAS TROUVE L''INSTANT')
            CALL UTIMPR('L','   INSTANT DE REPRISE: ',1,TEMPS)
            CALL UTIMPR('L','   PAS DE TEMPS: ',1, ZR(JLPAS+IINT-1))
            CALL UTIMPR('L','   BORNE MIN: ',1,ZR(JBINT+IINT-1))
            CALL UTIMPR('L','   BORNE MAX: ',1,ZR(JBINT+IINT))
            CALL UTFINM()
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
            ZR(JVALE) = TEMPS
            DO 140 I = 1,NBGRPA
               DT = ZR(JLPAS+I-1)
               NBP = ZI(JNBPA+I-1)
               TINIT = ZR(JBINT+I-1)
               DO 142 K = 1,NBP
                 J = J + 1
                 ZR(JVALE+J) = TINIT + K*DT
 142           CONTINUE
 140        CONTINUE
         ELSE
            NPATOT = NBINST - 1
         ENDIF
C
         CALL GETVIS('INCREMENT','NUME_FIN',1,1,1,NUMEF,N1)
         IF ( N1 .EQ. 0 ) THEN
            CALL GETVR8('INCREMENT','INST_FIN',1,1,1,TFIN,N1)
            IF ( N1 .EQ. 0 ) GOTO 9999
         ELSE
            CALL JEVEUO(LI//'           .VALE','L',JVALR)
            CALL JELIRA(LI//'           .VALE','LONUTI',NBINSR,K1BID)
            IF (NUMEF.GE.NBINSR) GOTO 9999
            TFIN = ZR(JVALR+NUMEF)
         ENDIF
C
         IF (TFIN.LT.ZR(JBINT)) THEN
            CALL UTDEBM('F',NOMCMD,
     +           'INSTANT FINAL INFERIEUR A LA LISTE DES INSTANTS')
            CALL UTIMPR('L','   INSTANT FINAL: ',1,TFIN)
            CALL UTIMPR('L','   INSTANT MIN  : ',1,ZR(JBINT))
            CALL UTFINM()
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
         CALL UTDEBM('F',NOMCMD,'ON N''A PAS TROUVE L''INSTANT')
         CALL UTIMPR('L','   INSTANT FINAL: ',1,TFIN)
         CALL UTIMPR('L','   PAS DE TEMPS: ',1, ZR(JLPAS+IINT-1))
         CALL UTIMPR('L','   BORNE MIN: ',1,ZR(JBINT+IINT-1))
         CALL UTIMPR('L','   BORNE MAX: ',1,ZR(JBINT+IINT))
         CALL UTFINM()
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
C
         GOTO 9999
      ENDIF
C
C     --- DEFINITION DES INSTANTS DE CALCUL A PARTIR DE "FONC_INST" ---
C
      CALL GETVID('INCREMENT','FONC_INST' ,1,1,1,LI ,N2)
      IF (N2.EQ.0) 
     +   CALL UTMESS('F','DLTINS','METHODE DE NEWMARK OU WILSON: LA '//
     +             'DONNEE DE LIST_INST OU FONC_INST EST OBLIGATOIRE ')
       CALL GETVIS('INCREMENT','PAS_CALCUL',1,1,1,IPC,N3)
      CALL JEVEUO(LI//'           .PROL','L',LPROL)
      IF (ZK16(LPROL).NE.'FONCTION') CALL UTMESS('F',NOMCMD,
     +                     'FONC_INST: ON ATTEND UNE FONCTION.')
      CALL JEVEUO(LI//'           .VALE','L',LVAR)
      CALL JELIRA(LI//'           .VALE','LONUTI',NBVAL,K8B)
      NBINST  = NBVAL / 2
      NBGRPA = 1
      DT = ZR(LVAR+1) - ZR(LVAR)
      EPSI = DT * 1.D-04
      DO 20 I = 0,NBINST-2
         DTI = ZR(LVAR+I+1) - ZR(LVAR+I)
         IF (ABS(DTI-DT).GT.EPSI) THEN
            CALL UTMESS('F',NOMCMD,'FONC_INST: IL FAUT UNE FONC'//
     +                                'TION A PAS CONSTANT.')
         ENDIF
 20   CONTINUE
C
C     --- SI REPRISE, IL FAUT SE RECALER ---
      IF (NDY.NE.0) THEN
         DO 22 I = 1, NBINST
            IF (ABS(ZR(LVAR+I-1)-TEMPS).LE.EPSI) GOTO 24
 22      CONTINUE
         CALL UTMESS('F',NOMCMD,'FONC_INST: TEMPS DE REPRISE '//
     +                          'SUPERIEUR A LA FONCTION.')
 24      CONTINUE
         INUME = I
         NBINST = NBINST - INUME + 1
      ELSE
         INUME = 1
      ENDIF
      TEMPS = ZR(LVAR+INUME-1)
      LISPAS = '&&OP0048.FI_LPAS'
      LIBINT = '&&OP0048.FI_BINT'
      LINBPA = '&&OP0048.FI_NBPA'
      CALL WKVECT('&&OP0048.FI_BINT','V V R',NBGRPA,JBINT)
      ZR(JBINT) = TEMPS
      CALL WKVECT('&&OP0048.FI_LPAS','V V R',NBGRPA,JLPAS)
      IF (IPC.EQ.1) THEN
         ZR(JLPAS) = DT
         NPATOT = NBINST - 1
      ELSE
         DT = DT / IPC
         ZR(JLPAS) = DT
         NPATOT = ( NBINST - 1 ) * IPC
      ENDIF
      CALL WKVECT('&&OP0048.FI_JVALE','V V R',NPATOT+1,JVALE)
      DO 26 I = 0,NPATOT
         ZR(JVALE+I) = TEMPS + I*DT
 26   CONTINUE
      CALL WKVECT('&&OP0048.FI_NBPA','V V I',NBGRPA,JNBPA)
C
      CALL GETVR8('INCREMENT','INST_FIN',1,1,1,TFIN,N1)
      IF ( N1 .EQ. 0 ) THEN
         CALL GETVIS('INCREMENT','NUME_FIN',1,1,1,NUMEF,N1)
         IF ( N1.NE.0 .AND. NUMEF.LE.NPATOT ) NPATOT = NUMEF
      ELSE
         DO 28 I = 1,NPATOT
            IF (ZR(JVALE+I-1).GT.TFIN) THEN
               NPATOT = I - 2
               GOTO 30
            ENDIF
 28      CONTINUE
 30      CONTINUE
      ENDIF
      ZI(JNBPA) = NPATOT
C
 9999 CONTINUE
      CALL JEDEMA()
      END
