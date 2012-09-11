      SUBROUTINE TRGENE ( IFIC, NOCC )
      IMPLICIT   NONE
      INCLUDE 'jeveux.h'
      INTEGER    IFIC, NOCC
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 11/09/2012   AUTEUR BERRO H.BERRO 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C TOLE CRP_20
C ----------------------------------------------------------------------
C     COMMANDE:  TEST_RESU      MOT CLE FACTEUR "GENE"
C ----------------------------------------------------------------------
      CHARACTER*6 NOMPRO
      PARAMETER (NOMPRO='TRGENE')
      INTEGER      VALI,IBID,IOCC,IRET,JLUE,JORDR,JDESC,JREFE,
     &             N1,N2,N3,NBORDR,NUMORD,NCMP,NBINST,IM,
     &             JINST,JCHAM,NBMODE,JVECG,JNUME,JDEEQ,ISTRU,I,
     &             IREFR,IREFI,IREFC,NREF,LXLGUT,NL1,NL2,JFREQ,NBFREQ
      REAL*8       VALR,EPSI,PREC,TEMPS,R8B,FREQ
      COMPLEX*16   VALC,C16B
      CHARACTER*1  TYPRES
      CHARACTER*3  SSIGNE
      CHARACTER*4  CH4
      CHARACTER*8  CRIT, CRIT2, INTERP, MODE
      CHARACTER*11 MOTCLE
      CHARACTER*14 NUGENE
      CHARACTER*16 NOPARA, NSYM, K16B, TYSD, CH16,TBTXT(2)
      CHARACTER*19 CHAM19, KNUM, RESU19
      CHARACTER*24 TRAVR,TRAVI,TRAVC
      CHARACTER*200 LIGN1,LIGN2
      INTEGER      IARG
C     ------------------------------------------------------------------
      CALL JEMARQ()
      MOTCLE = 'RESU_GENE'
      TRAVR  = '&&'//NOMPRO//'_TRAVR          '
      TRAVI  = '&&'//NOMPRO//'_TRAVI          '
      TRAVC  = '&&'//NOMPRO//'_TRAVC          '

      DO 100 IOCC = 1,NOCC
        LIGN1  = ' '
        LIGN2  = ' '

        CALL TRPREC ( 'GENE', IOCC, EPSI, CRIT, PREC, CRIT2 )

        CALL GETVTX('GENE','VALE_ABS', IOCC,IARG,1,SSIGNE,N1)

        CALL GETVR8('GENE','VALE'    , IOCC,IARG,0,R8B   ,N1)
        CALL GETVIS('GENE','VALE_I'  , IOCC,IARG,0,IBID  ,N2)
        CALL GETVC8('GENE','VALE_C'  , IOCC,IARG,0,C16B  ,N3)
        IF( N1 .NE. 0) THEN
          NREF=-N1
          TYPRES = 'R'
          CALL JEDETR(TRAVR)
          CALL WKVECT(TRAVR,'V V R',NREF,IREFR)
          CALL GETVR8('GENE','VALE', IOCC,IARG,NREF,ZR(IREFR),IRET)
        ELSEIF( N2 .NE. 0) THEN
          NREF=-N2
          TYPRES = 'I'
          CALL JEDETR(TRAVI)
          CALL WKVECT(TRAVI,'V V I',NREF,IREFI)
          CALL GETVIS('GENE','VALE_I', IOCC,IARG,NREF,ZI(IREFI),IRET)
        ELSEIF( N3 .NE. 0) THEN
          NREF=-N3
          TYPRES = 'C'
          CALL JEDETR(TRAVC)
          CALL WKVECT(TRAVC,'V V C',NREF,IREFC)
          CALL GETVC8('GENE','VALE_C', IOCC,IARG,NREF,ZC(IREFC),IRET)
        ENDIF

        CALL GETVID('GENE','RESU_GENE',IOCC,IARG,1,RESU19,N1)
        CALL GETTCO(RESU19,TYSD)
C ----------------------------------------------------------------------
        IF ( TYSD .EQ. 'VECT_ASSE_GENE' ) THEN
          CALL GETVIS('GENE','NUME_CMP_GENE',IOCC,IARG,1,NCMP,N1)
          CALL JEVEUO(RESU19//'.VALE','L',JLUE)
          CALL JELIRA(RESU19//'.VALE','TYPE',IBID,K16B)
C
          CALL JEVEUO(RESU19//'.REFE','L',JREFE)
          MODE = ZK24(JREFE)(1:8)
          IF ( MODE .EQ. '        ' ) THEN
            NUGENE = ZK24(JREFE+1)(1:14)
            CALL JEVEUO(NUGENE//'.NUME.DEEQ','L',JDEEQ)
            CALL JEVEUO(NUGENE//'.NUME.NEQU','L',JNUME)
            NBMODE = ZI(JNUME)
            IM = 0
            DO 110 I = 1 , NBMODE
              ISTRU = ZI(JDEEQ+2*(I-1)+2-1)
              IF ( ISTRU .LT. 0 ) GOTO 110
              IM = IM + 1
              IF ( IM .EQ. NCMP ) GOTO 114
 110        CONTINUE
            CALL U2MESS('F','CALCULEL6_98')
 114        CONTINUE
            IM = I
          ELSE
            IM = NCMP
          ENDIF
C
          IF (K16B(1:1).NE.TYPRES) THEN
            CALL U2MESS('F','CALCULEL6_95')
          ELSE IF (TYPRES.EQ.'R') THEN
            VALR = ZR(JLUE+IM-1)
          ELSE IF (TYPRES.EQ.'I') THEN
            VALI = ZI(JLUE+IM-1)
          ELSE IF (TYPRES.EQ.'C') THEN
            VALC = ZC(JLUE+IM-1)
          END IF

          LIGN1(1:21)='---- '//MOTCLE(1:9)
          LIGN1(22:22)='.'
          LIGN2(1:21)='     '//RESU19(1:8)
          LIGN2(22:22)='.'
          NL1 = LXLGUT(LIGN1)
          NL2 = LXLGUT(LIGN2)
          LIGN1(1:NL1+16)=LIGN1(1:NL1-1)//' NUME_ORDRE'
          CH4=' '
          CALL CODENT(NUMORD,'G',CH4)
          LIGN2(1:NL2+16)=LIGN2(1:NL2-1)//' '//CH4
          LIGN1(NL1+17:NL1+17)='.'
          LIGN2(NL2+17:NL2+17)='.'
          NL1 = LXLGUT(LIGN1)
          NL2 = LXLGUT(LIGN2)
          LIGN1(1:NL1+16)=LIGN1(1:NL1-1)//' NUME_CMP_GENE'
          CH4=' '
          CALL CODENT( NCMP , 'G' , CH4  )
          LIGN2(1:NL2+16)=LIGN2(1:NL2-1)//' '//CH4

          NL1 = LXLGUT(LIGN1)
          NL2 = LXLGUT(LIGN2)
          IF(NL1.LT.80)THEN
             WRITE (IFIC,*) LIGN1(1:NL1)
          ELSEIF(NL1.LT.160)THEN
             WRITE (IFIC,1160) LIGN1(1:80),
     &                         LIGN1(81:NL1)
          ELSE
             WRITE (IFIC,1200) LIGN1(1:80),
     &                         LIGN1(81:160),
     &                         LIGN1(161:NL1)
          ENDIF
          IF(NL2.LT.80)THEN
             WRITE (IFIC,*) LIGN2(1:NL2)
          ELSEIF(NL2.LT.160)THEN
             WRITE (IFIC,1160) LIGN2(1:80),
     &                         LIGN2(81:NL2)
          ELSE
             WRITE (IFIC,1200) LIGN2(1:80),
     &                         LIGN2(81:160),
     &                         LIGN2(161:NL2)
          ENDIF

          CALL UTEST3('GENE',IOCC,TBTXT)

          CALL UTITES(TBTXT(1),TBTXT(2),TYPRES,NREF,ZI(IREFI),ZR(IREFR),
     &                ZC(IREFC),VALI,VALR,VALC,EPSI,CRIT,IFIC,SSIGNE)
C ----------------------------------------------------------------------
        ELSEIF ( TYSD .EQ. 'MODE_GENE') THEN

          KNUM = '&&TRGENE.NUME_ORDRE'
          CALL RSUTNU(RESU19,'GENE',IOCC,KNUM,NBORDR,PREC,CRIT2,IRET)
          IF (IRET.NE.0) CALL U2MESK('F','CALCULEL6_99',1,RESU19)

          CALL JEVEUO(KNUM,'L',JORDR)
          NUMORD = ZI(JORDR)

          CALL GETVTX('GENE','PARA',IOCC,IARG,1,NOPARA,N1)
          IF (N1.NE.0) THEN
            CALL RSADPA(RESU19,'L',1,NOPARA,NUMORD,1,JLUE,K16B)
            IF (K16B(1:1).NE.TYPRES) THEN
              CALL U2MESS('F','CALCULEL6_95')
            ELSE IF (TYPRES.EQ.'R') THEN
              VALR = ZR(JLUE)
            ELSE IF (TYPRES.EQ.'I') THEN
              VALI = ZI(JLUE)
            ELSE IF (TYPRES.EQ.'C') THEN
              VALC = ZC(JLUE)
            END IF

            LIGN1(1:21)='---- '//MOTCLE(1:9)
            LIGN1(22:22)='.'
            LIGN2(1:21)='     '//RESU19(1:8)
            LIGN2(22:22)='.'
            NL1 = LXLGUT(LIGN1)
            NL2 = LXLGUT(LIGN2)
            LIGN1(1:NL1+16)=LIGN1(1:NL1-1)//' NUME_ORDRE'
            CH4=' '
            CALL CODENT(NUMORD,'G',CH4)
            LIGN2(1:NL2+16)=LIGN2(1:NL2-1)//' '//CH4
            LIGN1(NL1+17:NL1+17)='.'
            LIGN2(NL2+17:NL2+17)='.'
            NL1 = LXLGUT(LIGN1)
            NL2 = LXLGUT(LIGN2)
            LIGN1(1:NL1+16)=LIGN1(1:NL1-1)//' PARA'
            LIGN2(1:NL2+16)=LIGN2(1:NL2-1)//' '//NOPARA(1:16)

            NL1 = LXLGUT(LIGN1)
            NL2 = LXLGUT(LIGN2)
            IF(NL1.LT.80)THEN
               WRITE (IFIC,*) LIGN1(1:NL1)
            ELSEIF(NL1.LT.160)THEN
               WRITE (IFIC,1160) LIGN1(1:80),
     &                           LIGN1(81:NL1)
            ELSE
               WRITE (IFIC,1200) LIGN1(1:80),
     &                           LIGN1(81:160),
     &                           LIGN1(161:NL1)
            ENDIF
            IF(NL2.LT.80)THEN
               WRITE (IFIC,*) LIGN2(1:NL2)
            ELSEIF(NL2.LT.160)THEN
               WRITE (IFIC,1160) LIGN2(1:80),
     &                           LIGN2(81:NL2)
            ELSE
               WRITE (IFIC,1200) LIGN2(1:80),
     &                           LIGN2(81:160),
     &                           LIGN2(161:NL2)
            ENDIF

            CALL UTEST3('GENE',IOCC,TBTXT)

            CALL UTITES(TBTXT(1),TBTXT(2),TYPRES,NREF,ZI(IREFI),
     &                  ZR(IREFR),ZC(IREFC),VALI,VALR,VALC,EPSI,
     &                  CRIT,IFIC,SSIGNE)
            CALL JEDETR ( KNUM )
            GOTO 100
          END IF

          CALL GETVTX('GENE','NOM_CHAM'     ,IOCC,IARG,1,NSYM,N1)
          CALL GETVIS('GENE','NUME_CMP_GENE',IOCC,IARG,1,NCMP,N1)
          CALL RSEXCH('F',RESU19,NSYM,NUMORD,CHAM19,IRET)
          CALL JEVEUO(CHAM19//'.VALE','L',JLUE)
          CALL JELIRA(CHAM19//'.VALE','TYPE',IBID,K16B)
C
          CALL JEVEUO(CHAM19//'.REFE','L',JREFE)
          MODE = ZK24(JREFE)(1:8)
          IF ( MODE .EQ. '        ' ) THEN
            NUGENE = ZK24(JREFE+1)(1:14)
            CALL JEVEUO(NUGENE//'.NUME.DEEQ','L',JDEEQ)
            CALL JEVEUO(NUGENE//'.NUME.NEQU','L',JNUME)
            NBMODE = ZI(JNUME)
            IM = 0
            DO 120 I = 1 , NBMODE
              ISTRU = ZI(JDEEQ+2*(I-1)+2-1)
              IF ( ISTRU .LT. 0 ) GOTO 120
              IM = IM + 1
              IF ( IM .EQ. NCMP ) GOTO 124
 120        CONTINUE
            CALL U2MESS('F','CALCULEL6_98')
            GO TO 100
 124        CONTINUE
            IM = I
          ELSE
            IM = NCMP
          ENDIF
C
          IF (K16B(1:1).NE.TYPRES) THEN
            CALL U2MESS('F','CALCULEL6_95')
          ELSE IF (TYPRES.EQ.'R') THEN
            VALR = ZR(JLUE+IM-1)
          ELSE IF (TYPRES.EQ.'I') THEN
            VALI = ZI(JLUE+IM-1)
          ELSE IF (TYPRES.EQ.'C') THEN
            VALC = ZC(JLUE+IM-1)
          END IF

          LIGN1(1:21)='---- '//MOTCLE(1:9)
          LIGN1(22:22)='.'
          LIGN2(1:21)='     '//RESU19(1:8)
          LIGN2(22:22)='.'
          NL1 = LXLGUT(LIGN1)
          NL2 = LXLGUT(LIGN2)
          LIGN1(1:NL1+16)=LIGN1(1:NL1-1)//' NUME_ORDRE'
          CH4=' '
          CALL CODENT(NUMORD,'G',CH4)
          LIGN2(1:NL2+16)=LIGN2(1:NL2-1)//' '//CH4
          LIGN1(NL1+17:NL1+17)='.'
          LIGN2(NL2+17:NL2+17)='.'
          NL1 = LXLGUT(LIGN1)
          NL2 = LXLGUT(LIGN2)
          LIGN1(1:NL1+16)=LIGN1(1:NL1-1)//' NOM_CHAM'
          LIGN2(1:NL2+16)=LIGN2(1:NL2-1)//' '//NSYM
          LIGN1(NL1+17:NL1+17)='.'
          LIGN2(NL2+17:NL2+17)='.'
          NL1 = LXLGUT(LIGN1)
          NL2 = LXLGUT(LIGN2)
          LIGN1(1:NL1+16)=LIGN1(1:NL1-1)//' NUME_CMP_GENE'
          CH4=' '
          CALL CODENT( NCMP , 'G' , CH4  )
          LIGN2(1:NL2+16)=LIGN2(1:NL2-1)//' '//CH4

          NL1 = LXLGUT(LIGN1)
          NL2 = LXLGUT(LIGN2)
          IF(NL1.LT.80)THEN
             WRITE (IFIC,*) LIGN1(1:NL1)
          ELSEIF(NL1.LT.160)THEN
             WRITE (IFIC,1160) LIGN1(1:80),
     &                         LIGN1(81:NL1)
          ELSE
             WRITE (IFIC,1200) LIGN1(1:80),
     &                         LIGN1(81:160),
     &                         LIGN1(161:NL1)
          ENDIF
          IF(NL2.LT.80)THEN
             WRITE (IFIC,*) LIGN2(1:NL2)
          ELSEIF(NL2.LT.160)THEN
             WRITE (IFIC,1160) LIGN2(1:80),
     &                         LIGN2(81:NL2)
          ELSE
             WRITE (IFIC,1200) LIGN2(1:80),
     &                         LIGN2(81:160),
     &                         LIGN2(161:NL2)
          ENDIF

          CALL UTEST3('GENE',IOCC,TBTXT)

          CALL UTITES(TBTXT(1),TBTXT(2),TYPRES,NREF,ZI(IREFI),ZR(IREFR),
     &                ZC(IREFC),VALI,VALR,VALC,EPSI,CRIT,IFIC,SSIGNE)
          CALL JEDETR ( KNUM )
C ----------------------------------------------------------------------
        ELSEIF ( TYSD .EQ. 'HARM_GENE' ) THEN
          CALL GETVTX('GENE','NOM_CHAM'     ,IOCC,IARG,1,NSYM,N1)
          CALL GETVIS('GENE','NUME_CMP_GENE',IOCC,IARG,1,NCMP,N1)
C
          INTERP = 'NON'
          CALL JEVEUO(RESU19//'.DISC','L',JFREQ)
          CALL JELIRA(RESU19//'.DISC','LONMAX',NBFREQ,K16B)
C
          CALL GETVR8('GENE','FREQ',IOCC,IARG,1,FREQ,N1)
          IF ( N1 .EQ. 0 ) THEN
            CALL GETVIS('GENE','NUME_ORDRE',IOCC,IARG,1,NUMORD,N1)
            FREQ = ZR(JFREQ+NUMORD-1)
          END IF
C
          CALL JEEXIN(RESU19//'.'//NSYM(1:4),IRET)
          IF (IRET.EQ.0) CALL U2MESK('F','CALCULEL6_99',1,RESU19)
          CALL JEVEUO(RESU19//'.'//NSYM(1:4),'L',JCHAM)
          CALL JEVEUO(RESU19//'.DESC','L',JDESC)
          NBMODE = ZI(JDESC+2-1)
          CALL WKVECT('&&TRGENE.CHAMP','V V C',NBMODE,JVECG)
          CALL ZXTRAC(INTERP,PREC,CRIT2,NBFREQ,ZR(JFREQ),FREQ,
     &                ZC(JCHAM),NBMODE,ZC(JVECG),IRET)
          IF (IRET.NE.0) CALL U2MESK('F','CALCULEL6_2',1,RESU19)
          VALC = ZC(JVECG+NCMP-1)

          LIGN1(1:21)='---- '//MOTCLE(1:9)
          LIGN1(22:22)='.'
          LIGN2(1:21)='     '//RESU19(1:8)
          LIGN2(22:22)='.'
          NL1 = LXLGUT(LIGN1)
          NL2 = LXLGUT(LIGN2)
          LIGN1(1:NL1+16)=LIGN1(1:NL1-1)//' FREQ'
          CH16=' '
          CALL CODREE(FREQ,'E',CH16)
          LIGN2(1:NL2+16)=LIGN2(1:NL2-1)//' '//CH16
          LIGN1(NL1+17:NL1+17)='.'
          LIGN2(NL2+17:NL2+17)='.'
          NL1 = LXLGUT(LIGN1)
          NL2 = LXLGUT(LIGN2)
          LIGN1(1:NL1+16)=LIGN1(1:NL1-1)//' NOM_CHAM'
          LIGN2(1:NL2+16)=LIGN2(1:NL2-1)//' '//NSYM(1:4)
          LIGN1(NL1+17:NL1+17)='.'
          LIGN2(NL2+17:NL2+17)='.'
          NL1 = LXLGUT(LIGN1)
          NL2 = LXLGUT(LIGN2)
          LIGN1(1:NL1+16)=LIGN1(1:NL1-1)//' NUME_CMP_GENE'
          CH4=' '
          CALL CODENT( NCMP , 'G' , CH4  )
          LIGN2(1:NL2+16)=LIGN2(1:NL2-1)//' '//CH4

          NL1 = LXLGUT(LIGN1)
          NL2 = LXLGUT(LIGN2)
          IF(NL1.LT.80)THEN
             WRITE (IFIC,*) LIGN1(1:NL1)
          ELSEIF(NL1.LT.160)THEN
             WRITE (IFIC,1160) LIGN1(1:80),
     &                         LIGN1(81:NL1)
          ELSE
             WRITE (IFIC,1200) LIGN1(1:80),
     &                         LIGN1(81:160),
     &                         LIGN1(161:NL1)
          ENDIF
          IF(NL2.LT.80)THEN
             WRITE (IFIC,*) LIGN2(1:NL2)
          ELSEIF(NL2.LT.160)THEN
             WRITE (IFIC,1160) LIGN2(1:80),
     &                         LIGN2(81:NL2)
          ELSE
             WRITE (IFIC,1200) LIGN2(1:80),
     &                         LIGN2(81:160),
     &                         LIGN2(161:NL2)
          ENDIF

          CALL UTEST3('GENE',IOCC,TBTXT)
          CALL UTITES(TBTXT(1),TBTXT(2),'C',NREF,ZI(IREFI),ZR(IREFR),
     &               ZC(IREFC),VALI,VALR,VALC,EPSI,CRIT,IFIC,SSIGNE)

          CALL JEDETR ('&&TRGENE.CHAMP')
C ----------------------------------------------------------------------
        ELSEIF ( TYSD .EQ. 'TRAN_GENE' ) THEN
          CALL GETVTX('GENE','NOM_CHAM'     ,IOCC,IARG,1,NSYM,N1)
          CALL GETVIS('GENE','NUME_CMP_GENE',IOCC,IARG,1,NCMP,N1)
C
          INTERP = 'NON'
          CALL JEVEUO(RESU19//'.DISC','L',JINST)
          CALL JELIRA(RESU19//'.DISC','LONMAX',NBINST,K16B)
C
          CALL GETVR8('GENE','INST',IOCC,IARG,1,TEMPS,N1)
          IF ( N1 .EQ. 0 ) THEN
            CALL GETVIS('GENE','NUME_ORDRE',IOCC,IARG,1,NUMORD,N1)
            TEMPS = ZR(JINST+NUMORD-1)
          END IF
C
          CALL JEEXIN(RESU19//'.'//NSYM(1:4),IRET)
          IF (IRET.EQ.0) CALL U2MESK('F','CALCULEL6_99',1,RESU19)
          CALL JEVEUO(RESU19//'.'//NSYM(1:4),'L',JCHAM)
          CALL JEVEUO(RESU19//'.DESC','L',JDESC)
          NBMODE = ZI(JDESC+2-1)
          CALL WKVECT('&&TRGENE.CHAMP','V V R',NBMODE,JVECG)
          CALL EXTRAC(INTERP,PREC,CRIT2,NBINST,ZR(JINST),TEMPS,
     &                ZR(JCHAM),NBMODE,ZR(JVECG),IRET)
          IF (IRET.NE.0) CALL U2MESK('F','CALCULEL6_2',1,RESU19)
          VALR = ZR(JVECG+NCMP-1)

          LIGN1(1:21)='---- '//MOTCLE(1:9)
          LIGN1(22:22)='.'
          LIGN2(1:21)='     '//RESU19(1:8)
          LIGN2(22:22)='.'
          NL1 = LXLGUT(LIGN1)
          NL2 = LXLGUT(LIGN2)
          LIGN1(1:NL1+16)=LIGN1(1:NL1-1)//' INST'
          CH16=' '
          CALL CODREE(TEMPS,'E',CH16)
          LIGN2(1:NL2+16)=LIGN2(1:NL2-1)//' '//CH16
          LIGN1(NL1+17:NL1+17)='.'
          LIGN2(NL2+17:NL2+17)='.'
          NL1 = LXLGUT(LIGN1)
          NL2 = LXLGUT(LIGN2)
          LIGN1(1:NL1+16)=LIGN1(1:NL1-1)//' NOM_CHAM'
          LIGN2(1:NL2+16)=LIGN2(1:NL2-1)//' '//NSYM(1:4)
          LIGN1(NL1+17:NL1+17)='.'
          LIGN2(NL2+17:NL2+17)='.'
          NL1 = LXLGUT(LIGN1)
          NL2 = LXLGUT(LIGN2)
          LIGN1(1:NL1+16)=LIGN1(1:NL1-1)//' NUME_CMP_GENE'
          CH4=' '
          CALL CODENT( NCMP , 'G' , CH4  )
          LIGN2(1:NL2+16)=LIGN2(1:NL2-1)//' '//CH4

          NL1 = LXLGUT(LIGN1)
          NL2 = LXLGUT(LIGN2)
          IF(NL1.LT.80)THEN
             WRITE (IFIC,*) LIGN1(1:NL1)
          ELSEIF(NL1.LT.160)THEN
             WRITE (IFIC,1160) LIGN1(1:80),
     &                         LIGN1(81:NL1)
          ELSE
             WRITE (IFIC,1200) LIGN1(1:80),
     &                         LIGN1(81:160),
     &                         LIGN1(161:NL1)
          ENDIF
          IF(NL2.LT.80)THEN
             WRITE (IFIC,*) LIGN2(1:NL2)
          ELSEIF(NL2.LT.160)THEN
             WRITE (IFIC,1160) LIGN2(1:80),
     &                         LIGN2(81:NL2)
          ELSE
             WRITE (IFIC,1200) LIGN2(1:80),
     &                         LIGN2(81:160),
     &                         LIGN2(161:NL2)
          ENDIF

          CALL UTEST3('GENE',IOCC,TBTXT)
          CALL UTITES(TBTXT(1),TBTXT(2),'R',NREF,ZI(IREFI),ZR(IREFR),
     &               ZC(IREFC),VALI,VALR,VALC,EPSI,CRIT,IFIC,SSIGNE)

          CALL JEDETR ('&&TRGENE.CHAMP')
        ENDIF
        WRITE (IFIC,*)' '
 100  CONTINUE
1160  FORMAT(1X,A80,A)
1200  FORMAT(1X,2(A80),A)
      CALL JEDETR(TRAVR)
      CALL JEDETR(TRAVC)
      CALL JEDETR(TRAVI)
      CALL JEDEMA()
      END
