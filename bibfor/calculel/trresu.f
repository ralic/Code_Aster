      SUBROUTINE TRRESU ( IFIC, NOCC )
      IMPLICIT   NONE
      INCLUDE 'jeveux.h'
      INTEGER    IFIC, NOCC
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
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
C     COMMANDE:  TEST_RESU
C                MOT CLE FACTEUR "RESU"
C ----------------------------------------------------------------------


      INTEGER      VALI,IBID,IE,IOCC,IRET,IVARI,JLUE,JORDR,
     &             N1,N2,N3,N4,NBORDR,NUMORD,NUPO,NBCMP,JCMP
      INTEGER       N1R,N2R,N3R,IREFRR, IREFIR,IREFCR
      INTEGER      NUSP,IREFR,
     &             IREFI,IREFC,NREF, NL1,NL2,LXLGUT,NL11,NL22
      REAL*8       VALR,EPSI,EPSIR,PREC,R8B
      COMPLEX*16   VALC,C16B
      CHARACTER*1  TYPRES
      CHARACTER*3  SSIGNE
      CHARACTER*4  TYPCH,TESTOK,CHPT
      CHARACTER*8  CRIT,CRIT2,NOMAIL,NODDL,NOGRNO,NOMMA
      CHARACTER*8  NORESU,TYPTES,NOMGD
      CHARACTER*8  LERESU
      CHARACTER*11 MOTCLE
      CHARACTER*16 NOPARA,K16B,TBTXT(2),TBREF(2)
      CHARACTER*19 CHAM19,KNUM
      CHARACTER*17 NONOEU
      CHARACTER*24 TRAVR,TRAVI,TRAVC,TRAVRR,TRAVIR,TRAVCR
      CHARACTER*33 TITRES,VALK(3)
      CHARACTER*200 LIGN1,LIGN2
      INTEGER      IARG
      LOGICAL       LREF
C     NONOEU= NOM_NOEUD (K8) SUIVI EVENTUELLEMENT DU NOM DU GROUP_NO
C             A PARTIR DUQUEL ON TROUVE LE NOM DU NOEUD.
C     ------------------------------------------------------------------
      CALL JEMARQ()

      MOTCLE = 'RESULTAT'
      TRAVR  = '&&TRRESU_TRAVR'
      TRAVI  = '&&TRRESU_TRAVI'
      TRAVC  = '&&TRRESU_TRAVC'
      TRAVRR = '&&TRRESU_TRAVR_R'
      TRAVIR = '&&TRRESU_TRAVI_R'
      TRAVCR = '&&TRRESU_TRAVC_R'
      IREFI=1
      IREFR=1
      IREFC=1
      IREFIR=1
      IREFCR=1
      IREFRR=1
      DO 70 IOCC = 1,NOCC
        NODDL = ' '
        TESTOK = 'NOOK'

        CALL GETVTX('RESU','NOM_CMP',IOCC,IARG,1,NODDL,N1)
        CALL GETVID('RESU','RESULTAT',IOCC,IARG,1,NORESU,N1)

        CALL TRPREC ( 'RESU', IOCC, EPSI, CRIT, PREC, CRIT2 )

        CALL GETVTX('RESU','VALE_ABS', IOCC,IARG,1,SSIGNE,N1)

        CALL GETVR8('RESU','VALE_CALC'    , IOCC,IARG,0,R8B   ,N1)
        CALL GETVIS('RESU','VALE_CALC_I'  , IOCC,IARG,0,IBID  ,N2)
        CALL GETVC8('RESU','VALE_CALC_C'  , IOCC,IARG,0,C16B  ,N3)
        IF( N1 .NE. 0) THEN
          NREF=-N1
          TYPRES = 'R'
          CALL JEDETR(TRAVR)
          CALL WKVECT(TRAVR,'V V R',NREF,IREFR)
          CALL GETVR8('RESU','VALE_CALC', IOCC,IARG,
     &                NREF,ZR(IREFR),IRET)
        ELSEIF( N2 .NE. 0) THEN
          NREF=-N2
          TYPRES = 'I'
          CALL JEDETR(TRAVI)
          CALL WKVECT(TRAVI,'V V I',NREF,IREFI)
          CALL GETVIS('RESU','VALE_CALC_I', IOCC,IARG,
     &                NREF,ZI(IREFI),IRET)
        ELSEIF( N3 .NE. 0) THEN
          NREF=-N3
          TYPRES = 'C'
          CALL JEDETR(TRAVC)
          CALL WKVECT(TRAVC,'V V C',NREF,IREFC)
          CALL GETVC8('RESU','VALE_CALC_C', IOCC,IARG,
     &                NREF,ZC(IREFC),IRET)
        ENDIF
C ----------------------------------------------------------------------
        LREF=.FALSE.
        CALL GETVR8('RESU','PRECISION',IOCC,IARG,1,EPSIR,IRET)
        IF (IRET.NE.0) THEN
           LREF=.TRUE.
           CALL GETVR8('RESU','VALE_REFE'     ,IOCC,IARG,0,R8B ,N1R)
           CALL GETVIS('RESU','VALE_REFE_I'   ,IOCC,IARG,0,IBID,N2R)
           CALL GETVC8('RESU','VALE_REFE_C'   ,IOCC,IARG,0,C16B,N3R)
           IF (N1R.NE.0) THEN
             CALL ASSERT((N1R.EQ.N1))
             NREF=-N1R
             CALL JEDETR(TRAVRR)
             CALL WKVECT(TRAVRR,'V V R',NREF,IREFRR)
             CALL GETVR8('RESU','VALE_REFE', IOCC,IARG,NREF,
     &                   ZR(IREFRR),IRET)
           ELSEIF (N2R.NE.0) THEN
             CALL ASSERT((N2R.EQ.N2))
             NREF=-N2R
             CALL JEDETR(TRAVIR)
             CALL WKVECT(TRAVIR,'V V I',NREF,IREFIR)
             CALL GETVIS('RESU','VALE_REFE_I', IOCC,IARG,NREF,
     &                   ZI(IREFIR),IRET)
           ELSEIF (N3R.NE.0) THEN
             CALL ASSERT((N3R.EQ.N3))
             NREF=-N3R
             CALL JEDETR(TRAVCR)
             CALL WKVECT(TRAVCR,'V V C',NREF,IREFCR)
             CALL GETVC8('RESU','VALE_REFE_C', IOCC,IARG,NREF,
     &                  ZC(IREFCR),IRET)
           ENDIF
        ENDIF
C ----------------------------------------------------------------------

          LIGN1  = ' '
          LIGN2  = ' '
          LERESU = NORESU
          TITRES = ' '

          KNUM = '&&TRRESU.NUME_ORDRE'
          CALL RSUTNU(LERESU,'RESU',IOCC,KNUM,NBORDR,PREC,CRIT2,IRET)
          IF (IRET.NE.0) CALL U2MESS('F','CALCULEL6_94')

          CALL JEVEUO(KNUM,'L',JORDR)
          NUMORD = ZI(JORDR)

          LIGN1(1:21)='---- '//MOTCLE(1:8)
          LIGN1(22:22)='.'
          LIGN2(1:21)='     '//NORESU
          LIGN2(22:22)='.'
          NL1 = LXLGUT(LIGN1)
          NL2 = LXLGUT(LIGN2)
          LIGN1(1:NL1+16)=LIGN1(1:NL1-1)//' NUME_ORDRE'
          CALL CODENT(NUMORD,'G',CHPT)
          LIGN2(1:NL2+16)=LIGN2(1:NL2-1)//' '//CHPT
          LIGN1(NL1+17:NL1+17)='.'
          LIGN2(NL2+17:NL2+17)='.'

          CALL GETVTX('RESU','PARA',IOCC,IARG,1,NOPARA,N1)

          IF (N1.NE.0) THEN

            NL1 = LXLGUT(LIGN1)
            NL2 = LXLGUT(LIGN2)
            LIGN1(1:NL1+16)=LIGN1(1:NL1-1)//' PARA'
            LIGN2(1:NL2+16)=LIGN2(1:NL2-1)//' '//NOPARA
            LIGN1(NL1+17:NL1+17)='.'
            LIGN2(NL2+17:NL2+17)='.'


            CALL UTEST3('RESU',IOCC,TBTXT)

            CALL RSADPA(LERESU,'L',1,NOPARA,NUMORD,1,JLUE,K16B)
            IF (K16B(1:1).NE.TYPRES) THEN
              CALL U2MESS('F','CALCULEL6_95')
            ELSE IF (TYPRES.EQ.'R') THEN
              VALR = ZR(JLUE)
            ELSE IF (TYPRES.EQ.'I') THEN
              VALI = ZI(JLUE)
            ELSE IF (TYPRES.EQ.'C') THEN
              VALC = ZC(JLUE)
            END IF

            NL1 = LXLGUT(LIGN1)
            NL11 = LXLGUT(LIGN1(1:NL1-1))
            NL2 = LXLGUT(LIGN2)
            NL22 = LXLGUT(LIGN2(1:NL2-1))
            IF(NL11.LT.80)THEN
               WRITE (IFIC,*) LIGN1(1:NL11)
            ELSEIF(NL11.LT.160)THEN
               WRITE (IFIC,1160) LIGN1(1:80),
     &                           LIGN1(81:NL11)
            ELSE
               WRITE (IFIC,1200) LIGN1(1:80),
     &                           LIGN1(81:160),
     &                           LIGN1(161:NL11)
            ENDIF
            IF(NL22.LT.80)THEN
               WRITE (IFIC,*) LIGN2(1:NL22)
            ELSEIF(NL22.LT.160)THEN
               WRITE (IFIC,1160) LIGN2(1:80),
     &                           LIGN2(81:NL22)
            ELSE
               WRITE (IFIC,1200) LIGN2(1:80),
     &                           LIGN2(81:160),
     &                           LIGN2(161:NL22)
            ENDIF

            IF (LREF) THEN
              TBREF(1)=TBTXT(1)
              TBREF(2)=TBTXT(2)
              TBTXT(1)='NON_REGRESSION'
            ENDIF
            CALL UTITES(TBTXT(1),TBTXT(2),TYPRES,NREF,ZI(IREFI),
     &                  ZR(IREFR),ZC(IREFC),VALI,VALR,VALC,EPSI,
     &                  CRIT,IFIC,.TRUE.,SSIGNE)
            IF (LREF) THEN
              CALL UTITES(TBREF(1),TBREF(2),TYPRES,NREF,ZI(IREFIR),
     &                    ZR(IREFRR),ZC(IREFCR),VALI,VALR,VALC,EPSIR,
     &                    CRIT,IFIC,.FALSE.,SSIGNE)
            ENDIF
          END IF

          CALL GETVTX('RESU','NOM_CHAM',IOCC,IARG,1,NOPARA,N1)

          IF (N1.NE.0) THEN

            CALL RSEXCH('F',LERESU,NOPARA,NUMORD,CHAM19,IRET)

            NL1 = LXLGUT(LIGN1)
            NL2 = LXLGUT(LIGN2)
            LIGN1(1:NL1+16)=LIGN1(1:NL1-1)//' NOM_CHAM'
            LIGN2(1:NL2+16)=LIGN2(1:NL2-1)//' '//NOPARA
            LIGN1(NL1+17:NL1+17)='.'
            LIGN2(NL2+17:NL2+17)='.'

            CALL UTEST3('RESU',IOCC,TBTXT)

            CALL GETVTX('RESU','TYPE_TEST',IOCC,IARG,1,TYPTES,N1)

            IF (N1.NE.0) THEN

                NL1 = LXLGUT(LIGN1)
                NL2 = LXLGUT(LIGN2)
                LIGN1(1:NL1+16)=LIGN1(1:NL1-1)//' TYPE_TEST'
                LIGN2(1:NL2+16)=LIGN2(1:NL2-1)//' '//TYPTES
                LIGN1(NL1+17:NL1+17)='.'
                LIGN2(NL2+17:NL2+17)='.'



                CALL GETVTX('RESU','NOM_CMP',IOCC,IARG,0,NODDL,N4)

                IF (N4.EQ.0) THEN
                  NL1 = LXLGUT(LIGN1)
                  NL11 = LXLGUT(LIGN1(1:NL1-1))
                  NL2 = LXLGUT(LIGN2)
                  NL22 = LXLGUT(LIGN2(1:NL2-1))
                  IF(NL11.LT.80)THEN
                    WRITE (IFIC,*) LIGN1(1:NL11)
                  ELSEIF(NL11.LT.160)THEN
                    WRITE (IFIC,1160) LIGN1(1:80),LIGN1(81:NL11)
                  ELSE
                    WRITE (IFIC,1200) LIGN1(1:80),LIGN1(81:160),
     &                               LIGN1(161:NL11)
                  ENDIF
                  IF(NL22.LT.80)THEN
                    WRITE (IFIC,*) LIGN2(1:NL22)
                  ELSEIF(NL22.LT.160)THEN
                    WRITE (IFIC,1160) LIGN2(1:80),LIGN2(81:NL22)
                  ELSE
                    WRITE (IFIC,1200) LIGN2(1:80),LIGN2(81:160),
     &                               LIGN2(161:NL22)
                  ENDIF

                  IF (LREF) THEN
                    TBREF(1)=TBTXT(1)
                    TBREF(2)=TBTXT(2)
                    TBTXT(1)='NON_REGRESSION'
                  ENDIF
                  CALL UTEST1(CHAM19,TYPTES,TYPRES,NREF,TBTXT,ZI(IREFI),
     &                      ZR(IREFR),ZC(IREFC),EPSI,
     &                      CRIT,IFIC,.TRUE.,SSIGNE)
                  IF (LREF) THEN
                    CALL UTEST1(CHAM19,TYPTES,TYPRES,NREF,TBREF,
     &                          ZI(IREFIR),ZR(IREFRR),ZC(IREFCR),EPSI,
     &                          CRIT,IFIC,.FALSE.,SSIGNE)
                  ENDIF
                ELSE
                  NBCMP = -N4
                  CALL WKVECT('&&TRRESU.NOM_CMP','V V K8',NBCMP,JCMP)
                  CALL GETVTX('RESU','NOM_CMP',IOCC,IARG,NBCMP,
     &                        ZK8(JCMP),
     &                        N4)
                  IF (LREF) THEN
                    TBREF(1)=TBTXT(1)
                    TBREF(2)=TBTXT(2)
                    TBTXT(1)='NON_REGRESSION'
                  ENDIF
                  CALL UTEST4(CHAM19,TYPTES,TYPRES,NREF,TBTXT,ZI(IREFI),
     &                      ZR(IREFR),ZC(IREFC),EPSI,LIGN1,LIGN2,
     &                      CRIT,IFIC,NBCMP,ZK8(JCMP),.TRUE.,SSIGNE)
                  IF (LREF) THEN
                    CALL UTEST4(CHAM19,TYPTES,TYPRES,NREF,TBREF,
     &                     ZI(IREFIR),ZR(IREFRR),ZC(IREFCR),EPSIR,LIGN1,
     &                     LIGN2,CRIT,IFIC,NBCMP,ZK8(JCMP),
     &                     .FALSE.,SSIGNE)
                  ENDIF
                  CALL JEDETR('&&TRRESU.NOM_CMP')
                END IF

            ELSE
              CALL GETVTX('RESU','NOM_CMP',IOCC,IARG,1,NODDL,N1)

              NL1 = LXLGUT(LIGN1)
              NL2 = LXLGUT(LIGN2)
              LIGN1(1:NL1+16)=LIGN1(1:NL1-1)//' NOM_CMP'
              LIGN2(1:NL2+16)=LIGN2(1:NL2-1)//' '//NODDL
              LIGN1(NL1+17:NL1+17)='.'
              LIGN2(NL2+17:NL2+17)='.'


              NONOEU = ' '
              CALL DISMOI('F','NOM_MAILLA',CHAM19,'CHAMP',IBID,NOMMA,IE)
              CALL GETVEM(NOMMA,'NOEUD','RESU','NOEUD',IOCC,IARG,1,
     &                    NONOEU(1:8),N1)
              IF (N1.NE.0) THEN
                NL1 = LXLGUT(LIGN1)
                NL2 = LXLGUT(LIGN2)
                LIGN1(1:NL1+16)=LIGN1(1:NL1-1)//' NOEUD'
                LIGN2(1:NL2+16)=LIGN2(1:NL2-1)//' '//NONOEU(1:8)
                LIGN1(NL1+17:NL1+17)='.'
                LIGN2(NL2+17:NL2+17)='.'
              ENDIF

              CALL GETVEM(NOMMA,'GROUP_NO','RESU','GROUP_NO',IOCC,IARG,
     &                    1,
     &                    NOGRNO,N2)
              IF (N2.NE.0) THEN
                NL1 = LXLGUT(LIGN1)
                NL2 = LXLGUT(LIGN2)
                LIGN1(1:NL1+16)=LIGN1(1:NL1-1)//' GROUP_NO'
                LIGN2(1:NL2+16)=LIGN2(1:NL2-1)//' '//NOGRNO
                LIGN1(NL1+17:NL1+17)='.'
                LIGN2(NL2+17:NL2+17)='.'
              ENDIF

              IF (N1.NE.0) THEN
C              RIEN A FAIRE.
              ELSE IF (N2.NE.0) THEN
                CALL UTNONO('A',NOMMA,'NOEUD',NOGRNO,NONOEU(1:8),IRET)
                IF (IRET.NE.0) THEN
                  WRITE (IFIC,*) TESTOK
                  GO TO 50
                END IF
                NONOEU(10:17) = NOGRNO
              END IF
              CALL DISMOI('F','TYPE_CHAMP',CHAM19,'CHAMP',IBID,TYPCH,IE)
              CALL DISMOI('F','NOM_MAILLA',CHAM19,'CHAMP',IBID,NOMMA,IE)
              CALL DISMOI('F','NOM_GD',CHAM19,'CHAMP',IBID,NOMGD,IE)
              CALL UTCMP1(NOMGD,'RESU',IOCC,NODDL,IVARI)
              CALL GETVIS('RESU','SOUS_POINT',IOCC,IARG,1,NUSP,N2)
              IF (N2.EQ.0) NUSP = 0
              NUPO=0
              CALL GETVIS('RESU','POINT',IOCC,IARG,1,NUPO,N2)
              IF (TYPCH.EQ.'NOEU') THEN
                IF (N2.NE.0) THEN
                  VALK(1) = NORESU
                  VALK(2) = NOPARA
                  VALK(3) = TITRES
                  CALL U2MESG('F','CALCULEL6_97',3,VALK,1,NUMORD,0,0.D0)
                END IF

                NL1 = LXLGUT(LIGN1)
                NL11 = LXLGUT(LIGN1(1:NL1-1))
                NL2 = LXLGUT(LIGN2)
                NL22 = LXLGUT(LIGN2(1:NL2-1))

                IF(NL11.LT.80)THEN
                   WRITE (IFIC,*) LIGN1(1:NL11)
                ELSEIF(NL11.LT.160)THEN
                   WRITE (IFIC,1160) LIGN1(1:80),
     &                               LIGN1(81:NL11)
                ELSE
                   WRITE (IFIC,1200) LIGN1(1:80),
     &                               LIGN1(81:160),
     &                               LIGN1(161:NL11)
                ENDIF
                IF(NL22.LT.80)THEN
                   WRITE (IFIC,*) LIGN2(1:NL22)
                ELSEIF(NL22.LT.160)THEN
                   WRITE (IFIC,1160) LIGN2(1:80),
     &                               LIGN2(81:NL22)
                ELSE
                   WRITE (IFIC,1200) LIGN2(1:80),
     &                               LIGN2(81:160),
     &                               LIGN2(161:NL22)
                ENDIF

                IF (LREF) THEN
                  TBREF(1)=TBTXT(1)
                  TBREF(2)=TBTXT(2)
                  TBTXT(1)='NON_REGRESSION'
                ENDIF
                CALL UTESTR(CHAM19,NONOEU,NODDL,NREF,TBTXT,ZI(IREFI),
     &                      ZR(IREFR),ZC(IREFC),TYPRES,
     &                      EPSI,CRIT,IFIC,.TRUE.,SSIGNE)
                IF (LREF) THEN
                  CALL UTESTR(CHAM19,NONOEU,NODDL,NREF,TBREF,
     &                    ZI(IREFIR),ZR(IREFRR),ZC(IREFCR),TYPRES,EPSIR,
     &                    CRIT,IFIC,.FALSE.,SSIGNE)
                ENDIF
              ELSE IF (TYPCH(1:2).EQ.'EL') THEN
                CALL GETVEM(NOMMA,'MAILLE','RESU','MAILLE',IOCC,IARG,1,
     &                      NOMAIL,N1)
                IF (N1.EQ.0) THEN
                  CALL U2MESS('F','CALCULEL5_8')
                END IF

                NL1 = LXLGUT(LIGN1)
                NL2 = LXLGUT(LIGN2)
                LIGN1(1:NL1+16)=LIGN1(1:NL1-1)//' MAILLE'
                LIGN2(1:NL2+16)=LIGN2(1:NL2-1)//' '//NOMAIL
                LIGN1(NL1+17:NL1+17)='.'
                LIGN2(NL2+17:NL2+17)='.'

                NL1 = LXLGUT(LIGN1)
                NL2 = LXLGUT(LIGN2)
                LIGN1(1:NL1+16)=LIGN1(1:NL1-1)//' POINT'
                CALL CODENT(NUPO,'G',CHPT)
                LIGN2(1:NL2+16)=LIGN2(1:NL2-1)//' '//CHPT
                LIGN1(NL1+17:NL1+17)='.'
                LIGN2(NL2+17:NL2+17)='.'

                IF(NUSP.NE.0)THEN
                  NL1 = LXLGUT(LIGN1)
                  NL2 = LXLGUT(LIGN2)
                  LIGN1(1:NL1+16)=LIGN1(1:NL1-1)//' SOUS_POINT'
                  CALL CODENT(NUSP,'G',CHPT)
                  LIGN2(1:NL2+16)=LIGN2(1:NL2-1)//' '//CHPT
                  LIGN1(NL1+17:NL1+17)='.'
                  LIGN2(NL2+17:NL2+17)='.'
                ENDIF


                NL1 = LXLGUT(LIGN1)
                NL11 = LXLGUT(LIGN1(1:NL1-1))
                NL2 = LXLGUT(LIGN2)
                NL22 = LXLGUT(LIGN2(1:NL2-1))
                IF(NL11.LT.80)THEN
                   WRITE (IFIC,*) LIGN1(1:NL11)
                ELSEIF(NL11.LT.160)THEN
                   WRITE (IFIC,1160) LIGN1(1:80),
     &                               LIGN1(81:NL11)
                ELSE
                   WRITE (IFIC,1200) LIGN1(1:80),
     &                               LIGN1(81:160),
     &                               LIGN1(161:NL11)
                ENDIF
                IF(NL22.LT.80)THEN
                   WRITE (IFIC,*) LIGN2(1:NL22)
                ELSEIF(NL22.LT.160)THEN
                   WRITE (IFIC,1160) LIGN2(1:80),
     &                               LIGN2(81:NL22)
                ELSE
                   WRITE (IFIC,1200) LIGN2(1:80),
     &                               LIGN2(81:160),
     &                               LIGN2(161:NL22)
                ENDIF

                IF (LREF) THEN
                  TBREF(1)=TBTXT(1)
                  TBREF(2)=TBTXT(2)
                  TBTXT(1)='NON_REGRESSION'
                ENDIF
                CALL UTEST2(CHAM19,NOMAIL,NONOEU,NUPO,NUSP,IVARI,NODDL,
     &                      NREF,TBTXT,ZI(IREFI),ZR(IREFR),ZC(IREFC),
     &                      TYPRES,EPSI,CRIT,IFIC,.TRUE.,SSIGNE)
                IF (LREF) THEN
                  CALL UTEST2(CHAM19,NOMAIL,NONOEU,NUPO,NUSP,
     &                        IVARI,NODDL,NREF,TBREF,ZI(IREFIR),
     &                        ZR(IREFRR),ZC(IREFCR),TYPRES,EPSIR,CRIT,
     &                        IFIC,.FALSE.,SSIGNE)
                ENDIF
              END IF
            END IF
          END IF
   50     CONTINUE
          CALL JEDETR(KNUM)
        WRITE (IFIC,*)' '
   70 CONTINUE

1160  FORMAT(1X,A80,A)
1200  FORMAT(1X,2(A80),A)

      CALL JEDETR(TRAVR)
      CALL JEDETR(TRAVC)
      CALL JEDETR(TRAVI)

      CALL JEDEMA()
      END
