      SUBROUTINE TRCHNO ( IFIC, NOCC )
      IMPLICIT   NONE
      INTEGER    IFIC, NOCC
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 06/09/2010   AUTEUR REZETTE C.REZETTE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ----------------------------------------------------------------------
C     COMMANDE:  TEST_RESU
C                MOT CLE FACTEUR "CHAM_NO"
C ----------------------------------------------------------------------
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
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------

      CHARACTER*6 NOMPRO
      PARAMETER (NOMPRO='TRCHNO')

      INTEGER      VALI, IOCC, IBID, IRET, NBCMP,JCMP,N1,N2,N3,N4
      INTEGER      IREFR, IREFI,IREFC,NREF, NL1, LXLGUT, NL2,NL11,NL22
      REAL*8       VALR, EPSI, PREC, R8B
      COMPLEX*16   VALC, C16B
      CHARACTER*1  TYPRES
      CHARACTER*3  SSIGNE
      CHARACTER*4  TYPCH, TESTOK
      CHARACTER*8  CRIT, NODDL, NOGRNO, NOMMA, NOCMP, TYPTES, K8B
      CHARACTER*11 MOTCLE
      CHARACTER*19 CHAM19
      CHARACTER*16 TBTXT(2)
      CHARACTER*17 NONOEU, LABEL
      CHARACTER*24 TRAVR,TRAVI,TRAVC
      CHARACTER*200 LIGN1,LIGN2
C     ------------------------------------------------------------------
      CALL JEMARQ()

      MOTCLE = 'CHAM_NO'
      TRAVR  = '&&'//NOMPRO//'_TRAVR          '
      TRAVI  = '&&'//NOMPRO//'_TRAVI          '
      TRAVC  = '&&'//NOMPRO//'_TRAVC          '

      DO 100 IOCC = 1,NOCC
        LIGN1  = ' '
        LIGN2  = ' '
        TESTOK = 'NOOK'
        NONOEU = ' '
        NODDL = ' '
        CALL GETVID('CHAM_NO','CHAM_GD',IOCC,1,1,CHAM19,N1)
        LIGN1(1:21)='---- '//MOTCLE(1:8)
        LIGN1(22:22)='.'
        LIGN2(1:21)='     '//CHAM19(1:8)
        LIGN2(22:22)='.'

        CALL UTEST3('CHAM_NO',IOCC,TBTXT)

        CALL GETVTX ( 'CHAM_NO', 'NOM_CMP',   IOCC,1,1, NODDL, N1 )
        IF( N1 .NE. 0) THEN
           NL1 = LXLGUT(LIGN1)
           NL2 = LXLGUT(LIGN2)
           LIGN1(1:NL1+16)=LIGN1(1:NL1-1)//' NOM_CMP'
           LIGN2(1:NL2+16)=LIGN2(1:NL2-1)//' '//NODDL
           LIGN1(NL1+17:NL1+17)='.'
           LIGN2(NL2+17:NL2+17)='.'
        ENDIF
        CALL GETVTX ( 'CHAM_NO', 'VALE_ABS',  IOCC,1,1, SSIGNE,N1 )
        IF(SSIGNE.EQ.'OUI')THEN
            NL1 = LXLGUT(LIGN1)
            NL2 = LXLGUT(LIGN2)
            LIGN1(1:NL1+16)=LIGN1(1:NL1-1)//' VALE_ABS'
            LIGN2(1:NL2+16)=LIGN2(1:NL2-1)//' '//SSIGNE
            LIGN1(NL1+17:NL1+17)='.'
            LIGN2(NL2+17:NL2+17)='.'
        ENDIF
        CALL GETVR8 ( 'CHAM_NO', 'PRECISION', IOCC,1,1, EPSI,  N1 )
        CALL GETVTX ( 'CHAM_NO', 'CRITERE',   IOCC,1,1, CRIT,  N1 )

        CALL GETVR8('CHAM_NO','VALE'    , IOCC,1,0,R8B   ,N1)
        CALL GETVIS('CHAM_NO','VALE_I'  , IOCC,1,0,IBID  ,N2)
        CALL GETVC8('CHAM_NO','VALE_C'  , IOCC,1,0,C16B  ,N3)
        IF( N1 .NE. 0) THEN
          NREF=-N1
          TYPRES = 'R'
          CALL JEDETR(TRAVR)
          CALL WKVECT(TRAVR,'V V R',NREF,IREFR)
          CALL GETVR8('CHAM_NO','VALE', IOCC,1,NREF,ZR(IREFR),IRET)
        ELSEIF( N2 .NE. 0) THEN
          NREF=-N2
          TYPRES = 'I'
          CALL JEDETR(TRAVI)
          CALL WKVECT(TRAVI,'V V I',NREF,IREFI)
          CALL GETVIS('CHAM_NO','VALE_I', IOCC,1,NREF,ZI(IREFI),IRET)
        ELSEIF( N3 .NE. 0) THEN
          NREF=-N3
          TYPRES = 'C'
          CALL JEDETR(TRAVC)
          CALL WKVECT(TRAVC,'V V C',NREF,IREFC)
          CALL GETVC8('CHAM_NO','VALE_C', IOCC,1,NREF,ZC(IREFC),IRET)
        ENDIF


        CALL GETVTX('CHAM_NO','TYPE_TEST',IOCC,1,1,TYPTES,N1)

        IF (N1.NE.0) THEN

          NL1 = LXLGUT(LIGN1)
          NL2 = LXLGUT(LIGN2)
          LIGN1(1:NL1+16)=LIGN1(1:NL1-1)//' TYPE_TEST'
          LIGN2(1:NL2+16)=LIGN2(1:NL2-1)//' '//TYPTES
          LIGN1(NL1+17:NL1+17)='.'
          LIGN2(NL2+17:NL2+17)='.'

          CALL GETVTX('CHAM_NO','NOM_CMP',IOCC,1,0,NODDL,N4)
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
     &                          LIGN1(161:NL11)
            ENDIF
            IF(NL22.LT.80)THEN
              WRITE (IFIC,*) LIGN2(1:NL22)
            ELSEIF(NL22.LT.160)THEN
              WRITE (IFIC,1160) LIGN2(1:80),LIGN2(81:NL22)
            ELSE
              WRITE (IFIC,1200) LIGN2(1:80),LIGN2(81:160),
     &                          LIGN2(161:NL22)
            ENDIF
            CALL UTEST1(CHAM19,TYPTES,TYPRES,NREF,TBTXT,ZI(IREFI),
     &                  ZR(IREFR),ZC(IREFC),EPSI,CRIT,IFIC,SSIGNE)
          ELSE
            NBCMP = -N4
            CALL WKVECT('&&TRCHNO.NOM_CMP','V V K8',NBCMP,JCMP)
            CALL GETVTX('CHAM_NO','NOM_CMP',IOCC,1,NBCMP,ZK8(JCMP),N4)
            CALL UTEST4(CHAM19,TYPTES,TYPRES,NREF,TBTXT,ZI(IREFI),
     &                  ZR(IREFR),ZC(IREFC),EPSI,LIGN1,LIGN2,
     &                  CRIT,IFIC,NBCMP,ZK8(JCMP),SSIGNE)
            CALL JEDETR('&&TRCHNO.NOM_CMP')
          END IF

        ELSE

          CALL GETVTX('CHAM_NO','NOM_CMP',IOCC,1,1,NODDL,N1)
          CALL DISMOI('F','NOM_MAILLA',CHAM19,'CHAMP',IBID,NOMMA,IRET)
          CALL GETVEM(NOMMA,'NOEUD','CHAM_NO','NOEUD',IOCC,1,1,
     &                NONOEU(1:8),N1)
          IF (N1.NE.0) THEN
            NL1 = LXLGUT(LIGN1)
            NL2 = LXLGUT(LIGN2)
            LIGN1(1:NL1+16)=LIGN1(1:NL1-1)//' NOEUD'
            LIGN2(1:NL2+16)=LIGN2(1:NL2-1)//' '//NONOEU(1:8)
          ENDIF

          CALL GETVEM(NOMMA,'GROUP_NO','CHAM_NO','GROUP_NO',IOCC,1,1,
     &                NOGRNO,N2)
          IF (N2.NE.0) THEN
            NL1 = LXLGUT(LIGN1)
            NL2 = LXLGUT(LIGN2)
            LIGN1(1:NL1+16)=LIGN1(1:NL1-1)//' GROUP_NO'
            LIGN2(1:NL2+16)=LIGN2(1:NL2-1)//' '//NOGRNO
          ENDIF

          NL1 = LXLGUT(LIGN1)
          NL2 = LXLGUT(LIGN2)
          WRITE (IFIC,*) LIGN1(1:NL1)
          WRITE (IFIC,*) LIGN2(1:NL2)


          IF (N1.EQ.1) THEN
C            RIEN A FAIRE.
          ELSE
            CALL UTNONO('E',NOMMA,'NOEUD',NOGRNO,NONOEU(1:8),IRET)
            IF (IRET.NE.0) THEN
              WRITE (IFIC,*) TESTOK
              GO TO 100
            END IF
            NONOEU(10:17) = NOGRNO
          END IF
          CALL UTESTR(CHAM19,NONOEU,NODDL,NREF,TBTXT,ZI(IREFI),
     &                ZR(IREFR),ZC(IREFC),TYPRES,EPSI,CRIT,IFIC,SSIGNE)
        END IF
        WRITE (IFIC,*)' '
 100  CONTINUE

1160  FORMAT(1X,A80,A)
1200  FORMAT(1X,2(A80),A)

      CALL JEDETR(TRAVR)
      CALL JEDETR(TRAVC)
      CALL JEDETR(TRAVI)


      CALL JEDEMA()
      END
