      SUBROUTINE TRGENE ( IFIC, NOCC )
      IMPLICIT   NONE
      INTEGER    IFIC, NOCC
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 10/10/2006   AUTEUR REZETTE C.REZETTE 
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
C                MOT CLE FACTEUR "GENE"
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
      PARAMETER (NOMPRO='TRGENE')

      INTEGER      VALI,IBID,IE,IOCC,IRET,JLUE,JORDR,JDESC,JREFE,
     &             N1,N2,N3,N4,NBORDR,NC,NP,NUMORD,NCMP,JCMP,NBINST,IM,
     &             JINST,JCHAM,NBMODE,JVECG,JNUME,JDEEQ,ISTRU,I,
     &             IREFR,IREFI,IREFC,NREF
      REAL*8       VALR,EPSI,PREC,TEMPS,R8B
      COMPLEX*16   VALC,C16B
      CHARACTER*1  TYPRES
      CHARACTER*3  SSIGNE
      CHARACTER*4  TESTOK
      CHARACTER*8  CRIT, CRIT2, NOCMP, TYPTES, INTERP, MODE, K8B
      CHARACTER*11 MOTCLE
      CHARACTER*14 NUGENE
      CHARACTER*16 NOPARA, NSYM, K16B, TYSD
      CHARACTER*19 CHAM19, KNUM, RESU19
      CHARACTER*17 LABEL
      CHARACTER*24 TRAVR,TRAVI,TRAVC
C     ------------------------------------------------------------------
      CALL JEMARQ()

      MOTCLE = 'RESU_GENE: '
      TRAVR  = '&&'//NOMPRO//'_TRAVR          '
      TRAVI  = '&&'//NOMPRO//'_TRAVI          '
      TRAVC  = '&&'//NOMPRO//'_TRAVC          '

      DO 100 IOCC = 1,NOCC
        TESTOK = 'NOOK'

        CALL TRPREC ( 'GENE', IOCC, EPSI, CRIT, PREC, CRIT2 )

        CALL GETVTX('GENE','VALE_ABS', IOCC,1,1,SSIGNE,N1)

        CALL GETVR8('GENE','VALE'    , IOCC,1,0,R8B   ,N1)
        CALL GETVIS('GENE','VALE_I'  , IOCC,1,0,IBID  ,N2)
        CALL GETVC8('GENE','VALE_C'  , IOCC,1,0,C16B  ,N3)
        IF( N1 .NE. 0) THEN
          NREF=-N1
          TYPRES = 'R'
          CALL JEDETR(TRAVR)
          CALL WKVECT(TRAVR,'V V R',NREF,IREFR)
          CALL GETVR8('GENE','VALE', IOCC,1,NREF,ZR(IREFR),IRET)
        ELSEIF( N2 .NE. 0) THEN
          NREF=-N2
          TYPRES = 'I'
          CALL JEDETR(TRAVI)
          CALL WKVECT(TRAVI,'V V I',NREF,IREFI)
          CALL GETVIS('GENE','VALE_I', IOCC,1,NREF,ZI(IREFI),IRET)
        ELSEIF( N3 .NE. 0) THEN
          NREF=-N3
          TYPRES = 'C'
          CALL JEDETR(TRAVC)
          CALL WKVECT(TRAVC,'V V C',NREF,IREFC)
          CALL GETVC8('GENE','VALE_C', IOCC,1,NREF,ZC(IREFC),IRET)
        ENDIF

        CALL GETVID('GENE','RESU_GENE',IOCC,1,1,RESU19,N1)
        CALL GETTCO(RESU19,TYSD)

C ----------------------------------------------------------------------
        IF ( TYSD .EQ. 'VECT_ASSE_GENE_R' ) THEN
          CALL GETVIS('GENE','NUME_CMP_GENE',IOCC,1,1,NCMP,N1)
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
            WRITE (IFIC,*) TESTOK,' COMPOSANTE GENERALISEE NON TROUVEE'
            GO TO 100
 114        CONTINUE
            IM = I
          ELSE
            IM = NCMP
          ENDIF
C
          IF (K16B(1:1).NE.TYPRES) THEN
            WRITE (IFIC,*) TESTOK,' TYPE DE LA VALEUR DE REFER'//
     &          'ENCE INCOMPATIBLE AVEC LE TYPE DES VALEURS DU CHAMP'
            GOTO 100
          ELSE IF (TYPRES.EQ.'R') THEN
            VALR = ZR(JLUE+IM-1)
          ELSE IF (TYPRES.EQ.'I') THEN
            VALI = ZI(JLUE+IM-1)
          ELSE IF (TYPRES.EQ.'C') THEN
            VALC = ZC(JLUE+IM-1)
          END IF
          WRITE (IFIC,'(1X,3A)') '---- ',MOTCLE,RESU19(1:8)
          NOCMP = ' '
          LABEL = 'NUME_CMP_GENE:'
          CALL CODENT( NCMP , 'G' , LABEL(15:17)  )
          CALL UTITES(NOCMP,LABEL,TYPRES,NREF,ZI(IREFI),ZR(IREFR),
     &                ZC(IREFC),VALI,VALR,VALC,EPSI,CRIT,IFIC,SSIGNE)

C ----------------------------------------------------------------------
        ELSEIF ( TYSD .EQ. 'MODE_GENE' .OR.
     +           TYSD .EQ. 'HARM_GENE' ) THEN

          KNUM = '&&TRGENE.NUME_ORDRE'
          CALL RSUTNU(RESU19,'GENE',IOCC,KNUM,NBORDR,PREC,CRIT2,IRET)
          IF (IRET.NE.0) THEN
            WRITE (IFIC,*) TESTOK,' PAS D''ACCES AU RESU_GENE '//RESU19
            GO TO 100
          END IF
          CALL JEVEUO(KNUM,'L',JORDR)
          NUMORD = ZI(JORDR)

          CALL GETVTX('GENE','PARA',IOCC,1,1,NOPARA,N1)
          IF (N1.NE.0) THEN
            WRITE (IFIC,'(1X,4A,I4)') '---- ',MOTCLE,RESU19(1:8),
     &        ' NUME_ORDRE:',NUMORD
            CALL UTEST3(IFIC,'GENE',IOCC)
            CALL RSADPA(RESU19,'L',1,NOPARA,NUMORD,1,JLUE,K16B)
            IF (K16B(1:1).NE.TYPRES) THEN
              WRITE (IFIC,*) TESTOK,' TYPE DE LA VALEUR DE REFER'//
     &          'ENCE INCOMPATIBLE AVEC LE TYPE DES VALEURS DU CHAMP'
              GOTO 100
            ELSE IF (TYPRES.EQ.'R') THEN
              VALR = ZR(JLUE)
            ELSE IF (TYPRES.EQ.'I') THEN
              VALI = ZI(JLUE)
            ELSE IF (TYPRES.EQ.'C') THEN
              VALC = ZC(JLUE)
            END IF
            NOCMP = ' '
            LABEL = NOPARA(1:16)
            CALL UTITES(NOCMP,LABEL,TYPRES,NREF,ZI(IREFI),ZR(IREFR),
     &                  ZC(IREFC),VALI,VALR,VALC,EPSI,CRIT,IFIC,SSIGNE)
            CALL JEDETR ( KNUM )
            GOTO 100
          END IF

          CALL GETVTX('GENE','NOM_CHAM'     ,IOCC,1,1,NSYM,N1)
          CALL GETVIS('GENE','NUME_CMP_GENE',IOCC,1,1,NCMP,N1)
          WRITE (IFIC,'(1X,4A,I4,2A)') '---- ',MOTCLE,RESU19(1:8),
     &        ' NUME_ORDRE:',NUMORD,' NOM_CHAM: ',NSYM
          CALL RSEXCH(RESU19,NSYM,NUMORD,CHAM19,IRET)
          IF (IRET.NE.0) THEN
            WRITE (IFIC,*) TESTOK,' PAS D''ACCES AU RESU_GENE '//RESU19
            GO TO 100
          END IF
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
            WRITE (IFIC,*) TESTOK,' COMPOSANTE GENERALISEE NON TROUVEE'
            GO TO 100
 124        CONTINUE
            IM = I
          ELSE
            IM = NCMP
          ENDIF
C
          IF (K16B(1:1).NE.TYPRES) THEN
            WRITE (IFIC,*) TESTOK,' TYPE DE LA VALEUR DE REFER'//
     &          'ENCE INCOMPATIBLE AVEC LE TYPE DES VALEURS DU CHAMP'
            GOTO 100
          ELSE IF (TYPRES.EQ.'R') THEN
            VALR = ZR(JLUE+IM-1)
          ELSE IF (TYPRES.EQ.'I') THEN
            VALI = ZI(JLUE+IM-1)
          ELSE IF (TYPRES.EQ.'C') THEN
            VALC = ZC(JLUE+IM-1)
          END IF
          NOCMP = ' '
          LABEL = 'NUME_CMP_GENE:'
          CALL CODENT( NCMP , 'G' , LABEL(15:17)  )
          CALL UTITES(NOCMP,LABEL,TYPRES,NREF,ZI(IREFI),ZR(IREFR),
     &                ZC(IREFC),VALI,VALR,VALC,EPSI,CRIT,IFIC,SSIGNE)
          CALL JEDETR ( KNUM )

C ----------------------------------------------------------------------
        ELSEIF ( TYSD .EQ. 'TRAN_GENE' ) THEN
          CALL GETVTX('GENE','NOM_CHAM'     ,IOCC,1,1,NSYM,N1)
          CALL GETVIS('GENE','NUME_CMP_GENE',IOCC,1,1,NCMP,N1)
C
          INTERP = 'NON'
          CALL JEVEUO(RESU19//'.INST','L',JINST)
          CALL JELIRA(RESU19//'.INST','LONMAX',NBINST,K16B)
C
          CALL GETVR8('GENE','INST',IOCC,1,1,TEMPS,N1)
          IF ( N1 .EQ. 0 ) THEN
            CALL GETVIS('GENE','NUME_ORDRE',IOCC,1,1,NUMORD,N1)
            TEMPS = ZR(JINST+NUMORD-1)
          END IF
C
          CALL JEEXIN(RESU19//'.'//NSYM(1:4),IRET)
          IF (IRET.EQ.0) THEN
            WRITE (IFIC,*) TESTOK,' PAS D''ACCES AU RESU_GENE '//RESU19
            GO TO 100
          END IF
          CALL JEVEUO(RESU19//'.'//NSYM(1:4),'L',JCHAM)
          CALL JEVEUO(RESU19//'.DESC','L',JDESC)
          NBMODE = ZI(JDESC+2-1)
          CALL WKVECT('&&TRGENE.CHAMP','V V R',NBMODE,JVECG)
          CALL EXTRAC(INTERP,PREC,CRIT2,NBINST,ZR(JINST),TEMPS,
     +                ZR(JCHAM),NBMODE,ZR(JVECG),IRET)
          IF (IRET.NE.0) THEN
            WRITE (IFIC,*) TESTOK,' PB EXTRACTION RESU_GENE '//RESU19
            GO TO 100
          END IF
          VALR = ZR(JVECG+NCMP-1)
          NOCMP = NSYM(1:4)//'    '
          LABEL = 'NUME_CMP_GENE:'
          CALL CODENT( NCMP , 'G' , LABEL(15:17)  )
          WRITE (IFIC,'(1X,4A,1P,E12.5)') '---- ',MOTCLE,
     &              RESU19(1:8),' INST:',TEMPS
          CALL UTITES(NOCMP,LABEL,'R',NREF,ZI(IREFI),ZR(IREFR),
     &               ZC(IREFC),VALI,VALR,VALC,EPSI,CRIT,IFIC,SSIGNE)
          CALL JEDETR ('&&TRGENE.CHAMP')
        ENDIF

 100  CONTINUE

      CALL JEDETR(TRAVR)
      CALL JEDETR(TRAVC)
      CALL JEDETR(TRAVI)

      CALL JEDEMA()
      END
