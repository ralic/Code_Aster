      SUBROUTINE OP0135 ( IER )
      IMPLICIT REAL*8 (A-H,O-Z)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 23/08/2004   AUTEUR CIBHHLV L.VIVAN 
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
C RESPONSABLE MCOURTOI M.COURTOIS
C
C     COMMANDE:  TEST_FONCTION
C
C ----------------------------------------------------------------------
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
      INTEGER      REFI, VALI
      REAL*8       VALPU(2),RESURE,RESUIM,R8NNEM,RVAL
      CHARACTER*1  CBID
      CHARACTER*3  OUINON
      CHARACTER*4  TESTOK
      CHARACTER*8  K8B, NOMPU(2), CRIT, ATTR
      CHARACTER*12 K12
      CHARACTER*16 ATT, NOMFIC
      CHARACTER*17 LABEL
      CHARACTER*19 NOMFON, SPECTR
      CHARACTER*24 CHPROL, CHPARA
      CHARACTER*50 TEXTE
      COMPLEX*16   REFC, VALC , ZEROC
      LOGICAL      LOK, ULEXIS
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFMAJ()
C
      ZERO   = 0.D0
      ZEROC  = DCMPLX(0.D0,0.D0)
      CALL GETVTX(' ','TEST_NOOK',0,1,1,OUINON,N1)
      CALL GETFAC('TABL_INTSP',NOCC)
      IF ( OUINON.EQ.'OUI' .AND. NOCC.NE.0 ) THEN
         CALL UTMESS('F','TEST_FONCTION','MOT CLE "TEST_NOOK" NON '//
     +                 'VALIDE AVEC LE MOT CLE FACTEUR "INTE_SPEC".')
      ENDIF
C
      IFIC   = 0
      NOMFIC = ' '
      CALL GETVIS ( ' ', 'UNITE'  , 1,1,1, IFIC  , N1 )
      IF ( .NOT. ULEXIS( IFIC ) ) THEN
         CALL ULOPEN ( IFIC, ' ', NOMFIC, 'NEW', 'O' )
      ENDIF
      WRITE(IFIC,1000)
C
      CALL GETVTX(' ','TEST_NOOK',0,1,1,OUINON,N1)
C
C     --- TRAITEMENT DU MOT CLE FACTEUR "VALEUR" -----------------------
C
      CALL GETFAC('VALEUR',NOCC)
      DO 10 IOCC = 1,NOCC
         CALL GETVR8('VALEUR','PRECISION' ,IOCC,1,1,EPSI  ,N1)
         CALL GETVTX('VALEUR','CRITERE'   ,IOCC,1,1,CRIT  ,N2)
         CALL GETVID('VALEUR','FONCTION'  ,IOCC,1,1,NOMFON,N3)
C
         CHPROL = NOMFON//'.PROL'
         CALL JEVEUO(CHPROL,'L',LPROL)
         CALL GETVTX('VALEUR','NOM_PARA'  ,IOCC,1,0,K8B  ,N4)
         IF (N4.NE.0) THEN
            NBPU = -N4
            CALL GETVTX('VALEUR','NOM_PARA',IOCC,1,NBPU,NOMPU,N4)
         ELSE
            IF (ZK16(LPROL).EQ.'INTERPRE') THEN
              CALL FONBPA(NOMFON,ZK16(LPROL),CBID,2,NBPU,NOMPU)
            ELSE
              NBPU = 1
              NOMPU(1) = ZK16(LPROL+2)
            ENDIF
         ENDIF
         IF (ZK16(LPROL).EQ.'NAPPE   ' .AND. NBPU.EQ.1) THEN
            CALL UTMESS('A','TEST_FONCTION','IL FAUT DEFINIR DEUX PAR'//
     +                                        'AMETRES POUR UNE NAPPE.')
            GOTO 10
         ENDIF
C
         IF (ZK16(LPROL).EQ. 'FONCT_C') THEN
           CALL GETVR8('VALEUR','VALE_PARA'  ,IOCC,1,1,VALPU,N5)
           CALL GETVC8('VALEUR','VALE_REFE_C',IOCC,1,1,REFC,N7)
         ELSE
           CALL GETVR8('VALEUR','VALE_PARA' ,IOCC,1,NBPU,VALPU,N5)
           CALL GETVR8('VALEUR','VALE_REFE' ,IOCC,1,1   ,REFR ,N6)
         ENDIF
C
         IF (ZK16(LPROL).EQ.'NAPPE   ') THEN
            L1 = MAX(1,LXLGUT(NOMFON))
            L2 = MAX(1,LXLGUT(NOMPU(1)))
            WRITE(IFIC,*)'---- NAPPE: ',NOMFON(1:L1),
     +                ', NOM_PARA: ',NOMPU(1)(1:L2),', PARA: ',VALPU(1)
         ELSE
            WRITE(IFIC,*)'---- FONCTION: ',NOMFON
         ENDIF
         CALL UTEST3 ( IFIC, 'VALEUR', IOCC )
         LABEL = ' '
         WRITE(LABEL(6:17),'(1P,E12.5)' ) VALPU(NBPU)
C
         IF (ZK16(LPROL).EQ. 'FONCT_C') THEN
           CALL FOINTC(NOMFON,0,' ',VALPU(1),RESURE,RESUIM,IRET)
           IF ( OUINON .EQ. 'OUI' ) THEN
              K12 = ZK16(LPROL+2)
              TESTOK = ' OK '
              IF ( IRET .EQ. 0 ) THEN
                VALC = DCMPLX(RESURE,RESUIM)
                IF ( CRIT(1:4) .EQ. 'RELA' ) THEN
                  LOK = ( ABS(VALC-REFC) .LE. EPSI * ABS(REFC))
                  IF ( ABS(REFC) .NE. ZEROC ) THEN
                    ERR = ABS(VALC - REFC) /  ABS(REFC)
                  ELSE
                    ERR = 999.999999D0
                  ENDIF
                  IF ( LOK ) THEN
                    TESTOK = 'NOOK'
                    TEXTE = 'PAS DE CHANCE LE TEST EST CORRECT !!!'
                    WRITE(IFIC,1300) TESTOK, TEXTE
                  ELSE
                    TESTOK = ' OK '
                    WRITE(IFIC,3000) TESTOK, K12, CRIT(1:4), ERR, VALC
                    WRITE(IFIC,3100) LABEL, EPSI, REFC
                  ENDIF
                ELSE
                  LOK = ( ABS(VALC - REFC) .LE. EPSI )
                  ERR =   ABS(VALC - REFC)
                  IF ( LOK ) THEN
                    TESTOK = 'NOOK'
                    TEXTE = 'PAS DE CHANCE LE TEST EST CORRECT !!!'
                    WRITE(IFIC,1300) TESTOK, TEXTE
                  ELSE
                    TESTOK = ' OK '
                    WRITE(IFIC,3200) TESTOK, K12, CRIT(1:4), ERR, VALC
                    WRITE(IFIC,3210) LABEL, EPSI, REFC
                  ENDIF
                ENDIF
              ELSEIF ( IRET .EQ. 10 ) THEN
                 TEXTE = ' MOINS DE 1 POINT POUR DEFINIR LA FONCTION'
                 WRITE(IFIC,1300) TESTOK, TEXTE
              ELSEIF ( IRET .EQ. 20 ) THEN
                 TEXTE = ' EXTRAPOLATION NON PERMISE'
                 WRITE(IFIC,1300) TESTOK, TEXTE
              ELSEIF ( IRET .EQ. 30 ) THEN
                 TEXTE = ' DEBORDEMENT A GAUCHE'
                 WRITE(IFIC,1300) TESTOK, TEXTE
              ELSEIF ( IRET .EQ. 40 ) THEN
                 TEXTE = ' DEBORDEMENT A DROITE'
                 WRITE(IFIC,1300) TESTOK, TEXTE
              ELSEIF ( IRET .EQ. 200 ) THEN
                 TEXTE = ' INTERPOLATION NON PERMISE'
                 WRITE(IFIC,1300) TESTOK, TEXTE
              ELSEIF ( IRET .EQ. 210 ) THEN
                 TEXTE = ' PARAMETRE EN DOUBLE'
                 WRITE(IFIC,1300) TESTOK, TEXTE
              ELSEIF ( IRET .EQ. 220 ) THEN
                 TEXTE = ' PARAMETRE NON CORRECT'
                 WRITE(IFIC,1300) TESTOK, TEXTE
              ELSEIF ( IRET .EQ. 230 ) THEN
                 TEXTE = ' TYPE INTERPOLATION INCONNU'
                 WRITE(IFIC,1300) TESTOK, TEXTE
              ELSEIF ( IRET .EQ. 240 ) THEN
                 TEXTE = ' RECHERCHE DE LA VALEUR INCONNUE'
                 WRITE(IFIC,1300) TESTOK, TEXTE
              ELSEIF ( IRET .EQ. 100 ) THEN
                 TEXTE = ' TYPE DE FONCTION NON VALIDE'
                 WRITE(IFIC,1300) TESTOK, TEXTE
              ELSEIF ( IRET .EQ. 110 ) THEN
                 TEXTE = ' PAS ASSEZ DE PARAMETRES'
                 WRITE(IFIC,1300) TESTOK, TEXTE
              ELSEIF ( IRET .EQ. 120 ) THEN
                 TEXTE = ' PARAMETRE EN DOUBLE'
                 WRITE(IFIC,1300) TESTOK, TEXTE
              ELSEIF ( IRET .EQ. 130 ) THEN
                 TEXTE = ' PARAMETRE NON CORRECT'
                 WRITE(IFIC,1300) TESTOK, TEXTE
              ELSEIF ( IRET .EQ. 140 ) THEN
                 TEXTE = ' TYPE INTERPOLATION PARA NAPPE INCONNU'
                 WRITE(IFIC,1300) TESTOK, TEXTE
              ELSEIF ( IRET .EQ. 150 ) THEN
                 TEXTE = ' TYPE DE FONCTION NON TRAITE'
                 WRITE(IFIC,1300) TESTOK, TEXTE
              ELSEIF ( IRET .EQ. 160 ) THEN
                 TEXTE = ' PAS ASSEZ DE PARAMETRES'
                 WRITE(IFIC,1300) TESTOK, TEXTE
              ELSEIF ( IRET .EQ. 170 ) THEN
                 TEXTE = ' INTERPOLATION PARA NAPPE NON PERMISE'
                 WRITE(IFIC,1300) TESTOK, TEXTE
              ENDIF
           ELSE
             IF (IRET.NE. 0) THEN
               RVAL = R8NNEM()
               VALC = DCMPLX(RVAL,RVAL)
               TESTOK = 'NOOK'
               TEXTE = ' PB INTERPOLATION. VOIR MESSAGE CI-DESSUS'
               WRITE(IFIC,1300) TESTOK, TEXTE
             ELSE
               VALC = DCMPLX(RESURE,RESUIM)
               CALL UTITES(ZK16(LPROL+2),LABEL,'C',REFI,REFR,REFC,
     +                                  VALI,VALR,VALC,EPSI,CRIT,IFIC)
             ENDIF
           ENDIF
         ELSE
           CALL FOINTE (' ',NOMFON,NBPU,NOMPU,VALPU,VALR,IRET)
           IF ( OUINON .EQ. 'OUI' ) THEN
              K12 = NOMPU(NBPU)
              TESTOK = ' OK '
              IF ( IRET .EQ. 0 ) THEN
                IF ( CRIT(1:4) .EQ. 'RELA' ) THEN
                  LOK = ( ABS(VALR-REFR) .LE. EPSI * ABS(REFR) )
                  IF ( ABS(REFR) .NE. ZERO ) THEN
                    ERR = (VALR - REFR) / REFR
                  ELSE
                    ERR = 999.999999D0
                  ENDIF
                  IF ( LOK ) THEN
                    TESTOK = 'NOOK'
                    TEXTE = 'PAS DE CHANCE LE TEST EST CORRECT !!!'
                    WRITE(IFIC,1300) TESTOK, TEXTE
                  ELSE
                    TESTOK = ' OK '
                    WRITE(IFIC,2000) TESTOK, K12, CRIT(1:4), ERR, VALR
                    WRITE(IFIC,2100) LABEL, EPSI, REFR
                  ENDIF
                ELSE
                  LOK = ( ABS(VALR - REFR) .LE. EPSI )
                  ERR =       VALR - REFR
                  IF ( LOK ) THEN
                    TESTOK = 'NOOK'
                    TEXTE = 'PAS DE CHANCE LE TEST EST CORRECT !!!'
                    WRITE(IFIC,1300) TESTOK, TEXTE
                  ELSE
                    TESTOK = ' OK '
                    WRITE(IFIC,2200) TESTOK, K12, CRIT(1:4), ERR, VALR
                    WRITE(IFIC,2210) LABEL, EPSI, REFR
                  ENDIF
                ENDIF
              ELSEIF ( IRET .EQ. 10 ) THEN
                 TEXTE = ' MOINS DE 1 POINT POUR DEFINIR LA FONCTION'
                 WRITE(IFIC,1300) TESTOK, TEXTE
              ELSEIF ( IRET .EQ. 20 ) THEN
                 TEXTE = ' EXTRAPOLATION NON PERMISE'
                 WRITE(IFIC,1300) TESTOK, TEXTE
              ELSEIF ( IRET .EQ. 30 ) THEN
                 TEXTE = ' DEBORDEMENT A GAUCHE'
                 WRITE(IFIC,1300) TESTOK, TEXTE
              ELSEIF ( IRET .EQ. 40 ) THEN
                 TEXTE = ' DEBORDEMENT A DROITE'
                 WRITE(IFIC,1300) TESTOK, TEXTE
              ELSEIF ( IRET .EQ. 200 ) THEN
                 TEXTE = ' INTERPOLATION NON PERMISE'
                 WRITE(IFIC,1300) TESTOK, TEXTE
              ELSEIF ( IRET .EQ. 210 ) THEN
                 TEXTE = ' PARAMETRE EN DOUBLE'
                 WRITE(IFIC,1300) TESTOK, TEXTE
              ELSEIF ( IRET .EQ. 220 ) THEN
                 TEXTE = ' PARAMETRE NON CORRECT'
                 WRITE(IFIC,1300) TESTOK, TEXTE
              ELSEIF ( IRET .EQ. 230 ) THEN
                 TEXTE = ' TYPE INTERPOLATION INCONNU'
                 WRITE(IFIC,1300) TESTOK, TEXTE
              ELSEIF ( IRET .EQ. 240 ) THEN
                 TEXTE = ' RECHERCHE DE LA VALEUR INCONNUE'
                 WRITE(IFIC,1300) TESTOK, TEXTE
              ELSEIF ( IRET .EQ. 100 ) THEN
                 TEXTE = ' TYPE DE FONCTION NON VALIDE'
                 WRITE(IFIC,1300) TESTOK, TEXTE
              ELSEIF ( IRET .EQ. 110 ) THEN
                 TEXTE = ' PAS ASSEZ DE PARAMETRES'
                 WRITE(IFIC,1300) TESTOK, TEXTE
              ELSEIF ( IRET .EQ. 120 ) THEN
                 TEXTE = ' PARAMETRE EN DOUBLE'
                 WRITE(IFIC,1300) TESTOK, TEXTE
              ELSEIF ( IRET .EQ. 130 ) THEN
                 TEXTE = ' PARAMETRE NON CORRECT'
                 WRITE(IFIC,1300) TESTOK, TEXTE
              ELSEIF ( IRET .EQ. 140 ) THEN
                 TEXTE = ' TYPE INTERPOLATION PARA NAPPE INCONNU'
                 WRITE(IFIC,1300) TESTOK, TEXTE
              ELSEIF ( IRET .EQ. 150 ) THEN
                 TEXTE = ' TYPE DE FONCTION NON TRAITE'
                 WRITE(IFIC,1300) TESTOK, TEXTE
              ELSEIF ( IRET .EQ. 160 ) THEN
                 TEXTE = ' PAS ASSEZ DE PARAMETRES'
                 WRITE(IFIC,1300) TESTOK, TEXTE
              ELSEIF ( IRET .EQ. 170 ) THEN
                 TEXTE = ' INTERPOLATION PARA NAPPE NON PERMISE'
                 WRITE(IFIC,1300) TESTOK, TEXTE
              ENDIF
           ELSE
              IF (IRET.NE.0) THEN
                 TESTOK = 'NOOK'
                 TEXTE = ' PB INTERPOLATION. VOIR MESSAGE CI-DESSUS'
                 WRITE(IFIC,1300) TESTOK, TEXTE
              ELSE
C
                 CALL UTITES(NOMPU(NBPU),LABEL,'R',REFI,REFR,REFC,
     +                                   VALI,VALR,VALC,EPSI,CRIT,IFIC)
              ENDIF
           ENDIF
         ENDIF
 10   CONTINUE
C
C     --- TRAITEMENT DU MOT CLE FACTEUR "ATTRIBUT" ---------------------
C
      CALL GETFAC('ATTRIBUT',NOCC)
      DO 20 IOCC = 1,NOCC
         IFONC = 1
         CALL GETVID('ATTRIBUT','FONCTION' ,IOCC,1,1,NOMFON,N1)
C
         CHPROL = NOMFON//'.PROL'
         CALL JEVEUO(CHPROL,'L',LPROL)
         IF (ZK16(LPROL).EQ.'NAPPE   ') THEN
            CALL GETVR8('ATTRIBUT','PARA'      ,IOCC,1,1,PARA,N2)
            IF (N2.EQ.0) GOTO 26
            CALL GETVR8('ATTRIBUT','PREC_PARA' ,IOCC,1,1,EPSI,N3)
            CALL GETVTX('ATTRIBUT','CRIT_PARA' ,IOCC,1,1,CRIT,N4)
C           --- RECHERCHE DE LA FONCTION ---
            CHPARA = NOMFON//'.PARA'
            CALL JEVEUO(CHPARA,'L',JPARA)
            CALL JELIRA(CHPARA,'LONUTI',NBPAR,K8B)
            DO 22 I = 1,NBPAR
               PARR = ZR(JPARA+I-1)
               IF ( CRIT(1:4) .EQ. 'RELA' ) THEN
                  LOK = ( ABS(PARA-PARR) .LE. EPSI*ABS(PARR) )
               ELSE
                  LOK = ( ABS(PARA-PARR) .LE. EPSI )
               ENDIF
               IF ( LOK ) GOTO 24
 22         CONTINUE
            CALL UTMESS('A','TEST_FONCTION','POUR LE PARAMETRE DONNE '//
     +                                'ON N''A PAS TROUVE LA FONCTION.')
            GOTO 20
 24         CONTINUE
            IFONC = I
         ENDIF
 26      CONTINUE
C
         CALL GETVTX('ATTRIBUT','ATTR'      ,IOCC,1,1,ATT ,N5)
         CALL GETVTX('ATTRIBUT','ATTR_REFE' ,IOCC,1,1,ATTR,N6)
C
         TESTOK = 'NOOK'
         IND = 8
         IF (ATT(1:13).EQ.'INTERPOL_FONC') THEN
            NOMPU(1) = ZK16(LPROL+5+2*(IFONC-1)+1)
         ELSEIF (ATT(1:8).EQ.'INTERPOL') THEN
            NOMPU(1) = ZK16(LPROL+1)
         ELSEIF (ATT(1:13).EQ.'NOM_PARA_FONC') THEN
            NOMPU(1) = ZK16(LPROL+5)
         ELSEIF (ATT(1:8).EQ.'NOM_PARA') THEN
            NOMPU(1) = ZK16(LPROL+2)
         ELSEIF (ATT(1:8).EQ.'NOM_RESU') THEN
            NOMPU(1) = ZK16(LPROL+3)
         ELSEIF (ATT(1:16).EQ.'PROL_GAUCHE_FONC') THEN
            IND = 1
            NOMPU(1) = ZK16(LPROL+5+2*(IFONC-1)+2)(1:1)
         ELSEIF (ATT(1:11).EQ.'PROL_GAUCHE') THEN
            IND = 1
            NOMPU(1) = ZK16(LPROL+4)(1:1)
         ELSEIF (ATT(1:16).EQ.'PROL_DROITE_FONC') THEN
            IND = 1
            NOMPU(1) = ZK16(LPROL+5+2*(IFONC-1)+2)(2:2)
         ELSEIF (ATT(1:11).EQ.'PROL_DROITE') THEN
            IND = 1
            NOMPU(1) = ZK16(LPROL+4)(2:2)
         ENDIF
         IF ( NOMPU(1)(1:IND).EQ.ATTR(1:IND) ) TESTOK = ' OK '
C
         IF ( OUINON .EQ. 'OUI' ) THEN
            IF ( TESTOK .EQ. ' OK ' ) THEN
               TESTOK = 'NOOK'
            ELSE
               TESTOK = ' OK '
            ENDIF
         ENDIF
C
         IF (ZK16(LPROL).EQ.'NAPPE   ') THEN
            L1 = MAX(1,LXLGUT(NOMFON))
            L2 = MAX(1,LXLGUT(ZK16(LPROL+2)))
            WRITE(IFIC,*)'---- NAPPE: ',NOMFON(1:L1),
     +            ', NOM_PARA: ',ZK16(LPROL+2)(1:L2),', PARA: ',PARA
         ELSE
            WRITE(IFIC,*)'---- FONCTION: ',NOMFON
         ENDIF
         CALL UTEST3 ( IFIC, 'ATTRIBUT', IOCC )
         WRITE(IFIC,1100) TESTOK, ATT, NOMPU(1)
         WRITE(IFIC,1200) ATTR
 20   CONTINUE
C
C     --- TRAITEMENT DU MOT CLE FACTEUR "TABL_INTSP" -------------------
C
      CALL GETFAC ( 'TABL_INTSP', NOCC )
      DO 30 IOCC = 1,NOCC
         CALL GETVR8('TABL_INTSP','PRECISION'  ,IOCC,1,1,EPSI  ,N1)
         CALL GETVTX('TABL_INTSP','CRITERE'    ,IOCC,1,1,CRIT  ,N2)
         CALL GETVR8('TABL_INTSP','VALE_PARA'  ,IOCC,1,1,XPARA ,N6)
         CALL GETVC8('TABL_INTSP','VALE_REFE_C',IOCC,1,1,REFC  ,N7)
         CALL EXTBSP ( 'TABL_INTSP', IOCC, SPECTR )
C
         CHPROL = SPECTR//'.PROL'
         CALL JEVEUO(CHPROL,'L',LPROL)
C
         WRITE(IFIC,*)'---- INTERSPECTRE: ',SPECTR
         CALL UTEST3 ( IFIC, 'TABL_INTSP', IOCC )
         LABEL = ' '
         WRITE(LABEL(6:17),'(1P,E12.5)' ) XPARA
C
         CALL FOINTC(SPECTR,1,K8B,XPARA,RESURE,RESUIM,IER)
         IF (IER.NE. 0) THEN
           TESTOK = 'NOOK'
           TEXTE = ' PB INTERPOLATION. VOIR MESSAGE CI-DESSUS'
           WRITE(IFIC,1300) TESTOK, TEXTE
         ELSE
            VALC = DCMPLX(RESURE,RESUIM)
C
            CALL UTITES(ZK16(LPROL+2),LABEL,'C',REFI,REFR,REFC,
     +                                   VALI,VALR,VALC,EPSI,CRIT,IFIC)
         ENDIF
C
 30   CONTINUE
C
1000  FORMAT(/,80('-'))
1100  FORMAT(A,1X,A,12X,'VALE:',A)
1200  FORMAT(       33X,'REFE:',A)
1300  FORMAT(A,1X,A)
C
2000  FORMAT(A,1X,A,1X,A     ,1X,2P,F7.3,' % VALE:',1P,D20.13)
2100  FORMAT(A     ,1X,'TOLE',1X,2P,F7.3,' % REFE:',1P,D20.13)
2200  FORMAT(A,1X,A,1X,A     ,1X,1P,D9.2,  ' VALE:',1P,D20.13)
2210  FORMAT(     A,1X,'TOLE',1X,1P,D9.2,  ' REFE:',1P,D20.13)
C
3000  FORMAT(A,1X,A,1X,A     ,1X,2P,F7.3,' % VALE:',1P,D20.13,1X,D20.13)
3100  FORMAT(     A,1X,'TOLE',1X,2P,F7.3,' % REFE:',1P,D20.13,1X,D20.13)
3200  FORMAT(A,1X,A,1X,A     ,1X,1P,D9.2,  ' VALE:',1P,D20.13,1X,D20.13)
3210  FORMAT(     A,1X,'TOLE',1X,1P,D9.2,  ' REFE:',1P,D20.13,1X,D20.13)
C
      CALL JEDEMA()
      END
