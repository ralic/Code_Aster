      SUBROUTINE OP0132 ( IER )
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER             IER
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 30/01/2006   AUTEUR LEBOUVIE F.LEBOUVIER 
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
C TOLE CRP_20
C
C     CALCUL DYNAMIQUE ALEATOIRE
C
C     ------------------------------------------------------------------
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
C
      CHARACTER*3  TYPPAR
      CHARACTER*8  K8B, INTESP, NOMRES, TYPATN(5),
     +             TYPART(3), TYPAFO(5), KINDI, KINDJ, KVAL(4)
      CHARACTER*16 NOMCMD, TYCONC, TYCAL(4), NOCHAM, K16I, K16J
      CHARACTER*16 KPARFO(5), KVALST(10), NOPART(3),NOPATN(5)
      CHARACTER*19 NOMFON
      CHARACTER*24 K24B, KFOKIJ, SPEGRD, NOMOBJ, NOMOB1, NOMOB2,
     +             NOMOB3, NOMOB4, NEWTAB
      CHARACTER*75 CMESS
      LOGICAL      TABL, LINDI, EXISP
      INTEGER      IBID, IFVMIN(4), IFPAS(4), IVAL(3)
      COMPLEX*16   C16B
      REAL*8       VALMIN(4),VALMAX(4),PAS(4),DUREE(4)
C
      DATA NOPART / 'NUME_VITE_FLUI' , 'NUME_ORDRE_I' , 'NUME_ORDRE_J' /
      DATA TYPART / 'I' , 'I' , 'I' /
      DATA NOPATN / 'NUME_VITE_FLUI' , 'NOEUD_I' , 'NOM_CMP_I' ,
     +                                 'NOEUD_J' , 'NOM_CMP_J' /
      DATA TYPATN / 'I' , 'K8' , 'K8' , 'K8' , 'K8' /
C
      DATA KVALST / 'LAMBDA_00' , 'LAMBDA_01' , 'LAMBDA_02','LAMBDA_03',
     +              'LAMBDA_04' , 'ECART' , 'NB_EXTREMA_P_S' ,
     +              'NB_PASS_ZERO_P_S' , 'FREQ_APPAR' , 'FACT_IRRE' /
C
      DATA KPARFO / 'GRANDEUR' , 'RAYLEIGH' , 'GAUSS' , 'DEPASSEMENT',
     +              'VANMARCKE'/
      DATA TYPAFO / 'K8' , 'K24' , 'K24' , 'K24' , 'K24' /
C     ------------------------------------------------------------------
      CALL JEMARQ()
C
      PI     = R8PI()
      EPSMIN = R8MIEM()
C
      CALL GETRES ( NOMRES, TYCONC, NOMCMD )
C
C   --------------------0 ---VERIFICATION DES ARGUMENTS -----------
C
      CALL GETVIS(' ','NUME_ORDRE_I',1,1,0,INDI,NBINDI)
      CALL GETVIS(' ','NUME_ORDRE_J',1,1,0,INDJ,NBINDJ)
      IF ( NBINDI .EQ. 0 ) THEN
         CALL GETVID(' ','NOEUD_I'  ,1,1,0,K8B,NBINDI)
         CALL GETVID(' ','NOEUD_J'  ,1,1,0,K8B,NBINDJ)
         CALL GETVTX(' ','NOM_CMP_I',1,1,0,K8B,NBCMPI)
         CALL GETVTX(' ','NOM_CMP_J',1,1,0,K8B,NBCMPJ)
         IF ( NBCMPI .NE. NBCMPJ ) THEN
            CALL UTMESS('F','OP0132','IL FAUT AUTANT DE CMP EN I ET J')
         ENDIF
         IF ( NBCMPI .NE. NBINDI ) THEN
         CALL UTMESS('F','OP0132','IL FAUT AUTANT DE CMP QUE DE NOEUD')
         ENDIF
      ENDIF
      IF ( NBINDJ .NE. NBINDI ) THEN
         CALL UTMESS('F','OP0132','IL FAUT AUTANT D INDICES EN I ET J')
      ENDIF
C
      CALL INFMAJ
      CALL INFNIV ( IFM , NIV )
C
C---1.2--TYPE DE RESULTAT SOUHAITE
C
C---1.2.1-REPARTITION DE PICS DE RAYLEIGH
C
      ITM = 0
      CALL GETFAC ( 'RAYLEIGH' , NBOCC )
      IF ( NBOCC .EQ. 1 ) THEN
         ITM = ITM + 1
         TYCAL(ITM) = 'RAYLEIGH'
      ENDIF
C
C---1.2.2-REPARTITION DE PICS NORMALE
C
      CALL GETFAC ( 'GAUSS' , NBOCC )
      IF ( NBOCC .EQ. 1 ) THEN
         ITM = ITM + 1
         TYCAL(ITM) = 'GAUSS'
      ENDIF
C
C---1.2.3-LOI DE PROBABILITE CUMULEE DE VANMARCKE
C
      CALL GETFAC ( 'VANMARCKE' , NBOCC )
      IF ( NBOCC .EQ. 1 ) THEN
         ITM = ITM + 1
         TYCAL(ITM) = 'VANMARCKE'
         CALL GETVR8('VANMARCKE','DUREE',1,1,1,DUREE(ITM),IBID)
      ENDIF
C
C---1.2.4-DEPASSEMENT DE SEUILS
C
      CALL GETFAC ( 'DEPASSEMENT' , NBOCC )
      IF ( NBOCC .EQ. 1 ) THEN
         ITM = ITM + 1
         TYCAL(ITM) = 'DEPASSEMENT'
         CALL GETVR8('DEPASSEMENT','DUREE',1,1,1,DUREE(ITM),IBID)
      ENDIF
C
C---- BANDE DE FREQUENCE ET PAS
C
      IF ( ITM .NE. 0 ) THEN
         DO 124 I=1,ITM
            CALL GETVR8(TYCAL(I),'VALE_MIN',1,1,0,RBID,IBID)
            IF ( IBID .NE. 0 ) THEN
               CALL GETVR8(TYCAL(I),'VALE_MIN',1,1,1,VALMIN(I),IBID)
               CALL GETVR8(TYCAL(I),'VALE_MAX',1,1,1,VALMAX(I),IBID)
               IFVMIN(I) = 1
            ELSE
               IFVMIN(I) = 0
            ENDIF
            CALL GETVR8(TYCAL(I),'PAS',1,1,0,PAS(I),IBID)
            IF ( IBID .NE. 0 ) THEN
               CALL GETVR8(TYCAL(I),'PAS',1,1,1,PAS(I),IBID)
               IFPAS(I) = 1
            ELSE
               IFPAS(I) = 0
            ENDIF
 124     CONTINUE
      ENDIF
C
C---1.4---LISTE  DES MOMENTS SPECTRAUX
C
      CALL GETVIS(' ','MOMENT',1,1,0,IBID1,NMOMEN)
      NMOMEN = -NMOMEN
      NMOMEN = NMOMEN + 5
      CALL WKVECT('&&OP0132.LISTE_MOMENTS','V V I',NMOMEN,ILNUMO)
      CALL WKVECT('&&OP0132.VALE_MOMENTS','V V R8',NMOMEN,IVALMO)
      ZI(ILNUMO  ) = 0
      ZI(ILNUMO+1) = 1
      ZI(ILNUMO+2) = 2
      ZI(ILNUMO+3) = 3
      ZI(ILNUMO+4) = 4
C
      CALL WKVECT ( '&&OP0132.VALSTAT', 'V V R8' , NMOMEN+5, IVALST )
      CALL WKVECT ( '&&OP0132.KVALST' , 'V V K16', NMOMEN+5, IKVAL  )
      CALL WKVECT ( '&&OP0132.TYP_PAR', 'V V K8' , NMOMEN+5, IKPAR  )
C
      NBPAR = 10
      DO 129 IK = 1 , NBPAR
         ZK16(IKVAL-1+IK) = KVALST(IK)
         ZK8 (IKPAR-1+IK) = 'R'
 129  CONTINUE
      IF ( NMOMEN .GT. 5 ) THEN
         NMOM = NMOMEN - 5
         NBPAR = NBPAR + NMOM
         CALL GETVIS ( ' ', 'MOMENT', 1,1,NMOM, ZI(ILNUMO+5), IBID )
         DO 125 I=1, NMOM
            WRITE (ZK16(IKVAL+9+I),'(A7,I2.2)') 'LAMBDA_',ZI(ILNUMO+4+I)
            ZK8 (IKPAR+9+I) = 'R'
 125     CONTINUE
      ENDIF
C
C
C---1.3---DONNEES INTERSPECTRES :
C
C   1.3.1--REPERER LE TYPE DE INTERSPECTRE   ET SON NOM
C                1- CONCEPT INTERSPECTRE
C                2- TABLE DE TABLE D INTERSPECTRE
C
      CALL GETVID ( ' ', 'INTE_SPEC', 1,1,1, INTESP, IBID)
      CALL TBEXP2(INTESP,'FONCTION')
C
      TABL = .FALSE.
      CALL TBEXIP ( INTESP, 'NUME_VITE_FLUI', EXISP ,TYPPAR)
      IF ( EXISP ) TABL = .TRUE.
C
      IF ( TABL ) THEN
         CALL GETVTX (' ','TOUT_ORDRE',1,1,0,K8B,ITTORD)
         ITTORD = -ITTORD
         IF ( ITTORD .EQ. 1 ) THEN
            NOMOBJ = '&&OP0132.TEMP.NUOR'
            CALL TBEXVE (INTESP,'NUME_VITE_FLUI',NOMOBJ,'V',NBV,K8B)
            CALL JEVEUO ( NOMOBJ, 'L', JNUOR )
            CALL ORDIS  ( ZI(JNUOR) , NBV )
            CALL WKVECT ( '&&OP0132.VITE', 'V V I', NBV, JVITE )
            NNN = 1
            ZI(JVITE) = ZI(JNUOR)
            DO 40 I = 2 , NBV
               IF ( ZI(JNUOR+I-1) .EQ. ZI(JVITE+NNN-1) ) GOTO 40
               NNN = NNN + 1
               ZI(JVITE+NNN-1) = ZI(JNUOR+I-1)
 40         CONTINUE
            NBV = NNN
         ELSE
            NBV = 1
            NOMOBJ = '&&OP0132.TEMP.NUOR'
            CALL WKVECT ( NOMOBJ, 'V V I', 1, JVITE )
            CALL GETVIS(' ','NUME_VITE_FLUI',1,1,1,ZI(JVITE),L)
         ENDIF
C
         CALL TBEXP2(INTESP,'NOM_CHAM')
         CALL TBEXP2(INTESP,'VITE_FLUIDE')
         CALL TBLIVA ( INTESP, 0, K8B, IBID, R8B, C16B, K8B, K8B, R8B,
     +                  'NOM_CHAM', K8B, IBID, R8B, C16B, NOCHAM, IRET )
         IF ( IRET .NE. 0 ) CALL UTMESS('F','OP0132','Y A UN BUG 1' )
         SPEGRD = 'DSP_'//NOCHAM(1:4)
C
      ELSE
        NBV = 1
        SPEGRD = 'DSP'
      ENDIF
C
C---1.3.2 -- REPERER LES COUPLES D'INDICES SELECTIONNES
C (ON A PREALABLEMENT VERIFIE
C                       L EGALITE DU NOMBRE D INDICES EN I ET J)
      LINDI = .TRUE.
      NBINDI = 0
      CALL GETVIS(' ','NUME_ORDRE_I',1,1,0,INDI,NBINM)
      IF ( NBINM .NE. 0 ) THEN
         NBINDI = -NBINM
         CALL WKVECT('&&OP0132.LISTENUM_I','V V I',NBINDI,IAINDI)
         CALL WKVECT('&&OP0132.LISTENUM_J','V V I',NBINDI,IAINDJ)
         CALL GETVIS(' ','NUME_ORDRE_I',1,1,NBINDI,ZI(IAINDI),L)
         CALL GETVIS(' ','NUME_ORDRE_J',1,1,NBINDI,ZI(IAINDJ),L)
      ENDIF
      CALL GETVID(' ','NOEUD_I',1,1,0,K8B,NBINN)
      IF ( NBINN .NE. 0 ) THEN
         LINDI = .FALSE.
         NBINDI = -NBINN
         CALL WKVECT('&&OP0132.LISTENUM_I','V V K8',NBINDI,IAINDI)
         CALL WKVECT('&&OP0132.LISTENUM_J','V V K8',NBINDI,IAINDJ)
         CALL WKVECT('&&OP0132.LISTECMP_I','V V K8',NBINDI,IACMPI)
         CALL WKVECT('&&OP0132.LISTECMP_J','V V K8',NBINDI,IACMPJ)
         CALL GETVID(' ', 'NOEUD_I'  , 1,1,NBINDI, ZK8(IAINDI),L)
         CALL GETVID(' ', 'NOEUD_J'  , 1,1,NBINDI, ZK8(IAINDJ),L)
         CALL GETVTX(' ', 'NOM_CMP_I', 1,1,NBINDI, ZK8(IACMPI),L)
         CALL GETVTX(' ', 'NOM_CMP_J', 1,1,NBINDI, ZK8(IACMPJ),L)
      ENDIF
C
C
C---1.3.3-CAS DE TOUS LES INDICES CENTRAUX
C
      CALL GETVTX ( ' ', 'OPTION', 1,1,0, K8B, IFTOI )
      IF ( IFTOI .NE. 0 ) THEN
         CALL TBEXIP ( INTESP, 'NUME_ORDRE_I', EXISP ,TYPPAR)
         IF ( EXISP ) THEN
            NOMOB2 = '&&OP0132.NUME_MODE'
            CALL TBEXVE (INTESP,'NUME_ORDRE_I',NOMOB2,'V',NBMR,K8B)
            CALL JEVEUO ( NOMOB2, 'L', INUOR )
            CALL ORDIS  ( ZI(INUOR) , NBMR )
            CALL WKVECT ( '&&OP0132.MODE', 'V V I', NBMR, IMODE )
            NNN = 1
            ZI(IMODE) = ZI(INUOR)
            DO 20 I = 2 , NBMR
               IF ( ZI(INUOR+I-1) .EQ. ZI(IMODE+NNN-1) ) GOTO 20
               NNN = NNN + 1
               ZI(IMODE+NNN-1) = ZI(INUOR+I-1)
 20         CONTINUE
            NBINDI = NNN
            CALL WKVECT('&&OP0132.LISTEINDI_I','V V I',NBINDI,IAINDI)
            CALL WKVECT('&&OP0132.LISTEINDI_J','V V I',NBINDI,IAINDJ)
            DO 101 I1 = 1,NBINDI
               ZI(IAINDI-1+I1) = ZI(IMODE+I1-1)
               ZI(IAINDJ-1+I1) = ZI(IMODE+I1-1)
  101       CONTINUE
            CALL JEDETR ( NOMOB2 )
         ELSE
            LINDI = .FALSE.
            IF ( TABL ) THEN
               NEWTAB = '&&OP0132.NEW_TABL'
               CALL TBEXTB ( INTESP, 'V', NEWTAB, 1, 'NUME_VITE_FLUI',
     +                       'EQ', ZI(JVITE), R8B, C16B, K8B, R8B, K8B )
            ELSE
               NEWTAB = INTESP
            ENDIF
            NOMOB1 = '&&OP0132.NOEUD_I'
            CALL TBEXVE ( NEWTAB, 'NOEUD_I', NOMOB1, 'V', NBNI, K8B)
            CALL JEVEUO ( NOMOB1, 'L', JNOEI )
            NOMOB2 = '&&OP0132.CMP_I'
            CALL TBEXVE ( NEWTAB, 'NOM_CMP_I', NOMOB2, 'V', NBCI, K8B)
            IF (NBNI.NE.NBCI) CALL UTMESS('F','OP0132','Y A UN BUG 2')
            CALL JEVEUO ( NOMOB2, 'L', JCMPI )
            NOMOB3 = '&&OP0132.NOEUD_J'
            CALL TBEXVE ( NEWTAB, 'NOEUD_J', NOMOB3, 'V', NBNJ, K8B)
            CALL JEVEUO ( NOMOB3, 'L', JNOEJ )
            NOMOB4 = '&&OP0132.CMP_J'
            CALL TBEXVE ( NEWTAB, 'NOM_CMP_J', NOMOB4, 'V', NBCJ, K8B)
            IF (NBNJ.NE.NBCJ) CALL UTMESS('F','OP0132','Y A UN BUG 3')
            IF (NBNI.NE.NBNJ) CALL UTMESS('F','OP0132','Y A UN BUG 4')
            CALL JEVEUO ( NOMOB4, 'L', JCMPJ )
            CALL WKVECT('&&OP0132.LISTEINDI_I','V V K8',NBNI,IAINDI)
            CALL WKVECT('&&OP0132.LISTEINDI_J','V V K8',NBNI,IAINDJ)
            CALL WKVECT('&&OP0132.LISTECMP_I' ,'V V K8',NBNI,IACMPI)
            CALL WKVECT('&&OP0132.LISTECMP_J' ,'V V K8',NBNI,IACMPJ)
            NBINDI = 0
            DO 30 I = 1 , NBNI
               K16I = ZK8(JNOEI+I-1)//ZK8(JCMPI+I-1)
               K16J = ZK8(JNOEJ+I-1)//ZK8(JCMPJ+I-1)
               IF ( K16I .EQ. K16J ) THEN
                  NBINDI = NBINDI + 1
                  ZK8(IAINDI+NBINDI-1) = ZK8(JNOEI+I-1)
                  ZK8(IAINDJ+NBINDI-1) = ZK8(JNOEJ+I-1)
                  ZK8(IACMPI+NBINDI-1) = ZK8(JCMPI+I-1)
                  ZK8(IACMPJ+NBINDI-1) = ZK8(JCMPJ+I-1)
               ENDIF
 30         CONTINUE
            CALL JEDETR ( NOMOB1 )
            CALL JEDETR ( NOMOB2 )
            CALL JEDETR ( NOMOB3 )
            CALL JEDETR ( NOMOB4 )
            IF ( TABL ) CALL JEDETR ( NEWTAB )
            IF (NBINDI.EQ.0) CALL UTMESS('F','OP0132','Y A UN BUG 5')
         ENDIF
      ENDIF
C
      IF(LINDI)THEN
         CALL TBEXP2(INTESP,'NUME_ORDRE_I')
         CALL TBEXP2(INTESP,'NUME_ORDRE_J')
      ELSE
         CALL TBEXP2(INTESP,'NOEUD_I')
         CALL TBEXP2(INTESP,'NOM_CMP_I')
         CALL TBEXP2(INTESP,'NOEUD_J')
         CALL TBEXP2(INTESP,'NOM_CMP_J')
      ENDIF


C---2.----PREPARATION DU CALCUL DES RESULTATS DEMANDES
C         LES RESULTATS SONT STOCKES DANS UNE TABLE DE DEUX TABLES
C                 1 - DE VALEURS REELLES
C                 2 - DE K24 : TYPAGE + FONCTIONS
C
      CALL TBCRSD ( NOMRES, 'G' )
      IF ( TABL ) THEN
         NBI = 3
         IP = 1
         II = 2
         JJ = 3
      ELSE
         NBI = 2
         IP = 2
         II = 1
         JJ = 2
      ENDIF
      IF ( .NOT. LINDI ) NBI = NBI + 2
      IF ( LINDI ) THEN
         CALL TBAJPA ( NOMRES, NBI, NOPART(IP), TYPART(IP) )
       ELSE
         CALL TBAJPA ( NOMRES, NBI, NOPATN(IP), TYPATN(IP) )
      ENDIF
      CALL TBAJPA ( NOMRES, NBPAR, ZK16(IKVAL), ZK8(IKPAR)  )
      CALL TBAJPA ( NOMRES, 5, KPARFO, TYPAFO  )
C
C --- BOUCLE SUR LES TABLES
C
      DO 202 IV = 1 , NBV
C
         IF ( NIV .GE. 2 ) THEN
            WRITE (IFM,'(A40)' ) '                                    '
      WRITE (IFM,'(A32,A8)')'POUR LA MATRICE INTERSPECTRALE  ',INTESP
         ENDIF
         IF ( TABL ) THEN
            IVIT = ZI(JVITE+IV-1)
            IVAL(1) = IVIT
         IF ( NIV .GE. 2 ) 
     +      WRITE (IFM,'(A20,A24)') 'DE GRANDEUR :        ',NOCHAM
C
            CALL TBLIVA ( INTESP, 1, 'NUME_VITE_FLUI', IV, R8B, C16B,
     +                       K8B, K8B, R8B, 'VITE_FLUIDE', K8B,
     +                       IBID, R8B, C16B, K8B, IRET )
         IF ( NIV .GE. 2 )  WRITE (IFM,'(A30,D13.4,A4)')
     +                   'POUR UNE VITESSE D''ECOULEMENT ',R8B,' M/S'
         ENDIF
C
C-----BOUCLE SUR TOUS LES COUPLES D INDICES
C
         DO 201 I2 = 1,NBINDI

            ECART  = 0.D0
            XNMOYX = 0.D0
            XNPASZ = 0.D0
            XIREG  = 0.D0
C
            IF ( LINDI ) THEN
               INDI = ZI(IAINDI-1+I2)
               INDJ = ZI(IAINDJ-1+I2)
               IF ( INDI .GT. INDJ ) THEN
          CMESS='LA MATRICE EST TRIANGULAIRE SUPERIEUR-INVERSION INDICE'
                 CALL UTMESS ('A','OP0132',CMESS)
                  IPASS = INDI
                  INDI  = INDJ
                  INDJ  = IPASS
               ENDIF
               IF ( NIV .GE. 2 ) THEN
                  WRITE (IFM,'(A9,I4,A1,I4)') 'INDICES :',INDI,'-',INDJ
               ENDIF
C
            ELSE
               KINDI = ZK8(IAINDI-1+I2)
               KINDJ = ZK8(IAINDJ-1+I2)
               IF ( NIV .GE. 2 ) THEN
                  WRITE (IFM,'(A9,A8,A8,A1,A8,A8)') 'INDICES :',
     +                KINDI,ZK8(IACMPI-1+I2),'-',KINDJ,ZK8(IACMPJ-1+I2)
               ENDIF
            ENDIF
C
            IF ( LINDI ) THEN
               IVAL(II) = INDI
               IVAL(JJ) = INDJ
               CALL TBAJLI ( NOMRES, NBI, NOPART(IP),
     +                                    IVAL, R8B, C16B, K8B, 0 )
               CALL TBNULI ( NOMRES, NBI, NOPART(IP),
     +                          IVAL, R8B, C16B, K8B, R8B, K8B, ILIGN )
               IF (ILIGN.LE.0) CALL UTMESS('F','OP0132','Y A UN BUG 6')
               CALL TBLIVA ( INTESP, NBI, NOPART(IP), IVAL, R8B, C16B,
     +                          K8B, K8B, R8B, 'FONCTION',
     +                          K8B, IBID, R8B, C16B, K24B, IRET )
               IF (IRET.NE.0) CALL UTMESS('F','OP0132','Y A UN BUG 7')
            ELSE
               KVAL(1) = KINDI
               KVAL(2) = ZK8(IACMPI-1+I2)
               KVAL(3) = KINDJ
               KVAL(4) = ZK8(IACMPJ-1+I2)
               CALL TBAJLI ( NOMRES, NBI, NOPATN(IP),
     +                                    IVAL, R8B, C16B, KVAL, 0 )
               CALL TBNULI ( NOMRES, NBI, NOPATN(IP),
     +                          IVAL, R8B, C16B, KVAL, R8B, K8B, ILIGN )
               IF (ILIGN.LE.0) CALL UTMESS('F','OP0132','Y A UN BUG 8')
               CALL TBLIVA ( INTESP, NBI, NOPATN(IP), IVAL, R8B, C16B,
     +                          KVAL, K8B, R8B, 'FONCTION',
     +                          K8B, IBID, R8B, C16B, K24B, IRET )
               IF (IRET.NE.0) CALL UTMESS('F','OP0132','Y A UN BUG 9')
            ENDIF
C
            KFOKIJ = K24B(1:19)//'.VALE'
            CALL JEVEUO ( KFOKIJ, 'L', IAVALE )
            CALL JELIRA ( KFOKIJ, 'LONUTI', NPINTE, K8B )
C
C---3.----CALCUL DES  RESULTATS
C
C---3.1---MOMENTS SPECTRAUX
C
           DO 301,IMOMEN = 1,NMOMEN
              ICOEF = ZI(ILNUMO+IMOMEN-1)
              Y1 = ZR(IAVALE+NPINTE/3)
              X1 = ZR(IAVALE)
              FREZ = X1
              XW1 = ZR(IAVALE)*PI*2.D0
              VALMOM = 0.D0
              DO 302,IPOINT = 2,NPINTE/3
                 Y2 = ZR(IAVALE-1+NPINTE/3+2* (IPOINT-1)+1)
                 X2 = ZR(IAVALE-1+IPOINT)
                 XW2 = ZR(IAVALE-1+IPOINT)*PI*2.D0
                 IF ( ICOEF .EQ. 0 ) THEN
                    VALMOM = VALMOM + (Y1+Y2)/2.D0* (X2-X1)
                 ELSE
                    VALMOM = VALMOM + (Y1*(XW1**ICOEF)+Y2*(XW2**ICOEF))*
     +                                    (X2-X1)/2.D0
                 ENDIF
                 Y1 = Y2
                 X1 = X2
                 XW1 = XW2
  302         CONTINUE
              ZR(IVALMO+IMOMEN-1) = VALMOM
  301      CONTINUE
C
C---3.2---FONCTIONS STATISTIQUES
C
C
C---3.2.1-INITIALISATIONS
C
           XM0 = ZR(IVALMO+0)
           XM1 = ZR(IVALMO+1)
           XM2 = ZR(IVALMO+2)
           XM3 = ZR(IVALMO+3)
           XM4 = ZR(IVALMO+4)
C CAS 'TTA' =TABL, SEULE LA PARTIE POSITIVE DU SPECTRE EST UTILISEE
C IL FAUT DONC DOUBLER LAMBDA  POUR CALCULER LE BON ECART TYPE
           IF ( TABL .OR. (FREZ.GE.0.D0) ) THEN
              ECART = SQRT(2*XM0)
           ELSE
C             --- POUR LES CALCULS PROVENANT DE L'ANALYSE SISMIQUE,
C                 DONT LE SPECTRE EST PRIS EN ENTIER
              ECART = SQRT(XM0)
           ENDIF
           IF ( ABS(XM2) .GE. EPSMIN ) THEN
              XNMOYX = 1.D0/PI*SQRT(XM4/XM2)
           ENDIF
C
           IF ( ABS(XM0) .GE. EPSMIN ) THEN
              XNPASZ = 1.D0/PI*SQRT(XM2/XM0)
              FMOY = 0.5D0*XNPASZ
              IF ( ABS(XM4) .GE. EPSMIN ) THEN
                 XIREG = SQRT( XM2*XM2/XM0/XM4)
              ENDIF
           ENDIF
C
           ZR(IVALST    ) = XM0
           ZR(IVALST + 1) = XM1
           ZR(IVALST + 2) = XM2
           ZR(IVALST + 3) = XM3
           ZR(IVALST + 4) = XM4
           ZR(IVALST + 5) = ECART
           ZR(IVALST + 6) = XNMOYX
           ZR(IVALST + 7) = XNPASZ
           ZR(IVALST + 8) = FMOY
           ZR(IVALST + 9) = XIREG
           IF ( NMOMEN .GT. 5 ) THEN
              DO 310 I=1,NMOM
                 ZR(IVALST+9+I) = ZR(IVALMO+4+I)
 310          CONTINUE
           ENDIF
C
           CALL TBAJLI ( NOMRES, NBPAR, ZK16(IKVAL),
     +                        IBID, ZR(IVALST), C16B, K8B, ILIGN )
C
           CALL TBAJLI ( NOMRES, 1, 'GRANDEUR',
     +                        IBID, R8B, C16B, SPEGRD, ILIGN )
C
C------DANS LE CAS OU AUCUN TRAITEMENT A ETE DEMANDE
C
           IF ( ITM .EQ. 0 ) GOTO 201
C
           IF (ABS(XM0).LE.EPSMIN.OR.ABS(XM2).LE.EPSMIN) THEN
              WRITE (IFM,*)
     +    ' >>> ATTENTION DES MOMENTS SPECTRAUX SONT NULS  ON ARRETE LE
     +CALCUL POUR CETTE PAIRE D INDICES<<<'
              GO TO 201
           ENDIF
C
C---------VALEURS INITIALE,FINALE ET PAS
C
           DO 305 IT = 1 , ITM
              IF ( IFVMIN(IT) .EQ. 0 ) THEN
                 VALMIN(IT) = 0.D0
                 VALMAX(IT) = 6*SQRT(XM0)
              ENDIF
              IF ( IFPAS(IT) .EQ. 0 ) THEN
                 PAS(IT) = (VALMAX(IT)-VALMIN(IT))/200.D0
              ENDIF
C
              XNPOIN = (VALMAX(IT)-VALMIN(IT))/PAS(IT)
              NBPOIN = INT(XNPOIN) + 1
C
              IF ( NIV .GE. 2 ) THEN
                 WRITE (IFM,'(A17,A16)') 'TYPE DE RESULTAT:',TYCAL(IT)
              ENDIF
C
              IF ( LINDI ) THEN
                 WRITE(NOMFON,'(A8,A1,A6,2I2.2)') NOMRES(1:8),'_',
     +                                      TYCAL(IT)(1:6), INDI,INDJ
              ELSE
                 WRITE(NOMFON,'(A8,A1,A6,2I2.2)') NOMRES(1:8),'_',
     +                                      TYCAL(IT)(1:6), IT,IT
              ENDIF
C
              CALL WKVECT(NOMFON//'.VALE','G V R8',2*NBPOIN,IADVIJ)
              CALL WKVECT(NOMFON//'.PROL','G V K16',5,IADPRO)
C
              ZK16(IADPRO  ) = 'FONCTION'
              ZK16(IADPRO+1) = 'LIN LIN '
              ZK16(IADPRO+2) = 'PARAM   '
              ZK16(IADPRO+3) = 'PROBA/PICS'
              ZK16(IADPRO+4) = 'EL      '
C
C---3.2.3-LOIS STATISTIQUES
C
C---- CALCUL DE LA VALEUR P+Q
C
              IF ( TYCAL(IT) .EQ. 'VANMARCKE' ) THEN
                 XDELTA = SQRT(ABS(1.D0-XM1*XM1/XM0/XM2))
                 XH = SQRT(PI/2.D0)*XDELTA**1.2D0
                 XNU = XNPASZ
                 IF ( XDELTA .LT. 0.62D0 ) THEN
                    XNUE = XNU* (1.63D0* (XDELTA**0.45D0)-0.38D0)
                 ELSE
                    XNUE = XNU
                 ENDIF
                 XNUTO = 2.D0*LOG(XNUE*DUREE(IT))
                 XP = SQRT(XNUTO) + 0.5772D0/SQRT(XNUTO)
                 XQ = 1.2D0/SQRT(XNUTO) - 5.4D0/ (13.D0+ (XNUTO**3.2D0))
                 XPQ = (XP+XQ)*ECART
                 IF ( NIV .GE. 2 ) THEN
                    WRITE (IFM,'(A24,A1,E15.6)') 'XP',':',XP
                    WRITE (IFM,'(A24,A1,E15.6)') 'XQ',':',XQ
               WRITE (IFM,'(A24,A1,E15.6)') 'VALEUR P+Q (DER KIUR)',':',
     +                                        XPQ
                 ENDIF
              ENDIF
C
              IF ( NIV .GE. 2 ) THEN
                 WRITE (IFM,'(A10,E15.6)') 'VALE_MIN :',VALMIN(IT)
                 WRITE (IFM,'(A10,E15.6)') 'VALE_MAX :',VALMAX(IT)
                 WRITE (IFM,'(A10,E15.6)') 'PAS      :',PAS(IT)
            WRITE (IFM,*) '******************************************'
            WRITE (IFM,*) '   PARAMETRE         RESULTAT             '
            WRITE (IFM,*) '******************************************'
              ENDIF
              DO 303,IPOINT = 1,NBPOIN
                 IF ( IPOINT .EQ. NBPOIN ) THEN
                    X1 = VALMAX(IT)
                 ELSE
                    X1 = VALMIN(IT) + (IPOINT-1)*PAS(IT)
                 ENDIF
C
C---------DEPASSEMENT DE SEUILS
C
                 IF (TYCAL(IT).EQ.'DEPASSEMENT') THEN
                    Y1 = DUREE(IT)*XNPASZ*EXP(-X1*X1/2.D0/XM0)
C
C---------LOI DE PICS DE RAYLEIGH
C
                 ELSEIF (TYCAL(IT).EQ.'RAYLEIGH') THEN
                    Y1 = X1/XM0*EXP(-X1*X1/2.D0/XM0)
C
C---------LOI DE PICS NORMALE
C
                 ELSEIF (TYCAL(IT).EQ.'GAUSS') THEN
                    Y1 = 2.D0/SQRT(2.D0*PI*XM0)*EXP(-X1*X1/2.D0/XM0)
C
C---------LOI DE VANMARCKE
C
                 ELSEIF (TYCAL(IT).EQ.'VANMARCKE') THEN
                    IF ( X1 .LE. EPSMIN*PAS(IT) ) THEN
                       X1 = 0.D0
                       Y1 = 0.D0
                    ELSE
                       XS = X1/SQRT(XM0)
                       IF (XS.GT.10) THEN
                   WRITE (IFM,*) 'VANMARCKE: XC INCALCULABLE XS=',XS
                          GO TO 303
                       ENDIF
                       XA = 1.D0 - EXP(-0.5D0*XS*XS)
                       XB = 1.D0 - EXP(-XH*XS)
                       XC = EXP(0.5D0*XS*XS) - 1.D0
                       Y1 = XA*EXP(-XNU*XB*DUREE(IT)/XC)
                    ENDIF
                 ENDIF
C
                 IF ( NIV .GE. 2 )  WRITE (IFM,'(2E15.6)') X1,Y1
                 ZR(IADVIJ-1+IPOINT) = X1
                 ZR(IADVIJ-1+NBPOIN+IPOINT) = Y1
C
 303          CONTINUE
C
              CALL TBAJLI ( NOMRES, 1, TYCAL(IT),
     +                       IVAL, R8B, C16B, NOMFON, ILIGN )
C
 305       CONTINUE
C
 201     CONTINUE
C
 202  CONTINUE
C
      CALL TITRE
C
      CALL JEDEMA()
      END
