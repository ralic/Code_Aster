      SUBROUTINE OP0114 ( IER )
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER             IER
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 11/03/2003   AUTEUR DURAND C.DURAND 
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
C     INTERFACE ASTER - CLASSI : PROCEDURE  IMPR_CLASSI
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
      CHARACTER*32     JEXNOM, JEXNUM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      PARAMETER    ( NBPARA = 1 )
      INTEGER      APRNO, IDDL, NCMP, NEC, IERD, GD
      CHARACTER*1  K1B
      CHARACTER*4  CTYP
      CHARACTER*8  K8B, RESU, MECA, MASSE, CRIT, NOMA, NOMNOE
      CHARACTER*14 NUME
      CHARACTER*16 CONCEP, NOMCMD , NOMSY
      CHARACTER*19 KVEC, KVAL, KNUME
      CHARACTER*24 NPRNO, DESC, REFE, VALE, ADIA, ABLO, NOPARA(NBPARA)
      CHARACTER*24 REFA
      REAL*8       R8B, UN, COEF, PREC, OMEGA, ZETA
C     ------------------------------------------------------------------
      DATA  DESC  /'                   .DESC'/
      DATA  REFE  /'                   .REFE'/
      DATA  REFA  /'                   .REFA'/
      DATA  VALE  /'                   .VALE'/
      DATA  ADIA  /'                   .ADIA'/
      DATA  ABLO  /'                   .ABLO'/
      DATA  KVEC  /'&&OP0114.VAL_PROPRE'/
      DATA  KVAL  /'&&OP0114.GRAN_MODAL'/
      DATA  KNUME /'&&OP0114.NUME_ORDRE'/
      DATA  NOPARA / 'OMEGA2' /
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFMAJ()
      CALL GETRES( RESU , CONCEP , NOMCMD )
      UN     = 1.D0
      IFR = IUNIFI('RESULTAT')
C
C     ----- RECUPERATION UNITE DE CLASSI ---
      CALL GETVIS(' ','UNITE_CLASSI',1,1,1,IFCLA,NU)
      CALL ASOPEN ( IFCLA, ' ' )
C
C     ----- RECUPERATION DES MODES -----
      CALL GETVID(' ','MODE_MECA',1,1,1,MECA,NMM)
C
      CALL GETVR8 ( ' ', 'PRECISION', 1,1,1, PREC, NP )
      CALL GETVTX ( ' ', 'CRITERE'  , 1,1,1, CRIT, NC )
      CALL RSUTNU ( MECA, ' ', 0, KNUME, NBORDR, PREC, CRIT, IRET )
      IF (IRET.NE.0) GOTO 9999
      CALL JEVEUO ( KNUME, 'L', JORDR )
      REFE(1:8) = MECA
      CALL JEVEUO(REFE,'L',LMODE)
      MASSE = ZK24(LMODE  )
      NOMSY = 'DEPL'
      CALL VPRECU ( MECA, NOMSY, NBORDR,ZI(JORDR), KVEC,
     +              NBPARA, NOPARA, K8B, KVAL, K8B,
     +              NEQ, NBMODE, CTYP, NBPARI, NBPARR, NBPARK )
      CALL JEVEUO(KVEC,'L',LMOD)
      CALL JEVEUO(KVAL,'L',LVAL)
C
C     ----- RECUPERATION DES AMORTISSEMENTS -----
      CALL GETVR8(' ','AMOR',0,1,0,R8B,NA)
      IF (NA.NE.0) THEN
         NBAMOR = -NA
         CALL WKVECT('&&OP0114.AMORTISSEMENT','V V R',NBAMOR,JAMOR)
         CALL GETVR8(' ','AMOR',1,1,NBAMOR,ZR(JAMOR),NA)
         IF (NBAMOR.GT.NBMODE) THEN
            CALL UTDEBM('F',NOMCMD,'TROP D''AMORTISSEMENTS MODAUX')
            CALL UTIMPI('L','   NOMBRE D''AMORTISSEMENT : ',1,NBAMOR)
            CALL UTIMPI('L','   NOMBRE DE MODE : ',1,NBMODE)
            CALL UTFINM( )
         ENDIF
         IF (NBAMOR.LT.NBMODE) THEN
            CALL WKVECT('&&OP0114.AMORTISSEMEN2','V V R',NBMODE,JAMO2)
            DO 10 IAM = 1,NBAMOR
               ZR(JAMO2+IAM-1) = ZR(JAMOR+IAM-1)
 10         CONTINUE
            DO 12 IAM = NBAMOR,NBMODE
               ZR(JAMO2+IAM-1) = ZR(JAMOR+NBAMOR-1)
 12         CONTINUE
            NBAMOR = NBMODE
            JAMOR = JAMO2
         ENDIF
CCC   ELSE
C        --- CAS OU LES AMORTISSEMENTS SONT DONNES SOUS FORME MATRICE

C        DESC(1:8) = AMOGEN

C        NBAMOR = ZI(JDESC+1)
C        VALE(1:8) = AMOGEN
CCC      CALL JEVEUO(VALE,'L',JAMOR)
      ENDIF
      IF (NBAMOR.NE.NBMODE) THEN
         CALL UTDEBM('F',NOMCMD,'IL MANQUE DES AMORTISSEMENTS MODAUX')
         CALL UTIMPI('L','   NOMBRE D''AMORTISSEMENT : ',1,NBAMOR)
         CALL UTIMPI('L','   NOMBRE DE MODE : ',1,NBMODE)
         CALL UTFINM( )
      ENDIF
C
      CALL DISMOI('F','NOM_NUME_DDL',MASSE,'MATR_ASSE',IBID ,NUME,IERD)
      CALL DISMOI('F','NOM_MAILLA'  ,MASSE,'MATR_ASSE',IBID ,NOMA,IERD)
      CALL DISMOI('F','NB_NO_MAILLA',NOMA ,'MAILLAGE' ,NBNOEU,K8B,IERD)
      CALL DISMOI('F','NUM_GD_SI'   ,NUME ,'NUME_DDL' ,GD    ,K8B,IERD)
      CALL WKVECT('&&OP0114.DDL_LAGR','V V I',NEQ,JLAG)
      CALL WKVECT('&&OP0114.DDL_BLOQ','V V I',NEQ,JBLO)
      CALL TYPDDL('BLOQ',NUME,NEQ,ZI(JBLO),NBACT2,NBBLOQ,NBLAG2,NBLIAI)
      CALL TYPDDL('LAGR',NUME,NEQ,ZI(JLAG),NBACT2,NBBLO2,NBLAGR,NBLIAI)
C     --- TABLEAU DES DDL ACTIFS ---
      CALL WKVECT('&&OP0114.DDL_ACTI','V V I',NBNOEU*6,JACTI)
C
C     --- ECRITURE DESCRIPTION NOEUDS STRUCTURE ---
      CALL JEVEUO(NOMA//'.COORDO    .VALE','L',JCOOR)
      NPRNO = NUME//'.NUME.PRNO'
      CALL JENONU(JEXNOM(NPRNO(1:19)//'.LILI','&MAILLA'),IBID)
      CALL JEVEUO(JEXNUM(NPRNO,IBID),'L',APRNO)
      NEC = NBEC(GD)
      IACTI = 0
      DO 20 INOE = 1,NBNOEU
         IDDL = ZI( APRNO + (NEC+2)*(INOE-1) + 1 - 1 ) - 1
         NCMP = ZI( APRNO + (NEC+2)*(INOE-1) + 2 - 1 )
         WRITE( IFCLA , '(3F10.5,6I2)' )
     +                ( ZR(JCOOR+3*(INOE-1)+IN-1) , IN=1,3    ) ,
     +                ( ZI(JBLO+IDDL+IC-1)        , IC=1,NCMP )
         DO 25 IC=1,6
            IF (IC.LE.NCMP) THEN
              IF (ZI(JBLO+IDDL+IC-1).NE.1 .AND.
     +            ZI(JLAG+IDDL+IC-1).NE.1 ) THEN
                IACTI = IACTI + 1
                ZI(JACTI+(INOE-1)*6+IC-1) = IACTI
              ENDIF
            ENDIF
 25      CONTINUE
C
 20   CONTINUE
C
C     --- ECRITURE MATRICE MASSE DIAGONALE ---
      NDDL = NEQ - NBLAGR - NBBLOQ
      CALL WKVECT('&&OP0114.MASSE','V V R',NDDL,JMAS)
C     --- TYPE DE STOCKAGE DE LA MATRICE ---
      REFA(1:8) = MASSE
      CALL JEVEUO( REFA , 'L' , JREF )
C     --- ADRESSE DES TERMES DIAGONAUX ---
      ADIA(1:19) = ZK24(JREF+3-1)
      CALL JEVEUO( ADIA ,'L',JDIA)
C     --- NUMERO DES COLONNES ---
      ABLO(1:19) = ZK24(JREF+3-1)
      CALL JEVEUO( ABLO ,'L',JABL)
C     --- NOMBRE DE BLOC DE LA MATRICE ---
      DESC(1:19) = ZK24(JREF+3-1)
      CALL JEVEUO( DESC ,'L',JDES)
      NBBLOC = ZI(JDES+3-1)
C     --- RECUPERATION DE LA DIAGONALE ---
      VALE(1:8) = MASSE
      IDDL = 0
      DO 30 IB = 1,NBBLOC
         IF ( ZI(JABL+IB-1) .GE. NEQ ) GOTO 34
         CALL JEVEUO(JEXNUM(VALE,IB),'L',JVAL)
         DO 32 IN = ZI(JABL+IB-1)+1,ZI(JABL+IB)
            IF ( IN .GT. NEQ ) GOTO 36
            IF ( ZI(JLAG+IN-1).NE.1 .AND. ZI(JBLO+IN-1).NE.1 ) THEN
               IDDL = IDDL + 1
               ZR(JMAS+IDDL-1) = ZR(JVAL-1+ZI(JDIA+IN-1))
            ENDIF
 32      CONTINUE
 36      CONTINUE
         CALL JELIBE(JEXNUM(VALE,IB))
 30   CONTINUE
 34   CONTINUE
      WRITE( IFCLA , '(1P,8E10.3)' ) ( ZR(JMAS+IN-1) , IN=1,NDDL )
C
C     --- ECRITURE MODES ---
      NDD2 = ( NEQ - NBLAGR ) * NBMODE
      CALL WKVECT('&&OP0114.MODE','V V R',NDD2,JMOD)
      IDDL = 0
      DO 40 IM = 1,NBMODE
         CALL RSADPA(MECA,'L',1,'NORME',ZI(JORDR+IM-1),0,LNORM,K8B)
         IF (ZK24(LNORM)(1:9).EQ.'MASS_GENE') THEN
            COEF = UN
         ELSE
            CALL RSADPA(MECA,'L',1,'MASS_GENE',
     +                                      ZI(JORDR+IM-1),0,LMASG,K8B)
            COEF = UN / SQRT( ZR(LMASG) )
         ENDIF
         DO 42 IN = 1,NEQ
            IF ( ZI(JLAG+IN-1).NE.1 .AND. ZI(JBLO+IN-1).NE.1 ) THEN
               IDDL = IDDL + 1
               ZR(JMOD+IDDL-1) = ZR(LMOD-1+IN+(IM-1)*NEQ) * COEF
            ENDIF
 42      CONTINUE
 40   CONTINUE
      DO 44 IM = 1,NBMODE
         OMEGA = SQRT( ZR(LVAL+IM-1) )
         ZETA  = ZR(JAMOR+IM-1)
         WRITE( IFCLA , '(1P,2E10.3)' ) OMEGA , ZETA
         WRITE( IFCLA , '(1P,8E10.3)' )
     +                ( ZR(JMOD-1+IN+(IM-1)*NDDL) , IN=1,NDDL )
 44   CONTINUE
C
C     --- ECRITURE DES NUMEROS DE DDL ACTIFS ---
      CALL GETFAC('IMPRESSION',NIMPR)
      IF (NIMPR.NE.0) THEN
          WRITE(IFR,1000)
     +             ' IMPR_CLASSI : ECRITURE DES NUMEROS DE DDL ACTIF'
         NDMAX = 0
         NGMAX = 0
         DO 50 IMPR =1,NIMPR
            CALL GETVEM(NOMA,'NOEUD','IMPRESSION','NOEUD',
     +                IMPR,1,0,K8B,N1)
            NDMAX = MAX(NDMAX,-N1)
            CALL GETVEM(NOMA,'GROUP_NO','IMPRESSION','GROUP_NO',
     +                   IMPR,1,0,K8B,N1)
            NGMAX = MAX(NGMAX,-N1)
   50    CONTINUE
         IF (NGMAX .GT. 0) THEN
C        --- VERIFICATION DES GROUPES DE NOEUDS ET COMPTAGE NDMAX ---
           CALL WKVECT ('&&OP0114.IMPRESSION','V V K8',NGMAX,JGROU)
           DO 70 IMPR = 1, NIMPR
             CALL GETVEM(NOMA,'GROUP_NO','IMPRESSION','GROUP_NO',
     +                    IMPR,1,NGMAX,ZK8(JGROU),NGR)
             NOEUGR = 0
             DO 60 IGR = 1, NGR
               CALL JEEXIN (JEXNOM(NOMA//'.GROUPENO',
     +                      ZK8(JGROU+IGR-1)),IRET)
               IF (IRET .EQ. 0) THEN
                 CALL UTMESS('F','IMPRESSION','LE GROUPE '//
     +                       ZK8(JGROU+IGR-1)//
     +                      'NE FAIT PAS PARTIE DU MAILLAGE : '//NOMA)
               ELSE
                 CALL JELIRA (JEXNOM(NOMA//'.GROUPENO',
     +                        ZK8(JGROU+IGR-1)),'LONMAX',N1,K1B)
                 NOEUGR = NOEUGR + N1
               ENDIF
 60          CONTINUE
             NDMAX = MAX(NDMAX,NOEUGR)
 70       CONTINUE
        ENDIF
C
        CALL WKVECT ('&&OP0114.NOEUD','V V K8',NDMAX,JNOEUD)
        CALL WKVECT ('&&OP0114.DDL_K  ','V V K8',6    ,JDDL  )
        CALL WKVECT ('&&OP0114.DDL_I  ','V V I',6    ,JDDLA )
        DO 130 IMPR=1,NIMPR
C       --- LECTURE DES NOEUDS DU MOT CLE NOEUD ---
           CALL GETVEM(NOMA,'NOEUD','IMPRESSION','NOEUD',
     +               IMPR,1,0,K8B,NNO)
           NNO = -NNO
           CALL GETVEM(NOMA,'NOEUD','IMPRESSION','NOEUD',
     +               IMPR,1,NNO,ZK8(JNOEUD),N1)
           DO 80 INO = 1, NNO
              CALL JENONU (JEXNOM(NOMA//'.NOMNOE',ZK8(JNOEUD+INO-1)),
     +                     IRET)
              IF (IRET .EQ. 0) THEN
              CALL UTMESS('F','IMPRESSION','NOEUD'//' '//
     +                    ZK8(JNOEUD+INO-1)//
     +                    'NE FAIT PAS PARTIE DU MAILLAGE : '//NOMA )
              ENDIF
 80        CONTINUE
C       --- LECTURE DES NOEUDS DU MOT CLE GROUP_NO ---
           CALL GETVEM(NOMA,'GROUP_NO','IMPRESSION','GROUP_NO',
     +                  IMPR,1,0,K8B,NGR)
           NGR = -NGR
           CALL GETVEM(NOMA,'GROUP_NO','IMPRESSION','GROUP_NO',
     +                  IMPR,1,NGR,ZK8(JGROU),N1)
           DO 100 J = 1, NGR
              CALL JEVEUO (JEXNOM(NOMA//'.GROUPENO',ZK8(JGROU-1+J)),
     +                     'L',JGR0)
              CALL JELIRA (JEXNOM(NOMA//'.GROUPENO',ZK8(JGROU-1+J)),
     +                     'LONMAX',NOEUGR,K1B)
              DO 90 K = 1, NOEUGR
                 IN = ZI(JGR0-1+K)
                 CALL JENUNO(JEXNUM(NOMA//'.NOMNOE',IN),NOMNOE)
                 NNO = NNO + 1
                 ZK8(JNOEUD+NNO-1) = NOMNOE
  90          CONTINUE
 100       CONTINUE
C
           CALL GETVTX ('IMPRESSION','NOM_CMP',IMPR,1,0,K8B,N2)
           IF (N2.NE.0) THEN
             N2 = -N2
             CALL GETVTX ('IMPRESSION','NOM_CMP',IMPR,1,N2,ZK8(JDDL),N2)
             WRITE(IFR,1010) (ZK8(JDDL+I-1)(1:3),I=1,N2)
           ELSE
             WRITE(IFR,1010)
           ENDIF
           DO 120 I=1,NNO
              CALL JENONU(JEXNOM(NOMA//'.NOMNOE',ZK8(JNOEUD+I-1)),INOE)
              IF (N2.NE.0) THEN
                ICOMPO=0
                DO 110 IC=1,N2
                  IF (ZK8(JDDL+IC-1)(1:2).EQ.'DX') ICOMPO = 1
                  IF (ZK8(JDDL+IC-1)(1:2).EQ.'DY') ICOMPO = 2
                  IF (ZK8(JDDL+IC-1)(1:2).EQ.'DZ') ICOMPO = 3
                  IF (ZK8(JDDL+IC-1)(1:3).EQ.'DRX') ICOMPO = 4
                  IF (ZK8(JDDL+IC-1)(1:3).EQ.'DRY') ICOMPO = 5
                  IF (ZK8(JDDL+IC-1)(1:3).EQ.'DRZ') ICOMPO = 6
                  ZI(JDDLA+IC-1) = ZI(JACTI+(INOE-1)*6+ICOMPO-1)
  110           CONTINUE
                IF (ICOMPO.NE.0)
     +             WRITE(IFR,1020) ZK8(JNOEUD+I-1),
     +                             (ZI(JDDLA+IC-1),IC=1,N2)
              ELSE
                NCMP = ZI( APRNO + (NEC+2)*(INOE-1) + 2 - 1 )
                WRITE(IFR,1020) ZK8(JNOEUD+I-1),
     +                  (ZI(JACTI+(INOE-1)*6+IC-1),IC=1,NCMP)
              ENDIF
  120      CONTINUE
           WRITE(IFR,1030)
  130    CONTINUE
      ENDIF
C
 9999 CONTINUE
      CALL JEDETC('V','&&OP0114',1)
C
 1000 FORMAT(A)
 1010 FORMAT('NOEUD   ',1X,6(7X,A3))
 1020 FORMAT(A8,6I10)
 1030 FORMAT(80X)
C
      CALL JEDEMA()
      END
