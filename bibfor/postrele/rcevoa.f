      SUBROUTINE RCEVOA ( TYPTAB, NOMMAT )
      IMPLICIT      NONE
      INCLUDE 'jeveux.h'
      CHARACTER*8         NOMMAT
      CHARACTER*16        TYPTAB
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.
C
C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFAL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.
C
C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C     ------------------------------------------------------------------
C     OPERATEUR POST_RCCM, TYPE_RESU_MECA = 'EVOLUTION'
C                                  OPTION = 'AMORCAGE'
C
C     ------------------------------------------------------------------
C
      INTEGER      N1, IBID, NBTRAN, NBPAR, NBTETA, NBCYCL, NBABSC,
     &             NBINS0, IRET, IND, I, K, L, JNOCK, JNOCL, NK, NL,
     &             JTETA, KINST, JFAIJ, JABSC,I1, I2, IFM, NIV, NDIM,
     &             IOC, IT, IS1, IS2, JINST, JTABL, JNBCY, NBITOT
      REAL*8       R8B, PREC(2), VALE(4), VALRES(4), RIJ, RAPP,
     &             FATOT, FAKL, FAM, THETA, R8DGRD, RCAL,
     &             SITTEF, AAMORC, BAMORC, DAMORC, RAMORC, D,
     &             SITT1, SITT2,FKL,R8PREM
      COMPLEX*16   CBID
      LOGICAL      EXIST, TROUVE
      INTEGER ICODRE(4)
      CHARACTER*8  K8B, NOMRES, CRIT(2), NOMPAR, NOMVAL(4)
      CHARACTER*16 MOTCLF, VALEK(4), TABLE, CONCEP, NOMCMD
      CHARACTER*19 NOMF
      CHARACTER*24 INSTAN, KTHETA, ABSCUR, VALK(7)
C
      INTEGER      NPARM, NPARD
      PARAMETER  ( NPARM=2 , NPARD=2 )
      CHARACTER*8  TYPARM(NPARM), TYPARD(NPARD)
      CHARACTER*16 NOPARM(NPARM), NOPARD(NPARD)
      INTEGER      IARG
      DATA NOPARM / 'THETA', 'FACT_AMORCAGE' /
      DATA TYPARM / 'R', 'R' /
      DATA NOPARD / 'THETA', 'FACT_AMORCAGE' /
      DATA TYPARD / 'R', 'R' /
C DEB ------------------------------------------------------------------
      CALL JEMARQ()
C
      MOTCLF = 'TRANSITOIRE'
      CALL GETFAC ( MOTCLF, NBTRAN )
      IF (NBTRAN.EQ.0) GOTO 9999
C
      CALL GETRES ( NOMRES , CONCEP , NOMCMD )
C
      CALL INFNIV ( IFM, NIV )
C
      CALL TBCRSD ( NOMRES, 'G' )
      IF ( TYPTAB .EQ. 'VALE_MAX' ) THEN
         CALL TBAJPA ( NOMRES, NPARM, NOPARM, TYPARM )
      ELSE
         CALL TBAJPA ( NOMRES, NPARD, NOPARD, TYPARD )
      ENDIF
C
      VALEK(1) = 'ANGLE           '
      VALEK(2) = 'INST            '
      VALEK(3) = 'SIZZ            '
      VALEK(4) = 'ABSC_CURV       '
C
      PREC(1) = 1.0D-06
      PREC(2) = 1.0D-06
      CRIT(1) = 'RELATIF'
      CRIT(2) = 'RELATIF'
C
C --- RECUPERATION DES DONNEES MATERIAU
C
      NBPAR  = 0
      NOMPAR = ' '
      NOMVAL(1) = 'A_AMORC'
      NOMVAL(2) = 'B_AMORC'
      NOMVAL(3) = 'D_AMORC'
      NOMVAL(4) = 'R_AMORC'
      CALL RCVALE ( NOMMAT, 'RCCM', NBPAR, NOMPAR, R8B, 4,
     &                      NOMVAL, VALRES, ICODRE, 2)
      AAMORC = VALRES(1)
      BAMORC = VALRES(2)
      DAMORC = VALRES(3)
      RAMORC = VALRES(4)
C
      KTHETA = '&&RCEVOA.THETA'
      INSTAN = '&&RCEVOA.INSTANT'
      ABSCUR = '&&RCEVOA.ABSC_CURV'
C
C --- LA PREMIERE TABLE DEFINIT LES THETA A TRAITER
C     ON VERIFIE QUE LES ABSC_CURV CORRESPONDENT AU RAMORC
C
      CALL GETVID ( MOTCLF, 'TABL_SIGM_THETA', 1,IARG,1, TABLE , N1 )
      CALL TBEXIP ( TABLE, VALEK(1), EXIST, K8B )
      IF ( .NOT. EXIST ) THEN
         VALK(1) = TABLE
         VALK(2) = VALEK(1)
         CALL U2MESK('F', 'POSTRCCM_1',2,VALK)
      ENDIF
      CALL TBEXV1 ( TABLE, VALEK(1), KTHETA, 'V', NBTETA, K8B )
      CALL JEVEUO ( KTHETA, 'L', JTETA )
C
      CALL TBEXIP ( TABLE, VALEK(4), EXIST, K8B )
      IF ( .NOT. EXIST ) THEN
         VALK(1) = TABLE
         VALK(2) = VALEK(4)
         CALL U2MESK('F', 'POSTRCCM_1',2,VALK)
      ENDIF
      CALL TBEXV1 ( TABLE, VALEK(4), ABSCUR, 'V', NBABSC, K8B )
      CALL JEVEUO ( ABSCUR, 'L', JABSC )
C
C --- VERIFICATION DE LA DISTANCE D
C
      DO 10 IT = 1, NBTETA-1
         THETA = (ZR(JTETA+IT) - ZR(JTETA+IT-1)) * R8DGRD()
         D = ZR(JABSC+IT) - ZR(JABSC+IT-1)
         RCAL = D / (2*SIN(0.5D0*THETA))
         RAPP = ABS( ( RCAL - DAMORC ) / DAMORC )
C ------ TOLERANCE DE 1%
         IF ( RAPP .GT. 0.01D0 ) THEN
            VALE(1) = RCAL
            VALE(2) = DAMORC
            CALL U2MESG('A','POSTRCCM_33',1,TABLE,0,0,2,VALE)
         ENDIF
 10   CONTINUE
C
C --- DETERMINATION DU NOMBRE DE SITUATION
C        = NOMBRE D'INSTANTS DES TRANSITOIRES
C
      CALL JECREC('&&RCEVOA.SITUATION', 'V V R', 'NU',
     &                               'DISPERSE', 'VARIABLE', NBTRAN )
      NBITOT = 0
      DO 20 IOC = 1, NBTRAN
C
        CALL GETVID ( MOTCLF, 'TABL_SIGM_THETA', IOC,IARG,1, TABLE, N1 )
        VALK(1) = TABLE
        DO 22 I1 = 1, 4
           CALL TBEXIP ( TABLE, VALEK(I1), EXIST, K8B )
           IF ( .NOT. EXIST ) THEN
              VALK(2) = VALEK(I1)
              CALL U2MESK('F', 'POSTRCCM_1',2,VALK)
           ENDIF
 22     CONTINUE
C
        CALL GETVR8 ( MOTCLF, 'INST', IOC,IARG,0, R8B, N1 )
        IF ( N1 .NE. 0 ) THEN
          NBINS0 = -N1
          CALL JECROC (JEXNUM('&&RCEVOA.SITUATION',IOC))
          CALL JEECRA (JEXNUM('&&RCEVOA.SITUATION',IOC),
     &                                          'LONMAX', NBINS0, ' ' )
          CALL JEECRA (JEXNUM('&&RCEVOA.SITUATION',IOC),
     &                                          'LONUTI', NBINS0, ' ' )
          CALL JEVEUO (JEXNUM('&&RCEVOA.SITUATION',IOC),'E', KINST )
          CALL GETVR8 ( MOTCLF, 'INST', IOC,IARG,NBINS0,ZR(KINST), N1 )
        ELSE
          CALL GETVID ( MOTCLF, 'LIST_INST', IOC,IARG,1, NOMF, N1 )
          IF ( N1 .NE. 0 ) THEN
            CALL JELIRA ( NOMF//'.VALE', 'LONMAX', NBINS0, K8B )
            CALL JEVEUO ( NOMF//'.VALE', 'L', JINST )
          ELSE
            CALL TBEXV1 ( TABLE, VALEK(2), INSTAN, 'V', NBINS0, K8B)
            CALL JEVEUO ( INSTAN, 'L', JINST )
          ENDIF
          CALL JECROC (JEXNUM('&&RCEVOA.SITUATION',IOC))
          CALL JEECRA (JEXNUM('&&RCEVOA.SITUATION',IOC),
     &                                          'LONMAX', NBINS0, ' ' )
          CALL JEECRA (JEXNUM('&&RCEVOA.SITUATION',IOC),
     &                                          'LONUTI', NBINS0, ' ' )
          CALL JEVEUO (JEXNUM('&&RCEVOA.SITUATION',IOC),'E', KINST )
          DO 24 I = 1, NBINS0
             ZR(KINST-1+I) = ZR(JINST-1+I)
 24       CONTINUE
          CALL JEDETR ( INSTAN )
        ENDIF
        NBITOT = NBITOT + NBINS0
 20   CONTINUE
C
C --- CREATION DES OBJETS DE TRAVAIL
C
      CALL WKVECT ( '&&RCEVOA.TABL_T', 'V V K8' , NBITOT , JTABL )
      CALL WKVECT ( '&&RCEVOA.INST_T', 'V V R'  , NBITOT , JINST )
      CALL WKVECT ( '&&RCEVOA.NBCY_T', 'V V I'  , NBITOT , JNBCY )

      IND = 0
      DO 30 IOC = 1 , NBTRAN
C
         CALL JEVEUO ( JEXNUM('&&RCEVOA.SITUATION',IOC), 'L', KINST )
         CALL JELIRA ( JEXNUM('&&RCEVOA.SITUATION',IOC), 'LONUTI',
     &                                                    NBINS0, K8B )
C
         CALL GETVIS ( MOTCLF, 'NB_OCCUR', IOC,IARG,1, NBCYCL, N1 )
C
         CALL GETVID (MOTCLF,'TABL_SIGM_THETA',IOC,IARG,1,
     &                TABLE, N1 )
C
         DO 32 I = 1 , NBINS0
            IND = IND + 1
            ZK8(JTABL-1+IND) = TABLE
             ZR(JINST-1+IND) = ZR(KINST-1+I)
             ZI(JNBCY-1+IND) = NBCYCL
 32     CONTINUE
 30   CONTINUE
C
C --- CALCUL DU FACTEUR D'AMORCAGE
C
      NDIM = NBITOT * NBITOT
      CALL WKVECT ( '&&RCEVFU.MATR_FA', 'V V R', NDIM,   JFAIJ )
      CALL WKVECT ( '&&RCEVFU.NB_CYCL', 'V V I', NBITOT, JNOCL )
      CALL WKVECT ( '&&RCEVFU.NB_CYCK', 'V V I', NBITOT, JNOCK )
C
      DO 200 IT = 1 , NBTETA
        VALE(1) = ZR(JTETA+IT-1)
        IF ( NIV .EQ. 2 ) THEN
          WRITE(IFM,*) '   '
          WRITE(IFM,*) '--->> ANGLE: ', ZR(JTETA+IT-1)
        ENDIF
C
        DO 210 I1 = 1, NBITOT
C
            TABLE   = ZK8(JTABL-1+I1)
            VALE(2) =  ZR(JINST-1+I1)
            ZI(JNOCK-1+I1) = ZI(JNBCY-1+I1)
            ZI(JNOCL-1+I1) = ZI(JNBCY-1+I1)
C
            CALL TBLIVA ( TABLE, 2, VALEK, IBID, VALE,
     &                    CBID, K8B, CRIT, PREC, VALEK(3),
     &                    K8B, IBID, SITT1, CBID, K8B, IRET)
            IF (IRET.NE.0) THEN
              VALK(1) = TABLE
              VALK(2) = VALEK(3)
              VALK(3) = VALEK(1)
              VALK(4) = VALEK(2)
              CALL U2MESG('F', 'POSTRCCM_2',4,VALK,0,0,2,VALE)
            ENDIF
            SITT1 = ABS(SITT1)
C
            DO 220 I2 = I1+1, NBITOT
C
              TABLE   = ZK8(JTABL-1+I2)
              VALE(2) =  ZR(JINST-1+I2)
C
              CALL TBLIVA ( TABLE, 2, VALEK, IBID, VALE,
     &                      CBID, K8B, CRIT, PREC, VALEK(3),
     &                      K8B, IBID, SITT2, CBID, K8B, IRET)
              IF (IRET.NE.0) THEN
                VALK(1) = TABLE
                VALK(2) = VALEK(3)
                VALK(3) = VALEK(1)
                VALK(4) = VALEK(2)
                CALL U2MESG('F', 'POSTRCCM_2',4,VALK,0,0,2,VALE)
              ENDIF
              SITT2 = ABS(SITT2)
C
              ZR(JFAIJ-1+NBITOT*(I1-1)+I2) = 0.D0
              IF (MAX(SITT1,SITT2) .GT. R8PREM() ) THEN
C ------------calcul du rapport de charge
                RIJ = MIN(SITT1,SITT2) / MAX(SITT1,SITT2)
C ------------calcul de DELTASIGTT efficace
                SITTEF = ABS(SITT1-SITT2) / ( 1.D0 - ( RIJ / RAMORC ))
C ------------calcul du facteur d'amorcage elementaire
                FAM = ( SITTEF / AAMORC ) ** ( -1.D0 / BAMORC )
                ZR(JFAIJ-1+NBITOT*(I1-1)+I2) = FAM
              ENDIF
 220        CONTINUE
 210     CONTINUE
C
         FATOT = 0.D0
C
         IND = 0
 100     CONTINUE
         IND = IND + 1
         IF ( NIV .EQ. 2 ) THEN
           IF ( IND .EQ. 1 ) THEN
             WRITE(IFM,*) 'MATRICE FACTEURS D''AMORCAGE INITIALE'
           ELSE
             WRITE(IFM,*) 'MATRICE FACTEURS D''AMORCAGE MODIFIEE'
           ENDIF
           WRITE(IFM,1010) ( ZI(JNOCL-1+L),L=1,NBITOT )
           DO 700 K = 1 , NBITOT
             I1 = NBITOT*(K-1)
             WRITE(IFM,1000) ZI(JNOCK-1+K),
     &                      (ZR(JFAIJ-1+I1+L),L=1,NBITOT)
 700       CONTINUE
         ENDIF
C
         FAM = 0.D0
         TROUVE = .FALSE.
         DO 110 K = 1 , NBITOT
C
           IF ( ZI(JNOCK-1+K) .EQ. 0 ) GOTO 110
C
           DO 112 L = 1 , NBITOT
C
             IF ( ZI(JNOCL-1+L) .EQ. 0 ) GOTO 112
C
             FAKL = ZR(JFAIJ-1+NBITOT*(K-1)+L)
             IF ( FAKL .GT. FAM ) THEN
                TROUVE = .TRUE.
                FAM  = FAKL
                IS1 = K
                IS2 = L
                NL = ZI(JNOCL-1+L)
                NK = ZI(JNOCK-1+K)
             ENDIF
C
 112       CONTINUE
C
 110     CONTINUE
C
         IF ( TROUVE ) THEN
C
           NBCYCL = MIN( NK , NL )
           FKL = FAM*NBCYCL
           IF ( NIV .EQ. 2 ) THEN
             WRITE(IFM,1020)'=> FACTEUR D''AMORCAGE MAXI: ',FAM,IS1,IS2
             WRITE(IFM,1030) NBCYCL,FKL
           ENDIF
C
C -------- ON CUMULE
C
           FATOT = FATOT + FKL
C
C -------- ON MET A ZERO LES FACTEURS D'AMORCAGE INCRIMINES
C
           ZI(JNOCL-1+IS2) = ZI(JNOCL-1+IS2) - NBCYCL
           ZI(JNOCK-1+IS1) = ZI(JNOCK-1+IS1) - NBCYCL
           ZI(JNOCL-1+IS1) = ZI(JNOCL-1+IS1) - NBCYCL
           ZI(JNOCK-1+IS2) = ZI(JNOCK-1+IS2) - NBCYCL
           DO 40 I = 1 , NBITOT
              IF ( ZI(JNOCK-1+IS1) .EQ. 0) THEN
                 ZR(JFAIJ-1+NBITOT*(IS1-1)+I) = 0.D0
              ENDIF
              IF ( ZI(JNOCL-1+IS2) .EQ. 0 ) THEN
                 ZR(JFAIJ-1+NBITOT*(I-1)+IS2) = 0.D0
              ENDIF
 40        CONTINUE
C
         GOTO 100
C
        ENDIF
C
        IF ( NIV .EQ. 2 )
     &    WRITE(IFM,*)'-->> FACTEUR D''AMORCAGE CUMULE = ', FATOT
C
        VALE(2) = FATOT
C
        IF ( TYPTAB .EQ. 'VALE_MAX' ) THEN
           CALL TBAJLI ( NOMRES,NPARM,NOPARM,IBID,VALE,CBID,K8B,0)
        ELSE
           CALL TBAJLI ( NOMRES,NPARD,NOPARD,IBID,VALE,CBID,K8B,0)
        ENDIF
C
 200  CONTINUE
C
 1000 FORMAT(1P,I10,'|',40(E10.3,'|'))
 1010 FORMAT(1P,' NB_OCCUR ','|',40(I10,'|'))
 1020 FORMAT(1P,A28,E12.5,', LIGNE:',I4,', COLONNE:',I4)
 1030 FORMAT(1P,'   NB_OCCUR = ',I8,', FA_KL = ',E9.2)
C
 9999 CONTINUE
C
      CALL JEDEMA( )
      END
