      SUBROUTINE RCEV22 ( NBINTI, KINTI, IOCC, CSIGM, CINST, CCONT,
     +                    LFATIG, FLEXIO, LROCHT, CNOC, CRESU, CPRES )
      IMPLICIT      NONE
      INCLUDE 'jeveux.h'
      INTEGER       NBINTI
      LOGICAL       LFATIG, FLEXIO, LROCHT
      CHARACTER*16  KINTI
      CHARACTER*24  CSIGM, CINST, CCONT, CNOC, CRESU, CPRES
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
C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.
C
C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C     ------------------------------------------------------------------
C     OPERATEUR POST_RCCM, TYPE_RESU_MECA='EVOLUTION'
C     LECTURE DU MOT CLE FACTEUR "TRANSITOIRE"
C
C     ------------------------------------------------------------------
C
      INTEGER      IBID, N1, NBINST, KINST, JCONT, JCOFL, NCMPR,
     +             I, J, K, L, NDIM, NBABSC, JABSC, JSIGM, JINST, NCMP,
     +             IRET, JSIOE, IOCC, NBINS0, JNOCC,
     +             JRESU, NBCYCL, JCOPR, JRESP
      PARAMETER  ( NCMP = 6 )
      REAL*8       R8B, PREC(2), MOMEN0, MOMEN1, VALE(2),
     +              R8VIDE
      COMPLEX*16   CBID
      LOGICAL      EXIST, CFAIT
      CHARACTER*8  K8B, CRIT(2), NOCMP(NCMP)
      CHARACTER*16 MOTCLF, VALEK(2), TABLE, TABL0, TABFLE, TABFL0,
     +             TABPRE, TABPR0
      CHARACTER*19 NOMF
      CHARACTER*24 INSTAN, ABSCUR
      CHARACTER*24 VALK(7)
      INTEGER      IARG
C DEB ------------------------------------------------------------------
      CALL JEMARQ()
C
      MOTCLF = 'TRANSITOIRE'
C
      NOCMP(1) = 'SIXX'
      NOCMP(2) = 'SIYY'
      NOCMP(3) = 'SIZZ'
      NOCMP(4) = 'SIXY'
      NOCMP(5) = 'SIXZ'
      NOCMP(6) = 'SIYZ'
C
      VALEK(1) = 'INST            '
      VALEK(2) = 'ABSC_CURV       '
C
      PREC(1) = 1.0D-06
      PREC(2) = 1.0D-06
      CRIT(1) = 'RELATIF'
      CRIT(2) = 'RELATIF'
C
      INSTAN = '&&RCEV22.INSTANT'
      ABSCUR = '&&RCEV22.ABSC_CURV'
C
C --- RECHERCHE DU NOMBRE D'INSTANTS A COMBINER
C
      NBINST = 0
      CFAIT  = .FALSE.
C
      CALL GETVID ( MOTCLF, 'TABL_RESU_MECA', IOCC,IARG,1, TABL0 , N1 )
      CALL GETVID ( MOTCLF, 'TABL_SIGM_THER', IOCC,IARG,1, TABFL0, N1 )
      IF ( N1 .NE. 0 )  FLEXIO = .TRUE.
      CALL GETVID ( MOTCLF, 'TABL_RESU_PRES', IOCC,IARG,1, TABPR0, N1 )
      IF ( N1 .NE. 0 ) THEN
         LROCHT = .TRUE.
         CALL WKVECT ( CPRES, 'V V K8', 1, JRESP )
         ZK8(JRESP-1+1) = TABPR0
      ENDIF
C
      CALL GETVR8 ( MOTCLF, 'INST', IOCC,IARG,1, R8B, N1 )
      IF ( N1 .NE. 0 ) THEN
         NBINS0 = -N1
      ELSE
         CALL GETVID ( MOTCLF, 'LIST_INST', IOCC,IARG,1, NOMF, N1 )
         IF ( N1 .NE. 0 ) THEN
            CALL JELIRA ( NOMF//'.VALE', 'LONMAX', NBINS0, K8B )
         ELSE
            IF ( NBINTI .EQ. 1 ) THEN
               TABLE  = TABL0
               TABFLE = TABFL0
               TABPRE = TABPR0
            ELSE
               CFAIT  = .TRUE.
               TABLE  = '&&RCEV22.RESU_ME'
               TABFLE = '&&RCEV22.SIGM_TH'
               TABPRE = '&&RCEV22.RESU_PR'
               CALL TBEXTB ( TABL0, 'V', TABLE, 1, 'INTITULE', 'EQ',
     +                       IBID, R8B, CBID, KINTI, R8B, K8B, IRET )
               IF ( IRET .EQ. 10 ) THEN
                  VALK(1) = 'INTITULE'
                  VALK(2) = TABL0
                  CALL U2MESK('F', 'UTILITAI7_1',2,VALK)
               ELSEIF ( IRET .EQ. 20 ) THEN
                  VALK(1) = TABL0
                  VALK(2) = 'INTITULE'
                  CALL U2MESK('F', 'UTILITAI7_3',2,VALK)
               ENDIF
               IF ( FLEXIO ) THEN
                  CALL TBEXTB ( TABFL0, 'V', TABFLE, 1, 'INTITULE',
     +                 'EQ', IBID, R8B, CBID, KINTI, R8B, K8B, IRET )
                  IF ( IRET .EQ. 10 ) THEN
                     VALK(1) = 'INTITULE'
                     VALK(2) = TABFL0
                     CALL U2MESK('F', 'UTILITAI7_1',2,VALK)
                  ELSEIF ( IRET .EQ. 20 ) THEN
                     VALK(1) = TABFL0
                     VALK(2) = 'INTITULE'
                     CALL U2MESK('F', 'UTILITAI7_3',2,VALK)
                  ENDIF
               ENDIF
               IF ( LROCHT ) THEN
                  CALL TBEXTB ( TABPR0, 'V', TABPRE, 1, 'INTITULE',
     +                 'EQ', IBID, R8B, CBID, KINTI, R8B, K8B, IRET )
                  IF ( IRET .EQ. 10 ) THEN
                     VALK(1) = 'INTITULE'
                     VALK(2) = TABPR0
                     CALL U2MESK('F', 'UTILITAI7_1',2,VALK)
                  ELSEIF ( IRET .EQ. 20 ) THEN
                     VALK(1) = TABPR0
                     VALK(2) = 'INTITULE'
                     CALL U2MESK('F', 'UTILITAI7_3',2,VALK)
                  ENDIF
               ENDIF
            ENDIF
            CALL TBEXIP ( TABLE, VALEK(1), EXIST, K8B )
            IF ( .NOT. EXIST ) THEN
               VALK (1) = TABLE
               VALK (2) = 'INTITULE'
               VALK (3) = KINTI
               VALK (4) = VALEK(1)
               CALL U2MESG('F', 'POSTRCCM_17',4,VALK,0,0,0,0.D0)
            ENDIF
            CALL TBEXV1 ( TABLE, VALEK(1), INSTAN, 'V', NBINS0, K8B)
            CALL JEDETR ( INSTAN )
         ENDIF
      ENDIF
      NBINST = NBINST + NBINS0
C
C --- PRESENCE DES COMPOSANTES DANS LA TABLE
C
      IF ( .NOT. CFAIT ) THEN
         IF ( NBINTI .EQ. 1 ) THEN
            TABLE  = TABL0
            TABFLE = TABFL0
            TABPRE = TABPR0
         ELSE
            CFAIT  = .TRUE.
            TABLE  = '&&RCEV22.RESU_ME'
            TABFLE = '&&RCEV22.SIGM_TH'
            TABPRE = '&&RCEV22.RESU_PR'
            CALL TBEXTB ( TABL0, 'V', TABLE, 1, 'INTITULE', 'EQ',
     +                    IBID, R8B, CBID, KINTI, R8B, K8B, IRET )
            IF ( IRET .EQ. 10 ) THEN
               VALK(1) = 'INTITULE'
               VALK(2) = TABL0
               CALL U2MESK('F', 'UTILITAI7_1',2,VALK)
            ELSEIF ( IRET .EQ. 20 ) THEN
               VALK(1) = TABL0
               VALK(2) = 'INTITULE'
               CALL U2MESK('F', 'UTILITAI7_3',2,VALK)
            ENDIF
            IF ( FLEXIO ) THEN
               CALL TBEXTB ( TABFL0, 'V', TABFLE, 1, 'INTITULE',
     +             'EQ', IBID, R8B, CBID, KINTI, R8B, K8B, IRET )
               IF ( IRET .EQ. 10 ) THEN
                  VALK(1) = 'INTITULE'
                  VALK(2) = TABFL0
                  CALL U2MESK('F', 'UTILITAI7_1',2,VALK)
               ELSEIF ( IRET .EQ. 20 ) THEN
                  VALK(1) = TABFL0
                  VALK(2) = 'INTITULE'
                  CALL U2MESK('F', 'UTILITAI7_3',2,VALK)
               ENDIF
            ENDIF
            IF ( LROCHT ) THEN
               CALL TBEXTB ( TABPR0, 'V', TABPRE, 1, 'INTITULE',
     +             'EQ', IBID, R8B, CBID, KINTI, R8B, K8B, IRET )
               IF ( IRET .EQ. 10 ) THEN
                  VALK(1) = 'INTITULE'
                  VALK(2) = TABPR0
                  CALL U2MESK('F', 'UTILITAI7_1',2,VALK)
               ELSEIF ( IRET .EQ. 20 ) THEN
                  VALK(1) = TABPR0
                  VALK(2) = 'INTITULE'
                  CALL U2MESK('F', 'UTILITAI7_3',2,VALK)
               ENDIF
            ENDIF
         ENDIF
      ENDIF
C
      NCMPR = 6
      DO 12 I = 1, 4
         CALL TBEXIP ( TABLE, NOCMP(I), EXIST, K8B )
         IF ( .NOT. EXIST ) THEN
            VALK (1) = TABLE
            VALK (2) = 'INTITULE'
            VALK (3) = KINTI
            VALK (4) = NOCMP(I)
            CALL U2MESG('F', 'POSTRCCM_17',4,VALK,0,0,0,0.D0)
         ENDIF
         IF ( FLEXIO ) THEN
            CALL TBEXIP ( TABFLE, NOCMP(I), EXIST, K8B )
            IF ( .NOT. EXIST ) THEN
               VALK (1) = TABFLE
               VALK (2) = 'INTITULE'
               VALK (3) = KINTI
               VALK (4) = NOCMP(I)
               CALL U2MESG('F', 'POSTRCCM_17',4,VALK,0,0,0,0.D0)
            ENDIF
         ENDIF
         IF ( LROCHT ) THEN
            CALL TBEXIP ( TABPRE, NOCMP(I), EXIST, K8B )
            IF ( .NOT. EXIST ) THEN
               VALK (1) = TABPRE
               VALK (2) = 'INTITULE'
               VALK (3) = KINTI
               VALK (4) = NOCMP(I)
               CALL U2MESG('F', 'POSTRCCM_17',4,VALK,0,0,0,0.D0)
            ENDIF
         ENDIF
 12   CONTINUE
      CALL TBEXIP ( TABLE, NOCMP(5), EXIST, K8B )
      IF ( .NOT. EXIST ) NCMPR = 4
C
C --- ON RECUPERE L'ABSC_CURV DANS LA TABLE
C
      CALL TBEXIP ( TABLE, VALEK(2), EXIST, K8B )
      IF ( .NOT. EXIST ) THEN
         VALK (1) = TABLE
         VALK (2) = 'INTITULE'
         VALK (3) = KINTI
         VALK (4) = VALEK(2)
         CALL U2MESG('F', 'POSTRCCM_17',4,VALK,0,0,0,0.D0)
      ENDIF
      CALL TBEXV1 ( TABLE, VALEK(2), ABSCUR, 'V', NBABSC, K8B)
C
      CALL JEVEUO ( ABSCUR, 'L', JABSC )
      CALL WKVECT ( '&&RCEV22.CONTRAINTES', 'V V R', NBABSC, JCONT )
      CALL WKVECT ( '&&RCEV22.CONT_FLEXIO', 'V V R', NBABSC, JCOFL )
      CALL WKVECT ( '&&RCEV22.CONT_PRESSI', 'V V R', NBABSC, JCOPR )
C
C --- CREATION DES OBJETS DE TRAVAIL
C
      NDIM = 6 * NBINST * NCMP
      CALL WKVECT ( CSIGM, 'V V R' , NDIM  , JSIGM )
      CALL WKVECT ( CINST, 'V V R' , NBINST, JINST )
      CALL WKVECT ( CNOC , 'V V I' , NBINST, JNOCC )
      CALL WKVECT ( CRESU, 'V V K8', NBINST, JRESU )
      IF ( LFATIG ) THEN
         NDIM = 2 * NBINST * NCMP
         CALL WKVECT ( CCONT, 'V V R', NDIM, JSIOE )
      ENDIF
C
C --- RECUPERATION DES INFORMATIONS
C
      CALL GETVIS ( MOTCLF, 'NB_OCCUR', IOCC,IARG,1, NBCYCL, N1 )
C
C --- ON RECUPERE LES INSTANTS DANS LA TABLE
C
      CALL GETVR8 ( MOTCLF, 'INST', IOCC,IARG,1, R8B, N1 )
      IF ( N1 .NE. 0 ) THEN
         NBINS0 = -N1
         CALL WKVECT ( INSTAN, 'V V R', NBINS0, KINST )
         CALL GETVR8 ( MOTCLF, 'INST', IOCC,IARG,NBINS0, ZR(KINST), N1)
         CALL GETVR8 ( MOTCLF, 'PRECISION', IOCC,IARG,1, PREC(1), N1 )
         CALL GETVTX ( MOTCLF, 'CRITERE'  , IOCC,IARG,1, CRIT(1), N1 )
      ELSE
         CALL GETVID ( MOTCLF, 'LIST_INST', IOCC,IARG,1, NOMF, N1 )
         IF ( N1 .NE. 0 ) THEN
            CALL JELIRA ( NOMF//'.VALE', 'LONMAX', NBINS0, K8B )
            CALL JEVEUO ( NOMF//'.VALE', 'L', KINST )
            CALL GETVR8 (MOTCLF,'PRECISION',IOCC,IARG,1,
     &                   PREC(1), N1 )
            CALL GETVTX (MOTCLF,'CRITERE',IOCC,IARG,1,
     &                   CRIT(1), N1 )
         ELSE
            PREC(1) = 1.0D-06
            CRIT(1) = 'RELATIF'
            CALL TBEXIP ( TABLE, VALEK(1), EXIST, K8B )
            IF ( .NOT. EXIST ) THEN
               VALK (1) = TABLE
               VALK (2) = 'INTITULE'
               VALK (3) = KINTI
               VALK (4) = VALEK(1)
               CALL U2MESG('F', 'POSTRCCM_17',4,VALK,0,0,0,0.D0)
            ENDIF
            CALL TBEXV1 ( TABLE, VALEK(1), INSTAN, 'V', NBINS0, K8B)
            CALL JEVEUO ( INSTAN, 'L', KINST )
         ENDIF
      ENDIF
C
C ---
      DO 102 I = 1 , NBINS0
C
         ZR (JINST+I-1) = ZR(KINST+I-1)
         ZI (JNOCC-1+I) = NBCYCL
         ZK8(JRESU-1+I) = TABL0
C
         VALE(1) = ZR(KINST+I-1)
C
         DO 104 J = 1 , NCMPR
C
            DO 106 K = 1 , NBABSC
              VALE(2) = ZR(JABSC+K-1)
C
              CALL TBLIVA ( TABLE, 2, VALEK, IBID, VALE,
     +                      CBID, K8B, CRIT, PREC, NOCMP(J),
     +                      K8B, IBID, ZR(JCONT+K-1), CBID, K8B, IRET)
              IF (IRET.NE.0) THEN
                VALK (1) = TABLE
                VALK (2) = NOCMP(J)
                VALK (3) = VALEK(1)
                VALK (4) = VALEK(2)
                CALL U2MESG('F', 'POSTRCCM_2',4,VALK,0,0,2,VALE)
              ENDIF
C
              IF ( FLEXIO ) THEN
                CALL TBLIVA ( TABFLE, 2, VALEK, IBID, VALE,
     +                        CBID, K8B, CRIT, PREC, NOCMP(J), K8B,
     +                        IBID, ZR(JCOFL+K-1), CBID, K8B, IRET)
                IF (IRET.NE.0) THEN
                  VALK (1) = TABFLE
                  VALK (2) = NOCMP(J)
                  VALK (3) = VALEK(1)
                  VALK (4) = VALEK(2)
                  CALL U2MESG('F', 'POSTRCCM_2',4,VALK,0,0,2,VALE)
                ENDIF
              ENDIF
C
              IF ( LROCHT ) THEN
                CALL TBLIVA ( TABPRE, 2, VALEK, IBID, VALE,
     +                        CBID, K8B, CRIT, PREC, NOCMP(J), K8B,
     +                        IBID, ZR(JCOPR+K-1), CBID, K8B, IRET)
                IF (IRET.NE.0) THEN
                  VALK (1) = TABPRE
                  VALK (2) = NOCMP(J)
                  VALK (3) = VALEK(1)
                  VALK (4) = VALEK(2)
                  CALL U2MESG('F', 'POSTRCCM_2',4,VALK,0,0,2,VALE)
                ENDIF
              ENDIF
C
 106        CONTINUE
C
            IF ( LFATIG ) THEN
              L = NCMP*(I-1) + J
              ZR(JSIOE-1+L) = ZR(JCONT)
              L = NCMP*NBINST + NCMP*(I-1) + J
              ZR(JSIOE-1+L) = ZR(JCONT+NBABSC-1)
            ENDIF
C
            CALL RC32MY (NBABSC, ZR(JABSC), ZR(JCONT), MOMEN0, MOMEN1)
            MOMEN1 = 0.5D0*MOMEN1
C
            L = NCMP*(I-1) + J
            ZR(JSIGM-1+L) = MOMEN0
            L = NCMP*NBINST + NCMP*(I-1) + J
            ZR(JSIGM-1+L) = MOMEN1
C
            IF ( FLEXIO ) THEN
              CALL RC32MY (NBABSC,ZR(JABSC),ZR(JCOFL), MOMEN0, MOMEN1)
              MOMEN1 = 0.5D0*MOMEN1
            ELSE
              MOMEN0 = 0.D0
              MOMEN1 = 0.D0
            ENDIF
            L = 2*NCMP*NBINST + NCMP*(I-1) + J
            ZR(JSIGM-1+L) = MOMEN0
            L = 3*NCMP*NBINST + NCMP*(I-1) + J
            ZR(JSIGM-1+L) = MOMEN1
C
            IF ( LROCHT ) THEN
              CALL RC32MY (NBABSC,ZR(JABSC),ZR(JCOPR), MOMEN0, MOMEN1)
              MOMEN1 = 0.5D0*MOMEN1
            ELSE
              MOMEN0 = R8VIDE()
              MOMEN1 = R8VIDE()
            ENDIF
            L = 4*NCMP*NBINST + NCMP*(I-1) + J
            ZR(JSIGM-1+L) = MOMEN0
            L = 5*NCMP*NBINST + NCMP*(I-1) + J
            ZR(JSIGM-1+L) = MOMEN1
C
 104     CONTINUE
C
 102  CONTINUE
C
      CALL JEDETR ( INSTAN )
      CALL JEDETR ( ABSCUR )
      CALL JEDETR ( '&&RCEV22.CONTRAINTES' )
      CALL JEDETR ( '&&RCEV22.CONT_FLEXIO' )
      CALL JEDETR ( '&&RCEV22.CONT_PRESSI' )
      IF ( NBINTI .NE. 1 ) THEN
         CALL DETRSD ( 'TABLE', TABLE )
         IF ( FLEXIO ) CALL DETRSD ( 'TABLE', TABFLE )
         IF ( LROCHT ) CALL DETRSD ( 'TABLE', TABPRE )
      ENDIF
C
      CALL JEDEMA( )
      END
