      SUBROUTINE RC32MU
      IMPLICIT   NONE
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
C     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3200
C     LECTURE DU MOT CLE FACTEUR "RESU_MECA_UNIT"
C
C     ------------------------------------------------------------------
C
      INCLUDE 'jeveux.h'
      INTEGER      IBID, NS(13), NBABSC, JABSC, IRET, JMUNE, JMUNO,
     &             I, J, K, L, NDIM, JCONT, NCMP, JCORP
      PARAMETER  ( NCMP = 6 )
      REAL*8       PREC, MOMEN0, MOMEN1
      COMPLEX*16   CBID
      LOGICAL      EXIST
      CHARACTER*8  K8B, CRIT, NOCMP(NCMP), TBSIG(13)
      CHARACTER*16 MOTCLF, VALEK
      CHARACTER*24 ABSCUR
      CHARACTER*24 VALK(7)
      INTEGER      IARG
C DEB ------------------------------------------------------------------
      CALL JEMARQ()
C
      MOTCLF = 'RESU_MECA_UNIT'
      PREC = 1.0D-06
      CRIT = 'RELATIF'
C
      CALL GETVID ( MOTCLF, 'TABL_PRES', 1,IARG,1, TBSIG(13), NS(13) )
      IF( NS(13) .NE. 0 )THEN
        CALL RCVERI(TBSIG(13))
      ENDIF

      CALL GETVID ( MOTCLF, 'TABL_FX', 1,IARG,1, TBSIG(1), NS(1) )
      IF ( NS(1) .EQ. 0 )
     &   CALL GETVID (MOTCLF,'TABL_FX_TUBU',1,IARG,1,
     &                TBSIG(1), NS(1) )
      IF( NS(1) .NE. 0 )
     &   CALL RCVER1('MECANIQUE',TBSIG(13),TBSIG(1))
      CALL GETVID ( MOTCLF, 'TABL_FY', 1,IARG,1, TBSIG(2), NS(2) )
      IF ( NS(2) .EQ. 0 )
     &   CALL GETVID (MOTCLF,'TABL_FY_TUBU',1,IARG,1,
     &                TBSIG(2), NS(2) )
      IF ( NS(2) .NE. 0 )
     &   CALL RCVER1('MECANIQUE',TBSIG(13),TBSIG(2))
      CALL GETVID ( MOTCLF, 'TABL_FZ', 1,IARG,1, TBSIG(3), NS(3) )
      IF ( NS(3) .EQ. 0 )
     &   CALL GETVID (MOTCLF,'TABL_FZ_TUBU',1,IARG,1,
     &                TBSIG(3), NS(3) )
      IF ( NS(3) .NE. 0 )
     &   CALL RCVER1('MECANIQUE',TBSIG(13),TBSIG(3))
C
      CALL GETVID ( MOTCLF, 'TABL_MX', 1,IARG,1, TBSIG(4), NS(4) )
      IF ( NS(4) .EQ. 0 )
     &   CALL GETVID (MOTCLF,'TABL_MX_TUBU',1,IARG,1,
     &                TBSIG(4), NS(4) )
      IF ( NS(4) .NE. 0 )
     &   CALL RCVER1('MECANIQUE',TBSIG(13),TBSIG(4))
      CALL GETVID ( MOTCLF, 'TABL_MY', 1,IARG,1, TBSIG(5), NS(5) )
      IF ( NS(5) .EQ. 0 )
     &   CALL GETVID (MOTCLF,'TABL_MY_TUBU',1,IARG,1,
     &                TBSIG(5), NS(5) )
      IF ( NS(5) .NE. 0 )
     &   CALL RCVER1('MECANIQUE',TBSIG(13),TBSIG(5))
      CALL GETVID ( MOTCLF, 'TABL_MZ', 1,IARG,1, TBSIG(6), NS(6) )
      IF ( NS(6) .EQ. 0 )
     &   CALL GETVID (MOTCLF,'TABL_MZ_TUBU',1,IARG,1,
     &                TBSIG(6), NS(6) )
      IF ( NS(6) .NE. 0 )
     &   CALL RCVER1('MECANIQUE',TBSIG(13),TBSIG(6))
C
      CALL GETVID ( MOTCLF, 'TABL_FX_CORP', 1,IARG,1, TBSIG(7), NS(7) )
      IF( NS(7) .NE. 0 )THEN
        CALL RCVER1('MECANIQUE',TBSIG(13),TBSIG(7))
      ENDIF
      CALL GETVID ( MOTCLF, 'TABL_FY_CORP', 1,IARG,1, TBSIG(8), NS(8) )
      IF( NS(8) .NE. 0 )THEN
        CALL RCVER1('MECANIQUE',TBSIG(13),TBSIG(8))
      ENDIF
      CALL GETVID ( MOTCLF, 'TABL_FZ_CORP', 1,IARG,1, TBSIG(9), NS(9) )
      IF( NS(9) .NE. 0 )THEN
        CALL RCVER1('MECANIQUE',TBSIG(13),TBSIG(9))
      ENDIF
      CALL GETVID (MOTCLF,'TABL_MX_CORP',1,IARG,1,
     &             TBSIG(10), NS(10) )
      IF( NS(10) .NE. 0 )THEN
        CALL RCVER1('MECANIQUE',TBSIG(13),TBSIG(10))
      ENDIF
      CALL GETVID (MOTCLF,'TABL_MY_CORP',1,IARG,1,
     &             TBSIG(11), NS(11) )
      IF( NS(11) .NE. 0 )THEN
        CALL RCVER1('MECANIQUE',TBSIG(13),TBSIG(11))
      ENDIF
      CALL GETVID (MOTCLF,'TABL_MZ_CORP',1,IARG,1,
     &             TBSIG(12), NS(12) )
      IF( NS(12) .NE. 0 )THEN
        CALL RCVER1('MECANIQUE',TBSIG(13),TBSIG(12))
      ENDIF

C
      CALL WKVECT ( '&&RC3200.CORPS', 'V V L', 1, JCORP )
      IF( NS(10) .NE. 0 )THEN
        ZL(JCORP) = .TRUE.
      ELSE
        ZL(JCORP) = .FALSE.
      ENDIF
C

C
C
C --- ON RECUPERE L'ABSC_CURV DANS LA TABLE 'TABL_MX'
C
      VALEK = 'ABSC_CURV       '
      CALL TBEXIP ( TBSIG(4), VALEK, EXIST, K8B )
      IF ( .NOT. EXIST ) THEN
         VALK (1) = TBSIG(4)
         VALK (2) = VALEK
         CALL U2MESK('F', 'POSTRCCM_1',2,VALK)
      ENDIF
      ABSCUR = '&&RC32MU.ABSC_CURV'
      CALL TBEXV1 ( TBSIG(4), VALEK, ABSCUR, 'V', NBABSC, K8B)
      CALL JEVEUO ( ABSCUR, 'L', JABSC )
C
      CALL WKVECT ( '&&RC32MU.CONTRAINTES', 'V V R', NBABSC, JCONT )
C
      NOCMP(1) = 'SIXX'
      NOCMP(2) = 'SIYY'
      NOCMP(3) = 'SIZZ'
      NOCMP(4) = 'SIXY'
      NOCMP(5) = 'SIXZ'
      NOCMP(6) = 'SIYZ'
C
C --- 13 TABLES A  ( 6 COMPOSANTES + 6 LINEARISEES + 6 M_0 + 6 M_1 )
      NDIM = 13 * ( 6 + 6 + 6 + 6 )
      CALL WKVECT ( '&&RC3200.MECA_UNIT .ORIG', 'V V R', NDIM, JMUNO )
      CALL WKVECT ( '&&RC3200.MECA_UNIT .EXTR', 'V V R', NDIM, JMUNE )
C
C --- LES PROFILS DE CONTRAINTES ISSUS DES CALCULS MECANIQUES UNITAIRES
C
      DO 10 I = 1 , 13
C
         IF ( NS(I) .EQ. 0 ) GOTO 10
C
         CALL TBEXIP ( TBSIG(I), VALEK, EXIST, K8B )
         IF ( .NOT. EXIST ) THEN
            VALK (1) = TBSIG(I)
            VALK (2) = VALEK
            CALL U2MESK('F', 'POSTRCCM_1',2,VALK)
         ENDIF
         DO 12 J = 1 , NCMP
C
            DO 14 K = 1 , NBABSC
               CALL TBLIVA ( TBSIG(I),1,VALEK,IBID,ZR(JABSC+K-1),
     &                       CBID, K8B, CRIT, PREC, NOCMP(J),
     &                       K8B, IBID, ZR(JCONT+K-1), CBID, K8B, IRET)
               IF (IRET.NE.0) THEN
                  VALK (1) = TBSIG(I)
                  VALK (2) = NOCMP(J)
                  VALK (3) = VALEK
                  CALL U2MESG('F','POSTRCCM_44',3,VALK,0,0,
     &                                         1,ZR(JABSC+K-1))
               ENDIF
 14         CONTINUE
C
            L = NCMP*(I-1) + J
            ZR(JMUNO-1+L) = ZR(JCONT)
            ZR(JMUNE-1+L) = ZR(JCONT+NBABSC-1)
C
            CALL RC32MY ( NBABSC, ZR(JABSC), ZR(JCONT), MOMEN0, MOMEN1)
C
            L = 13*NCMP + NCMP*(I-1) + J
            ZR(JMUNO-1+L) = MOMEN0 - 0.5D0*MOMEN1
            ZR(JMUNE-1+L) = MOMEN0 + 0.5D0*MOMEN1
C
            L = 2*13*NCMP + NCMP*(I-1) + J
            ZR(JMUNO-1+L) = MOMEN0
            ZR(JMUNE-1+L) = MOMEN0
C
            L = 3*13*NCMP + NCMP*(I-1) + J
            ZR(JMUNO-1+L) = 0.5D0*MOMEN1
            ZR(JMUNE-1+L) = 0.5D0*MOMEN1
C
 12      CONTINUE
C
 10   CONTINUE
C
      CALL JEDETR ( ABSCUR )
      CALL JEDETR ( '&&RC32MU.CONTRAINTES' )
C
      CALL JEDEMA( )
      END
