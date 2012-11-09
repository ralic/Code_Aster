      SUBROUTINE RC32TH
      IMPLICIT   NONE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
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
C     LECTURE DU MOT CLE FACTEUR "RESU_THER"
C
C     ------------------------------------------------------------------
C
      INCLUDE 'jeveux.h'

      CHARACTER*32 JEXNOM
      INTEGER      IBID, N1, IOCC, NBTHER, NUME, NBINST, JINST,
     +             I, J, K, L, NDIM, NBABSC, JABSC, JORIG, JEXTR, NCMP,
     +             JCONT, IRET,KK
      PARAMETER  ( NCMP = 6 )
      REAL*8       PREC(2), MOMEN0, MOMEN1, VALE(2)
      REAL*8       SIGTH(1000*NCMP),TRESC(2),TRACE,R3(2)
      REAL*8       TREMIN(2),TREMAX(2),TMIN(2),TMAX(2)
      COMPLEX*16   CBID
      LOGICAL      EXIST
      CHARACTER*8  K8B,CRIT(2),NOCMP(NCMP),TABLE,KNUME,TBPRES
      CHARACTER*16 MOTCLF, VALEK(2), MOTCL2
      CHARACTER*24 INSTAN, ABSCUR, JVORIG, JVEXTR
      CHARACTER*24 VALK(7)
      INTEGER      IARG
C DEB ------------------------------------------------------------------
      CALL JEMARQ()
C
      MOTCLF = 'RESU_THER'
      CALL GETFAC ( MOTCLF, NBTHER )
      IF (NBTHER.EQ.0) GOTO 9999
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
      JVORIG = '&&RC3200.THER_UNIT .ORIG'
      JVEXTR = '&&RC3200.THER_UNIT .EXTR'
      CALL JECREC (JVORIG, 'V V R', 'NO','DISPERSE','VARIABLE',NBTHER)
      CALL JECREC (JVEXTR, 'V V R', 'NO','DISPERSE','VARIABLE',NBTHER)
C
      MOTCL2 = 'RESU_MECA_UNIT'
      CALL GETVID ( MOTCL2, 'TABL_PRES', 1,IARG,1, TBPRES, N1 )
      CALL ASSERT(N1.NE.0)

      DO 10, IOCC = 1, NBTHER, 1
C
         CALL GETVIS ( MOTCLF, 'NUME_RESU_THER', IOCC,IARG,1, NUME, N1 )
         KNUME = 'T       '
         CALL CODENT ( NUME , 'D0' , KNUME(2:8)  )
C
         CALL GETVID (MOTCLF,'TABL_RESU_THER',IOCC,IARG,1,
     &                TABLE, N1 )

C ------ ON VERIFIE L'ORDRE DES NOEUDS
         CALL RCVERI(TABLE)

C ------ ON VERIFIE LA COHERENCE AVEC LA TABLE TBPRES
         CALL RCVER1('THERMIQUE',TBPRES,TABLE)
C
C ------ ON RECUPERE LES INSTANTS DANS LA TABLE
C
         CALL TBEXIP ( TABLE, VALEK(1), EXIST, K8B )
         IF ( .NOT. EXIST ) THEN
            VALK (1) = TABLE
            VALK (2) = VALEK(1)
            CALL U2MESG('F', 'POSTRCCM_1',2,VALK,0,0,0,0.D0)
         ENDIF
         INSTAN = '&&RC32TH.INSTANT'
         CALL TBEXV1 ( TABLE, VALEK(1), INSTAN, 'V', NBINST, K8B)
         CALL JEVEUO ( INSTAN, 'L', JINST )
C
C ------ ON RECUPERE L'ABSC_CURV DANS LA TABLE
C
         CALL TBEXIP ( TABLE, VALEK(2), EXIST, K8B )
         IF ( .NOT. EXIST ) THEN
            VALK (1) = TABLE
            VALK (2) = VALEK(2)
            CALL U2MESG('F', 'POSTRCCM_1',2,VALK,0,0,0,0.D0)
         ENDIF
         ABSCUR = '&&RC32TH.ABSC_CURV'
         CALL TBEXV1 ( TABLE, VALEK(2), ABSCUR, 'V', NBABSC, K8B)
         CALL JEVEUO ( ABSCUR, 'L', JABSC )
C
         CALL WKVECT ( '&&RC32TH.CONTRAINTES', 'V V R', NBABSC, JCONT )
C
         NDIM = 4 * NCMP * 2
         CALL JECROC (JEXNOM(JVORIG,KNUME))
         CALL JEECRA (JEXNOM(JVORIG,KNUME),'LONMAX',NDIM,' ')
         CALL JEECRA (JEXNOM(JVORIG,KNUME),'LONUTI',NDIM,' ')
         CALL JEVEUO (JEXNOM(JVORIG,KNUME), 'E', JORIG )
C
         CALL JECROC (JEXNOM(JVEXTR,KNUME))
         CALL JEECRA (JEXNOM(JVEXTR,KNUME),'LONMAX',NDIM,' ')
         CALL JEECRA (JEXNOM(JVEXTR,KNUME),'LONUTI',NDIM,' ')
         CALL JEVEUO (JEXNOM(JVEXTR,KNUME), 'E', JEXTR )
C
         DO 116 K = 1 , NBABSC*NCMP
           SIGTH(K) = 0.D0
 116     CONTINUE
         DO 1116 K = 1 , 2
           TRESC(K)  = 0.D0
           R3(K)     = 0.D0
           TMIN(K)   = 0.D0
           TREMIN(K) = 0.D0
           TMAX(K)   = 0.D0
           TREMAX(K) = 0.D0
 1116    CONTINUE
C
         DO 12 I = 1 , NBINST
           VALE(1) = ZR(JINST+I-1)
C
           DO 14 J = 1 , NCMP
C
             KK = 0
             DO 16 K = 1 , NBABSC, NBABSC-1
               KK = KK + 1
               VALE(2) = ZR(JABSC+K-1)
C
               CALL TBLIVA ( TABLE, 2, VALEK, IBID, VALE,
     +                       CBID, K8B, CRIT, PREC, NOCMP(J),
     +                       K8B, IBID, ZR(JCONT+K-1), CBID, K8B, IRET)
               IF (IRET.NE.0) THEN
                  VALK (1) = TABLE
                  VALK (2) = NOCMP(J)
                  VALK (3) = VALEK(1)
                  VALK (4) = VALEK(2)
                  CALL U2MESG('F', 'POSTRCCM_2',4,VALK,0,0,2,VALE)
               ENDIF
               SIGTH((K-1)*NCMP+J) = ZR(JCONT+K-1)
               IF (J.EQ.NCMP) THEN
                 CALL RCTRES(SIGTH((K-1)*NCMP+1),TRESC(KK))

                 IF (TRACE(3,SIGTH((K-1)*NCMP+1)).LT.0.D0) THEN
                   R3(KK) = TRESC(KK)*(-1.0D0)
                 ELSE
                   R3(KK) = TRESC(KK)
                 ENDIF
                 IF (I.EQ.1) THEN
                    TREMIN(KK) = R3(KK)
                    TREMAX(KK) = R3(KK)
                    TMIN(KK) = VALE(1)
                    TMAX(KK) = VALE(1)
                 ELSE
                   IF (R3(KK).LT.TREMIN(KK)) THEN
                    TREMIN(KK) = R3(KK)
                    TMIN(KK) = VALE(1)
                   ENDIF
                   IF (R3(KK).GT.TREMAX(KK)) THEN
                    TREMAX(KK) = R3(KK)
                    TMAX(KK) = VALE(1)
                   ENDIF
                 ENDIF
               ENDIF

 16          CONTINUE
C
 14        CONTINUE
C
 12      CONTINUE
C
C   POUR LA VALEUR MINIMALE
         DO 66 J = 1,NCMP
           VALE(1) = TMIN(1)
           DO 166 K=1,NBABSC
             VALE(2) = ZR(JABSC+K-1)

             CALL TBLIVA ( TABLE, 2, VALEK, IBID, VALE,
     +                       CBID, K8B, CRIT, PREC, NOCMP(J),
     +                       K8B, IBID, ZR(JCONT+K-1), CBID, K8B, IRET)
 166       CONTINUE
           ZR(JORIG-1+J) = ZR(JCONT)
           CALL RC32MY (NBABSC, ZR(JABSC), ZR(JCONT), MOMEN0, MOMEN1)
C
           L = NCMP*2 + J
           ZR(JORIG-1+L) = MOMEN0 - 0.5D0*MOMEN1
C
           L = NCMP*4 + J
           ZR(JORIG-1+L) = MOMEN0

           VALE(1) = TMIN(2)
           DO 266 K=1,NBABSC
             VALE(2) = ZR(JABSC+K-1)

             CALL TBLIVA ( TABLE, 2, VALEK, IBID, VALE,
     +                       CBID, K8B, CRIT, PREC, NOCMP(J),
     +                       K8B, IBID, ZR(JCONT+K-1), CBID, K8B, IRET)
 266       CONTINUE
           ZR(JEXTR-1+J) = ZR(JCONT+NBABSC-1)
C
           CALL RC32MY (NBABSC, ZR(JABSC), ZR(JCONT), MOMEN0, MOMEN1)
C
           L = NCMP*2 + J
           ZR(JEXTR-1+L) = MOMEN0 + 0.5D0*MOMEN1
C
           L = NCMP*4 + J
           ZR(JEXTR-1+L) = MOMEN0
C
           L = NCMP*6 + J
           ZR(JEXTR-1+L) = 0.5D0*MOMEN1

C   POUR LA VALEUR MAXIMALE
           VALE(1) = TMAX(1)
           DO 366 K=1,NBABSC
             VALE(2) = ZR(JABSC+K-1)

             CALL TBLIVA ( TABLE, 2, VALEK, IBID, VALE,
     +                       CBID, K8B, CRIT, PREC, NOCMP(J),
     +                       K8B, IBID, ZR(JCONT+K-1), CBID, K8B, IRET)
 366       CONTINUE
           L = NCMP + J
           ZR(JORIG-1+L) = ZR(JCONT)
C
           CALL RC32MY (NBABSC, ZR(JABSC), ZR(JCONT), MOMEN0, MOMEN1)
C
           L = NCMP*3 + J
           ZR(JORIG-1+L) = MOMEN0 - 0.5D0*MOMEN1
C
           L = NCMP*5 + J
           ZR(JORIG-1+L) = MOMEN0
C
           L = NCMP*7 + J
           ZR(JORIG-1+L) = 0.5D0*MOMEN1

           VALE(1) = TMAX(2)
           DO 466 K=1,NBABSC
             VALE(2) = ZR(JABSC+K-1)

             CALL TBLIVA ( TABLE, 2, VALEK, IBID, VALE,
     +                       CBID, K8B, CRIT, PREC, NOCMP(J),
     +                       K8B, IBID, ZR(JCONT+K-1), CBID, K8B, IRET)
 466       CONTINUE
           L = NCMP + J
           ZR(JEXTR-1+L) = ZR(JCONT+NBABSC-1)
C
           CALL RC32MY (NBABSC, ZR(JABSC), ZR(JCONT), MOMEN0, MOMEN1)
C
             L = NCMP*3 + J
             ZR(JEXTR-1+L) = MOMEN0 + 0.5D0*MOMEN1
C
             L = NCMP*5 + J
             ZR(JEXTR-1+L) = MOMEN0
C
             L = NCMP*7 + J
             ZR(JEXTR-1+L) = 0.5D0*MOMEN1
 66      CONTINUE
         CALL JEDETR ( INSTAN )
         CALL JEDETR ( ABSCUR )
         CALL JEDETR ( '&&RC32TH.CONTRAINTES' )
C
 10   CONTINUE
C
9999  CONTINUE
      CALL JEDEMA( )
      END
