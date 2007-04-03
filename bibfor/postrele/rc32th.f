      SUBROUTINE RC32TH
      IMPLICIT   NONE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 03/04/2007   AUTEUR VIVAN L.VIVAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
      CHARACTER*32     JEXNOM, JEXNUM
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER      IBID, N1, IOCC, NBTHER, NUME, NBINST, JINST,
     +             I, J, K, L, NDIM, NBABSC, JABSC, JORIG, JEXTR, NCMP,
     +             JCONT, IRET
      PARAMETER  ( NCMP = 6 )
      REAL*8       PREC(2), MOMEN0, MOMEN1, VALE(2), SIGMLO, SIGMLE
      REAL*8 VALR
      COMPLEX*16   CBID
      LOGICAL      EXIST
      CHARACTER*8  K8B, CRIT(2), NOCMP(NCMP), TABLE, TABFLE, KNUME
      CHARACTER*16 MOTCLF, VALEK(2)
      CHARACTER*24 INSTAN, ABSCUR, JVORIG, JVEXTR
      CHARACTER*24 VALK(7)
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
      DO 10, IOCC = 1, NBTHER, 1
C
         CALL GETVIS ( MOTCLF, 'NUME_RESU_THER', IOCC,1,1, NUME, N1 )
         KNUME = 'T       '
         CALL CODENT ( NUME , 'D0' , KNUME(2:8)  )
C
         CALL GETVID ( MOTCLF, 'TABL_RESU_THER', IOCC,1,1, TABLE, N1 )
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
         NDIM = 4 * NCMP * NBINST
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
         DO 12 I = 1 , NBINST
           VALE(1) = ZR(JINST+I-1)
C
           DO 14 J = 1 , NCMP
C
             DO 16 K = 1 , NBABSC 
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
C
 16          CONTINUE
C
             L = NCMP*(I-1) + J
             ZR(JORIG-1+L) = ZR(JCONT)
             ZR(JEXTR-1+L) = ZR(JCONT+NBABSC-1)
C
             CALL RC32MY (NBABSC, ZR(JABSC), ZR(JCONT), MOMEN0, MOMEN1)
C
             L = NCMP*NBINST + NCMP*(I-1) + J
             ZR(JORIG-1+L) = MOMEN0 - 0.5D0*MOMEN1
             ZR(JEXTR-1+L) = MOMEN0 + 0.5D0*MOMEN1
C
             L = 2*NCMP*NBINST + NCMP*(I-1) + J
             ZR(JORIG-1+L) = MOMEN0
             ZR(JEXTR-1+L) = MOMEN0
C
             L = 3*NCMP*NBINST + NCMP*(I-1) + J
             ZR(JORIG-1+L) = 0.5D0*MOMEN1
             ZR(JEXTR-1+L) = 0.5D0*MOMEN1
C
 14        CONTINUE
C
 12      CONTINUE
C
         CALL JEDETR ( INSTAN )
         CALL JEDETR ( ABSCUR )
         CALL JEDETR ( '&&RC32TH.CONTRAINTES' )
C
 10   CONTINUE
C
9999  CONTINUE
      CALL JEDEMA( )
      END
