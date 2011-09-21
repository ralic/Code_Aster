      SUBROUTINE GCHARG ( MODELE, NCHAR, LCHAR, CHVOLU, CF1D2D, CF2D3D,
     &                 CHPRES,CHEPSI,CHPESA,CHROTA,FONC,EPSI,TIME,IORD)
      IMPLICIT NONE
      INTEGER         NCHAR, IORD
      CHARACTER*8     MODELE, LCHAR(*)
      CHARACTER*19    CHVOLU,CF1D2D,CF2D3D,CHPRES,CHEPSI,CHPESA,CHROTA
      LOGICAL         FONC, EPSI
      REAL*8  TIME
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 21/09/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C ----------------------------------------------------------------------
C
      INTEGER       IBID, I, IF3D3D, IF2D2D, IF1D2D, IF2D3D,
     &              IPRESS, IEPSIN, IROTA, IPESA, IRET, N1,
     &               NEXCI, JPARA, JFCHA, NRES,
     &              NVOLU, N1D2D, N2D3D, NPRESS, NEPSI, NROTA, NPESA,
     &              IIVOLU,II1D2D,II2D3D,IIPRES,
     &              JFONCI,ISVOLU,IS1D2D,IS2D3D,ISPRES
      CHARACTER*8   K8B, RESU, NOMF, NOMCHF
      CHARACTER*16  TYPE, OPER
      CHARACTER*19  CHARG,CHTMP1, CHTMP2
      CHARACTER*24   NOMFCT
      CHARACTER*24  EXCISD
      LOGICAL LCHSD, FONCI, FONC1, FONC2
      INTEGER      IARG
C     ------------------------------------------------------------------
C
      CALL JEMARQ()

      CALL GETRES ( K8B, TYPE, OPER )
      CHTMP1='&&CHARGE_INTERM1'
      CHTMP2='&&CHARGE_INTERM2'
      NOMCHF='&FM00000'

      FONC   = .FALSE.
      EPSI   = .FALSE.
      NVOLU = 0
      N1D2D = 0
      N2D3D = 0
      NPRESS = 0
      NEPSI = 0
      NROTA = 0
      NPESA = 0
C
      IIVOLU=0
      II1D2D=0
      II2D3D=0
      IIPRES=0
C
      ISVOLU=0
      IS1D2D=0
      IS2D3D=0
      ISPRES=0
C
      CALL GETFAC('EXCIT',NEXCI)
      CALL GETVID(' ','RESULTAT',0,IARG,1,RESU,NRES)
      LCHSD=.FALSE.
      IF(NRES.NE.0.AND.NEXCI.EQ.0) LCHSD=.TRUE.
      IF(NCHAR.GT.0)CALL WKVECT('&&GCHARG.FONCI','V V L', NCHAR,JFONCI)
C
C--- LECTURE DES INFORMATIONS CONTENUES DANS LA SD RESULTAT
C
      IF(LCHSD) THEN
        CALL RSADPA(RESU,'L',1,'EXCIT',IORD,0,JPARA,K8B)
        EXCISD = ZK24(JPARA)
        CALL JEEXIN(EXCISD(1:19)//'.FCHA',IRET)
        IF (IRET .EQ. 0) GO TO 99
        CALL JEVEUO(EXCISD(1:19)//'.FCHA','L',JFCHA)
      ENDIF
C

      DO 10 I = 1 , NCHAR
C
        NOMCHF(7:8)='00'

        IF(LCHAR(I).NE.' ') THEN

         CALL DISMOI('F','TYPE_CHARGE',LCHAR(I),'CHARGE',IBID,K8B,IRET)
         IF ( K8B(5:7) .EQ. '_FO' ) THEN
            FONC = .TRUE.
            FONCI = .TRUE.
         ELSE
           FONCI = .FALSE.
         ENDIF
         ZL(JFONCI+I-1)=FONCI
C
C  ---   CHVOLU  ---
C
         CALL JEEXIN ( LCHAR(I)//'.CHME.F3D3D.DESC', IF3D3D )
         CALL JEEXIN ( LCHAR(I)//'.CHME.F2D2D.DESC', IF2D2D )
         IF ( IF3D3D .NE. 0 ) THEN
            CHARG= LCHAR(I)//'.CHME.F3D3D'
         ELSEIF( IF2D2D .NE. 0 ) THEN
            CHARG= LCHAR(I)//'.CHME.F2D2D'
         ENDIF
         IF(IF3D3D .NE. 0 .OR. IF2D2D .NE. 0 )THEN
           IF(LCHSD) THEN
             N1 = 1
             NOMFCT = ZK24(JFCHA-1+I)
             IF(NOMFCT(1:2).EQ.'&&') N1 = 0
           ELSE
             CALL GETVID('EXCIT','FONC_MULT',I,IARG,1,NOMFCT,N1)
           ENDIF

           IF (N1. NE. 0)THEN
             IF( FONCI) THEN
               CALL CODENT(I*10+1,'D0',NOMCHF(6:7))
               NOMF=NOMCHF
             ELSE
               NOMF=' '
             ENDIF
           ENDIF

           IF(FONCI)THEN
              CALL GVERFO(CHARG,IRET)
              IF(IRET.EQ.1)ISVOLU=ISVOLU+1
           ENDIF

           IF ( NVOLU .EQ. 0) THEN
             NVOLU = NVOLU + 1
             CALL COPISD('CHAMP_GD','V',CHARG,CHVOLU)
             IF (N1. NE. 0 ) THEN
               CALL GCHARM(FONCI,CHARG,NOMFCT,NOMF,TIME,IORD,CHVOLU)
             ENDIF
           ELSE
             IF(ISVOLU.GE.1)CALL U2MESS('F','CALCULEL5_57')
             CALL COPISD('CHAMP_GD','V',CHVOLU,CHTMP1)
             CALL COPISD('CHAMP_GD','V',CHARG,CHTMP2)
             CALL DETRSD('CHAMP_GD',CHVOLU)
             IF (N1. NE. 0 ) THEN
               CALL GCHARM(FONCI,CHARG,NOMFCT,NOMF,TIME,IORD,CHTMP2)
             ENDIF
             FONC1=ZL(JFONCI+IIVOLU-1)
             FONC2=ZL(JFONCI+I-1)

C            -- LA ROUTINE GCHS2F NE FONCTIONNE QUE SUR DES CARTES SUR
C               LESQUELLES ON A FAIT UN "CALL TECART" :
             CALL TECART(CHTMP1)
             CALL TECART(CHTMP2)
             CALL GCHARF(I,FONC1,CHTMP1,FONC2,CHTMP2,CHVOLU)
             CALL DETRSD('CHAMP_GD',CHTMP1)
             CALL DETRSD('CHAMP_GD',CHTMP2)
           ENDIF
           IIVOLU=I

         ENDIF
C
C  ---   CF1D2D  ---
C
         CALL JEEXIN ( LCHAR(I)//'.CHME.F1D2D.DESC', IF1D2D )
         IF ( IF1D2D .NE. 0 ) THEN

           IF(LCHSD) THEN
             N1 = 1
             NOMFCT = ZK24(JFCHA-1+I)
             IF(NOMFCT(1:2).EQ.'&&') N1 = 0
           ELSE
             CALL GETVID('EXCIT','FONC_MULT',I,IARG,1,NOMFCT,N1)
           ENDIF

           CHARG= LCHAR(I)//'.CHME.F1D2D'
           IF (N1. NE. 0)THEN
             IF( FONCI) THEN
               CALL CODENT(I*10+2,'D0',NOMCHF(6:7))
               NOMF=NOMCHF
             ELSE
               NOMF=' '
             ENDIF
           ENDIF

           IF(FONCI)THEN
             CALL GVERFO(CHARG,IRET)
             IF(IRET.EQ.1)IS1D2D=IS1D2D+1
           ENDIF

           IF ( N1D2D .EQ. 0 ) THEN
             N1D2D = N1D2D + 1
             CALL COPISD('CHAMP_GD','V',CHARG,CF1D2D)
             IF (N1. NE. 0 ) THEN
               CALL GCHARM(FONCI,CHARG,NOMFCT,NOMF,TIME,IORD,CF1D2D)
             ENDIF
           ELSE
             IF(IS1D2D.GE.1)CALL U2MESS('F','CALCULEL5_57')
             CALL COPISD('CHAMP_GD','V',CF1D2D,CHTMP1)
             CALL COPISD('CHAMP_GD','V',CHARG,CHTMP2)
             CALL DETRSD('CHAMP_GD',CF1D2D)
             IF (N1. NE. 0 ) THEN
               CALL GCHARM(FONCI,CHARG,NOMFCT,NOMF,TIME,IORD,CHTMP2)
             ENDIF
             FONC1=ZL(JFONCI+II1D2D-1)
             FONC2=ZL(JFONCI+I-1)
C            -- LA ROUTINE GCHS2F NE FONCTIONNE QUE SUR DES CARTES SUR
C               LESQUELLES ON A FAIT UN "CALL TECART" :
             CALL TECART(CHTMP1)
             CALL TECART(CHTMP2)
             CALL GCHARF(I,FONC1,CHTMP1,FONC2,CHTMP2,CF1D2D)
             CALL DETRSD('CHAMP_GD',CHTMP1)
             CALL DETRSD('CHAMP_GD',CHTMP2)
           ENDIF
           II1D2D=I

         ENDIF
C
C  ---   CF2D3D  ---
C
         CALL JEEXIN ( LCHAR(I)//'.CHME.F2D3D.DESC', IF2D3D )
         IF ( IF2D3D .NE. 0 ) THEN

           IF(LCHSD) THEN
             N1 = 1
             NOMFCT = ZK24(JFCHA-1+I)
             IF(NOMFCT(1:2).EQ.'&&') N1 = 0
           ELSE
             CALL GETVID('EXCIT','FONC_MULT',I,IARG,1,NOMFCT,N1)
           ENDIF

           CHARG= LCHAR(I)//'.CHME.F2D3D'
           IF (N1. NE. 0)THEN
             IF( FONCI) THEN
               CALL CODENT(I*10+3,'D0',NOMCHF(6:7))
               NOMF=NOMCHF
             ELSE
               NOMF=' '
             ENDIF
           ENDIF

           IF(FONCI)THEN
             CALL GVERFO(CHARG,IRET)
             IF(IRET.EQ.1)IS2D3D=IS2D3D+1
           ENDIF

           IF ( N2D3D .EQ. 0 ) THEN
             N2D3D = N2D3D +1
             CALL COPISD('CHAMP_GD','V',CHARG,CF2D3D)
             IF (N1. NE. 0 ) THEN
               CALL GCHARM(FONCI,CHARG,NOMFCT,NOMF,TIME,IORD,CF2D3D)
             ENDIF
           ELSE
             IF(IS2D3D.GE.1)CALL U2MESS('F','CALCULEL5_57')
             CALL COPISD('CHAMP_GD','V',CF2D3D,CHTMP1)
             CALL COPISD('CHAMP_GD','V',CHARG,CHTMP2)
             CALL DETRSD('CHAMP_GD',CF2D3D)
             IF (N1. NE. 0 ) THEN
               CALL GCHARM(FONCI,CHARG,NOMFCT,NOMF,TIME,IORD,CHTMP2)
             ENDIF
             FONC1=ZL(JFONCI+II2D3D-1)
             FONC2=ZL(JFONCI+I-1)
C            -- LA ROUTINE GCHS2F NE FONCTIONNE QUE SUR DES CARTES SUR
C               LESQUELLES ON A FAIT UN "CALL TECART" :
             CALL TECART(CHTMP1)
             CALL TECART(CHTMP2)
             CALL GCHARF(I,FONC1,CHTMP1,FONC2,CHTMP2,CF2D3D)
             CALL DETRSD('CHAMP_GD',CHTMP1)
             CALL DETRSD('CHAMP_GD',CHTMP2)
           ENDIF
           II2D3D=I

         ENDIF
C
C  ---   CHPRES  ---
C
         CALL JEEXIN ( LCHAR(I)//'.CHME.PRESS.DESC', IPRESS )
         IF ( IPRESS .NE. 0 ) THEN

           IF(LCHSD) THEN
             N1 = 1
             NOMFCT = ZK24(JFCHA-1+I)
             IF(NOMFCT(1:2).EQ.'&&') N1 = 0
           ELSE
             CALL GETVID('EXCIT','FONC_MULT',I,IARG,1,NOMFCT,N1)
           ENDIF

           CHARG= LCHAR(I)//'.CHME.PRESS'
           IF (N1. NE. 0)THEN
             IF( FONCI) THEN
               CALL CODENT(I*10+4,'D0',NOMCHF(6:7))
               NOMF=NOMCHF
             ELSE
               NOMF=' '
             ENDIF
           ENDIF

           IF(FONCI)THEN
             CALL GVERFO(CHARG,IRET)
             IF(IRET.EQ.1)ISPRES=ISPRES+1
           ENDIF

           IF ( NPRESS .EQ. 0 ) THEN
             NPRESS = NPRESS + 1
             CALL COPISD('CHAMP_GD','V',CHARG,CHPRES)
             IF (N1. NE. 0 ) THEN
               CALL GCHARM(FONCI,CHARG,NOMFCT,NOMF,TIME,IORD,CHPRES)
             ENDIF
           ELSE
             IF(ISPRES.GE.1) CALL U2MESS('F','CALCULEL5_57')
             CALL COPISD('CHAMP_GD','V',CHPRES,CHTMP1)
             CALL COPISD('CHAMP_GD','V',CHARG,CHTMP2)
             CALL DETRSD('CHAMP_GD',CHPRES)
             IF (N1. NE. 0 ) THEN
               CALL GCHARM(FONCI,CHARG,NOMFCT,NOMF,TIME,IORD,CHTMP2)
             ENDIF
             FONC1=ZL(JFONCI+IIPRES-1)
             FONC2=ZL(JFONCI+I-1)
C            -- LA ROUTINE GCHS2F NE FONCTIONNE QUE SUR DES CARTES SUR
C               LESQUELLES ON A FAIT UN "CALL TECART" :
             CALL TECART(CHTMP1)
             CALL TECART(CHTMP2)
             CALL GCHARF(I,FONC1,CHTMP1,FONC2,CHTMP2,CHPRES)
             CALL DETRSD('CHAMP_GD',CHTMP1)
             CALL DETRSD('CHAMP_GD',CHTMP2)
           ENDIF
           IIPRES=I


         ENDIF
C
C  ---   CHEPSI  ---
C
         CALL JEEXIN ( LCHAR(I)//'.CHME.EPSIN.DESC', IEPSIN )
         IF ( IEPSIN .NE. 0 ) THEN

           IF(LCHSD) THEN
             N1 = 1
             NOMFCT = ZK24(JFCHA-1+I)
             IF(NOMFCT(1:2).EQ.'&&') N1 = 0
           ELSE
             CALL GETVID('EXCIT','FONC_MULT',I,IARG,1,NOMFCT,N1)
           ENDIF

           CHARG= LCHAR(I)//'.CHME.EPSIN'

           IF (N1. NE. 0)THEN
             IF( FONCI) THEN
               CALL CODENT(I*10+5,'D0',NOMCHF(6:7))
               NOMF=NOMCHF
             ELSE
               NOMF=' '
             ENDIF
           ENDIF

           EPSI = .TRUE.

           IF ( NEPSI .EQ. 0 ) THEN
             NEPSI = NEPSI + 1
             CALL COPISD('CHAMP_GD','V',CHARG,CHEPSI)
             IF (N1. NE. 0 ) THEN
               CALL GCHARM(FONCI,CHARG,NOMFCT,NOMF,TIME,IORD,CHEPSI)
             ENDIF
           ELSE
             CALL U2MESS('F','CALCULEL5_58')
           ENDIF

         ENDIF
C
C  ---   CHPESA  ---
C
         CALL JEEXIN ( LCHAR(I)//'.CHME.PESAN.DESC', IPESA )
         IF ( IPESA .NE. 0 ) THEN

           IF(LCHSD) THEN
             N1 = 1
             NOMFCT = ZK24(JFCHA-1+I)
             IF(NOMFCT(1:2).EQ.'&&') N1 = 0
           ELSE
             CALL GETVID('EXCIT','FONC_MULT',I,IARG,1,NOMFCT,N1)
           ENDIF

           CHARG= LCHAR(I)//'.CHME.PESAN'

           IF (N1. NE. 0)THEN
             IF( FONCI) THEN
               CALL CODENT(I*10+6,'D0',NOMCHF(6:7))
               NOMF=NOMCHF
             ELSE
               NOMF=' '
             ENDIF
           ENDIF

           IF ( NPESA .EQ. 0 ) THEN
             NPESA = NPESA + 1
             CALL COPISD('CHAMP_GD','V',CHARG,CHPESA)
             IF (N1. NE. 0 ) THEN
               CALL GCHARM(FONCI,CHARG,NOMFCT,NOMF,TIME,IORD,CHPESA)
             ENDIF
           ELSE
             CALL U2MESS('F','CALCULEL5_59')
           ENDIF

         ENDIF

C
C  ---   CHROTA  ---
C
         CALL JEEXIN ( LCHAR(I)//'.CHME.ROTAT.DESC', IROTA )
         IF ( IROTA .NE. 0 ) THEN

           IF(LCHSD) THEN
             N1 = 1
             NOMFCT = ZK24(JFCHA-1+I)
             IF(NOMFCT(1:2).EQ.'&&') N1 = 0
           ELSE
             CALL GETVID('EXCIT','FONC_MULT',I,IARG,1,NOMFCT,N1)
           ENDIF

           CHARG= LCHAR(I)//'.CHME.ROTAT'

           IF (N1. NE. 0)THEN
             IF( FONCI) THEN
               CALL CODENT(I*10+7,'D0',NOMCHF(6:7))
               NOMF=NOMCHF
             ELSE
               NOMF=' '
             ENDIF
           ENDIF

           IF ( NROTA .EQ. 0 ) THEN
             NROTA = NROTA + 1
             CALL COPISD('CHAMP_GD','V',CHARG,CHROTA)
             IF (N1. NE. 0 ) THEN
               CALL GCHARM(FONCI,CHARG,NOMFCT,NOMF,TIME,IORD,CHROTA)
             ENDIF
           ELSE
             CALL U2MESS('F','CALCULEL5_60')
           ENDIF

         ENDIF
       ENDIF
C
 10   CONTINUE
C
C  -  SI ABSENCE D'UN CHAMP DE FORCES, CREATION D'UN CHAMP NUL
C
 99   CONTINUE

      IF(NCHAR.GT.0)CALL JEDETR('&&GCHARG.FONCI')

      IF ( NVOLU .EQ. 0 ) THEN
         CALL MEFOR0 ( MODELE, CHVOLU, FONC )
      ENDIF
      IF ( N1D2D .EQ. 0 ) THEN
         CALL MEFOR0 ( MODELE, CF1D2D, FONC )
      ENDIF
      IF ( N2D3D .EQ. 0 ) THEN
         CALL MEFOR0 ( MODELE, CF2D3D, FONC )
      ENDIF
      IF ( NPRESS .EQ. 0 ) THEN
         CALL MEPRES ( MODELE, CHPRES, FONC )
      ENDIF
C
      CALL JEDEMA()
      END
