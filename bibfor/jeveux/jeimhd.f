       SUBROUTINE JEIMHD ( FICHDF, CLAS )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C TOLE CRP_18 CRS_508 CRS_512
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)       FICHDF, CLAS
C ----------------------------------------------------------------------
C IMPRESSION DE L'ENSEMBLE DES OBJETS JEVEUX AU FORMAT HDF
C
C IN  FICHDF : NOM LOCAL DU FICHIER HDF UTILISE POUR L'IMPRESSION
C IN  CLAS   : NOM DE LA CLASSE ASSOCIEE CR
C
C ----------------------------------------------------------------------
      CHARACTER*1      K1ZON
      COMMON /KZONJE/  K1ZON(8)
      INTEGER          LK1ZON , JK1ZON , LISZON , JISZON , ISZON(1)
      COMMON /IZONJE/  LK1ZON , JK1ZON , LISZON , JISZON
      REAL*8           R8ZON(1)
      LOGICAL          LSZON(1)
      EQUIVALENCE    ( ISZON(1) , K1ZON(1) , R8ZON(1) , LSZON(1) )
C ----------------------------------------------------------------------
      PARAMETER  ( N = 5 )
      INTEGER          LTYP    , LONG    , DATE    , IADD    , IADM    ,
     &                 LONO    , HCOD    , CARA    , LUTI    , IMARQ
      COMMON /IATRJE/  LTYP(1) , LONG(1) , DATE(1) , IADD(1) , IADM(1) ,
     &                 LONO(1) , HCOD(1) , CARA(1) , LUTI(1) , IMARQ(1)
      COMMON /JIATJE/  JLTYP(N), JLONG(N), JDATE(N), JIADD(N), JIADM(N),
     &                 JLONO(N), JHCOD(N), JCARA(N), JLUTI(N), JMARQ(N)
C
      CHARACTER*1      GENR    , TYPE
      CHARACTER*4      DOCU
      CHARACTER*8      ORIG
      CHARACTER*32     RNOM
      COMMON /KATRJE/  GENR(8) , TYPE(8) , DOCU(2) , ORIG(1) , RNOM(1)
      COMMON /JKATJE/  JGENR(N), JTYPE(N), JDOCU(N), JORIG(N), JRNOM(N)
C
      INTEGER          NRHCOD    , NREMAX    , NREUTI
      COMMON /ICODJE/  NRHCOD(N) , NREMAX(N) , NREUTI(N)
C
      CHARACTER*2      DN2
      CHARACTER*5      CLASSE
      CHARACTER*8                  NOMFIC    , KSTOUT    , KSTINI
      COMMON /KFICJE/  CLASSE    , NOMFIC(N) , KSTOUT(N) , KSTINI(N) ,
     &                 DN2(N)
      INTEGER          ICLAS ,ICLAOS , ICLACO , IDATOS , IDATCO , IDATOC
      COMMON /IATCJE/  ICLAS ,ICLAOS , ICLACO , IDATOS , IDATCO , IDATOC
      INTEGER          NUMEC
      COMMON /INUMJE/  NUMEC
      CHARACTER *24                     NOMCO
      CHARACTER *32    NOMUTI , NOMOS ,         NOMOC , BL32
      COMMON /NOMCJE/  NOMUTI , NOMOS , NOMCO , NOMOC , BL32
      INTEGER          IFNIVO, NIVO
      COMMON /JVNIVO/  IFNIVO, NIVO
      INTEGER          IPGC, KDESMA, LGD, LGDUTI, KPOSMA, LGP, LGPUTI
      COMMON /IADMJE/  IPGC, KDESMA, LGD, LGDUTI, KPOSMA, LGP, LGPUTI
C ----------------------------------------------------------------------
      INTEGER          LBIS , LOIS , LOLS , LOUA , LOR8 , LOC8
      COMMON /IENVJE/  LBIS , LOIS , LOLS , LOUA , LOR8 , LOC8
C ----------------------------------------------------------------------
      CHARACTER*1      KCLAS , GENRI , TYPEI , CLASI
      CHARACTER*16     K16,MACH,OS,PROC
      CHARACTER*24     NOMO,KATTR(5),NOMATR,KATTRC(5),NOMATC
      PARAMETER      ( NOMATR = 'BASE GLOBALE JEVEUX' )
      PARAMETER      ( NOMATC = 'ATTRIBUTS JEVEUX' )
      CHARACTER*32     CRNOM,NGRP,NOMCOL
      CHARACTER*75     CMESS
      CHARACTER*80     NHDF
      LOGICAL          LEXP
      INTEGER          IC,JULIST,LTYPI,ILONG,LONOI,IADDI(2),IADMI,IADMX
      INTEGER          IDFIC,NBMAX,JCTAB,IDG,IDGC
      INTEGER          HDFCRF,HDFCLF,HDFCRG,HDFOPG,HDFWAT,HDFCLG
      INTEGER          LIDBAS      , LIDEFF
      PARAMETER      ( LIDBAS = 20 , LIDEFF = 15 )
C ----------------------------------------------------------------------
      INTEGER          ILOREP , IDENO , ILNOM , ILMAX , ILUTI , IDEHC
      PARAMETER      ( ILOREP=1,IDENO=2,ILNOM=3,ILMAX=4,ILUTI=5,IDEHC=6)
C     ------------------------------------------------------------------
      INTEGER        IVNMAX     , IDDESO     ,IDIADD     , IDIADM     ,
     &               IDMARQ     , IDNOM      ,IDREEL     , IDLONG     ,
     &               IDLONO     , IDLUTI     ,IDNUM
      PARAMETER    ( IVNMAX = 0 , IDDESO = 1 ,IDIADD = 2 , IDIADM = 3 ,
     &               IDMARQ = 4 , IDNOM  = 5 ,IDREEL = 6 , IDLONG = 7 ,
     &               IDLONO = 8 , IDLUTI = 9 ,IDNUM  = 10 )
C DEB ------------------------------------------------------------------
      IPGCEX = IPGC
      IPGC   = -2
      KCLAS = CLAS
      NHDF  = FICHDF
      IDFIC = HDFCRF (NHDF)
      IF ( IDFIC .LT. 0 ) THEN
        CMESS = 'IMPOSSIBLE DE CREER LE FICHIER HDF '// NHDF
        CALL U2MESK('F','JEVEUX_01',1,CMESS)
      ENDIF
      NGRP ='/'
      IDG  = HDFOPG (IDFIC,NGRP)
      CALL VERSIO (IVERS, IUTIL, INIVO, K16, LEXP )
      WRITE(KATTR(1),'(A16,I2,''.'',I2,''.'',I2)')
     &               K16,IVERS,IUTIL,INIVO
      CALL NODNAM(1,KATTR(2)(1:16),KATTR(3)(1:16),K16)
      KATTR(2)(17:24) =' '
      KATTR(3)(17:24) =' '
      CALL ENLIRD(KATTR(4))
      WRITE(KATTR(5),'(''LBIS='',I2,'' LOIS='',I2,'' LOUA='',I2)')
     &                   LBIS, LOIS, LOUA
      IRET = HDFWAT (IDG,NOMATR,5,KATTR)
      IRET = HDFCLG (IDG)

      IC = INDEX ( CLASSE , KCLAS)
      CLASI = CLASSE(IC:IC)
      JULIST = IUNIFI ('MESSAGE')
      IF ( CLASI .NE. ' ' ) THEN
        IF (NIVO .GE. 2) THEN
          WRITE(JULIST ,*) '<INFO> IMPRESSION DE LA BASE '//KCLAS//
     &                     ' AU FORMAT HDF DANS LE FICHIER '//NHDF
        ENDIF
C       ----------- ACTUALISER CARA
C
        CARA(JCARA(IC)+1) = NREUTI(IC)
        DO 5 J = 1 , NREUTI(IC)
          CRNOM = RNOM(JRNOM(IC)+J)
          DO 1 K=1,5
             KATTR(K)=' '
 1        CONTINUE
          KATTR(3)(1:1)=KCLAS
          IF ( J .EQ. 17 ) GOTO 5
          IF ( J .GT. LIDBAS .AND.
     &        (CRNOM(25:26) .EQ. '$$' .OR. CRNOM(1:1) .EQ. '?') ) GOTO 5
          GENRI = GENR(JGENR(IC)+J)
          TYPEI = TYPE(JTYPE(IC)+J)
          LTYPI = LTYP(JLTYP(IC)+J)
          ILONG = LONG(JLONG(IC)+J)
          LONOI = LONO(JLONO(IC)+J)*LTYPI
          IADDI(1) = IADD(JIADD(IC)+2*J-1)
          IADDI(2) = IADD(JIADD(IC)+2*J  )
          IADMI = IADM(JIADM(IC)+J)
          IADMX = IADMI
          IF ( GENRI .NE. 'X' ) THEN
C           ON TRAITE UN OBJET SIMPLE
            IDATOS = J
            ICLAOS = IC
            NOMOS  = CRNOM
            INAT  = 1
            INAT0 = 1
            IF ( J .LE. LIDBAS ) INAT0 = 0
            IF ( IADMX .EQ. 0 ) THEN
              IF ( IADDI(1) .EQ. 0 .OR. LONOI .EQ. 0 ) THEN
                IF (NIVO .GE. 2) THEN
                  CMESS = 'OBJET INEXISTANT EN MEMOIRE ET SUR DISQUE'
                  CALL U2MESK('A','JEVEUX_01',1,CMESS)
                ENDIF
                GOTO 5
              ENDIF
              CALL JJALTY (TYPEI , LTYPI , 'L' , 1 , JCTAB)
              IADMI = IADM ( JIADM(IC) + J )
            ENDIF
            WRITE(KATTR(2),'(16X,I8)') J
            CALL JJIMHD (IDFIC,INAT0,CRNOM,NGRP,KATTR,IADMI,GENRI,TYPEI,
     &                   LTYPI,LONOI)
            IF (NIVO .GE. 2) THEN
              IF ( MOD(J,25) .EQ. 1 ) THEN
                WRITE ( JULIST , '(/,A,A/)' )
     &       ' NUM  ------------- NOM ---------------- G T L- --LONG---'
     &       ,' -LOTY- -IADD- ------ --KADM--'
              ENDIF
              WRITE(JULIST , 1001) J,CRNOM,GENRI,TYPEI,LTYPI,
     &                           ILONG,LONOI,IADDI(1),IADDI(2),IADMI
            ENDIF
            IF ( IADMX .EQ. 0 ) THEN
              CALL JJLIDE ( 'JEIMPO' , CRNOM , INAT )
            ENDIF
          ELSE
C         ON TRAITE UNE COLLECTION
            IDATCO = J
            ICLACO = IC
            NOMCO  = CRNOM
            NOMCOL = CRNOM
            INAT   = 2
            CALL JJALLC ( IC , J , 'L' , IBACOL )
            TYPEI  = TYPE( JTYPE(IC) + J )
            LTYPI  = LTYP( JLTYP(IC) + J )
            LONOI = LONO( JLONO(IC) + J ) * LTYPI
            WRITE(KATTR(2),'(16X,I8)') J
            CALL JJIMHD (IDFIC,-1,CRNOM,NGRP,KATTR,IBACOL,GENRI,
     &                   TYPEI,LTYPI,LONOI)
            IXIADD = ISZON ( JISZON + IBACOL + IDIADD )
            IXDESO = ISZON ( JISZON + IBACOL + IDDESO )
            GENRI  = GENR( JGENR(IC) + IXDESO )
            LTYPI  = LTYP( JLTYP(IC) + IXDESO )
            IF ( IXIADD .EQ. 0 ) THEN
C             ON TRAITE UNE COLLECTION CONTIGUE :
C                TRAITEMENT PARTICULIER DU $$DESO
              TYPEI = TYPE( JTYPE(IC) + IXDESO )
              LTYPI = LTYP( JLTYP(IC) + IXDESO )
              LONOI = LONO( JLONO(IC) + IXDESO ) * LTYPI
              IADMI    = IADM ( JIADM(IC) + IXDESO )
              IADDI(1) = IADD ( JIADD(IC) + 2*IXDESO-1 )
              IADDI(2) = IADD ( JIADD(IC) + 2*IXDESO   )
              IADMX = IADMI
              IF ( IADMX .EQ. 0 ) THEN
                IF ( IADDI(1) .EQ. 0 ) THEN
                  IF (NIVO .GE. 2) THEN
                    CMESS = 'COLLECTION INEXISTANTE EN MEMOIRE ET'
     &                    //'SUR DISQUE'
                    CALL U2MESK('A','JEVEUX_01',1,CMESS)
                  ENDIF
                  GOTO 5
                ENDIF
                CALL JJALTY (TYPEI , LTYPI , 'L' , 2 , JCTAB)
                IADMI = IADM ( JIADM(IC) + IXDESO )
              ENDIF
              CRNOM = RNOM( JRNOM(IC) + IXDESO )
              WRITE(KATTR(2),'(16X,I8)') IXDESO
              CALL JJIMHD (IDFIC,INAT,CRNOM,NGRP,KATTR,IADMI,GENRI,
     &                     TYPEI,LTYPI,LONOI)
            ELSE
C             ON TRAITE UNE COLLECTION DISPERSEE
C                TRAITEMENT PARTICULIER DES OBJETS DE COLLECTION
              INAT   = 3
              IXIADM = ISZON ( JISZON + IBACOL + IDIADM )
              IBIADM = IADM ( JIADM(IC) + IXIADM )
              IBIADD = IADM ( JIADM(IC) + IXIADD )
              NBMAX= ISZON (JISZON + IBACOL + IVNMAX )
              IF (NBMAX .GT. 0) THEN
                NOMCOL(25:32) = '__OBJETS'
                IRET = HDFCRG(IDFIC,NGRP,NOMCOL)
                IDGC = HDFOPG(IDFIC,NOMCOL)
                KATTRC(1)='COLLECTION'
                KATTRC(2)=' '
                KATTRC(3)=' '
                KATTRC(4)=' '
                KATTRC(5)=' '
                IRET = HDFWAT(IDGC,NOMATC,5,KATTRC)
              ENDIF
              DO 10 K = 1,NBMAX
                IADMI = ISZON(JISZON + IBIADM - 1 + K )
                IDATOC = K
                IF ( IADMI .EQ. 0 ) THEN
                  IADDI(1) = ISZON(JISZON + IBIADD - 1 + 2*K-1 )
                  IADDI(2) = ISZON(JISZON + IBIADD - 1 + 2*K   )
                  IF ( IADDI(1) .EQ. 0 ) THEN
                    IF (NIVO .GE. 2) THEN
                      CMESS= 'OBJET INEXISTANT EN MEMOIRE ET SUR DISQUE'
                      CALL U2MESK('A','JEVEUX_01',1,CMESS)
                    ENDIF
                    GOTO 10
                  ENDIF
                  CALL JJALTY (TYPEI , LTYPI , 'L' , INAT , JCTAB)
                  IADMI  = ISZON(JISZON + IBIADM - 1 + K )
                ENDIF
                IXLONO = ISZON ( JISZON + IBACOL + IDLONO )
                IF ( IXLONO .EQ. 0 ) THEN
                  LONOI = LONO ( JLONO(IC) + IXDESO ) * LTYPI
                ELSE
                  IBLONO = IADM ( JIADM(IC) + IXLONO )
                  LONOI  = ISZON ( JISZON + IBLONO - 1 + K ) * LTYPI
                ENDIF
                IXNOM = ISZON ( JISZON + IBACOL + IDNOM )
                IF ( IXNOM .GT. 0 ) THEN
                  IBNOM = IADM ( JIADM(IC) + IXNOM )
                  IDENOM = ISZON ( JISZON + IBNOM - 1 + IDENO )
                  LNOM   = ISZON ( JISZON + IBNOM - 1 + ILNOM )
                  IDECO = (IBNOM - 1) * LOIS + IDENOM + LNOM * (K - 1)
                  DO 12 L = 1,MIN (24,LNOM)
                    KATTR(2)(L:L) = K1ZON ( JK1ZON + IDECO + L )
 12               CONTINUE
                ELSE
                  WRITE(KATTR(2),'(16X,I8)') K
                ENDIF
                WRITE(CRNOM(25:32),'(I8)') K
                CALL JJIMHD (IDFIC,INAT,CRNOM,NOMCOL,KATTR,IADMI,GENRI,
     &                       TYPEI,LTYPI,LONOI)
                NUMEC = K
                CALL JJLIDE ('JEIMPO' , NOMCO(1:24)//'$$XNUM  ' , 2)
 10           CONTINUE
              IF (IDGC .GT. 0) IRET = HDFCLG(IDGC)
            ENDIF
C                TRAITEMENT DES OBJETS SYSTEME DE COLLECTION
            INAT = 2
            DO 20 K = IDIADD,IDNUM
              IX  = ISZON( JISZON + IBACOL + K )
              IF ( IX .GT. 0 ) THEN
                IADMI    = IADM (JIADM(IC) + IX)
                IF ( IADMI .NE. 0 ) THEN
                  GENRI = GENR(JGENR(IC)+IX)
                  TYPEI = TYPE(JTYPE(IC)+IX)
                  LTYPI = LTYP(JLTYP(IC)+IX)
                  ILONG = LONG(JLONG(IC)+IX)
                  LONOI = LONO(JLONO(IC)+IX)*LTYPI
                  CRNOM = RNOM(JRNOM(IC)+IX)
                  IADDI(1) = IADD (JIADD(IC)+2*IX-1 )
                  IADDI(2) = IADD (JIADD(IC)+2*IX   )
                  WRITE(KATTR(2),'(16X,I8)') IX
                  CALL JJIMHD (IDFIC,INAT,CRNOM,NGRP,KATTR,IADMI,GENRI,
     &                         TYPEI,LTYPI,LONOI)
                  IF (NIVO .GE. 2) THEN
                    WRITE(JULIST , 1001) J,CRNOM,GENRI,TYPEI,LTYPI,
     &                           ILONG,LONOI,IADDI(1),IADDI(2),IADMI
                  ENDIF
                ENDIF
              ENDIF
 20         CONTINUE
            CALL JJLIDE ( 'JEIMPO' , NOMCO , 2 )
          ENDIF
 5      CONTINUE
      ENDIF
 1001 FORMAT(I5,1X,A,'  -',2(A,'-'),I2,1X,I8,1X,I7,I7,I7,I9)
      IRET = HDFCLF (IDFIC)
      IF (IRET .NE. 0 ) THEN
        CMESS = 'IMPOSSIBLE DE FERMER LE FICHIER '//NHDF
        CALL U2MESK('S','JEVEUX_01',1,CMESS)
      ENDIF
      IPGC = IPGCEX
C FIN ------------------------------------------------------------------
      END
