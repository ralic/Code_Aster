       SUBROUTINE JEIMHD ( FICHDF, CLAS )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C RESPONSABLE LEFEBVRE
C MODIF JEVEUX  DATE 13/11/2012   AUTEUR COURTOIS M.COURTOIS 
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
      IMPLICIT NONE
      INCLUDE 'jeveux_private.h'
      CHARACTER*(*)       FICHDF, CLAS
C ----------------------------------------------------------------------
C IMPRESSION DE L'ENSEMBLE DES OBJETS JEVEUX AU FORMAT HDF
C
C IN  FICHDF : NOM LOCAL DU FICHIER HDF UTILISE POUR L'IMPRESSION
C IN  CLAS   : NOM DE LA CLASSE ASSOCIEE CR
C
C ----------------------------------------------------------------------
      INTEGER          LK1ZON , JK1ZON , LISZON , JISZON 
      COMMON /IZONJE/  LK1ZON , JK1ZON , LISZON , JISZON
C ----------------------------------------------------------------------
C-----------------------------------------------------------------------
      INTEGER IBACOL ,IBIADD ,IBIADM ,IBLONO ,IBNOM ,IDECO ,IDENOM 
      INTEGER INAT ,INAT0 ,IPGCEX ,IRET ,IUNIFI ,IX ,IXDESO 
      INTEGER IXIADD ,IXIADM ,IXLONO ,IXNOM ,J ,JCARA ,JDATE 
      INTEGER JDOCU ,JGENR ,JHCOD ,JIADD ,JIADM ,JLONG ,JLONO 
      INTEGER JLTYP ,JLUTI ,JMARQ ,JORIG ,JRNOM ,JTYPE ,K 
      INTEGER L ,LNOM ,LOUA ,N 
C-----------------------------------------------------------------------
      PARAMETER  ( N = 5 )
      COMMON /JIATJE/  JLTYP(N), JLONG(N), JDATE(N), JIADD(N), JIADM(N),
     &                 JLONO(N), JHCOD(N), JCARA(N), JLUTI(N), JMARQ(N)
C
      COMMON /JKATJE/  JGENR(N), JTYPE(N), JDOCU(N), JORIG(N), JRNOM(N)
C
      INTEGER          NRHCOD    , NREMAX    , NREUTI
      COMMON /ICODJE/  NRHCOD(N) , NREMAX(N) , NREUTI(N)
      INTEGER          NBLMAX    , NBLUTI    , LONGBL    ,
     &                 KITLEC    , KITECR    ,             KIADM    ,
     &                 IITLEC    , IITECR    , NITECR    , KMARQ
      COMMON /IFICJE/  NBLMAX(N) , NBLUTI(N) , LONGBL(N) ,
     &                 KITLEC(N) , KITECR(N) ,             KIADM(N) ,
     &                 IITLEC(N) , IITECR(N) , NITECR(N) , KMARQ(N)
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
      INTEGER          IPGC,KDESMA(2),LGD,LGDUTI,KPOSMA(2),LGP,LGPUTI
      COMMON /IADMJE/  IPGC,KDESMA,   LGD,LGDUTI,KPOSMA,   LGP,LGPUTI
C ----------------------------------------------------------------------
      INTEGER          LBIS , LOIS , LOLS , LOR8 , LOC8
      COMMON /IENVJE/  LBIS , LOIS , LOLS , LOR8 , LOC8
C ----------------------------------------------------------------------
      CHARACTER*1      KCLAS , GENRI , TYPEI , CLASI
      CHARACTER*8      K8
      CHARACTER*16     K16
      CHARACTER*24     KATTR(5),NOMATR,KATTRC(5),NOMATC
      PARAMETER      ( NOMATR = 'BASE GLOBALE JEVEUX' )
      PARAMETER      ( NOMATC = 'ATTRIBUTS JEVEUX' )
      CHARACTER*32     CRNOM,NGRP,NOMCOL
      CHARACTER*80     NHDF
      REAL*8           RBID
      INTEGER          IC,JULIST,LTYPI,ILONG,LONOI,IADDI(2),IADMI,IADMX
      INTEGER          IDFIC,NBMAX,JCTAB,IDG,IDGC
      INTEGER          HDFCRF,HDFCLF,HDFCRG,HDFOPG,HDFWAT,HDFCLG
      INTEGER          LIDBAS      
      PARAMETER      ( LIDBAS = 20 )
C ----------------------------------------------------------------------
      INTEGER          IDENO , ILNOM
      PARAMETER      ( IDENO=2,ILNOM=3)
C     ------------------------------------------------------------------
      INTEGER        IVNMAX     , IDDESO     , IDIADD     , IDIADM     ,
     &               IDNOM      ,
     &               IDLONO     , IDNUM
      PARAMETER    ( IVNMAX = 0 , IDDESO = 1 , IDIADD = 2 , IDIADM = 3 ,
     &               IDNOM  = 5 ,
     &               IDLONO = 8 , IDNUM  = 10 )
C DEB ------------------------------------------------------------------
      IPGCEX = IPGC
      IPGC   = -2
      KCLAS = CLAS
      NHDF  = FICHDF
      IDFIC = HDFCRF (NHDF)
      IF ( IDFIC .LT. 0 ) THEN
        CALL U2MESG('F','JEVEUX_66',1,NHDF,1,IDFIC,0,RBID)
      ENDIF
      NGRP ='/'
      IDG  = HDFOPG (IDFIC,NGRP)
      K8 = ' '
      K16 = ' '
      CALL GTOPTK('date', K16, IRET)
      CALL GTOPTK('versionD0', K8, IRET)
      KATTR(1) = K16//K8
      CALL GTOPTK('hostname', KATTR(2), IRET)
      CALL GTOPTK('system', KATTR(3), IRET)

      CALL ENLIRD(KATTR(4))
      LOUA = 1
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
        CARA(JCARA(IC)+3) = NBLMAX(IC)
        DO 5 J = 1 , NREUTI(IC)
          CRNOM = RNOM(JRNOM(IC)+J)
          DO 1 K=1,5
             KATTR(K)=' '
 1        CONTINUE
          KATTR(3)(1:1)=KCLAS
          IF ( J .GT. LIDBAS .AND.
     &        (CRNOM(25:26) .EQ. '$$' .OR. CRNOM(1:1) .EQ. '?') ) GOTO 5
          GENRI = GENR(JGENR(IC)+J)
          TYPEI = TYPE(JTYPE(IC)+J)
          LTYPI = LTYP(JLTYP(IC)+J)
          ILONG = LONG(JLONG(IC)+J)
          LONOI = LONO(JLONO(IC)+J)*LTYPI
          IADDI(1) = IADD(JIADD(IC)+2*J-1)
          IADDI(2) = IADD(JIADD(IC)+2*J  )
          IADMI = IADM(JIADM(IC)+2*J-1)
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
                  CALL U2MESK('A','JEVEUX_27',1,CRNOM)
                ENDIF
                GOTO 5
              ENDIF
              CALL JJALTY (TYPEI , LTYPI , 'L' , 1 , JCTAB)
              IADMI = IADM ( JIADM(IC) + 2*J-1 )
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
            NOMCO  = CRNOM(1:24)
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
              IADMI    = IADM ( JIADM(IC) + 2*IXDESO-1 )
              IADDI(1) = IADD ( JIADD(IC) + 2*IXDESO-1 )
              IADDI(2) = IADD ( JIADD(IC) + 2*IXDESO   )
              IADMX = IADMI
              IF ( IADMX .EQ. 0 ) THEN
                IF ( IADDI(1) .EQ. 0 ) THEN
                  IF (NIVO .GE. 2) THEN
                    CALL U2MESK('A','JEVEUX_28',1,NOMCOL(1:24))
                  ENDIF
                  GOTO 5
                ENDIF
                CALL JJALTY (TYPEI , LTYPI , 'L' , 2 , JCTAB)
                IADMI = IADM ( JIADM(IC) + 2*IXDESO-1 )
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
              IBIADM = IADM ( JIADM(IC) + 2*IXIADM-1 )
              IBIADD = IADM ( JIADM(IC) + 2*IXIADD-1 )
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
                IADMI = ISZON(JISZON + IBIADM - 1 + 2*K-1 )
                IDATOC = K
                IF ( IADMI .EQ. 0 ) THEN
                  IADDI(1) = ISZON(JISZON + IBIADD - 1 + 2*K-1 )
                  IADDI(2) = ISZON(JISZON + IBIADD - 1 + 2*K   )
                  IF ( IADDI(1) .EQ. 0 ) THEN
                    IF (NIVO .GE. 2) THEN
                      CALL U2MESG('A','JEVEUX_29',1,NOMCOL(1:24),1,K,
     &                             0,RBID)
                    ENDIF
                    GOTO 10
                  ENDIF
                  CALL JJALTY (TYPEI , LTYPI , 'L' , INAT , JCTAB)
                  IADMI  = ISZON(JISZON + IBIADM - 1 + 2*K-1 )
                ENDIF
                IXLONO = ISZON ( JISZON + IBACOL + IDLONO )
                IF ( IXLONO .EQ. 0 ) THEN
                  LONOI = LONO ( JLONO(IC) + IXDESO ) * LTYPI
                ELSE
                  IBLONO = IADM ( JIADM(IC) + 2*IXLONO-1 )
                  LONOI  = ISZON ( JISZON + IBLONO - 1 + K ) * LTYPI
                ENDIF
                IXNOM = ISZON ( JISZON + IBACOL + IDNOM )
                IF ( IXNOM .GT. 0 ) THEN
                  IBNOM = IADM ( JIADM(IC) + 2*IXNOM-1 )
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
                IADMI    = IADM (JIADM(IC) + 2*IX-1)
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
        CALL U2MESK('F','JEVEUX_55',1,NHDF)
      ENDIF
      IPGC = IPGCEX
C FIN ------------------------------------------------------------------
      END
