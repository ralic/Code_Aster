      SUBROUTINE JXVERI (CUNIT , CMESS)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 20/10/2008   AUTEUR LEFEBVRE J-P.LEFEBVRE 
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
C TOLE CFT_720 CFT_726 CFT_889 CRP_18 CRS_508 CRS_505 CRS_512
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)       CUNIT , CMESS
C ----------------------------------------------------------------------
C VERIFIE L'INTEGRITE DU CHAINAGE AVANT DES SEGMENTS DE VALEURS ET DE LA
C ZONE MEMOIRE UTILISEE
C
C IN  CUNIT  : NOM LOCAL DU FICHIER D'IMPRESSION
C IN  CMESS  : MESSAGE D'INFORMATION
C ----------------------------------------------------------------------
      CHARACTER*1      K1ZON
      COMMON /KZONJE/  K1ZON(8)
      INTEGER          LK1ZON , JK1ZON , LISZON , JISZON , ISZON(1)
      COMMON /IZONJE/  LK1ZON , JK1ZON , LISZON , JISZON
      EQUIVALENCE    ( ISZON(1) , K1ZON(1) )
C ----------------------------------------------------------------------
      INTEGER          ISSTAT
      COMMON /ICONJE/  ISSTAT
      INTEGER          ISTAT
      COMMON /ISTAJE/  ISTAT(4)
      PARAMETER  ( N = 5 )
      INTEGER          LTYP    , LONG    , DATE    , IADD    , IADM    ,
     +                 LONO    , HCOD    , CARA    , LUTI    , IMARQ   
      COMMON /IATRJE/  LTYP(1) , LONG(1) , DATE(1) , IADD(1) , IADM(1) ,
     +                 LONO(1) , HCOD(1) , CARA(1) , LUTI(1) , IMARQ(1)
      COMMON /JIATJE/  JLTYP(N), JLONG(N), JDATE(N), JIADD(N), JIADM(N),
     +                 JLONO(N), JHCOD(N), JCARA(N), JLUTI(N), JMARQ(N)
      CHARACTER*2      DN2
      CHARACTER*5      CLASSE
      CHARACTER*8                  NOMFIC    , KSTOUT    , KSTINI
      COMMON /KFICJE/  CLASSE    , NOMFIC(N) , KSTOUT(N) , KSTINI(N) ,
     +                 DN2(N)
      CHARACTER*1      GENR    , TYPE
      CHARACTER*4      DOCU
      CHARACTER*8      ORIG
      CHARACTER*32     RNOM
      COMMON /KATRJE/  GENR(8) , TYPE(8) , DOCU(2) , ORIG(1) , RNOM(1)
      COMMON /JKATJE/  JGENR(N), JTYPE(N), JDOCU(N), JORIG(N), JRNOM(N)
      INTEGER          IDINIT   ,IDXAXD   ,ITRECH,ITIAD,ITCOL,LMOTS,IDFR
      COMMON /IXADJE/  IDINIT(2),IDXAXD(2),ITRECH,ITIAD,ITCOL,LMOTS,IDFR
      INTEGER          NRHCOD    , NREMAX    , NREUTI
      COMMON /ICODJE/  NRHCOD(N) , NREMAX(N) , NREUTI(N)
      INTEGER          LDYN , LGDYN , NBDYN , NBFREE
      COMMON /IDYNJE/  LDYN , LGDYN , NBDYN , NBFREE
      INTEGER        IVNMAX     , IDDESO     , IDIADD     , IDIADM     ,
     &               IDMARQ     , IDNOM      ,              IDLONG     ,
     &               IDLONO     , IDLUTI     , IDNUM
      PARAMETER    ( IVNMAX = 0 , IDDESO = 1 , IDIADD = 2 , IDIADM = 3 ,
     &               IDMARQ = 4 , IDNOM  = 5 ,              IDLONG = 7 ,
     &               IDLONO = 8 , IDLUTI = 9 , IDNUM  = 10 )
C ----------------------------------------------------------------------
      CHARACTER*32     NOM32
      CHARACTER*1      CGENR
      INTEGER          ICL
C DEB ------------------------------------------------------------------
C
C     ON EXAMINE LA SEGMENTATION MEMOIRE ASSOCIEE A LA GESTION STATIQUE
C   
      NOM32  = '??'
      DO 100 IZ=1,2
        ID = IDINIT(IZ)
        CALL ASSERT( (ID.GE.0) .AND. (ID.LE.LISZON) )
        IF (ID .EQ. 0) GOTO 100
 10     CONTINUE
        IS = ISZON ( JISZON + ID )
        IF ( IS .NE. 0 ) THEN
          CALL ASSERT( (IS.GT.ID+8) .AND. (IS.LE.LISZON) )
          ISD  = ISZON(JISZON + ID + 3)
          CALL ASSERT( ISD.EQ.ISTAT(1) .OR. ISD.EQ.ISTAT(2) )
          ICL  = ISZON(JISZON + IS - 2)
          CALL ASSERT( ICL.GE.0 .AND. ICL.LE.N )
          IDOS = ISZON(JISZON + ID + 2)
          CALL ASSERT( IDOS.GE.0 )
          ISF  = ISZON(JISZON + IS - 4)
          CALL ASSERT( ISF.EQ.ISTAT(3) .OR. ISF.EQ.ISTAT(4)
     &                                 .OR. ISF.EQ.ISTAT(1) )
          IDCO = ISZON(JISZON + IS - 3)
          CALL ASSERT( IDCO.GE.0 .AND. IDCO.LE. NREMAX(ICL))
          NOM32 = ' '
          IF ( ISF .NE. ISTAT(1) .AND. IDOS .NE. 0 ) THEN
             IF ( IDCO .EQ. 0 ) THEN
                CALL ASSERT( IDOS.GT.0 .AND. IDOS.LE. NREMAX(ICL))
                NOM32 = RNOM(JRNOM(ICL)+IDOS)
             ELSE
                NOM32(1:24) = RNOM(JRNOM(ICL)+IDCO)
                WRITE ( NOM32(25:32) , '(I8)') IDOS
             ENDIF
          ENDIF
          IF ( ISD .EQ. ISTAT(1) .AND. ISF .EQ. ISTAT(1) ) THEN
            NOM32 = '<<<<    ZONE LIBRE          >>>>'
          ENDIF
          IF ( (ISD.LT.ISTAT(1).OR.ISD.GT.ISTAT(2)) ) THEN
            CALL U2MESK('F','JEVEUX_15',1,NOM32)
          ELSE IF ( (ISF.NE.ISTAT(1)).AND.(ISF.LT.ISTAT(3)
     &        .OR. ISF.GT.ISTAT(4))) THEN
            CALL U2MESK('F','JEVEUX_16',1,NOM32)
          ENDIF
          ID  = IS
          GO TO 10
        ELSE
          ISD = ISZON(JISZON + ID + 3)
          IF (ISD.NE.ISSTAT) THEN   
            CALL U2MESK('F','JEVEUX_17',1,NOM32)
          END IF
          IF ( IZ .EQ. 1 ) THEN
            IF ( IDINIT(2) .EQ. 0 ) THEN
              IF ( ID .NE. LISZON-3 ) THEN
                CALL U2MESK('F','JEVEUX_17',1,NOM32)
              ENDIF
            ELSE
              IF ( ID .NE. IDINIT(2)-8 ) THEN
                CALL U2MESK('F','JEVEUX_17',1,NOM32)
              ENDIF
            ENDIF
          ELSE
            IF ( ID .NE. LISZON-3 ) THEN
              CALL U2MESK('F','JEVEUX_17',1,NOM32)
            ENDIF
          ENDIF
        ENDIF
 100  CONTINUE
C
C     ON TRAITE MAINTENANT LES OBJETS ALLOUES EN MEMOIRE DYNAMIQUE
C   
      IF ( LDYN .NE. 1 .AND. LDYN.NE. 2 ) GOTO 300      
      NCLA1 = 1
      NCLA2 = INDEX ( CLASSE , '$' ) - 1
      IF (NCLA2 .LT. 0) NCLA2 = N
      DO 200  IC = NCLA2 , NCLA1, - 1 
        DO 205 J = 1 , NREMAX(IC)
          IADMI = IADM(JIADM(IC)+2*J-1)
          IADYN = IADM(JIADM(IC)+2*J  )
          IF ( IADMI .EQ. 0 .OR. IADYN .EQ. 0) GOTO 205
          CGENR = GENR(JGENR(IC)+J)
          NOM32 = RNOM(JRNOM(IC)+J)
C
          ISDC  = ISZON(JISZON + IADMI - 1) / ISSTAT
          CALL ASSERT( ISDC.EQ.1 .OR. ISDC.EQ.2 )
          IF (CGENR .EQ. 'X' .AND. ISDC .EQ. 2) THEN  
            CALL JJVERN (NOM32 , 0 , IRET)
            CALL JJALLC (IC , J , 'L' , IBACOL)
            IXIADM = ISZON ( JISZON + IBACOL + IDIADM )
            IXIADD = ISZON ( JISZON + IBACOL + IDIADD )
            IXDESO = ISZON ( JISZON + IBACOL + IDDESO )
            IXLONO = ISZON ( JISZON + IBACOL + IDLONO )
            NMAX   = ISZON ( JISZON + IBACOL + IVNMAX )
            IF (IXIADM .GT. 0) THEN
              IBIADM = IADM ( JIADM(IC) + 2*IXIADM-1 )
              DO 210 K=1,NMAX
                IADMOC = ISZON(JISZON + IBIADM - 1 +2*K-1)
                IADYOC = ISZON(JISZON + IBIADM - 1 +2*K  )
                IF (IADYOC .NE. 0) THEN
                  IDM  = IADMOC - 4 
                  ISD  = ISZON(JISZON + IDM + 3) / ISSTAT
                  CALL ASSERT( ISD.EQ.1 .OR. ISD.EQ.2 )
                  ISF  = ISZON(JISZON + ISZON(JISZON+IDM) - 4) / ISSTAT
                  CALL ASSERT( ISF.EQ.3 .OR. ISF.EQ.4 )
                  IL = ISZON(JISZON+IDM) - 8 - IDM 
                  CALL ASSERT( IL .GT. 0 )
                ENDIF  
 210          CONTINUE  
            ENDIF
            CALL JJLIDE ('JEIMPO' , NOM32(1:24) , 2)
            GOTO 205
          ELSE 
            IDM = IADMI - 4 
            ISD = ISZON(JISZON + IDM + 3) / ISSTAT
            CALL ASSERT( ISD.EQ.1 .OR. ISD.EQ.2 )
            ISF = ISZON(JISZON + ISZON(JISZON+IDM) - 4) / ISSTAT
            CALL ASSERT( ISF.EQ.3 .OR. ISF.EQ.4 )
            IL  = ISZON(JISZON+IDM) - 8 - IDM 
            CALL ASSERT( IL .GT. 0 )
          ENDIF 
 205    CONTINUE
 200  CONTINUE
C
 300  CONTINUE
C FIN ------------------------------------------------------------------
      END
