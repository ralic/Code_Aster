      SUBROUTINE JEIMPM ( CUNIT , CMESS )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C RESPONSABLE LEFEBVRE J-P.LEFEBVRE
C MODIF JEVEUX  DATE 08/11/2011   AUTEUR LEFEBVRE J-P.LEFEBVRE 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C TOLE CRP_18 CRS_508 CRS_512
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)       CUNIT , CMESS
C ----------------------------------------------------------------------
C IMPRIME LA SEGMENTATION DE LA MEMOIRE
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
C
      INTEGER          NRHCOD    , NREMAX    , NREUTI
      COMMON /ICODJE/  NRHCOD(N) , NREMAX(N) , NREUTI(N)
      INTEGER          ISSTAT
      COMMON /ICONJE/  ISSTAT
      CHARACTER *4     KSTAT
      COMMON /KSTAJE/  KSTAT
      INTEGER          LBIS , LOIS , LOLS , LOR8 , LOC8
      COMMON /IENVJE/  LBIS , LOIS , LOLS , LOR8 , LOC8
      INTEGER          IDINIT   ,IDXAXD   ,ITRECH,ITIAD,ITCOL,LMOTS,IDFR
      COMMON /IXADJE/  IDINIT(2),IDXAXD(2),ITRECH,ITIAD,ITCOL,LMOTS,IDFR
C ----------------------------------------------------------------------
      INTEGER        IVNMAX     , IDDESO     , IDIADD    , IDIADM     ,
     +               IDMARQ     , IDNOM      ,             IDLONG     ,
     +               IDLONO     , IDLUTI     , IDNUM
      PARAMETER    ( IVNMAX = 0 , IDDESO = 1 , IDIADD = 2 , IDIADM = 3 ,
     +               IDMARQ = 4 , IDNOM  = 5 ,              IDLONG = 7 ,
     +               IDLONO = 8 , IDLUTI = 9  ,IDNUM  = 10 )
C ----------------------------------------------------------------------
      CHARACTER*32     NOM32
      CHARACTER*8      NOM8
      CHARACTER*1      CLA,CGENR
      INTEGER          ICL , K
      REAL*8           VUSTA,VUDYN,VXSTA,VXDYN
C DEB ------------------------------------------------------------------
C
      JULIST = IUNIFI ( CUNIT )
      IF ( JULIST .EQ. 0 ) GOTO 9999
      VUSTA = 0.D0
      VUDYN = 0.D0
      VXSTA = 0.D0
      VXDYN = 0.D0
      WRITE (JULIST,'(4A)' ) ('--------------------',K=1,4)
      WRITE (JULIST,'(A,/,2A)' ) '---- SEGMENTATION MEMOIRE',
     +                   '---- ',CMESS(1:MIN(72,LEN(CMESS)))
      WRITE (JULIST,'(4A)' ) ('--------------------',K=1,4)
      K  = 1
      DO 100 IZ=1,2
        ID = IDINIT(IZ)
        IF (ID .EQ. 0) GOTO 100
        WRITE (JULIST,'(4A)' ) ('--------------------',K=1,4)
        WRITE (JULIST,'(A,I4)') 'PARTITION : ',IZ
        WRITE (JULIST,'(4A)' ) ('--------------------',K=1,4)
        WRITE ( JULIST , '(/A,A/)')
     +        ' CL-  --NUM-- -MA-  -----IADM----- -U- - LON UA -  -S- ',
     +        '------------- NOM --------------'
   10   CONTINUE
        IS = ISZON ( JISZON + ID )
        IF ( IS .NE. 0 ) THEN
          ISD  = ISZON(JISZON + ID + 3 ) / ISSTAT
          CALL ASSERT( (ISD.GT.0) .AND. (ISD.LT.5) )
          IDOS = ISZON(JISZON + ID + 2 )
          ISF  = ISZON(JISZON + IS - 4 ) / ISSTAT
          CALL ASSERT( (ISF.GT.0) .AND. (ISF.LT.5) )
          ICL  = ISZON(JISZON + IS - 2 )
          CALL ASSERT( ICL.LE.5 )
          IDCO = ISZON(JISZON + IS - 3 )
          CLA = ' '
          IF ( ICL .GT. 0 ) CLA = CLASSE(ICL:ICL)
          IDM = ID + 4
          NOM32 = ' '
          IM = 0
          IF ( ISF .NE. 1 .AND. IDOS .NE. 0 ) THEN
            IF ( IDCO .EQ. 0 ) THEN
              NOM32 = RNOM(JRNOM(ICL)+IDOS)
              IM = IMARQ(JMARQ(ICL)+2*IDOS-1)
            ELSE
              NOM32(1:24) = RNOM(JRNOM(ICL)+IDCO)
              IBACOL = IADM(JIADM(ICL)+2*IDCO-1)
              IXMARQ = ISZON( JISZON+IBACOL+IDMARQ )
              IBMARQ = IADM(JIADM(ICL)+2*IXMARQ-1)
              IM = ISZON(JISZON+IBMARQ-1+2*IDOS-1)
              WRITE ( NOM32(25:32) , '(I8)') IDOS
            ENDIF
          ENDIF
          IL  = ( IS - ID - 8 )
          IF (KSTAT(ISD:ISD) .EQ.'X' .AND. KSTAT(ISF:ISF) .EQ.'X') THEN
            IDOS = 0
            IDCO = 0
            NOM32 = '<<<<         LIBRE          >>>>'
          ENDIF
          IF ( ISD .EQ. 2 ) THEN  
            VUSTA = VUSTA + (IS - ID)
          ELSE
            VXSTA = VXSTA + (IS - ID)
          ENDIF
          WRITE(JULIST,
     +        '(''|'',A1,''|'',I4,''|'',I8,''|'',I4,''|'','//
     +        'I12,''|'',A1,''|'',I11,''| '',A1,''| '',A)')
     +     CLA,IDCO,IDOS,IM,IDM,KSTAT(ISD:ISD),IL,KSTAT(ISF:ISF),NOM32
          K   = K + 1
          ID  = IS
          GO TO 10
        ENDIF
 100  CONTINUE
C
C     ON LISTE MAINTENANT LES OBJETS ALLOUES DYNAMIQUEMENT
C
      WRITE (JULIST,'(4A)' ) ('--------------------',K=1,4)
      WRITE (JULIST,'(A)') 'OBJETS ALLOUES DYNAMIQUEMENT '
      WRITE (JULIST,'(4A)' ) ('--------------------',K=1,4)
      WRITE ( JULIST , '(/A,A/)')
     +      ' CL-  --NUM-- -MA-  ---------IADY--------- -U- - LON UA',
     +      ' -  -S- ------------- NOM --------------'
      NCLA1 = 1
      NCLA2 = INDEX ( CLASSE , '$' ) - 1
      IF (NCLA2 .LT. 0) NCLA2 = N
      DO 200  IC = NCLA1 , NCLA2
        CLA = CLASSE(IC:IC)
        DO 205 J = 1 , NREMAX(IC)
          IDCO = 0
          IADMI = IADM(JIADM(IC)+2*J-1)
          IADYN = IADM(JIADM(IC)+2*J  )
          CGENR = GENR(JGENR(IC)+J)
          NOM32 = RNOM(JRNOM(IC)+J)
          IF ( NOM32 .EQ. ' ' .OR. NOM32 .EQ. '?' ) GOTO 205
          IF (IADYN .NE. 0) THEN
            IDM   = IADMI - 4
            IM = IMARQ(JMARQ(IC)+2*J-1)
            IL = ISZON(JISZON+IDM) - 8 - IDM
            ISD  = ISZON(JISZON + IDM + 3) / ISSTAT
            CALL ASSERT( (ISD.GT.0) .AND. (ISD.LT.5) )
            ISF  = ISZON(JISZON + ISZON(JISZON+IDM) - 4) / ISSTAT
            CALL ASSERT( (ISF.GT.0) .AND. (ISF.LT.5) )
            WRITE(JULIST,
     +        '(''|'',A1,''|'',I4,''|'',I8,''|'',I4,''|'','//
     +        'I20,''|'',A1,''|'',I11,''| '',A1,''| '',A)')
     +        CLA,IDCO,J,IM,IADYN,KSTAT(ISD:ISD),IL,KSTAT(ISF:ISF),NOM32
            IF ( ISD .EQ. 2 ) THEN  
              VUDYN = VUDYN + ISZON(JISZON+IDM) - IDM + 1
            ELSE
              VXDYN = VXDYN + ISZON(JISZON+IDM) - IDM + 1     
            ENDIF
          ENDIF
          IF (CGENR .EQ. 'X' .AND. IADMI .NE. 0) THEN
            NOM32  = RNOM(JRNOM(IC)+J)(1:24)                
            IBACOL = IADMI
            IXIADM = ISZON ( JISZON + IBACOL + IDIADM )
            IXMARQ = ISZON ( JISZON + IBACOL + IDMARQ )
            NMAX   = ISZON ( JISZON + IBACOL + IVNMAX )
            IF (IXIADM .GT. 0) THEN
              IBIADM = IADM ( JIADM(IC) + 2*IXIADM-1 )
              IBMARQ = IADM ( JIADM(IC) + 2*IXMARQ-1 )
              IF (IBIADM .NE. 0) THEN         
                DO 210 K=1,NMAX
                  IADMOC = ISZON(JISZON + IBIADM - 1 +2*K-1)
                  IADYOC = ISZON(JISZON + IBIADM - 1 +2*K  )
                  IF (IADYOC .NE. 0) THEN
                    IDM   = IADMOC - 4
                    IM = ISZON(JISZON + IBMARQ - 1 + 2*K-1)
                    IL = ISZON(JISZON+IDM) - 8 - IDM
                    ISD  = ISZON(JISZON + IDM + 3) / ISSTAT
                    ISF  = ISZON(JISZON + ISZON(JISZON+IDM) - 4)/ISSTAT
                    WRITE(NOM8,'(I8)') K
                    NOM32  = RNOM(JRNOM(IC)+J)(1:24)//NOM8
                    WRITE(JULIST,
     +              '(''|'',A1,''|'',I4,''|'',I8,''|'',I4,''|'','//
     +              'I20,''|'',A1,''|'',I11,''| '',A1,''| '',A)')
     +              CLA,J,K,IM,IADYOC,KSTAT(ISD:ISD),IL,
     +              KSTAT(ISF:ISF),NOM32
                    IF ( ISD .EQ. 2 ) THEN  
                      VUDYN = VUDYN + ISZON(JISZON+IDM) - IDM + 1
                    ELSE
                      VXDYN = VXDYN + ISZON(JISZON+IDM) - IDM + 1     
                    ENDIF
                  ENDIF
 210            CONTINUE
              ENDIF
            ENDIF
          ENDIF
 205    CONTINUE
 200  CONTINUE
C      
      WRITE(JULIST,*) '  '
      WRITE(JULIST,*) ' CUMUL DES LONGUEURS DES SEGMENTS UTILISES UA/UD'
      WRITE(JULIST,60) ' ALLOCATION STATIQUE  :',VUSTA*LOIS/(1024*1024),
     +                ' Mo'
      WRITE(JULIST,60) ' ALLOCATION DYNAMIQUE :',VUDYN*LOIS/(1024*1024),
     +                ' Mo'
      WRITE(JULIST,60) ' ALLOCATION TOTALE    :',(VUSTA+VUDYN)*LOIS
     +                /(1024*1024),' Mo',(VUSTA+VUDYN)*LOIS,' o '
      WRITE(JULIST,*) '  '
      WRITE(JULIST,*) ' CUMUL DES LONGUEURS DES SEGMENTS DECHARGEABLES'
     +                //' XA/XD'
      WRITE(JULIST,60) ' ALLOCATION STATIQUE  :',VXSTA*LOIS/(1024*1024),
     +                ' Mo'
      WRITE(JULIST,60) ' ALLOCATION DYNAMIQUE :',VXDYN*LOIS/(1024*1024),
     +                ' Mo'
      WRITE(JULIST,60) ' ALLOCATION TOTALE    :',(VXSTA+VXDYN)*LOIS
     +                /(1024*1024),' Mo',(VXSTA+VXDYN)*LOIS,' o '
      WRITE(JULIST,*) '  '
      WRITE(JULIST,60) ' ESPACE MEMOIRE JEVEUX OCCUPE    :',
     +  (VUSTA+VUDYN+VXSTA+VXDYN)*LOIS/(1024*1024),' Mo',
     +  (VUSTA+VUDYN+VXSTA+VXDYN)*LOIS,' o '
C     
      CALL JXVERI(CUNIT , CMESS)  
C      
 60   FORMAT(A,2(1PE12.2,A3))
 9999 CONTINUE
C FIN ------------------------------------------------------------------
      END
