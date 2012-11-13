      SUBROUTINE JEIMPM ( UNIT )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C RESPONSABLE LEFEBVRE J-P.LEFEBVRE
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
      IMPLICIT NONE
      INCLUDE 'jeveux_private.h'
      INTEGER          UNIT
C ----------------------------------------------------------------------
C IMPRIME LA SEGMENTATION DE LA MEMOIRE
C
C IN  UNIT  : NUMERO D'UNITE LOGIQUE ASSOCIE AU FICHIER D'IMPRESSION
C ----------------------------------------------------------------------
      INTEGER          LK1ZON , JK1ZON , LISZON , JISZON 
      COMMON /IZONJE/  LK1ZON , JK1ZON , LISZON , JISZON
C ----------------------------------------------------------------------
C-----------------------------------------------------------------------
      INTEGER IADMI ,IADMOC ,IADYN ,IADYOC ,IBACOL ,IBIADM ,IBMARQ 
      INTEGER IC ,IDCO ,IDM ,IL ,IM ,ISD ,ISF 
      INTEGER IXIADM ,IXMARQ ,J ,JCARA ,JDATE ,JDOCU ,JGENR 
      INTEGER JHCOD ,JIADD ,JIADM ,JLONG ,JLONO ,JLTYP ,JLUTI 
      INTEGER JMARQ ,JORIG ,JRNOM ,JTYPE ,N ,NCLA1 ,NCLA2 
      INTEGER NMAX 
C-----------------------------------------------------------------------
      PARAMETER  ( N = 5 )
      COMMON /JIATJE/  JLTYP(N), JLONG(N), JDATE(N), JIADD(N), JIADM(N),
     +                 JLONO(N), JHCOD(N), JCARA(N), JLUTI(N), JMARQ(N)
      CHARACTER*2      DN2
      CHARACTER*5      CLASSE
      CHARACTER*8                  NOMFIC    , KSTOUT    , KSTINI
      COMMON /KFICJE/  CLASSE    , NOMFIC(N) , KSTOUT(N) , KSTINI(N) ,
     +                 DN2(N)
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
C ----------------------------------------------------------------------
      INTEGER        IVNMAX       , IDIADM     ,
     +               IDMARQ
      PARAMETER    ( IVNMAX = 0   , IDIADM = 3 ,
     +               IDMARQ = 4   )
C ----------------------------------------------------------------------
      CHARACTER*32     NOM32
      CHARACTER*8      NOM8
      CHARACTER*1      CLA,CGENR
      INTEGER           K
      REAL*8           VUSTA,VUDYN,VXSTA,VXDYN
C DEB ------------------------------------------------------------------
C
      IF ( UNIT .LE. 0 ) GOTO 9999
      VUSTA = 0.D0
      VUDYN = 0.D0
      VXSTA = 0.D0
      VXDYN = 0.D0
C
C     ON LISTE MAINTENANT LES OBJETS ALLOUES DYNAMIQUEMENT
C
      WRITE (UNIT,'(4A)' ) ('--------------------',K=1,4)
      WRITE (UNIT,'(A)') 'OBJETS ALLOUES DYNAMIQUEMENT '
      WRITE (UNIT,'(4A)' ) ('--------------------',K=1,4)
      WRITE (UNIT,'(/A,A/)')
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
            WRITE(UNIT,
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
                    WRITE(UNIT,
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
      WRITE(UNIT,*) '  '
      WRITE(UNIT,*) ' CUMUL DES LONGUEURS DES SEGMENTS UTILISES UA/UD'
      WRITE(UNIT,60) ' ALLOCATION STATIQUE  :',VUSTA*LOIS/(1024*1024),
     +                ' Mo'
      WRITE(UNIT,60) ' ALLOCATION DYNAMIQUE :',VUDYN*LOIS/(1024*1024),
     +                ' Mo'
      WRITE(UNIT,60) ' ALLOCATION TOTALE    :',(VUSTA+VUDYN)*LOIS
     +                /(1024*1024),' Mo',(VUSTA+VUDYN)*LOIS,' o '
      WRITE(UNIT,*) '  '
      WRITE(UNIT,*) ' CUMUL DES LONGUEURS DES SEGMENTS DECHARGEABLES'
     +                //' XA/XD'
      WRITE(UNIT,60) ' ALLOCATION STATIQUE  :',VXSTA*LOIS/(1024*1024),
     +                ' Mo'
      WRITE(UNIT,60) ' ALLOCATION DYNAMIQUE :',VXDYN*LOIS/(1024*1024),
     +                ' Mo'
      WRITE(UNIT,60) ' ALLOCATION TOTALE    :',(VXSTA+VXDYN)*LOIS
     +                /(1024*1024),' Mo',(VXSTA+VXDYN)*LOIS,' o '
      WRITE(UNIT,*) '  '
      WRITE(UNIT,60) ' ESPACE MEMOIRE JEVEUX OCCUPE    :',
     +  (VUSTA+VUDYN+VXSTA+VXDYN)*LOIS/(1024*1024),' Mo',
     +  (VUSTA+VUDYN+VXSTA+VXDYN)*LOIS,' o '
C
      CALL JXVERI(' ')
C
 60   FORMAT(A,2(1PE12.2,A3))
 9999 CONTINUE
C FIN ------------------------------------------------------------------
      END
