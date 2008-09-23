      SUBROUTINE JJLDYN ( LTOT )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 22/09/2008   AUTEUR LEFEBVRE J-P.LEFEBVRE 
C RESPONSABLE LEFEBVRE J-P.LEFEBVRE
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
C TOLE CRP_18 CRS_505 CRS_508 CRS_512 
      IMPLICIT REAL*8 (A-H,O-Z)
C ----------------------------------------------------------------------
C LIBERE LES SEGMENTS DE VALEURS ALLOUES DYNAMIQUEMENT
C
C OUT  LTOT  : LONGUEUR CUMULEE EN ENTIERS DES SEGMENTS DESALLOUES
C
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
      INTEGER          LDYN , LGDYN , NBDYN , NBFREE
      COMMON /IDYNJE/  LDYN , LGDYN , NBDYN , NBFREE
      INTEGER          ICDYN , MXLTOT
      COMMON /XDYNJE/  ICDYN , MXLTOT
      REAL *8          MXDYN , MCDYN , MLDYN , VMXDYN  
      COMMON /RDYNJE/  MXDYN , MCDYN , MLDYN , VMXDYN 
      INTEGER          LBIS , LOIS , LOLS , LOUA , LOR8 , LOC8
      COMMON /IENVJE/  LBIS , LOIS , LOLS , LOUA , LOR8 , LOC8
      INTEGER          IRECUR
      COMMON /LDYNJE/  IRECUR
C ----------------------------------------------------------------------
      INTEGER        IVNMAX     , IDDESO     , IDIADD    , IDIADM     ,
     +               IDMARQ     , IDNOM      ,             IDLONG     ,
     +               IDLONO     , IDLUTI     , IDNUM
      PARAMETER    ( IVNMAX = 0 , IDDESO = 1 , IDIADD = 2 , IDIADM = 3 ,
     +               IDMARQ = 4 , IDNOM  = 5 ,              IDLONG = 7 ,
     +               IDLONO = 8 , IDLUTI = 9  ,IDNUM  = 10 )
C ----------------------------------------------------------------------
      CHARACTER*1    CGENR
      CHARACTER*32   NOM32
      INTEGER        IADDI(2),ICOUNT,LMEMT,LGS
C
C     ON LISTE LES OBJETS ALLOUES DYNAMIQUEMENT EN BALAYANT
C     L'ENSEMBLE DES OBJETS, EN COMMENCANT PAR LA BASE VOLATILE
C 
      IF ( IRECUR .NE. 0 ) GOTO 100 
C
C     LE MECANISME DE RECUPERARION DE MEMOIRE EST MIS EN OEUVRE 
C     SI LA VARIABLE IRECUR EST NULLE.
C
C
      ICDYN = ICDYN+1
      LTOT = 0
      NCLA1 = 1
      NCLA2 = INDEX ( CLASSE , '$' ) - 1
      IF (NCLA2 .LT. 0) NCLA2 = N
      DO 200  IC = NCLA2 , NCLA1, - 1 
        DO 205 J = 1 , NREMAX(IC)
          IADMI = IADM(JIADM(IC)+2*J-1)
          IF ( IADMI .EQ. 0 ) GOTO 205
          IADYN = IADM(JIADM(IC)+2*J  )
          CGENR = GENR(JGENR(IC)+J)
          NOM32 = RNOM(JRNOM(IC)+J)
          IF (CGENR .EQ. 'X') THEN
            CALL JJVERN (NOM32 , 0 , IRET)
            CALL JJALLC (IC , J , 'L' , IBACOL)
            IXIADM = ISZON ( JISZON + IBACOL + IDIADM )
            IXIADD = ISZON ( JISZON + IBACOL + IDIADD )
            IXDESO = ISZON ( JISZON + IBACOL + IDDESO )
            IXLONO = ISZON ( JISZON + IBACOL + IDLONO )
            NMAX   = ISZON ( JISZON + IBACOL + IVNMAX )
            IF (IXIADM .GT. 0) THEN
              IBIADM = IADM ( JIADM(IC) + 2*IXIADM-1 )
              IBIADD = IADM ( JIADM(IC) + 2*IXIADD-1 )
              DO 210 K=1,NMAX
                IADMOC = ISZON(JISZON + IBIADM - 1 +2*K-1)
                IADYOC = ISZON(JISZON + IBIADM - 1 +2*K  )
                IF (IADYOC .NE. 0) THEN
                  IDM  = IADMOC - 4 
                  ISD  = ISZON(JISZON + IDM + 3) / ISSTAT
                  ISF  = ISZON(JISZON + ISZON(JISZON+IDM) - 4) / ISSTAT
                  IL = ISZON(JISZON+IDM) - 8 - IDM 
                  IF ( ISD .EQ. 1 ) THEN
C
C     LE SEGMENT DE VALEURS EST MARQUE X A OU X D, ON PEUT LE LIBERER
C
                    IF ( IXLONO .NE. 0 ) THEN
                      IBLONO = IADM ( JIADM(IC) + 2*IXLONO-1 )
                      LONOI = ISZON(JISZON + IBLONO - 1 + K)
                    ELSE
                      LONOI = LONO(JLONO(IC)+ IXDESO)
                    ENDIF
                    LTYPI = LTYP( JLTYP(IC)+IXDESO )
                    LSV   = LONOI * LTYPI
                    IF ( ISF .EQ. 4 ) THEN
C
C     LE SEGMENT DE VALEURS EST MARQUE X D, IL FAUT D'ABORD L'ECRIRE
C
                      IADDI(1) = ISZON(JISZON + IBIADD -1 + 2*K-1)
                      IADDI(2) = ISZON(JISZON + IBIADD -1 + 2*K  )
                      CALL JXECRO ( IC, IADMOC, IADDI, LSV, 0, K)
                      ISZON(JISZON + IBIADD -1 + 2*K-1) = IADDI(1)
                      ISZON(JISZON + IBIADD -1 + 2*K  ) = IADDI(2)
                    ENDIF
                    LGS = ISZON(JISZON+IADMOC-4) - IADMOC + 5 
                    MCDYN = MCDYN - LGS*LOIS 
                    MLDYN = MLDYN + LGS*LOIS
                    CALL HPDEALLC ( IADYOC , NBFREE , IBID )
C                   write(6,*) ' OC ',NOM32,' objet ',K,' lg =',IL,LSV
                    LTOT = LTOT + IL
                    ISZON(JISZON + IBIADM - 1 +2*K-1) = 0
                    ISZON(JISZON + IBIADM - 1 +2*K  ) = 0
                  ENDIF
                ENDIF  
 210          CONTINUE  
            ENDIF
            IRECUR = 999
            CALL JJLIDE ('JEIMPO' , NOM32(1:24) , 2)
            IRECUR = 0
            GOTO 205
          ELSE IF ( NOM32(25:32) .EQ. ' ' ) THEN 
            IF (IADYN .NE. 0) THEN
              IDM   = IADMI - 4 
              ISD  = ISZON(JISZON + IDM + 3) / ISSTAT
              ISF  = ISZON(JISZON + ISZON(JISZON+IDM) - 4) / ISSTAT
              IL = ISZON(JISZON+IDM) - 8 - IDM 
              IF ( ISD .EQ. 1 ) THEN
C
C     LE SEGMENT DE VALEURS EST MARQUE X A OU X D, ON PEUT LE LIBERER
C
                LTYPI = LTYP( JLTYP(IC)+J )
                LSV   = LONO( JLONO(IC)+J ) * LTYPI
                IF ( ISF .EQ. 4 ) THEN
C
C     LE SEGMENT DE VALEURS EST MARQUE X D, IL FAUT D'ABORD L'ECRIRE
C
                  IADDI(1) = IADD ( JIADD(IC)+2*J-1 )
                  IADDI(2) = IADD ( JIADD(IC)+2*J   )
                  CALL JXECRO ( IC, IADMI, IADDI, LSV, 0, J)
                  IADD( JIADD(IC)+2*J-1 ) = IADDI(1)
                  IADD( JIADD(IC)+2*J   ) = IADDI(2)
                ENDIF
                LGS = ISZON(JISZON+IADMI-4) - IADMI + 5 
                MCDYN = MCDYN - LGS*LOIS
                MLDYN = MLDYN + LGS*LOIS
                CALL HPDEALLC ( IADYN , NBFREE , IBID )
C               write(6,*) ' OS ',NOM32,' lg =',IL,LSV
                LTOT = LTOT + IL
                IADM(JIADM(IC)+2*J-1) = 0 
                IADM(JIADM(IC)+2*J  ) = 0
              ENDIF
            ENDIF
          ENDIF 
 205    CONTINUE
 200  CONTINUE
      MXLTOT=MXLTOT+(LTOT*LOIS)/(1024*1024)
C
 100  CONTINUE
C
      END
