      SUBROUTINE JJLDYN ( IMODE , LMIN , LTOT )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 20/12/2011   AUTEUR COURTOIS M.COURTOIS 
C RESPONSABLE LEFEBVRE J-P.LEFEBVRE
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
C TOLE CRP_18 CRS_505 CRS_508 CRS_512
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER             IMODE , LMIN , LTOT
C ----------------------------------------------------------------------
C LIBERE LES SEGMENTS DE VALEURS ALLOUES DYNAMIQUEMENT
C
C IN   IMODE :
C              =1 ON NE TRAITE QUE LA BASE VOLATILEC
C              =2 ON NE TRAITE QUE LES OBJETS XA
C              =3 ON NE TRAITE QUE LES OBJETS XA DE LA BASE VOLATILE
C              SINON ON EXAMINE TOUTE LA MEMOIRE
C IN   LMIN  : TAILLE MINIMUM EN ENTIERS REQUISE
C              =< 0 ON LIBERE TOUT
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
      INTEGER          NBLMAX    , NBLUTI    , LONGBL    ,
     &                 KITLEC    , KITECR    ,             KIADM    ,
     &                 IITLEC    , IITECR    , NITECR    , KMARQ
      COMMON /IFICJE/  NBLMAX(N) , NBLUTI(N) , LONGBL(N) ,
     &                 KITLEC(N) , KITECR(N) ,             KIADM(N) ,
     &                 IITLEC(N) , IITECR(N) , NITECR(N) , KMARQ(N)
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
      COMMON /IACCED/  IACCE(1)
      COMMON /JIACCE/  JIACCE(N),NBACCE(2*N)
      COMMON /KINDIR/  INDIR(1)
      COMMON /JINDIR/  JINDIR(N)
      INTEGER          ISSTAT
      COMMON /ICONJE/  ISSTAT
      INTEGER          LDYN , LGDYN , NBDYN , NBFREE
      COMMON /IDYNJE/  LDYN , LGDYN , NBDYN , NBFREE
      INTEGER          ICDYN , MXLTOT
      COMMON /XDYNJE/  ICDYN , MXLTOT
      REAL *8          MXDYN , MCDYN , MLDYN , VMXDYN , LGIO
      COMMON /RDYNJE/  MXDYN , MCDYN , MLDYN , VMXDYN , LGIO(2)
      INTEGER          LBIS , LOIS , LOLS , LOR8 , LOC8
      COMMON /IENVJE/  LBIS , LOIS , LOLS , LOR8 , LOC8
      INTEGER          DATEI
      COMMON /IHEUJE/  DATEI
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
      INTEGER        IADDI(2),LGS,NBIOAV(2)
      INTEGER        RANG, NBPROC
      REAL*8         GRAINE

      CALL UTTCPU('CPU.MEMD.1','DEBUT',' ')
C
C     ON LISTE LES OBJETS ALLOUES DYNAMIQUEMENT EN BALAYANT
C     L'ENSEMBLE DES OBJETS, EN COMMENCANT PAR LA BASE VOLATILE
C
      ICDYN = ICDYN+1
      LTOT = 0
      NCLA1 = 1
      NCLA2 = INDEX ( CLASSE , '$' ) - 1
      IF (NCLA2 .LT. 0) NCLA2 = N
      IF (IMODE .EQ. 1 .OR. IMODE .EQ. 3) THEN
        NCLA2 = INDEX ( CLASSE , 'V' )
        NCLA1 = NCLA2
      ENDIF
      DO 200  IC = NCLA2 , NCLA1, - 1
        IF (NREUTI(IC) .EQ. 0) GOTO 200
        CALL MPICM0(RANG, NBPROC)
        IF ( RANG .NE. 0 ) THEN
          GRAINE = (RANG+1)*DATEI*1.5D0
          DO 202 I= 2,NREUTI(IC)
            CALL RANDOM(GRAINE)
            K = INT(GRAINE*I)+1
            J = INDIR(JINDIR(IC)+I)
            INDIR(JINDIR(IC)+I) = INDIR(JINDIR(IC)+K)
            INDIR(JINDIR(IC)+K) = J
 202      CONTINUE
        ENDIF
C
        NBIOAV(1) = NBACCE(2*IC-1)
        NBIOAV(2) = NBACCE(2*IC  )
        DO 205 JJ = 1,NREUTI(IC)
          J = INDIR(JINDIR(IC)+JJ)
          IADMI = IADM(JIADM(IC)+2*J-1)
          IF ( IADMI .EQ. 0 ) GOTO 205
          IADYN = IADM(JIADM(IC)+2*J  )
          CGENR = GENR(JGENR(IC)+J)
          NOM32 = RNOM(JRNOM(IC)+J)
C
C    ISD DESIGNE LE STATUT DE LA COLLECTION
C        =U ON PASSE PAR LES ROUTINES HABITUELLES (JJALLC, JJLIDE)
C        =X ON TRAITE DIRECTEMENT
C
          ISDC  = ISZON(JISZON + IADMI - 1) / ISSTAT
          IF (CGENR .EQ. 'X' .AND. ISDC .EQ. 2) THEN
            IBACOL = IADMI
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
                      IF ( IMODE .EQ. 2 .OR. IMODE .EQ. 3 ) THEN
C
C     ON NE TRAITE PAS LE SEGMENT DE VALEURS MARQUE X D
C
                        GOTO 210
                      ENDIF
C
C     LE SEGMENT DE VALEURS EST MARQUE X D, IL FAUT D'ABORD L'ECRIRE
C
                      IADDI(1) = ISZON(JISZON + IBIADD -1 + 2*K-1)
                      IADDI(2) = ISZON(JISZON + IBIADD -1 + 2*K  )
                      CALL JXECRO ( IC, IADMOC, IADDI, LSV, J, K)
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
                    IF ( LMIN .GT. 0 ) THEN
                      IF ( LTOT .GE. LMIN ) THEN
                        LGIO(1) = LGIO(1)+1024*LONGBL(IC)*LOIS*
     &                           (NBACCE(2*IC-1)-NBIOAV(1))
                        LGIO(2) = LGIO(2)+1024*LONGBL(IC)*LOIS*
     &                           (NBACCE(2*IC  )-NBIOAV(2))
                        GOTO 300
                      ENDIF
                    ENDIF
                  ENDIF
                ENDIF
 210          CONTINUE
            ENDIF
            GOTO 205
C          ELSE IF ( NOM32(25:32) .EQ. ' ' ) THEN
          ELSE
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
                  IF ( IMODE .EQ. 2 .OR. IMODE .EQ. 3 ) THEN
C
C     ON NE TRAITE PAS LE SEGMENT DE VALEURS MARQUE X D
C
                    GOTO 205
                  ENDIF
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
                IF ( LMIN .GT. 0 ) THEN
                  IF ( LTOT .GE. LMIN ) THEN
                        LGIO(1) = LGIO(1)+1024*LONGBL(IC)*LOIS*
     &                           (NBACCE(2*IC-1)-NBIOAV(1))
                        LGIO(2) = LGIO(2)+1024*LONGBL(IC)*LOIS*
     &                           (NBACCE(2*IC  )-NBIOAV(2))
                    GOTO 300
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDIF
 205    CONTINUE
C
        LGIO(1)=LGIO(1)+1024*LONGBL(IC)*LOIS*(NBACCE(2*IC-1)-NBIOAV(1))
        LGIO(2)=LGIO(2)+1024*LONGBL(IC)*LOIS*(NBACCE(2*IC  )-NBIOAV(2))
 200  CONTINUE
 300  CONTINUE
      MXLTOT=MXLTOT+(LTOT*LOIS)/(1024*1024)
C
      CALL UTTCPU('CPU.MEMD.1','FIN',' ')
      END
