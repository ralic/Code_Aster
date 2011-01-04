      SUBROUTINE JELIBD ( NOMLU , LTOT )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 03/01/2011   AUTEUR LEFEBVRE J-P.LEFEBVRE 
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
C ----------------------------------------------------------------------
C 
C LIBERE DE LA MEMOIRE LE SEGMENT DE VALEURS ASSOCIES A UN OBJET 
C JEVEUX ALLOUE DYNAMIQUEMENT LORSQU'IL EST DANS L'ETAT XA OU XD
C 
C IN   NOMLU : NOM DE L'OBJET A LIBERER
C OUT  LTOT  : LONGUEUR EN ENTIERS LIBEREE
C
C ----------------------------------------------------------------------
C RESPONSABLE LEFEBVRE J-P.LEFEBVRE
C TOLE CRP_18 CRS_508 CRS_512 
C
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)    NOMLU
C
      CHARACTER*1      K1ZON
      COMMON /KZONJE/  K1ZON(8)
      INTEGER          LK1ZON , JK1ZON , LISZON , JISZON , ISZON(1)
      COMMON /IZONJE/  LK1ZON , JK1ZON , LISZON , JISZON
      EQUIVALENCE    ( ISZON(1) , K1ZON(1) )
C     -----------------------------------------------------------------
      INTEGER          ICLAS ,ICLAOS , ICLACO , IDATOS , IDATCO , IDATOC
      COMMON /IATCJE/  ICLAS ,ICLAOS , ICLACO , IDATOS , IDATCO , IDATOC
C     ------------------------------------------------------------------
      PARAMETER  ( N = 5 )
      INTEGER          LTYP    , LONG    , DATE    , IADD    , IADM    ,
     +                 LONO    , HCOD    , CARA    , LUTI    , IMARQ
      COMMON /IATRJE/  LTYP(1) , LONG(1) , DATE(1) , IADD(1) , IADM(1) ,
     +                 LONO(1) , HCOD(1) , CARA(1) , LUTI(1) , IMARQ(1)
      COMMON /JIATJE/  JLTYP(N), JLONG(N), JDATE(N), JIADD(N), JIADM(N),
     +                 JLONO(N), JHCOD(N), JCARA(N), JLUTI(N), JMARQ(N)
C
      CHARACTER*1      GENR    , TYPE
      CHARACTER*4      DOCU
      CHARACTER*8      ORIG
      CHARACTER*32     RNOM
      COMMON /KATRJE/  GENR(8) , TYPE(8) , DOCU(2) , ORIG(1) , RNOM(1)
      COMMON /JKATJE/  JGENR(N), JTYPE(N), JDOCU(N), JORIG(N), JRNOM(N)
C
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
      INTEGER          LBIS , LOIS , LOLS , LOUA , LOR8 , LOC8
      COMMON /IENVJE/  LBIS , LOIS , LOLS , LOUA , LOR8 , LOC8
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
      CHARACTER*32   NOML32
      CHARACTER*8    NOML8
      INTEGER        ICRE,IRET
      INTEGER        IADDI(2),ICOUNT,LMEMT,LGS,KI
C
      NOML32 = NOMLU
      NOML8  = NOML32(25:32)
C
      ICRE = 0
      CALL JJVERN ( NOML32 , ICRE , IRET )
C
      IF ( IRET .EQ. 0 ) THEN
        CALL U2MESK('A','JEVEUX_26',1,NOML32(1:24))
        GOTO 9999
      ELSE IF ( IRET .EQ. 1 ) THEN
C
C ----  CAS D'UN OBJET SIMPLE
C
        INAT = 1
        IADMI  = IADM (JIADM(ICLAOS)+2*IDATOS-1)
        LTYPI  = LTYP (JLTYP(ICLAOS)+IDATOS)
        LONOI  = LONO (JLONO(ICLAOS)+IDATOS) * LTYPI
        IF ( IADMI .EQ. 0 ) THEN
          GOTO 9999
        ENDIF
        IADYN = IADM(JIADM(ICLAOS)+2*IDATOS)
C	  
        CALL JJLBSG (ICLAOS, IDATOS, 0, 0, IADMI, IADYN, LTOT) 
C	  
      ELSE IF ( IRET .EQ. 2 ) THEN
C
C ----- CAS D'UNE COLLECTION
C
        CALL JJALLC ( ICLACO , IDATCO , 'L' , IBACOL )
        IF ( NOML32(25:32) .EQ. '        ') THEN
          INAT = 2
        ELSE
          CALL JJCROC ( NOML32(25:32) , ICRE )
          INAT = 3
        ENDIF
      ENDIF
      IF ( INAT .EQ. 2 ) THEN
C
C ----- CAS D'UNE COLLECTION ENTIERE : ON LIBERE TOUS LES OC
C
        IXIADD = ISZON ( JISZON + IBACOL + IDIADD )
        IXIADM = ISZON ( JISZON + IBACOL + IDIADM )
        IXDESO = ISZON ( JISZON + IBACOL + IDDESO )
        LTYPI  = LTYP( JLTYP(ICLACO) + IXDESO )
        IF ( IXIADD .EQ. 0 ) THEN
C
C ------- COLLECTION CONTIGUE
C
          IADMI  = IADM ( JIADM(ICLACO) + 2*IXDESO-1 )
          IADDI(1) = IADD ( JIADD(ICLACO) + 2*IXDESO-1 )
          IADDI(2) = IADD ( JIADD(ICLACO) + 2*IXDESO   )
          IF ( IADMI .EQ. 0 ) THEN
            GOTO 9999
          ENDIF
          IADYN = IADM ( JIADM(ICLACO) + 2*IXDESO   )
C	  
          CALL JJLBSG (ICLACO, IXDESO, 0, 0, IADMI, IADYN, LTOT) 
C	  
        ELSE
C
C ------- COLLECTION DISPERSEE
C
          NBMAX  = ISZON ( JISZON + IBACOL + IVNMAX )
          IBIADM = IADM ( JIADM(ICLACO) + 2*IXIADM-1 )
          IBIADD = IADM ( JIADM(ICLACO) + 2*IXIADD-1 )
          IDECI  = 0
          DO 10 K = 1,NBMAX
            IADMI = ISZON(JISZON + IBIADM - 1 + 2*K-1 )
            IF ( IADMI .EQ. 0 ) THEN
              GOTO 10
            ENDIF
            IADYN  = ISZON(JISZON + IBIADM - 1 + 2*K   )
C	  
            CALL JJLBSG (ICLACO, IDATCO, K, IBACOL, IADMI, IADYN, LTOT) 
C	  
 10       CONTINUE
        ENDIF
      ELSE IF ( INAT .EQ. 3 ) THEN
C       ------ CAS D'UN OBJET DE COLLECTION  ------
        IXIADD = ISZON ( JISZON + IBACOL + IDIADD )
        IXIADM = ISZON ( JISZON + IBACOL + IDIADM )
        IXDESO = ISZON ( JISZON + IBACOL + IDDESO )
        LTYPI = LTYP( JLTYP(ICLACO) + IXDESO )
        IF ( IXIADD .NE. 0 ) THEN
C
C -------- COLLECTION DISPERSEE
C
          IBIADM = IADM ( JIADM(ICLACO) + 2*IXIADM-1 )
          IBIADD = IADM ( JIADM(ICLACO) + 2*IXIADD-1 )
          IADMI  = ISZON(JISZON + IBIADM - 1 + 2*IDATOC-1 )
          IADMEX = IADMI
          IF ( IADMI .EQ. 0 ) THEN
            GOTO 9999
          ENDIF
          IADYN  = ISZON(JISZON + IBIADM - 1 + 2*IDATOC)
C	 
          CALL JJLBSG (ICLACO,IDATCO,IDATOC,IBACOL,IADMI,IADYN,LTOT) 
C	 
        ENDIF
      ENDIF
 9999 CONTINUE
C
      END
