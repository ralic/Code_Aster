      SUBROUTINE JEDEBU(NBFI, MXZON, IDB)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 30/07/2012   AUTEUR LEFEBVRE J-P.LEFEBVRE 
C RESPONSABLE LEFEBVRE J-P.LEFEBVRE
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
C TOLE CRP_18 CRP_6 CRS_508 CRS_512
      IMPLICIT NONE
      INTEGER             NBFI, MXZON, IDB
C ----------------------------------------------------------------------
C ROUTINE UTILISATEUR D'INITIALISATION GENERALE POUR LE GESTIONNAIRE
C         DE MEMOIRE
C
C IN  NBFI   : NOMBRE MAXIMUM DE BASES SIMULTANEES ( =< 5)
C IN  MXZON  : LIMITE MEMOIRE DYNAMIQUE (EN ENTIER words)
C IN  IDB    : PARAMETRE DE DEBUG
C              ( 0: RIEN, 1: MISE A UNDEF DES SEGMENTS DE VALEURS )
C
C ----------------------------------------------------------------------
      CHARACTER*1      K1ZON
      COMMON /KZONJE/  K1ZON(8)
      INTEGER          LK1ZON , JK1ZON , LISZON , JISZON , ISZON(1)
      COMMON /IZONJE/  LK1ZON , JK1ZON , LISZON , JISZON
      EQUIVALENCE    ( ISZON(1) , K1ZON(1) )
C ----------------------------------------------------------------------
      INTEGER          NBFIC
      COMMON /IPARJE/  NBFIC
      INTEGER          ILOC
      COMMON /ILOCJE/  ILOC
C ----------------------------------------------------------------------
C-----------------------------------------------------------------------
      INTEGER I ,IACCE ,JCARA ,JDATE ,JDOCU ,JGENR ,JHCOD 
      INTEGER JIACCE ,JIADD ,JIADM ,JJPREM ,JLONG ,JLONO ,JLTYP 
      INTEGER JLUTI ,JMARQ ,JORIG ,JRNOM ,JTYPE ,K ,LOFIEM 
      INTEGER MOFIEM ,N ,NBACCE, IBID 
      REAL*8 VAL 
C-----------------------------------------------------------------------
      PARAMETER  ( N = 5 )
C
      INTEGER          LTYP    , LONG    , DATE    , IADD    , IADM    ,
     &                 LONO    , HCOD    , CARA    , LUTI    , IMARQ
      COMMON /IATRJE/  LTYP(1) , LONG(1) , DATE(1) , IADD(1) , IADM(1) ,
     &                 LONO(1) , HCOD(1) , CARA(1) , LUTI(1) , IMARQ(1)
      COMMON /JIATJE/  JLTYP(N), JLONG(N), JDATE(N), JIADD(N), JIADM(N),
     &                 JLONO(N), JHCOD(N), JCARA(N), JLUTI(N), JMARQ(N)
      CHARACTER*1      GENR    , TYPE
      CHARACTER*4      DOCU
      CHARACTER*8      ORIG
      CHARACTER*32     RNOM
      COMMON /KATRJE/  GENR(8) , TYPE(8) , DOCU(2) , ORIG(1) , RNOM(1)
      COMMON /JKATJE/  JGENR(N), JTYPE(N), JDOCU(N), JORIG(N), JRNOM(N)
C ----------------------------------------------------------------------
      INTEGER          NBLMAX    , NBLUTI    , LONGBL    ,
     &                 KITLEC    , KITECR    ,             KIADM    ,
     &                 IITLEC    , IITECR    , NITECR    , KMARQ
      COMMON /IFICJE/  NBLMAX(N) , NBLUTI(N) , LONGBL(N) ,
     &                 KITLEC(N) , KITECR(N) ,             KIADM(N) ,
     &                 IITLEC(N) , IITECR(N) , NITECR(N) , KMARQ(N)
      LOGICAL          LITLEC
      COMMON /LFICJE/  LITLEC(N)
      INTEGER          NRHCOD    , NREMAX    , NREUTI
      COMMON /ICODJE/  NRHCOD(N) , NREMAX(N) , NREUTI(N)
      CHARACTER*2      DN2
      CHARACTER*5      CLASSE
      CHARACTER*8                  NOMFIC    , KSTOUT    , KSTINI
      COMMON /KFICJE/  CLASSE    , NOMFIC(N) , KSTOUT(N) , KSTINI(N) ,
     &                 DN2(N)
C ----------------------------------------------------------------------
      CHARACTER *24                     NOMCO
      CHARACTER *32    NOMUTI , NOMOS ,         NOMOC , BL32
      COMMON /NOMCJE/  NOMUTI , NOMOS , NOMCO , NOMOC , BL32
C ----------------------------------------------------------------------
      INTEGER          ISSTAT
      COMMON /ICONJE/  ISSTAT
      INTEGER          MSSTAT, LSSTAT
      COMMON /JCONJE/  MSSTAT, LSSTAT
C ----------------------------------------------------------------------
      INTEGER          DATEI
      COMMON /IHEUJE/  DATEI
C ----------------------------------------------------------------------
      INTEGER          ILLICI , JCLASS(0:255)
      COMMON /JCHAJE/  ILLICI , JCLASS
C ----------------------------------------------------------------------
      INTEGER          ISTAT
      COMMON /ISTAJE/  ISTAT(4)
      CHARACTER*4      KSTAT
      COMMON /KSTAJE/  KSTAT
      INTEGER          MSLOIS
      COMMON /JENVJE/  MSLOIS
      INTEGER          LBIS , LOIS , LOLS , LOR8 , LOC8
      COMMON /IENVJE/  LBIS , LOIS , LOLS , LOR8 , LOC8
      INTEGER          IPGC,KDESMA(2),LGD,LGDUTI,KPOSMA(2),LGP,LGPUTI
      COMMON /IADMJE/  IPGC,KDESMA,   LGD,LGDUTI,KPOSMA,   LGP,LGPUTI
      INTEGER          IDN    , IEXT    , NBENRG
      COMMON /IEXTJE/  IDN(N) , IEXT(N) , NBENRG(N)
      INTEGER          LFIC,MFIC
      COMMON /FENVJE/  LFIC,MFIC
      CHARACTER*128    REPGLO,REPVOL
      COMMON /BANVJE/  REPGLO,REPVOL
      INTEGER          LREPGL,LREPVO
      COMMON /BALVJE/  LREPGL,LREPVO
      INTEGER          LUNDEF,IDEBUG
      COMMON /UNDFJE/  LUNDEF,IDEBUG
      INTEGER          LDYN , LGDYN , NBDYN , NBFREE
      COMMON /IDYNJE/  LDYN , LGDYN , NBDYN , NBFREE
      INTEGER          ICDYN , MXLTOT
      COMMON /XDYNJE/  ICDYN , MXLTOT
      REAL *8         MXDYN, MCDYN, MLDYN, VMXDYN, VMET, LGIO
      COMMON /R8DYJE/ MXDYN, MCDYN, MLDYN, VMXDYN, VMET, LGIO(2)
      REAL *8          SVUSE,SMXUSE
      COMMON /STATJE/  SVUSE,SMXUSE
      COMMON /IACCED/  IACCE(1)
      COMMON /JIACCE/  JIACCE(N),NBACCE(2*N)
C --------------------------------- ------------------------------------
      INTEGER          MXLICI , IPREM  , INIT,    IRET, IRET2
      INTEGER          ISPBEM , LBISEM , LOISEM , LOLSEM
      INTEGER          LOR8EM , LOC8EM , ISNNEM , IRT
      REAL*8           R8BID,RVAL(6),RV(2)
      CHARACTER*8      K8TAB(6)
      PARAMETER      ( MXLICI = 67 )
      CHARACTER *(MXLICI) CLICIT
      DATA CLICIT/' ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789.$&_abcdefghijkl
     &mnopqrstuvwxyz'/
C DEB ------------------------------------------------------------------
C
C -----------------  ENVIRONNEMENT MACHINE -----------------------------
      LFIC = LOFIEM()
      CALL GTOPTR('maxbase', VAL, IRET)
      IF (VAL .LE. 0 .OR. IRET .NE. 0) THEN
         MFIC = MOFIEM()
      ELSE
         MFIC = NINT(VAL)*1024
      ENDIF
      CALL GTOPTK('repglob', REPGLO, IRET)
      IF (IRET .NE. 0) THEN
        REPGLO='. '
        LREPGL=1
      ELSE
        LREPGL=INDEX(REPGLO,' ') - 1
        IF (LREPGL .GT. 119 ) THEN
          CALL U2MESG ( 'F' ,'JEVEUX1_69',1,REPGLO,1,LREPGL,0,R8BID)
        ENDIF
      ENDIF
      CALL GTOPTK('repvola', REPVOL, IRET)
      IF (IRET .NE. 0) THEN
        REPVOL='. '
        LREPVO=1
      ELSE
        LREPVO=INDEX(REPVOL,' ') - 1
        IF (LREPVO .GT. 119 ) THEN
          CALL U2MESG ( 'F' ,'JEVEUX1_70',1,REPVOL,1,LREPVO,0,R8BID)
        ENDIF
      ENDIF
      LBIS = LBISEM()
      LOR8 = LOR8EM()
      LOC8 = LOC8EM()
      LOIS = LOISEM()
      LOLS = LOLSEM()
      LUNDEF = ISNNEM()
      MSLOIS = LOIS - 1
      LDYN   = 1
      LGDYN  = 1
      MXDYN  = 0
      LGIO(1) = 0
      LGIO(2) = 0
      MCDYN  = 0
      MLDYN  = 0
      NBDYN  = 0
      NBFREE = 0
      ICDYN  = 0
      MXLTOT = 0
      SVUSE  = 16
      SMXUSE = SVUSE
C -----------------  NOMBRE DE BASES -----------------------------------
      NBFIC = MIN ( NBFI , N , LEN(CLASSE) )
      CALL ASSERT ( NBFIC .GT. 0 .AND. NBFIC .EQ. NBFI)
C -----------------  CONSTANTES DE STATUT DES SEGMENTS DE VALEURS ------
      KSTAT  = 'XUAD'
      ISSTAT = ISPBEM( LBIS - 3 )
      DO 2 K = 1 , 4
        ISTAT(K) = K * ISSTAT
 2    CONTINUE
      IDEBUG = IDB
C -----------------  ZONE MEMOIRE  -------------------------------------
      VMXDYN = MXZON*LOIS
      IF ( MXZON .EQ. 0 ) THEN
        VMXDYN = 1024
      ENDIF
      VMET = VMXDYN
C
      CALL UTPTME(1,'MEM_MUMP',0.D0,IRET)
      CALL UTGTME(1,'VMPEAK  ',RVAL,IRET)
      IF ( RVAL(1) .LE. 0 ) THEN
        CALL U2MESG('I','JEVEUX1_75',0,' ',0,IBID,0,RVAL)  
      ENDIF
      K8TAB(1) = 'LIMIT_JV'
      K8TAB(2) = 'MEM_TOTA'   
      K8TAB(3) = 'VMSIZE'
      K8TAB(4) = 'CMAX_JV'  
      K8TAB(5) = 'COUR_JV'  
      K8TAB(6) = 'MEM_MUMP'  
      CALL UTGTME(6,K8TAB,RVAL,IRET)
C  
      IF ( RVAL(3) .GT. 0 ) THEN
C     
        CALL UTPTME(1,'RLQ_MEM ',RVAL(3),IRET)
        IF ( RVAL(1)-RVAL(3) .LE. 0 ) THEN
          CALL U2MESG('F' ,'JEVEUX1_71',0,' ',0,IBID,3,RVAL)
        ENDIF 
        CALL JERMXD((RVAL(1)-RVAL(3))*1024*1024,IRET)
        IF ( IRET.EQ.0 ) THEN
          K8TAB(5) = 'RLQ_MEM'       
          K8TAB(6) = 'COUR_JV'  
          CALL UTGTME(6,K8TAB,RVAL,IRET2)
          CALL U2MESG('I' ,'JEVEUX1_74',0,' ',0,IBID,5,RVAL) 
        ENDIF 
      ENDIF 
C 
      LISZON = 1
      JISZON = 1
      LK1ZON = LISZON * LOIS
      JK1ZON = JISZON * LOIS
      ILOC = LOC ( ISZON(JISZON) )
C -------------------  POINTEURS D'ATTRIBUTS  --------------------------
      DO 5 I = 1 , LEN(CLASSE)
         CLASSE(I:I) = '$'
   5  CONTINUE
      DO 10 I = 1 , NBFIC
        JGENR(I)  = 0
        JTYPE(I)  = 0
        JLTYP(I)  = 0
        JDOCU(I)  = 0
        JORIG(I)  = 0
        JRNOM(I)  = 0
        JLONO(I)  = 0
        JLONG(I)  = 0
        JDATE(I)  = 0
        JIADD(I)  = 0
        JIADM(I)  = 0
        JMARQ(I)  = 0
        JLUTI(I)  = 0
        JCARA(I)  = 0
        JHCOD(I)  = 0
        NREMAX(I) = 0
        NRHCOD(I) = 0
        NREUTI(I) = 0
        NBLMAX(I) = 0
        NBENRG(I) = 1
        NBLUTI(I) = 0
        LONGBL(I) = 0
        KITLEC(I) = 0
        KITECR(I) = 0
        KIADM (I) = 0
        IITLEC(I) = 0
        IITECR(I) = 0
        NITECR(I) = 0
        LITLEC(I) = .FALSE.
        NOMFIC(I)   = '        '
        KSTOUT(I)   = '        '
        KSTINI(I)   = '        '
        CLASSE(I:I) = ' '
        DN2(I)      = ' '
        NBACCE(2*I-1) = 0
        NBACCE(2*I  ) = 0
   10 CONTINUE
C -------------------  CONSTANTES DE GESTION  --------------------------
      LSSTAT = LBIS-4
      MSSTAT = 0
      DO 20 K = 1,LBIS-4
         MSSTAT = MSSTAT + ISPBEM(K)
 20   CONTINUE
      BL32  = ' '
C
      CALL JXDATE ( DATEI )
C
      ILLICI = -1
      DO 30 K = 0 , 255
         JCLASS(K) = ILLICI
 30   CONTINUE
      DO 31 K = 1 , MXLICI
         JCLASS(ICHAR( CLICIT(K:K) ) ) = K
 31   CONTINUE
C
      KDESMA(1) = 0
      KDESMA(2) = 0
      LGDUTI = 0
      KPOSMA(1) = 0
      KPOSMA(2) = 0
      LGPUTI = 0
      IPGC   = 0
      INIT   = 100
      IRT    = 0
      IPREM  = JJPREM(INIT,IRT)
C FIN ------------------------------------------------------------------
      END
