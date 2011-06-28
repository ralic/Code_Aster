      SUBROUTINE JJAREP ( ICLAS , NRMAX )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C RESPONSABLE LEFEBVRE J-P.LEFEBVRE
C MODIF JEVEUX  DATE 27/06/2011   AUTEUR LEFEBVRE J-P.LEFEBVRE 
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
C TOLE CRP_18 CRS_508 CRS_505
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER             ICLAS , NRMAX
C ----------------------------------------------------------------------
C     PERMET D'AGRANDIR UN REPERTOIRE DE NOM
C
C     IN    ICLAS  : CLASSE ASSOCIEE AU REPERTOIRE
C     IN    NRMAX  : DIMENSION DU NOUVEAU REPERTOIRE
C
C ----------------------------------------------------------------------
      INTEGER          IGENR,ITYPE,IDOCU,IORIG,IRNOM(4)
      EQUIVALENCE      (IGENR,GENR),(ITYPE,TYPE),
     &                 (IDOCU,DOCU),(IORIG,ORIG),(IRNOM,RNOM)
      CHARACTER*1      K1ZON
      COMMON /KZONJE/  K1ZON(8)
      INTEGER          LK1ZON , JK1ZON , LISZON , JISZON , ISZON(1)
      COMMON /IZONJE/  LK1ZON , JK1ZON , LISZON , JISZON
      EQUIVALENCE    ( ISZON(1) , K1ZON(1) )
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
C ----------------------------------------------------------------------
      INTEGER          NBLMAX    , NBLUTI    , LONGBL    ,
     &                 KITLEC    , KITECR    ,             KIADM    ,
     &                 IITLEC    , IITECR    , NITECR    , KMARQ
      COMMON /IFICJE/  NBLMAX(N) , NBLUTI(N) , LONGBL(N) ,
     &                 KITLEC(N) , KITECR(N) ,             KIADM(N) ,
     &                 IITLEC(N) , IITECR(N) , NITECR(N) , KMARQ(N)
      CHARACTER*8      NOMBAS
      COMMON /KBASJE/  NOMBAS(N)
      COMMON /KINDIR/  INDIR(1)
      COMMON /JINDIR/  JINDIR(N)
C
      INTEGER          NRHCOD    , NREMAX    , NREUTI
      COMMON /ICODJE/  NRHCOD(N) , NREMAX(N) , NREUTI(N)
      INTEGER          LBIS , LOIS , LOLS , LOR8 , LOC8
      COMMON /IENVJE/  LBIS , LOIS , LOLS , LOR8 , LOC8
      INTEGER          IPGC,KDESMA(2),LGD,LGDUTI,KPOSMA(2),LGP,LGPUTI
      COMMON /IADMJE/  IPGC,KDESMA,   LGD,LGDUTI,KPOSMA,   LGP,LGPUTI
      INTEGER          LDYN , LGDYN , NBDYN , NBFREE
      COMMON /IDYNJE/  LDYN , LGDYN , NBDYN , NBFREE
C ----------------------------------------------------------------------
      CHARACTER *32    CLEL,CLE
      REAL *8          RBID
      CHARACTER *4     Z
      INTEGER          JCOD,KADHC,JNOM,KADNO,LOREP,IADRS(20),KAT(20)
      INTEGER          LGNOM,NUTI,LSO(20),IMQ(2),IADDI(2),KDY(20)
      PARAMETER       (NBATT=12,NBTOT=NBATT+3,LGNOM=32)
      INTEGER          NUMATT(NBTOT),IDM(NBTOT),IDY(NBTOT),IRT
      DATA NUMATT,Z   /2,3,4,5,6,8,9,10,11,12,16,7,13,20,17,'INIT'/
C DEB ------------------------------------------------------------------
      IPGCA = IPGC
      IPGC  = -2
      IRT = 0
      IC = ICLAS
      CALL JJLDYN (0,-1,LTOT)
C
C --- ON INTERDIT L'APPEL A JJLDYN AVEC LE PARAMETRE MODE=1 LORS DE
C --- L'ALLOCATION DYNAMIQUE  (ET LES APPELS RECURSIFS)
C
      LDYNOL = LDYN
      IF ( LDYN .EQ. 1 ) THEN
        LDYN   = 2
      ENDIF
C
C --- ALLOCATION DU SEGMENT DE VALEURS POUR LE NOUVEAU REPERTOIRE
C
      LHCOD  = JJPREM ( NRMAX , IRT )
      IF ( IRT .EQ. 1 ) THEN
        IF ( IC .EQ. 1 ) THEN
          CALL U2MESG('A','JEVEUX_64',1,NOMBAS(IC),1,NRMAX,0,RBID)
        ELSE
          CALL U2MESG('A','JEVEUX_65',1,NOMBAS(IC),1,NRMAX,0,RBID)
        ENDIF
      ENDIF
      CALL JJALLS (LHCOD*LOIS,IC,'V','I',LOIS,'INIT',HCOD,JCOD,KADHC,
     &             KHCDY)
      CALL JJECRS (KADHC,KHCDY,IC,13,0,'E',IMQ)
      CALL JJALLS (NRMAX*LGNOM,IC,'V','K',LGNOM,'NOINIT',IRNOM,
     &             JNOM,KADNO,KNODY)
      CALL JJECRS (KADNO,KNODY,IC,7,0,'E',IMQ)
      DO 60 I = 1,NRMAX
        RNOM( JNOM - 1 + I ) = '?'
 60   CONTINUE
      NUTI = 0
C
C --- REMPLISSAGE DU REPERTOIRE DE NOM
C
      LOREP = LHCOD
      DO 100 KN=1,NREMAX(IC)
        CLEL = RNOM(JRNOM(IC)+KN)
        IF ( CLEL(1:1) .EQ. '?' ) THEN
          IDATIN  = NUTI + 1
          GOTO 101
        ENDIF
        NE = 1
        IREF = JXHCOD (CLEL,LOREP)
        I  = IREF
 5      CONTINUE
        IF ( HCOD(JCOD-1+I) .EQ. 0 ) THEN
          IF ( NUTI .GE. NRMAX ) THEN
            CALL U2MESS('F','JEVEUX_58')
          ELSE
            IDATIN = NUTI + 1
            IIN    = I
          ENDIF
        ELSE
          J = HCOD(JCOD-1+I)
          CLE  = RNOM(JNOM-1+ABS(J))
          IF ( CLE .EQ. CLEL ) THEN
            CALL U2MESK('F','JEVEUX_59',1,CLEL)
          ELSE
            IF ( NE .EQ. 1 ) IN = JXHCOD (CLEL,LOREP-2)
            NE = NE + 1
            I = 1 + MOD (I+IN,LOREP)
            IF ( NE .LE. LOREP ) THEN
              GOTO 5
            ELSE
              CALL U2MESS('F','JEVEUX_58')
            END IF
          END IF
        END IF
        HCOD(JCOD-1+IIN)   = IDATIN
 101    CONTINUE
        RNOM(JNOM-1+IDATIN) = RNOM(JRNOM(IC)+KN)
        NUTI = NUTI + 1
 100  CONTINUE
C
C --- RECOPIE DES OBJETS SYSTEME APRES AGRANDISSEMENT
C
      NREMAX(IC) = NRMAX
      NRHCOD(IC) = LHCOD
      CARA(JCARA(IC)    ) = NREMAX(IC)
      CARA(JCARA(IC) +2 ) = NRHCOD(IC)
C
      DO 200 K=1,NBTOT
        KL = NUMATT(K)
        LONOI = LONO(JLONO(IC)+KL)*LTYP(JLTYP(IC)+KL)
        IF ( IADD(JIADD(IC)+2*KL-1) .GT. 0 ) THEN
          CALL JXLIBD ( 0 , KL , IC ,IADD(JIADD(IC)+2*KL-1) , LONOI )
          IADD(JIADD(IC)+2*KL-1) = 0
          IADD(JIADD(IC)+2*KL  ) = 0
        ENDIF
 200  CONTINUE
C
      KAT (7)            = KADNO
      KDY (7)            = KNODY
      LONG(JLONG(IC)+7)  = NRMAX
      LSO (7)            = LHCOD*LOIS
      KAT (13)           = KADHC
      KDY (13)           = KHCDY
      LONG(JLONG(IC)+13) = LHCOD
      LSO (13)           = NRMAX*LGNOM
C
C --- ALLOCATION DES DIFFERENTS SEGMENTS DE VALEURS :
C
      LON = 2*NREMAX(IC)*LOIS
      CALL JJALLS (LON,IC,'V','I',LOIS,Z,IMARQ,IADRS(16),KAT(16),
     &             KDY(16))
      CALL JJECRS (KAT(16),KDY(16),IC,16,0,'E',IMQ)
      LON = NREMAX(IC) * LEN(GENR(1))
      CALL JJALLS (LON,IC,'V','K',LEN(GENR(1)),Z,IGENR,IADRS(3),KAT(3),
     &             KDY(3))
      CALL JJECRS (KAT(3),KDY(3),IC, 3,0,'E',IMQ)
      LON = NREMAX(IC) * LEN(TYPE(1))
      CALL JJALLS (LON,IC,'V','K',LEN(TYPE(1)),Z,ITYPE,IADRS(4),KAT(4),
     &             KDY(4))
      CALL JJECRS (KAT(4),KDY(4),IC, 4,0,'E',IMQ)
      LON = NREMAX(IC) * LEN(DOCU(1))
      CALL JJALLS (LON,IC,'V','K',LEN(DOCU(1)),Z,IDOCU,IADRS(5),KAT(5),
     &             KDY(5))
      CALL JJECRS (KAT(5),KDY(5),IC, 5,0,'E',IMQ)
      LON = NREMAX(IC) * LEN(ORIG(1))
      CALL JJALLS (LON,IC,'V','K',LEN(ORIG(1)),Z,IORIG,IADRS(6),KAT(6),
     &             KDY(6))
      CALL JJECRS (KAT(6),KDY(6),IC, 6,0,'E',IMQ)
      LON = NREMAX(IC) * LOIS
      CALL JJALLS (2*LON,IC,'V','I',LOIS,Z,IADD,IADRS(2),KAT(2),KDY(2))
      CALL JJECRS (KAT(2),KDY(2),IC, 2,0,'E',IMQ)
      CALL JJALLS ( LON,IC,'V','I',LOIS,Z,LTYP,IADRS(8) ,KAT(8),KDY(8))
      CALL JJECRS (KAT(8),KDY(8),IC, 8,0,'E',IMQ)
      CALL JJALLS ( LON,IC,'V','I',LOIS,Z,LONG,IADRS(9) ,KAT(9),KDY(9))
      CALL JJECRS (KAT(9),KDY(9),IC, 9,0,'E',IMQ)
      CALL JJALLS (LON,IC,'V','I',LOIS,Z,LONO,IADRS(10),KAT(10),KDY(10))
      CALL JJECRS (KAT(10),KDY(10),IC,10,0,'E',IMQ)
      CALL JJALLS (LON,IC,'V','I',LOIS,Z,DATE,IADRS(11),KAT(11),KDY(11))
      CALL JJECRS (KAT(11),KDY(11),IC,11,0,'E',IMQ)
      CALL JJALLS (LON,IC,'V','I',LOIS,Z,LUTI,IADRS(12),KAT(12),KDY(12))
      CALL JJECRS (KAT(12),KDY(12),IC,12,0,'E',IMQ)
      CALL JJALLS (2*LON,IC,'V','I',LOIS,Z,IADM,IADRS(20),KAT(20),
     &             KDY(20))
      CALL JJECRS (KAT(20),KAT(20),IC,20,0,'E',IMQ)
      CALL JJALLS (LON,IC,'V','I',LOIS,Z,INDIR,IADRS(17),KAT(17),
     &             KDY(17))
      CALL JJECRS (KAT(17),KAT(17),IC,17,0,'E',IMQ)
      DO 300 K=1,NREUTI(IC)
        GENR(IADRS( 3)-1+K) =  GENR(JGENR(IC)+K)
        TYPE(IADRS( 4)-1+K) =  TYPE(JTYPE(IC)+K)
        DOCU(IADRS( 5)-1+K) =  DOCU(JDOCU(IC)+K)
        ORIG(IADRS( 6)-1+K) =  ORIG(JORIG(IC)+K)
        LTYP(IADRS( 8)-1+K) =  LTYP(JLTYP(IC)+K)
        LONG(IADRS( 9)-1+K) =  LONG(JLONG(IC)+K)
        LONO(IADRS(10)-1+K) =  LONO(JLONO(IC)+K)
        DATE(IADRS(11)-1+K) =  DATE(JDATE(IC)+K)
        LUTI(IADRS(12)-1+K) =  LUTI(JLUTI(IC)+K)
 300  CONTINUE
      DO 310 K=1,2*NREUTI(IC)
        IADM (IADRS(20)-1+K) =  IADM (JIADM(IC)+K)
        IADD (IADRS( 2)-1+K) =  IADD (JIADD(IC)+K)
        IMARQ(IADRS(16)-1+K) =  IMARQ(JMARQ(IC)+K)
 310  CONTINUE
      DO 315 I = 1,NBTOT
        IL     = NUMATT(I)
        IDM(I) = IADM(JIADM(IC)+2*IL-1)
        IDY(I) = IADM(JIADM(IC)+2*IL  )
 315  CONTINUE
      DO 320 I = 1,NBTOT
        IF (IDY(I) .NE. 0) THEN
          CALL JJLIDY ( IDY(I) , IDM(I) )
        ELSE IF (IDM(I) .NE. 0) THEN
          CALL JJLIBP (IDM(I))
        ENDIF
 320  CONTINUE
C
      JIADD(IC) = IADRS( 2) - 1
      JGENR(IC) = IADRS( 3) - 1
      JTYPE(IC) = IADRS( 4) - 1
      JDOCU(IC) = IADRS( 5) - 1
      JORIG(IC) = IADRS( 6) - 1
      JLTYP(IC) = IADRS( 8) - 1
      JLONG(IC) = IADRS( 9) - 1
      JLONO(IC) = IADRS(10) - 1
      JDATE(IC) = IADRS(11) - 1
      JLUTI(IC) = IADRS(12) - 1
      JMARQ(IC) = IADRS(16) - 1
      KMARQ(IC) = KAT(16)
      JIADM(IC) = IADRS(20) - 1
      KIADM(IC) = KAT(20)
      JHCOD(IC) = JCOD - 1
      JRNOM(IC) = JNOM - 1
      JINDIR(IC)= IADRS(17) - 1
C
      DO 325 I = 1,NBTOT
        IL = NUMATT(I)
        IF (IL .EQ. 13 ) THEN
          LONG(JLONG(IC)+IL) = LHCOD
          LONO(JLONO(IC)+IL) = LHCOD
        ELSE IF (IL .EQ. 16 .OR. IL .EQ. 2 .OR. IL .EQ. 20) THEN
          LONG(JLONG(IC)+IL) = 2*NRMAX
          LONO(JLONO(IC)+IL) = 2*NRMAX
        ELSE
          LONG(JLONG(IC)+IL) = NRMAX
          LONO(JLONO(IC)+IL) = NRMAX
        ENDIF
        LLOC = LONO(JLONO(IC)+IL) * LTYP(JLTYP(IC)+IL)
        IF ( MOD(LLOC,LOIS) .NE. 0 ) THEN
          LONO(JLONO(IC)+IL) = ((1+LLOC/LOIS)*LOIS)/LTYP(JLTYP(IC)+IL)
        ENDIF
        LSO(IL) = LONO(JLONO(IC)+IL) * LTYP(JLTYP(IC)+IL)
        IADM(JIADM(IC)+2*IL-1) = KAT(IL)
        IADM(JIADM(IC)+2*IL  ) = KDY(IL)
 325  CONTINUE
C
      DO 330 I = 1,NBTOT
        IL = NUMATT(I)
        IADDI(1) = 0
        IADDI(2) = 0
        CALL JXECRO ( IC , KAT(IL) , IADDI , LSO(IL) , 0 , IL )
        IADD (JIADD(IC)+2*IL-1) = IADDI(1)
        IADD (JIADD(IC)+2*IL  ) = IADDI(2)
 330  CONTINUE
      CARA(JCARA(IC)+6) = IADD(JIADD(IC) + 2*2-1  )
      CARA(JCARA(IC)+7) = IADD(JIADD(IC) + 2*2    )
      DO 345 IND = 1 , NREMAX(IC)
        INDIR(JINDIR(IC)+IND) = IND
 345  CONTINUE
      LDYN = LDYNOL
      IPGC = IPGCA
C FIN ------------------------------------------------------------------
      END
