      SUBROUTINE JJAREP ( ICLAS , NRMAX )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 10/03/98   AUTEUR VABHHTS J.PELLET 
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
C TOLE CFT_720 CRP_18 CRS_508
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
C ----------------------------------------------------------------------
      INTEGER          NBLMAX    , NBLUTI    , LONGBL    ,
     +                 KITLEC    , KITECR    , KINDEF    , KIADM    ,
     +                 IITLEC    , IITECR    , NITECR    , KMARQ
      COMMON /IFICJE/  NBLMAX(N) , NBLUTI(N) , LONGBL(N) ,
     +                 KITLEC(N) , KITECR(N) , KINDEF(N) , KIADM(N) ,
     +                 IITLEC(N) , IITECR(N) , NITECR(N) , KMARQ(N)
C
      INTEGER          NRHCOD    , NREMAX    , NREUTI
      COMMON /ICODJE/  NRHCOD(N) , NREMAX(N) , NREUTI(N)
      INTEGER          LBIS , LOIS , LOLS , LOUA , LOR8 , LOC8
      COMMON /IENVJE/  LBIS , LOIS , LOLS , LOUA , LOR8 , LOC8
      INTEGER          IPGC, KDESMA, LGD, LGDUTI, KPOSMA, LGP, LGPUTI
      COMMON /IADMJE/  IPGC, KDESMA, LGD, LGDUTI, KPOSMA, LGP, LGPUTI
C ----------------------------------------------------------------------
      CHARACTER *32    CLEL,CLE
      CHARACTER *75    CMESS
      CHARACTER *4     Z
      INTEGER          JCOD,KADHC,JNOM,KADNO,LOREP,IADRS(20),KAT(20)
      INTEGER          LGNOM,NUTI,LSO(20),IMQ(2),IADDI(2)
      PARAMETER       (NBATT=12,NBTOT=NBATT+2,LGNOM=32)
      INTEGER          NUMATT(NBTOT)
      DATA NUMATT,Z   /2,3,4,5,6,8,9,10,11,12,16,7,13,20,'INIT'/
C DEB ------------------------------------------------------------------
      IPGCA = IPGC
      IPGC  = -2
      IC = ICLAS
C
C --- ALLOCATION DU SEGMENT DE VALEURS POUR LE NOUVEAU REPERTOIRE
C
      LHCOD  = JJPREM ( NRMAX )
      CALL JJALLS (LHCOD*LOIS ,'V','I',LOIS,'INIT',HCOD,JCOD,KADHC)
      CALL JJECRS (KADHC,IC,13,0,'E',IMQ)
      CALL JJALLS (NRMAX*LGNOM,'V','K',LGNOM,'NOINIT',IRNOM,JNOM,KADNO)
      CALL JJECRS (KADNO,IC,7,0,'E',IMQ)
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
            CMESS = ' LE NOUVEAU REPERTOIRE EST SATURE'
            CALL JVMESS ( 'F' , 'JJAREP01' , CMESS )
          ELSE
            IDATIN = NUTI + 1
            IIN    = I
          ENDIF
        ELSE
          J = HCOD(JCOD-1+I)
          CLE  = RNOM(JNOM-1+ABS(J))
          IF ( CLE .EQ. CLEL ) THEN
            CMESS = ' LE NOM DEMANDE EXISTE DEJA DANS LE REPERTOIRE '
            CALL JVMESS ( 'S' , 'JJAREP02' , CMESS )
          ELSE
            IF ( NE .EQ. 1 ) IN = JXHCOD (CLEL,LOREP-2)
            NE = NE + 1
            I = 1 + MOD (I+IN,LOREP)
            IF ( NE .LE. LOREP ) THEN
              GOTO 5
            ELSE
              CMESS = ' LE REPERTOIRE EST SATURE'
              CALL JVMESS ( 'F' , 'JJAREP03' , CMESS )
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
      LONG(JLONG(IC)+7)  = NRMAX
      LSO (7)            = LHCOD*LOIS
      KAT (13)           = KADHC
      LONG(JLONG(IC)+13) = LHCOD
      LSO (13)           = NRMAX*LGNOM
C
C --- ALLOCATION DES DIFFERENTS SEGMENTS DE VALEURS :
C
      LON = 2*NREMAX(IC)*LOIS
      CALL JJALLS ( LON,'V','I',LOIS,Z,IMARQ,IADRS(16),KAT(16))
      CALL JJECRS (KAT(16),IC,16,0,'E',IMQ)
      LON = NREMAX(IC) * LEN(GENR(1))
      CALL JJALLS (LON,'V','K',LEN(GENR(1)),Z,IGENR,IADRS(3),KAT( 3))
      CALL JJECRS (KAT( 3),IC, 3,0,'E',IMQ)
      LON = NREMAX(IC) * LEN(TYPE(1))
      CALL JJALLS (LON,'V','K',LEN(TYPE(1)),Z,ITYPE,IADRS(4),KAT( 4))
      CALL JJECRS (KAT( 4),IC, 4,0,'E',IMQ)
      LON = NREMAX(IC) * LEN(DOCU(1))
      CALL JJALLS (LON,'V','K',LEN(DOCU(1)),Z,IDOCU,IADRS(5),KAT( 5))
      CALL JJECRS (KAT( 5),IC, 5,0,'E',IMQ)
      LON = NREMAX(IC) * LEN(ORIG(1))
      CALL JJALLS (LON,'V','K',LEN(ORIG(1)),Z,IORIG,IADRS(6),KAT( 6))
      CALL JJECRS (KAT( 6),IC, 6,0,'E',IMQ)
      LON = NREMAX(IC) * LOIS
      CALL JJALLS ( 2*LON,'V','I',LOIS,Z,IADD,IADRS(2) ,KAT( 2))
      CALL JJECRS (KAT( 2),IC, 2,0,'E',IMQ)
      CALL JJALLS ( LON,'V','I',LOIS,Z,LTYP,IADRS(8) ,KAT( 8))
      CALL JJECRS (KAT( 8),IC, 8,0,'E',IMQ)
      CALL JJALLS ( LON,'V','I',LOIS,Z,LONG,IADRS(9) ,KAT( 9))
      CALL JJECRS (KAT( 9),IC, 9,0,'E',IMQ)
      CALL JJALLS ( LON,'V','I',LOIS,Z,LONO,IADRS(10),KAT(10))
      CALL JJECRS (KAT(10),IC,10,0,'E',IMQ)
      CALL JJALLS ( LON,'V','I',LOIS,Z,DATE,IADRS(11),KAT(11))
      CALL JJECRS (KAT(11),IC,11,0,'E',IMQ)
      CALL JJALLS ( LON,'V','I',LOIS,Z,LUTI,IADRS(12),KAT(12))
      CALL JJECRS (KAT(12),IC,12,0,'E',IMQ)
      CALL JJALLS ( LON,'V','I',LOIS,Z,IADM,IADRS(20),KAT(20))
      CALL JJECRS (KAT(20),IC,20,0,'E',IMQ)
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
        IADM(IADRS(20)-1+K) =  IADM(JIADM(IC)+K)
 300  CONTINUE
      DO 310 K=1,2*NREUTI(IC)
        IADD (IADRS( 2)-1+K) =  IADD (JIADD(IC)+K)
        IMARQ(IADRS(16)-1+K) =  IMARQ(JMARQ(IC)+K)
 310  CONTINUE
      DO 320 I = 1,NBTOT
        IL = NUMATT(I)
        IADMI = IADM(JIADM(IC)+IL)
        IF (IADMI .GT. 0) CALL JJLIBP (IADMI)
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
C
      DO 325 I = 1,NBTOT
        IL = NUMATT(I)
        IF (IL .EQ. 13 ) THEN
          LONG(JLONG(IC)+IL) = LHCOD
          LONO(JLONO(IC)+IL) = LHCOD
        ELSE IF (IL .EQ. 16 .OR. IL .EQ. 2) THEN
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
        IADM(JIADM(IC)+IL) = KAT(IL)
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
      IPGC = IPGCA
C FIN ------------------------------------------------------------------
      END
