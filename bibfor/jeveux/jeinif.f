      SUBROUTINE JEINIF ( STI, STO, NOMF, CLAS, NREP, NBLOC, LBLOC )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 27/10/2008   AUTEUR LEFEBVRE J-P.LEFEBVRE 
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
C TOLE CFT_720 CRP_18 CRP_20 CRS_508 
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER                                   NREP, NBLOC, LBLOC
      CHARACTER*(*)       STI, STO, NOMF, CLAS
C ----------------------------------------------------------------------
C ROUTINE UTILISATEUR D'OUVERTURE D'UNE CLASSE
C
C IN  STI    : STATUT EN DEBUT DE TRAVAIL ('DUMMY','DEBUT','POURSUIT')
C IN  STO    : STATUT EN FIN DE TRAVAIL ('SAUVE','DETRUIT')
C IN  NOMF   : NOM LOCALE DE LA BASE
C IN  CLAS   : NOM LOCALE DE LA CLASSE
C IN  NREP   : LONGUEUR DU REPERTOIRE
C IN  NBLOC  : NOMBRE D'ENREGISTREMMENTS DU FICHIER D'ACCES DIRECT
C              SI NBLOC = 0,  ON LE DETERMINE A PARTIR DE MFIC
C IN  LBLOC  : LONGUEUR DES ENREGISTREMMENTS DU FICHIER D'ACCES DIRECT
C              CETTE LONGUEUR EST DONNEE EN KILO (1024) MOT (ENTIER)
C
C ----------------------------------------------------------------------
      INTEGER          ICLAS ,ICLAOS , ICLACO , IDATOS , IDATCO , IDATOC
      COMMON /IATCJE/  ICLAS ,ICLAOS , ICLACO , IDATOS , IDATCO , IDATOC
C
      CHARACTER *24                     NOMCO
      CHARACTER *32    NOMUTI , NOMOS ,         NOMOC , BL32
      COMMON /NOMCJE/  NOMUTI , NOMOS , NOMCO , NOMOC , BL32
C ----------------------------------------------------------------------
      LOGICAL LEXP
C ----------------------------------------------------------------------
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
      COMMON /IACCED/  IACCE(1)
      COMMON /JIACCE/  JIACCE(N)
      COMMON /KUSADI/  IUSADI(1)
      COMMON /JUSADI/  JUSADI(N)
      COMMON /INBDET/  NBLIM(N),NBGROS(N),NBPETI(N)
C ----------------------------------------------------------------------
      INTEGER          NBLMAX    , NBLUTI    , LONGBL    ,
     &                 KITLEC    , KITECR    ,             KIADM    ,
     &                 IITLEC    , IITECR    , NITECR    , KMARQ
      COMMON /IFICJE/  NBLMAX(N) , NBLUTI(N) , LONGBL(N) ,
     &                 KITLEC(N) , KITECR(N) ,             KIADM(N) ,
     &                 IITLEC(N) , IITECR(N) , NITECR(N) , KMARQ(N)
C
      INTEGER          NRHCOD    , NREMAX    , NREUTI
      COMMON /ICODJE/  NRHCOD(N) , NREMAX(N) , NREUTI(N)
C
      CHARACTER*2      DN2
      CHARACTER*5      CLASSE
      CHARACTER*8                  NOMFIC    , KSTOUT    , KSTINI
      COMMON /KFICJE/  CLASSE    , NOMFIC(N) , KSTOUT(N) , KSTINI(N) ,
     &                 DN2(N)
      CHARACTER*8      NOMBAS
      COMMON /KBASJE/  NOMBAS(N)
      INTEGER          IDN    , IEXT    , NBENRG
      COMMON /IEXTJE/  IDN(N) , IEXT(N) , NBENRG(N)
      INTEGER          NBCLA
      COMMON /NFICJE/  NBCLA
C ----------------------------------------------------------------------
      INTEGER          IGENR,ITYPE,IDOCU,IORIG,IRNOM(4)
      EQUIVALENCE      (IGENR,GENR),(ITYPE,TYPE),
     &                 (IDOCU,DOCU),(IORIG,ORIG),(IRNOM,RNOM)
      INTEGER          LBIS , LOIS , LOLS , LOUA , LOR8 , LOC8
      COMMON /IENVJE/  LBIS , LOIS , LOLS , LOUA , LOR8 , LOC8
      INTEGER          LFIC,MFIC
      COMMON /FENVJE/  LFIC,MFIC
C
      INTEGER          IPGC,KDESMA(2),LGD,LGDUTI,KPOSMA(2),LGP,LGPUTI
      COMMON /IADMJE/  IPGC,KDESMA,   LGD,LGDUTI,KPOSMA,   LGP,LGPUTI
      INTEGER          LDYN , LGDYN , NBDYN , NBFREE 
      COMMON /IDYNJE/  LDYN , LGDYN , NBDYN , NBFREE 
C ----------------------------------------------------------------------
      CHARACTER*1      KCLAS
      CHARACTER*4      Z
      PARAMETER      ( Z = 'INIT' )
      CHARACTER*8      KNOM,KNOMF,KSTIN,KSTOU,CVERSB,CVERSU
      CHARACTER*16     K16BID
      CHARACTER*24     VALK(3)
      INTEGER          NCAR , ITLEC(1) , ITECR(1) , IADADD(2), LGBL
      INTEGER          VALI(7)
      REAL*8           VALR(1)
      PARAMETER      ( NCAR = 11 )
C ----------------------------------------------------------------------
      LOGICAL          LENRG
      INTEGER          LIDBAS      , LIDEFF
      PARAMETER      ( LIDBAS = 20 , LIDEFF = 15 )
      CHARACTER*8      CIDBAS(LIDBAS)
      INTEGER          KAT(LIDBAS) , LSO(LIDBAS) , KDY(LIDBAS)
      DATA CIDBAS  / '$$CARA  ' , '$$IADD  ' , '$$GENR  ' , '$$TYPE  ' ,
     &               '$$DOCU  ' , '$$ORIG  ' , '$$RNOM  ' , '$$LTYP  ' ,
     &               '$$LONG  ' , '$$LONO  ' , '$$DATE  ' , '$$LUTI  ' ,
     &               '$$HCOD  ' , '$$USADI ' , '$$ACCE  ' , '$$MARQ  ' ,
     &               '$$XXXX  ' , '$$TLEC  ' , '$$TECR  ' , '$$IADM  ' /
C DEB ------------------------------------------------------------------
      IPGCA = IPGC
      IPGC  = -2
C
      KAT(17) = 0
      KDY(17) = 0
      KCLAS = CLAS
      KSTIN = STI
      KSTOU = STO
      KNOM  = NOMF
      CALL LXMINS(NOMF)
      KNOMF = NOMF
C
      CALL ASSERT (KNOMF .NE. '        ' .AND. LEN(NOMF) .LE. 8)
      CALL ASSERT (KCLAS .NE. ' ' ) 
      CALL ASSERT (INDEX (CLASSE,KCLAS) .EQ. 0)
C
      CALL ASSERT (KSTIN .EQ. 'DEBUT   ' .OR. KSTIN .EQ. 'POURSUIT' .OR.
     &             KSTIN .EQ. 'DUMMY   ')
      CALL ASSERT (KSTOU .EQ. 'SAUVE   ' .OR. KSTOU .EQ. 'DETRUIT ') 
      CALL ASSERT (KSTIN .NE. 'DUMMY   ' .OR. KSTOU .NE. 'SAUVE   ') 
      CALL ASSERT (NREP .GT. 0) 
      CALL ASSERT (LBLOC .GT. 0) 
C
      IC = INDEX (CLASSE , ' ')
      CALL ASSERT (IC .GT. 0) 
      NOMFIC(IC) = KNOMF
      NOMBAS(IC) = KNOM
      KSTINI(IC) = KSTIN
      KSTOUT(IC) = KSTOU
      CLASSE(IC:IC) = KCLAS
      NBCLA = INDEX( CLASSE , '$' ) - 1
      IF ( NBCLA .EQ. -1 ) NBCLA = N
C
      ICLAS  = IC
      NBGROS(IC) = 0
      NBPETI(IC) = 0
      NOMUTI = ' '
      NOMOS  = '$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'
      NOMCO  = '$$$$$$$$$$$$$$$$$$$$$$$$'
      NOMOC  = '$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'

C --- ON INTERDIT L'APPEL A JJLDYN LORS DE L'ALLOCATION 
C --- DYNAMIQUE  (ET LES APPELS RECURSIFS)
C
      LDYNOL = LDYN
      IF ( LDYN .EQ. 1 ) THEN  
        LDYN   = 2
      ENDIF

      IF ( KSTIN .EQ. 'DEBUT   ' .OR. KSTIN .EQ. 'DUMMY   ') THEN
        NREMAX(IC) = NREP
        NREUTI(IC) = 0
        NRHCOD(IC) = JJPREM ( NREMAX(IC) )
        NBLUTI(IC) = 0
        LONGBL(IC) = LBLOC
        IF ( NBLOC .EQ. 0 ) THEN
          NBLMAX(IC) = MFIC/(LONGBL(IC)*LOIS)
        ELSE
          NBLMAX(IC) = MIN ( NBLOC , MFIC/(LONGBL(IC)*LOIS) )
        ENDIF
        NBLIM(IC) = 500
C
        LMARQ = 2 * NREP * LOIS
        CALL JJALLS (LMARQ,IC,'V','I',LOIS,Z,IMARQ,IADRS,KMARQ(IC),
     &               KDY(16))
        KAT(16) = KMARQ(IC)
        JMARQ(IC) = IADRS - 1
        CALL JJECRS(KAT(16),KDY(16),IC,16,0,'E',IMARQ(JMARQ(IC)+2*16-1))
C
        LCARAO = NCAR * LOIS
        CALL JJALLS ( LCARAO,IC,'V','I',LOIS,Z,CARA,IADRS,KAT(1),KDY(1))
        JCARA(IC) = IADRS
        CALL JJECRS (KAT(1),KDY(1),IC,1,0,'E',IMARQ(JMARQ(IC)+1))
C
        NBENRG(IC) = MIN(LFIC/(LONGBL(IC)*LOIS),NBLMAX(IC))
C
        NBLOCO = NBLMAX(IC) * LOIS
        CALL JJALLS (NBLOCO,IC,'V','I',LOIS,Z,IACCE,IADRS,KAT(15),
     &               KDY(15))
        JIACCE(IC) = IADRS - 1
        CALL JJECRS(KAT(15),KDY(15),IC,15,0,'E',IMARQ(JMARQ(IC)+2*15-1))
C
        LGBL = 1024*LONGBL(IC)*LOIS
        CALL JJALLS (LGBL,IC,'V','I',LOIS,Z,ITLEC,IADRS ,KITLEC(IC),
     &               KDY(18))
        KAT(18) = KITLEC(IC)
        KITLEC(IC) = ( KITLEC(IC) - 1 ) * LOIS
        CALL JJECRS(KAT(18),KDY(18),IC,18,0,'E',IMARQ(JMARQ(IC)+2*18-1))
C
        CALL JJALLS ( LGBL,IC,'V','I',LOIS,Z,ITECR,IADRS ,KITECR(IC),
     &               KDY(19))
        KAT(19) = KITECR(IC)
        KITECR(IC) = ( KITECR(IC) - 1 ) * LOIS
        CALL JJECRS(KAT(19),KDY(19),IC,19,0,'E',IMARQ(JMARQ(IC)+2*19-1))
C
        NREPO = NREP * LOIS
        CALL JJALLS (2*NREPO,IC,'V','I',LOIS,Z, IADM, IADRS ,KIADM(IC),
     &               KDY(20))
        KAT(20) = KIADM(IC)
        JIADM(IC) = IADRS - 1
        CALL JJECRS(KAT(20),KDY(20),IC,20,0,'E',IMARQ(JMARQ(IC)+2*20-1))
C
C ----- OPEN DU FICHIER
C
        IF ( KSTIN .NE. 'DUMMY   ' ) THEN
          CALL JXOUVR (IC, 1)
          IEXT(IC) = 1
        ENDIF
C
C ----- ECRITURE DANS L'OBJET CARA
C ----- NREMAX NREUTI NRHCOD NBLMAX NBLUTI
C
        CARA(JCARA(IC)    ) = NREMAX(IC)
        CARA(JCARA(IC) +1 ) = NREUTI(IC)
        CARA(JCARA(IC) +2 ) = NRHCOD(IC)
        CARA(JCARA(IC) +3 ) = NBLMAX(IC)
        CARA(JCARA(IC) +4 ) = NBLUTI(IC)
        CARA(JCARA(IC) +5 ) = LONGBL(IC)
        CALL VERSIO ( IVERS , IUTIL , INIVO , K16BID , LEXP )
        CARA(JCARA(IC) +8 ) = IVERS
        CARA(JCARA(IC) +9 ) = IUTIL
        CARA(JCARA(IC) +10) = INIVO
        LON = 2 * NREMAX(IC) * LOIS
        CALL JJALLS (LON,IC,'V','I',LOIS,Z,IADD,IADRS,KAT( 2),KDY(2))
        JIADD(IC) = IADRS - 1
        CALL JJECRS (KAT(2),KDY(2),IC,2,0,'E',IMARQ(JMARQ(IC)+2*2-1))
        LON = NREMAX(IC) * LEN(GENR(1))
        CALL JJALLS(LON,IC,'V','K',LEN(GENR(1)),Z,IGENR,IADRS,KAT(3),
     &               KDY(3))
        JGENR(IC) = IADRS - 1
        CALL JJECRS (KAT(3),KDY(3),IC,3,0,'E',IMARQ(JMARQ(IC)+2*3-1))
        LON = NREMAX(IC) * LEN(TYPE(1))
        CALL JJALLS(LON,IC,'V','K',LEN(TYPE(1)),Z,ITYPE,IADRS,KAT(4),
     &               KDY(4))
        JTYPE(IC) = IADRS - 1
        CALL JJECRS (KAT(4),KDY(4),IC,4,0,'E',IMARQ(JMARQ(IC)+2*4-1))
        LON = NREMAX(IC) * LEN(DOCU(1))
        CALL JJALLS(LON,IC,'V','K',LEN(DOCU(1)),Z,IDOCU,IADRS,KAT(5),
     &               KDY(5))
        JDOCU(IC) = IADRS - 1
        CALL JJECRS (KAT(5),KDY(5),IC,5,0,'E',IMARQ(JMARQ(IC)+2*5-1))
        LON = NREMAX(IC) * LEN(ORIG(1))
        CALL JJALLS(LON,IC,'V','K',LEN(ORIG(1)),Z,IORIG,IADRS,KAT(6),
     &               KDY(6))
        JORIG(IC) = IADRS - 1
        CALL JJECRS (KAT(6),KDY(6),IC,6,0,'E',IMARQ(JMARQ(IC)+2*6-1))
        LON = NREMAX(IC) * LEN(RNOM(1))
        CALL JJALLS(LON,IC,'V','K',LEN(RNOM(1)),Z,IRNOM,IADRS,KAT(7),
     &               KDY(7))
        JRNOM(IC) = IADRS - 1
        CALL JJECRS (KAT(7),KDY(7),IC,7,0,'E',IMARQ(JMARQ(IC)+2*7-1))
        DO 30 IND = 1 , NREMAX(IC)
           RNOM(JRNOM(IC) + IND ) = '?'
 30     CONTINUE
        LON = NREMAX(IC) * LOIS
        CALL JJALLS(LON,IC,'V','I',LOIS,Z,LTYP,IADRS,KAT(8),KDY(8))
        JLTYP(IC) = IADRS - 1
        CALL JJECRS (KAT(8),KDY(8),IC,8,0,'E',IMARQ(JMARQ(IC)+2*8-1))
        CALL JJALLS (LON,IC,'V','I',LOIS,Z,LONG,IADRS,KAT(9),KDY(9))
        JLONG(IC) = IADRS - 1
        CALL JJECRS (KAT(9),KDY(9),IC,9,0,'E',IMARQ(JMARQ(IC)+2*9-1))
        CALL JJALLS (LON,IC,'V','I',LOIS ,Z,LONO, IADRS,KAT(10),KDY(10))
        JLONO(IC) = IADRS - 1
        CALL JJECRS(KAT(10),KDY(10),IC,10,0,'E',IMARQ(JMARQ(IC)+2*10-1))
        CALL JJALLS ( LON,IC,'V','I',LOIS,Z,DATE, IADRS,KAT(11),KDY(11))
        JDATE(IC) = IADRS - 1
        CALL JJECRS(KAT(11),KDY(11),IC,11,0,'E',IMARQ(JMARQ(IC)+2*11-1))
        CALL JJALLS ( LON,IC,'V','I',LOIS,Z,LUTI, IADRS,KAT(12),KDY(12))
        JLUTI(IC) = IADRS - 1
        CALL JJECRS(KAT(12),KDY(12),IC,12,0,'E',IMARQ(JMARQ(IC)+2*12-1))
        LON = NRHCOD(IC) * LOIS
        CALL JJALLS ( LON,IC,'V','I',LOIS,Z,HCOD,IADRS ,KAT(13),KDY(13))
        JHCOD(IC) = IADRS - 1
        CALL JJECRS(KAT(13),KDY(13),IC,13,0,'E',IMARQ(JMARQ(IC)+2*13-1))
        LON = 3*NBLMAX(IC)*LOIS
        CALL JJALLS (LON,IC,'V','I',LOIS,Z,IUSADI,IADRS,KAT(14),KDY(14))
        DO 123 L=1,NBLMAX(IC)
          IUSADI( IADRS + (3*L-2) - 1 ) = -1
          IUSADI( IADRS + (3*L-1) - 1 ) = -1
          IUSADI( IADRS + (3*L  ) - 1 ) =  0
 123    CONTINUE
        JUSADI(IC) = IADRS - 1
        CALL JJECRS(KAT(14),KDY(14),IC,14,0,'E',IMARQ(JMARQ(IC)+2*14-1))
        ICRE = 1
        LTYP(JLTYP(IC)+3 ) = LEN(GENR(1))
        LTYP(JLTYP(IC)+4 ) = LEN(TYPE(1))
        LTYP(JLTYP(IC)+5 ) = LEN(DOCU(1))
        LTYP(JLTYP(IC)+6 ) = LEN(ORIG(1))
        LTYP(JLTYP(IC)+7 ) = LEN(RNOM(1))
        DO 5 I = 1,LIDBAS
          NOMUTI = '________'//NOMBAS(IC)//'________'//CIDBAS(I)
          CALL JJCREN ( NOMUTI , ICRE , IRET )
          GENR(JGENR(IC)+I) = 'V'
          IF ( (I.GE.3 .AND. I.LE.7) ) THEN
            TYPE(JTYPE(IC)+I) = 'K'
          ELSE
            TYPE(JTYPE(IC)+I) = 'I'
            LTYP(JLTYP(IC)+I) = LOIS
          ENDIF
          IF ( I.EQ.1 ) THEN
            LONG(JLONG(IC)+I) = NCAR
            LONO(JLONO(IC)+I) = NCAR
            IADD(JIADD(IC)+2*I-1) = 0
            IADD(JIADD(IC)+2*I  ) = 0
            CALL JXECRO(IC,KAT(1),IADD(JIADD(IC)+2*I-1),
     &                  LONO(JLONO(IC)+I)*LOIS,0,1)
          ELSE IF ( I.EQ.2 .OR. I.EQ.16 .OR. I.EQ.20) THEN
            LONG(JLONG(IC)+I) = 2*NREMAX(IC)
            LONO(JLONO(IC)+I) = 2*NREMAX(IC)
            LSO(I)            = 2*NREMAX(IC) * LOIS
          ELSE IF ( I.EQ.13) THEN
            LONG(JLONG(IC)+I) = NRHCOD(IC)
            LONO(JLONO(IC)+I) = NRHCOD(IC)
            LSO(I)            = NRHCOD(IC) * LTYP(JLTYP(IC)+I)
          ELSE IF ( I.EQ.14 ) THEN
            LONG(JLONG(IC)+I) = 3*NBLMAX(IC)
            LONO(JLONO(IC)+I) = 3*NBLMAX(IC)
            LSO(I)            = 3*NBLMAX(IC) * LTYP(JLTYP(IC)+I)
          ELSE IF ( I.EQ.15 ) THEN
            LONG(JLONG(IC)+I) = NBLMAX(IC)
            LONO(JLONO(IC)+I) = NBLMAX(IC)
            LSO(I)            = NBLMAX(IC) * LTYP(JLTYP(IC)+I)
         ELSE IF ( I.EQ.18 .OR. I.EQ.19 ) THEN
            LONG(JLONG(IC)+I) = LBLOC
            LONO(JLONO(IC)+I) = LBLOC
            LSO(I)            = LBLOC * LTYP(JLTYP(IC)+I)
          ELSE
            LONG(JLONG(IC)+I) = NREMAX(IC)
            LONO(JLONO(IC)+I) = NREMAX(IC)
            LLOC = LONO(JLONO(IC)+I) * LTYP(JLTYP(IC)+I)
            IF ( MOD(LLOC,LOIS) .NE. 0 ) THEN
              LONO(JLONO(IC)+I) = ((1+LLOC/LOIS)*LOIS)/LTYP(JLTYP(IC)+I)
            ENDIF
            LSO(I) = LONO(JLONO(IC)+I) * LTYP(JLTYP(IC)+I)
          ENDIF
          IADM(JIADM(IC)+2*I-1) = KAT(I)
          IADM(JIADM(IC)+2*I  ) = KDY(I)
    5   CONTINUE
    
        DO 10 I=2,LIDEFF
          IADD (JIADD(IC)+2*I-1) = 0
          IADD (JIADD(IC)+2*I  ) = 0
          CALL JXECRO (IC,KAT(I),IADD(JIADD(IC)+2*I-1),LSO(I),0,I)
   10   CONTINUE
        CARA(JCARA(IC)+6) = IADD(JIADD(IC) + 2*2-1 )
        CARA(JCARA(IC)+7) = IADD(JIADD(IC) + 2*2   )
      ELSE
C
C ----- OPEN FICHIER
C ----- LECTURE DANS LE PREMIER BLOC DU FICHIER ET FERMETURE
C
        LCARAO = NCAR * LOIS
        CALL JJALLS (LCARAO,IC,'V','I',LOIS,Z,CARA,IADRS,KAT(1),KDY(1))
        JCARA(IC) = IADRS
        CALL JXLIR1 ( IC , CARA(JCARA(IC)) )
        CVERSB = '  .  .  '
        CALL CODENT(CARA(JCARA(IC) + 8 ),'D ',CVERSB(1:2) )
        CALL CODENT(CARA(JCARA(IC) + 9 ),'D0',CVERSB(4:5) )
        CALL CODENT(CARA(JCARA(IC) + 10),'D0',CVERSB(7:8) )
        CALL VERSIO ( IVERS , IUTIL , INIVO , K16BID , LEXP )
        CVERSU = '  .  .  '
        CALL CODENT(IVERS,'D ',CVERSU(1:2) )
        CALL CODENT(IUTIL,'D0',CVERSU(4:5) )
        CALL CODENT(INIVO,'D0',CVERSU(7:8) )
        NREMAX(IC) = CARA(JCARA(IC)     )
        NREUTI(IC) = CARA(JCARA(IC) + 1 )
        NRHCOD(IC) = CARA(JCARA(IC) + 2 )
        NBLMAX(IC) = CARA(JCARA(IC) + 3 )
        NBLUTI(IC) = CARA(JCARA(IC) + 4 )
        LONGBL(IC) = CARA(JCARA(IC) + 5 )
        IADADD(1)  = CARA(JCARA(IC) + 6 )
        IADADD(2)  = CARA(JCARA(IC) + 7 )
        IF ( CVERSU .NE. CVERSB ) THEN
           VALK(1) = NOMBAS(IC)
           VALK(2) = CVERSB
           VALK(3) = CVERSU
           CALL U2MESK('A','JEVEUX_08', 3 ,VALK)
        ENDIF

        IF ( NBLOC .EQ. 0 ) THEN
          NBLMA2 = MFIC/(LONGBL(IC)*LOIS)
        ELSE
          NBLMA2 = MIN ( NBLOC , MFIC/(LONGBL(IC)*LOIS) )
        ENDIF
C
C ---- LORSQUE LE NOMBRE D'ENREGISTREMENTS MAXIMUM EST MODIFIE
C
        NBLMA1 = NBLMAX(IC)
        IF ( NBLMAX(IC) .GE. NBLMA2 ) THEN
          LENRG = .FALSE.
          NBLMA2 = NBLMAX(IC)
        ELSE
          VALI(1) = NBLMAX(IC)
          VALI(2) = NBLMA2
          VALK(1) = NOMBAS(IC)
          CALL U2MESG('I','JEVEUX_36', 1 , VALK, 2 , VALI, 0 , VALR)
          LENRG = .TRUE.
        ENDIF

        VALK(1)= NOMBAS(IC)
        VALK(2)= CVERSB
        VALI(1)= NBLUTI(IC)
        VALI(2)= NBLMAX(IC)
        VALI(3)= 1024*LONGBL(IC)*LOIS
        VALI(4)= NREUTI(IC)
        VALI(5)= NREMAX(IC)
        VALI(6)= (NREUTI(IC)*100)/NREMAX(IC)

        CALL U2MESG ('I','JEVEUX_21',2,VALK,6,VALI,0,VALR)
C
        NBLMAX(IC)= NBLMA2
C
        LMARQ = 2 * NREMAX(IC) * LOIS
        CALL JJALLS (LMARQ,IC,'V','I',LOIS,Z,IMARQ,IADRS,KMARQ(IC),
     &               KDY(16))
        KAT(16) = KMARQ(IC)
        JMARQ(IC) = IADRS - 1
        CALL JJECRS(KAT(16),KDY(16),IC,16,0,'E',IMARQ(JMARQ(IC)+2*16-1))
C
        LCARAO = NCAR * LOIS
        CALL JJALLS (LCARAO ,IC,'V','I',LOIS,Z,CARA,IADRS,KAT(1),KDY(1))
        JCARA(IC) = IADRS
        CALL JXLIR1 ( IC , CARA(JCARA(IC)) )
        CALL JJECRS (KAT(1),KDY(1),IC,1,0,'E',IMARQ(JMARQ(IC)+2*1-1))
C
        NBENRG(IC) = MIN ( LFIC/(LONGBL(IC)*LOIS) , NBLMA2 )
C
C ----- NOUVEL OPEN DE LA BASE 
        NBEXT = (NBLUTI(IC)/NBENRG(IC))+1
        DO 100 K = 0,NBEXT-1
          CALL JXOUVR (IC , K+1)
 100    CONTINUE
        IEXT(IC) = NBEXT
C
        LGBL = 1024*LONGBL(IC)*LOIS
        CALL JJALLS (LGBL,IC,'V','I',LOIS,Z,ITLEC,IADRS ,KITLEC(IC),
     &               KDY(18))
        KAT(18) = KITLEC(IC)
        KITLEC(IC) = ( KITLEC(IC) - 1 ) * LOIS
        CALL JJECRS(KAT(18),KDY(18),IC,18,0,'E',IMARQ(JMARQ(IC)+2*18-1))
        CALL JJALLS (LGBL,IC,'V','I',LOIS,Z,ITECR,IADRS ,KITECR(IC),
     &               KDY(19))
        KAT(19) = KITECR(IC)
        KITECR(IC) = ( KITECR(IC) - 1 ) * LOIS
        CALL JJECRS(KAT(19),KDY(19),IC,19,0,'E',IMARQ(JMARQ(IC)+2*19-1))
        LON = NREMAX(IC) * LOIS
        CALL JJALLS (2*LON,IC,'V','I',LOIS,Z,IADM,IADRS,KIADM(IC),
     &               KDY(20))
        KAT(20) = KIADM(IC)
        JIADM(IC) = IADRS - 1
        CALL JJECRS(KAT(20),KDY(20),IC,20,0,'E',IMARQ(JMARQ(IC)+2*20-1))

        LON2 = NBLMA2 * LOIS
        CALL JJALLS (LON2,IC,'V','I',LOIS,Z,IACCE,IADRS,KAT(15),KDY(15))
        JIACCE(IC) = IADRS - 1
        CALL JJECRS(KAT(15),KDY(15),IC,15,0,'E',IMARQ(JMARQ(IC)+2*15-1))
C
        CALL JJALLS ( 2*LON, IC,'V','I',LOIS,Z,IADD,IADRS,KAT(2),KDY(2))
        JIADD(IC) = IADRS - 1
        CALL JJECRS(KAT(2),KDY(2),IC,2,0,'E',IMARQ(JMARQ(IC)+2*2-1))
C
        CALL JXLIRO ( IC , KAT( 2) , IADADD  , 2*LON )
C       
        LON2 = 3*NBLMA2 * LOIS
        CALL JJALLS(LON2,IC,'V','I',LOIS,Z,IUSADI,IADRS,KAT(14),KDY(14))
        DO 231 L=1,NBLMA2
          IUSADI( IADRS + (3*L-2) - 1 ) = -1
          IUSADI( IADRS + (3*L-1) - 1 ) = -1
          IUSADI( IADRS + (3*L  ) - 1 ) =  0
 231    CONTINUE
        JUSADI(IC) = IADRS - 1
        CALL JJECRS(KAT(14),KDY(14),IC,14,0,'E',IMARQ(JMARQ(IC)+2*14-1))
        LON1 = 3*NBLMA1 * LOIS
        CALL JXLIRO ( IC , KAT(14), IADD(JIADD(IC)+2*14-1), LON1 )
        IF ( LENRG ) THEN
          CALL JXLIBD ( 0, 14 , IC , IADD(JIADD(IC)+2*14-1) , LON1 )
          IADD(JIADD(IC)+2*14-1) = 0
          IADD(JIADD(IC)+2*14  ) = 0
          CALL JXECRO(IC,KAT(14),IADD(JIADD(IC)+2*14-1),LON2,0,14)
        ENDIF
C 
        LON1 = NBLMA1 * LOIS
        CALL JXLIRO ( IC , KAT(15) , IADD(JIADD(IC)+2*15-1) , LON1 )
        IF ( LENRG ) THEN
          CALL JXLIBD ( 0, 15 , IC , IADD(JIADD(IC)+2*15-1) , LON1 )
          IADD(JIADD(IC)+2*15-1) = 0
          IADD(JIADD(IC)+2*15  ) = 0
          CALL JXECRO(IC,KAT(15),IADD(JIADD(IC)+2*15-1),LON2,0,15)
        ENDIF
C
        LON = NREMAX(IC) * LEN(GENR(1))
        CALL JJALLS(LON,IC,'V','K',LEN(GENR(1)),Z,IGENR,IADRS,KAT(3),
     &              KDY(3))
        JGENR(IC) = IADRS - 1
        CALL JJECRS(KAT(3),KDY(3),IC,3,0,'E',IMARQ(JMARQ(IC)+2*3-1))
        CALL JXLIRO ( IC , KAT(3) , IADD(JIADD(IC)+2*3-1) , LON )
C
        LON = NREMAX(IC) * LEN(TYPE(1))
        CALL JJALLS(LON,IC,'V','K',LEN(TYPE(1)),Z,ITYPE,IADRS,KAT(4),
     &              KDY(4))
        JTYPE(IC) = IADRS - 1
        CALL JJECRS (KAT(4),KDY(4),IC,4,0,'E',IMARQ(JMARQ(IC)+2*4-1))
        CALL JXLIRO ( IC , KAT(4) , IADD(JIADD(IC)+2*4-1) , LON )
        LON = NREMAX(IC) * LEN(DOCU(1))
        CALL JJALLS(LON,IC,'V','K',LEN(DOCU(1)),Z,IDOCU,IADRS,KAT(5),
     &              KDY(5))
        JDOCU(IC) = IADRS - 1
        CALL JJECRS (KAT(5),KDY(5),IC,5,0,'E',IMARQ(JMARQ(IC)+2*5-1))
        CALL JXLIRO ( IC , KAT(5) , IADD(JIADD(IC)+2*5-1) , LON )
        LON = NREMAX(IC) * LEN(ORIG(1))
        CALL JJALLS(LON,IC,'V','K',LEN(ORIG(1)),Z,IORIG,IADRS,KAT(6),
     &              KDY(6))
        JORIG(IC) = IADRS - 1
        CALL JJECRS (KAT(6),KDY(6),IC,6,0,'E',IMARQ(JMARQ(IC)+2*6-1))
        CALL JXLIRO ( IC , KAT(6) , IADD(JIADD(IC)+2*6-1) , LON )
        LON = NREMAX(IC) * LEN(RNOM(1))
        CALL JJALLS(LON,IC,'V','K',LEN(RNOM(1)),Z,IRNOM,IADRS,KAT(7),
     &              KDY(7))
        JRNOM(IC) = IADRS - 1
        CALL JJECRS (KAT(7),KDY(7),IC,7,0,'E',IMARQ(JMARQ(IC)+2*7-1))
        CALL JXLIRO ( IC , KAT(7) , IADD(JIADD(IC)+2*7-1) , LON )
        LON = NREMAX(IC) * LOIS
        CALL JJALLS (LON, IC,'V','I',LOIS,Z,LTYP,IADRS,KAT(8),KDY(8))
        JLTYP(IC) = IADRS - 1
        CALL JJECRS (KAT(8),KDY(8),IC,8,0,'E',IMARQ(JMARQ(IC)+2*8-1))
        CALL JXLIRO ( IC , KAT(8) , IADD(JIADD(IC)+2*8-1) , LON )
        CALL JJALLS (LON, IC,'V','I',LOIS,Z,LONG,IADRS,KAT(9),KDY(9))
        JLONG(IC) = IADRS - 1
        CALL JJECRS (KAT(9),KDY(9),IC,9,0,'E',IMARQ(JMARQ(IC)+2*9-1))
        CALL JXLIRO ( IC , KAT(9),  IADD(JIADD(IC)+2*9-1), LON )
        CALL JJALLS (LON,IC,'V','I',LOIS,Z,LONO,IADRS,KAT(10),KDY(10))
        JLONO(IC) = IADRS - 1
        CALL JJECRS(KAT(10),KDY(10),IC,10,0,'E',IMARQ(JMARQ(IC)+2*10-1))
        CALL JXLIRO ( IC , KAT(10), IADD(JIADD(IC)+2*10-1), LON )
        CALL JJALLS (LON,IC,'V','I',LOIS,Z,DATE,IADRS,KAT(11),KDY(11))
        JDATE(IC) = IADRS - 1
        CALL JJECRS(KAT(11),KDY(11),IC,11,0,'E',IMARQ(JMARQ(IC)+2*11-1))
        CALL JXLIRO ( IC , KAT(11), IADD(JIADD(IC)+2*11-1), LON )
        CALL JJALLS (LON,IC,'V','I',LOIS ,Z,LUTI,IADRS,KAT(12),KDY(12))
        JLUTI(IC) = IADRS - 1
        CALL JJECRS(KAT(12),KDY(12),IC,12,0,'E',IMARQ(JMARQ(IC)+2*12-1))
        CALL JXLIRO ( IC , KAT(12), IADD(JIADD(IC)+2*12-1), LON )
        LON = NRHCOD(IC) * LOIS
        CALL JJALLS (LON,IC,'V','I',LOIS,Z,HCOD,IADRS,KAT(13),KDY(13))
        JHCOD(IC) = IADRS - 1
        CALL JJECRS(KAT(13),KDY(13),IC,13,0,'E',IMARQ(JMARQ(IC)+2*13-1))
        CALL JXLIRO ( IC , KAT(13), IADD(JIADD(IC)+2*13-1), LON )
        DO 20 I = 1 , LIDBAS
           IADM(JIADM(IC) + 2*I-1 ) = KAT(I)
           IADM(JIADM(IC) + 2*I   ) = KDY(I)
 20     CONTINUE
        IF ( LENRG ) THEN
          LONG(JLONG(IC)+15) = NBLMA2
          LONO(JLONO(IC)+15) = NBLMA2
          LONG(JLONG(IC)+14) = 2*NBLMA2
          LONO(JLONO(IC)+14) = 2*NBLMA2
          LON2 = LONO(JLONO(IC)+9) * LTYP(JLTYP(IC)+9)
          CALL JXECRO(IC,KAT(9), IADD(JIADD(IC)+2*9-1), LON2,0,9)
          LON2 = LONO(JLONO(IC)+10) * LTYP(JLTYP(IC)+10)
          CALL JXECRO(IC,KAT(10),IADD(JIADD(IC)+2*10-1),LON2,0,10)
          CALL JXECRO(IC,KAT(1),IADD(JIADD(IC)+2*1-1),
     &                  LONO(JLONO(IC)+1)*LOIS,0,1)
        ENDIF
      ENDIF
C
      LDYN = LDYNOL
      IPGC = IPGCA
C FIN ------------------------------------------------------------------
      END
