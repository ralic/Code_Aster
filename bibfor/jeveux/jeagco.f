      SUBROUTINE JEAGCO(SCHIN, SCHOUT, NBOCNW, LONTNW, CLAOUT)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C RESPONSABLE LEFEBVRE J-P.LEFEBVRE
C TOLE CRP_18 CRS_508  CRS_512
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)     SCHIN, SCHOUT, CLAOUT
      INTEGER           NBOCNW, LONTNW
C ----------------------------------------------------------------------
C     AGRANDIT LA COLLECTION DE NOM SCHOUT A PARTIR DE SCHIN SUR LA BASE
C     CLAOUT
C
C IN  SCHIN  : SOUS-CHAINE EN ENTREE : NOM DE COLLECTION
C OUT SCHOUT : SOUS-CHAINE EN SORTIE : NOM DE LA COLLECTION A CREER
C              SI L'OBJET SCHOUT EXISTE, IL EST DETRUIT
C IN  NBOCNW : NOMBRE D'OBJET DE LA NOUVELLE COLLECTION
C IN  LONTNW : LONGUEUR TOTALE DE LA NOUVELLE COLLECTION DANS LE CAS
C              COLLECTION CONTIGUE
C IN  CLAOUT : NOM DE LA CLASSE DE LA COLLECTION A CREER
C     POUR UNE COLLECTION DISERSEE ON VERIFIE QUE LOBNTNW > 0
C     POUR UNE COLLECTION CONTIGUEE ON S'ASSURE QUE LONTNW >= LONT SCHIN
C ----------------------------------------------------------------------
      CHARACTER*1      K1ZON
      COMMON /KZONJE/  K1ZON(8)
      INTEGER          LK1ZON , JK1ZON , LISZON , JISZON , ISZON(1)
      COMMON /IZONJE/  LK1ZON , JK1ZON , LISZON , JISZON
      EQUIVALENCE    ( ISZON(1) , K1ZON(1) )
      INTEGER          ICLAS ,ICLAOS , ICLACO , IDATOS , IDATCO , IDATOC
      COMMON /IATCJE/  ICLAS ,ICLAOS , ICLACO , IDATOS , IDATCO , IDATOC
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
      CHARACTER*2      DN2
      CHARACTER*5      CLASSE
      CHARACTER*8                  NOMFIC    , KSTOUT    , KSTINI
      COMMON /KFICJE/  CLASSE    , NOMFIC(N) , KSTOUT(N) , KSTINI(N) ,
     +                 DN2(N)
      INTEGER          LBIS , LOIS , LOLS , LOR8 , LOC8
      COMMON /IENVJE/  LBIS , LOIS , LOLS , LOR8 , LOC8
      INTEGER          ISTAT
      COMMON /ISTAJE/  ISTAT(4)
C     ------------------------------------------------------------------
      INTEGER        IVNMAX     , IDDESO     , IDIADD     , IDIADM     ,
     +               IDMARQ     , IDNOM      ,              IDLONG     ,
     +               IDLONO     , IDLUTI     , IDNUM
      PARAMETER    ( IVNMAX = 0 , IDDESO = 1 , IDIADD = 2 , IDIADM = 3 ,
     +               IDMARQ = 4 , IDNOM  = 5 ,              IDLONG = 7 ,
     +               IDLONO = 8 , IDLUTI = 9 , IDNUM  = 10 )
      INTEGER          IV(IDNUM)
      CHARACTER*8      CSUFFI(IDNUM)
      CHARACTER*32   JEXNUM,JEXNOM
C ----------------------------------------------------------------------
      INTEGER          LTYPI,IADDI(2)
      CHARACTER*24     NOM24
      CHARACTER*32     NOMIN,NOMOUT,NOMCOL
      CHARACTER*1      KCLAS,GENRI,TYPEI
      LOGICAL          LIBCOL,X2U,LCONST,LNOM
      DATA             IV / 0 , 0 , 0 , 0 , 1 , 0 , 1 , 1 , 1 , 1  /
      DATA             CSUFFI / '$$DESO  ','$$IADD  ','$$IADM  ',
     +                          '$$MARQ  ','$$NOM   ','        ',
     +                          '$$LONG  ','$$LONO  ','$$LUTI  ',
     +                          '$$NUM   ' /
C DEB ------------------------------------------------------------------
      KCLAS = CLAOUT
      ICOUT = INDEX ( CLASSE , KCLAS)
C
      NOMIN  = SCHIN
      NOMOUT = SCHOUT

      CALL JJCREN ( NOMIN(1:24) , 0 , IRET1 )
C
      IF ( IRET1 .EQ. 2 ) THEN
C
C ----  OBJET DE TYPE COLLECTION
C
        NOMCOL = NOMIN
        IDIN   = IDATCO
        ICIN   = ICLACO
        IBACOL = IADM ( JIADM(ICIN) + 2*IDIN-1 )
        LIBCOL = .FALSE.
        IF (IBACOL.EQ.0) THEN
          LIBCOL = .TRUE.
        ELSE
          IF (ISZON(JISZON+IBACOL-1) .EQ. ISTAT(1)) LIBCOL = .TRUE.
        ENDIF
        CALL JJALLC (ICIN, IDIN, 'L', IBACOL )
        NMAX = ISZON(JISZON+IBACOL+IVNMAX)
        CALL ASSERT (NMAX .LE. NBOCNW)
        ICLAS = ICOUT
        CALL JJCREN ( NOMOUT(1:24) , 0 , IRET2 )
        IF ( IRET2 .NE. 0 ) THEN
          CALL JEDETR ( NOMOUT(1:24) )
        ENDIF
        CALL JJCREN ( NOMOUT(1:24) , 2 , IRET2 )
        IDOUT  = IDATCO
        IDCOUT = IDATCO
        CALL JJCREC (ICOUT,IDOUT,'X','I',IDNUM+1,IADZON)
        ISZON(JISZON+IADZON+IVNMAX) = NBOCNW
        IBAOUT = IADM(JIADM(ICOUT)+2*IDOUT-1)
        IF (ISZON(JISZON+IBACOL+IDIADD) .EQ. 0) THEN
C
C ----  COLLECTION CONTIGUE
          IV(1) = 1
        ELSE
C
C ----  COLLECTION DISPERSEE
          IV(1) = 0
          CALL ASSERT(LONTNW .EQ. 0)
        ENDIF

        IXLONG  = ISZON (JISZON+IBACOL+IDLONG)
        LCONST  = (IXLONG .EQ. 0)
        IXNOM   = ISZON (JISZON+IBACOL+IDNOM)
        LNOM    = (IXNOM .NE. 0)
C
C ----- RECOPIE DES OBJETS ATTRIBUTS DE COLLECTION
C
        DO 1 K = 1,IDNUM
          IDAT = ISZON ( JISZON + IBACOL + K )
          IF ( IDAT .GT. 0 ) THEN
            NOMIN = RNOM(JRNOM(ICIN)+IDAT)
            NOMOUT = NOMOUT(1:24)//CSUFFI(K)
            CALL JJCREN ( NOMOUT(1:32) , 0 , IRET3 )
            IF ( IRET3 .EQ. 0 ) THEN
              CALL JJCREN ( NOMOUT(1:32) , 1 , IRET3 )
              IDOUT = IDATOS
              IADMI = IADM(JIADM(ICIN)+2*IDAT-1)
              IADDI(1) = IADD(JIADD(ICIN)+2*IDAT-1)
              IADDI(2) = IADD(JIADD(ICIN)+2*IDAT  )
              GENR(JGENR(ICOUT)+IDOUT) = GENR(JGENR(ICIN)+IDAT)
              LTYP(JLTYP(ICOUT)+IDOUT) = LTYP(JLTYP(ICIN)+IDAT)
              TYPE(JTYPE(ICOUT)+IDOUT) = TYPE(JTYPE(ICIN)+IDAT)
              LLECT = LONO(JLONO(ICIN)+IDAT)
C
              IF ( K.EQ.IDDESO .AND. IV(1).EQ.1 ) THEN
                LONO(JLONO(ICOUT)+IDOUT)= MAX(LONTNW,LLECT)
                IF ( LCONST ) THEN
                  LONG(JLONG(ICOUT)+IDOUT)=LONTNW/NBOCNW
                ENDIF
              ELSE IF ( K.EQ.IDLONO ) THEN
                LONO(JLONO(ICOUT)+IDOUT)= NBOCNW+1
                LONG(JLONG(ICOUT)+IDOUT)= NBOCNW+1
              ELSE IF ( K.EQ.IDNUM ) THEN
                LONO(JLONO(ICOUT)+IDOUT)= 2
                LONG(JLONG(ICOUT)+IDOUT)= 2
              ELSE
                LONO(JLONO(ICOUT)+IDOUT)= NBOCNW
                LONG(JLONG(ICOUT)+IDOUT)= NBOCNW
              ENDIF
              GENRI = GENR(JGENR(ICOUT)+IDOUT)
              TYPEI = TYPE(JTYPE(ICOUT)+IDOUT)
              LTYPI = LTYP(JLTYP(ICOUT)+IDOUT)
              LONOI = LONO(JLONO(ICOUT)+IDOUT)
              NBL   = LONOI * LTYPI
              IF (NBL .GT. 0 ) THEN
                IF( (K.EQ.1 .AND. IV(1).EQ.1 ) .OR. K .GT. 1) THEN
                  CALL JJALLT(NBL,ICOUT,GENRI,TYPEI,LTYPI,'INIT',IADOUT,
     &                        IADYN)
                  CALL JJECRS(IADOUT,IADYN,ICOUT,IDOUT,0,'E',
     &                        IMARQ(JMARQ(ICOUT)+2*IDOUT-1))
                  IADM(JIADM(ICOUT)+2*IDOUT-1) = IADOUT
                  IADM(JIADM(ICOUT)+2*IDOUT  ) = IADYN
                ENDIF
              ENDIF
C
              IF ( K.EQ.IDNOM ) THEN
                IF( LNOM ) THEN
C
C -- IL FAUT TRAITER LE REPERTOIRE DE NOM A PART
C
                  DO 20 IOC =1,LUTI(JLUTI(ICIN) + IXNOM)
                    CALL JENUNO(JEXNUM(NOMIN,IOC),NOM24)
                    CALL JECROC(JEXNOM(NOMOUT,NOM24))
 20               CONTINUE
                ENDIF
C
              ELSE IF (  IV(K) .EQ. 1 ) THEN
                IF ( IADMI .NE. 0 ) THEN
                  IADMO1 = (IADMI-1)*LOIS+ISZON(JISZON+IADMI-3)+1
                  IADMO2 = (IADOUT-1)*LOIS+ISZON(JISZON+IADOUT-3)+1
                  CALL JXDEPS(IADMO1,IADMO2,LLECT*LTYPI)
                ELSE IF ( IADDI(1) .GT. 0 ) THEN
                  CALL JXLIRO (ICIN,IADOUT,IADDI,LLECT*LTYPI)
                ENDIF
              ENDIF
              DOCU(JDOCU(ICOUT)+IDOUT) = DOCU(JDOCU(ICIN)+IDAT)
              LUTI(JLUTI(ICOUT)+IDOUT) = LUTI(JLUTI(ICIN)+IDAT)
              IF ( K.EQ.IDNUM .AND. IADMI .NE. 0 ) THEN
                ISZON(JISZON+IADOUT)   = NBOCNW
                ISZON(JISZON+IADOUT+1) = ISZON(JISZON+IADMI+1)
              ENDIF
            ENDIF
            ISZON(JISZON+IBAOUT+K) = IDATOS
          ENDIF
 1      CONTINUE
C
C ----- POUR UNE COLLECTION DISPERSEE, RECOPIE DES SEGMENTS DE VALEURS
C ----- ASSOCIES AUX OBJETS DE COLLECTION
C
        IF ( IV(1) .EQ. 0 ) THEN
          IXDESO = ISZON(JISZON+IBACOL+IDDESO)
          IXIADM = ISZON(JISZON+IBACOL+IDIADM)
          IBIADM = IADM(JIADM(ICIN)+2*IXIADM-1)
C
          IXMARO = ISZON(JISZON+IBAOUT+IDMARQ)
          IBMARO = IADM(JIADM(ICOUT)+2*IXMARO-1)
C
          IXIADO = ISZON(JISZON+IBAOUT+IDIADM)
          IBIADO = IADM(JIADM(ICOUT)+2*IXIADO-1)
          IXIADD = ISZON(JISZON+IBACOL+IDIADD)
          IBIADD = IADM(JIADM(ICIN)+2*IXIADD-1)
          IXLONO = ISZON(JISZON+IBACOL+IDLONO)
          GENRI = GENR(JGENR(ICIN)+IXDESO)
          TYPEI = TYPE(JTYPE(ICIN)+IXDESO)
          LTYPI = LTYP(JLTYP(ICIN)+IXDESO)
          DO 2 K=1,NMAX
            IADMI = ISZON(JISZON+IBIADM-1+2*K-1)
            IADDI(1) = ISZON(JISZON+IBIADD-1+2*K-1)
            IADDI(2) = ISZON(JISZON+IBIADD-1+2*K  )
            IF ( IADMI .EQ. 0 .AND. IADDI(1) .EQ. 0 ) GOTO 2
            IF ( IXLONO .EQ. 0 ) THEN
              NBL = LONO(JLONO(ICIN)+IXDESO)*LTYPI
            ELSE
              IBLONO = IADM(JIADM(ICIN)+2*IXLONO-1)
              NBL = ISZON(JISZON+IBLONO-1+K)*LTYPI
            ENDIF
            IF ( IADMI .NE. 0 ) THEN
              ISTA1 = ISZON(JISZON+IADMI-1)
              ISTA2 = ISZON(JISZON+ISZON(JISZON+IADMI-4)-4)
              IF ( ISTA1.EQ.ISTAT(1) .AND.
     &            (ISTA2.EQ.ISTAT(3).OR.ISTA2.EQ.ISTAT(4)) ) THEN
                X2U = .TRUE.
                ISZON(JISZON+IADMI-1) = ISTAT(2)
              ELSE
                X2U = .FALSE.
              ENDIF
            ENDIF
            CALL JJALLT (NBL,ICOUT,GENRI,TYPEI,LTYPI,'INIT',IADOUT,
     &                   IADYN)
            CALL JJECRS (IADOUT,IADYN,ICOUT,K,IDCOUT,'E',
     &                   ISZON(JISZON+IBMARO-1+2*K-1))
            ISZON(JISZON+IBIADO-1+2*K-1) = IADOUT
            ISZON(JISZON+IBIADO-1+2*K  ) = IADYN
            IF ( IADMI .NE. 0 ) THEN
              IADMO1 = (IADMI-1)*LOIS+ISZON(JISZON+IADMI-3)+1
              IADMO2 = (IADOUT-1)*LOIS+ISZON(JISZON+IADOUT-3)+1
              CALL JXDEPS(IADMO1,IADMO2,NBL)
              IF (X2U) ISZON(JISZON+IADMI-1) = ISTAT(1)
            ELSE IF ( IADDI(1) .GT. 0 ) THEN
              CALL JXLIRO (ICIN,IADOUT,IADDI,NBL)
            ELSE
              CALL U2MESG('F','JEVEUX1_65',1,NOMIN,1,K,0,VAL)
            ENDIF
 2        CONTINUE
        ENDIF
        IF (LIBCOL) CALL JJLIDE('JELIBE',NOMIN(1:24),IRET1)
        CALL JJLIDE('JELIBE',NOMOUT(1:24),IRET2)
      ENDIF
C FIN ------------------------------------------------------------------
      END
