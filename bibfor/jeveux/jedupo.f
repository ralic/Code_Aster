      SUBROUTINE JEDUPO(SCHIN, CLAOUT, SCHOUT, DUPCOL)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 30/05/2011   AUTEUR LEFEBVRE J-P.LEFEBVRE 
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
C TOLE CFT_726 CFT_720 CRP_18 CRS_508  CRS_512
C RESPONSABLE LEFEBVRE J-P.LEFEBVRE
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)       SCHIN, CLAOUT, SCHOUT
      LOGICAL                                           DUPCOL
C ----------------------------------------------------------------------
C     RECOPIE L'OBJET DE NOM SCHIN LA CLASSE DANS
C     SCHOUT AVEC LE NOM DISTINCT SCHOUT
C
C IN  SCHIN  : SOUS-CHAINE EN ENTREE
C IN  CLAOUT : NOM DE LA CLASSE EN SORTIE
C IN  SCHOUT : SOUS-CHAINE EN SORTIE
C IN  DUPCOL : .TRUE. DUPLIQUE LES OBJETS PARTAGEABLES D'UNE COLLECTION
C              .FALSE. S'ARRETE SUR ERREUR
C
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
      INTEGER          LBIS , LOIS , LOLS , LOUA , LOR8 , LOC8
      COMMON /IENVJE/  LBIS , LOIS , LOLS , LOUA , LOR8 , LOC8
      INTEGER          ISTAT
      COMMON /ISTAJE/  ISTAT(4)
      INTEGER          NUMEC
      COMMON /INUMJE/  NUMEC
      CHARACTER*24     NOMEC
      COMMON /KNOMJE/  NOMEC
C     ------------------------------------------------------------------
      INTEGER        IVNMAX     , IDDESO     , IDIADD     , IDIADM     ,
     +               IDMARQ     , IDNOM      ,              IDLONG     ,
     +               IDLONO     , IDLUTI     , IDNUM
      PARAMETER    ( IVNMAX = 0 , IDDESO = 1 , IDIADD = 2 , IDIADM = 3 ,
     +               IDMARQ = 4 , IDNOM  = 5 ,              IDLONG = 7 ,
     +               IDLONO = 8 , IDLUTI = 9 , IDNUM  = 10 )
      INTEGER          IV(IDNUM)
      CHARACTER*8      CSUFFI(IDNUM)
C ----------------------------------------------------------------------
      INTEGER          LTYPI,IADDI(2)
      CHARACTER*32     NOMIN,NOMOUT,NOMCOL
      CHARACTER*1      KCLAS,GENRI,TYPEI
      LOGICAL          IDENBA,LIBCOL,X2U
      REAL*8           VAL
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
      IF ( IRET1 .EQ. 1 ) THEN
        ICIN  = ICLAOS
        IDIN  = IDATOS
        ICLAS = ICOUT
        CALL JJCREN ( NOMOUT(1:24) , 0 , IRET2 )
        IF ( IRET2 .NE. 0 ) THEN
          CALL JEDETR ( NOMOUT(1:24) )
        ENDIF
        CALL JJCREN ( NOMOUT(1:24) , 1 , IRET2 )
        IDOUT = IDATOS
        GENR(JGENR(ICOUT)+IDOUT) = GENR(JGENR(ICIN)+IDIN)
        LTYP(JLTYP(ICOUT)+IDOUT) = LTYP(JLTYP(ICIN)+IDIN)
        TYPE(JTYPE(ICOUT)+IDOUT) = TYPE(JTYPE(ICIN)+IDIN)
        LONO(JLONO(ICOUT)+IDOUT) = LONO(JLONO(ICIN)+IDIN)
        LONG(JLONG(ICOUT)+IDOUT) = LONG(JLONG(ICIN)+IDIN)
        GENRI = GENR(JGENR(ICOUT)+IDOUT)
        TYPEI = TYPE(JTYPE(ICOUT)+IDOUT)
        LTYPI = LTYP(JLTYP(ICOUT)+IDOUT)
        LONOI = LONO(JLONO(ICOUT)+IDOUT)
        NBL   = LONOI * LTYPI
        IADMI = IADM(JIADM(ICIN)+2*IDIN-1)
C
        IF ( IADMI .NE. 0 ) THEN
          ISTA1 = ISZON(JISZON+IADMI-1)
          ISTA2 = ISZON(JISZON+ISZON(JISZON+IADMI-4)-4)
          IF ( ISTA1.EQ.ISTAT(1) .AND.
     &        (ISTA2.EQ.ISTAT(3).OR.ISTA2.EQ.ISTAT(4)) ) THEN
            X2U = .TRUE.
            ISZON(JISZON+IADMI-1) = ISTAT(2)
          ELSE
            X2U = .FALSE.
          ENDIF
        ENDIF
        CALL JJALLT (NBL,ICOUT,GENRI,TYPEI,LTYPI,'INIT',IADOUT,IADYN)
        CALL JJECRS (IADOUT,IADYN,ICOUT,IDOUT,0,'E',
     &               IMARQ(JMARQ(ICOUT)+2*IDOUT-1))
        IADM(JIADM(ICOUT)+2*IDOUT-1) = IADOUT
        IADM(JIADM(ICOUT)+2*IDOUT  ) = IADYN
        IADMI = IADM(JIADM(ICIN)+2*IDIN-1)
        IADDI(1) = IADD(JIADD(ICIN)+2*IDIN-1)
        IADDI(2) = IADD(JIADD(ICIN)+2*IDIN  )
        IF ( IADMI .NE. 0 ) THEN
          IADMO1 = (IADMI-1)*LOIS+ISZON(JISZON+IADMI-3)+1
          IADMO2 = (IADOUT-1)*LOIS+ISZON(JISZON+IADOUT-3)+1
          CALL JXDEPS(IADMO1,IADMO2,LONOI*LTYPI)
          IF (X2U) ISZON(JISZON+IADMI-1) = ISTAT(1)
        ELSE IF ( IADDI(1) .GT. 0 ) THEN
          CALL JXLIRO (ICIN,IADOUT,IADDI,LONOI*LTYPI)
        ELSE
          CALL U2MESK('F','JEVEUX1_66',1,NOMIN)
        ENDIF
        DOCU(JDOCU(ICOUT)+IDOUT) = DOCU(JDOCU(ICIN)+IDIN)
        LUTI(JLUTI(ICOUT)+IDOUT) = LUTI(JLUTI(ICIN)+IDIN)
        CALL JJLIDE('JELIBE',NOMOUT(1:24),IRET2)
      ELSE IF ( IRET1 .EQ. 2 ) THEN
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
        ICLAS = ICOUT
        CALL JJCREN ( NOMOUT(1:24) , 0 , IRET2 )
        IF ( IRET2 .NE. 0 ) THEN
          CALL JEDETR ( NOMOUT(1:24) )
        ENDIF
        CALL JJCREN ( NOMOUT(1:24) , 2 , IRET2 )
        IDOUT  = IDATCO
        IDCOUT = IDATCO
        CALL JJCREC (ICOUT,IDOUT,'X','I',IDNUM+1,IADZON)
        ISZON(JISZON+IADZON) = NMAX
        IBAOUT = IADM(JIADM(ICOUT)+2*IDOUT-1)
        IF (ISZON(JISZON+IBACOL+2) .EQ. 0) THEN
          IV(1) = 1
        ELSE
          IV(1) = 0
        ENDIF
C
C ----- RECOPIE DES OBJETS ATTRIBUTS DE COLLECTION
C
        IDENBA = ICIN .EQ. ICOUT
        DO 1 K = 1,IDNUM
          IDAT = ISZON ( JISZON + IBACOL + K )
          IF ( IDAT .GT. 0 ) THEN
            NOMIN = RNOM(JRNOM(ICIN)+IDAT)
            IF ( NOMIN(1:24) .NE. NOMCOL(1:24) ) THEN
              IF ( DUPCOL .OR. .NOT. IDENBA ) THEN
                NOMOUT = NOMOUT(1:24)//CSUFFI(K)
              ELSE
                NOMOUT = NOMIN
              ENDIF
            ELSE
              NOMOUT = NOMOUT(1:24)//NOMIN(25:32)
            ENDIF
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
              LONO(JLONO(ICOUT)+IDOUT) = LONO(JLONO(ICIN)+IDAT)
              LONG(JLONG(ICOUT)+IDOUT) = LONG(JLONG(ICIN)+IDAT)
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
              IF ( IV(K) .EQ. 1 ) THEN
                IF ( IADMI .NE. 0 ) THEN
                  IADMO1 = (IADMI-1)*LOIS+ISZON(JISZON+IADMI-3)+1
                  IADMO2 = (IADOUT-1)*LOIS+ISZON(JISZON+IADOUT-3)+1
                  CALL JXDEPS(IADMO1,IADMO2,LONOI*LTYPI)
                ELSE IF ( IADDI(1) .GT. 0 ) THEN
                  CALL JXLIRO (ICIN,IADOUT,IADDI,LONOI*LTYPI)
                ENDIF
              ENDIF
              DOCU(JDOCU(ICOUT)+IDOUT) = DOCU(JDOCU(ICIN)+IDAT)
              LUTI(JLUTI(ICOUT)+IDOUT) = LUTI(JLUTI(ICIN)+IDAT)
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
            NUMEC=K
            NOMEC=' '
            IF ( IADMI .NE. 0 ) THEN
              IADMO1 = (IADMI-1)*LOIS+ISZON(JISZON+IADMI-3)+1
              IADMO2 = (IADOUT-1)*LOIS+ISZON(JISZON+IADOUT-3)+1
              CALL JXDEPS(IADMO1,IADMO2,NBL)
              IF (X2U) ISZON(JISZON+IADMI-1) = ISTAT(1)
              CALL JJLIDE('JELIBE',NOMIN(1:24)//'$$XNUM  ',2)
            ELSE IF ( IADDI(1) .GT. 0 ) THEN
              CALL JXLIRO (ICIN,IADOUT,IADDI,NBL)
            ELSE
              CALL U2MESG('F','JEVEUX1_65',1,NOMIN,1,K,0,VAL)
            ENDIF
            CALL JJLIDE('JELIBE',NOMOUT(1:24)//'$$XNUM  ',2)
 2        CONTINUE
        ENDIF
        IF (LIBCOL) CALL JJLIDE('JELIBE',NOMIN(1:24),IRET1)
        CALL JJLIDE('JELIBE',NOMOUT(1:24),IRET2)
      ENDIF
C FIN ------------------------------------------------------------------
      END
