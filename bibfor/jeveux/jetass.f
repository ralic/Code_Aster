      SUBROUTINE JETASS ( CLAS )
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
      CHARACTER*1         CLAS
C ----------------------------------------------------------------------
C COMPRESSION D'UNE BASE DE DONNEES PAR RECUPERATION DES ENREGISTREMENTS
C LIBERES LORS D'UNE DESTRUCTION
C
C IN  CLAS   : NOM DE CLASSE ASSOCIEE
C ----------------------------------------------------------------------
      INTEGER          LK1ZON , JK1ZON , LISZON , JISZON 
      COMMON /IZONJE/  LK1ZON , JK1ZON , LISZON , JISZON
C     ------------------------------------------------------------------
      INTEGER          LBIS , LOIS , LOLS , LOR8 , LOC8
      COMMON /IENVJE/  LBIS , LOIS , LOLS , LOR8 , LOC8
      INTEGER          ICLAS ,ICLAOS , ICLACO , IDATOS , IDATCO , IDATOC
      COMMON /IATCJE/  ICLAS ,ICLAOS , ICLACO , IDATOS , IDATCO , IDATOC
      CHARACTER *24                     NOMCO
      CHARACTER *32    NOMUTI , NOMOS ,         NOMOC , BL32
      COMMON /NOMCJE/  NOMUTI , NOMOS , NOMCO , NOMOC , BL32
      INTEGER          ISTAT
      COMMON /ISTAJE/  ISTAT(4)
C     ------------------------------------------------------------------
C-----------------------------------------------------------------------
      INTEGER IBACOL ,IBIADD ,IC ,IDCO ,IDCOL ,IDCOP ,IDEC 
      INTEGER IDOS ,IDOSL ,IDOSP  ,IXIADD ,JCARA ,JDATE 
      INTEGER JDOCU ,JGENR ,JHCOD ,JIADD ,JIADM ,JLONG ,JLONO 
      INTEGER JLTYP ,JLUTI ,JMARQ ,JORIG ,JRNOM ,JTYPE ,JUSADI 
      INTEGER K ,KADD ,KLIB ,LADD ,LD ,LGL ,N 
      INTEGER NCLA1 ,NCLA2 
C-----------------------------------------------------------------------
      PARAMETER  ( N = 5 )
      COMMON /JIATJE/  JLTYP(N), JLONG(N), JDATE(N), JIADD(N), JIADM(N),
     +                 JLONO(N), JHCOD(N), JCARA(N), JLUTI(N), JMARQ(N)
C
      COMMON /JKATJE/  JGENR(N), JTYPE(N), JDOCU(N), JORIG(N), JRNOM(N)
C
      INTEGER          NBLMAX    , NBLUTI    , LONGBL    ,
     +                 KITLEC    , KITECR    ,             KIADM    ,
     +                 IITLEC    , IITECR    , NITECR    , KMARQ
      COMMON /IFICJE/  NBLMAX(N) , NBLUTI(N) , LONGBL(N) ,
     +                 KITLEC(N) , KITECR(N) ,             KIADM(N) ,
     +                 IITLEC(N) , IITECR(N) , NITECR(N) , KMARQ(N)
      LOGICAL          LITLEC
      COMMON /LFICJE/  LITLEC(N)
      COMMON /JUSADI/  JUSADI(N)
      CHARACTER*2      DN2
      CHARACTER*5      CLASSE
      CHARACTER*8                  NOMFIC    , KSTOUT    , KSTINI
      COMMON /KFICJE/  CLASSE    , NOMFIC(N) , KSTOUT(N) , KSTINI(N) ,
     +                 DN2(N)
      REAL *8          SVUSE,SMXUSE   
      COMMON /STATJE/  SVUSE,SMXUSE  
C     ------------------------------------------------------------------
      INTEGER        IDIADD
      PARAMETER    ( IDIADD = 2 )
C     ------------------------------------------------------------------
      LOGICAL          LIBRE,ACTU
      CHARACTER*1      KCLAS
      INTEGER          ITP(1),JITP,IADITP,IADDI(2),IADDIB(2),LGBL,IADYN
C DEB ------------------------------------------------------------------
      IADDI(2)  = 0
      IADDIB(2) = 0
      KCLAS = CLAS
      IF ( KCLAS .EQ. ' ' ) THEN
        NCLA1 = 1
        NCLA2 = INDEX ( CLASSE , '$' ) - 1
        IF ( NCLA2 .LT. 0 ) NCLA2 = N
      ELSE
        NCLA1 = INDEX ( CLASSE , KCLAS)
        NCLA2 = NCLA1
      ENDIF
      DO 100 IC=NCLA1,NCLA2
        LGBL = 1024*LONGBL(IC)*LOIS
        CALL JJALLS(LGBL,0,'V','I',LOIS,'INIT',ITP,JITP,IADITP,IADYN)
        ISZON(JISZON+IADITP-1) = ISTAT(2)
        ISZON(JISZON+ISZON(JISZON+IADITP-4)-4) = ISTAT(4)
        SVUSE = SVUSE + (ISZON(JISZON+IADITP-4) - IADITP + 4)
C
C ----- BOUCLE "TANT QUE" SUR LES ENREGISTREMENTS UTILISES
C
        K = 1
        KLIB  = 0
        IDOSP = 0
        IDCOP = 0
 200    CONTINUE
C
C ----- DECHARGEMENT DES TAMPONS DE LECTURE ET D'ECRITURE
C ----- AFIN D'ACTUALISER LES ADRESSES DISQUES DES COLLECTIONS
C ----- STOCKEES DANS DES PETITS OBJETS
C
        IF ( IITECR(IC) .GT. 0 ) THEN
           CALL JXECRB (IC, IITECR(IC), KITECR(IC)+1, LGBL, 0, 0)
           IITECR(IC) = 0
           NITECR(IC) = 0
        ENDIF
        IF ( LITLEC(IC) ) THEN
           CALL JXECRB (IC, IITLEC(IC), KITLEC(IC)+1, LGBL, 0, 0)
           LITLEC(IC) = .FALSE.
           IITLEC(IC) = 0
        ENDIF
        K = K + 1
        IF ( K .LE. NBLUTI(IC) ) THEN
          LIBRE  = IUSADI(JUSADI(IC)+3*K-2) .LT. -1 .OR.
     &             IUSADI(JUSADI(IC)+3*K-1) .LT. -1
C
C ------- "GROS" OBJET DETRUIT
          IF ( LIBRE ) THEN
            IF ( KLIB .EQ. 0 ) THEN
              KLIB = K
C
C ------- ON POSITIONNE LES INDICATEURS POUR EVITER D'UTILISER 
C-------- L'ENREGISTREMENT DANS JXECRO
              IUSADI(JUSADI(IC)+3*KLIB-2)=0
              IUSADI(JUSADI(IC)+3*KLIB-1)=0
            ENDIF
          ELSE
C
C --------- ENREGISTREMENT A DEPLACER
            IF ( KLIB .NE. 0 ) THEN
              IDCO = IUSADI(JUSADI(IC)+3*K-2)
              IDOS = IUSADI(JUSADI(IC)+3*K-1)
              IF ( IDOS .GT. 0 .OR. IDCO .GT. 0 ) THEN
C
C ----------- L'ENREGISTREMENT CONTIENT UN OU UNE PARTIE D'UN "GROS"
C ----------- OBJET ON ACTUALISE L'ADRESSE DISQUE ET ON REECRIT
C
                IADDI(1) = K
                CALL JXLIRO (IC, IADITP, IADDI, LGBL)
                IDOSL = IDOS
                IDCOL = IDCO
                IADDIB(1) = KLIB
                CALL JXECRO (IC, IADITP, IADDIB, LGBL, IDCO, IDOS)
                IF ( IDOSL .NE. IDOSP .OR. IDCOL .NE. IDCOP ) THEN
C
C ------------- ON ACTUALISE L'ADRESSE DISQUE QUE SI L'UN DES 
C ------------- IDENTIFICATEURS A ETE MODIFIE
C
                  IDOSP = IDOSL
                  IDCOP = IDCOL
                  IF ( IDCO .EQ. 0 ) THEN
                    IADD(JIADD(IC)+2*IDOS-1) = KLIB
                  ELSE
                    CALL JJALLC(IC,IDCO,'E',IBACOL)
                    IXIADD = ISZON(JISZON+IBACOL+IDIADD)
                    IF ( IXIADD .GT. 0 ) THEN
                      IBIADD = IADM(JIADM(IC)+2*IXIADD-1)
                      ISZON(JISZON+IBIADD-1+2*IDOS-1) = KLIB
                    ENDIF
C
C ----------------- MISE A JOUR DU COMMON /IATCJE/ POUR APPEL JJLIDE
                    ICLAS  = IC
                    ICLAOS = IC
                    IDATOS = IXIADD
                    NOMOS  = RNOM(JRNOM(IC)+IXIADD)
                    CALL JJLIDE('JETASS',RNOM(JRNOM(IC)+IXIADD),1)
                  ENDIF
                ENDIF
                CALL JXLIBD (IDCOL, IDOSL, IC, IADDI, LOIS)
                KLIB = MIN(KLIB+1,K)
                IUSADI(JUSADI(IC)+3*KLIB-2)=0
                IUSADI(JUSADI(IC)+3*KLIB-1)=0
C
C ----------- L'ENREGISTREMENT CONTIENT DES PETITS OBJETS
C ----------- ON ACTUALISE LES ADRESSES DISQUE ET ON REECRIT
C ----------- IL N'Y A PAS DE RETASSAGE AU SEIN DE L'ENREGISTREMENT
C ----------- ON EXPLORE L'ENREGISTREMENT
C
              ELSE IF ( IDCO .EQ. 0 .AND. IDOS .EQ. 0 ) THEN
                IADDI(1) = K
                CALL JXLIRO (IC, IADITP, IADDI, LGBL)
                ACTU = .FALSE.
                IDEC = 0
 300            CONTINUE
                IDCOL  = ISZON(JISZON+IADITP+IDEC  )
                IDOSL  = ISZON(JISZON+IADITP+IDEC+1)
                LGL  = ISZON(JISZON+IADITP+IDEC+2)
                IF ( IDCOL .EQ. 0 .AND. IDOSL .EQ. 0 ) THEN
                  GOTO 350
                ELSE IF ( IDCOL .LT. 0 .OR. IDOSL .LT. 0 ) THEN
                  GOTO 320
                ENDIF
                ACTU = .TRUE.
                IF ( IDCOL .EQ. 0 ) THEN
                  IADD(JIADD(IC)+2*IDOSL-1) = KLIB
                ELSE
                  CALL JJALLC(IC,IDCOL,'E',IBACOL)
                  IXIADD = ISZON(JISZON+IBACOL+IDIADD)
                  KADD = IADD(JIADD(IC)+2*IXIADD-1)
                  IF ( IXIADD .GT. 0 ) THEN
                    IBIADD = IADM(JIADM(IC)+2*IXIADD-1)
                    LADD = ISZON(JISZON+IBIADD-1+2*IDOSL)
                    ISZON(JISZON+IBIADD-1+2*IDOSL-1) = KLIB
                    IF ( KADD .NE. K ) THEN
                      ICLAS  = IC
                      ICLAOS = IC
                      IDATOS = IXIADD
                      NOMOS  = RNOM(JRNOM(IC)+IXIADD)
                      CALL JJLIDE('JETASS',RNOM(JRNOM(IC)+IXIADD),1)
                    ELSE
                      LD = IADD(JIADD(IC)+2*IXIADD)
                      ISZON(JISZON+IADITP+(LD/LOIS)-1+2*IDOSL-1) = KLIB
                      ISZON(JISZON+IADITP+(LD/LOIS)-1+2*IDOSL  ) = LADD
                    ENDIF
                  ENDIF
                ENDIF
 320            CONTINUE
                IDEC = IDEC+LGL+3
                GOTO 300
 350            CONTINUE
                IF ( ACTU ) THEN
                  CALL JXLIBD (IDCOL, IDOSL, IC, IADDI, LOIS)
                  IADDIB(1) = KLIB
                  CALL JXECRO (IC,IADITP,IADDIB,LGBL,IDCO,IDOS)
                  IUSADI(JUSADI(IC)+3*KLIB) = IUSADI(JUSADI(IC)+3*K)
                  KLIB = MIN(KLIB+1,K)
                  IUSADI(JUSADI(IC)+3*KLIB-2)=0
                  IUSADI(JUSADI(IC)+3*KLIB-1)=0
                ENDIF
              ENDIF
            ENDIF
          ENDIF
          GOTO 200
        ENDIF
        IF ( KLIB .GT. 0 ) THEN
          DO 400 K = KLIB , NBLUTI(IC)
            IUSADI(JUSADI(IC)+3*K-2) = -1
            IUSADI(JUSADI(IC)+3*K-1) = -1
            IUSADI(JUSADI(IC)+3*K  ) =  0
 400      CONTINUE
          NBLUTI(IC) = KLIB-1
        ENDIF
        CALL JJLIDY ( IADYN , IADITP )
 100  CONTINUE
C FIN ------------------------------------------------------------------
      END
