      SUBROUTINE JERECU ( CLAS )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C RESPONSABLE LEFEBVRE
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
      IMPLICIT NONE
      INCLUDE 'jeveux_private.h'
      CHARACTER*1         CLAS
C ----------------------------------------------------------------------
C MARQUE LIBRES LES ENREGISTREMENTS ASSOCIÉS AUX PETITS OBJETS QUAND
C L'ENSEMBLE DES OBJETS ASSOCIÉS A ETE DETRUIT
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
      INTEGER IC ,IDCO ,IDCOL ,IDEC ,IDOS ,IDOSL 
      INTEGER JCARA ,JDATE ,JDOCU ,JGENR ,JHCOD 
      INTEGER JIADD ,JIADM ,JLONG ,JLONO ,JLTYP ,JLUTI ,JMARQ 
      INTEGER JORIG ,JRNOM ,JTYPE ,JUSADI ,K ,LGL ,N 
      INTEGER NBDET ,NBGROS ,NBLIM ,NBPETI ,NCLA1 ,NCLA2 
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
      COMMON /INBDET/  NBLIM(N),NBGROS(N),NBPETI(N)
      CHARACTER*2      DN2
      CHARACTER*5      CLASSE
      CHARACTER*8                  NOMFIC    , KSTOUT    , KSTINI
      COMMON /KFICJE/  CLASSE    , NOMFIC(N) , KSTOUT(N) , KSTINI(N) ,
     +                 DN2(N)
      REAL *8          SVUSE,SMXUSE   
      COMMON /STATJE/  SVUSE,SMXUSE  
C     ------------------------------------------------------------------
      LOGICAL          ACTU
      CHARACTER*1      KCLAS
      INTEGER          ITP(1),JITP,IADITP,IADDI(2),LGBL,IADYN
C DEB ------------------------------------------------------------------
      IADDI(2)  = 0
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
        IF ( NBPETI(IC) .LT. NBLIM(IC) ) GOTO 100
        LGBL = 1024*LONGBL(IC)*LOIS
        CALL JJALLS(LGBL,0,'V','I',LOIS,'INIT',ITP,JITP,IADITP,IADYN)
        ISZON(JISZON+IADITP-1) = ISTAT(2)
        ISZON(JISZON+ISZON(JISZON+IADITP-4)-4) = ISTAT(4)
        SVUSE = SVUSE + (ISZON(JISZON+IADITP-4) - IADITP + 4)
        SMXUSE = MAX(SMXUSE,SVUSE)
C
C ----- DECHARGEMENT DES TAMPONS DE LECTURE ET D'ECRITURE
C ----- AFIN D'ACTUALISER LES ADRESSES DISQUES DES COLLECTIONS
C ----- STOCKEES DANS DES PETITS OBJETS
C
        IF ( IITECR(IC) .GT. 0 ) THEN
           CALL JXECRB (IC, IITECR(IC), KITECR(IC)+1, LGBL, 0, 0)
           IITECR(IC) = 0
        ENDIF
        IF ( LITLEC(IC) ) THEN
           CALL JXECRB (IC, IITLEC(IC), KITLEC(IC)+1, LGBL, 0, 0)
           LITLEC(IC) = .FALSE.
           IITLEC(IC) = 0
        ENDIF
C
C ----- BOUCLE "TANT QUE" SUR LES ENREGISTREMENTS UTILISES
C
        K = 1
 200    CONTINUE
C --------L'ENREGISTREMENT 1 N'EST JAMAIS RECUPERABLE
        K = K + 1
        IF ( K .LE. NBLUTI(IC) ) THEN
          IDCO  = IUSADI(JUSADI(IC)+3*K-2)
          IDOS  = IUSADI(JUSADI(IC)+3*K-1)
          NBDET = IUSADI(JUSADI(IC)+3*K  ) 
C
C --------L'ENREGISTREMENT CONTIENT DES PETITS OBJETS 
C --------ON PARCOURT LE CHAINAGE POUR DETERMINER SI UNE
C --------PARTIE DE L'ENREGISTREMENT EST OCCUPEE
C --------ON EXPLORE L'ENREGISTREMENT
C
          IF ( IDCO .EQ. 0 .AND. IDOS .EQ. 0 .AND. NBDET .GT. 0 ) THEN
            IADDI(1) = K
            CALL JXLIRO (IC, IADITP, IADDI, LGBL)
            ACTU = .TRUE.
            IDEC = 0
 300        CONTINUE
            IDCOL  = ISZON(JISZON+IADITP+IDEC  )
            IDOSL  = ISZON(JISZON+IADITP+IDEC+1)
            LGL  = ISZON(JISZON+IADITP+IDEC+2)
            IF ( IDCOL .EQ. 0 .AND. IDOSL .EQ. 0 ) THEN
              GOTO 350
            ELSE IF ( IDCOL .LT. 0 .OR. IDOSL .LT. 0 ) THEN
              GOTO 320
            ENDIF
            ACTU = .FALSE.
            GOTO 350
 320        CONTINUE
            IDEC = IDEC+LGL+3
            GOTO 300
 350        CONTINUE
            IF ( ACTU ) THEN
              IUSADI(JUSADI(IC)+3*K-2) = -1
              IUSADI(JUSADI(IC)+3*K-1) = -1
              IUSADI(JUSADI(IC)+3*K  ) =  0
            ENDIF
          ENDIF
          GOTO 200
        ENDIF
        CALL JJLIDY ( IADYN , IADITP )
        NBPETI(IC) = 0
 100  CONTINUE
C FIN ------------------------------------------------------------------
      END
