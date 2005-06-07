      SUBROUTINE JELIBF ( COND , CLAS )
C COMPIL PARAL
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 21/11/2004   AUTEUR D6BHHJP J.P.LEFEBVRE 
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
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)       COND , CLAS
C ----------------------------------------------------------------------
      INTEGER          LBIS , LOIS , LOLS , LOUA , LOR8 , LOC8
      COMMON /IENVJE/  LBIS , LOIS , LOLS , LOUA , LOR8 , LOC8
      LOGICAL          LCRA
      COMMON /LENVJE/  LCRA
C ----------------------------------------------------------------------
      PARAMETER  ( N = 5 )
      INTEGER          LTYP    , LONG    , DATE    , IADD    , IADM    ,
     +                 LONO    , HCOD    , CARA    , LUTI    , IMARQ
      COMMON /IATRJE/  LTYP(1) , LONG(1) , DATE(1) , IADD(1) , IADM(1) ,
     +                 LONO(1) , HCOD(1) , CARA(1) , LUTI(1) , IMARQ(1)
      COMMON /JIATJE/  JLTYP(N), JLONG(N), JDATE(N), JIADD(N), JIADM(N),
     +                 JLONO(N), JHCOD(N), JCARA(N), JLUTI(N), JMARQ(N)
C ----------------------------------------------------------------------
      INTEGER          NBLMAX    , NBLUTI    , LONGBL    ,
     +                 KITLEC    , KITECR    , KINDEF    , KIADM    ,
     +                 IITLEC    , IITECR    , NITECR    , KMARQ
      COMMON /IFICJE/  NBLMAX(N) , NBLUTI(N) , LONGBL(N) ,
     +                 KITLEC(N) , KITECR(N) , KINDEF(N) , KIADM(N) ,
     +                 IITLEC(N) , IITECR(N) , NITECR(N) , KMARQ(N)
      LOGICAL          LITLEC
      COMMON /LFICJE/  LITLEC(N)
C
      CHARACTER*1      GENR    , TYPE
      CHARACTER*4      DOCU
      CHARACTER*8      ORIG
      CHARACTER*32     RNOM
      COMMON /KATRJE/  GENR(8) , TYPE(8) , DOCU(2) , ORIG(1) , RNOM(1)
      COMMON /JKATJE/  JGENR(N), JTYPE(N), JDOCU(N), JORIG(N), JRNOM(N)
      INTEGER          NRHCOD    , NREMAX    , NREUTI
      COMMON /ICODJE/  NRHCOD(N) , NREMAX(N) , NREUTI(N)
C
      CHARACTER*2      DN2
      CHARACTER*5      CLASSE
      CHARACTER*8                  NOMFIC    , KSTOUT    , KSTINI
      COMMON /KFICJE/  CLASSE    , NOMFIC(N) , KSTOUT(N) , KSTINI(N) ,
     +                 DN2(N)
      CHARACTER*8      NOMBAS
      COMMON /KBASJE/  NOMBAS(N)
C
      INTEGER          ICLAS ,ICLAOS , ICLACO , IDATOS , IDATCO , IDATOC
      COMMON /IATCJE/  ICLAS ,ICLAOS , ICLACO , IDATOS , IDATCO , IDATOC
C
      CHARACTER *24                     NOMCO
      CHARACTER *32    NOMUTI , NOMOS ,         NOMOC , BL32
      COMMON /NOMCJE/  NOMUTI , NOMOS , NOMCO , NOMOC , BL32
C
      COMMON /IACCED/  IACCE(1)
      COMMON /JIACCE/  JIACCE(N)
      COMMON /KUSADI/  IUSADI(1)
      COMMON /JUSADI/  JUSADI(N)
      INTEGER          LUNDEF,IDEBUG
      COMMON /UNDFJE/  LUNDEF,IDEBUG
C ----------------------------------------------------------------------
      INTEGER          NBENRG, LGENRG, NBENRV, LGENRV, NBENRL, LGENRL
      COMMON /STACOD/  NBENRG, LGENRG, NBENRV, LGENRV, NBENRL, LGENRL
C ----------------------------------------------------------------------
      INTEGER          LIDBAS      , LIDEFF
      PARAMETER      ( LIDBAS = 20 , LIDEFF = 15 )
      CHARACTER*1      KCLAS
      CHARACTER*8      KCOND
      CHARACTER*32     NOMCAR
      CHARACTER*75     CMESS
      INTEGER          IADCAR,IADDAC(2),LGBL
C DEB ------------------------------------------------------------------
      KCOND = COND
      KCLAS = CLAS
      IC = INDEX ( CLASSE , KCLAS )
      ICLAOS = IC
      IF ( IC .EQ. 0 ) THEN
        NOMUTI = 'CLASSE '//KCLAS
        CMESS  = ' FICHIER NON OUVERT OU DEJA LIBERE'
        CALL JVMESS ( 'S' , 'JELIBF01' , CMESS )
      ENDIF
      IF ( KCOND .NE. '        ' .AND. KCOND .NE. 'SAUVE   ' .AND.
     +     KCOND .NE. 'ERREUR  ' .AND. KCOND .NE. 'DETRUIT ' .AND.
     +     KCOND .NE. 'LIBERE  ' )                                 THEN
        NOMUTI = NOMBAS(IC)
        CMESS  = ' CONDITION DE LIBERATION ERRONEE'
        CALL JVMESS ( 'S' , 'JELIBF02' , CMESS )
      ELSE IF ( KCOND .EQ. '        '  ) THEN
        KCOND = KSTOUT(IC)
      ELSE IF ( KCOND .EQ. 'ERREUR  '  ) THEN
        KCOND = 'SAUVE'
      ENDIF
C
C     ----------- DECHARGER TOUTES LES COLLECTIONS -----------------
      DO 10 I = LIDBAS+1 , NREUTI(IC)
        IF ( GENR ( JGENR(IC) + I ) .EQ. 'X' ) THEN
          ICLACO = IC
          IBACOL = IADM( JIADM(IC) + I )
          IF ( IBACOL .NE. 0 ) THEN
            IDATCO = I
            NOMCO = RNOM ( JRNOM(IC) + I )
            CALL JJLIDE ( 'JELIBF' , NOMCO , 2 )
          END IF
        END IF
   10 CONTINUE
C     -- DECHARGER TOUS LES OBJETS SIMPLES Y COMPRIS AVEC $$ -------
      DO 20 I = LIDBAS+1 , NREUTI(IC)
        IADMI = IADM( JIADM(IC) + I )
        IF ( IADMI .NE. 0 ) THEN
          IDATOS = I
          NOMOS  = RNOM ( JRNOM(IC) + I )
          CALL JJLIDE ( 'JELIBF' , NOMOS , 1 )
        END IF
   20 CONTINUE
C     ----------- DECHARGER TOUS LES OBJETS SYSTEME ----------------
      IF ( KCOND .NE. 'LIBERE  ' ) THEN
        NOMCAR = RNOM ( JRNOM(IC) + 1 )
        IADCAR = IADM ( JIADM(IC) + 1 )
        IADACC = IADM ( JIADM(IC) + LIDEFF )
        LACC   = LONO ( JLONO(IC) + LIDEFF )
        IADDAC(1) = IADD ( JIADD(IC) + 2*LIDEFF-1)
        IADDAC(2) = IADD ( JIADD(IC) + 2*LIDEFF  )
        IDB    = IDEBUG
        IDEBUG = 0
        DO 30 I = LIDEFF-1 , 2 , -1
          IADMI = IADM( JIADM(IC) + I )
          IF ( IADMI .NE. 0 ) THEN
            IDATOS = I
            NOMOS  = RNOM ( JRNOM(IC) + I )
            CALL JJLIDE ( 'JELIBF' , NOMOS , 1 )
          END IF
   30   CONTINUE
        DO 32 I = 2 , LIDEFF-1
          IADMI = IADM( JIADM(IC) + I )
          IF ( IADMI .NE. 0 ) THEN
            CALL JJLIBP ( IADMI )
          END IF
   32   CONTINUE
        DO 33 I = LIDBAS+1 , NREUTI(IC)
          IADMI = IADM( JIADM(IC) + I )
          IF ( IADMI .NE. 0 ) THEN
            CALL JJLIBP ( IADMI )
          END IF
   33   CONTINUE
        IDEBUG = IDB
      ENDIF
C
      NBIO = 0
      DO 50 K=1,NBLMAX(IC)
        NBIO = NBIO + IACCE (JIACCE(IC)+K)
 50   CONTINUE
C  
      IF ( KCOND .EQ. 'SAUVE   '  ) THEN
C       ----------- STATISTIQUES DU FICHIER
C       ----------- ACTUALISER CARA
        CARA(JCARA(IC)+1) = NREUTI(IC)
        CARA(JCARA(IC)+4) = NBLUTI(IC)
        CARA(JCARA(IC)+6) = IADD(JIADD(IC)+3)
        CARA(JCARA(IC)+7) = IADD(JIADD(IC)+4)
        IF ( IADCAR.NE. 0 ) THEN
           IDATOS = 1
           CALL JJLIDE ( 'JELIBF' , NOMCAR , 1 )
           IADCAR = 0
        END IF
C       ----------- DECHARGER TAMPON D'ECRITURE ( PETITS OBJETS )
        LGBL = 1024*LONGBL(IC)*LOIS
        IF ( IITECR(IC) .GT. 0 ) THEN
           CALL JXECRB (IC, IITECR(IC), KITECR(IC)+1, LGBL, 0, 0)
        ENDIF
        IF ( LITLEC(IC) ) THEN
           CALL JXECRB (IC, IITLEC(IC), KITLEC(IC)+1, LGBL, 0, 0)
           LITLEC(IC) = .FALSE.
           IITLEC(IC) = 0
        ENDIF
C       ---- ON DECHARGE MAINTENANT LES STATISTIQUES SUR LES ACCES
        IDATOS = LIDEFF
        IACCE (JIACCE(IC)+IADDAC(1)) = IACCE (JIACCE(IC)+IADDAC(1))+1
        NBIO = NBIO + 1
        CALL JXECRO (IC,IADACC,IADDAC,LACC,0,LIDEFF)
        IITECR(IC) = 0
        IF ( LITLEC(IC) ) THEN
          CALL JXECRB ( IC,IITLEC(IC),KITLEC(IC)+1,LGBL,0,0)
        ENDIF
        CALL JJLIBP ( IADACC )
        IADACC = 0
C
C     GLUTE STAT CODE
C
        IF    ( CLAS(1:1) .EQ. 'G' ) THEN
          NBENRG = NBLUTI(IC)
          LGENRG = 1024*LONGBL(IC)*LOIS
        ELSEIF( CLAS(1:1) .EQ. 'V' ) THEN
          NBENRV = NBLUTI(IC)
          LGENRV = 1024*LONGBL(IC)*LOIS
        ELSEIF( CLAS(1:1) .EQ. 'L' ) THEN
          NBENRL = NBLUTI(IC)
          LGENRL = 1024*LONGBL(IC)*LOIS
        ENDIF
      ENDIF
C
      CALL JVDEBM ( 'I' , ' ' , ' ' )
      CALL JVIMPK ( 'L' , 'NOM DE LA BASE                    : ',
     &               1 , NOMBAS(IC))
      CALL JVIMPI ( 'L' , 'NOMBRE D''ENREGISTREMENTS UTILISES : ',
     &               1 , NBLUTI(IC) )
      CALL JVIMPI ( 'L' , 'NOMBRE D''ENREGISTREMENTS MAXIMUM  : ',
     &               1 , NBLMAX(IC) )
      LGBL = 1024*LONGBL(IC)*LOIS
      CALL JVIMPI ( 'L' , 'LONGUEUR D''ENREGISTREMENT (OCTETS): ',
     &               1 , LGBL )
      CALL JVIMPI ( 'L' , 'NOMBRE TOTAL D''ENTREES/SORTIES    : ',
     &               1 , NBIO)
      CALL JVIMPI ( 'L' , 'NOMBRE D''IDENTIFICATEURS UTILISES : ',
     &               1 , NREUTI(IC) )
      CALL JVIMPI ( 'L' , 'TAILLE MAXIMUM DU REPERTOIRE      : ',
     &               1 , NREMAX(IC) )
      CALL JVIMPI ( 'L' , 'TAUX D''UTILISATION DU REPERTOIRE %: ',
     &               1 , (NREUTI(IC)*100)/NREMAX(IC))
      CALL JVFINM
      IF ( KCOND .NE. 'LIBERE  ' ) THEN
        IF ( IADCAR.NE. 0 ) THEN
           IDATOS = 1
           CALL JJLIBP ( IADCAR )
        END IF
        IF ( IADACC.NE. 0 ) THEN
           IDATOS = LIDEFF
           CALL JJLIBP ( IADACC )
        END IF
C       ----------- CLORE LE FICHIER
        IF ( KSTINI(IC) .NE. 'DUMMY   ' ) THEN
          CALL JXFERM ( IC )
        ENDIF
C       ----------- LIBERER PLACE
        CALL JJLIBP ( 1 + KITLEC(IC) / LOIS )
        CALL JJLIBP ( 1 + KITECR(IC) / LOIS )
        IF ( LCRA ) CALL JJLIBP ( KINDEF(IC) )
        CALL JJLIBP ( KIADM (IC) )
        CALL JJLIBP ( KMARQ (IC) )
C
        CLASSE(IC:IC) = ' '
        NOMFIC(IC)    = ' '
        NOMBAS(IC)    = ' '
        KSTOUT(IC)    = ' '
        KSTINI(IC)    = ' '
        DN2(IC)       = ' '
        NRHCOD(IC)    = 0
        NREMAX(IC)    = 0
        NREUTI(IC)    = 0
        LITLEC(IC)    = .FALSE.
        NBLMAX(IC)    = 0
        NBLUTI(IC)    = 0
        LONGBL(IC)    = 0
        KITLEC(IC)    = 0
        KITECR(IC)    = 0
        KINDEF(IC)    = 0
        KIADM(IC)     = 0
        IITLEC(IC)    = 0
        IITECR(IC)    = 0
        NITECR(IC)    = 0
      ENDIF
C FIN ------------------------------------------------------------------
      END
