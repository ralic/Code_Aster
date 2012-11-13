      SUBROUTINE JEIMPR ( UNIT , CLAS , CMESS )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
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
      INTEGER           UNIT  
      CHARACTER*(*)           CLAS , CMESS
C ----------------------------------------------------------------------
C IMPRESSION DU REPERTOIRE D'UNE OU PLUSIEURS CLASSES
C
C IN  UNIT  : NUMERO D'UNITE LOGIQUE ASSOCIE AU FICHIER D'IMPRESSION
C IN  CLAS   : NOM DE LA CLASSE ASSOCIEE ( ' ' POUR TOUTES LES CLASSES )
C IN  CMESS  : MESSAGE D'INFORMATION
C
C ----------------------------------------------------------------------
C-----------------------------------------------------------------------
      INTEGER I ,IIADD ,IIBAS ,IIBDY ,ILONG ,ILONO ,ILTYP 
      INTEGER J ,JCARA ,JDATE ,JDOCU ,JGENR ,JHCOD ,JIADD 
      INTEGER JIADM ,JLONG ,JLONO ,JLTYP ,JLUTI ,JMARQ ,JORIG 
      INTEGER JRNOM ,JTYPE ,K ,N ,NCLA1 ,NCLA2 
C-----------------------------------------------------------------------
      PARAMETER  ( N = 5 )
      COMMON /JIATJE/  JLTYP(N), JLONG(N), JDATE(N), JIADD(N), JIADM(N),
     +                 JLONO(N), JHCOD(N), JCARA(N), JLUTI(N), JMARQ(N)
C
      COMMON /JKATJE/  JGENR(N), JTYPE(N), JDOCU(N), JORIG(N), JRNOM(N)
C
      INTEGER          NRHCOD    , NREMAX    , NREUTI
      COMMON /ICODJE/  NRHCOD(N) , NREMAX(N) , NREUTI(N)
C
      CHARACTER*2      DN2
      CHARACTER*5      CLASSE
      CHARACTER*8                  NOMFIC    , KSTOUT    , KSTINI
      COMMON /KFICJE/  CLASSE    , NOMFIC(N) , KSTOUT(N) , KSTINI(N) ,
     &                 DN2(N)
C ----------------------------------------------------------------------
      CHARACTER*1      KCLAS , CGENR , CTYPE , CLASI
      CHARACTER*32     CRNOM
C DEB ------------------------------------------------------------------

      KCLAS = CLAS ( 1: MIN(1,LEN(CLAS)))
      IF ( UNIT .LE. 0 ) GOTO 9999
      IF ( KCLAS .EQ. ' ' ) THEN
         NCLA1 = 1
         NCLA2 = INDEX ( CLASSE , '$' ) - 1
         IF ( NCLA2 .LT. 0 ) NCLA2 = N
      ELSE
         NCLA1 = INDEX ( CLASSE , KCLAS)
         NCLA2 = NCLA1
      ENDIF
      DO 10 I = NCLA1 , NCLA2
        CLASI = CLASSE(I:I)
        IF ( CLASI .NE. ' ' ) THEN
          WRITE (UNIT,'(4A)' ) ('---------------------',K=1,4)
          WRITE (UNIT,'(2A)' )
     &          '------     CATALOGUE CLASSE ',CLASI     ,
     &          '------    ', CMESS(1:MIN(72,LEN(CMESS)))
          WRITE (UNIT,'(4A)' ) ('---------------------',K=1,4)
           DO 5 J = 1 , NREMAX(I)
             CRNOM = RNOM(JRNOM(I)+J)
             IF ( CRNOM(1:1) .EQ. '?' ) GOTO 5
             IF ( MOD(J,25) .EQ. 1 ) THEN
                WRITE ( UNIT , '(/,A,A/)' )
     &      '--- NUM  -------------- NOM ---------------- G T L- --LONG'
     +      ,'--- -LOTY- -IADD- --------KADM------- --------KDYN-------'
             ENDIF
             CGENR = GENR(JGENR(I)+J)
             CTYPE = TYPE(JTYPE(I)+J)
             ILTYP = LTYP(JLTYP(I)+J)
             ILONG = LONG(JLONG(I)+J)
             ILONO = LONO(JLONO(I)+J)
             IIADD = IADD(JIADD(I)+2*J-1)
             IIBAS = IADM(JIADM(I)+2*J-1)
             IIBDY = IADM(JIADM(I)+2*J  )
             WRITE(UNIT , 1001) J,CRNOM,CGENR,CTYPE,ILTYP,
     +                            ILONG,ILONO,IIADD,IIBAS,IIBDY
    5      CONTINUE
           WRITE ( UNIT , '(/)' )
        ENDIF
   10 CONTINUE
 9999 CONTINUE
 1001 FORMAT(I8,2X,A,'  -',2(A,'-'),I2,1X,I8,1X,I7,I7,I20,I20)
C FIN ------------------------------------------------------------------
      END
