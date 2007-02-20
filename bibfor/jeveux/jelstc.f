      SUBROUTINE JELSTC ( CLAS , SOUCH , IPOS , MAXVAL , KLST , NBVAL )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 19/02/2007   AUTEUR LEFEBVRE J-P.LEFEBVRE 
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
      CHARACTER*(*)       CLAS , SOUCH                 , KLST(*)
      INTEGER                            IPOS , MAXVAL        , NBVAL
C ----------------------------------------------------------------------
C  BUT : RETROUVER LES NOMS DES OBJETS DONT LE NOM CONTIENT UNE CHAINE
C        DE CARATERES DONNEE, PRESENTS SUR UNE BASE JEVEUX.
C
C  IN  : CLAS : NOM DE LA BASE : 'G', 'V', ..( ' ' -> TOUTES LES BASES )
C  IN  : SOUCH: CHAINE DE CARACTERES A CHERCHER
C  IN  : IPOS : POSITION DU DEBUT DE LA CHAINE
C               SI IPOS=0 ON REND TOUS LES NOMS
C  IN  : MAXVAL: DIMENSION DU TABLEAU KLST
C  OUT : KLST  : TABLEAU DE K24 CONTENANT LES NOMS TROUVES
C  OUT : NBVAL : NOMBRE DE NOMS TROUVES (NBVAL = -NBVAL SI < MAXVAL)
C
C ----------------------------------------------------------------------
      PARAMETER  ( N = 5 )
C
      CHARACTER*1      GENR    , TYPE
      CHARACTER*4      DOCU
      CHARACTER*8      ORIG
      CHARACTER*32     RNOM
      COMMON /KATRJE/  GENR(8) , TYPE(8) , DOCU(2) , ORIG(1) , RNOM(1)
      COMMON /JKATJE/  JGENR(N), JTYPE(N), JDOCU(N), JORIG(N), JRNOM(N)
      LOGICAL          TROUVE
      CHARACTER *6     PGMA
      COMMON /KAPPJE/  PGMA
      CHARACTER*2      DN2
      CHARACTER*5      CLASSE
      CHARACTER*8                  NOMFIC    , KSTOUT    , KSTINI
      COMMON /KFICJE/  CLASSE    , NOMFIC(N) , KSTOUT(N) , KSTINI(N) ,
     &                 DN2(N)
      INTEGER          NRHCOD    , NREMAX    , NREUTI
      COMMON /ICODJE/  NRHCOD(N) , NREMAX(N) , NREUTI(N)
C     ==================================================================
      INTEGER          NCLA1,NCLA2,IC,J
      CHARACTER*75     CMESS
      CHARACTER*32     CRNOM
      CHARACTER*1      KCLAS
C     ==================================================================
      PGMA = 'JELSTC'
      L = LEN ( SOUCH )
      IF ( IPOS + L .GT. 25 .OR. IPOS .LT. 0 .OR. L .EQ. 0) THEN
         CMESS = ' LONGUEUR OU POSITION DE LA SOUS-CHAINE '//SOUCH//
     &           ' INVALIDE'
         CALL U2MESK('F','JEVEUX_01',1,CMESS)
      ENDIF
      KCLAS  = CLAS (1:MIN(1,LEN(CLAS)))
      IF ( KCLAS .EQ. ' ' ) THEN
         NCLA1 = 1
         NCLA2 = INDEX ( CLASSE , '$' ) - 1
         IF ( NCLA2 .LT. 0 ) NCLA2 = N
      ELSE
         NCLA1 = INDEX ( CLASSE , KCLAS)
         NCLA2 = NCLA1
      ENDIF
      NBL = 0
      DO 100 IC = NCLA1 , NCLA2
        DO 150 J = 1 , NREMAX(IC)
          CRNOM = RNOM(JRNOM(IC)+J)
          IF ( CRNOM(1:1) .EQ. '?' .OR.
     &         CRNOM(25:32) .NE. '        ' ) GOTO 150
          IF ( IPOS .EQ. 0 ) THEN
            TROUVE=.TRUE.
          ELSE IF ( SOUCH .EQ. CRNOM(IPOS:IPOS+L-1) ) THEN
            TROUVE=.TRUE.
          ELSE
            TROUVE=.FALSE.
          END IF
          IF ( TROUVE ) THEN
             NBL = NBL + 1
             IF ( NBL .LE. MAXVAL )  THEN
               KLST(NBL) =  CRNOM(1:24)
             END IF
          ENDIF
 150    CONTINUE
 100  CONTINUE
      IF ( NBL .GT. MAXVAL ) THEN
        NBVAL = -NBL
      ELSE
        NBVAL =  NBL
      ENDIF
C
      END
