      SUBROUTINE AVDOWH( NBVEC, NBORDR, NOMMAT, NOMCRI, NCYCL, GDEQ,
     &                   GRDVIE, FORVIE,POST, DOMEL, NRUPT )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C RESPONSABLE ANGLES J.ANGLES
      IMPLICIT     NONE
      INCLUDE 'jeveux.h'
      INTEGER      NBVEC, NBORDR, NCYCL(NBVEC)
      REAL*8       GDEQ(NBVEC*NBORDR)
      REAL*8       NRUPT(NBVEC*NBORDR), DOMEL(NBVEC*NBORDR)
      LOGICAL      POST
      CHARACTER*8  NOMMAT, GRDVIE
      CHARACTER*16 NOMCRI, FORVIE
C ----------------------------------------------------------------------
C BUT: CALCULER LE DOMMAGE ELEMENTAIRE A PARTIR D'UNE COURBE
C      GRANDEUR EQ - VIE POUR TOUS LES CYCLES
C      ELEMETAIRES DE CHAQUE VECTEUR NORMAL.
C ----------------------------------------------------------------------
C ARGUMENTS :
C  NBVEC    IN   I  : NOMBRE DE VECTEURS NORMAUX.
C  NBORDR   IN   I  : NOMBRE DE NUMEROS D'ORDRE.
C  NOMMAT   IN   K  : NOM DU MATERIAU.
C  NOMCRI   IN   K  : NOM DU CRITERE.
C  NCYCL    IN   I  : NOMBRE DE CYCLES ELEMENTAIRES POUR TOUS LES
C                     VECTEURS NORMAUX.
C  GDEQ     IN   R  : VECTEUR CONTENANT LES VALEURS DE LA GRANDEUR
C                     EQUIVALENTE (SIGEQ OU EPSEQ), POUR TOUS LES SOUS
C                     CYCLES DE CHAQUE VECTEUR NORMAL.
C  DOMEL    OUT  R  : VECTEUR CONTENANT LES VALEURS DES DOMMAGES
C                     ELEMENTAIRES, POUR TOUS LES SOUS CYCLES
C                     DE CHAQUE VECTEUR NORMAL.
C  NRUPT    OUT  R  : VECTEUR CONTENANT LES NOMBRES DE CYCLES
C                     ELEMENTAIRES, POUR TOUS LES SOUS CYCLES
C                     DE CHAQUE VECTEUR NORMAL.
C ----------------------------------------------------------------------
C     ------------------------------------------------------------------
      INTEGER        IVECT, ICYCL, ADRS, I
      REAL*8         R8MAEM
      INTEGER        ICODRE
      CHARACTER*16   PHENOM, KBID
      CHARACTER*8    NOMGRD
      LOGICAL        LIMIT
C     ------------------------------------------------------------------

C234567                                                              012

      CALL JEMARQ()

C INITITIALISATION
       DO 100 I = 1,NBVEC*NBORDR
         DOMEL(I) = 0
100   CONTINUE

      IF (.NOT. POST) THEN
         CALL RCCOME( NOMMAT, 'FATIGUE', PHENOM, ICODRE )
         IF ( ICODRE .EQ. 1 ) CALL U2MESS('F','FATIGUE1_24')
      ENDIF

      IF ( NOMCRI(1:16) .EQ. 'FATESOCI_MODI_AV' ) THEN
         CALL RCPARE( NOMMAT, 'FATIGUE', 'MANSON_C', ICODRE )
         IF (ICODRE .EQ. 1 ) THEN
            CALL U2MESK('F','FATIGUE1_89',1,NOMCRI(1:16))
         ENDIF

         DO 10 IVECT=1, NBVEC
            DO 20 ICYCL=1, NCYCL(IVECT)
               ADRS = (IVECT-1)*NBORDR + ICYCL

               CALL RCVALE(NOMMAT,'FATIGUE',1,'EPSI    ',GDEQ(ADRS),
     &                  1,'MANSON_C',NRUPT(ADRS),ICODRE,1)

               CALL LIMEND( NOMMAT, GDEQ(ADRS), 'MANSON_C',KBID, LIMIT )
               IF (LIMIT) THEN
                  NRUPT(ADRS)=R8MAEM()
               ELSE
                  CALL RCVALE(NOMMAT,'FATIGUE',1,'EPSI    ',
     &               GDEQ(ADRS),1,'MANSON_C',NRUPT(ADRS),ICODRE,1)
               ENDIF

               DOMEL(ADRS) = 1.0D0/NRUPT(ADRS)
               NRUPT(ADRS) = NINT(NRUPT(ADRS))

 20         CONTINUE
 10      CONTINUE

      ELSEIF (( NOMCRI(1:14) .EQ. 'MATAKE_MODI_AV' ) .OR.
     &         ( NOMCRI(1:16) .EQ. 'DANG_VAN_MODI_AV' )) THEN
         CALL RCPARE( NOMMAT, 'FATIGUE', 'WOHLER', ICODRE )
         IF (ICODRE .EQ. 1 ) THEN
            CALL U2MESK('F','FATIGUE1_90',1,NOMCRI(1:16))
         ENDIF

         DO 30 IVECT=1, NBVEC
            DO 40 ICYCL=1, NCYCL(IVECT)
               ADRS = (IVECT-1)*NBORDR + ICYCL

               CALL LIMEND( NOMMAT, GDEQ(ADRS), 'WOHLER', KBID, LIMIT)
               IF (LIMIT) THEN
                  NRUPT(ADRS)=R8MAEM()
               ELSE
                  CALL RCVALE(NOMMAT,'FATIGUE',1,'SIGM    ',
     &               GDEQ(ADRS),1,'WOHLER  ',NRUPT(ADRS),ICODRE,1)
               ENDIF

               DOMEL(ADRS) = 1.0D0/NRUPT(ADRS)
               NRUPT(ADRS) = NINT(NRUPT(ADRS))

 40         CONTINUE
 30      CONTINUE

      ELSEIF (NOMCRI(1:7) .EQ. 'FORMULE') THEN

         DO 50 IVECT=1, NBVEC
            DO 60 ICYCL=1, NCYCL(IVECT)
               ADRS = (IVECT-1)*NBORDR + ICYCL

               CALL LIMEND( NOMMAT, GDEQ(ADRS), GRDVIE, FORVIE, LIMIT)

               IF (LIMIT) THEN
                  NRUPT(ADRS)=R8MAEM()
               ELSE

                  IF (GRDVIE(1:6) .EQ. 'WOHLER') THEN
                     NOMGRD = 'SIGM    '
                     GRDVIE(7:8) = '  '

                     CALL RCVALE(NOMMAT,'FATIGUE',1,NOMGRD,
     &                  GDEQ(ADRS),1,GRDVIE,NRUPT(ADRS),ICODRE,1)
                  ENDIF

                  IF (GRDVIE(1:8) .EQ. 'MANSON_C') THEN
                     NOMGRD = 'EPSI    '
                     CALL RCVALE(NOMMAT,'FATIGUE',1,NOMGRD,
     &                  GDEQ(ADRS),1,GRDVIE,NRUPT(ADRS),ICODRE,1)

                  ENDIF

                  IF (GRDVIE(1:8) .EQ.'FORM_VIE') THEN
                     CALL RENRFA(FORVIE,GDEQ(ADRS),NRUPT(ADRS),ICODRE)
                  ENDIF

                  DOMEL(ADRS) = 1.0D0/NRUPT(ADRS)
                  NRUPT(ADRS) = NINT(NRUPT(ADRS))

               ENDIF


 60         CONTINUE
 50      CONTINUE



      ENDIF

      CALL JEDEMA()

      END
