      SUBROUTINE VPFOPR( OPTION, TYPRES, LMASSE, LRAIDE, LDYNAM, OMEMIN,
     &                   OMEMAX, OMESHI, NBFREQ , NPIVOT, OMECOR,
     &                   PRECDC, NBRSSA, NBLAGR, SOLVEU)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C     DETERMINATION D'UN SHIFT ET CALCUL DE LA MATRICE SHIFTEE
C     TROIS OPTIONS SONT POSSIBLES :
C       1) CALCUL DES N PLUS PETITES FREQUENCES DU SYSTEME
C       2) CALCUL DES N FREQUENCES LES PLUS PROCHES D'UNE VALEUR DONNEE
C       3) CALCUL DE TOUTES LES FREQUENCES DANS UNE BANDE DONNEE
C     ------------------------------------------------------------------
C IN  OPTION  : TX : CHOIX DE L'OPTION (PLUS_PETITE, CENTRE, BANDE)
C IN  TYPRES  : TX : TYPE DU CALCUL (DYNAMIQUE OU FLAMBEMENT)
C IN  LMASSE  : IS : DESCRIPTEUR DE LA MATRICE SECOND MEMBRE
C IN  LRAIDE  : IS : DESCRIPTEUR DE LA MATRICE PREMIER MEMBRE
C IN/OUT LDYNAM :IS : POINTEUR SUR LA FACTORISEE DE LA MATRICE DYNAMIQUE
C                    INDUITE PAR L'OPTION
C IN/OUT OMEMIN : R8 : VALEUR INFERIEURE DE LA BANDE DE RECHERCHE
C                      OU VALEUR DE DEPART POUR LES AUTRES OPTIONS
C IN/OUT OMEMAX : R8 : VALEUR SUPERIEURE DE LA BANDE DE RECHERCHE
C    OUT OMESHI : R8 : VALEUR DU SHIFT  DE LA MATRICE DE TRAVAIL
C    OUT NBFREQ : IS : NOMBRE DE FREQUENCES DANS LA BANDE
C    OUT NPIVOT : IS : NOMBRE DE PIVOTS NEGATIFS DE LA MATRICE DE
C                      TRAVAIL FACTORISEE
C IN  OMECOR : R8 : VALEUR DE LA PULSATION AU CARRE DEFINISSANT LES
C                   MODES DE CORPS RIGIDE
C IN  PRECDC : R8 : VALEUR DU DECALAGE DES SHIFTS QUAND LA MATRICE EST
C                   NON INVERSIBLE
C IN  NBRSSA : IS : NOMBRE DE DECALAGES DE SHIFTS AUTORISES
C IN  NBLAGR : IS : NOMBRE DE DDLS DE LAGRANGE
C IN  SOLVEU : K19 : SD SOLVEUR POUR PARAMETRER LE SOLVEUR LINEAIRE
C----------------------------------------------------------------------
C
      IMPLICIT NONE

C PARAMETRES D'APPEL
      INCLUDE 'jeveux.h'
      CHARACTER*(*)              OPTION
      CHARACTER*16               TYPRES
      CHARACTER*19               SOLVEU
      INTEGER                    LMASSE, LRAIDE, LDYNAM,  NBRSSA
      REAL*8                     OMEMIN, OMEMAX, OMESHI, OMECOR, PRECDC
      INTEGER                    NBFREQ, NPIVOT, NIV,    IFM,    NBLAGR


C VARIABLES LOCALES
      CHARACTER*8  K8BID
      CHARACTER*16 CH16
      INTEGER      IDET
      REAL*8       DET
      REAL*8       OMGMIN, OMGMAX, OMGSHI
      REAL*8       FREQOM
      INTEGER      NBESSA, IER, NBFMIN, NBFMAX

C     ------------------------------------------------------------------
C     ------------------------ OPTION CENTRE ---------------------------
C     ------------------------------------------------------------------

      CALL INFNIV(IFM,NIV)
C --- POUR NE PAS DECLANCHER INUTILEMENT LE CALCUL DU DETERMINANT DANS
C     VPSTUR
      IDET=-9999
      WRITE(IFM,900) OPTION
      IF ( OPTION .EQ. 'CENTRE'  ) THEN

         OMGSHI = OMEMIN
         NBESSA = 0

 10      CONTINUE

         CALL VPSTUR(LRAIDE,OMGSHI,LMASSE,LDYNAM,DET,
     &               IDET,NPIVOT,IER,SOLVEU)
         IF (IER .NE. 0 ) THEN
            IF (ABS(OMGSHI) .LT. OMECOR) THEN
               OMGSHI = OMECOR
               IF (NIV .GE. 1) THEN
                  WRITE(IFM,1300) OMGSHI
               ENDIF
            ELSE
              IF (OMGSHI .GT. 0.D0 ) THEN
                 OMGSHI   = (1.D0+PRECDC) * OMGSHI
              ELSE
                 OMGSHI   = (1.D0-PRECDC) * OMGSHI
              ENDIF
              IF (NIV .GE. 1) THEN
                 WRITE(IFM,1400) (PRECDC*100.D0)
                 WRITE(IFM,1500) OMGSHI
              ENDIF
            ENDIF
            NBESSA = NBESSA + 1
            IF (NBESSA .LE. NBRSSA) THEN
               GOTO 10
            ELSE
               CALL U2MESS('F','ALGELINE3_68')
               CALL VPSTUR(LRAIDE,OMGSHI,LMASSE,LDYNAM,DET,IDET,
     &                     NPIVOT,IER,SOLVEU)
            ENDIF
         ENDIF
         OMESHI = OMGSHI
         IF (NIV .GE. 1) THEN
           IF (TYPRES .EQ. 'DYNAMIQUE') THEN
             WRITE (IFM,1000) FREQOM(OMESHI)
           ELSE
             WRITE (IFM,1001) OMESHI
           ENDIF
         ENDIF

C     ------------------------------------------------------------------
C     ------------------------ OPTION BANDE ----------------------------
C     ------------------------------------------------------------------

      ELSE IF ( OPTION .EQ. 'BANDE' ) THEN

         OMGMIN = OMEMIN
         NBESSA = 0

 21      CONTINUE

         CALL VPSTUR( LRAIDE,OMGMIN,LMASSE,LDYNAM,DET,
     &                IDET,NBFMIN,IER,SOLVEU)
         IF (IER .NE. 0) THEN
            IF (ABS(OMGMIN) .LT. OMECOR) THEN
               OMGMIN = - OMECOR
               IF (NIV .GE. 1) THEN
                   WRITE(IFM,1600) OMGMIN
               ENDIF
            ELSE
               IF (OMGMIN .GE. 0.D0) THEN
                  OMGMIN = (1.D0-PRECDC) * OMGMIN
               ELSE
                  OMGMIN = (1.D0+PRECDC) * OMGMIN
               ENDIF
               IF (NIV .GE. 1) THEN
                  WRITE(IFM,1700) (PRECDC*100.D0),OMGMIN
               ENDIF
            ENDIF
            NBESSA = NBESSA + 1
            IF (NBESSA .LE. NBRSSA) THEN
               GOTO 21
            ELSE
               CALL U2MESS('A','ALGELINE3_66')
               CALL VPSTUR(LRAIDE,OMGMIN,LMASSE,LDYNAM,DET,IDET,
     &                     NBFMIN,IER,SOLVEU)
            ENDIF
         ENDIF
         OMEMIN = OMGMIN
         OMGMAX = OMEMAX
         NBESSA = 0

 22      CONTINUE

         CALL VPSTUR( LRAIDE,OMGMAX,LMASSE,LDYNAM,DET,
     &                IDET,NBFMAX,IER,SOLVEU)
         IF (IER .NE. 0) THEN
            IF (ABS(OMGMAX) .LT. OMECOR) THEN
               OMGMAX = OMECOR
               IF (NIV .GE. 1) THEN
                   WRITE(IFM,1800) OMGMAX
               ENDIF
            ELSE
               IF (OMGMAX .GT. 0.D0 ) THEN
                  OMGMAX   = (1.D0+PRECDC) * OMGMAX
               ELSE
                  OMGMAX   = (1.D0-PRECDC) * OMGMAX
               ENDIF
               IF (NIV .GE. 1) THEN
                   WRITE(IFM,1900) (PRECDC*100.D0),OMGMAX
               ENDIF
            ENDIF
            NBESSA = NBESSA + 1
            IF (NBESSA .LE. NBRSSA) THEN
               GOTO 22
            ELSE
               CALL U2MESS('A','ALGELINE3_67')
               CALL VPSTUR(LRAIDE,OMGMAX,LMASSE,LDYNAM,DET,IDET,
     &                    NBFMAX,IER,SOLVEU)
            ENDIF
         ENDIF
         OMEMAX = OMGMAX
         K8BID=' '         
         CALL VPECST(IFM,TYPRES,OMGMIN,OMGMAX,NBFMIN,NBFMAX,
     &               NBFREQ,NBLAGR,'R',K8BID,0.D0,DCMPLX(0.D0,0.D0))

C        --- CENTRAGE DE L INTERVALLE ---

         OMGSHI = ( OMGMAX + OMGMIN ) * 0.5D0
         NBESSA = 0

 23      CONTINUE

         CALL VPSTUR(LRAIDE,OMGSHI,LMASSE,LDYNAM,DET,
     &               IDET,NPIVOT,IER,SOLVEU)
         IF (IER .NE. 0) THEN
            IF (ABS(OMGSHI) .LT. OMECOR) THEN
               OMGSHI = OMECOR
               IF (NIV .GE. 1) THEN
                   WRITE(IFM,2000) OMGSHI
               ENDIF
            ELSE
               IF (OMGSHI .GT. 0.D0 ) THEN
                  OMGSHI   = (1.D0-PRECDC) * OMGSHI
               ELSE
                  OMGSHI   = (1.D0+PRECDC) * OMGSHI
               ENDIF
               IF (NIV .GE. 1) THEN
                  WRITE(IFM,2100) (PRECDC*100.D0),OMGSHI
               ENDIF
            ENDIF
            NBESSA = NBESSA + 1
            IF (NBESSA .LE. NBRSSA) THEN
               GOTO 23
            ELSE
               CALL U2MESS('F','ALGELINE3_66')
               CALL VPSTUR(LRAIDE,OMGSHI,LMASSE,LDYNAM,DET,IDET,
     &                     NPIVOT,IER,SOLVEU)
            ENDIF
         ENDIF
         OMESHI = OMGSHI
         IF (NIV .GE. 1) THEN
            IF (TYPRES .EQ. 'DYNAMIQUE') THEN
               WRITE(IFM,2200) FREQOM(OMGMIN)
               WRITE(IFM,2300) FREQOM(OMGMAX)
               WRITE(IFM,1000) FREQOM(OMESHI)
            ELSE
               WRITE(IFM,2201) OMGMIN
               WRITE(IFM,2301) OMGMAX
               WRITE(IFM,1001) OMESHI
            ENDIF
         ENDIF

C     ------------------------------------------------------------------
C     ------------------------ OPTION PLUS_PETITE OU TOUT -------------
C     ------------------------------------------------------------------

      ELSE IF ((OPTION.EQ.'PLUS_PETITE').OR.(OPTION.EQ.'TOUT')) THEN

         OMGSHI = 0.D0
         NBESSA = 0

 30      CONTINUE

         CALL VPSTUR(LRAIDE,OMGSHI,LMASSE,LDYNAM,DET,
     &               IDET,NPIVOT,IER,SOLVEU)
         IF (IER .NE. 0) THEN
            IF (ABS(OMGSHI) .LT. OMECOR) THEN
               OMGSHI = - OMECOR
               IF (NIV .GE. 1) THEN
                  WRITE(IFM,1800) OMGSHI
               ENDIF
            ELSE
               IF (OMGSHI .GT. 0.D0 ) THEN
                  OMGSHI   = (1.D0-PRECDC) * OMGSHI
               ELSE
                  OMGSHI   = (1.D0+PRECDC) * OMGSHI
               ENDIF
               IF (NIV .GE. 1) THEN
                  WRITE(IFM,2400) (PRECDC*100.D0),OMGSHI
               ENDIF
            ENDIF
            NBESSA = NBESSA + 1
            IF (NBESSA .LE. NBRSSA) THEN
               GOTO 30
            ELSE
               CALL U2MESS('F','ALGELINE3_68')
               CALL VPSTUR(LRAIDE,OMGSHI,LMASSE,LDYNAM,DET,IDET,
     &                     NPIVOT,IER,SOLVEU)
            ENDIF
         ENDIF
         OMESHI = OMGSHI
         IF (NIV .GE. 1) THEN
            IF (TYPRES .EQ. 'DYNAMIQUE') THEN
              WRITE(IFM,1000) FREQOM(OMESHI)
            ELSE
              WRITE(IFM,1001) OMESHI
            ENDIF
         ENDIF

C     ------------------------------------------------------------------
C     ------------------------ OPTION NON CONNUE -----------------------
C     ------------------------------------------------------------------

      ELSE
         CH16 = OPTION
         CALL U2MESK('F','ALGELINE3_69',1,CH16)
      ENDIF

      IF (NIV .GE. 1) THEN
        WRITE(IFM,1200)
      ENDIF

C     -----------------------------FORMAT------------------------------
  900 FORMAT('L''OPTION CHOISIE EST:',1X,A,/)
 1000 FORMAT('LA VALEUR DE DECALAGE EN FREQUENCE EST : ',1PE12.5)
 1001 FORMAT('LA VALEUR DE DECALAGE CHARGE CRITIQUE EST : ',1PE12.5)
 1200 FORMAT (72('-'),/)
 1300 FORMAT('LA VALEUR DE DECALAGE (OMEGA2)EST INFERIEURE A LA VALEUR '
     &      ,'DE CORPS RIGIDE ON LA MODIFIE, ELLE DEVIENT:',1X,1PE12.5)
 1400 FORMAT('ON AUGMENTE LA VALEUR DE DECALAGE DE: ',1PE12.5,
     &       'POUR CENT')
 1500 FORMAT('LA VALEUR CENTRALE DEVIENT: ',1PE12.5)
 1600 FORMAT('LA VALEUR MINIMALE EST INFERIEURE A LA VALEUR ',
     &       'DE CORPS RIGIDE ON LA MODIFIE, ELLE DEVIENT: ',1PE12.5)
 1700 FORMAT('ON DIMINUE LA VALEUR MINIMALE DE: ',1PE12.5,' POURCENT',/,
     &        'LA VALEUR MINIMALE DEVIENT: ',6X,1PE12.5)
 1800 FORMAT('LA VALEUR MAXIMALE EST INFERIEURE A LA VALEUR ',
     &       'DE CORPS RIGIDE ON LA MODIFIE, ELLE DEVIENT: ',1PE12.5)
 1900 FORMAT('ON AUGMENTE LA VALEUR MAXIMALE DE: ',1PE12.5,' POURCENT',/
     &       ,'LA VALEUR MAXIMALE DEVIENT:',8X,1PE12.5,/)
 2000 FORMAT('LA VALEUR DE DECALAGE EST INFERIEURE A LA VALEUR ',
     &       ' DE CORPS RIGIDE ON LA MODIFIE, ELLE DEVIENT: ',1PE12.5)
 2100 FORMAT('ON MODIFIE LA VALEUR DE DECALAGE DE: ',1PE12.5,
     &      'POURCENT',/,'LA VALEUR DE DECALAGE DEVIENT: ',1PE12.5)
 2200 FORMAT('VALEUR_MIN EN FREQUENCE EST :   ',1PE12.5)
 2300 FORMAT('VALEUR_MAX EN FREQUENCE EST :   ',1PE12.5)

 2201 FORMAT('VALEUR_MIN EN CHARGE CRITIQUE EST :   ',1PE12.5)
 2301 FORMAT('VALEUR_MAX EN CHARGE CRITIQUE EST :   ',1PE12.5)
 2400 FORMAT('ON DIMINUE LA VALEUR DE DECALAGE DE: ',1PE12.5,
     &       ' POURCENT',/, 'ELLE DEVIENT: ',26X,1PE12.5)
C     ------------------------------------------------------------------

      END
