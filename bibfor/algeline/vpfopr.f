      SUBROUTINE VPFOPR( OPTION, TYPRES, LMASSE, LRAIDE, LDYNAM, OMEMIN,
     &                   OMEMAX, OMESHI, NBFREQ , NPIVOT, OMECOR,
     &                   PRECSH, NBRSSA, NBLAGR, SOLVEU, DET, IDET)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 24/09/2012   AUTEUR BOITEAU O.BOITEAU 
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
C     DETERMINATION DE SHIFT(S), D'UNE MATRICE SHIFTEE, DE SA FACTORISEE
C     DU NBRE DE PIVOTS NEGATIFS (POUR TEST DE STURM) VOIRE DU NBRE
C     DE FREQ DANS UNE BANDE.
C     POUR ETAPE DE PRETRAITEMENTS DE MODE_ITER_SIMULT
C     OPTION='CENTRE' --> OUTPUT: MATRICE SHIFTEE + SA FACTORISEE +
C                         NPIVOT(1) + OMESHI
C     OPTION='BANDE'  --> OUTPUT: MATRICE SHIFTEE + SA FACTORISEE +
C                         NPIVOT(1) + NBFREQ + OMEMIN + OMEMAX +
C                         AFFICHAGES VPECST
C     OPTION='PLUS_PETITE'/'TOUT' --> OUTPUT: MATRICE SHIFTEE + SA FACTO
C                         RISEE + NPIVOT(1) + OMESHI
C
C     POUR ETAPE DE POST_TRAITEMENTS DE MODE_ITER_SIMULT
C     OPTION='STURM'  --> OUTPUT: NBFREQ + OMEMIN + OMEMAX + PAS
C                         AFFICHAGES VPECST
C
C     POUR INFO_MODE
C     OPTION='STURMA' --> OUTPUT: NBFREQ + OMEMIN + OMEMAX +
C                         AFFICHAGES VPECST
C
C     POUR ETAPE PRETRAITEMENT DE MODE_ITER_INV (AJUSTE/SEPARE)
C     OPTION='STURMAD' --> OUTPUT: NBFREQ + OMEMIN + OMEMAX +
C                     AFFICHAGES VPECST + DET(2) + IDET(2) + NPIVOT(2)
C     ------------------------------------------------------------------
C IN  OPTION  : TX : CHOIX DE L'OPTION (PLUS_PETITE,CENTRE,BANDE,STURM)
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
C    OUT NPIVOT : IS : VECTEUR NOMBRE DE PIVOTS NEGATIFS DE LA MATRICE
C                      DE TRAVAIL FACTORISEE
C IN  OMECOR : R8 : VALEUR DE LA PULSATION AU CARRE DEFINISSANT LES
C                   MODES DE CORPS RIGIDE
C IN  PRECSH : R8 : VALEUR DU DECALAGE DES SHIFTS QUAND LA MATRICE EST
C                   NON INVERSIBLE (CALC_FREQ/PREC_SHIFT)
C IN  NBRSSA : IS : NOMBRE DE DECALAGES DE SHIFTS AUTORISES
C IN  NBLAGR : IS : NOMBRE DE DDLS DE LAGRANGE
C IN  SOLVEU : K19 : SD SOLVEUR POUR PARAMETRER LE SOLVEUR LINEAIRE
C OUT  DET   : R8  : VECTEUR DES DEUX MANTISSES DE DETERMINANT
C OUT  IDET  : IS  : IDEM SUR LES EXPOSANTS
C----------------------------------------------------------------------
C
      IMPLICIT NONE

C PARAMETRES D'APPEL
      INCLUDE 'jeveux.h'
      CHARACTER*(*) OPTION
      CHARACTER*16  TYPRES
      CHARACTER*19  SOLVEU
      INTEGER       LMASSE,LRAIDE,LDYNAM,NBRSSA
      REAL*8        OMEMIN,OMEMAX,OMESHI,OMECOR,PRECSH,DET(2)
      INTEGER       NBFREQ,NPIVOT(2),NBLAGR,IDET(2)


C VARIABLES LOCALES
      CHARACTER*1  TYPEP
      CHARACTER*8  K8BID
      CHARACTER*16 CH16
      INTEGER      NIV,IFM,NBESSA,IER,NBFMIN,NBFMAX,IBID
      REAL*8       VALR,OMGMIN,OMGMAX,OMGSHI,FREQOM,RBID,PREC,OMGDEC
      LOGICAL      CALDET,LDYN      

      CALL INFNIV(IFM,NIV)
      IDET=0
      DET(1)=0.D0
      DET(2)=0.D0
      IDET(1)=0
      IDET(2)=0
      NPIVOT(1)=0
      NPIVOT(2)=0
      IF (OPTION.EQ.'STURMAD') THEN
        CALDET=.TRUE.
      ELSE
        CALDET=.FALSE.
      ENDIF
      IF (TYPRES.EQ.'DYNAMIQUE') THEN
        LDYN=.TRUE.
      ELSE
        LDYN=.FALSE.
      ENDIF
      IF (OPTION(1:5).NE.'STURM') WRITE(IFM,900)OPTION

C     ------------------------------------------------------------------
C     ------------------------ OPTION CENTRE ---------------------------
C     ------------------------------------------------------------------

      IF (OPTION.EQ.'CENTRE') THEN

         OMGSHI = OMEMIN
         NBESSA = 0
         PREC=PRECSH
 10      CONTINUE
         IER=0
         CALL VPSTUR(LRAIDE,OMGSHI,LMASSE,LDYNAM,RBID,
     &               IBID,NPIVOT(1),IER,SOLVEU,.FALSE.,.TRUE.)
         IF (IER.NE.0) THEN
            NBESSA= NBESSA+1
            IF (NBESSA.LE.NBRSSA) THEN
              IF (ABS(OMGSHI).LT.OMECOR) THEN
                OMGSHI=OMECOR
                IF (LDYN) THEN
                  VALR=FREQOM(OMGSHI)
                ELSE
                  VALR=OMGSHI
                ENDIF
                IF (NIV.GE.1) WRITE(IFM,1300)VALR
C --- CE N'EST PLUS LA PEINE DE DECALER, C'EST INUTILE
                NBESSA=NBRSSA
              ELSE
                OMGDEC=SIGN(1.D0,OMGSHI)*MAX(OMECOR,PREC*OMGSHI)
                OMGSHI=OMGSHI+OMGDEC
                IF (LDYN) THEN
                  VALR=FREQOM(OMGSHI)
                ELSE
                  VALR=OMGSHI
                ENDIF
                IF (NIV.GE.1) THEN
                  WRITE(IFM,1400)(PREC*100.D0)
                  WRITE(IFM,1500)VALR
                ENDIF
                PREC=2.D0*PREC
              ENDIF
              GOTO 10
            ELSE
              IF (LDYN) THEN
                VALR=FREQOM(OMGSHI)
              ELSE
                VALR=OMGSHI
              ENDIF
              CALL U2MESG('F', 'ALGELINE3_65',0,' ',0,0,1,VALR)
            ENDIF

         ENDIF
         OMESHI=OMGSHI
         IF (NIV.GE.1) THEN
           IF (LDYN) THEN
             WRITE (IFM,1000)FREQOM(OMESHI)
           ELSE
             WRITE (IFM,1001)OMESHI
           ENDIF
         ENDIF

C     ------------------------------------------------------------------
C     ------------------------ OPTION BANDE OU STURM** -----------------
C     ------------------------------------------------------------------

      ELSE IF ((OPTION.EQ.'BANDE').OR.(OPTION(1:5).EQ.'STURM')) THEN
         
         OMGMIN = OMEMIN
         NBESSA = 0
         PREC=PRECSH
 21      CONTINUE
         IER=0
         CALL VPSTUR(LRAIDE,OMGMIN,LMASSE,LDYNAM,DET(1),
     &               IDET(1),NPIVOT(1),IER,SOLVEU,CALDET,.FALSE.)
         NBFMIN=NPIVOT(1)
         IF (IER.NE.0) THEN
           NBESSA=NBESSA+1
           IF (NBESSA.LE.NBRSSA) THEN
             IF (ABS(OMGMIN).LT.OMECOR) THEN
               OMGMIN=-OMECOR
               IF (LDYN) THEN
                 VALR=FREQOM(OMGMIN)
               ELSE
                 VALR=OMGMIN
               ENDIF
               IF (NIV.GE.1) WRITE(IFM,1600)VALR
C --- CE N'EST PLUS LA PEINE DE DECALER, C'EST INUTILE
                NBESSA=NBRSSA
             ELSE
               OMGDEC=-SIGN(1.D0,OMGMIN)*MAX(OMECOR,PREC*OMGMIN)
               OMGMIN=OMGMIN+OMGDEC
               IF (LDYN) THEN
                 VALR=FREQOM(OMGMIN)
               ELSE
                 VALR=OMGMIN
               ENDIF
               IF (NIV.GE.1) WRITE(IFM,1700)(PREC*100.D0),VALR
               PREC=2.D0*PREC
             ENDIF
             GOTO 21
           ELSE
             CALL U2MESS('A','ALGELINE3_66')
           ENDIF
         ENDIF
         OMEMIN=OMGMIN

         OMGMAX=OMEMAX
         NBESSA=0
         PREC=PRECSH
 22      CONTINUE
         IER=0
         CALL VPSTUR(LRAIDE,OMGMAX,LMASSE,LDYNAM,DET(2),
     &               IDET(2),NPIVOT(2),IER,SOLVEU,CALDET,.FALSE.)
         NBFMAX=NPIVOT(2)
         IF (IER.NE.0) THEN
           NBESSA=NBESSA+1
           IF (NBESSA.LE.NBRSSA) THEN
             IF (ABS(OMGMAX).LT.OMECOR) THEN
               OMGMAX=OMECOR
               IF (LDYN) THEN
                 VALR=FREQOM(OMGMAX)
               ELSE
                 VALR=OMGMAX
               ENDIF
               IF (NIV.GE.1) WRITE(IFM,1800)VALR
C --- CE N'EST PLUS LA PEINE DE DECALER, C'EST INUTILE
                NBESSA=NBRSSA
             ELSE
               OMGDEC=SIGN(1.D0,OMGMAX)*MAX(OMECOR,PREC*OMGMAX)
               OMGMAX=OMGMAX+OMGDEC
               IF (LDYN) THEN
                 VALR=FREQOM(OMGMAX)
               ELSE
                 VALR=OMGMAX
               ENDIF
               IF (NIV.GE.1) WRITE(IFM,1900)(PREC*100.D0),VALR
               PREC=2.D0*PREC
             ENDIF
             GOTO 22
           ELSE
             CALL U2MESS('A','ALGELINE3_67')
           ENDIF
         ENDIF
         OMEMAX=OMGMAX
         K8BID=' '
         IF ((OPTION.EQ.'BANDE').OR.(OPTION.EQ.'STURMA')
     &        .OR.(OPTION.EQ.'STURMAD')) THEN
           TYPEP='R'
         ELSE IF (OPTION.EQ.'STURM') THEN
           TYPEP='S'
         ENDIF         
         CALL VPECST(IFM,TYPRES,OMGMIN,OMGMAX,NBFMIN,NBFMAX,
     &               NBFREQ,NBLAGR,TYPEP,K8BID,0.D0,DCMPLX(0.D0,0.D0))

         IF (OPTION.EQ.'BANDE') THEN
C          --- CENTRAGE DE L INTERVALLE ---
           OMGSHI=(OMGMAX+OMGMIN)*0.5D0
           NBESSA=0
           PREC=PRECSH
 23        CONTINUE
           IER=0
           CALL VPSTUR(LRAIDE,OMGSHI,LMASSE,LDYNAM,RBID,
     &                 IBID,NPIVOT(1),IER,SOLVEU,.FALSE.,.TRUE.)
           IF (IER.NE.0) THEN
             NBESSA=NBESSA+1
             IF (NBESSA.LE.NBRSSA) THEN
               IF (ABS(OMGSHI).LT.OMECOR) THEN
                 OMGSHI=OMECOR
                 IF (LDYN) THEN
                   VALR=FREQOM(OMGSHI)
                 ELSE
                   VALR=OMGSHI
                 ENDIF
                 IF (NIV.GE.1) WRITE(IFM,2000)OMGSHI
C --- CE N'EST PLUS LA PEINE DE DECALER, C'EST INUTILE
                NBESSA=NBRSSA
               ELSE
                 OMGDEC=-SIGN(1.D0,OMGSHI)*MAX(OMECOR,PREC*OMGSHI)
                 OMGSHI=OMGSHI+OMGDEC
                 IF (LDYN) THEN
                   VALR=FREQOM(OMGSHI)
                 ELSE
                   VALR=OMGSHI
                 ENDIF
                 IF (NIV.GE.1) WRITE(IFM,2100)(PREC*100.D0),VALR
                 PREC=2.D0*PREC
               ENDIF
               GOTO 23
             ELSE
               IF (LDYN) THEN
                 VALR=FREQOM(OMGSHI)
               ELSE
                 VALR=OMGSHI
               ENDIF
               CALL U2MESG('F', 'ALGELINE3_65',0,' ',0,0,1,VALR)
             ENDIF
           ENDIF
           OMESHI=OMGSHI
         ENDIF
         
C          --- AFFICHAGE COMMUN ---
         IF ((NIV.GE.1).AND.(OPTION.EQ.'BANDE')) THEN
           IF (LDYN) THEN
             WRITE(IFM,2200)FREQOM(OMGMIN)
             WRITE(IFM,2300)FREQOM(OMGMAX)
             IF (OPTION.EQ.'BANDE') WRITE(IFM,1000)FREQOM(OMESHI)
           ELSE
             WRITE(IFM,2201)OMGMIN
             WRITE(IFM,2301)OMGMAX
             IF (OPTION.EQ.'BANDE') WRITE(IFM,1001)OMESHI
          ENDIF
         ENDIF


C     ------------------------------------------------------------------
C     ------------------------ OPTION PLUS_PETITE OU TOUT -------------
C     ------------------------------------------------------------------

      ELSE IF ((OPTION.EQ.'PLUS_PETITE').OR.(OPTION.EQ.'TOUT')) THEN

         OMGSHI = 0.D0
         NBESSA = 0
         PREC=PRECSH
 30      CONTINUE
         IER=0
         CALL VPSTUR(LRAIDE,OMGSHI,LMASSE,LDYNAM,RBID,
     &               IBID,NPIVOT(1),IER,SOLVEU,.FALSE.,.TRUE.)
         IF (IER.NE.0) THEN
           NBESSA=NBESSA+1
           IF (NBESSA.LE.NBRSSA) THEN
             IF (ABS(OMGSHI).LT.OMECOR) THEN
               OMGSHI=-OMECOR
               IF (LDYN) THEN
                 VALR=FREQOM(OMGSHI)
               ELSE
                 VALR=OMGSHI
               ENDIF
               IF (NIV.GE.1) WRITE(IFM,1800)VALR
C --- CE N'EST PLUS LA PEINE DE DECALER, C'EST INUTILE
                NBESSA=NBRSSA
             ELSE
               OMGDEC=-SIGN(1.D0,OMGSHI)*MAX(OMECOR,PREC*OMGSHI)
               OMGSHI=OMGSHI+OMGDEC
               IF (LDYN) THEN
                 VALR=FREQOM(OMGSHI)
               ELSE
                 VALR=OMGSHI
               ENDIF
               IF (NIV.GE.1) WRITE(IFM,2400)(PREC*100.D0),VALR
               PREC=2.D0*PREC
             ENDIF
             GOTO 30
           ELSE
             IF (LDYN) THEN
               VALR=FREQOM(OMGSHI)
             ELSE
               VALR=OMGSHI
             ENDIF
             CALL U2MESG('F', 'ALGELINE3_65',0,' ',0,0,1,VALR)
           ENDIF
         ENDIF
         OMESHI=OMGSHI
         IF (NIV.GE.1) THEN
           IF (LDYN) THEN
             WRITE(IFM,1000)FREQOM(OMESHI)
           ELSE
             WRITE(IFM,1001)OMESHI
           ENDIF
         ENDIF

C     ------------------------------------------------------------------
C     ------------------------ OPTION NON CONNUE -----------------------
C     ------------------------------------------------------------------

      ELSE
        CH16=OPTION
        CALL U2MESK('F','ALGELINE3_69',1,CH16)
      ENDIF

      IF ((NIV.GE.1).AND.(OPTION(1:5).NE.'STURM')) WRITE(IFM,1200)

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
