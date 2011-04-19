      SUBROUTINE DIARME (NBT,NEQ,ICODMA,UL,DUL,UTL,SIM,VARIM,
     &                   KLV,VARIP,KTY2,DULY)
C ----------------------------------------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER NBT,NEQ,ICODMA
      REAL*8  UL(NEQ),DUL(NEQ),UTL(NEQ),SIM(NEQ),VARIM
      REAL*8  KLV(NBT),VARIP,KTY2,DULY
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 20/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
C
C     RELATION DE COMPORTEMENT "ARME" (ARMEMENT).
C
C ----------------------------------------------------------------------
C
C IN  : NBT    : NOMBRE DE VALEURS POUR LA DEMI-MATRICE
C       NEQ    : NOMBRE DE DDL DE L'ELEMENT
C       ICODMA : ADRESSE DU MATERIAU CODE
C       UL     : DEPLACEMENT PRECEDENT REPERE LOCAL
C       DUL    : INCREMENT DE DEPLACEMENT REPERE LOCAL
C       UTL    : DEPLACEMENT COURANT REPERE LOCAL
C       SIM    : EFFORTS GENERALISES A L'INSTANT PRECEDENT
C       VARIM  : VARIABLE INTERNE A L'INSTANT PRECEDENT
C
C OUT : KLV    : MATRICE TANGENTE
C       VARIP  : VARIABLE INTERNE REACTUALISEE
C       KTY2   :
C       DULY   :
C
C**************** DECLARATION DES VARIABLES LOCALES ********************
C
      PARAMETER    ( NBRE2 = 5 )
      REAL*8       KTY, KYE, KYP, KYG
      REAL*8       VALRE2(NBRE2)
      INTEGER CODRE2(NBRE2)
      CHARACTER*8  NOMPAR, NOMRE2(NBRE2)
C
      DATA NOMRE2 /'KYE','DLE','KYP','DLP','KYG'/
C
C ----------------------------------------------------------------------
C --- DEFINITION DES PARAMETRES
C
      ZERO = 0.D0
      NBPAR = 0
      NOMPAR = ' '
      VALPAR = 0.D0
      CALL R8INIR (NBRE2,ZERO,VALRE2,1)
C
C --- CARACTERISTIQUES DU MATERIAU
C
      CALL RCVALA(ICODMA,' ','ARME',NBPAR,NOMPAR,VALPAR,NBRE2,
     &                            NOMRE2,VALRE2,CODRE2, 1)
C
      KYE = VALRE2(1)
      DLE = VALRE2(2)
      KYP = VALRE2(3)
      DLP = VALRE2(4)
      KYG = VALRE2(5)
C
C --- INITITIALISATIONS
C
      EFFOY = 0.5D0*ABS(SIM(8)+SIM(2))
      RAP = 0.D0
      VARMAX = DLP-DLE
      DULY = DUL(8)-DUL(2)
      ULY = UL(8)-UL(2)
      UTOT = ABS(UTL(8)-UTL(2))
      FLP = KYE*DLE+KYP*VARIM
      FLE = KYE*ABS(DULY)
      IF (ULY.NE.0.D0) RAP = DULY/ULY
C
C -*-*-*-*       TEST POUR SAVOIR SI L'ON DECHARGE OU NON      *-*-*-*-*
C
      IF (RAP.LT.0.D0.OR.DULY.EQ.0.D0) THEN
C
C ======================================================================
C                         ON DECHARGE
C ======================================================================
C
C ******   TEST POUR DETERMINER LA PENTE POUR LA DECHARGE
C
        IF (VARIM.LT.VARMAX) THEN
C
C ***** DECHARGE AVEC PENTE ELASTIQUE
C
          VARIP = VARIM
          KTY = KYE
          KTY2 = KYE
C
        ELSE
C
C ****** DECHARGE AVEC PENTE ULTIME
C
          VARIP = VARMAX
          KTY = KYG
          KTY2 = KYG
C
        END IF
C
      ELSE IF (RAP.GT.0.D0.OR.(ULY.EQ.0.D0.AND.DULY.NE.0.D0)) THEN
C
C ======================================================================
C                          ON CHARGE
C ======================================================================
C
C ****** TEST DE POSITION PAR RAPPORT A LA COURBE ULTIME
C
        IF (VARIM.LT.VARMAX) THEN
C
C ****** ON EST SOUS LA COURBE ULTIME
C
C **** TEST DE POSITION PAR RAPPORT A LA COURBE PLASTIQUE
C
          IF (EFFOY.LT.FLP.OR.VARIM.EQ.0.D0) THEN
C
C **** ON EST SOUS LA COURBE PLASTIQUE
C
C ** TEST POUR SAVOIR SI ON RESTE SOUS LA COURBE PLASTIQUE
C
            IF ((EFFOY+FLE).LT.FLP) THEN
C
C ** ON RESTE SOUS LA COURBE PLASTIQUE
C
              VARIP = VARIM
              KTY = KYE
              KTY2 = KYE
C
            ELSE
C
C ** ON NE RESTE PAS SOUS LA COURBE PLASTIQUE
C
              IF (UTOT.LT.DLP) THEN
C  ON REJOINT LA COURBE PLASTIQUE
                VARIP = ABS(UTOT-DLE)
                KTY = KYP
                KTY2 = (KYE*DLE+KYP*VARIP-EFFOY)/ABS(DULY)
              ELSE
C  ON REJOINT LA COURBE ULTIME
                VARIP = VARMAX
                KTY2 = KYE*DLE - EFFOY
     &               + KYP*VARMAX + KYG*ABS(UTOT-DLP)
                KTY2 = KTY2/ABS(DULY)
                KTY = KYG
              END IF
C
            END IF
C
          ELSE
C
C **** ON EST SUR LA COURBE PLASTIQUE
C
C ** TEST POUR SAVOIR SI ON RESTE SUR LA COURBE PLASTIQUE
C
            IF (UTOT.LT.DLP) THEN
C
C ** ON RESTE SUR LA COURBE PLASTIQUE
C
              VARIP = ABS(UTOT-DLE)
              KTY = KYP
              KTY2 = KYP
C
            ELSE
C
C ** ON REJOINT LA COURBE ULTIME
C
              VARIP = VARMAX
              KTY2 = KYP*ABS(VARMAX-VARIM)+KYG*ABS(UTOT-DLP)
              KTY2 = KTY2/ABS(DULY)
              KTY = KYG
C
            END IF
C
          END IF
C
        ELSE
C
C ****** ON EST SUR LA COURBE ULTIME
C
          VARIP = VARMAX
          KTY = KYG
          KTY2 = KYG
C
        END IF
C
      END IF
C
C ======================================================================
C                         MODIFICATIONS FINALES
C ======================================================================
C
      KLV(3) = KTY
      KLV(30) = -KTY
      KLV(36) = KTY
C
C ----------------------------------------------------------------------
C
      END
