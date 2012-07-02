      SUBROUTINE VPERMC(LMASSE,LRAIDE,NBPROP,VECP,FR,AM,EXCL,
     &                  OMECOR,ERNORM)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      INTEGER           LMASSE, LRAIDE,NBPROP, EXCL(*)
      COMPLEX*16        VECP(*)
      REAL*8            FR(*),AM(*),OMECOR,ERNORM(*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C ======================================================================
C     CALCUL DE LA NORME D'ERREUR MODALE
C  ( IE NORME D'ERREUR SUR LES VALEURS ET VECTEURS PROPRES COMPLEXES.)
C     ------------------------------------------------------------------
C     PROBLEME QUADRATIQUE:
C
C                   !! LRAIDE * VECP  - VALP * LMASSE * VECP !!
C       ERNORM   =     -------------------------------------
C                           !! LRAIDE * VECP !!
C     ------------------------------------------------------------------
C     REFERENCE:
C     ------------------------------------------------------------------
C IN  LMASSE : IS : DESCRIPTEUR MATRICE DE "MASSE"
C IN  LRAIDE : IS : DESCRIPTEUR MATRICE DE "RAIDEUR"
C IN  NBPROP : IS : NOMBRE DE VALEURS ET DE VECTEURS PROPRES
C IN  VECP   : C16 : TABLEAU DES VECTEURS PROPRES
C IN  VALP   : R8 : TABLEAU DES VALEURS PROPRES
C IN  EXCL   : IS : TABLEAU DES NON-EXCLUS
C IN  FCORIG : R8 : FREQUENCE MODE DE CORPS RIGIDE
C OUT ERNORM : R8 : TABLEAU DES NORMES D'ERREUR
C     ------------------------------------------------------------------
C
      CHARACTER*24 VALK(2)
C
C     ------------------------------------------------------------------
      REAL*8       XSEUIL
      REAL*8 VALR
      REAL*8       AMI,FRI
      COMPLEX*16   FREQ2,ANORM1, ANORM2
      INTEGER      IAUX1,IAUX2,IAUX4,I,J,IVEC,NEQ

C     ------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      CALL JEMARQ()
      XSEUIL = OMECOR
      NEQ    = ZI(LMASSE+2)
C
      CALL WKVECT('&&VPERMC.TAMP.PROV_1' ,'V V C',NEQ,IAUX1)
      CALL WKVECT('&&VPERMC.TAMP.PROV_2' ,'V V C',NEQ,IAUX2)
C      CALL WKVECT('&&VPERMC.TAMPON.PROV_3' ,'V V C',NEQ,IAUX3)
      CALL WKVECT('&&VPERMC.TYPEDDL      ' ,'V V I',NEQ,IAUX4)
C
      DO 1 I=1,NBPROP
C
        IVEC=(I-1)*NEQ+1
        DO 10 J = 0, NEQ-1
           VECP(IVEC+J) = VECP(IVEC+J) * EXCL(J+1)
 10     CONTINUE
C
        AMI = AM(I)
        FRI = FR(I)
        IF ( ABS(AMI) .EQ. 1.D0) THEN
          ERNORM(I)= 1.D+70
          VALK (1) = ' '
          VALK (2) = ' '
          VALR = 1.0D70
          CALL U2MESG('A', 'ALGELINE4_74',2,VALK,0,0,1,VALR)
        ELSE
          FREQ2=DCMPLX(FRI,AMI*FRI*2.D0)
          CALL MCMULT('ZERO',LRAIDE,VECP(IVEC),ZC(IAUX1),1,.FALSE.)
          CALL MCMULT('ZERO',LMASSE,VECP(IVEC),ZC(IAUX2),1,.FALSE.)

          DO 2 J = 0, NEQ-1
            ZC(IAUX2+J)=ZC(IAUX1+J)-FREQ2*ZC(IAUX2+J)
 2        CONTINUE
C
C           --- ON PREND LA NORME EUCLIDIENNE ---
          ANORM1 = DCMPLX(0.D0,0.D0)
          ANORM2 = DCMPLX(0.D0,0.D0)
          DO 3 J = 0, NEQ-1
              ANORM1 = ANORM1 +
     &             (DCONJG(ZC(IAUX1+J))*ZC(IAUX1+J))*EXCL(J+1)
              ANORM2 = ANORM2 +
     &             (DCONJG(ZC(IAUX2+J))*ZC(IAUX2+J))*EXCL(J+1)
 3        CONTINUE
          IF ( ABS(FREQ2) .GT. XSEUIL ) THEN
            IF (  ANORM1 .NE. DCMPLX(0.D0,0.D0) ) THEN
              ERNORM(I)= SQRT( ABS(ANORM2 / ANORM1) )
            ELSE
              ERNORM(I)= 1.D+70
            ENDIF
          ELSE
            ERNORM(I) = ABS(FREQ2) * SQRT( ABS(ANORM2) )
          ENDIF
C
        ENDIF
    1 CONTINUE
C
      CALL JEDETR('&&VPERMC.TAMP.PROV_1' )
      CALL JEDETR('&&VPERMC.TAMP.PROV_2' )
C      CALL JEDETR('&&VPERMC.TAMPON.PROV_3' )
      CALL JEDETR('&&VPERMC.TYPEDDL      ' )
C
      CALL JEDEMA()
      END
