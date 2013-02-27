      SUBROUTINE VPERMO( LMASSE,LRAIDE,NBPROP,VECP,VALP,EXCL,OMECOR,
     &                   ERNORM)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      INTEGER            LMASSE,LRAIDE,NBPROP,EXCL(*)
      REAL*8             VECP(*),VALP(*),OMECOR,ERNORM(*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 26/02/2013   AUTEUR BOITEAU O.BOITEAU 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     CALCUL DE LA NORME D'ERREUR MODALE
C     ( IE NORME D'ERREUR SUR LES VALEURS ET VECTEURS PROPRES.)
C     ------------------------------------------------------------------
C     PROBLEME GENERALISE :  (LRAIDE)*VECP = VALP *(LMASSE)*VECP
C
C                   !! LRAIDE * VECP  - VALP * LMASSE * VECP !!
C       ERNORM   =     -------------------------------------
C                           !! LRAIDE * VECP !!
C     ------------------------------------------------------------------
C     REFERENCE: BATHE ET WILSON
C     ------------------------------------------------------------------
C IN  LMASSE : IS : DESCRIPTEUR MATRICE DE "MASSE"
C IN  LRAIDE : IS : DESCRIPTEUR MATRICE DE "RAIDEUR"
C IN  NBPROP : IS : NOMBRE DE VALEURS ET DE VECTEURS PROPRES
C IN  VECP   : R8 : TABLEAU DES VECTEURS PROPRES
C IN  VALP   : R8 : TABLEAU DES VALEURS PROPRES
C IN  EXCL   : IS : TABLEAU DES NON-EXCLUS
C IN  FCORIG : R8 : FREQUENCE DE CORPS RIGIDE
C OUT ERNORM : R8 : TABLEAU DES NORMES D'ERREUR
C     ------------------------------------------------------------------
      REAL*8       ANORM1, ANORM2


C     --- SEUIL EN PULSATION POUR LES MODES DE CORPS RIGIDE ---
C-----------------------------------------------------------------------
      INTEGER   I ,IAUX1 ,IAUX2 ,J ,NEQ, IVEC 
      REAL*8    XSEUIL,RMIN,R8MIEM,RAUX
      INTEGER*4 NBI4,NEQ4 
C-----------------------------------------------------------------------
      CALL JEMARQ()
      XSEUIL = OMECOR

C     ------------------------------------------------------------------
C     ---------------------- DONNEES SUR LES MATRICES ------------------
C     ------------------------------------------------------------------
      NEQ    = ZI(LMASSE+2)
C     ------------------------------------------------------------------
C     -------------- ALLOCATION DES ZONES DE TRAVAIL -------------------
C     ------------------------------------------------------------------
      CALL WKVECT('&&VPERMO.TAMPON.PROV_1' ,'V V R',NEQ,IAUX1)
      CALL WKVECT('&&VPERMO.TAMPON.PROV_2' ,'V V R',NEQ,IAUX2)
C     ------------------------------------------------------------------
C     ---------------------- CALCUL DES NORMES D'ERREUR ----------------
C     ------------------------------------------------------------------
      RMIN=100.D0*R8MIEM()

      NBI4=NBPROP
      NEQ4=NEQ
C        --- NON PRISE EN COMPTE DES DDLS EXCLUS
      DO 15 I=1,NEQ
        RAUX=EXCL(I)
        CALL DSCAL(NBI4,RAUX,VECP(I),NEQ4)
 15   CONTINUE

      DO 30 I=1,NBPROP
        IVEC=(I-1)*NEQ+1
        CALL MRMULT('ZERO',LRAIDE,VECP(IVEC),ZR(IAUX1),1,.FALSE.)
        CALL MRMULT('ZERO',LMASSE,VECP(IVEC),ZR(IAUX2),1,.FALSE.)
        ANORM1 = 0.D0
        DO 20 J = 1, NEQ
          RAUX=ZR(IAUX1+J-1)
          ANORM1 = ANORM1+RAUX*RAUX*EXCL(J)
 20     CONTINUE
        RAUX=-VALP(I)
        CALL DAXPY(NEQ4,RAUX,ZR(IAUX2),1,ZR(IAUX1),1)
        ANORM2 = 0.D0
        DO 25 J = 1, NEQ
          RAUX=ZR(IAUX1+J-1)
          ANORM2 = ANORM2+RAUX*RAUX*EXCL(J)
 25     CONTINUE

 
        IF (ABS(VALP(I)).GT.XSEUIL) THEN
          IF (ANORM1.GE.RMIN) THEN
            ERNORM(I)= SQRT(ANORM2/ANORM1)
          ELSE
            ERNORM(I)= 1.D+70
          ENDIF
        ELSE
          ERNORM(I) = ABS(VALP(I)) * SQRT(ANORM2)
        ENDIF
 30   CONTINUE  

C     ----------------------------------------------------------------
C     -------------- DESALLOCATION DES ZONES DE TRAVAIL --------------
C     ----------------------------------------------------------------

      CALL JEDETR('&&VPERMO.TAMPON.PROV_1' )
      CALL JEDETR('&&VPERMO.TAMPON.PROV_2' )

      CALL JEDEMA()
      END
