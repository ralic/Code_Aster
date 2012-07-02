      SUBROUTINE VPERMO( LMASSE,LRAIDE,NBPROP,VECP,VALP,EXCL,OMECOR,
     &                   ERNORM)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      INTEGER            LMASSE,LRAIDE,NBPROP,EXCL(*)
      REAL*8             VECP(*),VALP(*),OMECOR,ERNORM(*)
C     ------------------------------------------------------------------
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
C
C
C     --- SEUIL EN PULSATION POUR LES MODES DE CORPS RIGIDE ---
C-----------------------------------------------------------------------
      INTEGER I ,IAUX1 ,IAUX2 ,IVEC ,J ,NEQ 
      REAL*8 R8MAEM ,XSEUIL 
C-----------------------------------------------------------------------
      CALL JEMARQ()
      XSEUIL = OMECOR
C
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
      DO 10 I = 1, NBPROP
C
C        --- NON PRISE EN COMPTE DES DDLS EXCLUS
         IVEC=(I-1)*NEQ+1
         DO 15 J = 0, NEQ-1
            VECP(IVEC+J) = VECP(IVEC+J) * EXCL(J+1)
 15      CONTINUE
C
C        -- PRODUIT DU VECTEUR PROPRE AVEC LA MATRICE DE RAIDEUR SHIFTEE
         CALL MRMULT('ZERO',LRAIDE,VECP(IVEC),ZR(IAUX1),1,.FALSE.)
C
C        --- PRODUIT DU VECTEUR PROPRE AVEC LA MATRICE DE MASSE
         CALL MRMULT('ZERO',LMASSE,VECP(IVEC),ZR(IAUX2),1,.FALSE.)
C
C        --- CALCUL DU VECTEUR ERREUR
         DO 20 J = 0, NEQ-1
            ZR(IAUX2+J) = ZR(IAUX1+J) - VALP(I) * ZR(IAUX2+J)
 20      CONTINUE
C
C        --- ON PREND LA NORME EUCLIDIENNE DU VECTEUR ERREUR ---
C
         ANORM1 = 0.D0
         ANORM2 = 0.D0
         DO 30 J = 0, NEQ-1
            ANORM1 = ANORM1+ZR(IAUX1+J)*ZR(IAUX1+J)*EXCL(J+1)
            ANORM2 = ANORM2+ZR(IAUX2+J)*ZR(IAUX2+J)*EXCL(J+1)
 30      CONTINUE
         IF ( ABS(VALP(I)) .GT. XSEUIL ) THEN
            IF (  ANORM1 .NE. 0.D0 ) THEN
               ERNORM(I)= SQRT( ANORM2 / ANORM1 )
            ELSE
               ERNORM(I)= R8MAEM()
            ENDIF
         ELSE
            ERNORM(I) = ABS(VALP(I)) * SQRT( ANORM2 )
         ENDIF
C
 10   CONTINUE
C     ----------------------------------------------------------------
C     -------------- DESALLOCATION DES ZONES DE TRAVAIL --------------
C     ----------------------------------------------------------------
C
      CALL JEDETR('&&VPERMO.TAMPON.PROV_1' )
      CALL JEDETR('&&VPERMO.TAMPON.PROV_2' )
C
      CALL JEDEMA()
      END
