      SUBROUTINE HBRCVX(SIG,VID,NMAT, MATERF, SEUIL, VP, VECP)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/12/2006   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
C =====================================================================
      IMPLICIT      NONE
      INTEGER       NMAT
      REAL*8        SIG(6),VID(3),MATERF(NMAT,2),SEUIL,VP(3),VECP(3,3)
C =====================================================================
C --- HOEK-BROWN : VALEUR SEUIL POUR LE CONVEXE ELASTO-PLASTIQUE ------
C --- CALCUL INITIAL A DG=0 -------------------------------------------
C =====================================================================
C IN  : SIG   :  TENSEUR DES CONTRAINTES (ELASTIQUE) A T+DT -----------
C --- : VID   :  VARIABLES INTERNES -----------------------------------
C --- : NMAT  :  NOMBRE DE PARAMETRES MATERIAU ------------------------
C --- : MATERF:  COEFFICIENTS MATERIAU A T+DT -------------------------
C ----------- :  MATER(*,1) = CARACTERISTIQUES ELASTIQUES -------------
C ----------- :  MATER(*,2) = CARACTERISTIQUES PLASTIQUES -------------
C OUT : VP    :  VALEURS PROPRES DU DEVIATEUR DE SIG (ELASTIQUE) ------
C OUT : VECP  :  VECTEURS PROPRES DU DEVIATEUR DE SIG (ELASTIQUE) -----
C OUT : SEUIL :  VALEUR DU CRITERE PLASTIQUE --------------------------
C =====================================================================
      REAL*8       DIFSIG,SIG3,SIGBD,DEUX,AUX
      REAL*8       AUX1,AUX2,PARAME(4),SEB(6)
      REAL*8       GAMMA,I1E,SE(6),TRACE,TU(6),TOL,TOLDYN,JACAUX(3)
      CHARACTER*10 CVP1,CVP2,CVP3
      CHARACTER*24 VALK(3)
      INTEGER      NDT,NDI,NPERM,TTRIJ,OTRIJ,NITJAC
C ======================================================================
      PARAMETER       ( DEUX   =  2.0D0  )
C ======================================================================
      COMMON /TDIM/   NDT, NDI
C ======================================================================
      DATA   NPERM ,TOL,TOLDYN    /12,1.D-10,1.D-2/
      DATA   TTRIJ,OTRIJ  /0,0/
C ======================================================================
C --- RECUPERATION DES DONNEES MATERIAU --------------------------------
C ======================================================================
      SIGBD = MATERF(14,2)
C ======================================================================
C --- CALCUL DES PARAMETRES D ECROUISSAGE ------------------------------
C ======================================================================
      GAMMA = VID(1)
      IF (GAMMA.LT.0.0D0) THEN
         CALL U2MESS('F','ALGORITH3_88')
      ENDIF
      CALL HBVAEC(GAMMA,NMAT,MATERF,PARAME)
C ======================================================================
C --- CALCUL DES VALEURS PROPRES DU DEVIATEUR ELASTIQUE ----------------
C ======================================================================
      CALL LCDEVI(SIG,SE)
      I1E    = TRACE(NDI,SIG)
      SEB(1) = SE(1)
      SEB(2) = SE(4)/SQRT(DEUX)
      SEB(4) = SE(2)
      SEB(6) = SE(3)
      IF (NDT.EQ.4) THEN
          SEB(3) = 0.0D0
          SEB(5) = 0.0D0
      ELSE
          SEB(3) = SE(5) / SQRT(DEUX)
          SEB(5) = SE(6) / SQRT(DEUX)
      ENDIF
C -- MATRICE UNITE POUR JACOBI ----------------------------------------
      TU(1) = 1.D0
      TU(2) = 0.D0
      TU(3) = 0.D0
      TU(4) = 1.D0
      TU(5) = 0.D0
      TU(6) = 1.D0
      CALL JACOBI(3,NPERM,TOL,TOLDYN,SEB,TU,VECP,VP,JACAUX,
     &       NITJAC,TTRIJ,OTRIJ)
      IF ((VP(2).LT.VP(1)) .OR. (VP(3).LT.VP(2))) THEN
          CALL CODREE(VP(1),'E',CVP1)
          CALL CODREE(VP(2),'E',CVP2)
          CALL CODREE(VP(3),'E',CVP3)
           VALK(1) = CVP1
           VALK(2) = CVP2
           VALK(3) = CVP3
           CALL U2MESK('F','ALGORITH3_89', 3 ,VALK)
      ENDIF
      DIFSIG  = VP(3)-VP(1)
      SIG3    = VP(3)+I1E/3.0D0
C ======================================================================
C --- CALCUL DU SEUIL --------------------------------------------------
C ======================================================================
      AUX1 = -SIG3*PARAME(2)+PARAME(1)
      AUX2 = PARAME(3)*(1.0D0+SIG3/SIGBD)
      IF (AUX1.LT.0.0D0) THEN
           SEUIL = 2.0D0
      ELSE
           SEUIL = DIFSIG - AUX2 - SQRT(AUX1)
      ENDIF
 10   CONTINUE
C ======================================================================
      END
