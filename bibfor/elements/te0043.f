      SUBROUTINE TE0043(OPTION,NOMTE)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*) OPTION,NOMTE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 29/02/2000   AUTEUR VABHHTS J.PELLET 
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
C     CALCUL DES CHARGES DE PESANTEUR DANS LES ELEMENTS DISCRETS
C     ------------------------------------------------------------------
C IN  OPTION : K16 : NOM DE L'OPTION A CALCULER
C        'CHAR_MECA_PESA_R'  : CALCUL DES CHARGES DE PESANTEUR
C IN  NOMTE  : K16 : NOM DU TYPE D'ELEMENT DISCRET :
C         MECA_DIS_T_N
C         MECA_DIS_T_L
C         MECA_DIS_TR_N
C         MECA_DIS_TR_L
C         MECA_2D_DIS_T_N
C         MECA_2D_DIS_T_L
C         MECA_2D_DIS_TR_N
C         MECA_2D_DIS_TR_L
C     ------------------------------------------------------------------

C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------

      CHARACTER*32 JEXNUM,JEXNOM,JEXR8,JEXATR
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)

C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------

      REAL*8 PGL(3,3),MAT(78)
      CHARACTER*16 CH16,CI16
C     ------------------------------------------------------------------
C     ------------------------------------------------------------------

      IF (NOMTE.EQ.'MECA_DIS_TR_L') THEN
        NBTERM = 78
        NNO = 2
        NC = 6
        NDIM = 3
      ELSE IF (NOMTE.EQ.'MECA_DIS_TR_N') THEN
        NBTERM = 21
        NNO = 1
        NC = 6
        NDIM = 3
      ELSE IF (NOMTE.EQ.'MECA_DIS_T_L') THEN
        NBTERM = 21
        NNO = 2
        NC = 3
        NDIM = 3
      ELSE IF (NOMTE.EQ.'MECA_DIS_T_N') THEN
        NBTERM = 6
        NNO = 1
        NC = 3
        NDIM = 3
      ELSE IF (NOMTE.EQ.'MECA_2D_DIS_TR_L') THEN
        NBTERM = 21
        NNO = 2
        NC = 3
        NDIM = 2
      ELSE IF (NOMTE.EQ.'MECA_2D_DIS_TR_N') THEN
        NBTERM = 6
        NNO = 1
        NC = 3
        NDIM = 2
      ELSE IF (NOMTE.EQ.'MECA_2D_DIS_T_L') THEN
        NBTERM = 10
        NNO = 2
        NC = 2
        NDIM = 2
      ELSE IF (NOMTE.EQ.'MECA_2D_DIS_T_N') THEN
        NBTERM = 3
        NNO = 1
        NC = 2
        NDIM = 2
      END IF

C     --- CALCUL DES VECTEURS ELEMENTAIRES ----
      IF (OPTION.EQ.'CHAR_MECA_PESA_R') THEN

C        --- MATRICE DE RIGIDITE LOCALE ---
        CALL JEVECH('PCADISM','L',LDIS)

        CALL JEVECH('PCAORIE','L',LORIEN)
        CALL MATROT(ZR(LORIEN),PGL)

C        --- GLOBAL VERS LOCAL ? ---
C        --- IREP = 2 = MATRICE EN REPERE LOCAL ==> PASSER EN GLOBAL ---
        IREP = NINT(ZR(LDIS+NBTERM))
        IF (IREP.EQ.2) THEN
          IF (NDIM.EQ.3) THEN
            CALL UTPSLG(NNO,NC,PGL,ZR(LDIS),MAT)
          ELSE IF (NDIM.EQ.2) THEN
            CALL UT2MLG(NNO,NC,PGL,ZR(LDIS),MAT)
          END IF
        ELSE
          DO 10 I = 1,NBTERM
            MAT(I) = ZR(LDIS+I-1)
   10     CONTINUE
        END IF

C        --- CHAMP DE PESANTEUR ---
        CALL JEVECH('PPESANR','L',LPESA)

C        --- VECTEUR CHARGEMENT ---
        CALL JEVECH('PVECTUR','E',LVEC)

C        --- ON Y VA ---
        LVEC = LVEC - 1
        LDIS = LDIS - 1
        IF (NOMTE.EQ.'MECA_DIS_TR_L') THEN
          ZR(LVEC+1) = MAT(01)*ZR(LPESA)*ZR(LPESA+1)
          ZR(LVEC+2) = MAT(03)*ZR(LPESA)*ZR(LPESA+2)
          ZR(LVEC+3) = MAT(06)*ZR(LPESA)*ZR(LPESA+3)
          ZR(LVEC+7) = MAT(28)*ZR(LPESA)*ZR(LPESA+1)
          ZR(LVEC+8) = MAT(36)*ZR(LPESA)*ZR(LPESA+2)
          ZR(LVEC+9) = MAT(45)*ZR(LPESA)*ZR(LPESA+3)
        ELSE IF (NOMTE.EQ.'MECA_DIS_TR_N') THEN
          ZR(LVEC+1) = MAT(01)*ZR(LPESA)*ZR(LPESA+1)
          ZR(LVEC+2) = MAT(03)*ZR(LPESA)*ZR(LPESA+2)
          ZR(LVEC+3) = MAT(06)*ZR(LPESA)*ZR(LPESA+3)
        ELSE IF (NOMTE.EQ.'MECA_DIS_T_L') THEN
          ZR(LVEC+1) = MAT(01)*ZR(LPESA)*ZR(LPESA+1)
          ZR(LVEC+2) = MAT(03)*ZR(LPESA)*ZR(LPESA+2)
          ZR(LVEC+3) = MAT(06)*ZR(LPESA)*ZR(LPESA+3)
          ZR(LVEC+4) = MAT(10)*ZR(LPESA)*ZR(LPESA+1)
          ZR(LVEC+5) = MAT(15)*ZR(LPESA)*ZR(LPESA+2)
          ZR(LVEC+6) = MAT(21)*ZR(LPESA)*ZR(LPESA+3)
        ELSE IF (NOMTE.EQ.'MECA_DIS_T_N') THEN
          ZR(LVEC+1) = MAT(01)*ZR(LPESA)*ZR(LPESA+1)
          ZR(LVEC+2) = MAT(03)*ZR(LPESA)*ZR(LPESA+2)
          ZR(LVEC+3) = MAT(06)*ZR(LPESA)*ZR(LPESA+3)
        ELSE IF (NOMTE.EQ.'MECA_2D_DIS_TR_L') THEN
          ZR(LVEC+1) = MAT(01)*ZR(LPESA)*ZR(LPESA+1)
          ZR(LVEC+2) = MAT(03)*ZR(LPESA)*ZR(LPESA+2)
          ZR(LVEC+4) = MAT(10)*ZR(LPESA)*ZR(LPESA+1)
          ZR(LVEC+5) = MAT(15)*ZR(LPESA)*ZR(LPESA+2)
        ELSE IF (NOMTE.EQ.'MECA_2D_DIS_TR_N') THEN
          ZR(LVEC+1) = MAT(01)*ZR(LPESA)*ZR(LPESA+1)
          ZR(LVEC+2) = MAT(03)*ZR(LPESA)*ZR(LPESA+2)
        ELSE IF (NOMTE.EQ.'MECA_2D_DIS_T_L') THEN
          ZR(LVEC+1) = MAT(01)*ZR(LPESA)*ZR(LPESA+1)
          ZR(LVEC+2) = MAT(03)*ZR(LPESA)*ZR(LPESA+2)
          ZR(LVEC+3) = MAT(06)*ZR(LPESA)*ZR(LPESA+1)
          ZR(LVEC+4) = MAT(10)*ZR(LPESA)*ZR(LPESA+2)
        ELSE IF (NOMTE.EQ.'MECA_2D_DIS_T_N') THEN
          ZR(LVEC+1) = MAT(01)*ZR(LPESA)*ZR(LPESA+1)
          ZR(LVEC+2) = MAT(03)*ZR(LPESA)*ZR(LPESA+2)
        ELSE
          CH16 = OPTION
          CI16 = NOMTE
          CALL UTMESS('F','OPTION "'//CH16//'" (TE0043)',
     &                'ELEMENT DISCRET "'//CI16//'" INCONNU.')
        END IF
      ELSE
        CH16 = OPTION
        CI16 = NOMTE
        CALL UTMESS('F','ELEMENT DISCRET '//CI16//' (TE0043)',
     &              'L''OPTION "'//CH16//'" EST INCONNUE')
      END IF

      END
