      SUBROUTINE TE0042(OPTION,NOMTE)
      IMPLICIT   NONE
      CHARACTER*(*) OPTION,NOMTE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 08/11/2005   AUTEUR CIBHHLV L.VIVAN 
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
C     ------------------------------------------------------------------
C     CALCUL DU VECTEUR ELEMENTAIRE EFFORT GENERALISE
C     ------------------------------------------------------------------
C IN  OPTION : K16 : NOM DE L'OPTION A CALCULER
C                   'SIEF_ELGA_DEPL'
C                   'EFGE_ELNO_DEPL'
C IN  NOMTE  : K16 : NOM DU TYPE D'ELEMENT DISCRET :
C         MECA_DIS_T_N      MECA_DIS_T_L       MECA_DIS_TR_N
C         MECA_DIS_TR_L
C         MECA_2D_DIS_T_N   MECA_2D_DIS_T_L    MECA_2D_DIS_TR_N
C         MECA_2D_DIS_TR_L
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
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
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C     ------------------------------------------------------------------
      INTEGER NBTERM,NNO,NC,NEQ,IREP,I,NDIM
      INTEGER LDIS,LORIEN,JEFFO,JDEPL
      REAL*8 ULR(12),FLR(12)
      REAL*8 PGL(3,3),KLC(144),MAT(78),UGR(12)
      CHARACTER*16 CH16,CI16
      LOGICAL LSIGNE
C     ------------------------------------------------------------------


      LSIGNE = .TRUE.
      IF (NOMTE.EQ.'MECA_DIS_TR_L') THEN
        NBTERM = 78
        NNO = 2
        NC = 6
        NEQ = 12
        NDIM = 3
        LSIGNE = .FALSE.
      ELSE IF (NOMTE.EQ.'MECA_DIS_TR_N') THEN
        NBTERM = 21
        NNO = 1
        NC = 6
        NEQ = 6
        NDIM = 3
      ELSE IF (NOMTE.EQ.'MECA_DIS_T_L') THEN
        NBTERM = 21
        NNO = 2
        NC = 3
        NEQ = 6
        NDIM = 3
        LSIGNE = .FALSE.
      ELSE IF (NOMTE.EQ.'MECA_DIS_T_N') THEN
        NBTERM = 6
        NNO = 1
        NC = 3
        NEQ = 3
        NDIM = 3
      ELSE IF (NOMTE.EQ.'MECA_2D_DIS_TR_L') THEN
        NBTERM = 21
        NNO = 2
        NC = 3
        NEQ = 6
        NDIM = 2
        LSIGNE = .FALSE.
      ELSE IF (NOMTE.EQ.'MECA_2D_DIS_TR_N') THEN
        NBTERM = 6
        NNO = 1
        NC = 3
        NEQ = 3
        NDIM = 2
      ELSE IF (NOMTE.EQ.'MECA_2D_DIS_T_L') THEN
        NBTERM = 10
        NNO = 2
        NC = 2
        NEQ = 4
        NDIM = 2
        LSIGNE = .FALSE.
      ELSE IF (NOMTE.EQ.'MECA_2D_DIS_T_N') THEN
        NBTERM = 3
        NNO = 1
        NC = 2
        NEQ = 2
        NDIM = 2
      ELSE
        CH16 = NOMTE
        CALL UTMESS('F','ELEMENTS DISCRET (TE0042)',
     &              '"'//CH16//'"    NOM D''ELEMENT INCONNU.')
      END IF

C     --- MATRICE DE RIGIDITE LOCALE ---
      CALL JEVECH('PCADISK','L',LDIS)

      CALL JEVECH('PCAORIE','L',LORIEN)
      CALL MATROT(ZR(LORIEN),PGL)

C     --- ABSOLU VERS LOCAL ? ---
C     --- IREP = 1 = MATRICE EN REPERE GLOBAL ==> PASSER EN LOCAL ---
      IREP = NINT(ZR(LDIS+NBTERM))
      IF (IREP.EQ.1) THEN
        IF (NDIM.EQ.3) THEN
          CALL UTPSGL(NNO,NC,PGL,ZR(LDIS),MAT)
        ELSE IF (NDIM.EQ.2) THEN
          CALL UT2MGL(NNO,NC,PGL,ZR(LDIS),MAT)
        END IF
      ELSE
        DO 10 I = 1,NBTERM
          MAT(I) = ZR(LDIS+I-1)
   10   CONTINUE
      END IF

C     ---- MATRICE RIGIDITE LIGNE > MATRICE RIGIDITE CARRE
      CALL VECMA(MAT,NBTERM,KLC,NEQ)

C     --- CALCUL DES VECTEURS ELEMENTAIRES ----
      IF (OPTION.EQ.'SIEF_ELGA_DEPL') THEN
C                       --------------
        CALL JEVECH('PCONTRR','E',JEFFO)
        CALL JEVECH('PDEPLAR','L',JDEPL)

C        --- VECTEUR DEPLACEMENT LOCAL  ULR = PGL * UG  ---
        IF (NDIM.EQ.3) THEN
          CALL UTPVGL(NNO,NC,PGL,ZR(JDEPL),ULR)
        ELSE IF (NDIM.EQ.2) THEN
          CALL UT2VGL(NNO,NC,PGL,ZR(JDEPL),ULR)
        END IF

C        --- VECTEUR EFFORT      LOCAL  FLR = KLC * ULR  ---
        CALL PMAVEC('ZERO',NEQ,KLC,ULR,FLR)

      ELSE IF (OPTION.EQ.'EFGE_ELNO_DEPL') THEN
C                          --------------
        CALL JEVECH('PEFFORR','E',JEFFO)
        CALL JEVECH('PDEPLAR','L',JDEPL)

C        --- VECTEUR DEPLACEMENT LOCAL  ULR = PGL * UG  ---
        IF (NDIM.EQ.3) THEN
          CALL UTPVGL(NNO,NC,PGL,ZR(JDEPL),ULR)
        ELSE IF (NDIM.EQ.2) THEN
          CALL UT2VGL(NNO,NC,PGL,ZR(JDEPL),ULR)
        END IF

C        --- VECTEUR EFFORT      LOCAL  FLR = KLC * ULR  ---
        CALL PMAVEC('ZERO',NEQ,KLC,ULR,FLR)

      ELSE
        CH16 = OPTION
        CI16 = NOMTE
        CALL UTMESS('F','ELEMENT DISCRET  "'//CI16//'"  (TE0042)',
     &              'L''OPTION "'//CH16//'" EST INCONNUE')
      END IF

C     --- ARCHIVAGE ---
C         ---- ON CHANGE LE SIGNE DES EFFORTS SUR LE PREMIER NOEUD
C         ---- POUR LES MECA_DIS_TR_L ,         MECA_DIS_T_L
C         ----         MECA_2D_DIS_TR_L      ET MECA_2D_DIS_T_L

      IF (LSIGNE) THEN
        DO 50 I = 1,NEQ
          ZR(JEFFO+I-1) = FLR(I)
   50   CONTINUE
      ELSE
        DO 60 I = 1,NC
          ZR(JEFFO+I-1) = -FLR(I)
          ZR(JEFFO+I+NC-1) = FLR(I+NC)
   60   CONTINUE
      END IF

      END
