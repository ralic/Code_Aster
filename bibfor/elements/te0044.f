      SUBROUTINE TE0044(OPTION,NOMTE)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*) OPTION,NOMTE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 23/10/2008   AUTEUR TORKHANI M.TORKHANI 
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
C     CALCUL DE L'ENERGIE DE DEFORMATION, ET CINETIQUE
C     ------------------------------------------------------------------
C IN  OPTION : K16 : NOM DE L'OPTION A CALCULER
C        'EPOT_ELEM_DEPL' : CALCUL DE L'ENERGIE DE DEFORMATION
C        'ECIN_ELEM_DEPL' : CALCUL DE L'ENERGIE CINETIQUE
C IN  NOMTE  : K16 : NOM DU TYPE D'ELEMENT DISCRET :
C         MECA_DIS_T_N
C         MECA_DIS_T_L
C         MECA_DIS_TR_N
C         MECA_DIS_TR_L

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

      REAL*8 ALPHA,BETA,GAMMA
      REAL*8 UL(12),PGL(3,3),KLC(12,12),MAT(144)
      INTEGER INFOD,INFODI
      CHARACTER*16 CH16,CI16,NOMCMD,TYPRES
      CHARACTER*19 NOMFON
      CHARACTER*24 VALK(2)

C     ------------------------------------------------------------------

      INFODI = 1
      CALL GETRES(NOMFON,TYPRES,NOMCMD)
      IF (NOMCMD.EQ.'POST_ELEM') THEN
         CALL JEVECH('PCINFDI','L',INFOD)
         INFODI = NINT(ZR(INFOD))
      ENDIF
      IF (NOMTE.EQ.'MECA_DIS_TR_L') THEN
        IF (INFODI.EQ.1) THEN
        NBTERM = 78
        NNO = 2
        NC = 6
        NEQ = 12
        ITYPE = 41
        KANL = 0
        NDIM = 3
        ELSEIF (INFODI.EQ.2) THEN
        NBTERM = 144
        NNO = 2
        NC = 6
        NEQ = 12
        ITYPE = 43
        KANL = 0
        NDIM = 3
        ENDIF
      ELSE IF (NOMTE.EQ.'MECA_DIS_TR_N') THEN
        IF (INFODI.EQ.1) THEN
        NBTERM = 21
        NNO = 1
        NC = 6
        NEQ = 6
        ITYPE = 21
        KANL = 0
        NDIM = 3
        ELSEIF (INFODI.EQ.2) THEN
        NBTERM = 36
        NNO = 1
        NC = 6
        NEQ = 6
        ITYPE = 23
        KANL = 0
        NDIM = 3
        ENDIF
      ELSE IF (NOMTE.EQ.'MECA_DIS_T_L') THEN
        IF (INFODI.EQ.1) THEN
        NBTERM = 21
        NNO = 2
        NC = 3
        NEQ = 6
        ITYPE = 40
        KANL = 0
        NDIM = 3
        ELSEIF (INFODI.EQ.2) THEN
        NBTERM = 36
        NNO = 2
        NC = 3
        NEQ = 6
        ITYPE = 42
        KANL = 0
        NDIM = 3
        ENDIF
      ELSE IF (NOMTE.EQ.'MECA_DIS_T_N') THEN
        IF (INFODI.EQ.1) THEN
        NBTERM = 6
        NNO = 1
        NC = 3
        NEQ = 3
        ITYPE = 20
        KANL = 0
        NDIM = 3
        ELSEIF (INFODI.EQ.2) THEN
        NBTERM = 9
        NNO = 1
        NC = 3
        NEQ = 3
        ITYPE = 22
        KANL = 0
        NDIM = 3
        ENDIF
      ELSE IF (NOMTE.EQ.'MECA_2D_DIS_TR_L') THEN
        IF (INFODI.EQ.1) THEN
        NBTERM = 21
        NNO = 2
        NC = 3
        NEQ = 6
        ITYPE = 41
        KANL = 0
        NDIM = 2
        ELSEIF (INFODI.EQ.2) THEN
        NBTERM = 36
        NNO = 2
        NC = 3
        NEQ = 6
        ITYPE = 43
        KANL = 0
        NDIM = 2
        ENDIF
      ELSE IF (NOMTE.EQ.'MECA_2D_DIS_TR_N') THEN
        IF (INFODI.EQ.1) THEN
        NBTERM = 6
        NNO = 1
        NC = 3
        NEQ = 3
        ITYPE = 21
        KANL = 0
        NDIM = 2
        ELSEIF (INFODI.EQ.2) THEN
        NBTERM = 9
        NNO = 1
        NC = 3
        NEQ = 3
        ITYPE = 23
        KANL = 0
        NDIM = 2
        ENDIF
      ELSE IF (NOMTE.EQ.'MECA_2D_DIS_T_L') THEN
        IF (INFODI.EQ.1) THEN
        NBTERM = 10
        NNO = 2
        NC = 2
        NEQ = 4
        ITYPE = 40
        KANL = 0
        NDIM = 2
        ELSEIF (INFODI.EQ.2) THEN
        NBTERM = 16
        NNO = 2
        NC = 2
        NEQ = 4
        ITYPE = 42
        KANL = 0
        NDIM = 2
        ENDIF
      ELSE IF (NOMTE.EQ.'MECA_2D_DIS_T_N') THEN
        IF (INFODI.EQ.1) THEN
        NBTERM = 3
        NNO = 1
        NC = 2
        NEQ = 2
        ITYPE = 20
        KANL = 0
        NDIM = 2
        ELSEIF (INFODI.EQ.2) THEN
        NBTERM = 4
        NNO = 1
        NC = 2
        NEQ = 2
        ITYPE = 22
        KANL = 0
        NDIM = 2
        ENDIF
      ELSE
        CH16 = OPTION
        CI16 = NOMTE
         VALK(1) = CH16
         VALK(2) = CI16
         CALL U2MESK('F','ELEMENTS2_89', 2 ,VALK)
      END IF

C     --- MATRICE DE ROTATION PGL ---
      CALL JEVECH('PCAORIE','L',LORIE)
      CALL MATROT(ZR(LORIE),PGL)

C     --- VECTEUR DEPLACEMENT LOCAL  UL = PGL * UG  ---
      CALL JEVECH('PDEPLAR','L',LDEPL)
      IF (NDIM.EQ.3) THEN
        CALL UTPVGL(NNO,NC,PGL,ZR(LDEPL),UL)
      ELSE IF (NDIM.EQ.2) THEN
        CALL UT2VGL(NNO,NC,PGL,ZR(LDEPL),UL)
      END IF

      IF (OPTION.EQ.'EPOT_ELEM_DEPL') THEN
        CALL JEVECH('PENERDR','E',JENDE)

C        --- MATRICE DE RIGIDITE ---
        IF (INFODI.EQ.1) THEN
           CALL JEVECH('PCADISK','L',LDIS)
        ELSEIF (INFODI.EQ.2) THEN
           CALL JEVECH('PCADNSK','L',LDIS)
        ENDIF

C        --- GLOBAL VERS LOCAL ? ---
C        --- IREP EQ 1 : MATRICE EN REPERE GLOBAL
C        --- IREP NE 1 : MATRICE EN REPERE LOCAL
        IREP = NINT(ZR(LDIS+NBTERM))
        IF (IREP.EQ.1) THEN
          IF (NDIM.EQ.3) THEN
            IF (INFODI.EQ.1) THEN
              CALL UTPSGL(NNO,NC,PGL,ZR(LDIS),MAT)
            ELSEIF (INFODI.EQ.2) THEN
              CALL UTPPGL(NNO,NC,PGL,ZR(LDIS),MAT)
            ENDIF
          ELSE IF (NDIM.EQ.2) THEN
            IF (INFODI.EQ.1) THEN
              CALL UT2MGL(NNO,NC,PGL,ZR(LDIS),MAT)
            ELSEIF (INFODI.EQ.2) THEN
              CALL UT2PGL(NNO,NC,PGL,ZR(LDIS),MAT)
            ENDIF
          END IF
        ELSE
          DO 10 I = 1,NBTERM
            MAT(I) = ZR(LDIS+I-1)
   10     CONTINUE
        END IF

C        ---- MATRICE RIGIDITE LIGNE > MATRICE RIGIDITE CARRE
        IF (INFODI.EQ.1) THEN
          CALL VECMA(MAT,NBTERM,KLC,NEQ)
        ELSEIF (INFODI.EQ.2) THEN
          CALL VECMAP(MAT,NBTERM,KLC,NEQ)
        ENDIF

C        --- ENERGIE DE DEFORMATION ---
        IF = 1
        CALL PTENPO(NEQ,UL,KLC,ZR(JENDE),ITYPE,IF)

      ELSE IF (OPTION.EQ.'ECIN_ELEM_DEPL') THEN
        CALL JEVECH('PENERCR','E',JENDE)

C        --- MATRICE DE MASSE ---
        IF (INFODI.EQ.1) THEN
           CALL JEVECH('PCADISM','L',LDIS)
        ELSEIF (INFODI.EQ.2) THEN
           CALL JEVECH('PCADNSM','L',LDIS)
        ENDIF

C        --- GLOBAL VERS LOCAL ? ---
C        --- IREP EQ 1 : MATRICE EN REPERE GLOBAL
C        --- IREP NE 1 : MATRICE EN REPERE LOCAL
        IREP = NINT(ZR(LDIS+NBTERM))
        IF (IREP.EQ.1) THEN
          IF (NDIM.EQ.3) THEN
            IF (INFODI.EQ.1) THEN
              CALL UTPSGL(NNO,NC,PGL,ZR(LDIS),MAT)
            ELSEIF (INFODI.EQ.2) THEN
              CALL UTPPGL(NNO,NC,PGL,ZR(LDIS),MAT)
            ENDIF
          ELSE IF (NDIM.EQ.2) THEN
            IF (INFODI.EQ.1) THEN
              CALL UT2MGL(NNO,NC,PGL,ZR(LDIS),MAT)
            ELSEIF (INFODI.EQ.2) THEN
              CALL UT2PGL(NNO,NC,PGL,ZR(LDIS),MAT)
            ENDIF
          END IF
        ELSE
          DO 20 I = 1,NBTERM
            MAT(I) = ZR(LDIS+I-1)
   20     CONTINUE
        END IF

C        ---- MATRICE RIGIDITE LIGNE > MATRICE RIGIDITE CARRE
        IF (INFODI.EQ.1) THEN
          CALL VECMA(MAT,NBTERM,KLC,NEQ)
        ELSEIF (INFODI.EQ.2) THEN
          CALL VECMAP(MAT,NBTERM,KLC,NEQ)
        ENDIF

C        --- FREQUENCE ---
        CALL JEVECH('PFREQR','L',JFREQ)

C        --- ENERGIE CINETIQUE  ---
        IF = 1
        CALL PTENCI(NEQ,UL,KLC,ZR(JFREQ),ZR(JENDE),ITYPE,KANL,IF)

      ELSE
        CH16 = OPTION
        CI16 = NOMTE
        CALL U2MESK('F','ELEMENTS2_47',1,CH16)
      END IF

      END
