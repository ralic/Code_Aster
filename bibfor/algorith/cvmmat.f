        SUBROUTINE CVMMAT ( FAMI, KPG, KSP, MOD,    IMAT,   NMAT,
     &                      MATERD, MATERF, MATCST, TYPMA,    NDT,
     &                      NDI,    NR,     NVI )
        IMPLICIT NONE
C       ================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 20/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
C       ----------------------------------------------------------------
C    VISCOCHABOCHE : RECUPERATION DU MATERIAU A T ET T+DT
C                    NB DE CMP DIRECTES/CISAILLEMENT , NB VAR. INTERNES
C                    MATER(*,1) = E , NU , ALPHA
C                    MATER(*,2) = K_0  , A_K , A_R , K    , N   , ALP
C                                 B    , M_R , G_R , MU   , Q_M , Q_0
C                                 QR_0 , ETA , A_I
C                                 M_1  , D1  , G_X1 ,G1_0 , C1
C                                 M_2  , D2  , G_X2 ,G2_0 , C2
C                    VARIABLES INTERNES : X1 , X2 , P , R , Q , XXI , E
C       ----------------------------------------------------------------
C       IN  IMAT   :  ADRESSE DU MATERIAU CODE
C           MOD    :  TYPE DE MODELISATION
C           NMAT   :  DIMENSION  DE MATER
C       OUT MATERD :  COEFFICIENTS MATERIAU A T
C           MATERF :  COEFFICIENTS MATERIAU A T+DT
C                     MATER(*,1) = CARACTERISTIQUES   ELASTIQUES
C                     MATER(*,2) = CARACTERISTIQUES   PLASTIQUES
C           MATCST :  'OUI' SI  MATERIAU A T = MATERIAU A T+DT
C                     'NON' SINON
C           NDT    :  NB TOTAL DE COMPOSANTES TENSEURS
C           NDI    :  NB DE COMPOSANTES DIRECTES  TENSEURS
C           NR     :  NB DE COMPOSANTES SYSTEME NL
C           NVI    :  NB DE VARIABLES INTERNES
C       ----------------------------------------------------------------
        INTEGER         KPG, KSP, NMAT, NDT , NDI  , NR , NVI
        INTEGER         IOPTIO, IDNR, I, J, IMAT
        REAL*8          MATERD(NMAT,2) ,MATERF(NMAT,2)
        REAL*8          EPSI , C1D , C2D
        CHARACTER*(*)   FAMI
        CHARACTER*8     MOD , NOMC(28) , TYPMA
      INTEGER CERR(28)
        CHARACTER*3     MATCST
        CHARACTER*11    METING
C       ----------------------------------------------------------------
        COMMON /OPTI/   IOPTIO , IDNR
        COMMON /METI/   METING
        COMMON /COED/   C1D , C2D
C       ----------------------------------------------------------------
        DATA EPSI       /1.D-15/
C
C -     NB DE COMPOSANTES / VARIABLES INTERNES -------------------------
C
CJMP    APPAREMMENT, NVI EST LE NOMBRE MAXI DE VARIABLES INTERNES
C       X1(6), X2(6), P, R, Q + XXI(6) SI ETA.NE.1.D0 + INDICATEUR
C       CE QUI CONDUIT A NDT+NDT+3(+NDT)+1
C       DU COUP NR DOIT VALOIR :
C          NDT (SIGMA) + 2*NDT+3 SI ETA.EQ.1.D0
C       ET NDT (SIGMA) + 3*NDT+3 SI ETA.NE.1.D0
C -     POUR INTEGRATION PAR METHODE EXPLICITE
C -     ON RESTE DIMENSIONNE EN 3D
C
                IF      (METING(1:11).EQ.'RUNGE_KUTTA')THEN
                NDT = 6
                NDI = 3
                NR  = 3*NDT+3
                NVI = 4*NDT+4
C - 3D
                ELSE IF (MOD(1:2).EQ.'3D')THEN
                NDT = 6
                NDI = 3
                NR  = 3*NDT+3
                NVI = 3*NDT+4
C - D_PLAN AXIS
                ELSE IF (MOD(1:6).EQ.'D_PLAN'.OR.MOD(1:4).EQ.'AXIS')THEN
                NDT = 4
                NDI = 3
                NR  = 3*NDT+3
                NVI = 3*NDT+4
C - C_PLAN
                ELSE IF (MOD(1:6).EQ.'C_PLAN')THEN
                NDT = 4
                NDI = 3
                NR  = 3*NDT+4
                NVI = 3*NDT+4
                ENDIF
C
C
C -     VISCO-PLASTICITE --->  CALCUL DE LA MATRICE DE COMPORTEMENT
C -     TANGENT  'COHERENT'
C
          TYPMA = 'COHERENT'
C
C -     RECUPERATION MATERIAU -----------------------------------------
C
          NOMC(1)  = 'E       '
          NOMC(2)  = 'NU      '
          NOMC(3)  = 'ALPHA   '
          NOMC(4)  = 'K_0     '
          NOMC(5)  = 'A_K     '
          NOMC(6)  = 'A_R     '
          NOMC(7)  = 'K       '
          NOMC(8)  = 'N       '
          NOMC(9)  = 'ALP     '
          NOMC(10) = 'B       '
          NOMC(11) = 'M_R     '
          NOMC(12) = 'G_R     '
          NOMC(13) = 'MU      '
          NOMC(14) = 'Q_M     '
          NOMC(15) = 'Q_0     '
          NOMC(16) = 'QR_0    '
          NOMC(17) = 'ETA     '
          NOMC(18) = 'C1      '
          NOMC(19) = 'M_1     '
          NOMC(20) = 'D1      '
          NOMC(21) = 'G_X1    '
          NOMC(22) = 'G1_0    '
          NOMC(23) = 'C2      '
          NOMC(24) = 'M_2     '
          NOMC(25) = 'D2      '
          NOMC(26) = 'G_X2    '
          NOMC(27) = 'G2_0    '
          NOMC(28) = 'A_I     '
C
          DO  9 J = 1 , 2
          DO  9 I = 1 , NMAT
          MATERD(I,J) = 0.D0
          MATERF(I,J) = 0.D0
 9       CONTINUE
C
C -     RECUPERATION MATERIAU A (T)
C
          CALL RCVALB(FAMI,KPG,KSP,'-',IMAT,' ', 'ELAS',0,' ',
     &                0.D0, 3,NOMC(1),  MATERD(1,1),  CERR(1), 0)
          IF ( CERR(3) .NE. 0 ) MATERD(3,1) = 0.D0
          CALL RCVALB(FAMI,KPG,KSP,'-',IMAT,' ', 'VISCOCHAB',0,' ',
     &                0.D0,25,NOMC(4),  MATERD(1,2),  CERR(4), 2)
C
C -     MISE A JOUR DU COMMUN COED POUR TRAITER LE CAS ANISOTHERME
C
        C1D      = MATERD(15,2)
        C2D      = MATERD(20,2)
C
C -     RECUPERATION MATERIAU A (T+DT)
C
          CALL RCVALB(FAMI,KPG,KSP,'+',IMAT,' ', 'ELAS',  0,' ',
     &                0.D0, 3,NOMC(1),  MATERF(1,1),  CERR(1), 0)
          IF ( CERR(3) .NE. 0 ) MATERF(3,1) = 0.D0
          CALL RCVALB(FAMI,KPG,KSP,'+',IMAT,' ', 'VISCOCHAB', 0,' ',
     &                0.D0, 25,NOMC(4),  MATERF(1,2),  CERR(4), 2)
C
C -     PARAMETRES DES LOIS DE COMPORTEMENT A 2 SEUILS
C
C       SI ETA=1, PAS DE MEMOIRE D'ECROUISSAGE
        IF ( MATERD(14,2) .EQ. 1.D0 ) THEN
          IOPTIO = 0
          IDNR = 0
        ELSE
          IOPTIO = 2
          IDNR = NDT
          NR=NR+NDT
        ENDIF
C
C -     MATERIAU CONSTANT ?
C
        MATCST = 'OUI'
        DO 30 I = 1,2
          IF ( ABS ( MATERD(I,1) - MATERF(I,1) ) .GT. EPSI )THEN
          MATCST = 'NON'
          GOTO 9999
          ENDIF
 30     CONTINUE
        DO 40 I = 1,25
          IF ( ABS ( MATERD(I,2) - MATERF(I,2) ) .GT. EPSI )THEN
          MATCST = 'NON'
          GOTO 9999
          ENDIF
 40     CONTINUE
C
 9999   CONTINUE
        END
