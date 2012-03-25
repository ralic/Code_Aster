      SUBROUTINE HAYMAT ( FAMI, KPG, KSP, MOD,    IMAT,   NMAT,
     1                      MATERD, MATERF, MATCST, TYPMA,    NDT,
     2                      NDI,    NR,     NVI )
      IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/03/2012   AUTEUR PROIX J-M.PROIX 
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
C       ----------------------------------------------------------------
C     HAYHURST      : RECUPERATION DU MATERIAU A T ET T+DT
C                  NB DE CMP DIRECTES/CISAILLEMENT , NB VAR. INTERNES
C                  MATER(*,1) = E , NU , ALPHA
C                  MATER(*,2) = EPS0 , K , H1 , H2 , DELTA1 , DELTA2 ,
C                               H1ST , H2ST , KC , BIGS , SMALLS ,
C                               EPSC
C                  VARIABLES INTERNES : 
C     ----------------------------------------------------------------
C     IN  IMAT   :  ADRESSE DU MATERIAU CODE
C         MOD    :  TYPE DE MODELISATION
C         NMAT   :  DIMENSION  DE MATER
C     OUT MATERD :  COEFFICIENTS MATERIAU A T
C         MATERF :  COEFFICIENTS MATERIAU A T+DT
C                   MATER(*,1) = CARACTERISTIQUES   ELASTIQUES
C                   MATER(*,2) = CARACTERISTIQUES   PLASTIQUES
C         MATCST :  'OUI' SI  MATERIAU A T = MATERIAU A T+DT
C                   'NON' SINON
C         NDT    :  NB TOTAL DE COMPOSANTES TENSEURS
C         NDI    :  NB DE COMPOSANTES DIRECTES  TENSEURS
C         NR     :  NB DE COMPOSANTES SYSTEME NL
C         NVI    :  NB DE VARIABLES INTERNES
C     ----------------------------------------------------------------
      INTEGER       KPG,KSP,NMAT,NDT,NDI,NR,NVI
      INTEGER       IOPTIO,IDNR,I,J,IMAT,CERR(16)
      REAL*8        MATERD(NMAT,2),MATERF(NMAT,2),EPSI,C1D,C2D,R8PREM
      CHARACTER*(*) FAMI
      CHARACTER*8   MOD, NOMC(16) , TYPMA
      CHARACTER*3   MATCST
      CHARACTER*11  METING
C     ----------------------------------------------------------------
      COMMON /OPTI/   IOPTIO , IDNR
      COMMON /METI/   METING
      COMMON /COED/   C1D , C2D
C     ----------------------------------------------------------------
      EPSI=R8PREM()
C
C -   NB DE COMPOSANTES / VARIABLES INTERNES -------------------------
C
      IF (METING(1:11).EQ.'RUNGE_KUTTA')THEN
         NDT = 6
         NDI = 3
         NR  = NDT+3
         NVI = NDT+6
      ELSE IF (MOD(1:2).EQ.'3D')THEN
C -      IMPLICITE 3D
         NDT = 6
         NDI = 3
         NR  = NDT+3
         NVI = NDT+6
      ELSE IF (MOD(1:6).EQ.'D_PLAN'.OR.MOD(1:4).EQ.'AXIS')THEN
C -      IMPLICITE D_PLAN AXIS
         NDT = 4
         NDI = 3
         NR  = NDT+3
         NVI = NDT+6
      ELSE IF (MOD(1:6).EQ.'C_PLAN')THEN
C -      PAS DE C_PLAN
         CALL ASSERT(.FALSE.)
      ENDIF
C
C -   VISCO-PLASTICITE --->  CALCUL DE LA MATRICE DE COMPORTEMENT
C -   TANGENT  'COHERENT'
C
        TYPMA = 'COHERENT'
C
C -   RECUPERATION MATERIAU -----------------------------------------
C
        NOMC(1)  = 'E'
        NOMC(2)  = 'NU'
        NOMC(3)  = 'ALPHA'
        NOMC(4)  = 'EPS0'
        NOMC(5)  = 'K'
        NOMC(6)  = 'H1'
        NOMC(7)  = 'H2'
        NOMC(8)  = 'DELTA1'
        NOMC(9)  = 'DELTA2'
        NOMC(10) = 'H1ST'
        NOMC(11) = 'H2ST'
        NOMC(12) = 'BIGA'
        NOMC(13) = 'SIG0'
        NOMC(14) = 'KC'
        NOMC(15) = 'ALPHAD'
        NOMC(16) = 'S_EQUI_D'
C
        DO  9 J = 1 , 2
        DO  9 I = 1 , NMAT
        MATERD(I,J) = 0.D0
        MATERF(I,J) = 0.D0
 9     CONTINUE
C
C -   RECUPERATION MATERIAU A (T)
C
        CALL RCVALB(FAMI,KPG,KSP,'-',IMAT,' ', 'ELAS',0,' ',
     1              0.D0, 3,NOMC(1),  MATERD(1,1),  CERR(1), 0 )
        IF ( CERR(3) .NE. 0 ) MATERD(3,1) = 0.D0
        CALL RCVALB(FAMI,KPG,KSP,'-',IMAT,' ', 'HAYHURST',0,' ',
     1              0.D0,13,NOMC(4),  MATERD(1,2),  CERR(4), 1 )
C
C -   RECUPERATION MATERIAU A (T+DT)
C
        CALL RCVALB(FAMI,KPG,KSP,'+',IMAT,' ', 'ELAS',  0,' ',
     1              0.D0, 3,NOMC(1),  MATERF(1,1),  CERR(1), 0 )
        IF ( CERR(3) .NE. 0 ) MATERF(3,1) = 0.D0
        CALL RCVALB(FAMI,KPG,KSP,'+',IMAT,' ', 'HAYHURST', 0,' ',
     1              0.D0, 13,NOMC(4),  MATERF(1,2),  CERR(4), 1 )
C
C -   MATERIAU CONSTANT ?
C
      MATCST = 'OUI'
      DO 30 I = 1,2
        IF (ABS(MATERD(I,1)-MATERF(I,1)).GT.EPSI*MATERD(I,1) )THEN
        MATCST = 'NON'
        GOTO 9999
        ENDIF
 30   CONTINUE
      DO 40 I = 1,25
        IF (ABS(MATERD(I,2)-MATERF(I,2)).GT.EPSI*MATERD(I,2) )THEN
        MATCST = 'NON'
        GOTO 9999
        ENDIF
 40   CONTINUE
C
 9999 CONTINUE
      END
