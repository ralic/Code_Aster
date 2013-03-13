      SUBROUTINE HAYMAT(FAMI,KPG,KSP,MOD,IMAT,NMAT,POUM,
     &                  COEFEL,COEFPL,NVI,NR)
      IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 11/03/2013   AUTEUR PROIX J-M.PROIX 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C       ----------------------------------------------------------------
C     HAYHURST   : RECUPERATION DU MATERIAU A T ET T+DT
C                  NB DE CMP DIRECTES/CISAILLEMENT , NB VAR. INTERNES
C                  MATER(*,1) = E , NU , ALPHA
C                  MATER(*,2) = EPS0 , K , H1 , H2 , DELTA1 , DELTA2 ,
C                               H1ST , H2ST , KC , BIGS , SMALLS ,
C                               EPSC
C                  VARIABLES INTERNES :
C     ----------------------------------------------------------------
C     IN  FAMI   :  FAMILLE DE POINT DE GAUSS (RIGI,MASS,...)
C         KPG,KSP:  NUMERO DU (SOUS)POINT DE GAUSS
C         MOD    :  TYPE DE MODELISATION
C         IMAT   :  ADRESSE DU MATERIAU CODE
C         NMAT   :  DIMENSION  DE MATER
C         POUM   :  '+' OU '-'
C     OUT COEFEL :  COEFFICIENTS MATERIAU POUR LA PARTIE ELASTIQUE
C         COEFPL :  COEFFICIENTS MATERIAU POUR LA PARTIE NON LINEAIRE
C         NDT    :  NB TOTAL DE COMPOSANTES TENSEURS
C         NDI    :  NB DE COMPOSANTES DIRECTES  TENSEURS
C         NR     :  NB DE COMPOSANTES SYSTEME NL
C         NVI    :  NB DE VARIABLES INTERNES
C     ----------------------------------------------------------------
      INTEGER      KPG,KSP,NMAT,NVI,IMAT,CERR(16),NR,NDT,NDI
      REAL*8        COEFEL(NMAT),COEFPL(NMAT)
      CHARACTER*(*) FAMI,POUM
      CHARACTER*8   MOD, NOMC(16)
C     ----------------------------------------------------------------
      COMMON /TDIM/   NDT  , NDI
C     ----------------------------------------------------------------
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
C
C -   RECUPERATION MATERIAU A (T)
C
        CALL RCVALB(FAMI,KPG,KSP,POUM,IMAT,' ', 'ELAS',0,' ',
     1              0.D0, 3,NOMC(1),  COEFEL,  CERR(1), 0 )

        IF ( CERR(3) .NE. 0 ) COEFEL(3) = 0.D0

        CALL RCVALB(FAMI,KPG,KSP,POUM,IMAT,' ', 'HAYHURST',0,' ',
     1              0.D0,13,NOMC(4),  COEFPL,  CERR(4), 1 )
        
C     NOMBRE DE COEF MATERIAU
      COEFPL(NMAT)=15
      
      IF (MOD(1:2).EQ.'3D') THEN
C =================================================================
C - MODELISATION DE TYPE 3D ---------------------------------------
C =================================================================
          NDT = 6
          NDI = 3
      ELSE IF ( MOD(1:6).EQ.'D_PLAN'.OR.
     &          MOD(1:4).EQ.'AXIS'  .OR.
     &          MOD(1:6).EQ.'C_PLAN'     ) THEN
C =================================================================
C - D_PLAN AXIS C_PLAN --------------------------------------------
C =================================================================
          NDT = 4
          NDI = 3
      ENDIF
C =================================================================
C - NOMBRE DE VARIABLES INTERNES 
C =================================================================
      NVI = 12
      NR=NDT+4
C     ON PEUT DIMINUER : NR=NDT+2

      END
