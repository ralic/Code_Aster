      SUBROUTINE LCMAEI (FAMI,KPG,KSP,POUM,NMATER,IMAT,NECRIS,NECOUL,
     &            NBVAL,VALRES,NMAT,HSR,IFA,NOMFAM,NBSYS,NBHSR)
      IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 23/04/2007   AUTEUR PROIX J-M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     ----------------------------------------------------------------
C     MONOCRISTAL : RECUPERATION DU MATERIAU A T(TEMPD) ET T+DT(TEMPF)
C                  MATER(*,2) = COEF ECRO ISOT ET CALCUL DE LA
C                  MATRICE D'INTERACTION HSR
C     ----------------------------------------------------------------
C     IN  IMAT   :  ADRESSE DU MATERIAU CODE
C         NMATER :  NOM DU MATERIAU
C         NMAT   :  DIMENSION  DE MATER
C         NECRIS :  NOM DE LA LOI D'ECOULEMENT
C         IFA    :  NUMERO DE LA FAMILLE DE GLISSEMENT
C         NBCOMM :  NOMBRE DE COEF MATERIAU PAR FAMILLE
C     OUT VALRES :  COEFFICIENTS MATERIAU
C     OUT NBVAL  :  NB DE COEFFICIENTS MATERIAU
C     OUT HSR    :  MATRICE D'INTERACTION
C     ----------------------------------------------------------------
      INTEGER         KPG,KSP
      INTEGER         NMAT,NBVAL,IMAT,I,NBSYS,IS,IR,IFA,J,NBHSR,NBCOEF
      REAL*8          MATER(NMAT,2),VALH(6)
      REAL*8          VALRES(NMAT),HSRI(24,24),H
      REAL*8          HSR(5,24,24)
      CHARACTER*(*)   FAMI,POUM
      CHARACTER*8     NOMRES(NMAT)
      CHARACTER*2     CODRET(NMAT)
      CHARACTER*16    NMATER, NECRIS, NOMFAM,NECOUL
C     ----------------------------------------------------------------
C
      IF (NECRIS.EQ.'ECRO_ISOT1') THEN
          NBVAL=3
          NOMRES(1)='R_0'
          NOMRES(2)='Q'
          NOMRES(3)='B'
          CALL RCVALB (FAMI,KPG,KSP,POUM,IMAT,NMATER, NECRIS,0,' ',0.D0,
     &                 3,NOMRES, VALRES,CODRET,'FM')
      ELSEIF (NECRIS.EQ.'ECRO_ISOT2') THEN
          NBVAL=5
          NOMRES(1)='R_0'
          NOMRES(2)='Q1'
          NOMRES(3)='B1'
          NOMRES(4)='Q2'
          NOMRES(5)='B2'
          CALL RCVALB (FAMI,KPG,KSP,POUM,IMAT,NMATER, NECRIS,0,' ',0.D0,
     &                 NBVAL,NOMRES, VALRES,CODRET,'FM')
      ENDIF
C     DEFINITION DE LA MATRICE D'INTERACTION
      NOMRES(1)='H'
      NBVAL=NBVAL+1
      VALRES(NBVAL)=NBHSR
      CALL RCVALB (FAMI,KPG,KSP,POUM,IMAT,NMATER, NECRIS,0,' ',0.D0,
     &             1, NOMRES, H,CODRET,' ')
      IF(NECOUL.EQ.'KOCKS_RAUCH') THEN
      CALL RCVALB (FAMI,KPG,KSP,POUM,IMAT,NMATER, NECOUL,0,' ',0.D0,
     &             1, NOMRES, H,CODRET,' ')
      ENDIF
      IF (CODRET(1).EQ.'OK') THEN
          NBCOEF=1                          
          VALH(1)=H                                        
      ELSE
          NOMRES(1)='H1'
          NOMRES(2)='H2'
          NOMRES(3)='H3'
          NOMRES(4)='H4'
          NOMRES(5)='H5'
          NOMRES(6)='H6'

          IF(NECOUL.EQ.'KOCKS_RAUCH') THEN
          CALL RCVALB (FAMI,KPG,KSP,POUM,IMAT,NMATER, NECOUL,0,' ',0.D0,
     &               6,NOMRES, VALH,CODRET,' ')
          ELSE
          CALL RCVALB (FAMI,KPG,KSP,POUM,IMAT,NMATER, NECRIS,0,' ',0.D0,
     &               6,NOMRES, VALH,CODRET,' ')
          ENDIF
          
          IF (CODRET(5).EQ.'OK') THEN
              NBCOEF=6                          
          ELSE
              NBCOEF=4
          ENDIF                          

      ENDIF
      
      CALL LCMHSR (NBSYS, NBCOEF, VALH, HSRI)
      DO 1 I=1,24
      DO 2 J=1,24
          HSR(NBHSR,I,J)=HSRI(I,J)
  2   CONTINUE
  1   CONTINUE


      END
