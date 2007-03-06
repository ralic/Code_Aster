      SUBROUTINE LCMAFL (NMATER,IMAT,NECOUL,NBVAL,NBPAR,NOMPAR,
     &                   VALPAR,VALRES,NMAT)
      IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 05/03/2007   AUTEUR ELGHARIB J.EL-GHARIB 
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
C                  MATER(*,2) = COEF ECOULEMENT VISCOPLASTIQUE
C     ----------------------------------------------------------------
C     IN  IMAT   :  ADRESSE DU MATERIAU CODE
C         NMATER :  NOM DU MATERIAU
C         NMAT   :  DIMENSION  DE MATER
C         NECOUL :  NOM DE LA LOI D'ECOULEMENT
C         VALPAR :  VALEUR DES PARAMETRES
C         NOMPAR :  NOM DES PARAMETRES
C     OUT VALRES :  COEFFICIENTS MATERIAU A T
C         NBVAL  :  NOMBRE DE COEF MATERIAU LUS
C     ----------------------------------------------------------------
      INTEGER         NMAT,NBPAR,NVINI,I,IFA,IMAT,NBVAL
      REAL*8          VALPAR(NBPAR),VALRES(NMAT)
      CHARACTER*8     NOMPAR(NBPAR),NOMRES(NMAT)
      CHARACTER*2     CODRET(NMAT)
      CHARACTER*16    NMATER, NECOUL
C     ----------------------------------------------------------------
C
      IF (NECOUL.EQ.'ECOU_VISC1') THEN
          NBVAL=3
          NOMRES(1)='N'
          NOMRES(2)='K'
          NOMRES(3)='C'
          CALL RCVALA (IMAT,NMATER, NECOUL,1, NOMPAR,VALPAR,NBVAL,
     1                 NOMRES, VALRES,CODRET,'FM')

      ENDIF
      IF (NECOUL.EQ.'ECOU_VISC2') THEN
          NBVAL=5
          NOMRES(1)='N'
          NOMRES(2)='K'
          NOMRES(3)='C'
          NOMRES(4)='A'
          NOMRES(5)='D'
          CALL RCVALA (IMAT,NMATER, NECOUL,1, NOMPAR,VALPAR,NBVAL,
     1                 NOMRES, VALRES,CODRET,'FM')

      ENDIF
      IF (NECOUL.EQ.'ECOU_VISC3') THEN
          NBVAL=5
          NOMRES(1)='K'
          NOMRES(2)='TAUMU'
          NOMRES(3)='GAMMA0'
          NOMRES(4)='DELTAV'
          NOMRES(5)='DELTAG0'
          CALL RCVALA (IMAT,NMATER, NECOUL,1, NOMPAR,VALPAR,NBVAL,
     1                 NOMRES, VALRES,CODRET,'FM')

      ENDIF
      IF (NECOUL.EQ.'KOCKS_RAUCH') THEN
          NBVAL=10
          NOMRES(1)='K'
          NOMRES(2)='TAUR'
          NOMRES(3)='TAU0'
          NOMRES(4)='GAMMA0'
          NOMRES(5)='DELTAG0'
          NOMRES(6)='BSD'
          NOMRES(7)='GCB'
          NOMRES(8)='KDCS'
          NOMRES(9)='P'
          NOMRES(10)='Q'
          CALL RCVALA (IMAT,NMATER, NECOUL,1, NOMPAR,VALPAR,NBVAL,
     1                 NOMRES, VALRES,CODRET,'FM')

      ENDIF
      IF (NECOUL.EQ.'ECOU_PLAS1') THEN
          NBVAL=1
          NOMRES(1)='C'
          CALL RCVALA (IMAT,NMATER, NECOUL,1, NOMPAR,VALPAR,NBVAL,
     1                 NOMRES, VALRES,CODRET,'FM')

      ENDIF
      END
