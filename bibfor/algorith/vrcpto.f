      SUBROUTINE VRCPTO (COMPOR,DEPS,NEPS,FAMI,KPG,KSP,IMATE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 11/10/2011   AUTEUR MEUNIER S.MEUNIER 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C
C
      IMPLICIT NONE
      INTEGER            IMATE,KPG,KSP
      CHARACTER*(*)      FAMI
      CHARACTER*16       COMPOR(*)
      INTEGER            NEPS
      REAL*8             DEPS(NEPS)
C ----------------------------------------------------------------------
C     ON CALCULE LA DEFORMATION MECANIQUE ASSOCIEE A LA VARIABLE DE
C     COMMANDE PTOT.
C     ON LA RETRANCHE ENSUITE AUX DEFORMATIONS MECANIQUES TOTALES DEPS
C
C
C IN COMPOR   : COMPORTEMENT DE L ELEMENT
C                COMPOR(1) = RELATION DE COMPORTEMENT (VMIS_...)
C                COMPOR(2) = NB DE VARIABLES INTERNES
C                COMPOR(3) = TYPE DE DEFORMATION (PETIT,GREEN...)
C IN/OUT DEPS : INCREMENT DE DEFORMATION
C IN  NEPS    : NOMBRE DE CMP DE DEPS (SUIVANT MODELISATION)
C IN  FAMI    : FAMILLE DE POINTS DE GAUSS
C IN  KPG,KSP : NUMERO DU (SOUS)POINT DE GAUSS
C IN  IMATE   : ADRESSE DU MATERIAU CODE
C
      INTEGER ICODRE(2)
      CHARACTER*8     NOMRES(2)
      REAL*8          VALRES(2)
      REAL*8          PTOTM,PTOTP,BIOTP,BIOTM,EM,NUM,EP,NUP,
     &                TROIKP,TROIKM
      INTEGER         IRET1,IRET2,K

      LOGICAL         LPOMEC
      INTEGER         DMMECA,II
      PARAMETER     ( DMMECA = 19 )
      CHARACTER*16    POMECA(DMMECA)

      DATA POMECA / 'ELAS'            ,
     &              'CJS'             ,
     &              'HUJEUX'          ,
     &              'CAM_CLAY'        ,
     &              'BARCELONE'       ,
     &              'LAIGLE'          ,
     &              'LETK'            ,
     &              'VISC_DRUC_PRAG'  ,
     &              'HOEK_BROWN_EFF'  ,
     &              'HOEK_BROWN_TOT'  ,
     &              'MAZARS'          ,
     &              'ENDO_ISOT_BETON' ,
     &              'ELAS_GONF'       ,
     &              'DRUCK_PRAGER'    ,
     &              'DRUCK_PRAG_N_A'  ,
     &              'JOINT_BANDIS'    ,
     &              'CZM_LIN_REG'     ,
     &              'CZM_EXP_REG'     ,
     &              'ENDO_HETEROGENE' /

C
C DEB ------------------------------------------------------------------
C
      CALL RCVARC(' ','PTOT','-',FAMI,KPG,KSP,PTOTM,IRET1)
      CALL RCVARC(' ','PTOT','+',FAMI,KPG,KSP,PTOTP,IRET2)

      IF ((IRET1.EQ.1).AND.(IRET2.EQ.1)) GOTO 9999

      IF (IRET1.NE.IRET2) THEN
        CALL U2MESS('F','CHAINAGE_11')  
      ENDIF

      IF ((IRET1.EQ.0).AND.(IRET2.EQ.0)) THEN

        LPOMEC = .FALSE.
        DO 1 II = 1, DMMECA
          IF (COMPOR(1).EQ.POMECA(II))  LPOMEC = .TRUE.
 1      CONTINUE

        IF (.NOT.LPOMEC) CALL U2MESK('F','CHAINAGE_9',1,COMPOR(1))

        IF (COMPOR(3).NE.'PETIT') CALL U2MESS('F','CHAINAGE_8')

C
C --- COEFFICIENT DE BIOT
C
        NOMRES(1)='BIOT_COE'

        CALL RCVALB(FAMI,KPG,KSP,'-',IMATE,' ','THM_DIFFU',0,' ',0.D0,1,
     +            NOMRES(1),VALRES(1),ICODRE,1 )
        IF ( ICODRE(1) .NE. 0 ) VALRES(1) = 0.D0
        BIOTM = VALRES(1)
C
        CALL RCVALB(FAMI,KPG,KSP,'+',IMATE,' ','THM_DIFFU',0,' ',0.D0,1,
     +            NOMRES(1),VALRES(1),ICODRE,1 )
        IF ( ICODRE(1) .NE. 0 ) VALRES(1) = 0.D0
        BIOTP = VALRES(1)
C
C --- MODULE DE YOUNG ET COEFFICIENT DE POISSON
C
        NOMRES(1)='E'
        NOMRES(2)='NU'

        CALL RCVALB(FAMI,KPG,KSP,'-',IMATE,' ','ELAS',0,' ',0.D0,
     +            2,NOMRES,VALRES,ICODRE, 2 )
        IF ( ICODRE(1) .NE. 0 ) VALRES(1) = 0.D0
        IF ( ICODRE(2) .NE. 0 ) VALRES(2) = 0.D0
        EM  = VALRES(1)
        NUM = VALRES(2)

        CALL RCVALB(FAMI,KPG,KSP,'+',IMATE,' ','ELAS',0,' ',0.D0,
     +            2,NOMRES,VALRES,ICODRE, 2)
        IF ( ICODRE(1) .NE. 0 ) VALRES(1) = 0.D0
        IF ( ICODRE(2) .NE. 0 ) VALRES(2) = 0.D0
        EP  = VALRES(1)
        NUP = VALRES(2)

        TROIKP = EP/(1.D0-2.D0*NUP)
        TROIKM = EM/(1.D0-2.D0*NUM)
C
C --- CALCUL DE LA DEFORMATION TOTALE ACTUALISEE
C
        DO 10 K=1,3
          DEPS(K) = DEPS(K)-(BIOTP/TROIKP*PTOTP-BIOTM/TROIKM*PTOTM)
 10     CONTINUE
C
      ENDIF
C
 9999 CONTINUE
C
      END
