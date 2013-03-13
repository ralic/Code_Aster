      SUBROUTINE LCMAFL (FAMI, KPG, KSP, POUM, NMATER,IMAT,NECOUL,
     &             NBVAL,VALRES,NMAT,ITBINT,NFS,NSG,HSRI,NBSYS)
      IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 11/03/2013   AUTEUR PROIX J-M.PROIX 
C TOLE CRS_1404
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
      INTEGER         KPG,KSP,NMAT,IMAT,NBVAL,NBCOEF,ITBINT,NFS,NSG
      INTEGER         IRET2,NBSYS
      REAL*8          VALRES(NMAT),HSRI(NSG,NSG),H,E,NU,MU
      REAL*8          TEMPF,VALH(6),VALLUE(NMAT)
      CHARACTER*8     NOMRES(NMAT)
      INTEGER ICODRE(NMAT)
      CHARACTER*(*)   FAMI,POUM
      CHARACTER*16    NMATER, NECOUL, NECRIS,PHENOM
      INTEGER IRR,DECIRR,NBSYST,DECAL,GDEF
C     ----------------------------------------------------------------
      COMMON/POLYCR/IRR,DECIRR,NBSYST,DECAL,GDEF
C     ----------------------------------------------------------------
C
      IF (NECOUL.EQ.'MONO_VISC1') THEN
          NBVAL=3
          NOMRES(1)='N'
          NOMRES(2)='K'
          NOMRES(3)='C'
          CALL RCVALB (FAMI,KPG,KSP,POUM,IMAT,NMATER, NECOUL,0,' ',0.D0,
     &                 NBVAL,NOMRES, VALLUE,ICODRE,1)
          CALL LCEQVN ( NBVAL , VALLUE  , VALRES(2) )
          NBVAL=NBVAL+1
C         PAR CONVENTION ECOU_VISC1 A LE NUMERO 1
          VALRES(1)=1

      ENDIF
      IF (NECOUL.EQ.'MONO_VISC2') THEN
          NBVAL=5
          NOMRES(1)='N'
          NOMRES(2)='K'
          NOMRES(3)='C'
          NOMRES(4)='A'
          NOMRES(5)='D'
          CALL RCVALB (FAMI,KPG,KSP,POUM,IMAT,NMATER, NECOUL,0,' ',0.D0,
     &                 NBVAL,NOMRES, VALLUE,ICODRE,1)
          CALL LCEQVN ( NBVAL , VALLUE  , VALRES(2) )
          NBVAL=NBVAL+1
C         PAR CONVENTION ECOU_VISC2 A LE NUMERO 2
          VALRES(1)=2

      ENDIF
      IF (NECOUL.EQ.'MONO_DD_CFC') THEN
          NBVAL=6
          NOMRES(1)='TAU_F'
          NOMRES(2)='GAMMA0'
          NOMRES(3)='A'
          NOMRES(4)='B'
          NOMRES(5)='N'
          NOMRES(6)='Y'
          CALL RCVALB (FAMI,KPG,KSP,POUM,IMAT,NMATER, NECOUL,0,' ',0.D0,
     &                 NBVAL,NOMRES, VALLUE,ICODRE,1)
          CALL LCEQVN ( NBVAL , VALLUE  , VALRES(2) )
          NBVAL=NBVAL+1
C         PAR CONVENTION ECOU_DD_CFC A LE NUMERO 5
          VALRES(1)=5
          NBVAL=NBVAL+1
          VALRES(NBVAL)=0.D0
      ENDIF
      IF (NECOUL.EQ.'MONO_DD_CFC_IRRA') THEN
          NBVAL=6
          NOMRES(1)='TAU_F'
          NOMRES(2)='GAMMA0'
          NOMRES(3)='A'
          NOMRES(4)='B'
          NOMRES(5)='N'
          NOMRES(6)='Y'

          CALL RCVALB (FAMI,KPG,KSP,POUM,IMAT,NMATER, NECOUL,0,' ',0.D0,
     &                 NBVAL,NOMRES, VALLUE,ICODRE,1)
          CALL LCEQVN ( NBVAL , VALLUE  , VALRES(2) )
          NBVAL=NBVAL+1
C         PAR CONVENTION ECOU_DD_CFC_IRRA A LE NUMERO 8
          VALRES(1)=8
          NBVAL=NBVAL+1
          VALRES(NBVAL)=0.D0
      ENDIF
      IF (NECOUL.EQ.'MONO_DD_FAT') THEN
          NBVAL=7
          NOMRES(1)='TAU_F'
          NOMRES(2)='GAMMA0'
          NOMRES(3)='BETA'
          NOMRES(4)='UN_SUR_D'
          NOMRES(5)='N'
          NOMRES(6)='GC0'
          NOMRES(7)='K'
          CALL RCVALB (FAMI,KPG,KSP,POUM,IMAT,NMATER, NECOUL,0,' ',0.D0,
     &                 NBVAL,NOMRES, VALLUE,ICODRE,1)
          CALL LCEQVN ( NBVAL , VALLUE  , VALRES(2) )
          NBVAL=NBVAL+1
C         PAR CONVENTION ECOU_ECP_CFC A LE NUMERO 6
          VALRES(1)=6

          NBVAL=NBVAL+1
          VALRES(NBVAL)=0.D0

      ENDIF
      IF (NECOUL(1:10).EQ.'MONO_DD_CC') THEN
          NBVAL=18
          NOMRES(1)='B'
          NOMRES(2)='GH'
          NOMRES(3)='DELTAG0'
          NOMRES(4)='TAU_0'
          NOMRES(5)='D'
          NOMRES(6)='GAMMA0'
          NOMRES(7)='N'
          NOMRES(8)='BETA'
          NOMRES(9)='Y_AT'
          NOMRES(10)='D_LAT'
          NOMRES(11)='K_F'
          NOMRES(12)='K_SELF'
          NOMRES(13)='TAU_F'
          NOMRES(14)='RHO_MOB'
          NOMRES(15)='K_BOLTZ'
          NOMRES(16)='DELTA1'
          NOMRES(17)='DELTA2'
          NOMRES(18)='DEPDT'
          CALL RCVALB (FAMI,KPG,KSP,POUM,IMAT,NMATER, NECOUL,0,' ',0.D0,
     &                 NBVAL,NOMRES, VALLUE,ICODRE,1)

C         CALCUL ET STOCKAGE DE MU
          CALL RCCOMA(IMAT,'ELAS',1,PHENOM,ICODRE)

          IF (PHENOM.EQ.'ELAS') THEN
             CALL RCVALB(FAMI,KPG,KSP,POUM,IMAT,' ','ELAS',0,' ', 0.D0,
     &                   1,'E',E,ICODRE,1)
             CALL RCVALB(FAMI,KPG,KSP,POUM,IMAT,' ','ELAS',0,' ', 0.D0,
     &                   1,'NU',NU,ICODRE,1)
             MU=E/(2.0D0+2.0D0*NU)
          ELSE
             CALL RCVALB(FAMI,KPG,KSP,POUM,IMAT,' ',PHENOM,0,' ', 0.D0,
     &                   1,'G_LN',MU,ICODRE,1)
          ENDIF
          CALL RCVARC('F','TEMP',POUM,FAMI,KPG,KSP,TEMPF,IRET2)
          IF (IRET2.NE.0) THEN
             CALL U2MESG('F','COMPOR1_82',1,NECOUL,0,0,0,0.D0)
          ENDIF

          NBVAL=NBVAL+1
          VALLUE(NBVAL)=TEMPF
          NBVAL=NBVAL+1
          VALLUE(NBVAL)=MU
          NBVAL=NBVAL+1
          VALLUE(NBVAL)=0.D0
          IRR=0
          IF (NECOUL.EQ.'MONO_DD_CC_IRRA') THEN
             IRR=1
             VALLUE(NBVAL)=1.D0
             NOMRES(1)='A_IRRA'
             NOMRES(2)='XI_IRRA'
             CALL RCVALB (FAMI,KPG,KSP,POUM,IMAT,NMATER, NECOUL,0,' ',
     &                 0.D0,2,NOMRES,VALLUE(NBVAL+1),ICODRE,1)
             NBVAL=NBVAL+2
          ENDIF
          CALL LCEQVN ( NBVAL , VALLUE  , VALRES(2) )
C         PAR CONVENTION ECOU_DD_CC A LE NUMERO 7
          NBVAL=NBVAL+1
          VALRES(1)=7
          NBVAL=NBVAL+1
          VALRES(NBVAL)=0.D0
      ENDIF

      IF (NECOUL.EQ.'MONO_DD_KR') THEN
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
          CALL RCVALB (FAMI,KPG,KSP,POUM,IMAT,NMATER, NECOUL,0,' ',0.D0,
     &                 NBVAL,NOMRES, VALLUE,ICODRE,1)
          CALL LCEQVN ( NBVAL , VALLUE  , VALRES(2) )
          NBVAL=NBVAL+1
C         PAR CONVENTION KOCKS_RAUCH A LE NUMERO 4
          VALRES(1)=4

          CALL RCVARC('F','TEMP',POUM,FAMI,KPG,KSP,TEMPF,IRET2)
          IF (IRET2.NE.0) THEN
             CALL U2MESG('F','COMPOR1_82',1,NECOUL,0,0,0,0.D0)
          ENDIF
          NBVAL=NBVAL+1
          VALRES(NBVAL)=TEMPF


C         DEFINITION DE LA MATRICE D'INTERACTION POUR KOCKS-RAUCH
          NOMRES(1)='H'
          CALL RCVALB (FAMI,KPG,KSP,POUM,IMAT,NMATER, NECOUL,0,' ',0.D0,
     &                 1, NOMRES, H,ICODRE,0)
          IF (ICODRE(1).EQ.0) THEN
              NBCOEF=1
              VALH(1)=H
          ELSE
              NOMRES(1)='H1'
              NOMRES(2)='H2'
              NOMRES(3)='H3'
              NOMRES(4)='H4'
              NOMRES(5)='H5'
              NOMRES(6)='H6'
              CALL RCVALB (FAMI,KPG,KSP,POUM,IMAT,NMATER, NECOUL,
     &                   0,' ',0.D0,6,NOMRES,VALH,ICODRE,0)
              IF (ICODRE(5).EQ.0) THEN
                  NBCOEF=6
              ELSE
                  NBCOEF=4
              ENDIF

          ENDIF
          IF (ITBINT.EQ.0) THEN
             NECRIS=NECOUL
             CALL LCMHSR (NECOUL,NECRIS,NBSYS, NBCOEF, VALH, NSG,HSRI)
          ENDIF

      ENDIF
      END
