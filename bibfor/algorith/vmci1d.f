      SUBROUTINE VMCI1D(FAMI,KPG,KSP,IMATE,EM,EP,SIGM,
     &                  DEPS,VIM,OPTION,MATERI,SIGP,VIP,DSDE)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 23/07/2012   AUTEUR FLEJOU J-L.FLEJOU 
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
C RESPONSABLE FLEJOU J-L.FLEJOU
      IMPLICIT NONE
      INTEGER        KPG,KSP,IMATE
      REAL*8         EP,EM,SIGM,DEPS,SIGP,DSDE
      REAL*8         VIM(*),VIP(*)
      CHARACTER*16   OPTION
      CHARACTER*(*)  FAMI,MATERI
C
C --- ------------------------------------------------------------------
C           PLASTICITE VON MISES CINEMATIQUE LINEAIRE EN 1D
C              FORTEMENT INSPIRE DE NM1DCI
C  IN
C        FAMI   : FAMILLE DU POINT DE GAUSS
C        KPG    : NUMERO DU POINT DE GAUSS
C        KSP    : NUMERO DU SOUS-POINT DE GAUSS / FIBRE
C        IMATE  : POINTEUR MATERIAU CODE
C        EM     : MODULE D YOUNG MOINS
C        EP     : MODULE D YOUNG PLUS
C        SIGM   : CONTRAINTE AU TEMPS MOINS
C        DEPS   : DEFORMATION TOTALE PLUS - DEFORMATION MOINS
C                       - INCREMENT DEFORMATION THERMIQUE
C        VIM    : VARIABLE INTERNES MOINS
C        OPTION : OPTION DE CALCUL
C  OUT
C        SIGP   : CONTRAINTES PLUS
C        VIP    : VARIABLE INTERNES PLUS
C        DSDE   : DSIG/DEPS
C --- ------------------------------------------------------------------
C     VARIABLES INTERNES
C        1  -> ICELS : CRITERE ELS
C        2  -> ICELU : CRITERE ELU
C        3  -> IXM   : ECROUISSAGE CINEMATIQUE
C        4  -> IPLAS : INDICATEUR PLASTIQUE
C        5  -> IDISS : DISSIPATION PLASTIQUE
C        6  -> IWTHE : DISSIPATION THERMODYNAMIQUE
C --- ------------------------------------------------------------------
C     INDEX DES VARIABLES INTERNES
      INTEGER        ICELS,  ICELU,  IXM,  IPLAS,  IDISS,  IWTHE
      PARAMETER     (ICELS=1,ICELU=2,IXM=3,IPLAS=4,IDISS=5,IWTHE=6)
C --- ------------------------------------------------------------------
      INTEGER     ICODRE(4)
      REAL*8      SIGY,SIELEQ,SIGE,DP,ETM,ETP,XP,XM,HM,HP,SGELS,EPELU
      REAL*8      VALRES(4)
      CHARACTER*8 NOMECL(4)

      DATA NOMECL/'D_SIGM_E','SY','SIGM_ELS','EPSI_ELU'/
C --- ------------------------------------------------------------------
C     INSTANT -
      CALL RCVALB(FAMI,KPG,KSP,'-',IMATE,MATERI,'ECRO_LINE',
     &            0,' ',0.D0,1,NOMECL,VALRES,ICODRE,1)
      ETM = VALRES(1)
      HM  = EM*ETM/(EM-ETM)
C     INSTANT +
      CALL RCVALB(FAMI,KPG,KSP,'+',IMATE,MATERI,'ECRO_LINE',
     &            0,' ',0.D0,4,NOMECL,VALRES,ICODRE,1)
      ETP   = VALRES(1)
      SIGY  = VALRES(2)
      SGELS = VALRES(3)
      EPELU = VALRES(4)
C --- ------------------------------------------------------------------
      HP    = EP*ETP/(EP-ETP)
      XM    = VIM(IXM)
C --- ------------------------------------------------------------------
      SIGE   = EP*(SIGM/EM+DEPS) - HP*XM/HM
      SIELEQ = ABS(SIGE)
C --- ------------------------------------------------------------------
C     CALCUL EPSP, P , SIG
C --- ------------------------------------------------------------------
      IF (  (OPTION(1:9).EQ.'FULL_MECA') .OR.
     &      (OPTION.EQ.'RAPH_MECA') ) THEN
         IF (SIELEQ.LE.SIGY) THEN
            VIP(IPLAS) = 0.D0
            DSDE       = EP
            DP         = 0.D0
            XP         = HP*XM/HM
            SIGP       = EP*(SIGM/EM+DEPS)
            VIP(IXM)   = XP
            VIP(ICELU) = ABS( (SIGM/EM+DEPS)/EPELU )
         ELSE
            VIP(IPLAS) = 1.D0
            DP         = (SIELEQ-SIGY)/(EP+HP)
            IF (OPTION.EQ.'FULL_MECA_ELAS') THEN
               DSDE    = EP
            ELSE
               DSDE    = ETP
            ENDIF
            XP         = HP*XM/HM + HP*DP*SIGE/SIELEQ
            SIGP       = XP + SIGY*SIGE/SIELEQ
            VIP(IXM)   = XP
            VIP(ICELU) = ABS( ((SIGP-SIGY)/ETP + SIGY/EP)/EPELU )
         END IF
         VIP(ICELS) = ABS( SIGP/SGELS )
C        DISSIPATION THERMODYNAMIQUE
         VIP(IWTHE) = VIM(IWTHE) + SIGY*DP
C        DISSIPATION IRREVERSIBLE
         VIP(IDISS) = VIM(IDISS) + (DSDE*DEPS-(SIGP-SIGM))*DEPS/2.0D0
      END IF
      IF (OPTION(1:10).EQ.'RIGI_MECA_') THEN
         IF (  (VIM(IPLAS).LT.0.5D0) .OR.
     &         (OPTION.EQ.'RIGI_MECA_ELAS') ) THEN
            DSDE = EP
         ELSE
            DSDE = ETP
         END IF
      END IF
      END
