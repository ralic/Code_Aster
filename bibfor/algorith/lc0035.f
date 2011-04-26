      SUBROUTINE LC0035(FAMI,KPG,KSP,NDIM,IMATE,COMPOR,CRIT,INSTAM,
     &             INSTAP,EPSM,DEPS,SIGM,VIM,OPTION,ANGMAS,SIGP,VIP,
     &                  TAMPON,TYPMOD,ICOMP,NVI,DSIDEP,CODRET)
      IMPLICIT NONE
      INTEGER         IMATE,NDIM,KPG,KSP,CODRET,ICOMP,NVI,IRET
      REAL*8          CRIT(*),ANGMAS(*),INSTAM,INSTAP,TAMPON(*)
      REAL*8          EPSM(6),DEPS(6),SIGM(6),SIGP(6),VIM(*),VIP(*)
      REAL*8          DSIDEP(6,6),TM,TP,TREF
      CHARACTER*16    COMPOR(*),OPTION
      CHARACTER*8     TYPMOD(*)
      CHARACTER*(*)   FAMI
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/04/2011   AUTEUR DELMAS J.DELMAS 
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
C TOLE CRP_21
C APPEL DE RCVARC POUR LE CALCUL DE LA TEMPERATURE
C RAISON: CETTE ROUTINE EST APPELEE EN THM AUSSI... (CALCME)
      CALL RCVARC(' ','TEMP','-',FAMI,KPG,KSP,TM,IRET)
      CALL RCVARC(' ','TEMP','+',FAMI,KPG,KSP,TP,IRET)
      CALL RCVARC(' ','TEMP','REF',FAMI,KPG,KSP,TREF,IRET)

      CALL LKCOMP (TYPMOD,IMATE,INSTAM,INSTAP,TM,TP,TREF,DEPS,SIGM,VIM,
     &              OPTION,SIGP,VIP,DSIDEP,CODRET)

      END
