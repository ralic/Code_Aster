      SUBROUTINE MAJPAD(P2,PVP,R,T,KH,DP2,PVPM,DT,PADP,PADM,DPAD)
      IMPLICIT NONE
      REAL*8        P2,PVP,R,T,KH,DP2,PVPM,DT,PADP,PADM,DPAD
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
C ======================================================================
C --- MISE A JOUR DE PRESSION D AIR DISSOUS ----------------------------
C ======================================================================
      INTEGER       IADZI,IAZK24,NIV,IFM
      CHARACTER*8   NOMAIL
C ======================================================================
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
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
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C ======================================================================
C --- CALCUL DES ARGUMENTS EN EXPONENTIELS -----------------------------
C --- ET VERIFICATION DE COHERENCES ------------------------------------
C ======================================================================
      PADP   = (P2 - PVP)*R*T/KH
      PADM   = ((P2-DP2) - PVPM)*R*(T-DT)/KH
      DPAD   = PADP - PADM
      IF (PADP .LT.0.D0) THEN
         CALL INFNIV(IFM,NIV)
         IF (NIV .EQ. 2)THEN
            CALL TECAEL(IADZI,IAZK24)
            NOMAIL = ZK24(IAZK24-1+3) (1:8)
            CALL U2MESK('I','COMPOR1_65',1,NOMAIL)
         END IF
      END IF
      END
