      SUBROUTINE TBIM52 ( TYPPAR, IADR, I, II, IR, IC, IK, VTI, VTR, 
     +                    VTC, VTK )
      IMPLICIT   NONE
      INTEGER             IADR, I, II, IR, IC, IK, VTI(*)
      REAL*8              VTR(*)
      COMPLEX*16          VTC(*)
      CHARACTER*(*)       TYPPAR, VTK(*)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 27/09/2004   AUTEUR CIBHHLV L.VIVAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ----------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
      IF (     TYPPAR(1:1) .EQ. 'I'   ) THEN
         II = II + 1
         VTI(II) = ZI(IADR+I-1)
      ELSEIF ( TYPPAR(1:1) .EQ. 'R'   ) THEN
         IR = IR + 1
         VTR(IR) = ZR(IADR+I-1)
      ELSEIF ( TYPPAR(1:1) .EQ. 'C'   ) THEN
         IC = IC + 1
         VTC(IC) = ZC(IADR+I-1)
      ELSEIF ( TYPPAR(1:3) .EQ. 'K80' ) THEN
         IK = IK + 1
         VTK(IK) = ZK80(IADR+I-1)
      ELSEIF ( TYPPAR(1:3) .EQ. 'K32' ) THEN
         IK = IK + 1
         VTK(IK) = ZK32(IADR+I-1)
      ELSEIF ( TYPPAR(1:3) .EQ. 'K24' ) THEN
         IK = IK + 1
         VTK(IK) = ZK24(IADR+I-1)
      ELSEIF ( TYPPAR(1:3) .EQ. 'K16' ) THEN
         IK = IK + 1
         VTK(IK) = ZK16(IADR+I-1)
      ELSEIF ( TYPPAR(1:2) .EQ. 'K8'  ) THEN
         IK = IK + 1
         VTK(IK) = ZK8(IADR+I-1)
      ENDIF
C
      CALL JEDEMA()
C
      END
