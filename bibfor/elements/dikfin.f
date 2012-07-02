      SUBROUTINE DIKFIN (NBT,DNSDU,DNSDT,DMSDT,DNSDU2,DNSDT2,DMSDT2,
     &                   KY,KZ,KRX,KRZ,KLV,KLV2)
C ----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NBT
      REAL*8  DNSDU,DNSDT,DMSDT,DNSDU2,DNSDT2,DMSDT2,KY,KZ,KRX,KRZ
      REAL*8  KLV(NBT),KLV2(NBT)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C
C     CALCUL DES MATRICES KLV ET KLV2 (MATRICES TANGENTE ET SECANTE)
C     POUR LES ELEMENTS DE TYPE CORNIERE.
C
C ----------------------------------------------------------------------
C
C IN  : NBT    : NOMBRE DE VALEURS POUR LES DEMI-MATRICES
C       DNSDU  :
C       DMSDT  :
C       DNSDT  :
C       DNSDU2 :
C       DMSDT2 :
C       DNSDT2 :
C       KY,KZ,KRX,KRZ : RAIDEURS POUR LES DIRECTIONS DE
C                       COMPORTEMENT LINEAIRE
C
C OUT : KLV    :
C       KLV2   :
C
C ----------------------------------------------------------------------
C --- MISE A ZERO DE KLV ET KLV2
C
C-----------------------------------------------------------------------
      REAL*8 ZERO 
C-----------------------------------------------------------------------
      ZERO = 0.D0
      CALL R8INIR (NBT,ZERO,KLV,1)
      CALL R8INIR (NBT,ZERO,KLV2,1)
C
C --- AFFECTATION DES TERMES NON NULS DE KLV
C
      KLV(1) = DNSDU
      KLV(11) = DNSDT
      KLV(3) = KY
      KLV(6) = KZ
      KLV(10) = KRX
      KLV(15) = DMSDT
      KLV(21) = KRZ
      KLV(22) = -DNSDU
      KLV(56) = -DNSDT
      KLV(26) = -DNSDT
      KLV(30) = -KY
      KLV(39) = -KZ
      KLV(49) = -KRX
      KLV(60) = -DMSDT
      KLV(72) = -KRZ
      KLV(28) = DNSDU
      KLV(62) = DNSDT
      KLV(36) = KY
      KLV(45) = KZ
      KLV(55) = KRX
      KLV(66) = DMSDT
      KLV(78) = KRZ
C
C --- AFFECTATION DES TERMES NON NULS DE KLV2
C
      KLV2(1) = DNSDU2
      KLV2(11) = DNSDT2
      KLV2(3) = KY
      KLV2(6) = KZ
      KLV2(10) = KRX
      KLV2(15) = DMSDT2
      KLV2(21) = KRZ
      KLV2(22) = -DNSDU2
      KLV2(56) = -DNSDT2
      KLV2(26) = -DNSDT2
      KLV2(30) = -KY
      KLV2(39) = -KZ
      KLV2(49) = -KRX
      KLV2(60) = -DMSDT2
      KLV2(72) = -KRZ
      KLV2(28) = DNSDU2
      KLV2(62) = DNSDT2
      KLV2(36) = KY
      KLV2(45) = KZ
      KLV2(55) = KRX
      KLV2(66) = DMSDT2
      KLV2(78) = KRZ
C ----------------------------------------------------------------------
C
      END
