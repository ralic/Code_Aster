      SUBROUTINE UTEXCP( NUM, IDMESS )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 24/04/2007   AUTEUR COURTOIS M.COURTOIS 
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
C     ------------------------------------------------------------------
C
C     ARGUMENTS :
C        NUM    = NUMERO DE L'EXCEPTION
C        IDMESS = IDENTIFIANT DU MESSAGE
C     ------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER          NUM
      CHARACTER* (*)   IDMESS
C     ------------------------------------------------------------------
      REAL*8           VALR(1), R8NNEM
      CHARACTER*8      VALK(1)
      INTEGER          NR, NK, NI, VALI(1), ISNNEM
      INTEGER          NEXCEP
      COMMON /UTEXC /  NEXCEP
C
      NEXCEP = NUM
C     ------------------------------------------------------------------
      NI = 0
      NK = 0
      NR = 0
      VALK(1) = ' '
      VALI(1) = ISNNEM()
      VALR(1) = R8NNEM()
      CALL U2MESG ('Z', IDMESS, NK, VALK, NI, VALI, NR, VALR)
C     ------------------------------------------------------------------
      END
