      SUBROUTINE LOCGLO(XLOCAL,SINA,COSA,SINB,COSB,
     &                  SING,COSG,XGLOBA
     &                    )
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C
C***********************************************************************
C 01/01/91    G.JACQUART AMV/P61 47 65 49 41
C***********************************************************************
C     FONCTION  : PASSAGE DU REPERE LOCAL AU REPERE GLOBAL
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C        NOM        MODE                    ROLE
C  ________________ ____ ______________________________________________
C                         VARIABLES DU SYSTEME DYNAMIQUE MODAL
C  ________________ ____ ______________________________________________
C    XLOCAL         <--   COORDONNES DANS LE REPERE LOCAL
C    SINA,SINB,SING <--   SINUS DES ANGLES DE ROTATION REP. GLOBAL LOC.
C    COSA,COSB,COSG <--   COSINUS DES ANGLES DE ROTATION REP. GLOB LOC.
C    XGLOBA          -->  COORDONEES DANS LE REPERE GLOBAL
C-----------------------------------------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 XGLOBA(3),XLOCAL(3)
C
      CALL ROT3DI(XLOCAL,-SINA,COSA,-SINB,COSB,-SING,COSG,XGLOBA)
      END
