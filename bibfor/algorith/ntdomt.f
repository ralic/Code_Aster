      SUBROUTINE NTDOMT (PARMER)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
      IMPLICIT NONE
      REAL*8                    PARMER(1)
C ----------------------------------------------------------------------
C SAISIE DES PARAMETRES DE LA METHODE DE RESOLUTION
C
C OUT PARMER  : PARAMETRES REELS DE LA METHODE
C               PARMER(1) : THETA
C
C ----------------------------------------------------------------------
      INTEGER N1
      REAL*8  THETA
      INTEGER      IARG
C DEB ------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      CALL GETVR8(' ','PARM_THETA',0,IARG,1,THETA,N1)
      IF ( (THETA.LT.0.0D0).OR.(THETA.GT.1.0D0) ) THEN
        CALL U2MESS('F','ALGORITH9_4')
      ENDIF
      PARMER(1) = THETA
C
C FIN ------------------------------------------------------------------
      END
