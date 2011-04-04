      FUNCTION DPLITG (MATER, PPLUS, PLAS)
C
      IMPLICIT      NONE
      REAL*8        MATER(5,2), PPLUS, PLAS, DPLITG
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 04/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
C --- BUT : INITIALISATION POUR L OPERATEUR TANGENT POUR LA LOI --------
C --- DRUCKER-PRAGER LINEAIRE SOUS RIGI_MECA_TANG ----------------------
C ======================================================================
      REAL*8   UN, DEUX, TROIS, YOUNG, NU, TROISK, DEUXMU, H, PULT
      REAL*8   ALPHA, R8NNEM
C ======================================================================
      PARAMETER  ( UN    = 1.0D0 )
      PARAMETER  ( DEUX  = 2.0D0 )
      PARAMETER  ( TROIS = 3.0D0 )
C ======================================================================
      YOUNG  = MATER(1,1)
      NU     = MATER(2,1)
      TROISK = YOUNG / (UN-DEUX*NU)
      DEUXMU = YOUNG / (UN+NU)
      H      = MATER(2,2)
      ALPHA  = MATER(3,2)
      PULT   = MATER(4,2)
      DPLITG = R8NNEM()
      IF (PLAS.EQ.1.0D0) THEN
         IF (PPLUS.LT.PULT) THEN
            DPLITG = TROIS*DEUXMU/DEUX + TROIS*TROISK*ALPHA*ALPHA + H
         ELSE
            DPLITG = TROIS*DEUXMU/DEUX + TROIS*TROISK*ALPHA*ALPHA
         ENDIF
      ELSE IF (PLAS.EQ.2.0D0) THEN
         IF (PPLUS.LT.PULT) THEN
            DPLITG = TROIS*TROISK*ALPHA*ALPHA + H
         ELSE
            DPLITG = TROIS*TROISK*ALPHA*ALPHA
         ENDIF
      ENDIF
C ======================================================================
      END
