      SUBROUTINE GRIROT ( ALPHA , BETA , ANGLL ,PGL , ROT , C, S)
C
      IMPLICIT REAL*8 (A-H,O-Z)
C       ================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 01/10/97   AUTEUR UFBHHLL C.CHAVANT 
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
C       ----------------------------------------------------------------
C     CALCUL DE LA MATRICE DE PASSAGE DU REPERE D'ORTHOTROPIE 
C     VERS LE REPERE LOCAL DE L'ELEMENT GRILLE
C       ----------------------------------------------------------------
C       IN   ALPHA  :  ANGLE AXE DE REFERENCE DANS LE PLAN TANGENT
C            BETA   :  ANGLE AXE DE REFERENCE DANS LE PLAN TANGENT
C            ANGLL  :  ANGLE 1IERE DIRECTION D'ARMATURES
C            PGL    :  MATRICE PASSAGE REPERE GLOBAL/LOCAL ELEMENT
C       OUT  ROT    :  MATRICE DE PASSAGE REPERE ORTHOTRO/LOCAL ELEMENT
C       OUT  C , S  :  COSINUS DIRECTEURS DE LA 1IERE DIRECTION 
C                      D'ORTHOTROPIE / LOCAL ELEMENT
C     ------------------------------------------------------------------
      REAL*8        PGL(3,3) , ROT(3,3)
      REAL*8        ALPHA , BETA , ANGLL , R8PREM
      REAL*8        DX , DY , DZ , NORM , PJDX , PJDY
      REAL*8        C , S , C2 , S2 , SC ,COR , SOR
C     ------------------------------------------------------------------
      DX = COS(BETA)*COS(ALPHA)
      DY = COS(BETA)*SIN(ALPHA)
      DZ = SIN(BETA)
      NORM = SQRT (DX*DX + DY*DY + DZ*DZ)
      DX = DX/NORM
      DY = DY/NORM
      DZ = DZ/NORM
C     ------------------------------------------------
      PJDX = DX*PGL(1,1) + DY*PGL(1,2) + DZ*PGL(1,3)
      PJDY = DX*PGL(2,1) + DY*PGL(2,2) + DZ*PGL(2,3)
      NORM = SQRT (PJDX*PJDX + PJDY*PJDY )
C     ------------------------------------------------
      IF ( NORM .LE. R8PREM() ) THEN
            CALL UTMESS('A','GRIROT','L''AXE DE REFERENCE EST NORMAL A'
     &      //' UN ELEMENT DE PLAQUE. VOUS NE POURREZ CALCULER LES '
     &      //' LES CONTRAINTES.')
      ENDIF
C
      PJDX = PJDX/NORM
      PJDY = PJDY/NORM
      COR= COS(ANGLL)
      SOR= SIN(ANGLL)
      C  = COR*PJDX - SOR*PJDY
      S  = SOR*PJDX + COR*PJDY
C
      C2 = C * C
      S2 = S * S
      SC = S * C
C
      ROT(1,1) = C2 
      ROT(1,2) = S2 
      ROT(1,3) = SC
      ROT(2,1) = S2
      ROT(2,2) = C2
      ROT(2,3) = -1.D0 * SC
      ROT(3,1) = -2.D0 * SC
      ROT(3,2) =  2.D0 * SC
      ROT(3,3) = C2 - S2
C
      END
