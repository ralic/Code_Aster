      SUBROUTINE MDGEPH(NEQ,NBMODE,BMODAL,XGENE,U)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 02/10/95   AUTEUR GIBHHAY A.Y.PORTABILITE 
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
C-----------------------------------------------------------------------
C     CONVERTIT EN BASE PHYSIQUE MODE VECTEUR
C     REM U EST INITIALISE A 0.
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C        NOM        MODE                    ROLE
C  ________________ ____ ______________________________________________
C    NEQ            <--   NB D'EQUATIONS DU SYSTEME ASSEMBLE
C    NBMODE         <--   NB DE MODES NORMAUX CONSIDERES
C    BMODAL         <--   BASE MODALE CONSIDEREE
C    XGENE          <--   VECTEUR DES COORDONNEES GENERALISEES
C    U              <--   VECTEUR DES COORDONNEES PHYSIQUES
C .________________.____.______________________________________________.
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 BMODAL(NEQ,NBMODE),XGENE(NBMODE),U(NEQ)
      DO 10 I=1,NEQ
        U(I)=0.0D0
        DO 10 J=1,NBMODE
          U(I) = U(I) + BMODAL(I,J)*XGENE(J)
 10   CONTINUE
 9999 CONTINUE
      END
