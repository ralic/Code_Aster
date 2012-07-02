      SUBROUTINE FOPRO1(VEC,I,PROLGD,INTERP)
      IMPLICIT NONE
      INTEGER               I
      CHARACTER*(*)     VEC(*),PROLGD,INTERP
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C     RECUPERE LES PROLONGEMENTS ET TYPE D'INTERPOLATION DANS
C     LE VECTEUR DESCRIPTEUR D'UN OBJET DE TYPE FONCTION
C     ------------------------------------------------------------------
C IN  VEC   : VECTEUR DESCRIPTEUR
C IN  I     : NUMERO DE LA FONCTION DANS LE CAS D'UNE NAPPE (0 SINON)
C OUT PROLGD: PROLONGEMENTS A GAUCHE ET A DROITE DE LA FONCTION I
C OUT INTERP: TYPE D'INTERPOLATION DE LA FONCTION I
C     ------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      IF (I.EQ.0) THEN
         INTERP = VEC(2)
         PROLGD = VEC(5)
      ELSE
         INTERP = VEC(7+(2*I-1))
         PROLGD = VEC(7+(2*I  ))
      END IF
      END
