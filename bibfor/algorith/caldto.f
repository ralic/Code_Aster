      SUBROUTINE CALDTO(S6,FKOOH,MSNS,DTODS)
      IMPLICIT NONE
      
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 11/07/2011   AUTEUR PROIX J-M.PROIX 
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
C RESPONSABLE PROIX J-M.PROIX
C     ----------------------------------------------------------------
C     
C     MONOCRISTAL : calcul de la derivee de Tau en GDEF    
C     IN  S6    :  CONTRAINTES NOTATION VOIGT
C     IN  FKOOH :  INVERSE TENSEUR HOOKE
C     IN  MSNS  :  MS * NS
C     OUT DTODS :  dTau/dS

      INTEGER I
      REAL*8 S6(6),FKOOH(6,6),MSNS(3,3),DTODS(3,3),HMS6(6),HMS(3,3)
      REAL*8 S(3,3),T1(3,3),T16(6),T1B(3,3)
C     ----------------------------------------------------------------

      CALL LCPRMV(FKOOH,S6,HMS6)
      CALL TNSVEC(6,3,HMS, HMS6, 1.D0)
      CALL PMAT(3,HMS,MSNS,DTODS)
      
      CALL TNSVEC(6,3,S, S6, 1.D0)
      CALL PMAT(3,MSNS,S,T1)
      CALL TNSVEC(3,3,T1, T16, 1.D0)
      CALL LCPRMV(FKOOH,T16,T1B)

      CALL DAXPY(9,1.D0,T1B,1,DTODS,1)
      
      CALL DSCAL(9,2.D0,DTODS,1)
      
      CALL DAXPY(9,1.D0,MSNS,1,DTODS,1)

      END
