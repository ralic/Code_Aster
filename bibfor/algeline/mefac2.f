      FUNCTION MEFAC2(N,M)                                              
      IMPLICIT NONE
C                                                                       
      INTEGER       N,M                                                 
      REAL*8        MEFAC2                                              
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C ----------------------------------------------------------------------
C     CALCUL DE L'EXPRESSION FACTORIELLE SUIVANTE :                     
C     (N+M-1)!/(M-1)!/(N-1)! = N(N+1)...(N+M-1)/(M-1)/.../1             
C     OPERATEUR APPELANT : OP0144 , FLUST3, MEFIST, MEFMAT              
C ----------------------------------------------------------------------
C     OPTION DE CALCUL   : CALC_FLUI_STRU , CALCUL DES PARAMETRES DE    
C     COUPLAGE FLUIDE-STRUCTURE POUR UNE CONFIGURATION DE TYPE "FAISCEAU
C     DE TUBES SOUS ECOULEMENT AXIAL"                                   
C ----------------------------------------------------------------------
C                                                                       
C                                                                       
C-----------------------------------------------------------------------
      INTEGER I 
C-----------------------------------------------------------------------
      MEFAC2 = N                                                        
      DO 1 I = 1,M-1                                                    
         MEFAC2 = MEFAC2*(N+I)/I                                        
  1   CONTINUE                                                          
C                                                                       
      END 
