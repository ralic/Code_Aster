      SUBROUTINE ZBINTE(RHO   ,RHOMIN,RHOMAX,RHOEXM,RHOEXP)
C     
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 20/10/2009   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2009  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT NONE
      REAL*8       RHO
      REAL*8       RHOMIN,RHOMAX,RHOEXM,RHOEXP         
C      
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (RECH. LINE. - UTILITAIRE)
C
C GESTION DES BORNES POUR LE RHO
C      
C ----------------------------------------------------------------------
C
C I/O RHO    : RHO AVEC RESPECT DES BORNES
C IN  RHOMIN : BORNE INFERIEURE DE RECHERCHE 
C IN  RHOMAX : BORNE SUPERIEURE DE RECHERCHE 
C IN  RHOEXM : INTERVALLE [RHOEXM,RHOEXP] POUR EXCLUSION 
C IN  RHOEXP : INTERVALLE [RHOEXM,RHOEXP] POUR EXCLUSION 
C      
C ----------------------------------------------------------------------
C
      REAL*8       RHOTMP
C      
C-----------------------------------------------------------------------
C
      RHOTMP = RHO
      IF (RHOTMP.LT.RHOMIN) RHO = RHOMIN
      IF (RHOTMP.GT.RHOMAX) RHO = RHOMAX
      IF (RHOTMP.LT.0.D0.AND.RHOTMP.GE.RHOEXM) RHO = RHOEXM
      IF (RHOTMP.GE.0.D0.AND.RHOTMP.LE.RHOEXP) RHO = RHOEXP
   
      END
