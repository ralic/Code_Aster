      SUBROUTINE UTEXCP( NUM, SPGLU, TEXTE )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 11/05/2005   AUTEUR MCOURTOI M.COURTOIS 
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
C     ROUTINE ANALOGUE A UTMESS
C
C     ARGUMENTS :
C        NUM    = NUMERO DE L'EXCEPTION
C        SPGLU  = NOM DU SOUS-PROGRAMME LEVANT L'EXCEPTION
C        TEXTE  = MESSAGE EXPLIQUANT POURQUOI L'EXCEPTION EST LEVEE.
C     ------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER            NUM
      CHARACTER*(*)           SPGLU, TEXTE
      INTEGER          NEXCEP
      COMMON /UTEXC /  NEXCEP
C
      NEXCEP = NUM
      CALL UTMESS('Z', SPGLU, TEXTE)
      END
