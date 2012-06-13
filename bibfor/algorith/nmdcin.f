      SUBROUTINE NMDCIN(SDDISC,NUMINS)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*19 SDDISC
      INTEGER      NUMINS
C
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (ALGORITHME)
C
C VERIFICATION DU PAS DE TEMPS
C
C ----------------------------------------------------------------------
C
C
C IN  SDDISC : SD DISCRETISATION
C IN  NUMINS : NUMERO D'INSTANT
C
C
C
C 
      INTEGER      DININS
      INTEGER      NIVEAP,NIVEAM
C      
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- NIVEAUX DE DECOUPE
C     
      NIVEAP = DININS(SDDISC,NUMINS)
      NIVEAM = DININS(SDDISC,NUMINS-1)
C
C --- ON DOIT ETRE NON REDECOUPE
C
      IF ((NIVEAM-NIVEAP).GT.1) THEN
        CALL ASSERT(.FALSE.)
      ENDIF             
C          
      CALL JEDEMA()
      END
