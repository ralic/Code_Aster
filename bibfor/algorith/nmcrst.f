      SUBROUTINE NMCRST(SDSTAT)
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
      CHARACTER*24  SDSTAT      
C
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (ALGORITHME - INITIALISATIONS)
C
C CREATION DE LA SD STATISTIQUES
C      
C ----------------------------------------------------------------------
C
C
C IN  SDSTAT : SD STATISTIQUES
C
C
C
C
      INTEGER      NBSTAT
      PARAMETER    (NBSTAT=15)
C
      CHARACTER*24 STVIP,STVIT,STVIN
      INTEGER      JSTVIP,JSTVIT,JSTVIN
      INTEGER      IFM,NIV          
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFDBG('MECA_NON_LINE',IFM,NIV)
C
C --- AFFICHAGE
C
      IF (NIV.GE.2) THEN
        WRITE (IFM,*) '<MECANONLINE> ... CREATION SD STATISTIQUES' 
      ENDIF   
C
C --- NOM DES SDS
C
      STVIP  = SDSTAT(1:19)//'.VLIP'
      STVIT  = SDSTAT(1:19)//'.VLIT'
      STVIN  = SDSTAT(1:19)//'.VLIN'
      CALL WKVECT(STVIP ,'V V I',NBSTAT,JSTVIP)
      CALL WKVECT(STVIT ,'V V I',NBSTAT,JSTVIT)
      CALL WKVECT(STVIN ,'V V I',NBSTAT,JSTVIN)    
C
      CALL JEDEMA()
      END
