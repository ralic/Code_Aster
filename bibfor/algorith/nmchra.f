      SUBROUTINE NMCHRA(SDDYNA,OPTAMO,LCAMOR)
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
      CHARACTER*19 SDDYNA
      CHARACTER*16 OPTAMO
      LOGICAL      LCAMOR    
C 
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (CALCUL)
C
C CHOIX DE REASSEMBLAGE DE LA MATRICE D'AMORTISSEMENT
C      
C ----------------------------------------------------------------------
C
C 
C IN  SDDISC : SD DISC_INST
C IN  SDDYNA : SD DYNAMIQUE
C OUT OPTAMO : OPTION POUR L'AMORTISSEMENT
C OUT LCAMOR : .TRUE. SI CALCUL MATRICE D'AMORTISSEMENT
C
C
C
C
      LOGICAL      NDYNLO
      LOGICAL      LKTAN
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
        WRITE (IFM,*) '<MECANONLINE><CALC> CHOIX D''ASSEMBLAGE DE '//
     &                'MATRICE AMORTISSEMENT' 
      ENDIF          
C
C --- INITIALISATIONS
C      
      LCAMOR = .FALSE.
      OPTAMO = 'AMOR_MECA'  
C
C --- FONCTIONNALITES ACTIVEES
C
      LKTAN  = NDYNLO(SDDYNA,'RAYLEIGH_KTAN')  
C
C --- REACTUALISATION DE LA MATRICE D AMORTISSEMENT DE RAYLEIGH
C           
      IF (LKTAN) THEN
        LCAMOR = .TRUE.
      ENDIF
C
C --- AFFICHAGE
C
      IF (NIV.GE.2) THEN
        IF (LCAMOR) THEN
          WRITE (IFM,*) '<MECANONLINE><CALC> ON ASSEMBLE LA MATRICE' //
     &                  ' D''AMORTISSEMENT'
        ELSE
          WRITE (IFM,*) '<MECANONLINE><CALC> ON N''ASSEMBLE PAS '//
     &                'LA MATRICE D''AMORTISSEMENT'        
        ENDIF      
      ENDIF             
C
      CALL JEDEMA()      
C
      END
