      SUBROUTINE MMMPHA(LOPTF ,LCONT ,LASPE ,LADHE ,TYPBAR,
     &                  TYPRAC,LPENAC,LPENAF,PHASEP)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 18/04/2011   AUTEUR ABBAS M.ABBAS 
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT NONE
      LOGICAL      LPENAF,LPENAC
      LOGICAL      LOPTF,LCONT,LASPE,LADHE
      INTEGER      TYPBAR,TYPRAC
      CHARACTER*9  PHASEP
C
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODE CONTINUE - CALCUL)
C
C PREPARATION DES CALCULS - PHASE DE CALCUL
C
C ----------------------------------------------------------------------
C
C
C IN  LOPTF  : .TRUE. SI OPTION DE FROTTEMENT
C IN  LCONT  : .TRUE. SI CONTACT (SU=1)
C IN  LASPE  : .TRUE. SI DANS ASPERITE (SA=1)
C IN  LADHE  : .TRUE. SI ADHERENCE
C IN  LPENAC : .TRUE. SI CONTACT PENALISE
C IN  LPENAF : .TRUE. SI FROTTEMENT PENALISE
C IN  TYPBAR : ARETE OU POINT EN FOND DE FISSURE
C IN  TYPRAC : ARETE SUR LAQUELLE UN NOEUD MILIEU EST A LINEARISER
C OUT PHASEP : 'SANS' - PAS DE CONTACT
C              'CONT' - CONTACT
C              'ADHE' - CONTACT ADHERENT
C              'GLIS' - CONTACT GLISSANT
C              'SANS_PENA' - PENALISATION - PAS DE CONTACT
C              'CONT_PENA' - PENALISATION - CONTACT
C              'ADHE_PENA' - PENALISATION - CONTACT ADHERENT
C              'GLIS_PENA' - PENALISATION - CONTACT GLISSANT
C
C ----------------------------------------------------------------------
C
      CHARACTER*4  PHASE      
C
C ----------------------------------------------------------------------
C
      PHASE  = ' '
      PHASEP = ' '
C      
C --- PHASE PRINCIPALE
C     
      IF (LOPTF) THEN
        IF (LCONT) THEN
          IF (LADHE) THEN
            PHASE = 'ADHE'
          ELSE
            PHASE = 'GLIS'
          ENDIF
        ELSE
          PHASE = 'SANS'
        ENDIF
      ELSE
        IF ((TYPBAR.NE.0).OR.(TYPRAC.NE.0)) THEN
          PHASE = 'CONT'
        ELSE
          IF (LASPE) THEN
            IF (LCONT) THEN
              PHASE = 'CONT'
            ELSE
              PHASE = 'SANS'
            ENDIF
          ELSE
            PHASE = 'SANS'
          ENDIF
        ENDIF
      ENDIF
C
C --- PRISE EN COMPTE DE LA PENALISATION
C
      IF (PHASE.EQ.'SANS') THEN
        IF (LPENAC.OR.LPENAF) THEN
          PHASEP = PHASE(1:4)//'_PENA'
        ELSE
          PHASEP = PHASE(1:4)
        ENDIF
      ELSEIF (PHASE.EQ.'CONT') THEN 
        IF (LPENAC) THEN
          PHASEP = PHASE(1:4)//'_PENA'
        ELSE
          PHASEP = PHASE(1:4)
        ENDIF
      ELSEIF ((PHASE.EQ.'ADHE').OR.(PHASE.EQ.'GLIS')) THEN 
        IF (LPENAF) THEN
          PHASEP = PHASE(1:4)//'_PENA'
        ELSE
          PHASEP = PHASE(1:4)
        ENDIF
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF
C
      END
