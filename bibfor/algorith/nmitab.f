      SUBROUTINE NMITAB(FONACT,DEFICO,CONVER,ERROR ,LTABL  )
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 24/05/2011   AUTEUR ABBAS M.ABBAS 
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
      IMPLICIT     NONE
      LOGICAL      CONVER,ERROR
      LOGICAL      LTABL
      CHARACTER*24 DEFICO
      INTEGER      FONACT(*)
C
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (ALGORITHME - CONVERGENCE)
C
C DOIT-ON AFFICHER LE TABLEAU DE CONVERGENCE ?
C
C ----------------------------------------------------------------------
C
C
C IN  FONACT : FONCTIONNALITES ACTIVEES (VOIR NMFONC)
C IN  DEFICO : SD POUR LA DEFINITION DU CONTACT
C IN  CONVER : .TRUE. SI CONVERGENCE REALISEE
C IN  ERROR  : .TRUE. SI ERREUR DETECTEE
C OUT LTABL  : .TRUE. SI AFFICHAGE TABLEAU
C              .FALSE. SINON (IL SERA AFFICHE PLUS TARD, DANS NMTBLE)
C
C ----------------------------------------------------------------------
C
      LOGICAL CFDISL,LALLV
      LOGICAL ISFONC,LCTCD,LELTC,LBOUCL
C
C ----------------------------------------------------------------------
C
C
C --- FONCTIONNALITES ACTIVEES
C
      LCTCD  = ISFONC(FONACT,'CONT_DISCRET')
      LELTC  = ISFONC(FONACT,'ELT_CONTACT')
      LBOUCL = ISFONC(FONACT,'BOUCLE_EXTERNE')
C
C --- INITIALISATIONS
C
      LTABL  = .FALSE.
C
C --- AFFICHAGE TABLEAU CONVERGENCE ?
C
      IF (LELTC) THEN
        LALLV  = CFDISL(DEFICO,'ALL_VERIF')
        IF (LALLV) THEN
          LTABL = .TRUE.
        ELSE
        IF (CONVER) THEN
          LTABL = .FALSE.
        ELSE
          LTABL = .TRUE.
          ENDIF
        ENDIF
      ELSEIF (LCTCD) THEN
        LALLV  = CFDISL(DEFICO,'ALL_VERIF')
        IF (LALLV) THEN
          LTABL = .TRUE.
        ELSE
          IF (CONVER) THEN
            IF (LBOUCL) THEN
              LTABL = .FALSE.
            ELSE
              LTABL = .TRUE.
            ENDIF
          ELSE
            LTABL = .TRUE.
          ENDIF
        ENDIF
      ELSE
        LTABL = .TRUE.
      ENDIF
C
C --- PAS D'AFFICHAGE SI ERREUR
C
      IF (ERROR) THEN
        LTABL  = .FALSE.
      ENDIF
C
      END
