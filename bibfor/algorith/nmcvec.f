      SUBROUTINE NMCVEC(OPER  ,TYPVEZ,OPTIOZ,LCALC ,LASSE ,
     &                  NBVECT,LTYPVE,LOPTVE,LCALVE,LASSVE)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 22/12/2009   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
      CHARACTER*4   OPER      
      CHARACTER*(*) TYPVEZ,OPTIOZ
      LOGICAL       LASSE,LCALC
      INTEGER       NBVECT   
      CHARACTER*6   LTYPVE(20)
      CHARACTER*16  LOPTVE(20)
      LOGICAL       LASSVE(20),LCALVE(20)
C 
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (CALCUL - UTILITAIRE)
C
C GESTION DE LA LISTE DES VECT_ELEM A CALCULER ET ASSEMBLER
C      
C ----------------------------------------------------------------------
C
C
C IN  OPER   : OPERATION SUR LA LISTE
C                'INIT'
C                'AJOU' 
C IN  TYPVEC : TYPE DU VECT_ELEM 
C IN  OPTION : OPTION DE CALCUL DU VECT_ELEM 
C IN  LCALC  : LE VECT_ELEM SERA A CALCULER
C IN  LASSE  : LE VECT_ELEM SERA A ASSEMBLER
C I/O NBVECT : NOMBRE DE VECT_ELEM DANS LA LISTE
C I/O LTYPVE : LISTE DES TYPES DES VECT_ELEM 
C I/O LOPTVE : LISTE DES OPTIONS DES VECT_ELEM 
C I/O LCALVE : SI VECT_ELEM A CALCULER
C I/O LASSVE : SI VECT_ELEM A ASSEMBLER
C      
C ----------------------------------------------------------------------
C
      CHARACTER*16 OPTION 
      CHARACTER*6  K6BLA,TYPVEC
      INTEGER      I
C      
C ----------------------------------------------------------------------
C
         
C
C --- INITIALISATIONS
C
      TYPVEC = TYPVEZ
      OPTION = OPTIOZ
      K6BLA  = ' '
C
C --- OPERATIONS
C      
      IF (OPER.EQ.'INIT')   THEN
        DO 10 I=1,20
          LTYPVE(I) = K6BLA
  10    CONTINUE      
        NBVECT = 0
      ELSEIF (OPER.EQ.'AJOU')   THEN  
        NBVECT = NBVECT + 1
        IF (NBVECT.EQ.21) THEN
          CALL ASSERT(.FALSE.)
        ENDIF  
C
C --- UTILISER NMFINT !
C        
        IF (TYPVEC.EQ.'CNFINT') THEN
          CALL ASSERT(.FALSE.)
        ENDIF
        LTYPVE(NBVECT) = TYPVEC       
        LOPTVE(NBVECT) = OPTION
        LASSVE(NBVECT) = LASSE   
        LCALVE(NBVECT) = LCALC                
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF   
C
      END
