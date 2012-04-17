      INTEGER FUNCTION APMMVD(VECT) 
C    
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 03/01/2011   AUTEUR ABBAS M.ABBAS 
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
      CHARACTER*5 VECT
C      
C ----------------------------------------------------------------------
C
C ROUTINE APPARIEMENT (UTILITAIRE)
C
C RETOURNE LA LONGUEUR FIXE DES VECTEURS DE LA SD SDAPPA
C
C ----------------------------------------------------------------------
C
C
C IN  VECT   : NOM DU VECTEUR DONT ON VEUT LA DIMENSION
C
C ----------------------------------------------------------------------
C
      INTEGER   ZINZR,ZINZI
      PARAMETER (ZINZR=23,ZINZI=15) 
      INTEGER   ZINFR,ZINFI
      PARAMETER (ZINFR=1,ZINFI=6 )       
C
C ----------------------------------------------------------------------
C

      IF (VECT.EQ.'ZINZR') THEN
        APMMVD = ZINZR
      ELSE IF (VECT.EQ.'ZINZI') THEN
        APMMVD = ZINZI  
      ELSEIF (VECT.EQ.'ZINFR') THEN
        APMMVD = ZINFR
      ELSE IF (VECT.EQ.'ZINFI') THEN
        APMMVD = ZINFI      
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF  
C
      END
