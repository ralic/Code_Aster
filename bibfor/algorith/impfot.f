      SUBROUTINE IMPFOT(UNITE,TIME,CHAINE)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 08/06/2009   AUTEUR ABBAS M.ABBAS 
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
C
      IMPLICIT NONE
      INTEGER       UNITE
      REAL*8        TIME
      CHARACTER*(*) CHAINE
C 
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (AFFICHAGE - UTILITAIRE)
C
C IMPRESSION D'UN TEMPS SUR UNE UNITE LOGICIELLE
C      
C ----------------------------------------------------------------------
C      
C
C IN  UNITE  : UNITE LOGICIELLE D'IMPRESSION
C IN  TIME   : TEMPS EN SECONDES A AFFICHER
C OUT CHAINE : CHAINE DE SORTIE SI UNITE = 0
C
C ----------------------------------------------------------------------
C 
      INTEGER      MINUT,HEURE,SECOND   
C
C ----------------------------------------------------------------------
C
      IF (TIME.LT.60.0D0) THEN
        SECOND = INT(TIME)
        IF (SECOND.LT.0) THEN
          SECOND = 0
        ENDIF 
        WRITE(CHAINE,10) SECOND
      ELSE 
        IF (TIME.LE.3600.D0) THEN
          MINUT  = INT(TIME/60)
          IF (MINUT.LT.0) THEN
            MINUT = 0
          ENDIF
          SECOND = INT(TIME - (MINUT*60))
          IF (SECOND.LT.0) THEN
            SECOND = 0
          ENDIF        
          WRITE(CHAINE,20) MINUT,SECOND
        ELSE
          HEURE  = INT(TIME/3600)
          IF (HEURE.LT.0) THEN
            HEURE = 0
          ENDIF
          MINUT  = INT((TIME - (HEURE*3600))/60)
          IF (MINUT.LT.0) THEN
            MINUT = 0
          ENDIF
          SECOND = INT(TIME - (HEURE*3600) - (MINUT*60))
          IF (SECOND.LT.0) THEN
            SECOND = 0
          ENDIF
          WRITE(CHAINE,30) HEURE,MINUT,SECOND
        ENDIF  
      ENDIF

 10   FORMAT (I2,' s')
 20   FORMAT (I2,' m ',I2,' s') 
 30   FORMAT (I8,' h ',I2,' m ',I2,' s') 
 

      END
