      CHARACTER*16 FUNCTION MMELTF(ITYP)
C      
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 24/09/2007   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
      INTEGER      ITYP  
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
C
C DONNE LE TYPE_ELEM DE L'ELEMENT DE FROTTEMENT ITYP
C      
C ----------------------------------------------------------------------
C
C
C IN  ITYP   : NUMERO DU TYPE
C OUT MMELTF : NOM DU TYPE ELEMENT 
C
C
C ----------------------------------------------------------------------
C
      INTEGER      NBTYP
      PARAMETER   (NBTYP=30)
      INTEGER      K
      CHARACTER*16 NOMTF(NBTYP)
C
      DATA (NOMTF(K),K=1,NBTYP) /
     &      'CFS2S2','CFS3S3','CFS2S3','CFS3S2','CFT3T3',
     &      'CFT3T6','CFT6T3','CFT6T6','CFQ4Q4','CFQ4Q8',
     &      'CFQ8Q4','CFQ8Q8','CFQ4T3','CFT3Q4','CFT6Q4',
     &      'CFQ4T6','CFT6Q8','CFQ8T6','CFT6Q9','CFQ9T6',
     &      'CFQ8T3','CFT3Q8','CFQ8Q9','CFQ9Q8','CFQ9Q4',
     &      'CFQ4Q9','CFQ9T3','CFT3Q9','CFQ9Q9','CFP2P2'/
C
C ----------------------------------------------------------------------
C
      MMELTF = '                '
      IF ((ITYP.LE.0).OR.(ITYP.GT.NBTYP)) THEN
        CALL ASSERT(.FALSE.)
      ELSE
        MMELTF = NOMTF(ITYP)
      ENDIF  
C
      END
