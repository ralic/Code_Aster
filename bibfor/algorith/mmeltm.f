      CHARACTER*16 FUNCTION MMELTM(ITYP)
C      
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 10/11/2008   AUTEUR COURTOIS M.COURTOIS 
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
C DONNE LE TYPE_MAILLE DE L'ELEMENT DE CONTACT ITYP
C      
C ----------------------------------------------------------------------
C
C
C IN  ITYP   : NUMERO DU TYPE
C OUT MMELTM : NOM DU TYPE ELEMENT 
C
C
C ----------------------------------------------------------------------
C
      INTEGER      NBTYP
      PARAMETER   (NBTYP=30)
      INTEGER      K
      CHARACTER*16 NOMTM(NBTYP)
C
      DATA (NOMTM(K),K=1,NBTYP) /
     &      'SEG22' ,'SEG33' ,'SEG23' ,'SEG32' ,'TRIA33',
     &      'TR3TR6','TR6TR3','TRIA66','QUAD44','QU4QU8',
     &      'QU8QU4','QUAD88','QU4TR3','TR3QU4','TR6QU4',
     &      'QU4TR6','TR6QU8','QU8TR6','TR6QU9','QU9TR6',
     &      'QU8TR3','TR3QU8','QU8QU9','QU9QU8','QU9QU4',
     &      'QU4QU9','QU9TR3','TR3QU9','QU9QU9','SEG22'/
C
C ----------------------------------------------------------------------
C
      MMELTM = '                '
      IF ((ITYP.LE.0).OR.(ITYP.GT.NBTYP)) THEN
        CALL ASSERT(.FALSE.)
      ELSE
        MMELTM = NOMTM(ITYP)
      ENDIF  
C
      END
