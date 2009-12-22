      INTEGER FUNCTION MMELTN(ITYP)
C      
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 22/12/2009   AUTEUR ABBAS M.ABBAS 
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
C DONNE LE NOMBRE DE NOEUDS DE L'ELEMENT DE CONTACT ITYP
C      
C ----------------------------------------------------------------------
C
C
C IN  ITYP   : NUMERO DU TYPE
C OUT MMELTN : NOMBRE DE NOEUDS DE L'ELEMENT DE CONTACT
C
C
C ----------------------------------------------------------------------
C
      INTEGER      NBTYP
      PARAMETER   (NBTYP=30)
      INTEGER      K
      INTEGER      NPL(NBTYP)
C
      DATA (NPL(K),K=1,NBTYP) /
     &      4 ,6 ,5 ,5 ,6 ,
     &      9 ,9 ,12,8 ,12,
     &      12,16,7 ,7 ,10,
     &      10,14,14,15,15,
     &      11,11,17,17,13,
     &      13,12,12,18,4/
C
C ----------------------------------------------------------------------
C
      MMELTN = 0
      IF ((ITYP.LE.0).OR.(ITYP.GT.NBTYP)) THEN
        CALL ASSERT(.FALSE.)
      ELSE
        MMELTN = NPL(ITYP)
      ENDIF  
C
      END
