      SUBROUTINE JJMMAA(CT,AUT)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C ======================================================================
      IMPLICIT NONE
C
      CHARACTER*12 AUT
      CHARACTER*4 CT(3)
      INTEGER T(9)
C
C  ----------- FIN DECLARATIONS _____________
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      AUT='INTERF_ST/TF'
C
C
      CALL KLOKLO(T)
      T(1)=T(2)
      T(2)=T(3)
      T(3)=T(4)
      IF (T(1).LE.9) THEN
        CT(1)='0   '
        CALL CODENT(T(1),'G',CT(1)(2:2))
      ELSE
        CT(1)='    '
        CALL CODENT(T(1),'G',CT(1)(1:2))
      ENDIF
      IF (T(2).LE.9) THEN
        CT(2)='0   '
        CALL CODENT(T(2),'G',CT(2)(2:2))
      ELSE
        CT(2)='    '
        CALL CODENT(T(2),'G',CT(2)(1:2))
      ENDIF
      CT(3)='    '
      CALL CODENT(T(3),'G',CT(3))
C
      END
