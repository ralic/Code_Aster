      SUBROUTINE CHSRAZ(CHSZ)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 26/01/2000   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE VABHHTS J.PELLET
C A_UTIL
      IMPLICIT NONE
      CHARACTER*(*) CHSZ
C ---------------------------------------------------------------------
C BUT: REMETTRE A "UNDEF" UN CHAMP SIMPLE (CHAM_NO_S OU CHAM_ELEM_S)
C ---------------------------------------------------------------------
C     ARGUMENTS:
C CHSZ   IN/JXVAR K19 : SD CHAMP_S A REMETTRE A "UNDEF"
C-----------------------------------------------------------------------
      CHARACTER*19 CHS
      INTEGER I1,I2
C     ------------------------------------------------------------------

      CHS = CHSZ


C     1. QUELQUES VERIFICATIONS :
C     -----------------------------------------------------------------
      CALL EXISD('CHAM_NO_S',CHS,I1)
      CALL EXISD('CHAM_ELEM_S',CHS,I2)
      IF (I1*I2.NE.0) CALL UTMESS('A','CHSIMP','LE CHAMP_S: '//CHS//
     &                            ' EST A LA FOIS CHAM_ELEM_S '//
     &                            'ET CHAM_NO_S.')
      IF (I1+I2.EQ.0) CALL UTMESS('A','CHSIMP','LE CHAMP_S: '//CHS//
     &                            'N''EXISTE PAS. ')


C     2. ON MET LES OBJETS A "UNDEF" :
C     -----------------------------------------------------------------
      IF (I1.GT.0) THEN
C      -- CAS D'UN CHAM_NO_S :
        CALL JEUNDF(CHS//'.CNSL')
        CALL JEUNDF(CHS//'.CNSV')
      ELSE
C      -- CAS D'UN CHAM_ELEM_S :
        CALL JEUNDF(CHS//'.CESL')
        CALL JEUNDF(CHS//'.CESV')
      END IF

      END
