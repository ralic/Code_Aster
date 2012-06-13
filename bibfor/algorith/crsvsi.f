      SUBROUTINE CRSVSI(SOLVEU)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*19 SOLVEU
C
C ----------------------------------------------------------------------
C
C ROUTINE *_NON_LINE (STRUCTURES DE DONNES)
C
C ACTIVATION DE STOP_SINGULIER = 'NON' EN CAS DE DECOUPE DU PAS DE TEMPS
C
C ----------------------------------------------------------------------
C
C 
C IN  SOLVEU  : NOM SD SOLVEUR
C
C
C
C
      INTEGER      ISLVI,ISLVK
      INTEGER      NPREC
      CHARACTER*24 NOMSLV
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
      CALL JEVEUO(SOLVEU//'.SLVK','L',ISLVK)
      CALL JEVEUO(SOLVEU//'.SLVI','E',ISLVI)
      NOMSLV = ZK24(ISLVK-1+1)
      NPREC  = ZI(ISLVI-1+1)
C      
      IF ((NOMSLV.EQ.'MUMPS').OR.
     &    (NOMSLV.EQ.'MULT_FRONT').OR.
     &    (NOMSLV.EQ.'LDLT')) THEN   
        IF (NPREC.GT.0) THEN
          ZI(ISLVI-1+3) = 2
        ELSE
          CALL U2MESK('I','DISCRETISATION_40',1,NOMSLV)
        ENDIF  
      ELSE
        CALL U2MESK('I','DISCRETISATION_40',1,NOMSLV)
      ENDIF
C
      CALL JEDEMA()
      END
