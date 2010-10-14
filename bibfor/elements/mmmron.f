      SUBROUTINE MMMRON(NDIM  ,NORM  ,TAU1  ,TAU2  )
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 11/10/2010   AUTEUR ABBAS M.ABBAS 
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
      INTEGER      NDIM
      REAL*8       TAU1(3)
      REAL*8       TAU2(3)
      REAL*8       NORM(3)
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
C
C CALCULE LES TANGENTES EXTERIEURES A PARTIR DE LA NORMALE INTERIEURE
C      
C ----------------------------------------------------------------------
C
C
C CETTE ROUTINE CALCULE LES VECTEURS TANGENTS EXTERIEURS A PARTIR
C DE LA NORMALE INTERIEURE A LA MAILLE
C
C CES VECTEURS TANGENTS DONNNENT: 
C  - LA NORMALE EXTERIEURE PAR CALL PROVEC(TAU1,TAU2,NORM)
C  - LA NORMALE INTERIEURE PAR CALL PROVEC(TAU2,TAU1,NORM) - MMNORM
C
C IN  NDIM   : DIMENSION DE LA MAILLE DE CONTACT
C IN  NORM   : NORMALE INTERIEURE
C OUT TAU1   : PREMIERE TANGENTE EXTERIEURE
C OUT TAU2   : SECONDE TANGENTE EXTERIEURE
C
C
C ----------------------------------------------------------------------
C
      REAL*8 CMPX,CMPY,CMPZ,R8PREM
C
C ----------------------------------------------------------------------
C
      CMPX   = NORM(1)
      CMPY   = NORM(2)
      CMPZ   = NORM(3)        
      IF (NDIM.EQ. 2) THEN
        TAU1(1) = CMPY
        TAU1(2) = -CMPX
        TAU1(3) = 0.D0
        TAU2(1) = 0.D0
        TAU2(2) = 0.D0
        TAU2(3) = 0.D0
      ELSEIF (NDIM.EQ. 3) THEN
        IF (ABS(CMPX) .GT. R8PREM()) THEN
          TAU1(1) = -CMPY/CMPX
          TAU1(2) = 1.D0
          TAU1(3) = 0.D0
        ELSEIF (ABS(CMPY) .GT. R8PREM()) THEN
          TAU1(1) = 1.D0
          TAU1(2) = -CMPX/CMPY
          TAU1(3) = 0.D0 
        ELSEIF (ABS(CMPZ) .GT. R8PREM()) THEN
          TAU1(1) = 0.D0
          TAU1(2) = 1.D0
          TAU1(3) = -CMPY/CMPZ            
        END IF   
        CALL PROVEC(TAU1,NORM,TAU2)  
      ELSE
        CALL ASSERT(.FALSE.)  
      END IF   
 
      END
