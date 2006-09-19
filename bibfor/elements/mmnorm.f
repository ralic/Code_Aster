      SUBROUTINE MMNORM(NDIM,TAU1,TAU2,NORM)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 18/09/2006   AUTEUR MABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT NONE
      INTEGER      NDIM
      REAL*8       TAU1(3)
      REAL*8       TAU2(3)
      REAL*8       NORM(3)      
C
C ----------------------------------------------------------------------
C ROUTINE APPELLEE PAR : MMMBCA/TE0364/TE0365
C ----------------------------------------------------------------------
C
C CALCULE LA NORMALE A PARTIR DES TANGENTES
C
C IN  NDIM   : DIMENSION DE LA MAILLE DE CONTACT
C IN  TAU1   : PREMIERE TANGENTE
C IN  TAU2   : SECONDE TANGENTE
C OUT NORM   : NORMALE RESULTANTE
C
C
C ----------------------------------------------------------------------
C
      REAL*8 NOOR
C
C ----------------------------------------------------------------------
C
      IF (NDIM.EQ.2) THEN
        NORM(1) = -TAU1(2)
        NORM(2) = TAU1(1)
        NORM(3) = 0.D0
      ELSE IF (NDIM.EQ.3) THEN
        CALL PROVEC(TAU2,TAU1,NORM)
      END IF
      
      CALL NORMEV(NORM,NOOR)
      IF (NOOR.EQ.0.D0) THEN
        CALL UTMESS('F','MMNORM','NORMALE NULLE !')
      ENDIF
      END
