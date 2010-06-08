      SUBROUTINE XTCALN(NDIM  ,TAU1  ,TAU2  ,NORM  ,MPROJT)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 22/12/2009   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2009  EDF R&D                  WWW.CODE-ASTER.ORG
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
      REAL*8       TAU1(3),TAU2(3)
      REAL*8       NORM(3)
      REAL*8       MPROJT(3,3)   
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODE XFEMGG - UTILITAIRE)
C
C CALCUL DE LA NORMALE ET DES MATRICES DE PROJECTION
C
C ----------------------------------------------------------------------
C
C
C IN  NDIM   : DIMENSION DE LA MAILLE DE CONTACT
C IN  TAU1   : PREMIERE TANGENTE EXTERIEURE
C IN  TAU2   : SECONDE TANGENTE EXTERIEURE
C OUT NORM   : NORMALE INTERIEURE
C OUT MPROJT : MATRICE DE PROJECTION TANGENTE
C
C ----------------------------------------------------------------------
C 
      INTEGER I,J
      REAL*8  R8PREM,NOOR
C
C ----------------------------------------------------------------------
C  
      CALL MATINI(3   ,3     ,0.D0  ,MPROJT  )    
C
C --- NORMALE
C
      IF (NDIM.EQ.2) THEN
        CALL MMNORM(NDIM,TAU1,TAU2,NORM,NOOR)
      ELSEIF (NDIM.EQ.3) THEN
        CALL PROVEC(TAU1,TAU2,NORM)
        CALL NORMEV(NORM,NOOR)
      ENDIF
      IF (NOOR.LT.R8PREM()) THEN
        CALL ASSERT(.FALSE.)
      ENDIF
C
C --- MATRICE DE PROJECTION TANGENTE
C
      DO 121 I = 1,NDIM
        DO 111 J = 1,NDIM
          MPROJT(I,J) = -1.D0*NORM(I)*NORM(J) 
111     CONTINUE
121   CONTINUE
      DO 330 I = 1,NDIM
        MPROJT(I,I) = 1.D0 + MPROJT(I,I)  
330   CONTINUE   
      
      END
