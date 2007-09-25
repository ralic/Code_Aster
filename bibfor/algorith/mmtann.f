      SUBROUTINE MMTANN(NDIM  ,TAU1  ,TAU2  ,NIVERR)
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
      INTEGER      NDIM
      REAL*8       TAU1(3),TAU2(3)
      INTEGER      NIVERR      
C
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODE CONTINUE - APPARIEMENT)
C
C NORMALISATION DES VECTEURS TANGENTS
C      
C ----------------------------------------------------------------------
C
C
C IN  NDIM   : DIMENSION DE LA MAILLE (2 OU 3)
C OUT TAU1   : PREMIER VECTEUR TANGENT EN XI,YI
C OUT TAU2   : SECOND VECTEUR TANGENT EN XI,YI
C OUT NIVERR : RETOURNE UN CODE ERREUR
C                0  TOUT VA BIEN
C                1  ELEMENT INCONNU
C                2  MATRICE SINGULIERE (VECTEURS TANGENTS COLINEAIRES)
C                3  DEPASSEMENT NOMBRE ITERATIONS MAX
C                4  VECTEURS TANGENTS NULS
C      
C ----------------------------------------------------------------------
C
      REAL*8       NTA1,NTA2
C
C ----------------------------------------------------------------------
C
      CALL NORMEV(TAU1,NTA1)
      IF (NDIM.EQ.3) THEN
        CALL NORMEV(TAU2,NTA2)
      ELSEIF (NDIM.EQ.2) THEN
        NTA2 = 1.D0 
      ELSE
        CALL ASSERT(.FALSE.)   
      ENDIF     
      IF ((NTA1.EQ.0.D0).OR.(NTA2.EQ.0.D0)) THEN
        NIVERR = 4
      ENDIF  
C
      END
