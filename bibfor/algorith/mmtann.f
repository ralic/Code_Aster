      SUBROUTINE MMTANN(NDIM  ,TAU1  ,TAU2  ,IRET  )
C     
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 01/04/2008   AUTEUR ABBAS M.ABBAS 
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
      INTEGER      NDIM,IRET
      REAL*8       TAU1(3),TAU2(3)  
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
C OUT IRET   : VAUT 1 SI TANGENTES NULLES, 0 SINON
C      
C ----------------------------------------------------------------------
C
      REAL*8       NTA1,NTA2,R8PREM
C
C ----------------------------------------------------------------------
C
      IRET = 0
C
      CALL NORMEV(TAU1,NTA1)
C
      IF (NDIM.EQ.2) THEN
        CALL NORMEV(TAU1,NTA1)
        NTA2  = 1.D0
      ELSEIF (NDIM.EQ.3) THEN
        CALL NORMEV(TAU1,NTA1)
        CALL NORMEV(TAU2,NTA2)
      ELSE
        CALL ASSERT(.FALSE.)   
      ENDIF 
C
C --- VERIFICATION DES TANGENTES
C
      IF (ABS(NTA1).LE.R8PREM()) THEN
        IRET = 1
      ENDIF   
      IF (ABS(NTA2).LE.R8PREM()) THEN
        IRET = 2
      ENDIF            
C
      END
