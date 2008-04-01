      SUBROUTINE CFORTH(NDIMG ,TAU1  ,TAU2  ,IRET)
C     
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 01/04/2008   AUTEUR ABBAS M.ABBAS 
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
      IMPLICIT     NONE
      INTEGER      NDIMG
      REAL*8       TAU1(3),TAU2(3)     
      INTEGER      IRET
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (TOUTES METHODES - APPARIEMENT )
C
C ORTHOGONALISATION DES VECTEURS TANGENTS
C
C ----------------------------------------------------------------------
C
C
C IN  NDIMG  : DIMENSION DU MODELE
C I/O TAU1   : PREMIERE TANGENTE SUR LA MAILLE MAITRE EN KSI1
C I/O TAU2   : SECONDE TANGENTE SUR LA MAILLE MAITRE EN KSI2 
C OUT IRET   : VAUT 1 SI TANGENTES NULLES
C                   2 SI NORMALE NULLE
C
C  NB: LE REPERE EST ORTHORNORME ET TEL QUE LA NORMALE POINTE VERS
C  L'INTERIEUR DE LA MAILLE
C
C ----------------------------------------------------------------------
C
      REAL*8       NORM(3),NOOR,R8PREM      
C
C ----------------------------------------------------------------------
C   
C
C --- NORMALISATION DES VECTEURS TANGENTS
C
      CALL MMTANN(NDIMG,TAU1 ,TAU2 ,IRET )     
C
C --- ORTHOGONALISATION VECTEURS TANGENTS
C     
      IF (IRET.EQ.0) THEN
        CALL MMNORM(NDIMG,TAU1 ,TAU2 ,NORM ,NOOR)
        IF (NOOR.LE.R8PREM()) THEN
          IRET   = 2
          GOTO 99
        ELSE        
          CALL MMMRON(NDIMG,NORM ,TAU1 ,TAU2 ) 
        ENDIF  
      ENDIF       
C
C --- NORMALISATION
C
      CALL MMTANN(NDIMG,TAU1 ,TAU2 ,IRET)
C
   99 CONTINUE      
C 
      END
