      SUBROUTINE CFMOYN(APPAZ,MOYEN,MNORM,ENORM,NORM)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/03/2007   AUTEUR ABBAS M.ABBAS 
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
      IMPLICIT     NONE
      INTEGER       MOYEN
      CHARACTER*(*) APPAZ
      REAL*8        MNORM(3),ENORM(3),NORM(3)
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODES DISCRETES - APPARIEMENT)
C
C CALCUL DE LA NORMALE MOYENNE
C
C ----------------------------------------------------------------------
C
C
C IN  MOYEN  : NORMALES D'APPARIEMENT
C               0 MAIT
C               1 MAIT_ESCL
C IN  MNORM  : NORMALE A LA MAILLE MAITRE
C IN  ENORM  : NORMALE AU NOEUD ESCLAVE COURANT
C IN  APPAR  : TYPE D'APPARIEMENT
C               'MAIT_ESCL'
C               'NODAL'
C OUT NORM   : NORMALE MOYENNEE 
C
C ----------------------------------------------------------------------
C
      REAL*8       NORME
      CHARACTER*16 APPAR
C
C ----------------------------------------------------------------------
C  
      APPAR = APPAZ
C   
      IF (APPAR(1:9).EQ.'MAIT_ESCL') THEN
        IF (MOYEN.EQ.0) THEN
          NORM(1) = MNORM(1)
          NORM(2) = MNORM(2) 
          NORM(3) = MNORM(3)
        ELSE IF (MOYEN.EQ.1) THEN
          NORM(1) = ENORM(1) + MNORM(1)
          NORM(2) = ENORM(2) + MNORM(2) 
          NORM(3) = ENORM(3) + MNORM(3)        
        ELSE
          CALL CFIMPA('CFMOYN',1)
        END IF
        NORME = SQRT(NORM(1)*NORM(1)+NORM(2)*NORM(2)+NORM(3)*NORM(3))
        IF (NORME.NE.0.D0) THEN
          CALL NORMEV(NORM,NORME) 
        ENDIF 
      ELSEIF (APPAR(1:5).EQ.'NODAL') THEN  
        IF (MOYEN.EQ.0) THEN
          NORM(1) = ENORM(1)
          NORM(2) = ENORM(2)
          NORM(3) = ENORM(3)    
        ELSEIF (MOYEN.EQ.1) THEN
          NORM(1) = (ENORM(1) - MNORM(1)) /2.D0
          NORM(2) = (ENORM(2) - MNORM(2)) /2.D0
          NORM(3) = (ENORM(3) - MNORM(3)) /2.D0
        ELSE 
          CALL CFIMPA('CFMOYN',2)  
        ENDIF      
      ELSE
        CALL CFIMPA('CFMOYN',3)
      ENDIF 
C     
      END
