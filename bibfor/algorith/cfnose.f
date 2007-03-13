      SUBROUTINE CFNOSE(DEFICO,IZONE ,NDIM  ,COORDA,COORDB,
     &                  MNORM )
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
      REAL*8       MNORM(3)
      INTEGER      IZONE
      CHARACTER*24 DEFICO      
      INTEGER      NDIM
      REAL*8       COORDA(3),COORDB(3)      
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODES DISCRETES - APPARIEMENT - MAIT/ESCL -
C                  NORMALES)
C
C CALCUL DES NORMALES POUR LES SEGMENTS
C
C ----------------------------------------------------------------------
C
C
C IN  DEFICO : SD DE DEFINITION DU CONTACT (ISSUE D'AFFE_CHAR_MECA)
C IN  IZONE  : ZONE DE CONTACT
C IN  NDIM   : DIMENSION DE L'ESPACE (2 OU 3)
C IN  COORDA : COORDONNEES DU SOMMET A 
C IN  COORDB : COORDONNEES DU SOMMET B
C OUT MNORM  : NORMALE A LA MAILLE
C
C ----------------------------------------------------------------------
C
      INTEGER TANGDF
      REAL*8  AB(3),ABSAB,VECSEG(3)
      REAL*8  DTANG(3)
C
C ----------------------------------------------------------------------
C
      AB(1) = COORDB(1)-COORDA(1)
      AB(2) = COORDB(2)-COORDA(2)
      IF (NDIM.EQ.3) THEN
        AB(3) = COORDB(3)-COORDA(3)        
      ELSE
        AB(3) = 0.D0      
      ENDIF        
      ABSAB = (AB(1)*AB(1)+AB(2)*AB(2)+AB(3)*AB(3))
      IF (ABSAB.EQ.0) THEN
        CALL U2MESS('F','CONTACT_10')
      END IF
      IF (NDIM.EQ.2) THEN
        MNORM(1)  = - AB(2)/SQRT(ABSAB)
        MNORM(2)  = AB(1)/SQRT(ABSAB)
        MNORM(3)  = 0.0D0
      ELSEIF (NDIM.EQ.3) THEN
        CALL CFTAND(DEFICO,IZONE,TANGDF,DTANG)
        IF (TANGDF.EQ.0) THEN
          CALL U2MESS('F','CONTACT_60')
        ENDIF
        VECSEG(1) = AB(1)/SQRT(ABSAB)
        VECSEG(2) = AB(2)/SQRT(ABSAB)
        VECSEG(3) = AB(3)/SQRT(ABSAB)
        MNORM(1)  = VECSEG(2)*DTANG(3) - VECSEG(3)*DTANG(2)
        MNORM(2)  = VECSEG(3)*DTANG(1) - VECSEG(1)*DTANG(3)
        MNORM(3)  = VECSEG(1)*DTANG(2) - VECSEG(2)*DTANG(1)
      ELSE 
        CALL CFIMPA('CFNOSE',1)       
      END IF        
C
      END
