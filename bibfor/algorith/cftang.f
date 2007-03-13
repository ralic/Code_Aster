      SUBROUTINE CFTANG(DEFICO,IZONE,NDIM,XNORM,XTANG)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/03/2007   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT   NONE
      CHARACTER*24 DEFICO
      INTEGER      IZONE
      INTEGER      NDIM
      REAL*8       XNORM(3)
      REAL*8       XTANG(6)
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODES DISCRETES - APPARIEMENT)
C
C CALCULE LE OU LES VECTEURS TANGENTS ASSOCIES A UNE NORMALE
C
C ----------------------------------------------------------------------
C
C
C IN  NDIM   : DIMENSION DE L'ESPACE (2 OU 3)
C IN  DEFICO : SD DE DEFINITION DU CONTACT (ISSUE D'AFFE_CHAR_MECA)
C IN  IZONE  : ZONE DE CONTACT
C IN  XNORM  : NORMALE
C OUT XTANG  : LES DEUX VECTEURS TANGENTS CALCULES
C
C ----------------------------------------------------------------------
C
      REAL*8       ZERO,UN
      PARAMETER  ( ZERO   =  0.0D0  )      
      PARAMETER  ( UN     =  1.0D0  )   
      REAL*8       DET,PSCAL,DTANG(3)
      REAL*8       LAMBDA,SPVECT(3),EPSI,TESTTG,NORME
      INTEGER      K,TANGDF  
C
C ----------------------------------------------------------------------
C
C
C --- INITIALISATIONS
C
      DO 10 K = 1,6
        XTANG(K) = ZERO
 10   CONTINUE 
      EPSI = 1.0D-6
C
C --- SI DEFINITION DU REPERE PAR L'UTILISATEUR (VECT_Y/VECT_ORIE_POU)
C
      CALL CFTAND(DEFICO,IZONE,TANGDF,DTANG)      
C
C --- NORMALISATION
C
      CALL NORMEV(XNORM,NORME)               
C
      IF (NDIM.EQ.2) THEN
        XTANG(1) = -XNORM(2)
        XTANG(2) = XNORM(1)
        XTANG(3) = ZERO
        XTANG(4) = ZERO
        XTANG(5) = ZERO
        XTANG(6) = ZERO    
      ELSEIF (NDIM.EQ.3) THEN
        IF (TANGDF.EQ.0) THEN
          DET = SQRT(XNORM(1)**2+XNORM(2)**2)
          IF (DET.EQ.0.0D0) THEN
C --- NORMALE A L'ORIGINE SUIVANT Z          
            XTANG(1) = ZERO
            XTANG(2) = -UN
            XTANG(3) = ZERO
            XTANG(4) = XNORM(3)
            XTANG(5) = ZERO
            XTANG(6) = ZERO
          ELSE            
            XTANG(1) = -XNORM(2)/DET
            XTANG(2) = XNORM(1)/DET
            XTANG(3) = ZERO
            XTANG(4) = XNORM(1)*XNORM(3)/DET
            XTANG(5) = XNORM(2)*XNORM(3)/DET
            XTANG(6) = -DET
          END IF            
        ELSEIF (TANGDF.EQ.1) THEN
C
C --- CALCUL DU TROISIEME VECTEUR PAR PRODUIT VECTORIEL NORM/DTANG
C        
          SPVECT(1) = XNORM(2)*DTANG(3) - XNORM(3)*DTANG(2)
          SPVECT(2) = XNORM(3)*DTANG(1) - XNORM(1)*DTANG(3)
          SPVECT(3) = XNORM(1)*DTANG(2) - XNORM(2)*DTANG(1)
          TESTTG    = SQRT(SPVECT(1)**2+SPVECT(2)**2+SPVECT(3)**2)
          IF (TESTTG.LT.EPSI) THEN
            CALL U2MESS('F','CONTACT_11')
          ELSE
            PSCAL    = DTANG(1)*XNORM(1) + DTANG(2)*XNORM(2) +
     &                 DTANG(3)*XNORM(3)
            LAMBDA   = XNORM(1)*XNORM(1) + XNORM(2)*XNORM(2) +
     &                 XNORM(3)*XNORM(3)
            LAMBDA   = -PSCAL/LAMBDA
            XTANG(1) = LAMBDA*XNORM(1) + DTANG(1)
            XTANG(2) = LAMBDA*XNORM(2) + DTANG(2)
            XTANG(3) = LAMBDA*XNORM(3) + DTANG(3)
            DET      = SQRT(XTANG(1)**2+XTANG(2)**2+XTANG(3)**2)
            XTANG(1) = XTANG(1)/DET
            XTANG(2) = XTANG(2)/DET
            XTANG(3) = XTANG(3)/DET
            XTANG(4) = XNORM(2)*XTANG(3) - XNORM(3)*XTANG(2)
            XTANG(5) = XNORM(3)*XTANG(1) - XNORM(1)*XTANG(3)
            XTANG(6) = XNORM(1)*XTANG(2) - XNORM(2)*XTANG(1)
            DET      = SQRT(XTANG(4)**2+XTANG(5)**2+XTANG(6)**2)
            XTANG(4) = -XTANG(4)/DET
            XTANG(5) = -XTANG(5)/DET
            XTANG(6) = -XTANG(6)/DET 
          ENDIF         
        ELSE
          CALL CFIMPA('CFTANG',1)
        ENDIF
      ELSE
        CALL CFIMPA('CFTANG',2)      
      ENDIF     
                       
C
      END
