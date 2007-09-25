      SUBROUTINE MMTANG(NDIM  ,NNO   ,COORMA,DFF   ,
     &                  TAU1  ,TAU2)
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
      INTEGER     NDIM,NNO
      REAL*8      COORMA(27)
      REAL*8      DFF(2,9)
      REAL*8      TAU1(3),TAU2(3)
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODE CONTINUE - APPARIEMENT)
C
C CALCULE LES VECTEURS TANGENTS LOCAUX SUR UNE MAILLE
C      
C ----------------------------------------------------------------------
C
C
C IN  NNO    : NOMBRE DE NOEUD SUR LA MAILLE
C IN  NDIM   : DIMENSION DE LA MAILLE (2 OU 3)
C IN  COORMA : COORDONNEES DES NOEUDS DE LA MAILLE
C IN  DFF    : DERIVEES PREMIERES DES FONCTIONS DE FORME  
C I/O TAU1   : PREMIER VECTEUR TANGENT EN (KSI1,KSI2)
C I/O TAU2   : SECOND VECTEUR TANGENT EN (KSI1,KSI2)
C      
C ----------------------------------------------------------------------
C
      INTEGER     IDIM,INO
C
C ----------------------------------------------------------------------
C    
C
C --- VERIF CARACTERISTIQUES DE LA MAILLE
C
      IF (NNO.GT.9)  CALL ASSERT(.FALSE.) 
      IF (NDIM.GT.3) CALL ASSERT(.FALSE.) 
      IF (NDIM.LE.1) CALL ASSERT(.FALSE.) 
C
C --- CALCUL DES TANGENTES
C
      DO 41 IDIM = 1,NDIM
        DO 31 INO = 1,NNO
          TAU1(IDIM)  = COORMA(3*(INO-1)+IDIM)*DFF(1,INO) + TAU1(IDIM)
          IF (NDIM.EQ.3) THEN
            TAU2(IDIM)  = COORMA(3*(INO-1)+IDIM)*DFF(2,INO) + TAU2(IDIM)
          ENDIF  
 31    CONTINUE
 41   CONTINUE 
C
  999 CONTINUE 
C
      END
