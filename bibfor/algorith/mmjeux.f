        SUBROUTINE  MMJEUX(NNO   ,NDIM  ,COORMA,COORPT,FF    ,
     &                     NORM  ,JEU   )
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
      INTEGER      NNO
      INTEGER      NDIM
      REAL*8       COORMA(27)
      REAL*8       COORPT(3)
      REAL*8       JEU,NORM(3)
      REAL*8       FF(9)
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODE CONTINUE - APPARIEMENT)
C
C CALCUL DU JEU ET DE LA NORMALE POINT DE CONTACT/MAILLE MAITRE APPARIEE
C      
C ----------------------------------------------------------------------
C
C
C IN  NNO    : NOMBRE DE NOEUD SUR LA MAILLE
C IN  NDIM   : DIMENSION DE LA MAILLE (2 OU 3)
C IN  COORMA : COORDONNEES DES NOEUDS DE LA MAILLE
C IN  COORPT : COORDONNEES DU NOEUD A PROJETER SUR LA MAILLE
C IN  FF     : FONCTIONS DE FORME  
C OUT NORM   : NORMALE ENTRE POINT ET SA PROJECTION SUR LA MAILLE
C OUT JEU    : DISTANCE ENTRE POINT ET SA PROJECTION SUR LA MAILLE
C              (NORME EUCLIDIENNE DE LA NORMALE)
C
C ----------------------------------------------------------------------
C
      REAL*8       VEC(3)
      INTEGER      IDIM,INO
      REAL*8       ZERO
      PARAMETER    (ZERO=0.D0)      
C
C ----------------------------------------------------------------------
C
      DO 10 IDIM  = 1,3
        VEC(IDIM)  = ZERO
        NORM(IDIM) = ZERO  
   10 CONTINUE      
C
C --- DISTANCE POINT DE CONTACT/PROJECTION
C
      DO 90 IDIM = 1,3
        DO 80 INO = 1,NNO
          VEC(IDIM) = COORMA(3*(INO-1)+IDIM)*FF(INO) + VEC(IDIM)
   80   CONTINUE
   90 CONTINUE
C
C --- NORMALE
C
      DO 140 IDIM = 1,3
        NORM(IDIM) = VEC(IDIM) - COORPT(IDIM)
  140 CONTINUE
C
C --- JEU
C  
      JEU = SQRT(NORM(1)**2+NORM(2)**2+NORM(3)**2)
      
  999 CONTINUE    
   
      END
