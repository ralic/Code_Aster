      SUBROUTINE MCOPCE(NDIM  ,ALIAS ,NNO   ,KSI1  ,KSI2  ,
     &                  COOR  ,GEOM  )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 22/06/2009   AUTEUR DESOZA T.DESOZA 
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
      IMPLICIT NONE
      INTEGER      NDIM,NNO
      CHARACTER*8  ALIAS
      REAL*8       KSI1
      REAL*8       KSI2
      REAL*8       COOR(27),GEOM(3)
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODE CONTINUE - APPARIEMENT - UTILITAIRE)
C
C CALCUL DES COORDONNEES D'UN NOEUD SUR UNE MAILLE A PARTIR
C DE SES COORDONNEES PARAMETRIQUES 
C
C ----------------------------------------------------------------------
C
C
C IN  NDIM   : DIMENSION DU MAILLAGE
C IN  ALIAS  : TYPE DE LA MAILLE
C IN  NNO    : NOMBRE DE NOEUDS DE LA MAILLE
C IN  KSI1   : COORDONNEE PARAMETRIQUE KSI DU PROJETE
C IN  KSI2   : COORDONNEE PARAMETRIQUE ETA DU PROJETE
C OUT GEOM   : COORDONNEES DU NOEUD
C
C ----------------------------------------------------------------------
C
      INTEGER      I,J
      REAL*8       FF(9)
C
C-----------------------------------------------------------------------
C
C --- INITIALISATIONS
C
      DO 10 I = 1,3
        GEOM(I) = 0.D0
   10 CONTINUE
C
C --- COORDONNEES DU PROJETE
C
      CALL MMNONF(NDIM,NNO,ALIAS,KSI1,KSI2,FF)
      DO 40 I = 1,3
        DO 30 J = 1,NNO
          GEOM(I) = FF(J)*COOR((J-1)*3+I) + GEOM(I)
   30   CONTINUE
   40 CONTINUE
C
      END
