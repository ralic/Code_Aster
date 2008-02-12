      SUBROUTINE XJACOB(NNO   ,NDIM  ,DFF   ,COOR  ,INVJAC)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 12/02/2008   AUTEUR ABBAS M.ABBAS 
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
C RESPONSABLE GENIAUT S.GENIAUT
C
      IMPLICIT   NONE
      INTEGER    NNO,NDIM
      REAL*8     COOR(NDIM*NNO)      
      REAL*8     DFF(3,NNO),INVJAC(NDIM,NDIM)
C
C ----------------------------------------------------------------------
C
C CALCUL DE L'INVERSE DE LA JACOBIENNE EN XE
C
C ----------------------------------------------------------------------
C
C
C IN  NNO    : NOMBRE DE NOEUDS DE L'ELT 
C IN  COOR   : COORDONNEES DES NOEUDS DE L'ÉLÉMENT
C IN  DFF    : DÉRIVÉES DES FONCTION DES FORMES AU POINT XE
C IN  NDIM   : DIMENSION DE L'ESPACE
C OUT INVJAC : INVERSE DE LA JACONIENNE AU POINT XE
C      
C ----------------------------------------------------------------------
C      
      INTEGER       I,J,K
      REAL*8        JACOBI(3,3),TEMP(3,3)
C      
C ----------------------------------------------------------------------
C
C
C --- JACOBIENNE EN XE
C
      CALL MATINI(3,3,0.D0,JACOBI)
      DO 100 I=1,NDIM
        DO 110 J=1,NDIM
          DO 120 K=1,NNO
            JACOBI(I,J) = JACOBI(I,J) + 
     &                    DFF(J,K) * COOR(NDIM*(K-1)+I) 
 120      CONTINUE
 110    CONTINUE
 100  CONTINUE
C   
      IF (NDIM .EQ. 2) THEN
         JACOBI(3,3) = 1.D0
      ELSEIF (NDIM .EQ. 1) THEN
         JACOBI(3,3) = 1.D0
         JACOBI(2,2) = 1.D0
      ENDIF    
C
C --- INVERSE DE LA JACOBIENNE
C      
      CALL MATINV(3,JACOBI,TEMP)      
      DO 200 I=1,NDIM
        DO 210 J=1, NDIM
           INVJAC(I,J)=TEMP(I,J)
 210    CONTINUE
 200  CONTINUE
C
      END
