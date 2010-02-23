      SUBROUTINE MMMVCC(PHASE ,NNL   ,HPG   ,FFL   ,JACOBI,
     &                  JEU   ,TYPBAR,TYPRAC,COEFCP,COEFCS,
     &                  DLAGRC,VECTCC)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 22/02/2010   AUTEUR DESOZA T.DESOZA 
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT NONE
      CHARACTER*4  PHASE
      INTEGER      NNL,TYPBAR,TYPRAC
      REAL*8       HPG,FFL(9),JACOBI,DLAGRC
      REAL*8       COEFCP,COEFCS,JEU
      REAL*8       VECTCC(9)
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
C
C CALCUL DU VECTEUR LAGR_C
C
C ----------------------------------------------------------------------
C
C
C IN  PHASE  : PHASE DE CALCUL
C              'SANS' - PAS DE CONTACT
C              'CONT' - CONTACT
C              'EXCL' - EXCLUSION D'UN NOEUD
C              'PSAN' - PENALISATION - PAS DE CONTACT
C              'PSAN' - PENALISATION - CONTACT
C IN  NNL    : NOMBRE DE NOEUDS LAGRANGE 
C IN  HPG    : POIDS DU POINT INTEGRATION DU POINT DE CONTACT
C IN  FFL    : FONCTIONS DE FORMES LAGRANGES
C IN  JACOBI : JACOBIEN DE LA MAILLE AU POINT DE CONTACT
C IN  NDEXCL : NUMERO DU NOEUD (MILIEU) EXCLU
C IN  JEU    : VALEUR DU JEU
C IN  COEFCP : COEF_PENA_CONT
C IN  COEFCS : COEF_STAB_CONT
C IN  DLAGRC : INCREMENT DEPDEL DU LAGRANGIEN DE CONTACT
C OUT VECTCC : VECTEUR ELEMENTAIRE LAGR_C
C
C ----------------------------------------------------------------------
C
      INTEGER   I
      INTEGER   IBID,NDEXCL(9),IEXCL      
C
C ----------------------------------------------------------------------
C
C
      IF (PHASE.EQ.'SANS') THEN
        DO 61 I = 1,NNL
          VECTCC(I) = VECTCC(I)-
     &                HPG*FFL(I)*DLAGRC*JACOBI/COEFCS
 61     CONTINUE
      ELSEIF (PHASE.EQ.'PSAN') THEN
        DO 64 I = 1,NNL
          VECTCC(I) = VECTCC(I)-
     &                HPG*FFL(I)*DLAGRC*JACOBI/COEFCP
 64     CONTINUE
      ELSEIF (PHASE.EQ.'PCON') THEN
        DO 63 I = 1,NNL
          VECTCC(I) = VECTCC(I)-
     &                HPG*FFL(I)*DLAGRC*JACOBI/COEFCP
     &               -HPG*FFL(I)*JEU*JACOBI 
 63     CONTINUE
      ELSEIF (PHASE.EQ.'CONT') THEN
        DO 62 I = 1,NNL      
          VECTCC(I) = VECTCC(I)-
     &                HPG*FFL(I)*JEU*JACOBI    
 62     CONTINUE
      ELSEIF (PHASE.EQ.'EXCL') THEN 
        CALL MMEXNO(TYPBAR,IBID  ,NDEXCL)
        DO 78 IEXCL = 1,9   
          IF (NDEXCL(IEXCL).EQ.1) THEN
            VECTCC(IEXCL) = 0D0
          ENDIF  
 78     CONTINUE 
        IF (TYPRAC.NE.0) THEN
          CALL MMEXNO(IBID  ,TYPRAC,NDEXCL)
          DO 79 IEXCL = 1,9   
            IF (NDEXCL(IEXCL).EQ.1) THEN
              VECTCC(IEXCL) = 0D0
            ENDIF  
 79       CONTINUE
        ENDIF      
               
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF
C
      END
