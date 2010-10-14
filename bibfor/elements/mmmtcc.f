      SUBROUTINE MMMTCC(PHASE ,NNL   ,HPG   ,FFL   ,JACOBI,
     &                  TYPBAR,TYPRAC,COEFCP,COEFCS,CWEAR ,
     &                  DISSIP,MATRCC)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 11/10/2010   AUTEUR ABBAS M.ABBAS 
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
      REAL*8       HPG,FFL(9),JACOBI
      REAL*8       COEFCP,COEFCS
      REAL*8       CWEAR,DISSIP
      REAL*8       MATRCC(9,9)
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
C
C CALCUL DE LA MATRICE LAGR_C/LAGR_C
C
C ----------------------------------------------------------------------
C
C
C IN  PHASE  : PHASE DE CALCUL
C              'SANS' - PAS DE CONTACT
C              'USUR' - USURE D'ARCHARD
C              'EXCL' - EXCLUSION D'UN NOEUD
C              'PSAN' - PENALISATION - PAS DE CONTACT
C              'PCON' - PENALISATION - CONTACT
C IN  NNL    : NOMBRE DE NOEUDS LAGRANGE 
C IN  HPG    : POIDS DU POINT INTEGRATION DU POINT DE CONTACT
C IN  FFL    : FONCTIONS DE FORMES LAGR.
C IN  TYPBAR : NUMERO DU NOEUD (MILIEU) EXCLU
C IN  JACOBI : JACOBIEN DE LA MAILLE AU POINT DE CONTACT
C IN  COEFCP : COEF_PENA_CONT
C IN  COEFCS : COEF_STAB_CONT
C IN  CWEAR  : COEFFICIENT D'USURE (KWEAR/HWEAR)
C IN  DISSIP : DISSIPATION CUMULEE POUR L'USURE
C OUT MATRCC : MATRICE ELEMENTAIRE LAGR_C/LAGR_C
C
C ----------------------------------------------------------------------
C
      INTEGER   INOC1,INOC2
      INTEGER   NDEXCL(9),IEXCL,I
C
C ----------------------------------------------------------------------
C
      DO 10 I = 1,9
        NDEXCL(I) = 0
 10   CONTINUE
C 
      IF (PHASE.EQ.'SANS') THEN
        DO 61 INOC1 = 1,NNL
          DO 51 INOC2 = 1,NNL
            MATRCC(INOC1,INOC2) = MATRCC(INOC1,INOC2)-
     &                    HPG*JACOBI/COEFCS*
     &                    FFL(INOC2)*FFL(INOC1)
 51       CONTINUE
 61     CONTINUE
      ELSEIF (PHASE.EQ.'USUR') THEN
        DO 64 INOC1 = 1,NNL
          DO 54 INOC2 = 1,NNL
            MATRCC(INOC1,INOC2) = MATRCC(INOC1,INOC2)+
     &                    HPG*JACOBI*(CWEAR*DISSIP)*
     &                    FFL(INOC2)*FFL(INOC1)
   54     CONTINUE
   64   CONTINUE
      ELSEIF (PHASE.EQ.'EXCL') THEN 
        CALL MMEXN1(TYPBAR,NDEXCL)
        DO 78 IEXCL = 1,9   
          IF (NDEXCL(IEXCL).EQ.1) THEN
            MATRCC(IEXCL,IEXCL) = - FFL(IEXCL)*FFL(IEXCL)
          ENDIF  
   78   CONTINUE 
        IF (TYPRAC.NE.0) THEN
          CALL MMEXN2(TYPRAC,NDEXCL)
          DO 79 IEXCL = 1,9   
            IF (NDEXCL(IEXCL).EQ.1) THEN
              MATRCC(IEXCL,IEXCL) = - FFL(IEXCL)*FFL(IEXCL)
            ENDIF  
   79     CONTINUE
        ENDIF
      ELSEIF ((PHASE.EQ.'PSAN').OR.(PHASE.EQ.'PCON')) THEN
        DO 62 INOC1 = 1,NNL
          DO 52 INOC2 = 1,NNL
            MATRCC(INOC1,INOC2) = MATRCC(INOC1,INOC2)-
     &                    HPG*JACOBI/COEFCP*
     &                    FFL(INOC2)*FFL(INOC1)
 52       CONTINUE
 62     CONTINUE
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF
C
      END
