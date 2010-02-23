      SUBROUTINE MMMVEC(PHASE ,
     &                LSTABC,LPENAC,LCOMPL,LFROTT,LSTABF,
     &                LPENAF,LADHER,LGLISS,NDIM  ,NNE   ,
     &                NNM   ,NNL   ,NBCPS ,NORM  ,TAU1  ,
     &                TAU2  ,MPROJT,HPG   ,FFE   ,FFM   ,
     &                FFL   ,JACOBI,COEFCP,COEFCR,COEFCS,
     &                COEFFP,COEFFR,COEFFS,COEFFF,JEU   ,
     &                LAMBDA,RESE  ,NRESE ,TYPBAR,TYPRAC,
     &                NDEXFR,ASPERI,KAPPAN,KAPPAV,DLAGRC,
     &                DLAGRF,DDEPLE,DDEPLM,JEVITP,VECTEE,
     &                VECTMM,VECTCC,VECTFF)
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
C TOLE CRP_21
C
      IMPLICIT NONE
      CHARACTER*4  PHASE
      INTEGER      NDIM,NNE,NNM,NNL,NBCPS
      INTEGER      TYPRAC,TYPBAR,NDEXFR
      LOGICAL      LSTABC,LCOMPL,LFROTT,LSTABF
      LOGICAL      LADHER,LGLISS,LPENAF,LPENAC
      REAL*8       NORM(3),TAU1(3),TAU2(3)
      REAL*8       MPROJT(3,3)       
      REAL*8       FFE(9),FFM(9),FFL(9)
      REAL*8       HPG,JACOBI
      REAL*8       RESE(3),NRESE      
      REAL*8       COEFCP,COEFCR,COEFCS
      REAL*8       COEFFP,COEFFR,COEFFS
      REAL*8       LAMBDA,COEFFF
      REAL*8       JEU
      REAL*8       KAPPAN,KAPPAV,ASPERI,JEVITP 
      REAL*8       DDEPLE(3),DDEPLM(3)
      REAL*8       DLAGRC,DLAGRF(2) 
      REAL*8       VECTCC(9)
      REAL*8       VECTFF(18)    
      REAL*8       VECTEE(27),VECTMM(27)     
C
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
C
C CALCUL DES VECTEURS
C
C ----------------------------------------------------------------------
C
C
C IN  PHASE  : PHASE DE CALCUL
C              'CONT' - CONTACT 
C              'SANS' - SANS CONTACT
C IN  NDIM   : DIMENSION DU PROBLEME
C IN  NNE    : NOMBRE DE NOEUDS DE LA MAILLE ESCLAVE
C IN  NNM    : NOMBRE DE NOEUDS DE LA MAILLE MAITRE
C IN  NORM   : NORMALE AU POINT DE CONTACT
C IN  MPROJT : MATRICE DE PROJECTION TANGENTE
C IN  HPG    : POIDS DU POINT INTEGRATION DU POINT DE CONTACT
C IN  FFE    : FONCTIONS DE FORMES DEPL. ESCL.
C IN  FFM    : FONCTIONS DE FORMES DEPL. MAIT.
C IN  JACOBI : JACOBIEN DE LA MAILLE AU POINT DE CONTACT
C IN  COEFCP : COEF_PENA_CONT
C IN  LAMBDA : VALEUR DU MULT. DE CONTACT (SEUIL DE TRESCA)
C IN  RESE   : SEMI-MULTIPLICATEUR GTK DE FROTTEMENT 
C               GTK = LAMBDAF + COEFFR*VITESSE
C IN  NRESE  : NORME DU SEMI-MULTIPLICATEUR GTK DE FROTTEMENT 
C IN  COEFFF : COEFFICIENT DE FROTTEMENT DE COULOMB
C IN  COEFFP : COEF_PENA_FROT
C IN  JEU    : VALEUR DU JEU
C IN  ASPERI : PARAMATRE A DU MODELE DE COMPLIANCE
C IN  KAPPAN : COEFFICIENT KN DU MODELE DE COMPLIANCE
C IN  KAPPAV : COEFFICIENT KV DU MODELE DE COMPLIANCE
C IN  DELTAT : INCREMENT DE TEMPS
C IN  DLAGRC : LAGR_C DEPDEL DU POINT DE CONTACT (USURE UNILATERALE)
C IN  DISSIP : DISSIPATION USURE
C OUT VECTEE : VECTEUR ELEMENTAIRE DEPL_E
C OUT VECTMM : VECTEUR ELEMENTAIRE DEPL_M
C OUT VECTCC : VECTEUR ELEMENTAIRE LAGR_C
C OUT VECTFF : VECTEUR ELEMENTAIRE LAGR_F
C
C ----------------------------------------------------------------------
C
      IF (PHASE.EQ.'SANS') THEN     
        IF (LFROTT) THEN
          IF(LPENAF) THEN
            CALL MMMVFF('PSAN',NDIM  ,NNL   ,NBCPS ,HPG   ,
     &                  FFL   ,TAU1  ,TAU2  ,JACOBI,NDEXFR,
     &                  COEFFS,COEFFP,DLAGRF,RESE  ,LAMBDA,
     &                  COEFFF,DDEPLE,DDEPLM,MPROJT,VECTFF)
          ELSE
            CALL MMMVFF('SANS',NDIM  ,NNL   ,NBCPS ,HPG   ,
     &                  FFL   ,TAU1  ,TAU2  ,JACOBI,NDEXFR,
     &                  COEFFS,COEFFP,DLAGRF,RESE  ,LAMBDA,
     &                  COEFFF,DDEPLE,DDEPLM,MPROJT,VECTFF)
          ENDIF
        ELSE
          IF(LPENAC) THEN
            CALL MMMVCC('PSAN',NNL   ,HPG   ,FFL   ,JACOBI,
     &                  JEU   ,TYPBAR,TYPRAC,COEFCP,COEFCS,
     &                  DLAGRC,VECTCC)
          ELSE
            CALL MMMVCC('SANS',NNL   ,HPG   ,FFL   ,JACOBI,
     &                  JEU   ,TYPBAR,TYPRAC,COEFCP,COEFCS,
     &                  DLAGRC,VECTCC)
          ENDIF
        ENDIF 
        IF (LCOMPL) THEN
          CALL MMMVUU('ASPE',NDIM  ,NNE   ,NNM   ,NORM  ,
     &                TAU1  ,TAU2  ,MPROJT,HPG   ,FFE   ,
     &                FFM   ,JACOBI,JEU   ,COEFCP,COEFFP,
     &                DLAGRC,KAPPAN,KAPPAV,ASPERI,JEVITP,
     &                LAMBDA,COEFFF,DLAGRF,DDEPLE,DDEPLM,
     &                RESE  ,NRESE ,VECTEE,VECTMM)
        ENDIF
      ELSEIF (PHASE.EQ.'CONT') THEN
        IF (LPENAC) THEN
          CALL MMMVUU('PCON',NDIM  ,NNE   ,NNM   ,NORM  ,
     &                TAU1  ,TAU2  ,MPROJT,HPG   ,FFE   ,
     &                FFM   ,JACOBI,JEU   ,COEFCP,COEFFP,
     &                DLAGRC,KAPPAN,KAPPAV,ASPERI,JEVITP,
     &                LAMBDA,COEFFF,DLAGRF,DDEPLE,DDEPLM,
     &                RESE  ,NRESE ,VECTEE,VECTMM)
          CALL MMMVCC('PCON',NNL   ,HPG   ,FFL   ,JACOBI,
     &                JEU   ,TYPBAR,TYPRAC,COEFCP,COEFCS,
     &                DLAGRC,VECTCC)
        ELSE
          CALL MMMVCC('CONT',NNL   ,HPG   ,FFL   ,JACOBI,
     &                JEU   ,TYPBAR,TYPRAC,COEFCP,COEFCS,
     &                DLAGRC,VECTCC)
          CALL MMMVUU('CONT',NDIM  ,NNE   ,NNM   ,NORM  ,
     &                TAU1  ,TAU2  ,MPROJT,HPG   ,FFE   ,
     &                FFM   ,JACOBI,JEU   ,COEFCP,COEFFP,
     &                DLAGRC,KAPPAN,KAPPAV,ASPERI,JEVITP,
     &                LAMBDA,COEFFF,DLAGRF,DDEPLE,DDEPLM,
     &                RESE  ,NRESE ,VECTEE,VECTMM)
          IF (LSTABC) THEN
            CALL MMMVUU('STAC',NDIM  ,NNE   ,NNM   ,NORM  ,
     &                 TAU1  ,TAU2  ,MPROJT,HPG   ,FFE   ,
     &                 FFM   ,JACOBI,JEU   ,COEFCP,COEFFP,
     &                 DLAGRC,KAPPAN,KAPPAV,ASPERI,JEVITP,
     &                 LAMBDA,COEFFF,DLAGRF,DDEPLE,DDEPLM,
     &                 RESE  ,NRESE ,VECTEE,VECTMM)
          ENDIF
          IF (LCOMPL) THEN
            CALL MMMVUU('COMP',NDIM  ,NNE   ,NNM   ,NORM  ,
     &                 TAU1  ,TAU2  ,MPROJT,HPG   ,FFE   ,
     &                 FFM   ,JACOBI,JEU   ,COEFCP,COEFFP,
     &                 DLAGRC,KAPPAN,KAPPAV,ASPERI,JEVITP,
     &                 LAMBDA,COEFFF,DLAGRF,DDEPLE,DDEPLM,
     &                 RESE  ,NRESE ,VECTEE,VECTMM)
          ENDIF 
        ENDIF
      ELSEIF (PHASE.EQ.'FROT') THEN
          IF (LADHER) THEN
            IF ( LPENAF) THEN
              CALL MMMVUU('PADH',NDIM  ,NNE   ,NNM   ,NORM  ,
     &                   TAU1  ,TAU2  ,MPROJT,HPG   ,FFE   ,
     &                   FFM   ,JACOBI,JEU   ,COEFCP,COEFFP,
     &                   DLAGRC,KAPPAN,KAPPAV,ASPERI,JEVITP,
     &                   LAMBDA,COEFFF,DLAGRF,DDEPLE,DDEPLM,
     &                   RESE  ,NRESE ,VECTEE,VECTMM)
              CALL MMMVFF('PADH',NDIM  ,NNL   ,NBCPS ,HPG   ,
     &                    FFL   ,TAU1  ,TAU2  ,JACOBI,NDEXFR,
     &                    COEFFS,COEFFP,DLAGRF,RESE  ,LAMBDA,
     &                    COEFFF,DDEPLE,DDEPLM,MPROJT,VECTFF)
            ELSE
              CALL MMMVUU('ADHE',NDIM  ,NNE   ,NNM   ,NORM  ,
     &                   TAU1  ,TAU2  ,MPROJT,HPG   ,FFE   ,
     &                   FFM   ,JACOBI,JEU   ,COEFCP,COEFFP,
     &                   DLAGRC,KAPPAN,KAPPAV,ASPERI,JEVITP,
     &                   LAMBDA,COEFFF,DLAGRF,DDEPLE,DDEPLM,
     &                   RESE  ,NRESE ,VECTEE,VECTMM)
              CALL MMMVFF('ADHE',NDIM  ,NNL   ,NBCPS ,HPG   ,
     &                    FFL   ,TAU1  ,TAU2  ,JACOBI,NDEXFR,
     &                    COEFFS,COEFFP,DLAGRF,RESE  ,LAMBDA,
     &                    COEFFF,DDEPLE,DDEPLM,MPROJT,VECTFF)
              IF (LSTABF) THEN
                CALL MMMVUU('STAF',NDIM  ,NNE   ,NNM   ,NORM  ,
     &                   TAU1  ,TAU2  ,MPROJT,HPG   ,FFE   ,
     &                   FFM   ,JACOBI,JEU   ,COEFCP,COEFFP,
     &                   DLAGRC,KAPPAN,KAPPAV,ASPERI,JEVITP,
     &                   LAMBDA,COEFFF,DLAGRF,DDEPLE,DDEPLM,
     &                   RESE  ,NRESE ,VECTEE,VECTMM)
              ENDIF
            ENDIF
          ELSE IF (LGLISS) THEN
            IF (LPENAF) THEN
              CALL MMMVUU('PGLI',NDIM  ,NNE   ,NNM   ,NORM  ,
     &                   TAU1  ,TAU2  ,MPROJT,HPG   ,FFE   ,
     &                   FFM   ,JACOBI,JEU   ,COEFCP,COEFFP,
     &                   DLAGRC,KAPPAN,KAPPAV,ASPERI,JEVITP,
     &                   LAMBDA,COEFFF,DLAGRF,DDEPLE,DDEPLM,
     &                   RESE  ,NRESE ,VECTEE,VECTMM)
              CALL MMMVFF('PGLI',NDIM  ,NNL   ,NBCPS ,HPG   ,
     &                    FFL   ,TAU1  ,TAU2  ,JACOBI,NDEXFR,
     &                    COEFFS,COEFFP,DLAGRF,RESE  ,LAMBDA,
     &                    COEFFF,DDEPLE,DDEPLM,MPROJT,VECTFF)
            ELSE
              CALL MMMVUU('GLIS',NDIM  ,NNE   ,NNM   ,NORM  ,
     &                   TAU1  ,TAU2  ,MPROJT,HPG   ,FFE   ,
     &                   FFM   ,JACOBI,JEU   ,COEFCP,COEFFP,
     &                   DLAGRC,KAPPAN,KAPPAV,ASPERI,JEVITP,
     &                   LAMBDA,COEFFF,DLAGRF,DDEPLE,DDEPLM,
     &                   RESE  ,NRESE ,VECTEE,VECTMM)
              CALL MMMVFF('GLIS',NDIM  ,NNL   ,NBCPS ,HPG   ,
     &                    FFL   ,TAU1  ,TAU2  ,JACOBI,NDEXFR,
     &                    COEFFS,COEFFP,DLAGRF,RESE  ,LAMBDA,
     &                    COEFFF,DDEPLE,DDEPLM,MPROJT,VECTFF)
            ENDIF
          ELSE
            CALL ASSERT(.FALSE.) 
          END IF        
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF
C
      IF (PHASE.EQ.'FROT') THEN 
        GOTO 999
      ENDIF  
C
C --- MODIFICATION DES TERMES SI BARSOUM 
C
      IF (TYPBAR.NE.0) THEN
        CALL MMMVCC('EXCL',NNL   ,HPG   ,FFL   ,JACOBI,
     &              JEU   ,TYPBAR,TYPRAC,COEFCP,COEFCS,
     &              DLAGRC,VECTCC) 
        CALL MMMVUU('EXCL',NDIM  ,NNE   ,NNM   ,NORM  ,
     &              TAU1  ,TAU2  ,MPROJT,HPG   ,FFE   ,
     &              FFM   ,JACOBI,JEU   ,COEFCP,COEFFP,
     &              DLAGRC,KAPPAN,KAPPAV,ASPERI,JEVITP,
     &              LAMBDA,COEFFF,DLAGRF,DDEPLE,DDEPLM,
     &              RESE  ,NRESE ,VECTEE,VECTMM)
      ENDIF
C
C --- MODIFICATION DES TERMES SI RACCORD_LINE_QUAD
C
      IF (TYPRAC.NE.0) THEN
        CALL MMMVCC('EXCL',NNL   ,HPG   ,FFL   ,JACOBI,
     &              JEU   ,TYPBAR,TYPRAC,COEFCP,COEFCS,
     &              DLAGRC,VECTCC) 
        CALL MMMVUU('EXCL',NDIM  ,NNE   ,NNM   ,NORM  ,
     &              TAU1  ,TAU2  ,MPROJT,HPG   ,FFE   ,
     &              FFM   ,JACOBI,JEU   ,COEFCP,COEFFP,
     &              DLAGRC,KAPPAN,KAPPAV,ASPERI,JEVITP,
     &              LAMBDA,COEFFF,DLAGRF,DDEPLE,DDEPLM,
     &              RESE  ,NRESE ,VECTEE,VECTMM)
      ENDIF               
C
C --- MODIFICATION DES TERMES SI EXCLUSION DIRECTION FROTT. SANS_NO_FR  
C
      IF (NDEXFR.NE.0) THEN
        CALL MMMVFF('EXFR',NDIM  ,NNL   ,NBCPS ,HPG   ,
     &              FFL   ,TAU1  ,TAU2  ,JACOBI,NDEXFR,
     &              COEFFS,COEFFP,DLAGRF,RESE  ,LAMBDA,
     &              COEFFF,DDEPLE,DDEPLM,MPROJT,VECTFF)
      ENDIF 
C
  999 CONTINUE       
C
      END
