      SUBROUTINE MMMMAT(PHASE ,
     &                  LUSURE,LSTABC,LCOMPL,LFROTT,LSTABF,
     &                  LADHER,LGLISS,NDIM  ,NNE   ,NNM   ,
     &                  NNL   ,NBCPS ,NORM  ,TAU1  ,TAU2  ,
     &                  MPROJN,MPROJT,HPG   ,FFE   ,FFM   ,
     &                  FFL   ,JACOBI,COEFCP,COEFCR,COEFCS,
     &                  COEFFP,COEFFR,COEFFS,COEFFF,JEU   ,
     &                  LAMBDA,RESE  ,NRESE ,TYPBAR,TYPRAC,
     &                  NDEXFR,ASPERI,KAPPAN,KAPPAV,DELTAT,
     &                  BETA  ,GAMMA ,CWEAR ,DISSIP,DLAGRC,
     &                  DELUSU,MATREE,MATRMM,MATREM,MATRME,
     &                  MATRCE,MATRCM,MATRMC,MATREC,MATRCC,
     &                  MATRFE,MATRFM,MATREF,MATRMF,MATRFF)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 26/01/2010   AUTEUR DESOZA T.DESOZA 
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
      INTEGER      TYPBAR,TYPRAC,NDEXFR
      LOGICAL      LUSURE,LSTABC,LCOMPL,LFROTT,LSTABF
      LOGICAL      LADHER,LGLISS
      REAL*8       NORM(3),TAU1(3),TAU2(3)
      REAL*8       MPROJN(3,3),MPROJT(3,3)       
      REAL*8       FFE(9),FFM(9),FFL(9)
      REAL*8       HPG,JACOBI
      REAL*8       RESE(3),NRESE      
      REAL*8       COEFCP,COEFCR,COEFCS
      REAL*8       COEFFP,COEFFR,COEFFS
      REAL*8       LAMBDA,COEFFF
      REAL*8       JEU
      REAL*8       KAPPAN,KAPPAV,ASPERI  
      REAL*8       BETA,GAMMA,DELTAT
      REAL*8       DLAGRC,DELUSU(3),DISSIP,CWEAR       
      REAL*8       MATREM(27,27),MATRME(27,27)
      REAL*8       MATREE(27,27),MATRMM(27,27)
      REAL*8       MATRCE(9,27) ,MATRCM(9,27)
      REAL*8       MATREC(27,9) ,MATRMC(27,9)
      REAL*8       MATRCC(9,9)
      REAL*8       MATRFF(18,18)   
      REAL*8       MATRFE(18,27),MATRFM(18,27)  
      REAL*8       MATRMF(27,18),MATREF(27,18)      
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
C
C CALCUL DES MATRICES
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
C IN  MPROJN : MATRICE DE PROJECTION NORMALE
C IN  MPROJT : MATRICE DE PROJECTION TANGENTE
C IN  HPG    : POIDS DU POINT INTEGRATION DU POINT DE CONTACT
C IN  FFE    : FONCTIONS DE FORMES DEPL. ESCL.
C IN  FFM    : FONCTIONS DE FORMES DEPL. MAIT.
C IN  JACOBI : JACOBIEN DE LA MAILLE AU POINT DE CONTACT
C IN  COEFCP : COEF_PENA_CONT
C IN  COEFCR : COEF_REGU_CONT
C IN  COEFFR : COEF_REGU_FROT
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
C IN  BETA   : COEFFICIENT SCHEMA DE NEWMARK
C IN  GAMMA  : COEFFICIENT SCHEMA DE NEWMARK
C IN  CWEAR  : COEFFICIENT D'USURE (KWEAR/HWEAR)
C IN  DLAGRC : LAGR_C DEPDEL DU POINT DE CONTACT (USURE UNILATERALE)
C IN  DELUSU : SAUT TGT DE L'INCREMENT DE DEPLACEMENT [[DELTA_U]]_TAU
C IN  DISSIP : DISSIPATION USURE
C OUT MATREE : MATRICE ELEMENTAIRE DEPL_E/DEPL_E
C OUT MATRMM : MATRICE ELEMENTAIRE DEPL_M/DEPL_M
C OUT MATRME : MATRICE ELEMENTAIRE DEPL_M/DEPL_E
C OUT MATREM : MATRICE ELEMENTAIRE DEPL_E/DEPL_M
C OUT MATRCE : MATRICE ELEMENTAIRE LAGR_C/DEPL_E
C OUT MATRCM : MATRICE ELEMENTAIRE LAGR_C/DEPL_M
C OUT MATRMC : MATRICE ELEMENTAIRE DEPL_M/LAGR_C
C OUT MATREC : MATRICE ELEMENTAIRE DEPL_E/LAGR_C
C OUT MATRCC : MATRICE ELEMENTAIRE LAGR_C/LAGR_C
C
C ----------------------------------------------------------------------
C
      IF (PHASE.EQ.'SANS') THEN
        IF (LFROTT) THEN
          CALL MMMTFF('SANS',NDIM  ,NBCPS ,NNL   ,HPG   ,
     &                FFL   ,JACOBI,TAU1  ,TAU2  ,RESE  ,
     &                NRESE ,LAMBDA,NDEXFR,COEFFS,COEFFF,
     &                MATRFF)  
        ELSE
          CALL MMMTCC('SANS',NNL   ,HPG   ,FFL   ,JACOBI,
     &                TYPBAR,TYPRAC,COEFCS,CWEAR ,DISSIP,
     &                MATRCC)        
        ENDIF     
      ELSEIF (PHASE.EQ.'CONT') THEN
        CALL MMMTCU('CONT',NDIM  ,NNL   ,NNE   ,NNM   ,
     &              NORM  ,HPG   ,FFL   ,FFE   ,FFM   ,
     &              JACOBI,TYPBAR,TYPRAC,COEFCR,CWEAR ,
     &              DISSIP,DLAGRC,DELUSU,MATRCE,MATRCM,
     &              MATREC,MATRMC)  
        IF (LUSURE) THEN
          CALL MMMTCC('USUR',NNL   ,HPG   ,FFL   ,JACOBI,
     &                TYPBAR,TYPRAC,COEFCS,CWEAR ,DISSIP,
     &                MATRCC)   
          CALL MMMTUU('USUR',NDIM  ,NNE   ,NNM   ,NORM  ,
     &                MPROJN,MPROJT,HPG   ,FFE   ,FFM   ,
     &                JACOBI,COEFCP,COEFCR,COEFFP,COEFFF,
     &                RESE  ,NRESE ,LAMBDA,COEFFR,JEU   ,
     &                ASPERI,KAPPAN,KAPPAV,DELTAT,BETA  ,
     &                GAMMA ,CWEAR ,DISSIP,DLAGRC,DELUSU,
     &                MATREE,MATRMM,MATREM,MATRME)
          CALL MMMTCU('USUR',NDIM  ,NNL   ,NNE   ,NNM   ,
     &              NORM  ,HPG   ,FFL   ,FFE   ,FFM   ,
     &              JACOBI,TYPBAR,TYPRAC,COEFCR,CWEAR ,
     &              DISSIP,DLAGRC,DELUSU,MATRCE,MATRCM,
     &              MATREC,MATRMC)     
        ENDIF  
        IF (LSTABC) THEN
          CALL MMMTUU('STAC',NDIM  ,NNE   ,NNM   ,NORM  ,
     &                MPROJN,MPROJT,HPG   ,FFE   ,FFM   ,
     &                JACOBI,COEFCP,COEFCR,COEFFP,COEFFF,
     &                RESE  ,NRESE ,LAMBDA,COEFFR,JEU   ,
     &                ASPERI,KAPPAN,KAPPAV,DELTAT,BETA  ,
     &                GAMMA ,CWEAR ,DISSIP,DLAGRC,DELUSU,
     &                MATREE,MATRMM,MATREM,MATRME)            
        ENDIF                 
        IF (LCOMPL) THEN
          CALL MMMTUU('COMP',NDIM  ,NNE   ,NNM   ,NORM  ,
     &                MPROJN,MPROJT,HPG   ,FFE   ,FFM   ,
     &                JACOBI,COEFCP,COEFCR,COEFFP,COEFFF,
     &                RESE  ,NRESE ,LAMBDA,COEFFR,JEU   ,
     &                ASPERI,KAPPAN,KAPPAV,DELTAT,BETA  ,
     &                GAMMA ,CWEAR ,DISSIP,DLAGRC,DELUSU,
     &                MATREE,MATRMM,MATREM,MATRME)
        ENDIF
      ELSEIF (PHASE.EQ.'FROT') THEN
          IF (LADHER) THEN
            CALL MMMTFU('ADHE',NDIM  ,NNL   ,NNE   ,NNM   ,
     &                  NBCPS ,HPG   ,JACOBI,FFL   ,FFE   ,
     &                  FFM   ,TAU1  ,TAU2  ,MPROJT,RESE  ,
     &                  NRESE ,LAMBDA,NDEXFR,COEFFF,COEFFS,
     &                  COEFFR,MATRFE,MATRFM,MATREF,MATRMF)
            IF (LSTABF) THEN
              CALL MMMTUU('STAF',NDIM  ,NNE   ,NNM   ,NORM  ,
     &                    MPROJN,MPROJT,HPG   ,FFE   ,FFM   ,
     &                    JACOBI,COEFCP,COEFCR,COEFFP,COEFFF,
     &                    RESE  ,NRESE ,LAMBDA,COEFFR,JEU   ,
     &                    ASPERI,KAPPAN,KAPPAV,DELTAT,BETA  ,
     &                    GAMMA ,CWEAR ,DISSIP,DLAGRC,DELUSU,
     &                    MATREE,MATRMM,MATREM,MATRME)             
            ENDIF
          ELSE IF (LGLISS) THEN
            CALL MMMTFF('GLIS',NDIM  ,NBCPS ,NNL   ,HPG   ,
     &                  FFL   ,JACOBI,TAU1  ,TAU2  ,RESE  ,
     &                  NRESE ,LAMBDA,NDEXFR,COEFFS,COEFFF,
     &                  MATRFF)
            CALL MMMTFU('GLIS',NDIM  ,NNL   ,NNE   ,NNM   ,
     &                  NBCPS ,HPG   ,JACOBI,FFL   ,FFE   ,
     &                  FFM   ,TAU1  ,TAU2  ,MPROJT,RESE  ,
     &                  NRESE ,LAMBDA,NDEXFR,COEFFF,COEFFS,
     &                  COEFFR,MATRFE,MATRFM,MATREF,MATRMF)  
            CALL MMMTUU('GLIS',NDIM  ,NNE   ,NNM   ,NORM  ,
     &                  MPROJN,MPROJT,HPG   ,FFE   ,FFM   ,
     &                  JACOBI,COEFCP,COEFCR,COEFFP,COEFFF,
     &                  RESE  ,NRESE ,LAMBDA,COEFFR,JEU   ,
     &                  ASPERI,KAPPAN,KAPPAV,DELTAT,BETA  ,
     &                  GAMMA ,CWEAR ,DISSIP,DLAGRC,DELUSU,
     &                  MATREE,MATRMM,MATREM,MATRME)
          ELSE
            CALL ASSERT(.FALSE.) 
          END IF        
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF
C
C --- MODIFICATION DES TERMES SI FOND_FISSURE
C
      IF (TYPBAR.NE.0) THEN
        CALL MMMTCC('SANS',NNL   ,HPG   ,FFL   ,JACOBI,
     &                TYPBAR,TYPRAC,COEFCS,CWEAR ,DISSIP,
     &                MATRCC)
        CALL MMMTCC('EXCL',NNL   ,HPG   ,FFL   ,JACOBI,
     &                TYPBAR,TYPRAC,COEFCS,CWEAR ,DISSIP,
     &                MATRCC)
        CALL MMMTCU('EXCL',NDIM  ,NNL   ,NNE   ,NNM   ,
     &              NORM  ,HPG   ,FFL   ,FFE   ,FFM   ,
     &              JACOBI,TYPBAR,TYPRAC,COEFCR,CWEAR ,
     &              DISSIP,DLAGRC,DELUSU,MATRCE,MATRCM,
     &              MATREC,MATRMC) 
      ENDIF 
C
C --- MODIFICATION DES TERMES SI LINE_QUAD
C
      IF (TYPRAC.NE.0) THEN
        CALL MMMTCC('SANS',NNL   ,HPG   ,FFL   ,JACOBI,
     &                TYPBAR,TYPRAC,COEFCS,CWEAR ,DISSIP,
     &                MATRCC)
        CALL MMMTCC('EXCL',NNL   ,HPG   ,FFL   ,JACOBI,
     &                TYPBAR,TYPRAC,COEFCS,CWEAR ,DISSIP,
     &                MATRCC)
        CALL MMMTCU('EXCL',NDIM  ,NNL   ,NNE   ,NNM   ,
     &              NORM  ,HPG   ,FFL   ,FFE   ,FFM   ,
     &              JACOBI,TYPBAR,TYPRAC,COEFCR,CWEAR ,
     &              DISSIP,DLAGRC,DELUSU,MATRCE,MATRCM,
     &              MATREC,MATRMC) 
      ENDIF              
C
C --- MODIFICATION DES TERMES SI EXCLUSION DIRECTION FROTT. SANS_NO_FR  
C
      IF (NDEXFR.NE.0) THEN
        CALL MMMTFF('EXFR',NDIM  ,NBCPS ,NNL   ,HPG   ,
     &              FFL   ,JACOBI,TAU1  ,TAU2  ,RESE  ,
     &              NRESE ,LAMBDA,NDEXFR,COEFFS,COEFFF,
     &              MATRFF)      
        CALL MMMTFU('EXFR',NDIM  ,NNL   ,NNE   ,NNM   ,
     &              NBCPS ,HPG   ,JACOBI,FFL   ,FFE   ,
     &              FFM   ,TAU1  ,TAU2  ,MPROJT,RESE  ,
     &              NRESE ,LAMBDA,NDEXFR,COEFFF,COEFFS,
     &              COEFFR,MATRFE,MATRFM,MATREF,MATRMF) 
      ENDIF       
C
      END
