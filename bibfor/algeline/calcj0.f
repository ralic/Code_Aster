      SUBROUTINE CALCJ0(T,SIGPRI,VALP)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 23/09/98   AUTEUR SABMTEC P.LACLERGUE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C ======================================================================
C TOLE CRP_18
C ----------------------------------------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
C     CALCUL LE MAXIMUM DES :
C                        . CONTRAINTES PRINCIPALES      (= 3 VALEURS)
C     AU MOYEN DE LA METHODE ITERATIVE DE JACOBI (ROUTINE JACOBI.F )
C     LE CALCUL EST EN DIM 3 (COHERENT AVEC CHOIX DE NMVPRK)
C ----------------------------------------------------------------------
C     IN     T     TENSEUR CONTRAINTE OU DEFORMATION (XX YY ZZ XY XZ YZ)
C            ON EST EN DIMENSION ESPACE = 3
C     OUT    VALP  VECTEUR DES GRANDEURS EQUIVALENTES
C ----------------------------------------------------------------------
C
C ----------------------------------------------------------------------
      REAL*8         T(6) ,    TB(6) , TS(6) , TU(6) , VECP(3,3)
      REAL*8         VALP(3) , JACAUX(3)
      REAL*8         TOL, TOLDYN
      REAL*8  SIGPRI
      INTEGER        NBVEC, NPERM
      INTEGER I, ITYPE, IORDRE
      DATA   NPERM ,TOL,TOLDYN    /12,1.D-10,1.D-2/
C ----------------------------------------------------------------------

C
C     ON RECALCULE LES TERMES REELS DU TENSEURS SACHANT
C     QUE LES TERMES NON DIAGONAUX ONT ETE MULTIPLIE PAR SQRT(2)
C
      DO 30 I=1,6
      TB(I)=T(I)
C      IF (I.GT.3) TB(I)=T(I)/RAC2
   30 CONTINUE
C
C     REANRANGEMENT POUR LA ROUTINE JACOBI EN COLONNE
C     A=(XX YY ZZ XY XZ YZ)->B=(XX XY XZ YY YZ ZZ)
C
      TS(1)=TB(1)
      TS(2)=TB(4)
      TS(3)=TB(5)
      TS(4)=TB(2)
      TS(5)=TB(6)
      TS(6)=TB(3)
C
C       MATRICE UNITE = (1 0 0 1 0 1) (POUR JACOBI)
C
        TU(1) = 1.D0
        TU(2) = 0.D0
        TU(3) = 0.D0
        TU(4) = 1.D0
        TU(5) = 0.D0
        TU(6) = 1.D0
C
C -       VALEURS PRINCIPALES
          NBVEC = 3
          ITYPE = 2
          IORDRE = 2
          CALL JACOBI(NBVEC,NPERM,TOL,TOLDYN,TS,TU,VECP,VALP(1),JACAUX,
     &                NITJAC,ITYPE,IORDRE)
C
C
         SIGPRI=MAX(VALP(1),VALP(2),VALP(3))
C
      END
