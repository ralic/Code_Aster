      FUNCTION POSPAN(DIM,NO,INO,NNO,PAN)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 16/12/2004   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C                                                                       
C                                                                       
C ======================================================================
C ----------------------------------------------------------------------
C         POSITION D'UN ENSEMBLE DE POINTS PAR RAPPORT A UN PAN
C ----------------------------------------------------------------------
C VARIABLES D'ENTREE 
C INTEGER  DIM          : DIMENSION DE L'ESPACE 
C REAL*8   NO(DIM,*)    : COORDONNEES DES POINTS
C INTEGER  INO(NNO)     : LISTE DE POINTS INDEX DANS NO
C INTEGER  NNO          : NOMBRE DE POINTS
C REAL*8   PAN(DIM+1)   : EQUATION DU PAN
C
C VALEUR RETOURNEE      : -1 : LES POINTS SONT TOUS A UNE DISTANCE 
C                              NEGLIGEABLE DU PAN
C                          0 : LES POINTS SONT DE PART EN PART DU PAN
C                          1 : LES POINTS SONT TOUS DU COTE - DU PAN
C                          2 : LES POINTS SONT TOUS DU COTE + DU PAN
C ----------------------------------------------------------------------

      IMPLICIT NONE

C --- PARAMETRES
      REAL*8 PREC1, PREC2
      PARAMETER (PREC1 = 0.005D0)
      PARAMETER (PREC2 = 0.0001D0)

C --- FONCTIONS
      REAL*8  DDOT
      INTEGER POSPAN

C --- VARIABLES
      INTEGER DIM,INO(*),NNO,I,J
      REAL*8  NO(DIM,*),PAN(*),R(8)

      DO 10 I = 1, NNO
        R(I) = PAN(DIM+1) + DDOT(DIM,PAN,1,NO(1,INO(I)),1)
 10   CONTINUE

      I = 1
      IF (ABS(R(1)).LT.PREC1) THEN
        DO 20 I = 2, NNO
          IF (ABS(R(I)).GE.PREC1) GOTO 30
 20     CONTINUE
        POSPAN = -1
        GOTO 60
      ENDIF
            
 30   CONTINUE
            
      POSPAN = 0 
      IF (R(I).LT.0.D0) THEN
        DO 40 J = 1, NNO
          IF (R(J).GT.PREC2) GOTO 60
 40     CONTINUE
        POSPAN = 1
      ELSE
        DO 50 J = 1, NNO
          IF (R(J).LT.-PREC2) GOTO 60
 50     CONTINUE
        POSPAN = 2
      ENDIF
          
 60   CONTINUE

      END
