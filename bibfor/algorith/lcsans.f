       SUBROUTINE LCSANS (NDIM,IMATE,COMPOR,EPSM,DEPS,SIGM,VIM,
     &                    OPTION,SIGP,VIP,DSIDEP)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 04/03/2003   AUTEUR ASSIRE A.ASSIRE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C---&s---1---------2---------3---------4---------5---------6---------7--
C IN  NDIM    : DIMENSION DE L'ESPACE
C IN  IMATE   : ADRESSE DU MATERIAU CODE
C IN  COMPOR  : COMPORTEMENT : RELCOM ET DEFORM 
C IN  DEPS    : INCREMENT DE DEFORMATION TOTALE
C IN  SIGM    : CONTRAINTES A L'INSTANT DU CALCUL PRECEDENT
C IN  VIM     : VARIABLES INTERNES A L'INSTANT DU CALCUL PRECEDENT
C IN  OPTION  : OPTION DEMANDEE : RIGI_MECA_TANG, FULL_MECA ,RAPH_MECA
C OUT SIGP    : CONTRAINTES A L'INSTANT ACTUEL
C OUT VIP     : VARIABLES INTERNES A L'INSTANT ACTUEL
C OUT DSIDEP  : MATRICE CARREE
C
C_______________________________________________________________________
C
C
C ROUTINE CALCULANT UN COMPORTEMENT VIDE:
C
C    - CONTRAINTES FINALES NULLES         : SIGP (NSTRS)
C    - VARIABLES INTERNES FINALES NULLES  : VIP (NVARI)
C
C_______________________________________________________________________
C
      IMPLICIT NONE
      INTEGER NDIM,IMATE,NSTRS,I,J
      CHARACTER*16    COMPOR(3),OPTION
      REAL*8           VIM(*),VIP(*),SIGM(6),SIGP(6),DSIDEP(6,6)
      REAL*8           EPSM(6),DEPS(6)

C   DIMENSION
C      
      NSTRS = 2*NDIM 
C
C
      IF ((OPTION(1:9).EQ.'FULL_MECA').OR.
     &    (OPTION(1:9).EQ.'RAPH_MECA')) THEN

        DO 20 I=1,NSTRS
          SIGP(I) = 0.D0
   20   CONTINUE

      ENDIF

C_______________________________________________________________________
C
C CONSTRUCTION DE LA MATRICE TANGENTE
C_______________________________________________________________________
C
      IF ((OPTION(1:9).EQ.'FULL_MECA').OR.
     &     (OPTION(1:9).EQ.'RIGI_MECA')) THEN
C     
        DO 35 I=1,NSTRS
          DO 35 J=1,NSTRS
            DSIDEP(I,J) = 0.D0
35      CONTINUE

      ENDIF
C
      END
