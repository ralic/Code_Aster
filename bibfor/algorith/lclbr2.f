      SUBROUTINE LCLBR2 (IMATE, COMPOR, NDIM, EPSM, T, E, SIGMT, SIGMC,
     &                   EPSIC, COMPN, GAMMA,POURC)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 18/04/2005   AUTEUR PBADEL P.BADEL 
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
      IMPLICIT NONE
      CHARACTER*16       COMPOR(*)
      INTEGER            IMATE,NDIM, T(3,3)
      REAL*8             EPSM(6), E, SIGMT, SIGMC, GAMMA, COMPN, EPSIC
      REAL*8             POURC      
C ----------------------------------------------------------------------
C     LOI DE COMPORTEMENT BETON REGLEMENTAIRE : INITIALISATION
C
C IN  COMPOR     : NOM DE LA LOI DE COMPORTEMENT
C IN  IMATE      : CODE MATERIAU
C IN  EPSM       : DEFORMATION AU TEMPS MOINS
C OUT T          : TENSEUR DE PLACEMENT (PASSAGE VECT -> MATRICE)
C OUT LAMBDA
C OUT DEUXMU
C OUT ALPHA
C OUT GAMMA
C OUT SEUIL
C ----------------------------------------------------------------------
      CHARACTER*2 CODRET(3)
      CHARACTER*8 NOMRES(6)
      INTEGER     I
      REAL*8      VALRES(6)
      REAL*8      K0, K1, SICR, TREPSM

      T(1,1)=1
      T(1,2)=4
      T(1,3)=5
      T(2,1)=4
      T(2,2)=2
      T(2,3)=6
      T(3,1)=5
      T(3,2)=6
      T(3,3)=3
      
C    LECTURE DES CARACTERISTIQUES DU MATERIAU
      NOMRES(1) = 'E'
      CALL RCVALA ( IMATE,' ','ELAS',0,' ',0.D0,1,
     &              NOMRES,VALRES,CODRET, 'FM')
      E     = VALRES(1)
C    LECTURE DES CARACTERISTIQUES D'ENDOMMAGEMENT
      NOMRES(1) = 'D_SIGM_EPSI'
      NOMRES(2) = 'SYT'
      NOMRES(3) = 'SYC'
      NOMRES(4) = 'EPSC'
      NOMRES(5) = 'N'
      NOMRES(6) = 'P_ENER'
      CALL RCVALA(IMATE,' ','BETON_REGLE_PR',0,' ',0.D0,6,
     &            NOMRES,VALRES,CODRET,' ')
      GAMMA  = - E/VALRES(1)
      SIGMT = VALRES(2)
      SIGMC = -VALRES(3)
      EPSIC = -VALRES(4)
      COMPN = VALRES(5)
      POURC = VALRES(6)
      
      END
