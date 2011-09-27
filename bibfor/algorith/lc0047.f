      SUBROUTINE LC0047(FAMI,KPG,KSP,NDIM,IMATE,COMPOR,CRIT,INSTAM,
     &              INSTAP,EPSM,DEPS,SIGM,VIM,OPTION,ANGMAS,SIGP,VIP,
     &              WKIN,WKOUT,TYPMOD,ICOMP,NVI,DSIDEP,CODRET)

C
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/09/2011   AUTEUR PROIX J-M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ======================================================================
C TOLE CRP_21
C ======================================================================
C
C    LOI D'ENDOMMAGEMENT D'UN MATERIAU ELASTIQUE HETEROGENE
C
C          RELATION : 'ENDO_HETEROGENE'
C
C       IN      NDIM    DIMENSION DE L ESPACE (3D=3,2D=2,1D=1)
C               TYPMOD  TYPE DE MODELISATION
C               OPTION     OPTION DE CALCUL A FAIRE
C                             'RIGI_MECA_TANG'> DSIDEP(T)
C                             'FULL_MECA'     > DSIDEP(T+DT) , SIG(T+DT)
C                             'RAPH_MECA'     > SIG(T+DT)
C               IMATE    ADRESSE DU MATERIAU CODE
C               EPSM   DEFORMATION TOTALE A T
C               DEPS   INCREMENT DE DEFORMATION TOTALE
C               VIM    VARIABLES INTERNES A T    + INDICATEUR ETAT T
C    ATTENTION  VIM    VARIABLES INTERNES A T MODIFIEES SI REDECOUPAGE
C       OUT     SIGP    CONTRAINTE A T+DT
C               VIP    VARIABLES INTERNES A T+DT + INDICATEUR ETAT T+DT
C               DSIDEP    MATRICE DE COMPORTEMENT TANGENT A T+DT OU T
C
C ----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER         IMATE,NDIM,KSP,KPG
      INTEGER         ICOMP,NVI
      INTEGER         CODRET
      REAL*8          ANGMAS(*)
      REAL*8          WKOUT(6,6),WKIN(6,6)
      CHARACTER*16    COMPOR(*),OPTION
      CHARACTER*8     TYPMOD(*)
      CHARACTER*(*)   FAMI
      REAL*8          EPSM(6),  DEPS(6),CRIT(*)
      REAL*8          SIGP(6),SIGM(*),INSTAM,INSTAP
      REAL*8          VIM(*),   VIP(*)
      REAL*8          DSIDEP(6,6)
C ----------------------------------------------------------------------
C      
C     FORMULATION NON-LOCALE AVEC REGULARISATION DES CONTRAINTES
      IF (TYPMOD(2).EQ.'GRADSIGM') THEN
        WKOUT(1,1)=WKIN(1,1)
        CALL LCBRGM(NDIM,TYPMOD,IMATE,EPSM,DEPS,
     &              VIM,OPTION,SIGP,VIP,DSIDEP,WKOUT,CODRET)

      ELSE IF ((TYPMOD(2).EQ.'GRADVARI').OR.
     &         (TYPMOD(2).EQ.'GRADEPSI'))THEN
        CALL U2MESS('F','COMPOR2_12')
C
      ENDIF

      END
