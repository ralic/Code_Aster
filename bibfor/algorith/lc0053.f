      SUBROUTINE LC0053(FAMI,KPG,KSP,NDIM,IMATE,COMPOR,CRIT,INSTAM,
     &            INSTAP,EPSM,DEPS,SIGM,VIM,OPTION,ANGMAS,SIGP,VIP,
     &            TAMPON,TYPMOD,ICOMP,NVI,DSIDEP,CODRET)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 15/02/2011   AUTEUR FLEJOU J-L.FLEJOU 
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
 
      IMPLICIT NONE
      INTEGER         IMATE,NDIM,CODRET,KPG,KSP
      INTEGER         ICOMP,NVI
      REAL*8          TAMPON(*),CRIT(*)
      REAL*8          EPSM(6),DEPS(6),SIGP(6),VIM(*),VIP(*),DSIDEP(6,6)
      REAL*8          INSTAM,INSTAP,SIGM(6),ANGMAS(3)
      CHARACTER*(*)   FAMI
      CHARACTER*8     TYPMOD(*)
      CHARACTER*16    OPTION,COMPOR(*)
C TOLE CRP_21
C ---------------------------------------------------------------------
C        BUT: LOI D'ENDOMMAGEMENT D'UN MATERIAU ELASTIQUE FRAGILE :
C                    RELATION : 'ENDO_CARRE'
C ---------------------------------------------------------------------
C
C     IN      NDIM    DIMENSION DE L ESPACE (3D=3,2D=2,1D=1)
C             TYPMOD  TYPE DE MODELISATION
C             OPTION  OPTION DE CALCUL A FAIRE
C                           'RIGI_MECA_TANG'> DSIDEP(T)
C                           'FULL_MECA'     > DSIDEP(T+DT) , SIG(T+DT)
C                           'RAPH_MECA'     > SIG(T+DT)
C             IMATE   ADRESSE DU MATERIAU CODE
C             EPSM    DEFORMATION TOTALE A T
C             DEPS    INCREMENT DE DEFORMATION TOTALE
C             VIM     VARIABLES INTERNES A T + INDICATEURS ETAT T
C  ATTENTION  VIM     VARIABLES INTERNES A T MODIFIEES SI REDECOUPAGE
C     OUT     SIGP    CONTRAINTE A T+DT
C             VIP     VARIABLES INTERNES A T+DT + INDICATEURS ETAT T+DT
C             DSIDEP  MATRICE DE COMPORTEMENT TANGENT A T+DT OU T
C
C ---------------------------------------------------------------------
        
C     FORMULATION NON-LOCALE A GRADIENT D'ENDOMMAGEMENT AUX NOEUDS
      IF (TYPMOD(2).EQ.'GDVARINO') THEN

        CALL LCKIMP(NDIM,TYPMOD,OPTION,IMATE,EPSM ,
     &               DEPS,VIM,TAMPON,SIGP,VIP,
     &               DSIDEP)          
C               
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF

      END
