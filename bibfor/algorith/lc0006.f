      SUBROUTINE LC0006(FAMI,KPG,KSP,NDIM,IMATE,COMPOR,CRIT,INSTAM,
     &              INSTAP,EPSM,DEPS,SIGM,VIM,OPTION,ANGMAS,SIGP,VIP,
     &                  TAMPON,TYPMOD,ICOMP,NVI,DSIDEP,CODRET)
      IMPLICIT NONE
      INTEGER         IMATE,NDIM,KPG,KSP
      INTEGER            ICOMP,NVI
        INTEGER         CODRET
      REAL*8             ANGMAS(*),TAMPON(*)
      REAL*8          CRIT(*),SIGM(*)
      REAL*8          INSTAM,INSTAP
      REAL*8          EPSM(6),  DEPS(6)
      REAL*8          SIGP(6)
      REAL*8          VIM(*),   VIP(*)
      REAL*8          DSIDEP(6,6)
      CHARACTER*16    COMPOR(*),     OPTION
      CHARACTER*8     TYPMOD(2)
      CHARACTER*(*)   FAMI

C
C TOLE CRP_21
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 07/07/2009   AUTEUR PROIX J-M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE PROIX J-M.PROIX
C ======================================================================
C.......................................................................
C
C     BUT: LOI D'ENDOMMAGEMENT ASYMETRIQUE DES BETONS,
C          AVEC EFFET DE RESTAURATION DE RIGIDITE
C
C          RELATION : 'ENDO_ISOT_BETON'
C
C       IN      FAMI    FAMILLE DE POINT DE GAUSS (RIGI,MASS,...)
C       IN      KPG,KSP NUMERO DU (SOUS)POINT DE GAUSS
C       IN      NDIM    DIMENSION DE L ESPACE (3D=3,2D=2,1D=1)
C               TYPMOD  TYPE DE MODELISATION
C               IMATE    ADRESSE DU MATERIAU CODE
C               COMP    COMPORTEMENT DE L ELEMENT
C                       COMP(1) = RELATION DE COMPORTEMENT (CHABOCHE...)
C                       COMP(2) = NB DE VARIABLES INTERNES
C                       COMP(3) = TYPE DE DEFORMATION (PETIT,JAUMANN...)
C               CRIT    CRITERES  LOCAUX
C                       CRIT(1) = NOMBRE D ITERATIONS MAXI A CONVERGENCE
C                                 (ITER_INTE_MAXI == ITECREL)
C                       CRIT(2) = TYPE DE JACOBIEN A T+DT
C                                 (TYPE_MATR_COMP == MACOMP)
C                                 0 = EN VITESSE     > SYMETRIQUE
C                                 1 = EN INCREMENTAL > NON-SYMETRIQUE
C                       CRIT(3) = VALEUR DE LA TOLERANCE DE CONVERGENCE
C                                 (RESI_INTE_RELA == RESCREL)
C                       CRIT(5) = NOMBRE D'INCREMENTS POUR LE
C                                 REDECOUPAGE LOCAL DU PAS DE TEMPS
C                                 (ITER_INTE_PAS == ITEDEC)
C                                 0 = PAS DE REDECOUPAGE
C                                 N = NOMBRE DE PALIERS
C               EPSM   DEFORMATION TOTALE A T
C               DEPS   INCREMENT DE DEFORMATION TOTALE
C               VIM    VARIABLES INTERNES A T    + INDICATEUR ETAT T
C    ATTENTION  VIM    VARIABLES INTERNES A T MODIFIEES SI REDECOUPAGE
C               OPTION     OPTION DE CALCUL A FAIRE
C                             'RIGI_MECA_TANG'> DSIDEP(T)
C                             'FULL_MECA'     > DSIDEP(T+DT) , SIG(T+DT)
C                             'RAPH_MECA'     > SIG(T+DT)
C               TAMPON  TABLEAUX DES ELEMENTS GEOMETRIQUES SPECIFIQUES
C                       AUX LOIS DE COMPORTEMENT (DIMENSION MAXIMALE
C                       FIXEE EN DUR)
C       OUT     SIGP    CONTRAINTE A T+DT
C               VIP    VARIABLES INTERNES A T+DT + INDICATEUR ETAT T+DT
C               DSIDEP    MATRICE DE COMPORTEMENT TANGENT A T+DT OU T
C
      REAL*8    TP,TM,TREF
      INTEGER   IRET

C
C     NORMALEMENT, LES VERIF ONT ETE FAITES AVANT POUR INTERDIRE
C     SIMO_MIEHE
C     EXPLICITE


C     FORMULATION NON-LOCALE A GRADIENT D'ENDOMMAGEMENT
      IF (TYPMOD(2).EQ.'GRADVARI') THEN

        CALL LCEIGV (FAMI,KPG,KSP,NDIM,TYPMOD,IMATE,COMPOR,
     &               EPSM,DEPS,VIM,TAMPON,
     &               OPTION,SIGP,VIP,DSIDEP)

C     FORMULATION NON-LOCALE AVEC REGULARISATION DES DEFORMATIONS
      ELSEIF (TYPMOD(2).EQ.'GRADEPSI') THEN

        CALL LCDSBE(FAMI,NDIM,TYPMOD,IMATE,COMPOR,EPSM,DEPS,
     &              VIM,OPTION,SIGP,VIP,DSIDEP,TAMPON)
C     FORMULATION LOCALE
      ELSE

        CALL RCVARC(' ','TEMP','-'  ,FAMI,KPG,KSP,TM  ,IRET)
        CALL RCVARC(' ','TEMP','+'  ,FAMI,KPG,KSP,TP  ,IRET)
        CALL RCVARC(' ','TEMP','REF',FAMI,KPG,KSP,TREF,IRET)

        CALL LCLDSB(FAMI,KPG,KSP,NDIM, TYPMOD,IMATE,COMPOR,EPSM,
     &              DEPS,VIM,TM,TP,TREF,OPTION,SIGP,VIP,DSIDEP,CRIT)

      ENDIF

      END
