      SUBROUTINE LC0015(FAMI,KPG,KSP,NDIM,IMATE,COMPOR,CRIT,INSTAM,
     &                  INSTAP,EPSM,DEPS,SIGM,VIM,OPTION,SIGP,VIP,
     &                  TAMPON,TYPMOD,DSIDEP,CODRET)
      IMPLICIT NONE
      INTEGER         IMATE,NDIM,KPG,KSP,CODRET
      REAL*8          CRIT(*)
      REAL*8          INSTAM,INSTAP,TAMPON(*)
      REAL*8          EPSM(6),DEPS(6)
      REAL*8          SIGM(6),SIGP(6)
      REAL*8          VIM(*),VIP(*)
      REAL*8          DSIDEP(6,6)
      CHARACTER*16    COMPOR(*),OPTION
      CHARACTER*8     TYPMOD(*)
      CHARACTER*(*)   FAMI
C
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 30/06/2008   AUTEUR PROIX J-M.PROIX 
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
C     BUT: LOI DE COMPORTEMENT EN METALLURGIE
C
C          RELATIONS : 
C
C                 META_P_IL
C                 META_P_IL_PT
C                 META_P_IL_RE
C                 META_P_IL_PT_RE
C                 META_V_IL
C                 META_V_IL_PT
C                 META_V_IL_RE
C                 META_V_IL_PT_RE
C                 META_P_INL
C                 META_P_INL_PT
C                 META_P_INL_RE
C                 META_P_INL_PT_RE
C                 META_V_INL
C                 META_V_INL_PT
C                 META_V_INL_RE
C                 META_V_INL_PT_RE
C                 META_P_CL
C                 META_P_CL_PT
C                 META_P_CL_RE
C                 META_P_CL_PT_RE
C                 META_V_CL
C                 META_V_CL_PT
C                 META_V_CL_RE
C                 META_V_CL_PT_RE
C                 META_LEMA_ANI
C
C

C
C       IN      FAMI    FAMILLE DE POINT DE GAUSS (RIGI,MASS,...)
C       IN      KPG,KSP NUMERO DU (SOUS)POINT DE GAUSS
C       IN      NDIM    DIMENSION DE L ESPACE (3D=3,2D=2,1D=1)
C               TYPMOD  TYPE DE MODELISATION
C               IMATE    ADRESSE DU MATERIAU CODE
C               COMPOR    COMPORTEMENT DE L ELEMENT
C                     COMPOR(1) = RELATION DE COMPORTEMENT (CHABOCHE...)
C                     COMPOR(2) = NB DE VARIABLES INTERNES
C                     COMPOR(3) = TYPE DE DEFORMATION (PETIT,JAUMANN...)
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
C               INSTAM   INSTANT T
C               INSTAP   INSTANT T+DT
C               EPSM   DEFORMATION TOTALE A T
C               DEPS   INCREMENT DE DEFORMATION TOTALE
C               SIGM    CONTRAINTE A T
C               VIM    VARIABLES INTERNES A T    + INDICATEUR ETAT T
C    ATTENTION  VIM    VARIABLES INTERNES A T MODIFIEES SI REDECOUPAGE
C               OPTION     OPTION DE CALCUL A FAIRE
C                             'RIGI_MECA_TANG'> DSIDEP(T)
C                             'FULL_MECA'     > DSIDEP(T+DT) , SIG(T+DT)
C                             'RAPH_MECA'     > SIG(T+DT)
C               TAMPON  TABLEAUX DES ELEMENTS GEOMETRIQUES SPECIFIQUES
C                       AUX LOIS DE COMPORTEMENT (DIMENSION MAXIMALE
C                       FIXEE EN DUR)
C               ANGMAS  ANGLES DU REPERE DU MATERIAU (AFFE_CARA_ELEM)
C       OUT     SIGP    CONTRAINTE A T+DT
C               VIP    VARIABLES INTERNES A T+DT + INDICATEUR ETAT T+DT
C               DSIDEP    MATRICE DE COMPORTEMENT TANGENT A T+DT OU T
C.......................................................................
C               CODRET  
C


      CHARACTER*16 MCMATE
      CHARACTER*2  K2BID
      REAL*8       R8BID
C
C     NORMALEMENT, LES VERIF ONT ETE FAITES AVANT POUR INTERDIRE
C     SIMO_MIEHE AVEC ECROUISSAGE CINEMATIQUE
C     META_LEMA_ANI AVEC ACIER

C     DEFORMATIONS DE SIMO_MIEHE
      IF (COMPOR(3).EQ.'SIMO_MIEHE') THEN

        IF (COMPOR(8).EQ.'ACIER') THEN

          CALL LCGDPM(FAMI,KPG,KSP,NDIM,IMATE,COMPOR,CRIT,INSTAM,INSTAP,
     &                EPSM,DEPS,SIGM,VIM,OPTION,SIGP,VIP,DSIDEP,CODRET)

        ELSEIF (COMPOR(8).EQ.'ZIRC') THEN
        
          CALL NZGDZI(FAMI,KPG,KSP,NDIM,IMATE,COMPOR,CRIT,INSTAM,INSTAP,
     &                EPSM,DEPS,SIGM,VIM,OPTION,SIGP,VIP,DSIDEP,CODRET)

        ENDIF
        CALL POSTSM(OPTION,EPSM,DEPS,SIGM,SIGP,DSIDEP)

C     PETITES DEFORMATIONS
      ELSE

C       ECROUISSAGE ISOTROPE
        IF (COMPOR(1)(8:8).EQ.'I') THEN

          IF (COMPOR(8).EQ.'ACIER') THEN

            CALL NZISFW(FAMI,KPG,KSP,NDIM,IMATE,COMPOR,CRIT,INSTAM,
     &                  INSTAP,EPSM,DEPS,SIGM,VIM,OPTION,SIGP,VIP,
     &                  DSIDEP,CODRET)

          ELSEIF (COMPOR(8).EQ.'ZIRC') THEN

            CALL NZEDGA(FAMI,KPG,KSP,NDIM,IMATE,COMPOR,CRIT,INSTAM,
     &                  INSTAP,EPSM,DEPS,SIGM,VIM,OPTION,SIGP,VIP,
     &                  DSIDEP,CODRET)

          ENDIF

C       ECROUISSAGE CINEMATIQUE
        ELSEIF (COMPOR(1)(8:8).EQ.'C') THEN

          IF (COMPOR(8).EQ.'ACIER') THEN

            CALL NZCIFW(FAMI,KPG,KSP,NDIM,IMATE,COMPOR,CRIT,INSTAM,
     &                  INSTAP,EPSM,DEPS,SIGM,VIM,OPTION,SIGP,VIP,
     &                  DSIDEP,CODRET)

          ELSEIF (COMPOR(8).EQ.'ZIRC') THEN

            CALL NZCIZI(FAMI,KPG,KSP,NDIM,IMATE,COMPOR,CRIT,INSTAM,
     &                  INSTAP,EPSM,DEPS,SIGM,VIM,OPTION,SIGP,VIP,
     &                  DSIDEP,CODRET)

          ENDIF

C       ECROUISSAGE ANISOTROPE
        ELSEIF (COMPOR(1).EQ.'META_LEMA_ANI') THEN

          CALL LCEDGA(FAMI,KPG,KSP,NDIM,IMATE,COMPOR,CRIT,TYPMOD,INSTAM,
     &                INSTAP,TAMPON,EPSM,DEPS,SIGM,VIM,OPTION,SIGP,VIP,
     &                DSIDEP,CODRET)

        ENDIF

      ENDIF
 
      END
