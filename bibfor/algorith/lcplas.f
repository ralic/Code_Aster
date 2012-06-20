      SUBROUTINE LCPLAS ( FAMI,KPG,KSP,LOI,TOLER, ITMAX, MOD,IMAT,
     &                    NMAT, MATERD, MATERF,NR, NVI,
     &                    TIMED, TIMEF,DEPS,EPSD, SIGD,VIND,
     &                    SIGF, VINF, 
     &                    COMP,NBCOMM, CPMONO, PGL,NFS,NSG,TOUTMS,HSR,
     &                    ICOMP,CODRET,THETA,
     &                    VP,VECP,SEUIL, DEVG, DEVGII,
     &                    DRDY,TAMPON,CRIT)
      IMPLICIT   NONE
C     ================================================================
C          CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 18/06/2012   AUTEUR PROIX J-M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C TOLE CRP_21
C     ----------------------------------------------------------------
C     INTEGRATION IMPLICITE DES COMPORTEMENTS. CALCUL DE SIGF,VINF,DSDE
C     ----------------------------------------------------------------
C     ARGUMENTS
C
C     IN FAMI    FAMILLE DE POINT DE GAUSS (RIGI,MASS,...)
C        KPG,KSP NUMERO DU (SOUS)POINT DE GAUSS
C        LOI    :  MODELE DE COMPORTEMENT
C        TOLER  :  TOLERANCE DE CONVERGENCE LOCALE
C        ITMAX  :  NOMBRE MAXI D'ITERATIONS LOCALES
C        MOD    :  TYPE DE MODELISATION
C        IMAT   :  ADRESSE DU MATERIAU CODE
C        NMAT   :  DIMENSION MATER
C        MATERD :  COEFFICIENTS MATERIAU A T
C        MATERF :  COEFFICIENTS MATERIAU A T+DT
C        NR     :  NB EQUATION DU SYSTEME R(DY)
C        NVI    :  NB VARIABLES INTERNES
C        TIMED  :  INSTANT  T
C        TIMEF  :  INSTANT T+DT
C        DEPS   :  INCREMENT DE DEFORMATION
C        EPSD   :  DEFORMATION A T
C        SIGD   :  CONTRAINTE A T
C        VIND   :  VARIABLES INTERNES A T
C        COMP   :  COMPOR - LOI ET TYPE DE DEFORMATION
C        NBCOMM :  INCIDES DES COEF MATERIAU monocristal
C        CPMONO :  NOM DES COMPORTEMENTS monocristal
C        PGL    :  MATRICE DE PASSAGE
C        TOUTMS :  TENSEURS D'ORIENTATION monocristal
C        HSR    :  MATRICE D'INTERACTION monocristal
C        ICOMP  :  COMPTEUR POUR LE REDECOUPAGE DU PAS DE TEMPS
C        THETA  :  PARAMETRE DE LA THETA-METHODE
C        VP     :  VALEURS PROPRES DU DEVIATEUR ELASTIQUE(HOEK-BROWN)
C        VECP   :  VECTEURS PROPRES DU DEVIATEUR ELASTIQUE(HOEK-BROWN)
C        TAMPON : TABLEAU DE TRAVAIL EN ENTREE(SUIVANT MODELISATION)
C        CRIT   : CRITERES DE CONVERGENCE LOCAUX 
C        
C    OUT SIGF   :  CONTRAINTE A T+DT
C        VINF   :  VARIABLES INTERNES A T+DT
C        CODRET :  CODE RETOUR. 0=OK, 1=ECHEC
C        DRDY   :  MATRICE JACOBIENNE
C       ----------------------------------------------------------------
        INTEGER         ITMAX, ICOMP, CODRET, IRTET, KPG,KSP
        INTEGER         IMAT, NMAT,   NVI,    NR

C
        REAL*8          TIMED, TIMEF,DELTAT, CRIT(*)
        REAL*8          TOLER,          THETA
        REAL*8          EPSD(6) ,       DEPS(6)
        REAL*8          SIGD(6),        SIGF(6)
        REAL*8          VIND(*),        VINF(*), TAMPON(*)
        REAL*8          MATERF(NMAT,2), MATERD(NMAT,2)
        REAL*8          SEUIL, DEVG(*), DEVGII
        REAL*8          VP(3),VECP(3,3) , DRDY(NR,NR)
C
        CHARACTER*8     MOD
        CHARACTER*16    LOI

        INTEGER         NBCOMM(NMAT,3),NFS,NSG
        REAL*8          PGL(3,3)
        REAL*8          TOUTMS(NFS,NSG,6),HSR(NSG,NSG)
        CHARACTER*16    CPMONO(5*NMAT+1),COMP(*)
        CHARACTER*(*)   FAMI
C       ----------------------------------------------------------------
C
      CODRET = 0
      DELTAT = TIMEF - TIMED
      
C       ----------------------------------------------------------------
C       CAS PARTICULIERS
C       ----------------------------------------------------------------

      IF     ( LOI(1:8).EQ. 'ROUSS_PR' .OR.
     1         LOI(1:10) .EQ. 'ROUSS_VISC'    ) THEN
         CALL LCROUS (FAMI,KPG,KSP, TOLER, ITMAX, IMAT, NMAT,MATERD,
     1                MATERF, NVI,   DEPS, SIGD, VIND, THETA,
     2                LOI, DELTAT,  SIGF, VINF, IRTET )
         IF ( IRTET.GT.0 ) GOTO 1
C
      ELSEIF (( LOI(1:10) .EQ. 'HOEK_BROWN'       ).OR.
     1         ( LOI(1:14) .EQ. 'HOEK_BROWN_EFF'   ))THEN
         CALL LCHOBR ( TOLER, ITMAX, MOD, NMAT, MATERF, NR, NVI,
     1                 DEPS, SIGD, VIND, SEUIL, VP,VECP,ICOMP, SIGF,
     2                 VINF, IRTET)
         IF ( IRTET.GT.0 ) GOTO 1
C
      ELSEIF ( LOI(1:6) .EQ. 'LAIGLE'   ) THEN
         CALL LCPLLG ( TOLER, ITMAX, MOD, NMAT, MATERF, NR, NVI,
     1                 DEPS, SIGD, VIND, SEUIL, ICOMP, SIGF,
     2                 VINF, DEVG, DEVGII, IRTET)
         IF ( IRTET.GT.0 ) GOTO 1
C
C       ----------------------------------------------------------------
C       CAS GENERAL : RESOLUTION PAR NEWTON
C       ----------------------------------------------------------------
      ELSE
         CALL LCPLNL ( FAMI, KPG, KSP, LOI,  TOLER, ITMAX, MOD,
     1                 IMAT,NMAT, MATERD,MATERF,NR, NVI,
     2                 TIMED,TIMEF,DEPS,EPSD,SIGD,VIND,COMP,
     3                 NBCOMM, CPMONO,PGL,NFS,NSG,TOUTMS,HSR,
     4                 SIGF, VINF, ICOMP, IRTET,DRDY,TAMPON,CRIT)
         IF ( IRTET.GT.0 ) GOTO (1,2), IRTET

      ENDIF
      
C     CONVERGENCE OK

      CODRET = 0
      GOTO 9999
C
 1    CONTINUE
 
C     PB INTEGRATION ou ITMAX ATTEINT : redecoupage local puis global
      CODRET = 1
      GOTO 9999
C
2     CONTINUE
C     ITMAX ATTEINT : redecoupage du pas de temps global
      CODRET = 2
C
 9999 CONTINUE
      END
