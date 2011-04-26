        SUBROUTINE LCDEHY ( FAMI, KPG, KSP, NMAT, MATERD, MATERF,
     &                      DEPSM, EPSDM )
        IMPLICIT NONE
C       ----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C       ----------------------------------------------------------------
C       RETRAIT DE DEFORMATION DUE AU RETRAIT ENDOGENE ET AU RETRAIT
C       DE DESSICCATION
C       POUR TENIR COMPTE DES CONTRAINTES DE RETRAIT :
C
C       ON RETIRE       - A DEPSM, L INCREMENT DE DEFORMATION DUE
C                         AU RETRAIT
C                       - ET A EPSDM , LE RETRAIT A T
C
C       POUR OBTENIR    - L'INCREMENT DE DEFORMATION MECANIQUE DEPSM
C                       - ET LA DEFORMATION MECANIQUE A T      EPSDM
C
C       ON A SIG = HOOK EPSE  = HOOK ( EPST - EPSP - EPSRET )
C                             = HOOK ( EPST - EPSP ) - HOOK EPSRET
C       DONC            SIG   = SIGM                 + SIGRET
C       AVEC            SIGRET= + HOOK (KAPPA*(SREF-SECH) + BETA*HYDR) I
C                       DE SIGNE OPPOSE A LA DEFORMATION THERMIQUE
C       OU   EN PRENANT EPS   = EPST - EPSRET
C                       SIG   = HOOK ( EPS - EPSP )
C
C       ON PEUT DONC - SOIT TRAVAILLER AVEC EPST ET AJOUTER SIGRET APRES
C                    - SOIT TRAVAILLER AVEC EPS = EPST - EPSRET
C                      CE QUI EST FAIT ICI
C       ----------------------------------------------------------------
C       IN      NMAT    DIMENSION  DE MATER
C               MATERD  COEFFICIENTS MATERIAU A T
C               MATERF  COEFFICIENTS MATERIAU A T+DT
C       VAR     DEPSM   INCREMENT DE DEFORMATION MECANIQUE
C               EPSDM   DEFORMATION MECANIQUE A T
C       ----------------------------------------------------------------
        INTEGER         NDT  , NDI , NMAT , K, IRET, KPG, KSP
        CHARACTER*(*)   FAMI
        REAL*8          HD, HF, SD, SF, SREF
        REAL*8          EPSDM(6), DEPSM(6)
        REAL*8          BENDOD, BENDOF, KDESSD, KDESSF
        REAL*8          MATERD(NMAT,2) ,MATERF(NMAT,2)
C       ----------------------------------------------------------------
        COMMON /TDIM/   NDT  , NDI
C       ----------------------------------------------------------------
C
        BENDOD = MATERD(4,1)
        BENDOF = MATERF(4,1)
        KDESSD = MATERD(5,1)
        KDESSF = MATERF(5,1)
        CALL RCVARC(' ','HYDR','-',FAMI,KPG,KSP,HD,IRET)
        IF ( IRET.NE.0) HD=0.D0
        CALL RCVARC(' ','HYDR','+',FAMI,KPG,KSP,HF,IRET)
        IF ( IRET.NE.0) HF=0.D0
        CALL RCVARC(' ','SECH','-',FAMI,KPG,KSP,SD,IRET)
        IF ( IRET.NE.0) SD=0.D0
        CALL RCVARC(' ','SECH','+',FAMI,KPG,KSP,SF,IRET)
        IF ( IRET.NE.0) SF=0.D0
        CALL RCVARC(' ','SECH','REF',FAMI,KPG,KSP,SREF,IRET)
        IF ( IRET.NE.0) SREF=0.D0
C
        DO 110 K = 1,NDI
            DEPSM(K) = DEPSM(K) + ( BENDOF*HF - BENDOD*HD)
     &                         + ( KDESSF*(SREF-SF) - KDESSD*(SREF-SD))
            EPSDM(K) = EPSDM(K) +  BENDOD*HD + KDESSD*(SREF-SD)
 110    CONTINUE
C
        END
