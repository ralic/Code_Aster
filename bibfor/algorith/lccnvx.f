        SUBROUTINE LCCNVX ( FAMI, KPG, KSP, LOI, IMAT, NMAT, MATERD,
     &                      MATERF,SIGD,SIGF,DEPS,VIND, NBCOMM,CPMONO, 
     &                      PGL,NVI,VP,VECP,HSR,NFS,NSG,TOUTMS,
     &                      TIMED,TIMEF,NR,YD,YF,TOLER,SEUIL)
        IMPLICIT  NONE
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/12/2011   AUTEUR FOUCAULT A.FOUCAULT 
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
C TOLE CRP_21
C ----------------------------------------------------------------------
C --- BUT : CONVEXE ELASTO PLASTIQUE A T+DT POUR (SIGF , VIND) DONNES --
C ----------------------------------------------------------------------
C IN  : FAMI   :  FAMILLE DES POINTS DE GAUSS  -------------------------
C --- : KPG    :  NUMERO DU POINT DE GAUSS  ----------------------------
C --- : KSP    :  NUMERO DU SOUS POINT DE GAUSS ------------------------
C --- : LOI    :  NOM DU MODELE DE COMPORTEMENT ------------------------
C --- : SIGD   :  CONTRAINTE A T ---------------------------------------
C --- : SIGF   :  CONTRAINTE A T+DT ------------------------------------
C --- : DEPS   :  INCRMEENT DE DEFORMATION -----------------------------
C --- : VIND   :  VARIABLES INTERNES A T -------------------------------
C --- : IMAT   :  ADRESSE DU MATERIAU CODE -----------------------------
C --- : NMAT   :  DIMENSION MATER --------------------------------------
C --- : TOLER  :  TOLERANCE DE CONVERGENCE LOCALE-----------------------
C --- : MATERD :  COEFFICIENTS MATERIAU A T ----------------------------
C --- : MATERF :  COEFFICIENTS MATERIAU A T+DT -------------------------
C --- : NBCOMM :  INDICES DES COEF MATERIAU ----------------------------
C --- : TYPMOD :  TYPE DE MODELISATION ---------------------------------
C --- : TIMED  :  INSTANT T --------------------------------------------
C --- : TIMEF  :  INSTANT T+DT -----------------------------------------
C --- : NR     :  DIMENSION VECTEUR INCONNUES --------------------------
C OUT : VP     :  VALEURS PROPRES DU DEVIATEUR ELASTIQUE (HOEK-BROWN) --
C --- : VECP   :  VECTEURS PROPRES DU DEVIATEUR ELASTIQUE (HOEK-BROWN) -
C --- : SEUIL  :  SEUIL  ELASTICITE  A T+DT ----------------------------
C --- : YD     :  VECTEUR INCONNUES A T --------------------------------
C --- : YF     :  VECTEUR INCONNUES A T+DT -----------------------------
C ----------------------------------------------------------------------
C ======================================================================
        INTEGER         NMAT , IMAT, NVI, KPG, KSP,NFS,NSG,NR
        CHARACTER*(*)   FAMI
        REAL*8          MATERF(NMAT,2),MATERD(NMAT,2),SEUIL
        REAL*8          TIMED,TIMEF,TOLER,DEPS(6)
        REAL*8          SIGD(6),SIGF(6),VIND(*),HSR(NSG,NSG)
        CHARACTER*16    LOI
        INTEGER         NBCOMM(NMAT,3)
        REAL*8          PGL(3,3),VP(3),VECP(3,3),TOUTMS(NFS,NSG,6)
        REAL*8          YD(NR),YF(NR)
        CHARACTER*16    CPMONO(5*NMAT+1)
        CHARACTER*8     TYPMOD
C ======================================================================
      IF ( LOI(1:8) .EQ. 'ROUSS_PR'  )THEN
         CALL RSLCVX ( FAMI, KPG, KSP, IMAT, NMAT, MATERF,
     &                 SIGF, VIND, SEUIL )
C ======================================================================
      ELSEIF ( LOI(1:10) .EQ. 'ROUSS_VISC' ) THEN
         CALL RSLCVX ( FAMI, KPG, KSP, IMAT, NMAT, MATERF,
     &                 SIGF, VIND, SEUIL )
C ======================================================================
      ELSEIF ( LOI(1:9) .EQ. 'VISCOCHAB') THEN
         CALL CVMCVX ( NMAT, MATERF, SIGF, VIND, SEUIL )
C ======================================================================
      ELSEIF ( LOI(1:6)  .EQ. 'LAIGLE') THEN
         CALL LGLCVX ( SIGF, VIND, NMAT, MATERF, SEUIL)
C ======================================================================
      ELSEIF (( LOI(1:10)  .EQ. 'HOEK_BROWN').OR.
     &        ( LOI(1:14)  .EQ. 'HOEK_BROWN_EFF')) THEN
         CALL HBRCVX ( SIGF, VIND, NMAT, MATERF, SEUIL, VP, VECP)
C ======================================================================
      ELSEIF ( LOI(1:8)  .EQ. 'MONOCRIS') THEN
         CALL LCMMVX (SIGF, VIND, NMAT, MATERF, NBCOMM, CPMONO, PGL,
     &                NVI,HSR, NFS,NSG,TOUTMS,TIMED,TIMEF,SEUIL)
C ======================================================================
      ELSEIF ( LOI(1:7)  .EQ. 'IRRAD3M') THEN
         CALL IRRCVX ( FAMI, KPG, KSP, NMAT, MATERF, SIGF, VIND, SEUIL)
C ======================================================================
C ======================================================================
      ELSEIF ( LOI(1:15)  .EQ. 'BETON_BURGER_FP') THEN
C --- LE FLUAGE EST CONSIDERE POUR TOUT TYPE DE SOLLICITATION MECANIQUE
         CALL BURCVX(TYPMOD,NMAT,MATERD,MATERF,TIMED,TIMEF,
     &               NVI,VIND,NR,SIGD,DEPS,YD,YF,TOLER,SEUIL)
C ======================================================================
      ENDIF
C ======================================================================
      END
