      SUBROUTINE LCMMIN ( TYPESS, ESSAI, MOD, NMAT,
     &                      MATERF, NR, NVI, YD, DEPS, DY,
     &                      COMP,NBCOMM, CPMONO, PGL,NFS,NSG,TOUTMS,
     &                      TIMED,TIMEF,VIND,SIGD,EPSTR )
      IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 19/12/2012   AUTEUR PELLET J.PELLET 
C TOLE CRP_21
C RESPONSABLE JMBHH01 J.M.PROIX
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C       ----------------------------------------------------------------
C       MONOCRISTAL : CALCUL SOLUTION INITIALE
C
C       IN  ESSAI  :  VALEUR DE LA SOLUTION D ESSAI
C           MOD    :  TYPE DE MODELISATION
C           NMAT   :  DIMENSION MATER
C           MATERF :  COEFFICIENTS MATERIAU A T+DT
C           NR     :  DIMENSION DECLAREE DRDY
C           NVI    :  NOMBRE DE VARIABLES INTERNES
C           YD     :  VARIABLES A T  
C           DY     :  SOLUTION  A L'ITERATION PRECEDENTE
C           COMP   :  NOM COMPORTEMENT
C           NBCOMM :  INCIDES DES COEF MATERIAU
C           CPMONO :  NOM DES COMPORTEMENTS
C           PGL    :  MATRICE DE PASSAGE
C           TOUTMS :  TENSEURS D'ORIENTATION
C           VIND   :  VARIABLES INTERNES A L'INSTANT PRECEDENT
C           SIGD   :  CONTRAINTE A T
C       VAR DEPS   :  INCREMENT DE DEFORMATION (ACTUALISE EN C_PLAN)
C           TYPESS :  TYPE DE SOLUTION D ESSAI
C                               0 = NUL(0)
C                               1 = ELASTIQUE
C                               2 = EXPLICITE (=-1 INITIALEMENT)
C                               3 = ESSAI
C       OUT DY     :  SOLUTION ESSAI  = ( DSIG DVIN (DEPS3) )
C       ----------------------------------------------------------------
C
      INTEGER         NDT , NDI , TYPESS , NMAT,NR,NVI,TYPES0,NFS,NSG
C
      REAL*8          YD(NR)     , DY(NR),  ESSAI
      REAL*8          HOOK(6,6)
      REAL*8          DEPS(6)
      REAL*8          DSIG(6)
      REAL*8          EPSTR(6),DKOOH(6,6),EPSED(6)
C
      REAL*8          MATERF(NMAT,2)
      REAL*8          TOUTMS(NFS,NSG,6)
C
      CHARACTER*8     MOD
C     ----------------------------------------------------------------
      COMMON /TDIM/   NDT , NDI
C     ----------------------------------------------------------------
      INTEGER         I ,NBFSYS,NBSYS,IS,NBCOMM(NMAT,3),IFA,NUMS
      REAL*8          EVP(6),FE(3,3),DF(3,3),FE1(3,3),FE1T(3,3)
      REAL*8          PGL(3,3),MS(6),NG(3),Q(3,3),LG(3),FETFE(3,3)
      CHARACTER*16    COMP(*)
      CHARACTER*24    CPMONO(5*NMAT+1)
      CHARACTER*16    NOMFAM
      REAL*8          TIMED,TIMEF,VIND(*),SIGD(6),SIGDN(6)
      INTEGER IRR,DECIRR,NBSYST,DECAL,GDEF
      COMMON/POLYCR/IRR,DECIRR,NBSYST,DECAL,GDEF
C
C - SOLUTION INITIALE = NUL
C
      TYPES0=TYPESS

      TYPESS=0
C         TYPESS=7

C     POUR LE CRITERE DE CONVERGENCE CF LCMMCV
      IF (MATERF(NMAT,1).EQ.0) THEN
         CALL LCOPIL  ( 'ISOTROPE' , MOD , MATERF(1,1) , DKOOH )
      ELSEIF (MATERF(NMAT,1).EQ.1) THEN
         CALL LCOPIL  ( 'ORTHOTRO' , MOD , MATERF(1,1) , DKOOH )
      ENDIF
      CALL LCPRMV ( DKOOH,   SIGD  , EPSED )
      CALL LCSOVE ( EPSED,   DEPS  , EPSTR )

      IF ( TYPESS .EQ. 0) THEN
         CALL VECINI ( NR  , 0.D0 , DY )
         IF(MOD(1:6).EQ.'C_PLAN')THEN
            DEPS(3) = 0.D0
         ENDIF
C Les autres intitialisations ci-dessous ne sont pas utilisées
C actuellement pour la loi MONOCRISTAL         
C
C - SOLUTION INITIALE = ELASTIQUE
C
      ELSEIF (TYPESS.EQ.1.OR.TYPESS.EQ.-1) THEN
         IF (MATERF(NMAT,1).EQ.0) THEN
            CALL LCOPLI ( 'ISOTROPE' , MOD , MATERF(1,1) , HOOK )
         ELSEIF (MATERF(NMAT,1).EQ.1) THEN
            CALL LCOPLI ( 'ORTHOTRO' , MOD , MATERF(1,1) , HOOK )
         ENDIF
C        GDEF : INITIALISATION PAR FET.DFT.DF.FE
         IF (GDEF.EQ.1) THEN
            CALL DCOPY(9,VIND(NVI-3-18+10),1,FE,1)
            CALL DCOPY(9,DEPS,1,DF,1)
            CALL PMAT(3,DF,FE,FE1)
            CALL LCTR2M(3,FE1,FE1T)
            CALL PMAT(3,FE1T,FE1,FETFE)
            CALL TNSVEC(3,3,FETFE,DY,1.D0 ) 
         ELSE
            CALL LCTRMA (HOOK , HOOK)
            CALL LCPRMV ( HOOK    , DEPS , DSIG  )
            CALL LCEQVN ( NDT     , DSIG , DY(1) )
         ENDIF
C
C - SOLUTION INITIALE = EXPLICITE
C
C      ELSEIF ( TYPESS .EQ. 2 ) THEN
C
C - SOLUTION INITIALE = VALEUR ESSAI POUR TOUTES LES COMPOSANTES
C
      ELSEIF ( TYPESS .EQ. 3 ) THEN
        CALL VECINI ( NR  , ESSAI , DY )
        IF ( MOD(1:6).EQ.'C_PLAN' )THEN
           DEPS(3) = ESSAI
           DY(3)   = 0.D0
        ENDIF
C
      ELSEIF ( TYPESS .EQ. 7 ) THEN

         NBFSYS=NBCOMM(NMAT,2)
         NUMS=0

      DO 111 IFA=1,NBFSYS

         NOMFAM=CPMONO(5*(IFA-1)+1)
C       RECUPERATION DU NOMBRE DE SYSTEME DE GLISSEMENT NBSYS
         CALL LCMMSG(NOMFAM,NBSYS,0,PGL,MS,NG,LG,0,Q)
         IF (NBSYS.EQ.0) CALL U2MESS('F','ALGORITH_70')

         CALL R8INIR(6,0.D0,EVP,1)

         DO 112 IS=1,NBSYS
            NUMS=NUMS+1
               DY (NDT+6+3*IFA*(IS-1)+3) = VIND(6+3*IFA*(IS-1)+3)
     &                       *(TIMEF-TIMED)/TIMEF
               DY (NDT+6+3*IFA*(IS-1)+2) =
     &        ABS(VIND(6+3*IFA*(IS-1)+2))*(TIMEF-TIMED)/TIMEF
               DY (NDT+6+3*IFA*(IS-1)+1) = VIND(6+3*IFA*(IS-1)+1)
     &                       *(TIMEF-TIMED)/TIMEF
C           RECUPERATION DE MS ET CALCUL DE EVP
              CALL LCMMSG(NOMFAM,NBSYS,IS,PGL,MS,NG,LG,0,Q)
              DO 110 I = 1,6
                 EVP(I) = EVP(I) + MS(I)*DY (NDT+6+3*IFA*(IS-1)+2)
  110           CONTINUE
  112       CONTINUE
  111     CONTINUE
C      ATTRIBUTIION A DY LA VALEUR DE EVP CALCULEE
         CALL LCEQVN ( 6    , EVP , DY(NDT+1))
C
         DO 113 I=1,6
         SIGDN(I) = SIGD(I)*(TIMEF-TIMED)/TIMEF
  113    CONTINUE
         CALL LCEQVN ( NDT     , SIGDN  , DY(1) )


        IF ( MOD(1:6).EQ.'C_PLAN' )THEN
           DY(3)   = 0.D0
        ENDIF

      ENDIF
C
      TYPESS=TYPES0
      END
