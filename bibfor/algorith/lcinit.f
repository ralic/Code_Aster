        SUBROUTINE LCINIT ( FAMI,KPG,KSP,LOI,TYPESS, ESSAI,MOD,
     1                      NMAT,MATERF,TIMED,TIMEF,
     2                      NR, NVI, YD,     EPSD,   DEPS, DY,
     3                      COMP,NBCOMM, CPMONO, PGL,TOUTMS,
     4                      VIND,SIGD, EPSTR)
        IMPLICIT   NONE
C       ================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 23/05/2011   AUTEUR PROIX J-M.PROIX 
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
C       ROUTINE AIGUILLAGE
C       ----------------------------------------------------------------
C       CALCUL DE LA SOLUTION INITIALE ESSAI DY = ( DSIG DVIN (DEPS3) )
C       IN  FAMI   :  FAMILLE DE POINT DE GAUSS
C           KPG    :  NUMERO DU POINT DE GAUSS
C           KSP    :  NUMERO DU SOUS-POINT DE GAUSS
C           LOI    :  MODELE DE COMPORTEMENT
C           TYPESS :  TYPE DE SOLUTION D ESSAI POUR DY(DEPEND DU MODELE)
C                      > VOIR XXXCVG ET XXXINI
C           ESSAI  :  SOLUTION D ESSAI
C           MOD    :  TYPE DE MODELISATION
C           NMAT   :  DIMENSION MATER
C           MATERF :  COEFFICIENTS MATERIAU A T+DT
C           TIMED  :  INSTANT  T
C           TIMEF  :  INSTANT  T+DT
C           EPSD   :  DEFORMATION A T
C           YD     :  VARIABLES A T   = ( SIG  VIN  (EPS3)  )
C       VAR DEPS   :  INCREMENT DE DEFORMATION
C       OUT DY     :  SOLUTION ESSAI  = ( DSIG DVIN (DEPS3) )
C       ----------------------------------------------------------------
C TOLE CRP_21
        INTEGER         TYPESS ,  NMAT, NR,NVI, KPG, KSP
        INTEGER         NBCOMM(NMAT,3)
        REAL*8          DEPS(6), EPSD(6), ESSAI
        REAL*8          YD(*) ,  DY(*)
        REAL*8          MATERF(NMAT,2)
        REAL*8          TIMED, TIMEF
        REAL*8          PGL(3,3)
        REAL*8          VIND(*),SIGD(6),EPSTR(6)
        REAL*8          TOUTMS(5,24,6)
        CHARACTER*(*)   FAMI
        CHARACTER*8     MOD
        CHARACTER*16    LOI
        CHARACTER*16    CPMONO(5*NMAT+1),COMP(*)
C       ----------------------------------------------------------------
C
         
      IF ( LOI(1:9) .EQ. 'VISCOCHAB' ) THEN
         CALL CVMINI(TYPESS,ESSAI,MOD,NMAT,MATERF,
     1                TIMED,TIMEF,YD,EPSD,DEPS,DY)
C
      ELSEIF ( LOI(1:8)  .EQ. 'MONOCRIS' ) THEN
         CALL LCMMIN(TYPESS,ESSAI,MOD,NMAT,MATERF,NR, NVI,YD,DEPS,DY,
     1                      COMP,NBCOMM, CPMONO, PGL,TOUTMS,
     2                     TIMED,TIMEF,VIND,SIGD,EPSTR)
      ELSEIF     ( LOI(1:7) .EQ. 'IRRAD3M' ) THEN
         CALL IRRINI(FAMI,KPG,KSP,TYPESS,ESSAI,MOD,NMAT,MATERF,YD,
     &               DEPS,DY)
      ELSE
C        SOLUTION INITIALE = ZERO
         CALL VECINI ( NR  , 0.D0 , DY )
         IF(MOD(1:6).EQ.'C_PLAN')THEN
            DEPS(3) = 0.D0
         ENDIF
      ENDIF
C
      END
