      SUBROUTINE LCMMJP ( MOD, NMAT, MATER,
     &            TIMED, TIMEF, COMP,NBCOMM, CPMONO, PGL,NR,NVI,
     &                  SIGF,VINF,SIGD,VIND, 
     &                   DSDE )
      IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 16/06/2004   AUTEUR JMBHH01 J.M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE JMBHH01 J.M.PROIX
C     ----------------------------------------------------------------
C     COMPORTEMENT MONOCRISTALLIN
C                :  MATRICE SYMETRIQUE DE COMPORTEMENT TANGENT
C                   COHERENT A T+DT
C     ----------------------------------------------------------------
C     IN  MOD    :  TYPE DE MODELISATION
C         NMAT   :  DIMENSION MATER
C         MATER  :  COEFFICIENTS MATERIAU
C         NR   :  DIMENSION DRDY
C         DRDY   :  MATRICE JACOBIENNE
C         NBCOMM :  NOMBRE DE COEF MATERIAU PAR FAMILLE
C         CPMONO :  NOMS DES LOIS MATERIAU PAR FAMILLE
C           PGL   : MATRICE DE PASSAGE GLOBAL LOCAL
C           NVI     :  NOMBRE DE VARIABLES INTERNES
C           VINF    :  VARIABLES INTERNES A T+DT
C         DRDY  = ( DGDS  DGDX  DGDX1  DGDX2  DGDV  )
C                 ( DLDS  DLDX  DLDX1  DLDX2  DLDV  )
C                 ( DJDS  DJDX  DJDX1  DJDX2  DJDV  )
C                 ( DIDS  DIDX  DIDX1  DIDX2  DIDV  )
C                 ( DKDS  DKDX  DKDX1  DKDX2  DKDV  )
C
C     OUT DSDE   :  MATRICE DE COMPORTEMENT TANGENT = DSIG/DEPS
C      DSDE = INVERSE(Y0-Y1*INVERSE(Y3)*Y2)
C     ----------------------------------------------------------------
      INTEGER         NDT , NDI , NMAT , NVI,I,J,NR, NVV
C DIMENSIONNEMENT DYNAMIQUE
      REAL*8          DRDY(NR,NR),Y0(6*6),Y1(6*(NVI-1)),DSDE(6,6)
      REAL*8          MATER(NMAT*2),Y2((NVI-1)*6),Y1Y2(6,6),DET,I6(6,6)
      REAL*8          Y3((NVI-1)*(NVI-1)),HOOK(6,6),Y2D((NVI-1)*6)
      REAL*8          YD(NR),YF(NR),DY(NR),Y2Y1((NVI-1)*(NVI-1)),UN,ZERO
      CHARACTER*8     MOD
      LOGICAL         IRET
      PARAMETER       ( UN   =  1.D0   )
      PARAMETER       ( ZERO =  0.D0   )
      
      INTEGER         NBCOMM(NMAT,3)
      REAL*8  SIGF(*),SIGD(*),VIND(*),VINF(*),TIMED,TIMEF,PGL(3,3)
      CHARACTER*16    CPMONO(5*NMAT+1),COMP(*)
      COMMON /TDIM/ NDT,NDI
      DATA  I6        /UN     , ZERO  , ZERO  , ZERO  ,ZERO  ,ZERO,
     1                 ZERO   , UN    , ZERO  , ZERO  ,ZERO  ,ZERO,
     2                 ZERO   , ZERO  , UN    , ZERO  ,ZERO  ,ZERO,
     3                 ZERO   , ZERO  , ZERO  , UN    ,ZERO  ,ZERO,
     4                 ZERO   , ZERO  , ZERO  , ZERO  ,UN    ,ZERO,
     5                 ZERO   , ZERO  , ZERO  , ZERO  ,ZERO  ,UN/

C
C - RECUPERER LES SOUS-MATRICES BLOC
C
      
      CALL LCEQVN ( NDT  ,  SIGD , YD )
      CALL LCEQVN ( NDT  ,  SIGF , YF )
      CALL LCEQVN ( NVI-1,  VIND , YD(NDT+1) )
      CALL LCEQVN ( NVI-1,  VINF , YF(NDT+1) )
      CALL LCEQVN ( NR,  YF , DY )
      CALL R8AXPY( NR, -1.D0, YD, 1,DY, 1)
      
C     RECALCUL DE LA DERNIERE MATRICE JACOBIENNE      
      CALL LCMMJA ( MOD, NMAT, MATER, TIMED, TIMEF,
     &              COMP,NBCOMM, CPMONO, PGL,NR,NVI,YF,DY,DRDY)
      NVV=NVI-1
      DET=0.D0
      CALL LCICMA (DRDY,NR,NR,NDT,NDT,1,1 ,Y0 ,6,6,1,1)
      CALL LCICMA (DRDY,NR,NR,NDT,NVV,1,NDT+1,Y1,6,NVV,1,1)
      CALL LCICMA (DRDY,NR,NR,NVV,NDT,NDT+1,1,Y2,NVV,6,1,1)
      CALL LCICMA (DRDY,NR,NR,NVV,NVV,NDT+1,NDT+1,Y3,NVV,NVV,1,1)
      IRET = .TRUE.
      
C       Y2=INVERSE(Y3)*Y2
      CALL MGAUSS ( Y3, Y2, NVV, NVV, 6, DET, IRET )
      IF (.NOT.IRET) CALL UTMESS('F','LCMMJP','Y3 SINGULIERE')
      
C      Y1Y2=Y1*INVERSE(Y3)*Y2
      CALL PROMAT(Y1,6,6,NVV,Y2,NVV,NVV,6,Y1Y2)
      
C      Y0=Y0-Y1*INVERSE(Y3)*Y2
      CALL R8AXPY(36, -1.D0, Y1Y2, 1,Y0, 1)
      CALL LCEQMA ( I6     , DSDE           )
      
C      DSDE = INVERSE(Y0-Y1*INVERSE(Y3)*Y2)
      CALL MGAUSS ( Y0, DSDE, 6, 6, 6, DET, IRET )
      IF (.NOT.IRET) CALL UTMESS('F','LCMMJP','Y0 SINGULIERE')

      END
