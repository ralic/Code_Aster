      SUBROUTINE LCMMJP ( MOD, NMAT, MATER,TEMPF,
     &       TIMED, TIMEF, COMP,NBCOMM, CPMONO, PGL,TOUTMS,HSR,NR,NVI,
     &                  SIGF,VINF,SIGD,VIND,
     &                   DSDE )
      IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 05/09/2006   AUTEUR JOUMANA J.EL-GHARIB 
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
      INTEGER         NDT , NDI , NMAT , NVI
      INTEGER         K,J,NR, NVV, IRET,NS
C DIMENSIONNEMENT DYNAMIQUE
      REAL*8          DRDY(NR,NR),Y0(6,6),Y1(6,(NVI-8)),DSDE(6,6)
      REAL*8          MATER(NMAT*2),Y2((NVI-8),6),KYL(6,6),DET,I6(6,6)
      REAL*8          Y3((NVI-8),(NVI-8))
      REAL*8          YD(NR),YF(NR),DY(NR),UN,ZERO,TEMPF
      REAL*8 Z0(6,6),Z1(6,(NR-NDT)),Z2((NR-NDT),6),Z3((NR-NDT),(NR-NDT))
        REAL*8 TOUTMS(5,24,6),HSR(5,24,24),MS(6)
      CHARACTER*8     MOD
      PARAMETER       ( UN   =  1.D0   )
      PARAMETER       ( ZERO =  0.D0   )

      INTEGER         NBCOMM(NMAT,3),MONO1
      INTEGER         NBFSYS,NBSYS,IS,NSFA,NUVRF, NUVR,IFA
      REAL*8  SIGF(*),SIGD(*),VIND(*),VINF(*),TIMED,TIMEF,PGL(3,3)
      CHARACTER*16    CPMONO(5*NMAT+1),COMP(*), NOMFAM
      COMMON /TDIM/ NDT,NDI
      DATA  I6        /UN     , ZERO  , ZERO  , ZERO  ,ZERO  ,ZERO,
     1                 ZERO   , UN    , ZERO  , ZERO  ,ZERO  ,ZERO,
     2                 ZERO   , ZERO  , UN    , ZERO  ,ZERO  ,ZERO,
     3                 ZERO   , ZERO  , ZERO  , UN    ,ZERO  ,ZERO,
     4                 ZERO   , ZERO  , ZERO  , ZERO  ,UN    ,ZERO,
     5                 ZERO   , ZERO  , ZERO  , ZERO  ,ZERO  ,UN/

C -  INITIALISATION

      NSFA = 6
      NUVRF = 6
      NBFSYS = NBCOMM(NMAT,2)
      NS = 0

C - RECUPERER LES SOUS-MATRICES BLOC

      CALL LCEQVN ( NDT  ,  SIGD , YD )
      CALL LCEQVN ( NDT  ,  SIGF , YF )

      IF (NBCOMM(NMAT,1).EQ.1) THEN
      
      DO 99 IFA = 1, NBFSYS
         NOMFAM = CPMONO(5*(IFA-1)+1)
         CALL LCMMSG(NOMFAM,NBSYS,0,PGL,MS)
         NUVR = NUVRF
          DO 98 IS = 1, NBSYS
            YD(NSFA+IS)=VIND(NUVR+2)
            YF(NSFA+IS)=VINF(NUVR+2)
            DY(NSFA+IS)=VINF(NUVR+2)-VIND(NUVR+2)
            NUVR = NUVRF + 3
  98     CONTINUE
            NS = NS + NBSYS
  99     CONTINUE
      NUVRF = NUVRF + 3*NBSYS
      NSFA = NSFA + NBSYS
                 
      ELSE
         CALL LCEQVN ( NVI-1,  VIND , YD(NDT+1) )
         CALL LCEQVN ( NVI-1,  VINF , YF(NDT+1) )
      CALL LCEQVN ( NR,  YF , DY )
      CALL DAXPY( NR, -1.D0, YD, 1,DY, 1)
      ENDIF   

C     RECALCUL DE LA DERNIERE MATRICE JACOBIENNE
      CALL LCMMJA ( MOD, NMAT, MATER, TIMED, TIMEF, TEMPF,
     &    COMP,NBCOMM, CPMONO, PGL,TOUTMS,HSR,NR,NVI,VIND,YF,DY,DRDY)
      MONO1=NBCOMM(NMAT,1)
         
      IF (MONO1.EQ.1) THEN

        DO 101 K=1,6
        DO 101 J=1,6
           Z0(K,J)=DRDY(K,J)
 101     CONTINUE
        DO 201 K=1,6
        DO 201 J=1,NS
           Z1(K,J)=DRDY(K,NDT+J)
 201     CONTINUE

        DO 301 K=1,NS
        DO 301 J=1,6
           Z2(K,J)=DRDY(NDT+K,J)
 301     CONTINUE
        DO 401 K=1,NS
        DO 401 J=1,NS
           Z3(K,J)=DRDY(NDT+K,NDT+J)
 401     CONTINUE
C       Z2=INVERSE(Z3)*Z2
        CALL MGAUSS ('NFWP',Z3, Z2, NS, NS, 6, DET, IRET )
        
C       KYL=Z1*INVERSE(Z3)*Z2
        CALL PROMAT(Z1,6,6,NS,Z2,NS,NS,6,KYL)

C       Z0=Z0+Z1*INVERSE(Z3)*Z2
        DO 501 K=1,6
        DO 501 J=1,6
           Z0(K,J)=Z0(K,J)-KYL(K,J)
           DSDE(K,J)=I6(K,J)
 501    CONTINUE
 
C       DSDE = INVERSE(Z0-Z1*INVERSE(Z3)*Z2)

      
C        CALL MGAUSS ('NFVP',Z0, DSDE, 6, 6, 6, DET, IRET )
        CALL MGAUSS ('NFWP',Z0, DSDE, 6, 6, 6, DET, IRET )
        
      ELSE   
                  
C        NVV est le nombre de varaibles internes liées aux systemes de
C        glissement, il y en a 3*Ns

         NVV=NVI-2-6
 
         DO 10 K=1,6
         DO 10 J=1,6
            Y0(K,J)=DRDY(K,J)
 10      CONTINUE

         DO 20 K=1,6
         DO 20 J=1,NVV
            Y1(K,J)=DRDY(NDT+K,NDT+6+J)
 20      CONTINUE

         DO 30 K=1,NVV
         DO 30 J=1,6
            Y2(K,J)=DRDY(NDT+6+K,J)
 30      CONTINUE

         DO 40 K=1,NVV
         DO 40 J=1,NVV
            Y3(K,J)=DRDY(NDT+6+K,NDT+6+J)
 40      CONTINUE
 

C          Y2=INVERSE(Y3)*Y2
         CALL MGAUSS ('NFVP',Y3, Y2, NVV, NVV, 6, DET, IRET )

C         KYL=Y1*INVERSE(Y3)*Y2
         CALL PROMAT(Y1,6,6,NVV,Y2,NVV,NVV,6,KYL)

C         Y0=Y0+Y1*INVERSE(Y3)*Y2
         CALL DAXPY(36, 1.D0, KYL, 1,Y0, 1)
C         DSDE = INVERSE(Y0-Y1*INVERSE(Y3)*Y2)
         CALL LCEQMN ( 6 , Y0,DSDE)
 
 
         CALL INVALM (DSDE, 6, 6)
       ENDIF
      END
