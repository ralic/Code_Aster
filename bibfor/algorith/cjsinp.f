        SUBROUTINE CJSINP(MATER,EPSD,DEPS,SIGF,VINF,NITER,NVI,NIVCJS,
     +                    NDEC,EPSCON)
C
        IMPLICIT NONE
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 03/10/2002   AUTEUR CIBHHBC R.FERNANDES 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C CETTE ROUTINE COMPLETE LES VARIABLES INTERNES EN VUE DE DEPOUILLEMENT
C     LOI CJS
C     IN
C          MATER    :  COEFFICIENTS MATERIAU
C          NVI      :  NB DE VARIABLES INTERNES
C          EPSD     :  DEFORMATIONS A T
C          DEPS     :  INCREMENTS DE DEFORMATION
C          SIGF     :  CONTRAINTE  A T+DT
C     VAR
C          VINF     :  VARIABLES INTERNES  A T+DT
C             NDT  6 EN 3D ET 4 EN 2D
C
C DANS TOUS LES CAS
C -----------------
C       VINF(NDT+6) = NITER
C       VINF(NDT+7) = EPSCON
C       VINF(NDT+8) = NDEC
C
C       VINF(NDT+5) = ABS((I1F+QINIT)/(VINF(1)*TROIS))
C                     ETAT CONTRAINTE / CRITERE ISOTROPE
C                     DE 0 A 1
C
C EN CJS1
C -------
C         VINF(NDT+3)=ABS(QII*HTQ/(RM*(I1F+QINIT)))
C                     ETAT CONTRAINTE / CRITERE DEVIATOIRE
C                     DE 0 A 1
C EN CJS2
C -------
C
C         VINF(NDT+3)=ABS(QII*HTQ/(R*(I1F+QINIT)))
C                     ETAT CONTRAINTE / CRITERE DEVIATOIRE
C                     DE 0 A 1
C         VINF(NDT+4) = R/RM
C                     RAYON SURFACE PLASTIQUE DEVIATOIRE /
C                        RAYON SURFACE LIMITE DEVIATOIRE
C
C EN CJS3
C -------
C
C        VINF(NDT+3)=ABS(QII*HTQ/(R*(I1F+QINIT)))
C                     ETAT CONTRAINTE / CRITERE DEVIATOIRE
C                     DE 0 A 1
C        VINF(NDT+4) = XII/XIIL
C                     DEPLACEMENT SURFACE PLASTIQUE DEVIATOIRE /
C                        POSITION SURFACE LIMITE DEVIATOIRE
C ======================================================================
        INTEGER       NDT, NDI,NVI,NITER,NDEC,I
        REAL*8        EPSD(6),DEPS(6),SIGF(6),VINF(NVI),EPSCON
        REAL*8        MATER(14,2),  RM, RC, PCO, PA, PC
        REAL*8        GAMMA, MUCJS, EPSSIG, PREF,QINIT
        REAL*8        I1F,C,S(6),Q(6),QQ(6), XF(6)
        REAL*8        R, XII, EPSV, PHIO, RR, COSA, COSDIF
        REAL*8        SII, HTS, DETS, COS3TS
        REAL*8        SIIREL, QIIREL, ZERO, UN , DEUX, TROIS
        REAL*8        QII, HTQ, DETQ, COS3TQ
        REAL*8        TANGS, TANGQ, TETAS, TETAQ
        REAL*8        XIIL,QQII,COEF2,COEF3,TRACE
        CHARACTER*4   NIVCJS
C ======================================================================
        PARAMETER     ( UN   = 1.D0   )
        PARAMETER     ( ZERO = 0.D0   )
        PARAMETER     ( DEUX = 2.D0   )
        PARAMETER     ( TROIS= 3.D0   )
        PARAMETER     ( EPSSIG = 1.D-8   )
C ======================================================================
        COMMON /TDIM/   NDT, NDI
C ======================================================================
        CALL JEMARQ ()
C ======================================================================
C --- PROPRIETES CJS MATERIAU ------------------------------------------
C ======================================================================
        RM    = MATER(2,2)
        RC    = MATER(5,2)
        C     = MATER(8,2)
        GAMMA = MATER(9,2)
        MUCJS = MATER(10,2)
        PCO   = MATER(11,2)
        PA    = MATER(12,2)
        QINIT = MATER(13,2)
C ======================================================================
C --- PREMIER INVARIANT ET AUTRES GRANDEURS UTILES ---------------------
C ======================================================================
        DO 5 I = 1 , NDT
           XF(I) = VINF(I+2)
 5      CONTINUE
C ======================================================================
        I1F = TRACE(NDI,SIGF)
        IF((I1F+QINIT)  .EQ. 0.D0 ) THEN
          I1F  = -QINIT+1.D-12 * PA
          PREF = ABS(PA)
        ELSE
          PREF = ABS(I1F+QINIT)
        ENDIF
C ======================================================================
        CALL CJSQCO( GAMMA, SIGF, XF, PREF, EPSSIG, I1F,
     +               S, SII, SIIREL, COS3TS, HTS, DETS,
     +               Q, QII, QIIREL, COS3TQ, HTQ, DETQ )
C ======================================================================
        CALL LCPRSC(XF,XF,XII)
        XII = SQRT(XII)

        EPSV = ZERO
        DO 30 I = 1,NDI
           EPSV = EPSV + EPSD(I)+ DEPS(I)
  30    CONTINUE
C ======================================================================
C --- CAS CJS3 ---------------------------------------------------------
C ======================================================================
        IF ( NIVCJS.EQ.'CJS3') THEN
           PC = PCO*EXP(-C*EPSV)
           IF(XII .LE. EPSSIG ) THEN
              PHIO= UN
           ELSE IF(SIIREL .LE. EPSSIG ) THEN
              COSA = UN
              COSDIF = UN
              RR = RC + MUCJS*MAX(ZERO,LOG(TROIS*PC/(I1F+QINIT)))
              PHIO = COSA/( RR - HTS/HTQ*RM*COSDIF)
           ELSE
              COSA =  ( QII*QII - SII*SII - I1F*I1F*XII*XII ) /
     &                 (DEUX*SII*I1F*XII)

              TANGS = SQRT(UN-COS3TS*COS3TS) / COS3TS
              TANGQ = SQRT(UN-COS3TQ*COS3TQ) / COS3TQ
              TETAS = ATAN2(TANGS,1.D0) / TROIS
              TETAQ = ATAN2(TANGQ,1.D0) / TROIS
              COSDIF = COS(TETAS-TETAQ)

              RR = RC + MUCJS*MAX(ZERO,LOG(TROIS*PC/(I1F+QINIT)))
              PHIO = COSA/( RR - HTS/HTQ*RM*COSDIF)
           ENDIF
           XIIL=UN/(PHIO*HTS)
        ENDIF

        IF ( NIVCJS.EQ.'CJS2'.OR.NIVCJS.EQ.'CJS3') THEN
          VINF(NDT+5) = ABS((I1F+QINIT)/(VINF(1)*TROIS))
        ENDIF
        VINF(NDT+6) = NITER
        VINF(NDT+7) = EPSCON
        VINF(NDT+8) = NDEC

        IF ( NIVCJS.EQ.'CJS1') THEN
         IF ( (ABS(I1F+QINIT)/PREF).LT.EPSSIG) THEN
          VINF(NDT+3) = UN
         ELSE
          VINF(NDT+3)=ABS(QII*HTQ/(RM*(I1F+QINIT)))
         ENDIF
        ELSE IF (NIVCJS.EQ.'CJS2') THEN
         R = VINF(2)
         VINF(NDT+4) = R/RM
         IF ( (ABS(R*(I1F+QINIT))/PREF).LT.EPSSIG) THEN
          VINF(NDT+3) = UN
         ELSE
          VINF(NDT+3)=ABS(QII*HTQ/(R*(I1F+QINIT)))
         ENDIF
        ELSE IF (NIVCJS.EQ.'CJS3') THEN
         R = VINF(2)
         VINF(NDT+4) = XII/XIIL
         IF ( (ABS(R*(I1F+QINIT))/PREF).LT.EPSSIG) THEN
          VINF(NDT+3) = UN
         ELSE
          VINF(NDT+3)=ABS(QII*HTQ/(R*(I1F+QINIT)))
         ENDIF
        ENDIF
C ======================================================================
        CALL JEDEMA ()
C ======================================================================
        END
