      SUBROUTINE CACDSU(MAXFA,MAXDIM,ALPHA,
     &                  NDIM,NNO,NFACE,GEOM,
     &                  VOL,MFACE,DFACE,XFACE,NORMFA,KDIAG,YSS,C,D)
      IMPLICIT NONE
      INTEGER MAXFA,MAXDIM,NDIM,NNO,NFACE
      REAL*8 ALPHA,VOL
      REAL*8 GEOM(NDIM,NNO)
      REAL*8 MFACE(MAXFA),DFACE(MAXFA),XFACE(MAXDIM,MAXFA)
      REAL*8 KDIAG(6),YSS(MAXDIM,MAXFA,MAXFA)
      REAL*8 C(MAXFA,MAXFA),D(MAXFA,MAXFA)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/04/2011   AUTEUR DELMAS J.DELMAS 
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
C TOLE CRS_1404
C
C  PAR CONVENTION LA FACE I A POUR PREMIER SOMMET LE SOMMET I
C  CE SERA PLUS COMPLIQUE EN 3D !!!
C ## VARIABLES IN :
C         NDIM DIMENSION D ESPACE
C         NFACE NOMBRE DE FACES
C         KDIAG(3)   PERMEABILITE INTRINSEQUE (DIAGONALE)
C         MFACE : MESURE DES FACES
C         DFACE : DISTANCE DU CENTRE DE GRAVITÉ AUX FACES
C         DFACE N EST PAS XG - XFACE
C         XFACE : COORDONNES DES CENTRES DE FACES
C         NORMFA NORMALE SORTANTE
C ## VARIABLES OUT :
C         YSS,C,D
C
C  ((DFACE(KFA)*MFACE(KFA))/DIM):     CORRESPOND A LA MESURE DU CONE
C                                     DE SOMMET X_K ET DE BASE SIGMA
C
C
C REMARQUE :
C
C    POUR LE MOMENT ON PRENDS BETA=SQRT(DIM) MAIS ON PEUT
C    FAIRE VARIER CETTE VALEUR
C
C ----------------------------------------------------------------------
C
      INTEGER       MAXFA1,MAXDI1
      PARAMETER    (MAXFA1=6,MAXDI1=3)

      INTEGER       IFA,JFA,KFA
      INTEGER       IDIM,JDIM

      REAL*8        DIM,SQDIM,BETA
      REAL*8        MCONE(MAXFA)
      REAL*8        NORMFA(MAXDIM,MAXFA)
      REAL*8        XG(MAXDI1)
      REAL*8        NDX(MAXFA1,MAXFA1),KINT(MAXDI1,MAXDI1)
      REAL*8        KUNI(MAXDI1,MAXDI1),KY,KY1
C
C ----------------------------------------------------------------------
C
      CALL ASSERT(MAXFA1.EQ.MAXFA)
      CALL ASSERT(MAXDI1.EQ.MAXDIM)

      IF ( NDIM.EQ.2) THEN
         KINT(1,1)=KDIAG(1)
         KINT(2,2)=KDIAG(2)
         KINT(1,2)=KDIAG(3)
         KINT(2,1)=KDIAG(3)
C
         KUNI(1,1)=1.D0
         KUNI(2,2)=1.D0
         KUNI(1,2)=0.D0
         KUNI(2,1)=0.D0
      ELSE IF ( NDIM.EQ.3) THEN
         KINT(1,1)=KDIAG(1)
         KINT(2,2)=KDIAG(2)
         KINT(3,3)=KDIAG(3)
         KINT(1,2)=KDIAG(4)
         KINT(1,3)=KDIAG(5)
         KINT(2,3)=KDIAG(6)
         KINT(2,1)=KDIAG(4)
         KINT(3,1)=KDIAG(5)
         KINT(3,2)=KDIAG(6)
C
         KUNI(1,1)=1.D0
         KUNI(2,2)=1.D0
         KUNI(3,3)=1.D0
         KUNI(1,2)=0.D0
         KUNI(1,3)=0.D0
         KUNI(2,3)=0.D0
         KUNI(2,1)=0.D0
         KUNI(3,1)=0.D0
         KUNI(3,2)=0.D0
      ELSE
         CALL ASSERT(.FALSE.)
      ENDIF
      DIM=NDIM
      SQDIM=SQRT(DIM)
      BETA=SQDIM*SQRT(ALPHA)
      DO 10 IDIM = 1 , NDIM
         XG(IDIM)=GEOM(IDIM,NNO)
   10 CONTINUE
C
C======================== INITIALISATION========================
      DO 20 IFA = 1 ,MAXFA
         DO 20 JFA=1,MAXFA
            DO 21 IDIM = 1 , MAXDIM
               YSS(IDIM,IFA,JFA)=0.D0
   21       CONTINUE
               C(IFA,JFA)=0.D0
               D(IFA,JFA)=0.D0
   20 CONTINUE
C
C =========================== CALCUL DE YSS==========================
      DO 30 IFA=1,NFACE
         MCONE(IFA)=(DFACE(IFA)*MFACE(IFA))/DIM
C
C
         DO 31 JFA = 1 , NFACE
            NDX(IFA,JFA) = 0.D0
            DO 32 IDIM = 1 , NDIM
               NDX(IFA,JFA)=NDX(IFA,JFA)+
     &                      NORMFA(IDIM,IFA)*(XFACE(IDIM,JFA)-XG(IDIM))
   32       CONTINUE
   31    CONTINUE
   30 CONTINUE
      DO 40 IFA = 1 ,NFACE
         DO 40 JFA=1,NFACE
            IF(JFA.EQ.IFA) THEN
               DO 41 IDIM = 1, NDIM
                  YSS(IDIM,IFA,IFA)= (MFACE(IFA)/VOL)*NORMFA(IDIM,IFA)+
     &                               (BETA/DFACE(IFA))*
     &                               (1.D0-(MFACE(IFA)/VOL)*
     &                                NDX(IFA,IFA))*NORMFA(IDIM,IFA)
   41          CONTINUE
            ELSE
               DO 42 IDIM = 1, NDIM
                  YSS(IDIM,IFA,JFA)= (MFACE(JFA)/VOL)*NORMFA(IDIM,JFA)
     &                              -(BETA/(DFACE(IFA)*VOL))*MFACE(JFA)*
     &                                NDX(JFA,IFA)*NORMFA(IDIM,IFA)
   42          CONTINUE
            ENDIF
   40 CONTINUE
C
      DO 50 IFA = 1, NFACE
         DO 50 JFA = 1 , NFACE
            C(IFA,JFA) = 0.D0
            D(IFA,JFA) = 0.D0
            DO 50 KFA = 1 , NFACE
               DO 52 IDIM = 1 , NDIM
                  KY  = 0.D0
                  KY1 = 0.D0
                  DO 51 JDIM = 1 , NDIM
                     KY  =  KY  +  MCONE(KFA)*KINT(IDIM,JDIM)*
     &                      YSS(JDIM,KFA,JFA)
                     KY1 =  KY1 +  MCONE(KFA)*KUNI(IDIM,JDIM)*
     &                      YSS(JDIM,KFA,JFA)
   51             CONTINUE
                  C(IFA,JFA) = C(IFA,JFA) + YSS(IDIM,KFA,IFA)*KY
                  D(IFA,JFA) = D(IFA,JFA) + YSS(IDIM,KFA,IFA)*KY1
   52          CONTINUE
   50 CONTINUE
      END
