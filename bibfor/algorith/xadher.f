      SUBROUTINE XADHER(P,SAUT,LAMB1,RHOTK,
     &                            PBOUL,KN,PTKNP,IK)
     
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 14/02/2005   AUTEUR DURAND C.DURAND 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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

      IMPLICIT NONE
      REAL*8    P(3,3),SAUT(3),LAMB1(3),RHOTK,PBOUL(3),KN(3,3)
      REAL*8    PTKNP(3,3),IK(3,3)
          
C ----------------------------------------------------------------------
C                      TEST DE L'ADHÉRENCE AVEC X-FEM 
C                ET CALCUL DES MATRICES DE FROTTEMENT UTILES

C IN    P           : OPÉRATEUR DE PROJECTION 
C IN    SAUT        : SAUT DES INCRÉMENTS DE DÉPLACEMENTS 
C                     DEPUIS L'ÉQUILIBRE PRÉCÉDENT
C IN    LAMB1       : INCRÉMENTS DU SEMI-MULTIPLICATEUR DE FROTTEMENT
C                     DEPUIS L'ÉQUILIBRE PRÉCÉDENT DANS LA BASE GLOBALE
C IN    RHOTK       : COEFFICIENT DE REGULARISATION DE FROTTEMENT 
C                       DIVISÉ PAR L'INCRÉMENT DE TEMPS 

C OUT   PBOUL       : PROJECTION SUR LA BOULE B(0,1)
C OUT   KN          : KN(LAMDBA + RHO [[DX]]/DELTAT )
C OUT   PTKNP       : MATRICE Pt.KN.P
C OUT   PTKNP       : MATRICE Id-KN

C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------

      CHARACTER*32 JEXNUM,JEXNOM,JEXR8,JEXATR
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL,EXIGEO
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)


C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------

      INTEGER    NDIM,I,J,K
      REAL*8     VITANG(3),PREC,NORME,XAB(3,3),KNP(3,3),GT(3)
      REAL*8     THETA,XIK(3,2)
      PARAMETER  (NDIM=3,PREC=1.D-12)
      LOGICAL    ADHER

C-----------------------------------------------------------------------
C     CALCUL DE GT = LAMDBA + RHO [[DX]]/DELTAT ET DE SA PROJECTION

      DO 10 I=1,NDIM
        VITANG(I)=0.D0
C       "VITESSE TANGENTE" : PROJECTION DU SAUT
        DO 20  K=1,NDIM
          VITANG(I)=VITANG(I)+P(I,K)*SAUT(K)
 20     CONTINUE
        GT(I)=LAMB1(I)+RHOTK*VITANG(I)
 10   CONTINUE

C      WRITE(6,*)'GT ',GT
      
      NORME=SQRT(GT(1)*GT(1)+GT(2)*GT(2)+GT(3)*GT(3))
C      write(6,*)'norme GT ',NORME

C     ADHER : TRUE SI ADHÉRENCE, FALSE SI GLISSEMENT      
      IF (NORME.LE.(1.D0+PREC)) THEN
        ADHER = .TRUE.
        DO 21 J=1,3
          PBOUL(J)=GT(J)
 21   CONTINUE
      ELSE  
        ADHER = .FALSE.
        DO 22 J=1,3
          PBOUL(J)=GT(J)/NORME
 22   CONTINUE
      ENDIF

C      write(6,*)'Adherence ? ',ADHER

C-----------------------------------------------------------------------
C     CALCUL DE KN(LAMDBA + RHO [[DX]]/DELTAT )

C     ADHERENCE
      IF (ADHER) THEN

        CALL MATINI(NDIM,NDIM,0.D0,KN)
        DO 30 I=1,NDIM
          KN(I,I)=1.D0
 30     CONTINUE

C     GLISSEMENT
      ELSEIF (.NOT.ADHER) THEN

        THETA=1.D0
        DO 40 I = 1,NDIM
          DO 41 J = 1,NDIM
            KN(I,J)=-THETA*GT(I)*GT(J)/(NORME*NORME)
 41       CONTINUE
 40     CONTINUE

        DO 42 I = 1,NDIM
          KN(I,I)= KN(I,I) + 1.D0
 42     CONTINUE

        CALL MATPRS(NDIM,1/NORME,KN)

      ENDIF

C-----------------------------------------------------------------------

C     CALCUL DE PT.KN.P
      CALL UTBTAB('ZERO',NDIM,NDIM,KN,P,XAB,PTKNP)
 
C     CALCUL DE Id-KN
      DO 50 I = 1,NDIM
        DO 51 J = 1,NDIM
          IK(I,J)= -1.D0 * KN(I,J)
 51     CONTINUE
 50   CONTINUE
      DO 52 I = 1,NDIM
        IK(I,I)= 1.D0 + IK(I,I)
 52   CONTINUE
      
C-----------------------------------------------------------------------

      END
