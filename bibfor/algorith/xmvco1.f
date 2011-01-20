      SUBROUTINE XMVCO1(NDIM,NNO,NNOL,NNOF,ALPHA,BETASQ,
     &                  DSIDEP,PP,P,PLA,IPGF,IVFF,IFA,CFACE,LACT ,
     &                  KA,VALRES,DNOR,DTANG,NFH,DDLS,JAC,FFC,FFP,
     &                  SINGU,E,RR,AM,NOEUD,CSTACO,ND,TAU1,TAU2,
     &                  VTMP)


      IMPLICIT NONE
      INTEGER     NDIM,NNO,NNOL,NNOF
      INTEGER     NFH,DDLS,PLA(27),LACT(8),CFACE(5,3)
      INTEGER     SINGU,IPGF,IVFF,IFA
      REAL*8      VTMP(400),AM(3),DSIDEP(6,6)
      REAL*8      FFP(27),JAC,ALPHA,KA
      REAL*8      BETASQ,VALRES(3)
      REAL*8      DTANG(3),DNOR(3),E,ND(3),TAU1(3),TAU2(3)
      REAL*8      PP(3,3),P(3,3),RR,FFC(8),CSTACO
      LOGICAL     NOEUD

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 19/01/2011   AUTEUR MASSIN P.MASSIN 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================

C TOLE CRP_21
C
C ROUTINE CONTACT (METHODE XFEM HPP - CALCUL ELEM.)
C
C --- CALCUL DES SECONDS MEMBRES DE COHESION
C      
C ----------------------------------------------------------------------
C
C IN  NDIM   : DIMENSION DE L'ESPACE
C IN  NNO    : NOMBRE DE NOEUDS DE L'ELEMENT DE REF PARENT
C IN  ALPHA  : 
C IN  ALPHA0 : 
C IN  DSIDEP : 
C IN  PP     : 
C IN  P      : 
C IN  SIGMC  : 
C IN  DNOR   : 
C IN  DTANG  : 
C IN  ALPHA : 
C IN  GC     : 
C IN  NFH    : NOMBRE DE FONCTIONS HEAVYSIDE
C IN  DDLS   : NOMBRE DE DDL (DEPL+CONTACT) À CHAQUE NOEUD SOMMET
C IN  JAC    : PRODUIT DU JACOBIEN ET DU POIDS
C IN  FFP    : FONCTIONS DE FORME DE L'ELEMENT PARENT
C IN  SINGU  : 1 SI ELEMENT SINGULIER, 0 SINON
C IN  RR     : DISTANCE AU FOND DE FISSURE
C IN  AM     : 
C I/O MMAT   : MATRICE ELEMENTAITRE DE CONTACT/FROTTEMENT 
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX --------------------
C
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)

C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX --------------------
C
      INTEGER I,J,K,PLI,NLI
      REAL*8  COEFF2,GC,SIGMC,TSELA1,TSELA2
      REAL*8  FFI,PTPG(3),PPTG(3),TTX(3),DDOT
C
C ----------------------------------------------------------------------

      GC=VALRES(1)
      SIGMC=VALRES(2)


                  IF ( ALPHA.GT.KA.AND.AM(1).GT.0 ) THEN  
C                   CHARGE COHESIVE-ENDOMAGEMENT
                    COEFF2 = (SIGMC/ALPHA) * EXP(-SIGMC*ALPHA/GC)
                    TSELA1=COEFF2
                    TSELA2=COEFF2                    

                  ELSE
                    TSELA1=DSIDEP(1,1)
                    IF(ALPHA.GT.KA) THEN
                      TSELA2=(-GC/(SIGMC*ALPHA))*DSIDEP(2,2)
                    ELSE
                      TSELA2=DSIDEP(2,2)
                    ENDIF                  
                  ENDIF

                  DO 248 I=1,NDIM
                    PTPG(I)=0.D0
                    PPTG(I)=0.D0
                    DO 249 K=1,NDIM
                      PTPG(I)=PTPG(I) + P(K,I)*DTANG(K)
                      PPTG(I)=PPTG(I) + PP(K,I)*DNOR(K)
 249                CONTINUE  
 248              CONTINUE                 

                  DO 450 I = 1,NNO
                    DO 451 J = 1,NFH*NDIM 
                      VTMP(DDLS*(I-1)+NDIM+J) =
     &                VTMP(DDLS*(I-1)+NDIM+J)- 
     &                 (2.D0*TSELA1*PPTG(J)*FFP(I)*JAC)-
     &                 (2.D0*BETASQ*TSELA2*PTPG(J)*FFP(I)*JAC)

 451                CONTINUE
                    DO 452 J = 1,SINGU*NDIM
                        VTMP(DDLS*(I-1)+NDIM*(1+NFH)+J) =
     &                  VTMP(DDLS*(I-1)+NDIM*(1+NFH)+J)- 
     &                 (2.D0*TSELA1*PPTG(J)*FFP(I)*JAC*RR)-
     &                 (2.D0*BETASQ*TSELA2*PTPG(J)*FFP(I)*JAC*RR)
 
 452                CONTINUE
 450              CONTINUE

                  CALL VECINI(3,0.D0,TTX)

                  DO 460 I = 1,NNOL
                    PLI=PLA(I)
                    IF (NOEUD) THEN
                    FFI=FFC(I)
                    NLI=LACT(I)
                    IF (NLI.EQ.0) GOTO 460
                    ELSE
                      FFI=ZR(IVFF-1+NNOF*(IPGF-1)+I)
                      NLI=CFACE(IFA,I)
                    ENDIF
                    TTX(1)=DDOT(NDIM,TAU1(1),1,DTANG,1)
                    IF (NDIM .EQ.3) TTX(2)=DDOT(NDIM,TAU2(1),
     &                                           1,DTANG,1)
                    DO 465 K=1,NDIM-1
                      VTMP(PLI+K) = VTMP(PLI+K)
     &              -  BETASQ*TSELA2*TTX(K)*FFI*JAC
 465                CONTINUE
 460              CONTINUE
  
                  DO 470 I = 1,NNOL
                    PLI=PLA(I)
                    IF (NOEUD) THEN
                    FFI=FFC(I)
                    NLI=LACT(I)
                    IF (NLI.EQ.0) GOTO 470
                    ELSE
                      FFI=ZR(IVFF-1+NNOF*(IPGF-1)+I)
                      NLI=CFACE(IFA,I)
                    ENDIF
                    DO 475 K = 1, NDIM
                      VTMP(PLI) = VTMP(PLI) - 
     &                TSELA1*DNOR(K)*ND(K)*FFI*JAC/CSTACO*E
 475                CONTINUE
 470              CONTINUE 

      END
