      SUBROUTINE TE0553 ( OPTION , NOMTE )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 11/04/2002   AUTEUR CIBHHLV L.VIVAN 
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
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*16        OPTION , NOMTE
C ......................................................................
C
C     BUT: CALCUL DES MATRICES ELEMENTAIRES EN MECANIQUE
C          CORRESPONDANT A UN AMORTISSEMENT 
C          SUR DES FACES D'ELEMENTS ISOPARAMETRIQUES 2D
C
C          OPTION : 'AMOR_MECA'
C
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
C
      CHARACTER*24       CARAC,FF
      CHARACTER*8        NOMRES(3),ELREFE
      CHARACTER*2        CODRET(3)
      REAL*8             POIDS,NX,NY,VALRES(3),E,NU,LAMBDA,MU
      REAL*8             RHOCP,RHOCS
      REAL*8             RHO,TAUX,TAUY,NUX,NUY,SCAL,VNX,VNY,VTX,VTY
      REAL*8             VITUNI(2,2),VECT(3,2,6),MATR(6,6),JAC
      INTEGER            NNO,KP,NPG,ICARAC,IFF,IPOIDS,IVF,IDFDE,IGEOM
      INTEGER            K,I,L,MATER
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      CALL ELREF1(ELREFE)

      CARAC='&INEL.'//ELREFE//'.CARAC'
      CALL JEVETE(CARAC,'L',ICARAC)
      NNO=ZI(ICARAC)
      NPG=ZI(ICARAC+2)
C
      FF   ='&INEL.'//ELREFE//'.FF'
      CALL JEVETE(FF,'L',IFF)
      IPOIDS=IFF
      IVF   =IPOIDS+NPG
      IDFDE =IVF   +NPG*NNO
C
      CALL JEVECH ( 'PGEOMER', 'L', IGEOM )
      CALL JEVECH ( 'PMATERC', 'L', IMATE )
      CALL JEVECH  ('PMATUUR', 'E', IMATUU)
C
      MATER=ZI(IMATE)
      NOMRES(1)='E'
      NOMRES(2)='NU'
      NOMRES(3)='RHO'
      CALL RCVALA(MATER,'ELAS',0,' ',R8B,3,NOMRES,VALRES,CODRET,'FM')
      E = VALRES(1)
      NU = VALRES(2)
      RHO = VALRES(3)

      LAMBDA = E*NU/(1.D0+NU)/(1.D0-2.D0*NU)
      MU = E/2.D0/(1.D0+NU)

      RHOCP = SQRT((LAMBDA+2.D0*MU)*RHO)
      RHOCS = SQRT(MU*RHO)

C     VITESSE UNITAIRE DANS LES 3 DIRECTIONS

      VITUNI(1,1) = 1.D0
      VITUNI(1,2) = 0.D0

      VITUNI(2,1) = 0.D0
      VITUNI(2,2) = 1.D0

      DO 310 I = 1,NNO
         DO 311 J = 1,2
            DO 312 K = 1,2*NNO
               VECT(I,J,K) = 0.D0
312         CONTINUE
311      CONTINUE
310   CONTINUE
C
C    BOUCLE SUR LES POINTS DE GAUSS
C
      DO 101 KP=1,NPG
        K = (KP-1)*NNO

        CALL VFF2DN (NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IGEOM),NX,NY,
     &               POIDS)
C
         JAC = SQRT (NX*NX + NY*NY)
C
C        --- CALCUL DE LA NORMALE UNITAIRE ---
C
          NUX = NX / JAC
          NUY = NY / JAC
C
C        --- CALCUL DE V.N ---
C
          SCAL = 0.D0
          DO 110 I = 1,NNO
             DO 111 J = 1,2
                SCAL = NUX*ZR(IVF+K+I-1)*VITUNI(J,1)
                SCAL = SCAL+NUY*ZR(IVF+K+I-1)*VITUNI(J,2)
C
C        --- CALCUL DE LA VITESSE NORMALE ET DE LA VITESSE TANGENCIELLE
C
                VNX = NUX*SCAL
                VNY = NUY*SCAL

                VTX = ZR(IVF+K+I-1)*VITUNI(J,1)
                VTY = ZR(IVF+K+I-1)*VITUNI(J,2)

                VTX = VTX - VNX
                VTY = VTY - VNY

C
C        --- CALCUL DU VECTEUR CONTRAINTE
C
                TAUX = RHOCP*VNX + RHOCS*VTX
                TAUY = RHOCP*VNY + RHOCS*VTY

C
C        --- CALCUL DU VECTEUR ELEMENTAIRE
C
                DO 130 L = 1,NNO
                   LL = 2*L-1
                   VECT(I,J,LL) = VECT(I,J,LL) +
     &               TAUX*ZR(IVF+K+L-1)*POIDS*JAC
                   VECT(I,J,LL+1) = VECT(I,J,LL+1) +
     &               TAUY*ZR(IVF+K+I-1)*POIDS*JAC
130             CONTINUE
111          CONTINUE
110       CONTINUE

101   CONTINUE

      DO 400 I = 1,NNO
          DO 401 J = 1,2
             DO 402 K = 1,2*NNO
                MATR(2*(I-1)+J,K) = VECT(I,J,K)
402          CONTINUE
401       CONTINUE
400   CONTINUE

C
C       --- PASSAGE AU STOCKAGE TRIANGULAIRE
C
      DO 210 I = 1,2*NNO
          DO 211 J =1,I
             IJ = (I-1)*I/2+J
             ZR(IMATUU+IJ-1) = MATR(I,J)
211       CONTINUE
210   CONTINUE

      END
