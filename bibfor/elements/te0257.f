      SUBROUTINE TE0257(OPTION,NOMTE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 04/04/2002   AUTEUR VABHHTS J.PELLET 
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
C.......................................................................
      IMPLICIT REAL*8 (A-H,O-Z)
C
C     BUT: CALCUL DES MATRICES DE MASSE  ELEMENTAIRES EN MECANIQUE
C          ELEMENTS 1D DE COUPLAGE ACOUSTICO-MECANIQUE
C
C          OPTION : 'MASS_MECA '
C
C     ENTREES  ---> OPTION : OPTION DE CALCUL
C          ---> NOMTE  : NOM DU TYPE ELEMENT
C.......................................................................
C
      CHARACTER*2        CODRET
      CHARACTER*8        ELREFE
      CHARACTER*16       NOMTE,OPTION
      CHARACTER*24       CHVAL,CARAC
      REAL*8             A(3,3,3,3),NX,NY,RHO,NORM(2),POIDS
      INTEGER            IGEOM,IMATE,I,J,K,L,IK,IJKL,LDEC,KCO,INO,JNO
      INTEGER            NNO,NPG2,NPG1,KP
      INTEGER            ICARAC,IFF,IPOIDS,IVF,IDFDE,IMATUU
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      CHARACTER*32       JEXNUM , JEXNOM , JEXR8 , JEXATR
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
      CARAC = '&INEL.'//ELREFE//'.CARAC'
      CALL JEVETE(CARAC,'L',ICARAC)
      NNO = ZI(ICARAC)
      NPG1= ZI(ICARAC+2)
C
      CHVAL = '&INEL.'//ELREFE//'.FF'
      CALL JEVETE(CHVAL,'L',IFF)
      IPOIDS = IFF
      IVF    = IPOIDS + NPG1
      IDFDE  = IVF    + NPG1*NNO
C
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PMATERC','L',IMATE)
      CALL JEVECH('PMATUUR','E',IMATUU)
C
      CALL RCVALA(ZI(IMATE),'FLUIDE',0,' ',R8B,1,'RHO',RHO,CODRET,'FM')
C
C     INITIALISATION DE LA MATRICE
C
      DO 112 K=1,3
         DO 112 L=1,3
            DO 112 I=1,NNO
            DO 112 J=1,I
                A(K,L,I,J) = 0.D0
112   CONTINUE
C
C    BOUCLE SUR LES POINTS DE GAUSS
C
      DO 113 KP=1,NPG1
         LDEC = (KP-1)*NNO
C
         CALL VFF2DN ( NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+LDEC),ZR(IGEOM),
     &                 NX,NY,POIDS)
C
         NORM(1) = NX
         NORM(2) = NY
C
        IF ( NOMTE(3:4) .EQ. 'AX' ) THEN
           R = 0.D0
           DO 102 I=1,NNO
             R = R + ZR(IGEOM+2*(I-1))*ZR(IVF+LDEC+I-1)
102        CONTINUE
           POIDS = POIDS*R
        ENDIF
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       CALCUL DU TERME PHI*(U.N DS)       C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        DO 130 INO=1,NNO
           DO 140 JNO=1,INO
               DO 150 KCO=1,2
C
              A(KCO,3,INO,JNO) = A(KCO,3,INO,JNO) +
     &            POIDS* NORM(KCO) * RHO *
     &            ZR(IVF+LDEC+INO-1) * ZR(IVF+LDEC+JNO-1)



C
150           CONTINUE
140        CONTINUE
130      CONTINUE

113     CONTINUE
C
      DO 151 INO=1,NNO
        DO 152 JNO=1,INO
          DO 153 KCO=1,2
             A(3,KCO,INO,JNO) = A(KCO,3,INO,JNO)
153        CONTINUE
152     CONTINUE
151   CONTINUE
C

C PASSAGE DU STOCKAGE RECTANGULAIRE (A) AU STOCKAGE TRIANGULAIRE (ZR)
C
      IJKL = 0
      IK = 0
      DO 160 K=1,3
         DO 160 L=1,3
            DO 160 I=1,NNO
                IK = ((3*I+K-4) * (3*I+K-3)) / 2
            DO 160 J=1,I
                IJKL = IK + 3 * (J-1) + L
                ZR(IMATUU+IJKL-1) = A(K,L,I,J)




160          CONTINUE

      END
