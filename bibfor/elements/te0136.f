      SUBROUTINE TE0136 ( OPTION , NOMTE )
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
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*16        OPTION , NOMTE
C ......................................................................
C    - FONCTION REALISEE:  CALCUL DES VECTEURS ELEMENTAIRES
C                          OPTION : 'RESI_THER_COEF_R'
C                          OPTION : 'RESI_THER_RAYO_R'
C                          ELEMENTS 2D
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
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
      CHARACTER*24       CARAC,FF
      CHARACTER*8        ELREFE
      REAL*8             POIDS,R,NX,NY,TPG,THETA
      REAL*8             COORSE(18),VECTT(9)
      INTEGER            NNO,KP,NPG,ICARAC,IFF,IPOIDS,IVF,IDFDE,IGEOM
      INTEGER            IVERES,K,I,L,LI,IECH,IRAY
      INTEGER            NNOP2,C(6,9),ISE,NSE
      REAL*8             HECH,SIGMA,EPSIL,TZ0,R8T0
C
      CALL ELREF1(ELREFE)
      TZ0  = R8T0()
C
C
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
      IF     (OPTION(11:14).EQ.'COEF') THEN
         CALL JEVECH('PCOEFHR','L',IECH)
         HECH  = ZR(IECH)
      ELSEIF (OPTION(11:14).EQ.'RAYO') THEN
         CALL JEVECH('PRAYONR','L',IRAY)
         SIGMA = ZR(IRAY)
         EPSIL = ZR(IRAY+1)
      ENDIF
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PTEMPSR','L',ITEMPS)
      CALL JEVECH('PTEMPEI','L',ITEMP)
      CALL JEVECH('PRESIDU','E',IVERES)
C
      THETA = ZR(ITEMPS+2)
C
      CALL CONNEC ( NOMTE, ZR(IGEOM), NSE, NNOP2, C )
C
      DO 10 I=1,NNOP2
        VECTT(I)=0.D0
10    CONTINUE
C
C BOUCLE SUR LES SOUS-ELEMENTS
C
      DO 100 ISE=1,NSE
C
        DO 105 I=1,NNO
          DO 105 J=1,2
            COORSE(2*(I-1)+J) = ZR(IGEOM-1+2*(C(ISE,I)-1)+J)
105     CONTINUE
C
        DO 101 KP=1,NPG
          K = (KP-1)*NNO
          CALL VFF2DN (NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),COORSE,NX,NY,
     &                 POIDS)
          R = 0.D0
          TPG = 0.D0
          DO 102 I=1,NNO
            L = (KP-1)*NNO+I
            R   = R   + COORSE(2*(I-1)+1)    * ZR(IVF+L-1)
            TPG = TPG + ZR(ITEMP-1+C(ISE,I)) * ZR(IVF+L-1)
102       CONTINUE
          IF ( NOMTE(3:4) .EQ. 'AX' ) POIDS = POIDS*R
CCDIR$ IVDEP
          IF     (OPTION(11:14).EQ.'COEF') THEN
            DO 103 I=1,NNO
              LI = IVF+(KP-1)*NNO+I-1
              VECTT(C(ISE,I)) = VECTT(C(ISE,I))+ POIDS * THETA *
     &                         ZR(LI) * HECH * TPG
103         CONTINUE
          ELSEIF (OPTION(11:14).EQ.'RAYO') THEN
            DO 104 I=1,NNO
              LI = IVF+(KP-1)*NNO+I-1
              VECTT(C(ISE,I)) = VECTT(C(ISE,I))+ POIDS*THETA*ZR(LI)*
     &                         SIGMA* EPSIL* ( TPG + TZ0 )**4
104         CONTINUE
          ENDIF
C
101     CONTINUE
100   CONTINUE
C
      DO 200 I=1,NNOP2
        ZR(IVERES-1+I)=VECTT(I)
200   CONTINUE
C
      END
