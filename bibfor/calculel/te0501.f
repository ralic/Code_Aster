      SUBROUTINE TE0501 ( OPTION , NOMTE )
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 04/04/2002   AUTEUR VABHHTS J.PELLET 
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
C
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*16        OPTION , NOMTE
C ----------------------------------------------------------------------
C    - FONCTION REALISEE:  CALCUL DES MATRICES ELEMENTAIRES
C                          OPTION : 'RIGI_THER_TRANS'
C
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
      CHARACTER*8  ELREFE
      REAL*8             DFDX(9),DFDY(9),POIDS,R,TPG,XKPT,ALPHA
      INTEGER            NNO,KP,NPG1,I,J,K,ITEMPS,IFON(3),IMATTT
      INTEGER            ICARAC,IFF,IPOIDS,IVF,IDFDE,IDFDK,IGEOM,IMATE
C DEB -----------------------------------------------------------------
C
      CALL ELREF1(ELREFE)
C

      CARAC='&INEL.'//ELREFE//'.CARAC'
      CALL JEVETE(CARAC,'L',ICARAC)
      NNO  = ZI(ICARAC)
      NPG1 = ZI(ICARAC+2)
      NPG2 = ZI(ICARAC+3)
C
      FF   ='&INEL.'//ELREFE//'.FF'
      CALL JEVETE(FF,'L',IFF)
      IPOIDS=IFF   +NPG1*(1+3*NNO)
      IVF   =IPOIDS+NPG2
      IDFDE =IVF   +NPG2*NNO
      IDFDK =IDFDE +NPG2*NNO
C
      CALL JEVECH('PGEOMER','L',IGEOM )
      CALL JEVECH('PMATERC','L',IMATE )
      CALL JEVECH('PTEMPSR','L',ITEMPS)
      CALL JEVECH('PTEMPER','L',ITEMP )
      CALL JEVECH('PTEMPEI','L',ITEMPI)
      CALL JEVECH('PMATTTR','E',IMATTT)
C
      CALL NTFCMA (ZI(IMATE),IFON)
      DO 101 KP=1,NPG2
        K=(KP-1)*NNO
        CALL DFDM2D ( NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IDFDK+K),
     &                ZR(IGEOM),DFDX,DFDY,POIDS )
        R = 0.D0
        TPG = 0.D0
        DO 102 I = 1, NNO
        R = R + ZR(IGEOM+2*(I-1))*ZR(IVF+K+I-1)
        TPG =TPG + ZR(ITEMPI+I-1)*ZR(IVF+K+I-1)
102     CONTINUE
        IF ( NOMTE(3:4) .EQ. 'AX' ) POIDS = POIDS*R
C
        CALL RCFODE(IFON(2), TPG, ALPHA, XKPT)
C
        IJ = IMATTT - 1
         DO 103 I=1,NNO
C
           DO 103 J=1,I
             IJ = IJ + 1
             ZR(IJ) = ZR(IJ) + POIDS*(
     &               ALPHA*(DFDX(I)*DFDX(J)+DFDY(I)*DFDY(J)) )
C
103      CONTINUE
101   CONTINUE
C
C FIN ------------------------------------------------------------------
      END
