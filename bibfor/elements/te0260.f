      SUBROUTINE TE0260 ( OPTION , NOMTE )
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
C    - FONCTION REALISEE:  CALCUL DES MATRICES ELEMENTAIRES
C                          OPTION : 'RIGI_THER'
C                          ELEMENTS FOURIER
C
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
C
      CHARACTER*24       CARAC,FF
      CHARACTER*8        ELREFE
      CHARACTER*2        CODRET
      REAL*8             DFDR(9),DFDZ(9),POIDS,R,THETA, VALRES
      INTEGER            NNO,KP,NPG1,I,J,K,ITEMPS,IMATTT
      INTEGER            ICARAC,IFF,IPOIDS,IVF,IDFDE,IDFDK,IGEOM,IMATE
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

      CARAC='&INEL.'//ELREFE//'.CARAC'
      CALL JEVETE(CARAC,'L',ICARAC)
      NNO  = ZI(ICARAC)
      NPG1 = ZI(ICARAC+2)
C
      FF   ='&INEL.'//ELREFE//'.FF'
      CALL JEVETE(FF,'L',IFF)
      IPOIDS=IFF
      IVF   =IPOIDS+NPG1
      IDFDE =IVF   +NPG1*NNO
      IDFDK =IDFDE +NPG1*NNO
C
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PHARMON','L',IHARM)
      NH = ZI(IHARM)
      IF(NH.EQ.-1) THEN
          CALL UTMESS('F','TE0260','NE PAS UTILISER THER_LINEAIRE '//
     +    'AVEC DES ELEMENTS DE FOURIER MAIS LES CMDES DEVELOPPEES')
      ENDIF
      XH = DBLE(NH)
      XH2 = XH*XH
      CALL JEVECH('PMATERC','L',IMATE)
      CALL JEVECH('PMATTTR','E',IMATTT)
      CALL JEVECH('PTEMPSR','L',ITEMPS)
      THETA = ZR(ITEMPS+2)
C
      CALL RCVALA ( ZI(IMATE),'THER',1,'INST',ZR(ITEMPS),1,'LAMBDA',
     &              VALRES, CODRET, 'FM')
C
      DO 101 KP=1,NPG1
         K = (KP-1)*NNO
         CALL DFDM2D ( NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IDFDK+K),
     &                 ZR(IGEOM),DFDR,DFDZ,POIDS )
C
         R = 0.D0
         DO 102 I=1,NNO
            R = R + ZR(IGEOM+2*(I-1))*ZR(IVF+K+I-1)
102      CONTINUE
         R2 = R*R
         POIDS = POIDS*R
C
         IJ = IMATTT - 1
         DO 103 I=1,NNO
C
          DO 103 J=1,I
            WIJ = ZR(IVF+K+I-1) * ZR(IVF+K+J-1)
            IJ = IJ + 1
            ZR(IJ) = ZR(IJ) + POIDS * VALRES * THETA
     &              * ( DFDR(I)*DFDR(J) + DFDZ(I)*DFDZ(J) + XH2*WIJ/R2)
103     CONTINUE
101   CONTINUE
      END
