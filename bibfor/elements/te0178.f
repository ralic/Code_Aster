      SUBROUTINE TE0178 ( OPTION , NOMTE )
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
C ......................................................................
C    - FONCTION REALISEE:  CALCUL DES MATRICES ELEMENTAIRES
      IMPLICIT REAL*8 (A-H,O-Z)
C                          D'AMORTISSEMENT ACOUSTIQUE SUR DES ARETES
C                          D'ELEMENTS 2D
C                          OPTION : 'AMOR_ACOU'
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
C
      COMPLEX*16         RHOSZ
      CHARACTER*16       OPTION , NOMTE
      CHARACTER*2        CODRET
      CHARACTER*24       CARAC,FF
      CHARACTER*8        ELREFE
      REAL*8             POIDS,R,NX,NY,THETA,RHO
      INTEGER            NNO,KP,NPG,ICARAC,IFF,IPOIDS,IVF,IDFDE,IGEOM
      INTEGER            IMATTT,K,I,J,IJ,L,LI,LJ
      INTEGER            IMATE,IIMPE
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
      NNO=ZI(ICARAC)
      NPG=ZI(ICARAC+2)
C
      FF = '&INEL.'//ELREFE//'.FF'
      CALL JEVETE(FF,'L',IFF)
      IPOIDS=IFF
      IVF   =IPOIDS+NPG
      IDFDE =IVF   +NPG*NNO
C
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PIMPEDC','L',IIMPE )
      CALL JEVECH('PMATERC','L',IMATE )
      CALL JEVECH('PMATTTC','E',IMATTT)
C
      MATER = ZI(IMATE)
      CALL RCVALA(MATER,'FLUIDE',0,' ',R8B,1,'RHO',RHO,CODRET,'FM')
C
      IF(ZC(IIMPE).NE.(0.D0,0.D0)) THEN
         RHOSZ = RHO/ZC(IIMPE)
      ELSE
         GOTO 120
      ENDIF
C
      DO 101 KP=1,NPG
        K = (KP-1)*NNO
        CALL VFF2DN (NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IGEOM),NX,NY,
     &               POIDS)
        IF ( NOMTE(3:4) .EQ. 'AX' ) THEN
           R = 0.D0
           DO 102 I=1,NNO
             L = (KP-1)*NNO+I
             R = R + ZR(IGEOM+2*I-2) * ZR(IVF+L-1)
102        CONTINUE
           POIDS = POIDS*R
        ENDIF
        IJ = IMATTT - 1
        DO 103 I=1,NNO
          LI = IVF+(KP-1)*NNO+I-1
          DO 103 J=1,I
            LJ = IVF+(KP-1)*NNO+J-1
            IJ = IJ + 1
            ZC(IJ) = ZC(IJ) + POIDS* RHOSZ* ZR(LI)* ZR(LJ)
103     CONTINUE
101   CONTINUE
120   CONTINUE
      END
