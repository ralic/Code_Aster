      SUBROUTINE TE0506 ( OPTION , NOMTE )
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
      IMPLICIT NONE
      CHARACTER*16        OPTION , NOMTE
C ......................................................................
C    - FONCTION REALISEE:  CALCUL DES VECTEURS ELEMENTAIRES
C                          OPTION : 'CHAR_THER_FLUTNL'
C
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
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
      CHARACTER*8        COEF,ELREFE
      CHARACTER*19       CH19
      CHARACTER*24       CARAC,FF
      REAL*8             DFDX(9),DFDY(9),POIDS,R,TPG,THETA
      REAL*8             ALPHA,ALPHAP,RBID,NX,NY,GAMMA
      INTEGER            NNO,KP,NPG,I,K,ITEMPS,IVECTT,ITEMP,ITEMPI,IFLUX
      INTEGER            ICARAC,IFF,IPOIDS,IVF,IDFDE,IGEOM,IMATE
      INTEGER            ILAGRM,ILAGRP,IVERES
      CHARACTER*8        K8BID
C
      CALL ELREF1(ELREFE)
      CALL JEMARQ()
C
C
      CARAC='&INEL.'//ELREFE//'.CARAC'
      CALL JEVETE(CARAC,' ',ICARAC)
      NNO=ZI(ICARAC)
      NPG=ZI(ICARAC+2)
C
      FF   ='&INEL.'//ELREFE//'.FF'
      CALL JEVETE(FF,' ',IFF)
      IPOIDS=IFF
      IVF   =IPOIDS+NPG
      IDFDE =IVF   +NPG*NNO
C
      CALL JEVECH('PGEOMER','L',IGEOM )
      CALL JEVECH('PTEMPSR','L',ITEMPS)
      CALL JEVECH('PTEMPER','L',ITEMP )
      CALL JEVECH('PTEMPEI','L',ITEMPI)
      CALL JEVECH('PFLUXNL','L',IFLUX )
      CALL JEVECH('PRESIDU','E',IVERES)
C
      COEF   = ZK8(IFLUX)
      IF ( COEF(1:7) .EQ. '&FOZERO' ) GOTO 999
C
C
      DO 101 KP=1,NPG
        K=(KP-1)*NNO
        CALL VFF2DN ( NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),
     &                ZR(IGEOM),NX,NY,POIDS )
        R      = 0.D0
        TPG    = 0.D0
        DO 102 I=1,NNO
          R      = R      + ZR(IGEOM+2*(I-1)) *ZR(IVF+K+I-1)
          TPG    = TPG    + ZR(ITEMPI+I-1)    *ZR(IVF+K+I-1)
 102    CONTINUE
        CALL FODERI (COEF,TPG ,ALPHA,ALPHAP)
        IF ( NOMTE(3:4) .EQ. 'AX' ) POIDS = POIDS*R
C
C
        DO 103 I=1,NNO
          ZR(IVERES+I-1) = ZR(IVERES+I-1)+
     &             POIDS*ZR(IVF+K+I-1)*(ALPHA - ALPHAP*TPG)
 103    CONTINUE
 101  CONTINUE
 999  CONTINUE
C FIN ------------------------------------------------------------------
      CALL JEDEMA()
      END
