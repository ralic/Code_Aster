      SUBROUTINE TE0251 ( OPTION , NOMTE )
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
C    - FONCTION REALISEE:  CALCUL DES MATRICES TANGENTES ELEMENTAIRES
C                          OPTION : 'MTAN_THER_FLUXNL'
C                          ELEMENTS DE FACE 2D
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C
C ......................................................................
C
      CHARACTER*24       CARAC,FF
      CHARACTER*8        ELREFE
      REAL*8             POIDS,R,NX,NY,THETA,ALPHAP,RBID,TPG
      REAL*8             MRIGT(9,9),COORSE(18)
      INTEGER            NNO,KP,NPG,ICARAC,IFF,IPOIDS,IVF,IDFDE
      INTEGER            IMATTT,K,I,J,IJ,L,LI,LJ
      INTEGER            C(6,9),ISE,NSE,NNOP2
      INTEGER            IGEOM,IFLUX,ITEMPI,ITEMPS
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
      CALL JEVECH('PFLUXNL','L',IFLUX)
      CALL JEVECH('PTEMPEI','L',ITEMPI)
      CALL JEVECH('PTEMPSR','L',ITEMPS)
      CALL JEVECH('PMATTTR','E',IMATTT)
C
      IF ( ZK8(IFLUX)(1:7) .EQ. '&FOZERO' ) GOTO 999
      THETA  = ZR(ITEMPS+2)
C
      CALL CONNEC ( NOMTE, ZR(IGEOM), NSE, NNOP2, C )
C
      DO 11 I=1,NNOP2
         DO 11 J=1,NNOP2
           MRIGT(I,J)=0.D0
11    CONTINUE
C
C --- CALCUL ISO-P2 : BOUCLE SUR LES SOUS-ELEMENTS -------

      DO 100 ISE=1,NSE

        DO 106 I=1,NNO
          DO 106 J=1,2
             COORSE(2*(I-1)+J) = ZR(IGEOM-1+2*(C(ISE,I)-1)+J)
106     CONTINUE
C
        DO 101 KP=1,NPG
          K = (KP-1)*NNO
          CALL VFF2DN (NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),COORSE,NX,NY,
     &                 POIDS)
          R   = 0.D0
          TPG = 0.D0
          DO 102 I=1,NNO
            L = (KP-1)*NNO+I
            R =   R +       COORSE(2*(I-1)+1) * ZR(IVF+L-1)
            TPG = TPG + ZR(ITEMPI-1+C(ISE,I)) * ZR(IVF+L-1)
 102      CONTINUE
          IF ( NOMTE(3:4) .EQ. 'AX' ) POIDS = POIDS*R
          CALL FODERI (ZK8(IFLUX),TPG,RBID,ALPHAP)
          IJ = IMATTT - 1
          DO 103 I=1,NNO
            LI = IVF+(KP-1)*NNO+I-1
CCDIR$ IVDEP
            DO 103 J=1,I
              LJ = IVF+(KP-1)*NNO+J-1
              IJ = IJ + 1
              MRIGT(C(ISE,I),C(ISE,J)) = MRIGT(C(ISE,I),C(ISE,J))
     &         - POIDS* THETA* ALPHAP* ZR(LI)* ZR(LJ)
 103      CONTINUE
 101    CONTINUE
 100  CONTINUE
C
C MISE SOUS FORME DE VECTEUR
C
      IJ = IMATTT-1
      DO 107 I=1,NNOP2
         DO 107 J=1,I
           IJ = IJ + 1
           ZR(IJ)=MRIGT(I,J)
107   CONTINUE
999   CONTINUE
      END
