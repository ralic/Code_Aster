      SUBROUTINE TE0442 ( OPTION , NOMTE )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 29/02/2000   AUTEUR JMBHH01 J.M.PROIX 
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
C                          POUR ELEMENTS INCOMPRESSIBLES
C                          OPTION : 'CHAR_MECA_PRES_F  '
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
C
      PARAMETER         ( NBRES=3 )
      CHARACTER*24       CARAC,FF
      CHARACTER*8        NOMPAR(NBRES)
      REAL*8             VALPAR(NBRES),POIDS,R,TX,TY,NX,NY
      REAL*8             PRES,CISA,Z
      INTEGER            NNO1,NNO2,KP,NPG,ICARAC,IFF,IPOIDS,IVF1,IDFDE1
      INTEGER            ICODE,IGEOM,ITEMPS,IPRES,IVECTU,K,I,L
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
      CALL JEMARQ()
      CARAC='&INEL.'//NOMTE(1:8)//'.CARAC'
      CALL JEVETE(CARAC,' ',ICARAC)
      NNO1=ZI(ICARAC)
      NNO2=ZI(ICARAC+1)
      NPG =ZI(ICARAC+2)
C
      FF   ='&INEL.'//NOMTE(1:8)//'.FF'
      CALL JEVETE(FF,' ',IFF)
      IPOIDS =IFF
      IVF1   =IPOIDS +NPG
      IDFDE1 =IVF1   +NPG*NNO1
C
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PPRESSF','L',IPRES)
      CALL JEVECH('PTEMPSR','L',ITEMPS)
      CALL JEVECH('PVECTUR','E',IVECTU)
C
      NOMPAR(1) = 'X'
      NOMPAR(2) = 'Y'
      NOMPAR(3) = 'INST'
      VALPAR(3) = ZR(ITEMPS)
C
      DO 101 KP=1,NPG
        K = (KP-1)*NNO1
        CALL VFF2DN (NNO1,ZR(IPOIDS+KP-1),ZR(IDFDE1+K),ZR(IGEOM),
     &               NX,NY,POIDS)
        R = 0.D0
        Z = 0.D0
        DO 102 I=1,NNO1
          L = (KP-1)*NNO1+I
          R = R + ZR(IGEOM+2*I-2) * ZR(IVF1+L-1)
          Z = Z + ZR(IGEOM+2*I-1) * ZR(IVF1+L-1)
102     CONTINUE
        IF ( NOMTE(3:4) .EQ. 'AX' ) POIDS = POIDS*R
        VALPAR(1) = R
        VALPAR(2) = Z
        CALL FOINTE('FM',ZK8(IPRES)  ,3,NOMPAR,VALPAR,PRES,ICODE)
        CALL FOINTE('FM',ZK8(IPRES+1),3,NOMPAR,VALPAR,CISA,ICODE)
        TX = -NX*PRES -NY*CISA
        TY = -NY*PRES +NX*CISA
        DO 103 I=1,NNO2
          L = (KP-1)*NNO1+I
          ZR(IVECTU+3*I-3) = ZR(IVECTU+3*I-3) + TX*ZR(IVF1+L-1)*POIDS
          ZR(IVECTU+3*I-2) = ZR(IVECTU+3*I-2) + TY*ZR(IVF1+L-1)*POIDS
          ZR(IVECTU+3*I-1) = 0.D0
103     CONTINUE
        DO 104 I=NNO2+1,NNO1
          L = (KP-1)*NNO1+I
          ZR(IVECTU+NNO2+2*I-2) = ZR(IVECTU+NNO2+2*I-2)
     &                                + TX*ZR(IVF1+L-1)*POIDS
          ZR(IVECTU+NNO2+2*I-1) = ZR(IVECTU+NNO2+2*I-1)
     &                                + TY*ZR(IVF1+L-1)*POIDS
104     CONTINUE
101   CONTINUE
      CALL JEDEMA()
      END
