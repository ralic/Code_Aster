      SUBROUTINE TE0223 ( OPTION , NOMTE )
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*16        OPTION , NOMTE
C ......................................................................
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
C    - FONCTION REALISEE:  CALCUL DES VECTEURS ELEMENTAIRES
C                          COQUE 1D
C                          OPTION : 'CHAR_MECA_FRCO2D  '
C                          ELEMENT: MECXSE3,METCSE3,METDSE3
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
C
      CHARACTER*24    CARAC,FF
      CHARACTER*8     ELREFE
      REAL*8          POIDS,R,FX,FY,MZ,F1,F3,M2,NX,NY,COUR,DFDX(3)
      INTEGER         NNO,NDDL,KP,NPG,ICARAC,IFF,IPOIDS,IVF,IDFDE,IGEOM
      INTEGER         IVECTU,K,I,L,IFORC
      LOGICAL         GLOBAL
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
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PFRCO2D','L',IFORC)
      CALL JEVECH('PVECTUR','E',IVECTU)
      NDDL = 3
      GLOBAL = ABS(ZR(IFORC+5)).LT.1.D-3
C
      DO 101 KP=1,NPG
        K = (KP-1)*NNO
        CALL DFDM1D (NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IGEOM),DFDX,
     &               COUR,POIDS,NX,NY)
           R = 0.D0
           FX= 0.D0
           FY= 0.D0
           MZ= 0.D0
           DO 102 I=1,NNO
             L = (KP-1) * NNO + I
             IF ( GLOBAL ) THEN
               FX = FX + ZR(IFORC-1+6*(I-1)+1)*ZR(IVF+L-1)
               FY = FY + ZR(IFORC-1+6*(I-1)+2)*ZR(IVF+L-1)
               MZ = MZ + ZR(IFORC-1+6*(I-1)+5)*ZR(IVF+L-1)
             ELSE
               F1 = ZR(IFORC+6*(I-1)  )
C-----------------------------------------------------
C  LE SIGNE AFFECTE A F3 A ETE CHANGE PAR AFFE_CHAR_MECA SI PRES
C  POUR RESPECTER LA CONVENTION
C      UNE PRESSION POSITIVE PROVOQUE UN GONFLEMENT
C-----------------------------------------------------
               F3 = ZR(IFORC+6*(I-1)+2)
               M2 = ZR(IFORC+6*(I-1)+3)
               FX = FX + (NX*F3 -NY*F1)*ZR(IVF+L-1)
               FY = FY + (NY*F3 +NX*F1)*ZR(IVF+L-1)
               MZ = MZ + M2*ZR(IVF+L-1)
             ENDIF
             R = R + ZR(IGEOM+2*(I-1))*ZR(IVF+L-1)
102        CONTINUE
        IF ( NOMTE(3:4) .EQ. 'CX' ) THEN
           POIDS = POIDS * R
        ENDIF
        DO 103 I=1,NNO
          L = (KP-1)*NNO+I
          ZR(IVECTU+NDDL*(I-1))   = ZR(IVECTU+NDDL*(I-1))
     &                              + FX*ZR(IVF+L-1)*POIDS
          ZR(IVECTU+NDDL*(I-1)+1) = ZR(IVECTU+NDDL*(I-1)+1)
     &                              + FY*ZR(IVF+L-1)*POIDS
          ZR(IVECTU+NDDL*(I-1)+2) = ZR(IVECTU+NDDL*(I-1)+2)
     &                              + MZ*ZR(IVF+L-1)*POIDS
103     CONTINUE
101   CONTINUE
      END
