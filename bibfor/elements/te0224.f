      SUBROUTINE TE0224 ( OPTION , NOMTE )
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
C                          OPTION : 'CHAR_MECA_FFCO2D  '
C                          ELEMENT: MECXSE3,METCSE3,METDSE3
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
C
      PARAMETER     ( NBRES=3 )
      CHARACTER*24    CARAC,FF
      CHARACTER*8     NOMPAR(NBRES),ELREFE
      REAL*8          VALPAR(NBRES), PRES
      REAL*8          POIDS,R,Z,FX,FY,MZ,F1,F3,M2,NX,NY,DFDX(3),COUR
      INTEGER         NNO,NDDL,KP,NPG,ICARAC,IFF,IPOIDS,IVF,IDFDE,IGEOM
      INTEGER         ITEMPS,IVECTU,K,I,L,IFORC
      LOGICAL         GLOBAL, LOCAPR
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
      CALL JEVECH('PTEMPSR','L',ITEMPS)
      CALL JEVECH('PVECTUR','E',IVECTU)
C
      NOMPAR(1) = 'X'
      NOMPAR(2) = 'Y'
      NOMPAR(3) = 'INST'
      VALPAR(3) = ZR(ITEMPS)
      CALL JEVECH('PFFCO2D','L',IFORC)
      NDDL = 3
C
      GLOBAL = ZK8(IFORC+5) .EQ. 'GLOBAL'
      LOCAPR = ZK8(IFORC+5) .EQ. 'LOCAL_PR'
C
      DO 101 KP=1,NPG
        K = (KP-1)*NNO
        CALL DFDM1D (NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IGEOM),DFDX,
     &               COUR,POIDS,NX,NY)
        R = 0.D0
        Z = 0.D0
        DO 102 I=1,NNO
          L = (KP-1)*NNO+I
          R = R + ZR(IGEOM+2*I-2) * ZR(IVF+L-1)

          Z = Z + ZR(IGEOM+2*I-1) * ZR(IVF+L-1)
102     CONTINUE
        IF ( NOMTE(3:4) .EQ. 'CX' ) POIDS = POIDS*R
        VALPAR(1) = R
        VALPAR(2) = Z
        IF (GLOBAL) THEN
          CALL FOINTE('FM',ZK8(IFORC  ),3,NOMPAR,VALPAR,FX,ICODE)
          CALL FOINTE('FM',ZK8(IFORC+1),3,NOMPAR,VALPAR,FY,ICODE)
          CALL FOINTE('FM',ZK8(IFORC+4),3,NOMPAR,VALPAR,MZ,ICODE)
        ELSE IF (LOCAPR) THEN
          F1 = 0.D0
          CALL FOINTE('FM',ZK8(IFORC+2),3,NOMPAR,VALPAR,PRES,ICODE)
C-----------------------------------------------------
C       LE SIGNE MOINS DE FOR(3,J+1) CORRESPOND A LA CONVENTION :
C          UNE PRESSION POSITIVE PROVOQUE UN GONFLEMENT
          F3 = -PRES
          M2 = 0.D0
C-----------------------------------------------------
          FX=NX*F3-NY*F1
          FY=NY*F3+NX*F1
          MZ=M2
        ELSE
          CALL FOINTE('FM',ZK8(IFORC  ),3,NOMPAR,VALPAR,F1,ICODE)
          CALL FOINTE('FM',ZK8(IFORC+2),3,NOMPAR,VALPAR,F3,ICODE)
          CALL FOINTE('FM',ZK8(IFORC+3),3,NOMPAR,VALPAR,M2,ICODE)
          FX=NX*F3-NY*F1
          FY=NY*F3+NX*F1
          MZ=M2
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
