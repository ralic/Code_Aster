       SUBROUTINE METAU1(OPTION,NOMTE,IRET)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 28/08/2006   AUTEUR CIBHHPD L.SALMONA 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C ======================================================================
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*16 OPTION,NOMTE
      INTEGER      IRET
C ......................................................................
C    - FONCTION REALISEE:  CALCUL DES VECTEURS ELEMENTAIRES
C                          OPTION : 'CHAR_MECA_TEMP_Z  '
C ......................................................................
C  IN  OPTION K16 : NOM DE L OPTION (CHAR_MECA_TEMP_Z)
C  IN  NOMTE  K16 : NOM DU TYPE D ELEMENT
C  OUT IRET   I   : =1 PRESENCE DE METALLURGIE
C                   =0 PAS DE METALLURGIE

      PARAMETER (NBRES=6)

      CHARACTER*8  NOMRES(NBRES),ACIER(4),ZIRC(2)
      CHARACTER*2  CODRET(NBRES)
      REAL*8       TTRG,VK3AL,VALRES(NBRES),COEF1,COEF2,EPSTH
      REAL*8       DFDX(9),DFDY(9),TPG,POIDS,R,PHASPG(7)
      INTEGER      NNO,KP,NPG1,I,ITEMPE,IVECTU,ITREF,NZ,JTAB(7),L
      INTEGER      IRE1,IRE2
      LOGICAL      LTEATT,LACIER
      INTEGER IPOIDS,IVF,IDFDE,IGEOM,IMATE,NDIM,NNOS,JGANO

C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------

      DATA ACIER /'PFERRITE','PPERLITE','PBAINITE','PMARTENS'/
      DATA ZIRC /'ALPHPUR','ALPHBETA'/


      IRET=1
      LACIER=.FALSE.

      CALL RCVARC(' ',ACIER(1),'+','RIGI',1,1,RBID,IRE1)
      IF (IRE1.EQ.1) THEN
        CALL RCVARC(' ',ZIRC(1),'+','RIGI',1,1,RBID,IRE2)
        IF (IRE2.EQ.1)  THEN
          IRET=0
          GOTO 9999
        ELSE
          NZ=2
        ENDIF
      ELSE
          NZ=4
          LACIER=.TRUE.
      ENDIF

      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG1,IPOIDS,IVF,IDFDE,JGANO)


      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PMATERC','L',IMATE)

      MATER = ZI(IMATE)

      NOMRES(1) = 'E'
      NOMRES(2) = 'NU'
      NOMRES(3) = 'F_ALPHA'
      NOMRES(4) = 'C_ALPHA'
      NOMRES(5) = 'PHASE_REFE'
      NOMRES(6) = 'EPSF_EPSC_TREF'
      CALL JEVECH('PTEREF','L',ITREF)
      CALL JEVECH('PTEMPER','L',ITEMPE)


      CALL JEVECH('PVECTUR','E',IVECTU)

      DO 40 KP = 1,NPG1
        K = (KP-1)*NNO
                
        CALL DFDM2D(NNO,KP,IPOIDS,IDFDE,ZR(IGEOM),DFDX,DFDY,POIDS)
        R = 0.D0
        TPG = 0.D0
        TTRG = 0.D0
C    RECUPERATION DES PHASES METALLURGIQUES
          DO 5 L=1,NZ
            IF (LACIER) THEN
              CALL RCVARC(' ',ACIER(L),'+','RIGI',KP,1,
     &            PHASPG(L),IRE1)
            ELSE
              CALL RCVARC(' ',ZIRC(L),'+','RIGI',KP,1,
     &            PHASPG(L),IRE1)
            ENDIF
    5     CONTINUE                          

        DO 10 I = 1,NNO
          R = R + ZR(IGEOM+2* (I-1))*ZR(IVF+K+I-1)
          TPG = TPG + ZR(ITEMPE+I-1)*ZR(IVF+K+I-1)
   10   CONTINUE
        
   
        TTRG = TPG - ZR(ITREF)
        CALL RCVALA(MATER,' ','ELAS_META',1,'TEMP',TPG,6,NOMRES,VALRES,
     &              CODRET,'FM')
        VK3AL = VALRES(1)/ (1.D0-2.D0*VALRES(2))
        IF (LTEATT(' ','AXIS','OUI')) THEN
          POIDS = POIDS*R
          DO 20 I = 1,NNO
            K = (KP-1)*NNO
            DFDX(I) = DFDX(I) + ZR(IVF+K+I-1)/R
   20     CONTINUE
        END IF

        ZALPHA=0.D0
        DO 25 I=1,NZ
          ZALPHA=ZALPHA+PHASPG(I)
25      CONTINUE

        COEF1 = (1.D0-ZALPHA)* (VALRES(4)*TTRG- (1-VALRES(5))*VALRES(6))
        COEF2 = ZALPHA* (VALRES(3)*TTRG+VALRES(5)*VALRES(6))
        EPSTH = COEF1 + COEF2
        POIDS = POIDS*VK3AL*EPSTH
        DO 30 I = 1,NNO
          K = (KP-1)*NNO
          ZR(IVECTU+2*I-2) = ZR(IVECTU+2*I-2) + POIDS*DFDX(I)
          ZR(IVECTU+2*I-1) = ZR(IVECTU+2*I-1) + POIDS*DFDY(I)
   30   CONTINUE
   40 CONTINUE
9999  CONTINUE
      END
