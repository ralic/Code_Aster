      SUBROUTINE TE0526 ( OPTION , NOMTE )
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
C                          ELEMENTS DE FACE 3D
C                            -  PROBLEME  DE  TRANSPORT  -
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
      REAL*8             NX,NY,NZ,SX(9,9),SY(9,9),SZ(9,9),JAC,PN,PNP1
      REAL*8             THETA,TPG,TPG0,ALPHA,ALPHA0,ALPHAP,RBID,RHO
      REAL*8             GAMMA
      INTEGER            JIN,NDIM,NNO,NPG1,JVAL,IPOIDS,IVF,IDFDX,IDFDY
      INTEGER            IGEOM,IFLUX,ITEMP,ITEMPS,INO,JNO,IJ,LI
      INTEGER            ITEMPI,ILAGRM,ILAGRP,IVECTT,IVERES
      INTEGER            I,J,KP,KDEC,LDEC,IDEC,JDEC
      CHARACTER*8        COEF
      CHARACTER*8        ELREFE
      CHARACTER*19       CH19
      CHARACTER*8        K8BID
      CALL ELREF1(ELREFE)
      CALL JEMARQ()
C
      CALL JEVETE('&INEL.'//ELREFE//'.CARACTE',' ',JIN)
      NDIM = ZI(JIN+1-1)
      NNO  = ZI(JIN+2-1)
      NPG1 = ZI(JIN+4-1)
      CALL JEVETE('&INEL.'//ELREFE//'.FFORMES',' ',JVAL)
      IPOIDS = JVAL + (NDIM+1)*NNO*NNO
      IVF    = IPOIDS + NPG1
      IDFDX  = IVF    + NPG1 * NNO
      IDFDY  = IDFDX  + 1
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
C    CALCUL DES PRODUITS VECTORIELS OMI   OMJ
C
      DO 1 INO = 1,NNO
        I = IGEOM + 3*(INO-1) -1
        DO 2 JNO = 1,NNO
          J = IGEOM + 3*(JNO-1) -1
          SX(INO,JNO) = ZR(I+2) * ZR(J+3) - ZR(I+3) * ZR(J+2)
          SY(INO,JNO) = ZR(I+3) * ZR(J+1) - ZR(I+1) * ZR(J+3)
          SZ(INO,JNO) = ZR(I+1) * ZR(J+2) - ZR(I+2) * ZR(J+1)
 2      CONTINUE
 1    CONTINUE
C
      DO 101 KP=1,NPG1
        KDEC = (KP-1)*NNO*NDIM
        LDEC = (KP-1)*NNO
        NX = 0.0D0
        NY = 0.0D0
        NZ = 0.0D0
C
C   CALCUL DE LA NORMALE AU POINT DE GAUSS KP
C
        DO 102 I=1,NNO
          IDEC = (I-1)*NDIM
          DO 102 J=1,NNO
            JDEC = (J-1)*NDIM
            NX = NX+ ZR(IDFDX+KDEC+IDEC)* ZR(IDFDY+KDEC+JDEC)* SX(I,J)
            NY = NY+ ZR(IDFDX+KDEC+IDEC)* ZR(IDFDY+KDEC+JDEC)* SY(I,J)
            NZ = NZ+ ZR(IDFDX+KDEC+IDEC)* ZR(IDFDY+KDEC+JDEC)* SZ(I,J)
 102    CONTINUE
C
C   CALCUL DU JACOBIEN AU POINT DE GAUSS KP
C
        JAC = SQRT(NX*NX + NY*NY + NZ*NZ)
C
        TPG  = 0.D0
        DO 103 I=1,NNO
          TPG  = TPG  + ZR(ITEMPI+I-1) * ZR(IVF+LDEC+I-1)
 103    CONTINUE
        CALL FODERI (COEF,TPG ,ALPHA ,ALPHAP)
C
        DO 104 I=1,NNO
          ZR(IVERES+I-1) = ZR(IVERES+I-1) + ZR(IPOIDS+KP-1)*JAC*
     &                (ALPHA - ALPHAP*TPG)*ZR(IVF+LDEC+I-1)
 104    CONTINUE
 101  CONTINUE
 999  CONTINUE
C FIN ------------------------------------------------------------------
      CALL JEDEMA()
      END
