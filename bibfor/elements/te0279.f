      SUBROUTINE TE0279(OPTION,NOMTE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 11/04/2002   AUTEUR CIBHHLV L.VIVAN 
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
      IMPLICIT NONE
      CHARACTER*16       NOMTE,OPTION
C ----------------------------------------------------------------------
C
C    - FONCTION REALISEE:  CALCUL DES MATRICES TANGENTES ELEMENTAIRES
C                          OPTION : 'MTAN_RIGI_MASS'
C                          ELEMENTS 3D ISO PARAMETRIQUES LUMPES
C
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C
C THERMIQUE NON LINEAIRE
C
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C
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
C
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      CHARACTER*8        ELREFE
      REAL*8             RHOCP,LAMBDA,THETA,DELTAT,KHI,TPGI
      REAL*8             DFDX(27),DFDY(27),DFDZ(27),POIDS,R8BID
      INTEGER            IPOIDS,IVF,IDFDE,IDFDN,IDFDK,IGEOM,IMATE
      INTEGER            NNO,KP,NPG,I,J,IJ,K,L,IMATTT,ITEMPS,IFON(3)
      INTEGER            ICOMP, ITEMPI
      INTEGER            NDIM,JIN,JVAL
C DEB ------------------------------------------------------------------
      CALL ELREF1(ELREFE)

      CALL JEVETE('&INEL.'//ELREFE//'.CARACTE','L',JIN)
      NDIM = ZI(JIN+1-1)
      NNO  = ZI(JIN+2-1)
      NPG  = ZI(JIN+3)
C
      CALL JEVETE('&INEL.'//ELREFE//'.FFORMES','L',JVAL)
      IPOIDS = JVAL + (NDIM+1)*NNO*NNO
      IVF    = IPOIDS + NPG
      IDFDE  = IVF    + NPG*NNO
      IDFDN  = IDFDE  + 1
      IDFDK  = IDFDN  + 1
C
      CALL JEVECH('PGEOMER','L',IGEOM )
      CALL JEVECH('PMATERC','L',IMATE )
      CALL JEVECH('PTEMPSR','L',ITEMPS)
      CALL JEVECH('PTEMPEI','L',ITEMPI)
      CALL JEVECH('PCOMPOR','L',ICOMP)
      CALL JEVECH('PMATTTR','E',IMATTT)
C
      IF ( (ZK16(ICOMP)(1:5).EQ.'SECH_')     .OR.
     &     (ZK16(ICOMP)(1:9).EQ.'THER_HYDR'))     THEN
        CALL UTMESS('F','TE0279','PAS D ELEMENTS LUMPES POUR'//
     &              'HYDRATATION ET SECHAGE')
      ENDIF
C
      DELTAT= ZR(ITEMPS+1)
      THETA = ZR(ITEMPS+2)
      KHI   = ZR(ITEMPS+3)
      CALL NTFCMA(ZI(IMATE),IFON)
C
      DO 101 KP=1,NPG
        K = (KP-1)*NNO*3
        L = (KP-1)*NNO
        CALL DFDM3D ( NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IDFDN+K),
     &                ZR(IDFDK+K),ZR(IGEOM),DFDX,DFDY,DFDZ,POIDS )
        TPGI = 0.D0
        DO 102 I=1,NNO
          TPGI = TPGI + ZR(ITEMPI+I-1) * ZR(IVF+L+I-1)
102     CONTINUE
        CALL RCFODE (IFON(2),TPGI,LAMBDA,R8BID)
C
        DO 106 I=1,NNO
CCDIR$ IVDEP
          DO 106 J=1,I
            IJ = (I-1)*I/2 + J
            ZR(IMATTT+IJ-1) = ZR(IMATTT+IJ-1) + POIDS*
     &              THETA*LAMBDA*
     &              (DFDX(I)*DFDX(J)+DFDY(I)*DFDY(J)+DFDZ(I)*DFDZ(J))
106     CONTINUE
101   CONTINUE
C
      IF ((NOMTE(12:13).EQ.'_D'.OR.NOMTE(11:12).EQ.'_D').AND.
     +     NOMTE(6:10).NE.'PYRAM') THEN  
        NPG    = NNO       
        IVF    = JVAL
        IDFDE  = IVF    + NPG*NNO
        IDFDN  = IDFDE  + 1
        IDFDK  = IDFDN  + 1
        IPOIDS = JVAL + (NDIM+1)*NNO*NNO
      ENDIF
C
      DO 201 KP=1,NPG
        K = (KP-1)*NNO*3
        L = (KP-1)*NNO
        TPGI = 0.D0
        DO 202 I=1,NNO
          TPGI = TPGI + ZR(ITEMPI+I-1) * ZR(IVF+L+I-1)
202     CONTINUE
        CALL RCFODE (IFON(1),TPGI,R8BID, RHOCP)
C
        DO 206 I=1,NNO
CCDIR$ IVDEP
          DO 206 J=1,I
            IJ = (I-1)*I/2 + J
            ZR(IMATTT+IJ-1) = ZR(IMATTT+IJ-1) + POIDS*
     &            KHI*RHOCP*ZR(IVF+L+I-1)*ZR(IVF+L+J-1)/DELTAT
206     CONTINUE
201   CONTINUE
C
C FIN ------------------------------------------------------------------
      END
