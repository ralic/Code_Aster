      SUBROUTINE TE0054(OPTION,NOMTE)
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
C.......................................................................
      IMPLICIT REAL*8 (A-H,O-Z)
C
C     BUT: CALCUL DES MATRICES DE MASSE ELEMENTAIRE EN THERMIQUE
C          ELEMENTS ISOPARAMETRIQUES 3D
C
C          OPTION : 'MASS_THER'
C
C     ENTREES  ---> OPTION : OPTION DE CALCUL
C              ---> NOMTE  : NOM DU TYPE ELEMENT
C.......................................................................
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
      CHARACTER*2        CODRET
      CHARACTER*8        ELREFE
      CHARACTER*16       NOMTE, OPTION, PHENOM
      CHARACTER*24       CHVAL, CHCTE
      REAL*8             VALPAR, DFDX(27),DFDY(27),DFDZ(27),POIDS
      INTEGER            IPOIDS,IVF,IDFDE,IDFDN,IDFDK,IGEOM,IMATE
      INTEGER            NNO,KP,NPG2,IJ,I,J,IMATTT,ITEMPS,NBPG(10)
C
      CALL ELREF1(ELREFE)
      CHCTE = '&INEL.'//ELREFE//'.CARACTE'
      CALL JEVETE(CHCTE,'L',JIN)
      NDIM = ZI(JIN+1-1)
      NNO  = ZI(JIN+2-1)
      NBFPG = ZI(JIN+3-1)
C
      DO 111 I = 1,NBFPG
         NBPG(I) = ZI(JIN+3-1+I)
  111 CONTINUE
C
      CHVAL = '&INEL.'//ELREFE//'.FFORMES'
      CALL JEVETE(CHVAL,'L',JVAL)
      IPOIDS = JVAL + (NDIM+1)*NNO*NNO
C
      IF (NOMTE(6:10).EQ.'PYRAM') THEN
           NPG2 = NBPG(1)
      ELSE
           NPG2 = NBPG(2)
           IPOIDS = IPOIDS + NBPG(1) + (NDIM+1)*NNO*NBPG(1)
      ENDIF
C
      IVF    = IPOIDS + NPG2
      IDFDE  = IVF    + NPG2*NNO
      IDFDN  = IDFDE  + 1
      IDFDK  = IDFDN  + 1
C
      IF ((NOMTE(12:13).EQ.'_D'.OR.NOMTE(11:12).EQ.'_D').AND.
     +     NOMTE(6:10).NE.'PYRAM') THEN
        NPG2   = NNO
        IVF    = JVAL
        IDFDE  = IVF    + NPG2*NNO
        IDFDN  = IDFDE  + 1
        IDFDK  = IDFDN  + 1
        IPOIDS = JVAL + (NDIM+1)*NNO*NNO
      ENDIF
C
      CALL JEVECH('PGEOMER','L',IGEOM )
      CALL JEVECH('PMATERC','L',IMATE )
      CALL JEVECH('PTEMPSR','L',ITEMPS)
      CALL JEVECH('PMATTTR','E',IMATTT)
C
      CALL RCCOMA ( ZI(IMATE), 'THER', PHENOM, CODRET )
      IF ( CODRET .NE. 'OK')
     +     CALL UTMESS ('A','TE0054','COMPORTEMENT NON TROUVE')
C
      VALPAR = ZR(ITEMPS)
      DELTAT = ZR(ITEMPS+1)
      CALL RCVALA ( ZI(IMATE),PHENOM, 1, 'INST', VALPAR,
     &                        1, 'RHO_CP', CP, CODRET, 'FM' )
C
C    BOUCLE SUR LES POINTS DE GAUSS
C
      DO 101 KP=1,NPG2
C
        L=(KP-1)*NNO
        K=(KP-1)*NNO*3
        CALL DFDM3D ( NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IDFDN+K),
     &                ZR(IDFDK+K),ZR(IGEOM),DFDX,DFDY,DFDZ,POIDS )
C
        DO 106 I=1,NNO
C
          DO 107 J=1,I
           IJ = (I-1)*I/2 + J
           ZR(IMATTT+IJ-1) = ZR(IMATTT+IJ-1)
     &     +     CP/ DELTAT * POIDS * ZR(IVF+L+I-1) * ZR(IVF+L+J-1)

107       CONTINUE
106     CONTINUE
C
101   CONTINUE
C
      END
