      SUBROUTINE TE0181 ( OPTION , NOMTE )
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*16        OPTION , NOMTE
C.......................................................................
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
C
C     BUT: CALCUL DES MATRICES DE MASSE ELEMENTAIRE EN ACOUSTIQUE
C          ELEMENTS ISOPARAMETRIQUES 3D
C
C          OPTION : 'MASS_ACOU '
C
C     ENTREES  ---> OPTION : OPTION DE CALCUL
C          ---> NOMTE  : NOM DU TYPE ELEMENT
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
      INTEGER         IFF,IPOIDS,IVF,IDFDE,IDFDN,IDFDK,IGEOM,IMATE
      INTEGER         NNO,KP,NPG2,IJ,I,J,IMATTT, NBPG(10)
      REAL*8          CEL, DFDX(27), DFDY(27), DFDZ(27), POIDS
      CHARACTER*2     CODRET
      CHARACTER*8     ELREFE
      CHARACTER*24    CHVAL, CHCTE
      COMPLEX*16      VALRES
C

      CALL ELREF1(ELREFE)
      CHCTE = '&INEL.'//ELREFE//'.CARACTE'
      CALL JEVETE(CHCTE,'L',JIN)
      NDIM  = ZI(JIN+1-1)
      NNO   = ZI(JIN+2-1)
      NBFPG = ZI(JIN+3-1)
      NDI   = NNO*(NNO+1)/2
      DO 111 I = 1,NBFPG
         NBPG(I) = ZI(JIN+3-1+I)
  111 CONTINUE
      NPG2 = NBPG(2)
C
      CHVAL = '&INEL.'//ELREFE//'.FFORMES'
      CALL JEVETE(CHVAL,'L',JVAL)
      IPOIDS = JVAL + (NDIM+1)*NNO*NNO
      IPOIDS = IPOIDS + NBPG(1) + (NDIM+1)*NNO*NBPG(1)
      IVF    = IPOIDS + NPG2
      IDFDE  = IVF    + NPG2*NNO
      IDFDN  = IDFDE  + 1
      IDFDK  = IDFDN  + 1
C
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PMATERC','L',IMATE)
      CALL JEVECH('PMATTTC','E',IMATTT)
C
      CALL RCVALC ( ZI(IMATE),'FLUIDE',0,' ',R8BID,1,'CELE_C',
     &                                       VALRES,CODRET, 'FM' )
      CEL = DBLE( VALRES )
C
      DO 10 I = 1 , NDI
         ZC(IMATTT-1+I) = (0.0D0,0.0D0)
10    CONTINUE
C
C    BOUCLE SUR LES POINTS DE GAUSS
C
      DO 101 KP=1,NPG2
         L = (KP-1)*NNO
         K = (KP-1)*NNO*3
C
         CALL DFDM3D ( NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IDFDN+K),
     &                 ZR(IDFDK+K),ZR(IGEOM),DFDX,DFDY,DFDZ,POIDS )
C
         DO 106 I=1,NNO
           DO 107 J=1,I
             IJ = (I-1)*I/2 + J
             ZC(IMATTT+IJ-1) = ZC(IMATTT+IJ-1) +
     &                ((1.0D0,0.0D0)/(CEL**2)) * POIDS * ZR(IVF+L+I-1)
     &                 * ZR(IVF+L+J-1)
107        CONTINUE
106      CONTINUE
101   CONTINUE
C
      END
