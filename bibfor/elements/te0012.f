      SUBROUTINE TE0012 ( OPTION , NOMTE )
      IMPLICIT   NONE
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
C     BUT: CALCUL DES MATRICES DE MASSE ELEMENTAIRES EN MECANIQUE
C          ELEMENTS ISOPARAMETRIQUES 3D
C
C          OPTION : 'MASS_MECA'
C          OPTION : 'MASS_MECA_DIAG'
C          OPTION : 'M_GAMMA'
C          OPTION : 'ECIN_ELEM_DEPL'
C
C     ENTREES  ---> OPTION : OPTION DE CALCUL
C              ---> NOMTE  : NOM DU TYPE ELEMENT
C.......................................................................
C
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
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
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      CHARACTER*2   CODRET
      CHARACTER*8   ELREFE
      CHARACTER*16  NOMTE,OPTION,PHENOM
      CHARACTER*24  CHVAL,CHCTE
      REAL*8        A(3,3,27,27), MATP(27,27), MATV(378)
      REAL*8        DFDX(27),DFDY(27),DFDZ(27),TPG,POIDS,RHO
      INTEGER       IPOIDS,IVF,IDFDE,IDFDN,IDFDK,IGEOM,IMATE
      INTEGER       NNO,KP,I,J,K,IMATUU,ITEMPE,NBPG(10),IACCE,IVECT
      INTEGER       IJKL,IK,L,JIN,NBFPG,JVAL,NDIM,NPG,NDDL,NVEC
      INTEGER       IDIAG
      INTEGER       IVITE, IECIN, IFREQ
      REAL*8        TRACE, ALFA, SOMME, WGT
      REAL*8        MASVIT(27)
      REAL*8        R8DOT
C.......................................................................
C
      CALL ELREF1(ELREFE)
      CHCTE = '&INEL.'//ELREFE//'.CARACTE'
      CALL JEVETE(CHCTE,'L',JIN)
      NDIM = ZI(JIN+1-1)
      NNO  = ZI(JIN+2-1)
      NBFPG = ZI(JIN+3-1)
      DO 111 I = 1,NBFPG
         NBPG(I) = ZI(JIN+3-1+I)
  111 CONTINUE
      NPG = NBPG(1)
      IF(ELREFE.NE.'PYRAM5  '.AND.ELREFE.NE.'PYRAM13 ') THEN
        NPG = NBPG(2)
      ENDIF
      NDDL = 3 * NNO
      NVEC = NDDL * ( NDDL + 1 ) / 2
C
      CHVAL = '&INEL.'//ELREFE//'.FFORMES'
      CALL JEVETE(CHVAL,'L',JVAL)
      IPOIDS = JVAL + (NDIM+1)*NNO*NNO
      IF(ELREFE.NE.'PYRAM5  '.AND.ELREFE.NE.'PYRAM13 ') THEN
        IPOIDS = IPOIDS + NBPG(1) + (NDIM+1)*NNO*NBPG(1)
      ENDIF
      IVF    = IPOIDS + NPG
      IDFDE  = IVF    + NPG*NNO
      IDFDN  = IDFDE  + 1
      IDFDK  = IDFDN  + 1
C
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PMATERC','L',IMATE)
      CALL JEVECH('PTEMPER','L',ITEMPE)
      CALL RCCOMA(ZI(IMATE),'ELAS',PHENOM,CODRET)
C
      DO 113 K=1,3
        DO 113 L=1,3
          DO 113 I=1,NNO
            DO 113 J=1,I
              A(K,L,I,J) = 0.0D0
113   CONTINUE
C
C    BOUCLE SUR LES POINTS DE GAUSS
C
      DO 101 KP=1,NPG
         L = (KP-1)*NNO
         K = (KP-1)*NNO*3
         CALL DFDM3D ( NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IDFDN+K),
     &                 ZR(IDFDK+K),ZR(IGEOM),DFDX,DFDY,DFDZ,POIDS )
C
         TPG = 0.0D0
         DO 102 I=1,NNO
            TPG = TPG + ZR(ITEMPE+I-1) *ZR(IVF+K+I-1)
102      CONTINUE
         CALL RCVALA ( ZI(IMATE),PHENOM,1,'TEMP',TPG,1,'RHO',RHO,
     &                CODRET,'FM')
         DO 106 I=1,NNO
           DO 107 J=1,I
             A(1,1,I,J)=A(1,1,I,J)+RHO*POIDS*ZR(IVF+L+I-1)*ZR(IVF+L+J-1)
107        CONTINUE
106      CONTINUE
101   CONTINUE
C
      DO 108 I=1,NNO
        DO 109 J=1,I
          A(2,2,I,J) = A(1,1,I,J)
          A(3,3,I,J) = A(1,1,I,J)
109     CONTINUE
108   CONTINUE
C
      IF ( OPTION .EQ. 'MASS_MECA' ) THEN
C
         CALL JEVECH('PMATUUR','E',IMATUU)
C
C PASSAGE DU STOCKAGE RECTANGULAIRE (A) AU STOCKAGE TRIANGULAIRE (ZR)
C
         DO 112 K=1,3
            DO 112 L=1,3
               DO 114 I=1,NNO
                  IK = ((3*I+K-4) * (3*I+K-3)) / 2
                  DO 116 J=1,I
                     IJKL = IK + 3 * (J-1) + L
                     ZR(IMATUU+IJKL-1) = A(K,L,I,J)
116              CONTINUE
114            CONTINUE
112      CONTINUE
C
      ELSEIF ( OPTION .EQ. 'MASS_MECA_DIAG' ) THEN
C
         CALL JEVECH('PMATUUR','E',IMATUU)
C
C-- CALCUL DE LA MASSE DE L'ELEMENT
C
         WGT = A(1,1,1,1)
         DO 310 I = 2,NNO
           DO 300 J = 1,I-1
            WGT = WGT + 2 * A(1,1,I,J)
 300       CONTINUE
           WGT = WGT +  A(1,1,I,I)
 310     CONTINUE
C
C-- CALCUL DE LA TRACE EN TRANSLATION SUIVANT X
C
         TRACE = 0.D0
         DO 320 I = 1,NNO
            TRACE = TRACE + A(1,1,I,I)
 320    CONTINUE
C
C-- CALCUL DU FACTEUR DE DIAGONALISATION
C
        ALFA = WGT / TRACE
C
C PASSAGE DU STOCKAGE RECTANGULAIRE (A) AU STOCKAGE TRIANGULAIRE (ZR)
C
        K = 0
        DO 312 J=1,NNO
          DO 311 I=1,3
            K = K + 1
            IDIAG = K*(K+1)/2
            ZR(IMATUU+IDIAG-1) = A(I,I,J,J) * ALFA
 311      CONTINUE
 312    CONTINUE
C
      ELSEIF ( OPTION .EQ. 'M_GAMMA' ) THEN
C
         CALL JEVECH('PDEPLAR','L',IACCE)
         CALL JEVECH('PVECTUR','E',IVECT)
         DO 210 K = 1,NVEC
            MATV(K) = 0.0D0
 210     CONTINUE
         DO 212 K = 1,3
            DO 212 L = 1,3
               DO 214 I = 1,NNO
                  IK = ((3*I+K-4) * (3*I+K-3)) / 2
                  DO 216 J=1,I
                     IJKL = IK + 3 * (J-1) + L
                     MATV(IJKL) = A(K,L,I,J)
 216              CONTINUE
 214           CONTINUE
 212     CONTINUE
         CALL VECMA(MATV,NVEC,MATP,NDDL)
         CALL PMAVEC('ZERO',NDDL,MATP,ZR(IACCE),ZR(IVECT))
C
C OPTION ECIN_ELEM_DEPL : CALCUL DE L'ENERGIE CINETIQUE
C
      ELSEIF ( OPTION .EQ. 'ECIN_ELEM_DEPL') THEN
C
         CALL JEVECH('PDEPLAR','L',IVITE)
         CALL JEVECH('PENERCR','E',IECIN)
         CALL JEVECH('PFREQR','E',IFREQ)

         DO 610 K = 1,NVEC
            MATV(K) = 0.0D0
 610     CONTINUE
         DO 612 K = 1,3
            DO 612 L = 1,3
               DO 614 I = 1,NNO
                  IK = ((3*I+K-4) * (3*I+K-3)) / 2
                  DO 616 J=1,I
                     IJKL = IK + 3 * (J-1) + L
                     MATV(IJKL) = A(K,L,I,J)
 616              CONTINUE
 614           CONTINUE
 612     CONTINUE
         CALL VECMA(MATV,NVEC,MATP,NDDL)
         CALL PMAVEC('ZERO',NDDL,MATP,ZR(IVITE),MASVIT)
C
      ZR(IECIN)=.5D0*R8DOT(NDDL,ZR(IVITE),1,MASVIT,1)*ZR(IFREQ)
C
      ELSE
         CALL UTMESS ('F' , 'TE0012' , 'OPTION NON TRAITEE')
      ENDIF
C
      END
