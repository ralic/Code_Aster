      SUBROUTINE TE0077 ( OPTION , NOMTE )
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
C    - FONCTION REALISEE:  CALCUL DES MATRICES ELEMENTAIRES
C                          OPTION : 'MASS_THER'
C
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
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
      CHARACTER*16       PHENOM
      CHARACTER*24       CARAC, FF
      CHARACTER*8        ELREFE
      REAL*8             VALRES, DFDX(9), DFDY(9), POIDS, R, CP
      REAL*8             MT(9,9),COORSE(18)
      INTEGER            NNO,KP,NPG1,NPG2,NPG3,I,J,K,IJ,ITEMPS,IMATTT
      INTEGER            C(6,9),ISE,NSE,NNOP2
      INTEGER            ICARAC,IFF,IPOIDS,IVF,IDFDE,IDFDK,IGEOM,IMATE
C
      CALL ELREF1(ELREFE)

      CARAC='&INEL.'//ELREFE//'.CARAC'
      CALL JEVETE(CARAC,'L',ICARAC)
      NNO  = ZI(ICARAC)
      NPG1 = ZI(ICARAC+2)
      NPG2 = ZI(ICARAC+3)
      NPG3 = ZI(ICARAC+4)
C
      CALL JEVECH('PGEOMER','L',IGEOM )
      CALL JEVECH('PMATERC','L',IMATE )
      CALL JEVECH('PTEMPSR','L',ITEMPS)
      CALL JEVECH('PMATTTR','E',IMATTT)
      DELTAT = ZR(ITEMPS+1)
C
      CALL RCCOMA ( ZI(IMATE), 'THER', PHENOM, CODRET )
      IF ( PHENOM .EQ. 'THER') THEN
        CALL RCVALA ( ZI(IMATE), PHENOM, 1, 'INST', ZR(ITEMPS),
     &                              1, 'RHO_CP', CP, CODRET, 'FM' )
      ELSEIF ( PHENOM .EQ. 'THER_ORTH') THEN
        CALL RCVALA ( ZI(IMATE), PHENOM, 1, 'INST', ZR(ITEMPS),
     &                              1, 'RHO_CP', CP, CODRET, 'FM' )
      ELSE
        CALL UTMESS ('F','TE0077','COMPORTEMENT NON TROUVE')
      ENDIF
C
C
      IF (NOMTE(6:6).NE.'L') THEN

C     CALCUL NON LUMPE : 2EME FAMILLE DE PTS DE GAUSS
C     ----------------
        FF   ='&INEL.'//ELREFE//'.FF'
        CALL JEVETE(FF,'L',IFF)
        IPOIDS=IFF   +NPG1*(1+3*NNO)
        IVF   =IPOIDS+NPG2
        IDFDE =IVF   +NPG2*NNO
        IDFDK =IDFDE +NPG2*NNO
C
        DO 101 KP=1,NPG2
          K=(KP-1)*NNO
          CALL DFDM2D ( NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IDFDK+K),
     &                  ZR(IGEOM),DFDX,DFDY,POIDS )
          IF ( NOMTE(3:4) .EQ. 'AX' ) THEN
             R = 0.D0
             DO 102 I=1,NNO
               R = R + ZR(IGEOM+2*(I-1))*ZR(IVF+K+I-1)
102          CONTINUE
             POIDS = POIDS*R
          ENDIF
          IJ = IMATTT - 1
          DO 103 I=1,NNO
C
            DO 103 J=1,I
              IJ = IJ + 1
              ZR(IJ) = ZR(IJ) + POIDS * CP/DELTAT
     &                        * ZR(IVF+K+I-1) * ZR(IVF+K+J-1)
103       CONTINUE
101     CONTINUE

      ELSE

C     CALCUL LUMPE : 3EME FAMILLE DE PTS DE GAUSS
C     ------------
C  CALCUL ISO-P2 : ELTS P2 DECOMPOSES EN SOUS-ELTS LINEAIRES

        FF   ='&INEL.'//ELREFE//'.FF'
        CALL JEVETE(FF,'L',IFF)
        IPOIDS=IFF   +(NPG1+NPG2)*(1+3*NNO)
        IVF   =IPOIDS+NPG3
        IDFDE =IVF   +NPG3*NNO
        IDFDK =IDFDE +NPG3*NNO
C
        CALL CONNEC ( NOMTE, ZR(IGEOM), NSE, NNOP2, C )

        DO 10 I=1,NNOP2
           DO 10 J=1,NNOP2
              MT(I,J)=0.D0
10      CONTINUE

C BOUCLE SUR LES SOUS-ELEMENTS

        DO 200 ISE=1,NSE

           DO 205 I=1,NNO
             DO 205 J=1,2
                COORSE(2*(I-1)+J) = ZR(IGEOM-1+2*(C(ISE,I)-1)+J)
205        CONTINUE

           DO 201 KP=1,NPG3
             K=(KP-1)*NNO
             CALL DFDM2D ( NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IDFDK+K),
     &                     COORSE,DFDX,DFDY,POIDS )
             IF ( NOMTE(3:4) .EQ. 'AX' ) THEN
                R = 0.D0
                DO 202 I=1,NNO
                  R = R + COORSE(2*(I-1)+1)*ZR(IVF+K+I-1)
202             CONTINUE

                POIDS = POIDS*R
                IF (R.EQ.0.D0) THEN
                  CALL UTMESS ('F','TE0077','ON NE PEUT PAS AFFECTER'//
     &                         ' LA MODELISATION "AXIS_DIAG"'//
     &                         ' AUX ELEMENTS DE L''AXE')
                ENDIF
             ENDIF

             DO 203 I=1,NNO
               DO 203 J=1,NNO
                 MT(C(ISE,I),C(ISE,J)) = MT(C(ISE,I),C(ISE,J))
     &              + POIDS * CP/DELTAT * ZR(IVF+K+I-1) * ZR(IVF+K+J-1)
203          CONTINUE
201        CONTINUE

200     CONTINUE

        IJ = IMATTT-1
        DO 206 I=1,NNOP2
           DO 206 J=1,I
             IJ = IJ +1
             ZR(IJ)=MT(I,J)
206     CONTINUE

      ENDIF

      END
