      SUBROUTINE TE0161 ( OPTION , NOMTE )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 06/05/2003   AUTEUR CIBHHPD D.NUNEZ 
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
C    - FONCTION REALISEE:  CALCUL FORCES REPARTIES POUR MECABL2
C                                               ET MECA_POU_D_T_GD
C                          OPTION : 'CHAR_MECA_FR1D1D'
C                          OPTION : 'CHAR_MECA_FF1D1D'
C                          OPTION : 'CHAR_MECA_SR1D1D'
C                          OPTION : 'CHAR_MECA_SF1D1D'
C
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
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
      CHARACTER*24       CARAC,FF
      CHARACTER*8        NOMPAR(4),ELREFE
      CHARACTER*2        CODRET
      INTEGER NNO,KP,I,IVECTU,IPESA,NDDL,NPG,IYTY,NORDRE,LSECT
      INTEGER ICARAC,IFF,IPOIDS,IVF,IGEOM,IMATE,IFORC
      INTEGER ITEMPS,NBPAR,IDEPLA,IDEPLP,K,L,IC,NEU,IRET,NEUM1
      LOGICAL NORMAL
      REAL*8  R8MIN,R8BID,RHO,A,COEF
      REAL*8  S,S2,S3,S4,S5,X(4),C1,C2(3),W(6),U(3),V(3),W2(3)

      INTEGER            IFCX
      CHARACTER*8        NOMPAV(1)
      REAL*8             VALPAV(1),FCX,VITE2,VP(3)
      LOGICAL            OKVENT
C
      CALL ELREF1(ELREFE)
C
      DATA NOMPAR/'X','Y','Z','INST'/
      DATA NOMPAV/'VITE'/
C
      R8MIN = R8MIEM()
C
      IF (NOMTE(1:15).EQ.'MECA_POU_D_T_GD') THEN
        NDDL = 6
      ELSE
        NDDL = 3
      ENDIF
      CARAC='&INEL.'//ELREFE//'.CARAC'
      CALL JEVETE(CARAC,'L',ICARAC)
      NNO  = ZI(ICARAC)
      NPG  = ZI(ICARAC+2)
C
      FF   ='&INEL.'//ELREFE//'.FF'
      CALL JEVETE(FF,'L',IFF)
      IPOIDS=IFF
      IVF   =IPOIDS+NPG
      IYTY  =IVF   +2*NPG*NNO
C
      NORDRE = 3 * NNO
C
      NORMAL = .FALSE.
      CALL JEVECH ('PGEOMER','L',IGEOM)
      CALL JEVECH ('PVECTUR','E',IVECTU)
C
      IF ( OPTION .EQ. 'CHAR_MECA_PESA_R' ) THEN
C          ------------------------------
         CALL JEVECH ('PMATERC','L',IMATE)
         CALL JEVECH ('PPESANR','L',IPESA)
         IF (NOMTE(1:15).EQ.'MECA_POU_D_T_GD') THEN
            CALL RCVALA ( ZI(IMATE),'ELAS',0,' ',R8BID,1,'RHO',RHO,
     &                    CODRET, 'FM' )
            CALL JEVECH('PCAGNPO','L',LSECT)
         ELSE
            CALL RCVALA ( ZI(IMATE),'CABLE',0,' ',R8BID,1,'RHO',RHO,
     &                    CODRET, 'FM' )
            CALL JEVECH('PCACABL','L',LSECT)
         ENDIF
         A = ZR(LSECT)
         C1=A*RHO*ZR(IPESA)
         C2(1)=ZR(IPESA+1)
         C2(2)=ZR(IPESA+2)
         C2(3)=ZR(IPESA+3)
      ENDIF
C
      OKVENT = .FALSE.
      IF ( OPTION .EQ. 'CHAR_MECA_FR1D1D' .OR.
     &     OPTION .EQ. 'CHAR_MECA_SR1D1D' ) THEN
C          ------------------------------
         C1=1.D0

C        POUR LE CAS DU VENT
         CALL TECACH('NNN','PVITER',1,IFORC,IRET)
         IF ( IFORC .NE. 0 ) THEN
           NORMAL = .TRUE.
           OKVENT = .TRUE.
         ELSE
           CALL JEVECH ('PFR1D1D','L',IFORC)
           IF (NOMTE(1:15).EQ.'MECA_POU_D_T_GD') THEN
              NORMAL = ABS(ZR(IFORC+6)) .GT. 1.001D0
           ELSEIF (NOMTE(1:15).EQ.'MECABL2') THEN
              NORMAL = ABS(ZR(IFORC+3)) .GT. 1.001D0
           ENDIF
         ENDIF
      ENDIF
C
      IF ( OPTION .EQ. 'CHAR_MECA_FF1D1D' .OR.
     &     OPTION .EQ. 'CHAR_MECA_SF1D1D' ) THEN
C          ------------------------------
         C1=1.D0
         CALL JEVECH('PFF1D1D','L',IFORC)
         IF (NOMTE(1:15).EQ.'MECA_POU_D_T_GD') THEN
            NORMAL = ZK8(IFORC+6).EQ.'VENT'
         ELSEIF (NOMTE(1:15).EQ.'MECABL2') THEN
            NORMAL = ZK8(IFORC+3).EQ.'VENT'
         END IF
         CALL TECACH ('NNN', 'PTEMPSR', 1, ITEMPS,IRET )
         IF ( ITEMPS .NE. 0 ) THEN
            X(4) = ZR(ITEMPS)
            NBPAR = 4
         ELSE
            NBPAR = 3
         ENDIF
      END IF
C
      IF ( OPTION .EQ. 'CHAR_MECA_SR1D1D' .OR.
     &     OPTION .EQ. 'CHAR_MECA_SF1D1D' ) THEN
C          ------------------------------
       CALL JEVECH('PDEPLMR','L',IDEPLA)
       CALL JEVECH('PDEPLPR','L',IDEPLP)
       DO 110 I=1,3
        W(I)=ZR(IGEOM+I-1)+ZR(IDEPLA-1+I)+ZR(IDEPLP-1+I)
        W(I+3)=ZR(IGEOM+I+2)+ZR(IDEPLA-1+I+NDDL)+ZR(IDEPLP-1+I+NDDL)
        W2(I)=W(I+3)-W(I)
  110  CONTINUE
      ELSE
       DO 111 I=1,3
        W(I)=ZR(IGEOM+I-1)
        W(I+3)=ZR(IGEOM+I+2)
        W2(I)=W(I+3)-W(I)
  111  CONTINUE
      END IF
C
C     --- FORCES REPARTIES PAR VALEURS REELLES---
      IF ( OPTION .EQ. 'CHAR_MECA_FR1D1D' ) THEN
C          ------------------------------
        C2(1)=ZR(IFORC)
        C2(2)=ZR(IFORC+1)
        C2(3)=ZR(IFORC+2)
        IF (NORMAL) THEN
          CALL PSCAL(3,C2,C2,S)
          S4 = SQRT(S)
          IF ( S4 .GT. R8MIN ) THEN
            CALL PSCAL(3,W2,W2,S)
            S2=1.D0/S
            CALL PROVEC(W2,C2,U)
            CALL PSCAL(3,U,U,S)
            S3 = SQRT(S)
            S5 = S3*SQRT(S2)/S4
            CALL PROVEC(U,W2,V)
            CALL PSCVEC(3,S2,V,U)
            CALL PSCVEC(3,S5,U,C2)
          ENDIF
        END IF
      END IF
C
      DO 31 KP=1,NPG
        K = (KP-1) * NORDRE * NORDRE
        L = (KP-1) * NNO
        IF ( OPTION .EQ. 'CHAR_MECA_FF1D1D' .OR.
     &       OPTION .EQ. 'CHAR_MECA_SF1D1D' ) THEN
C            ------------------------------
           DO 11 IC=1,3
              X(IC)=0.D0
              DO 21 NEU=1,NNO
                 X(IC)=X(IC)+W(3*NEU+IC-3)*ZR(IVF+L+NEU-1)
   21         CONTINUE
   11      CONTINUE
           DO 12 IC=1,3
            CALL FOINTE('FM',ZK8(IFORC+IC-1),NBPAR,NOMPAR,X,C2(IC),IRET)
   12      CONTINUE
           IF ( NORMAL) THEN
              CALL PSCAL(3,C2,C2,S)
              S4 = SQRT(S)
              IF ( S4 .GT. R8MIN ) THEN
                CALL PSCAL(3,W2,W2,S)
                S2=1.D0/S
                CALL PROVEC(W2,C2,U)
                CALL PSCAL(3,U,U,S)
                S3 = SQRT(S)
                S5 = S3*SQRT(S2)/S4
                CALL PROVEC(U,W2,V)
                CALL PSCVEC(3,S2,V,U)
                CALL PSCVEC(3,S5,U,C2)
              ENDIF
           ENDIF
        ENDIF
        IF ( OKVENT ) THEN
C         RECUPERATION DE LA VITESSE DE VENT RELATIVE AU NOEUD
          C2(1)=ZR(IFORC)
          C2(2)=ZR(IFORC+1)
          C2(3)=ZR(IFORC+2)

C         CALCUL DU VECTEUR VITESSE PERPENDICULAIRE
          CALL PSCAL(3,C2,C2,S)
          S4 = SQRT(S)
          FCX = 0.0D0
          IF ( S4 .GT. R8MIN ) THEN
            CALL PSCAL(3,W2,W2,S)
            S2=1.D0/S
            CALL PROVEC(W2,C2,U)
            CALL PROVEC(U,W2,V)
            CALL PSCVEC(3,S2,V,VP)
C           NORME DE LA VITESSE PERPENDICULAIRE
            CALL PSCAL(3,VP,VP,VITE2)
            VALPAV(1) = SQRT( VITE2 )
            IF ( VALPAV(1) .GT. R8MIN ) THEN
C             RECUPERATION DE L'EFFORT EN FONCTION DE LA VITESSE
              CALL TECACH('ONN','PVENTCX',1,IFCX,IRET)
              IF ( IFCX .LE. 0 ) GOTO  999
              IF ( ZK8(IFCX)(1:1) .EQ. '.' ) GOTO  999
              CALL FOINTE('FM',ZK8(IFCX),1,NOMPAV,VALPAV,FCX,IRET)
              FCX = FCX / VALPAV(1)
            ENDIF
          ENDIF
          CALL PSCVEC(3,FCX,VP,C2)
        ENDIF

        COEF = ZR(IPOIDS-1+KP)*C1
     &            *SQRT(BILINE(NORDRE,ZR(IGEOM),ZR(IYTY+K),ZR(IGEOM)) )
        DO 22 NEU=1,NNO
          NEUM1 = NEU - 1
          DO 14 IC=1,3
            ZR(IVECTU + NDDL*NEUM1 + (IC-1)) =
     &                 ZR(IVECTU + NDDL*NEUM1 + (IC-1)) +
     &                 COEF * C2(IC) * ZR(IVF+L+NEUM1)
   14     CONTINUE
   22   CONTINUE

   31 CONTINUE
      GOTO 1000

999   CONTINUE
      CALL UTMESS('F','TE0161_01',
     &        'UN CHAMP DE VITESSE DE VENT EST IMPOSE SANS DONNER '//
     &        'UN CX DEPENDANT DE LA VITESSE SUR UN DES CABLES.')


 1000 CONTINUE
      END
