      SUBROUTINE SHL329(NOMTE)
      IMPLICIT REAL*8  (A-H,O-Z)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 11/03/98   AUTEUR CIBHHLV L.VIVAN 
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
C....................................................................
C   CALCUL DES TERMES ELEMENTAIRES DE L'ACCEPTANCE
C     OPTION : ACCEPTANCE
C....................................................................
C
      CHARACTER*2        CODRET
      CHARACTER*7        IELEM,IMODE
      CHARACTER*8        ALIAS
      CHARACTER*16       NOMTE,OPTION
      CHARACTER*24       CHVAL,CHCTE,VETEL,DESR
      REAL*8             SX(9,9),SY(9,9),SZ(9,9),JAC(9)
      REAL*8             NX(9),NY(9),NZ(9),NORM(3,9),ACC(3,9)
      REAL*8             FLUFN(9),ACLOC(3,8),PGL(3,3),XYZL(3,4)
      REAL*8             X(3,9),FF(4,4),DFDX(4,4),DFDY(4,4)
      INTEGER            IPOIDS,IVF,IDFDX,IDFDY,IGEOM,IFREQ
      INTEGER            NDIM,NNO,IPG,NPG1,IVECTT,IMATE
      INTEGER            IDEC,JDEC,KDEC,LDEC
      INTEGER            NBPG(10),IMATTT
C
      INTEGER             NC
      INTEGER LJACO,LTOR,LQSI,LETA,LWGT,LXYC,LCOTE,LCOS,LSIN,
     &        LAIRE,LAIRN,LT1VE,LT2VE,LT2EV
               PARAMETER (NPG   = 4)
               PARAMETER (NNO   = 4)
               PARAMETER (NC    = 4)
               PARAMETER (LJACO = 2)
               PARAMETER (LTOR  = LJACO + 4)
               PARAMETER (LQSI  = LTOR  + 1)
               PARAMETER (LETA  = LQSI + NPG + NNO + 2*NC)
C---------------- COMMUNS NORMALISES  JEVEUX  --------------------------
      COMMON /IVARJE/ZI(1)
      COMMON /RVARJE/ZR(1)
      COMMON /CVARJE/ZC(1)
      COMMON /LVARJE/ZL(1)
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      INTEGER ZI
      REAL*8 ZR
      COMPLEX*16 ZC
      LOGICAL ZL
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80

C------------FIN  COMMUNS NORMALISES  JEVEUX  ---------------------
C


          DESR = '&INEL.'//NOMTE(1:8)//'.DESR'
          CALL JEVETE(DESR,'L',LZR)

C
          CALL JEVECH('PACCELR','L',IACCE)
          CALL JEVECH('PGEOMER','L',IGEOM)
          CALL JEVECH('PNUMMOD','L',IHARM)
          CALL JEVECH('PVECTUR','E',IVECTU)
C
C          CALL DXQPGL(ZR(IGEOM),PGL)
C C         CALL UTPVGL(NNO,3,PGL,ZR(IGEOM),XYZL)

C
      DO 1200 I=1,NNO
         ACLOC(1,I)=0.0D0
         ACLOC(2,I)=0.0D0
         ACLOC(3,I)=0.0D0
1200  CONTINUE
C
        K=0
        DO 1201 I=1,NNO
          DO 20 IDIM=1,3
            K=K+1
            ACLOC(IDIM,I) = ZR(IACCE+K-1)
20        CONTINUE
1201    CONTINUE
C
       DO 1052 IPG=1,NPG
          ACC(1,IPG)=0.0D0
          ACC(2,IPG)=0.0D0
          ACC(3,IPG)=0.0D0
1052   CONTINUE
C
       DO 1061 IPG=1,NPG
          FF(1,IPG)=(1-ZR(LZR+LQSI+IPG-1))*
     &                 (1-ZR(LZR+LETA+IPG-1))/4.D0
          FF(2,IPG)=(1+ZR(LZR+LQSI+IPG-1))*
     &                 (1-ZR(LZR+LETA+IPG-1))/4.D0
          FF(3,IPG)=(1+ZR(LZR+LQSI+IPG-1))*
     &                 (1+ZR(LZR+LETA+IPG-1))/4.D0
          FF(4,IPG)=(1-ZR(LZR+LQSI+IPG-1))*
     &                 (1+ZR(LZR+LETA+IPG-1))/4.D0
C
          DFDX(1,IPG)= -1.D0*
     &                 (1-ZR(LZR+LETA+IPG-1))/4.D0
          DFDX(2,IPG)=1.D0*
     &                 (1-ZR(LZR+LETA+IPG-1))/4.D0
          DFDX(3,IPG)=1.D0*
     &                 (1+ZR(LZR+LETA+IPG-1))/4.D0
          DFDX(4,IPG)=-1.D0*
     &                 (1+ZR(LZR+LETA+IPG-1))/4.D0
C
          DFDY(1,IPG)=-1.D0*(1-ZR(LZR+LQSI+IPG-1))
     &                 /4.D0
          DFDY(2,IPG)=-1.D0*(1+ZR(LZR+LQSI+IPG-1))
     &                /4.D0
          DFDY(3,IPG)=(1+ZR(LZR+LQSI+IPG-1))
     &                 /4.D0
          DFDY(4,IPG)=(1-ZR(LZR+LQSI+IPG-1))
     &                 /4.D0

1061   CONTINUE

C     CALCUL DES PRODUITS VECTORIELS OMI X OMJ
C
      DO 21 INO = 1,NNO
         I = IGEOM + 3*(INO-1) -1
         DO 22 JNO = 1,NNO
            J = IGEOM + 3*(JNO-1) -1
            SX(INO,JNO) = ZR(I+2) * ZR(J+3) - ZR(I+3) * ZR(J+2)
            SY(INO,JNO) = ZR(I+3) * ZR(J+1) - ZR(I+1) * ZR(J+3)
            SZ(INO,JNO) = ZR(I+1) * ZR(J+2) - ZR(I+2) * ZR(J+1)
22       CONTINUE
21    CONTINUE

C
C     BOUCLE SUR LES POINTS DE GAUSS

      DO 101 IPG=1,NPG
         NX(IPG) = 0.0D0
         NY(IPG) = 0.0D0
         NZ(IPG) = 0.0D0
         DO 102 I=1,NNO
            DO 104 J=1,NNO
              NX(IPG) = NX(IPG) + DFDX(I,IPG)
     &            * DFDY(J,IPG) * SX(I,J)
              NY(IPG) = NY(IPG) + DFDX(I,IPG)
     &            * DFDY(J,IPG) * SY(I,J)
              NZ(IPG) = NZ(IPG) + DFDX(I,IPG)
     &            * DFDY(J,IPG) * SZ(I,J)
104        CONTINUE
102      CONTINUE

C      CALCUL DU JACOBIEN AU POINT DE GAUSS IPG

         JAC(IPG) = SQRT (NX(IPG)*NX(IPG) + NY(IPG)*NY(IPG)
     &            + NZ(IPG)*NZ(IPG))

C       CALCUL DE LA NORMALE UNITAIRE

          NORM(1,IPG) = NX(IPG)/JAC(IPG)
          NORM(2,IPG) = NY(IPG)/JAC(IPG)
          NORM(3,IPG) = NZ(IPG)/JAC(IPG)
101     CONTINUE

C
       DO 1051 IPG=1,NPG
         DO 105 I=1,NNO
           ACC(1,IPG) = ACC(1,IPG) + ACLOC(1,I)*
     &           FF(I,IPG)
           ACC(2,IPG) = ACC(2,IPG) + ACLOC(2,I)*
     &           FF(I,IPG)
           ACC(3,IPG) = ACC(3,IPG) + ACLOC(3,I)*
     &           FF(I,IPG)
105      CONTINUE
1051   CONTINUE


C    CALCUL DE COORDONNEES AUX POINTS DE GAUSS

           DO 90 IPG=1,NPG
             X(1,IPG)=0.0D0
             X(2,IPG)=0.0D0
             X(3,IPG)=0.0D0

              DO 91 J=1,NNO

                X(1,IPG)= X(1,IPG)+ZR(IGEOM + 3*(J-1) -1+1)
     &                     *FF(J,IPG)
                X(2,IPG)= X(2,IPG)+ZR( IGEOM + 3*(J-1) -1+2)
     &                     *FF(J,IPG)
                X(3,IPG)= X(3,IPG)+ZR( IGEOM + 3*(J-1) -1+3)
     &                     *FF(J,IPG)

91            CONTINUE



C CALCUL DU FLUX FLUIDE NORMAL AUX POINTS DE GAUSS

                FLUFN(IPG) = ACC(1,IPG)*NORM(1,IPG)+ACC(2,IPG)*
     &               NORM(2,IPG)+ACC(3,IPG)*NORM(3,IPG)

90        CONTINUE

C STOCKAGE DU FLUX FLUIDE DANS UN VECTEUR INDEXE
C PAR LE MODE ET L'ELEMENT

         IMODE='CHBIDON'
         IELEM ='CHBIDON'
         CALL CODENT(ZI(IHARM),'D0',IMODE)
         CALL TECAEL(IADZI,IAZK24)
         CALL CODENT(ZI(IADZI),'D0',IELEM)
         VETEL = '&&329.M'//IMODE//'.EL'//IELEM
         CALL WKVECT(VETEL,'V V R8',4*NPG,IVETEL)
         DO 100 IPG=0,NPG-1
             ZR(IVETEL+4*IPG) = JAC(IPG+1)*FLUFN(IPG+1)
             ZR(IVETEL+4*IPG+1) = X(1,IPG+1)
             ZR(IVETEL+4*IPG+2) = X(2,IPG+1)
             ZR(IVETEL+4*IPG+3) = X(3,IPG+1)
100       CONTINUE

       END
