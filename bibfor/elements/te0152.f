      SUBROUTINE TE0152(OPTION,NOMTE)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)     OPTION,NOMTE
C     ------------------------------------------------------------------
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
C     CALCULE DES TERMES PROPRES A UN STRUCTURE
C     OPTION : 'MASS_INER'              (ELEMENTS FLUIDES 3D)
C     ------------------------------------------------------------------
C     --- DEBUT DECLARATIONS NORMALISEES JEVEUX ------------------------
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
C     --- FIN DECLARATIONS NORMALISEES JEVEUX --------------------------
      PARAMETER         ( NBRES=2 )
      CHARACTER*8        NOMRES(NBRES)
      CHARACTER*8        ELREFE
      CHARACTER*2        CODRET(NBRES)
      CHARACTER*24       CHVAL, CHCTE
      REAL*8             VALRES(NBRES)
      REAL*8             DFDX(27), DFDY(27), DFDZ(27), POIDS, VOLUME
      REAL*8             X(27), Y(27), Z(27), XG, YG, ZG, MATINE(6)
      INTEGER            IPOIDS, IVF, IDFDE, IDFDN, IDFDK, IGEOM
      INTEGER            NNO, KP, NPG, I, J, IMATE, NBPG(10)
C     ------------------------------------------------------------------
      CALL ELREF1(ELREFE)
      ZERO = 0.D0
      CHCTE = '&INEL.'//ELREFE//'.CARACTE'
      CALL JEVETE(CHCTE,'L',JIN)
      NDIM = ZI(JIN+1-1)
      NNO  = ZI(JIN+2-1)
      NBFPG = ZI(JIN+3-1)
      DO 111 I = 1,NBFPG
         NBPG(I) = ZI(JIN+3-1+I)
  111 CONTINUE
      NPG = NBPG(1)
C
      CHVAL = '&INEL.'//ELREFE//'.FFORMES'
      CALL JEVETE(CHVAL,'L',JVAL)
C
      IPOIDS = JVAL + (NDIM+1)*NNO*NNO
      IVF    = IPOIDS + NPG
      IDFDE  = IVF    + NPG*NNO
      IDFDN  = IDFDE  + 1
      IDFDK  = IDFDN  + 1
C
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PMATERC','L',IMATE)
C
      NOMRES(1)='RHO'
      NOMRES(2)='CELE_R'
      CALL RCVALA (ZI(IMATE),'FLUIDE',0,' ',R8BID,2,NOMRES,VALRES,
     &             CODRET , 'FM' )
      RHO    = VALRES(1)
C
      DO 105 I=1,NNO
         X(I) =  ZR(IGEOM+3*(I-1))
         Y(I) =  ZR(IGEOM+3*I-2)
         Z(I) =  ZR(IGEOM+3*I-1)
 105  CONTINUE
C
      CALL JEVECH('PMASSINE','E',LCASTR)
      DO 20 I = 0,3
         ZR(LCASTR+I) = ZERO
 20   CONTINUE
      DO 22 I = 1,6
         MATINE(I) = ZERO
 22   CONTINUE
C
C     --- BOUCLE SUR LES POINTS DE GAUSS
      VOLUME = 0.D0
      DO 110 KP = 1,NPG
         L = (KP-1)*NNO
         K = (KP-1)*NNO*3
         CALL DFDM3D(NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IDFDN+K),
     &                   ZR(IDFDK+K),ZR(IGEOM),DFDX,DFDY,DFDZ,POIDS )
C
         VOLUME  = VOLUME + POIDS
         DO 106 I = 1,NNO
C           --- CDG ---
            ZR(LCASTR+1) = ZR(LCASTR+1) + POIDS*X(I)*ZR(IVF+L+I-1)
            ZR(LCASTR+2) = ZR(LCASTR+2) + POIDS*Y(I)*ZR(IVF+L+I-1)
            ZR(LCASTR+3) = ZR(LCASTR+3) + POIDS*Z(I)*ZR(IVF+L+I-1)
C           --- INERTIE ---
            XXI = 0.D0
            YYI = 0.D0
            ZZI = 0.D0
            DO 107 J =  1,NNO
               XXI = XXI +  X(I)*ZR(IVF+L+I-1)*X(J)*ZR(IVF+L+J-1)
               YYI = YYI +  Y(I)*ZR(IVF+L+I-1)*Y(J)*ZR(IVF+L+J-1)
               ZZI = ZZI +  Z(I)*ZR(IVF+L+I-1)*Z(J)*ZR(IVF+L+J-1)
               MATINE(2) = MATINE(2)+POIDS*X(I)*ZR(IVF+L+I-1)*
     +                                           Y(J)*ZR(IVF+L+J-1)
               MATINE(4) = MATINE(4)+POIDS*X(I)*ZR(IVF+L+I-1)*
     +                                           Z(J)*ZR(IVF+L+J-1)
               MATINE(5) = MATINE(5)+POIDS*Y(I)*ZR(IVF+L+I-1)*
     +                                           Z(J)*ZR(IVF+L+J-1)
107         CONTINUE
            MATINE(1) = MATINE(1) + POIDS*(YYI + ZZI)
            MATINE(3) = MATINE(3) + POIDS*(XXI + ZZI)
            MATINE(6) = MATINE(6) + POIDS*(XXI + YYI)
106      CONTINUE
110   CONTINUE
C
      XG = ZR(LCASTR+1) / VOLUME
      YG = ZR(LCASTR+2) / VOLUME
      ZG = ZR(LCASTR+3) / VOLUME
      ZR(LCASTR)   = VOLUME * RHO
      ZR(LCASTR+1) = XG
      ZR(LCASTR+2) = YG
      ZR(LCASTR+3) = ZG
C
C     ---ON DONNE LES INERTIES EN G ---
      ZR(LCASTR+4) = MATINE(1)*RHO - ZR(LCASTR)*( YG*YG + ZG*ZG )
      ZR(LCASTR+5) = MATINE(3)*RHO - ZR(LCASTR)*( XG*XG + ZG*ZG )
      ZR(LCASTR+6) = MATINE(6)*RHO - ZR(LCASTR)*( XG*XG + YG*YG )
      ZR(LCASTR+7) = MATINE(2)*RHO - ZR(LCASTR)*( XG * YG )
      ZR(LCASTR+8) = MATINE(4)*RHO - ZR(LCASTR)*( XG * ZG )
      ZR(LCASTR+9) = MATINE(5)*RHO - ZR(LCASTR)*( YG * ZG )
C
      END
