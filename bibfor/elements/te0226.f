      SUBROUTINE TE0226 ( OPTION , NOMTE )
      IMPLICIT   NONE
      CHARACTER*16        OPTION , NOMTE
C ......................................................................
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
C    - FONCTION REALISEE:  CALCUL DES MATRICES ELEMENTAIRES
C                          COQUE 1D
C                          OPTION : 'MASS_MECA       '
C                          ELEMENT: MECXSE3,METCSE3,METDSE3
C
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
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
      CHARACTER*24  CARAC,FF
      CHARACTER*8   ELREFE
      CHARACTER*2   CODRET
      REAL*8        DFDX(3),R,RM,RF,RMF,POIDS,COUR,NX,NY,H,VFI,VFJ
      REAL*8        MATP(9,9),MATV(45),R8B,RHO
      INTEGER       NNO,ICARAC,IFF,IPOIDS,IVF,IDFDK,IGEOM,IMATE,ICACO
      INTEGER       KP,NPG,II,JJ,I,J,K,IMATUU,KD1,KD2,KD3,IJ1,IJ2,IJ3
      INTEGER       NDDL,NVEC,IACCE,IVECT
C ......................................................................
C
      CALL ELREF1(ELREFE)

      CARAC = '&INEL.'//ELREFE//'.CARAC'
      CALL JEVETE(CARAC,'L',ICARAC)
      NNO  = ZI(ICARAC)
      NPG  = ZI(ICARAC+2)
      NDDL = 3 * NNO
      NVEC = NDDL * ( NDDL + 1 ) / 2
C
      FF = '&INEL.'//ELREFE//'.FF'
      CALL JEVETE(FF,'L',IFF)
      IPOIDS = IFF
      IVF    = IPOIDS+NPG
      IDFDK  = IVF   +NPG*NNO
C
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PMATERC','L',IMATE)
      CALL JEVECH('PCACOQU','L',ICACO)
C
      CALL RCVALA (ZI(IMATE),'ELAS',0,' ',R8B,1,'RHO',RHO,CODRET,'FM')
      H   = ZR(ICACO)
      RM  = RHO * H
      RF  = RHO * H**3/12.D0
C
      DO 2 K = 1,NVEC
         MATV(K) = 0.0D0
 2    CONTINUE
C
      DO 10 KP = 1,NPG
         K=(KP-1)*NNO
         CALL DFDM1D (NNO,ZR(IPOIDS+KP-1),ZR(IDFDK+K),ZR(IGEOM),DFDX,
     &                COUR,POIDS,NX,NY)
         IF ( NOMTE(3:4).EQ.'CX' ) THEN
            R = 0.0D0
            DO 20 I=1,NNO
               R = R + ZR(IGEOM+2*(I-1))*ZR(IVF+K+I-1)
 20         CONTINUE
            POIDS = POIDS*R
            RMF   = RF*(COUR+NX/R)
         ENDIF
C
         IF ( NOMTE(1:7).EQ.'METDSE3'.OR.NOMTE(1:7).EQ.'METCSE3') THEN
             RMF = RF* COUR
         ENDIF
         KD1 = 5
         KD2 = 3
         KD3 = 2
         DO 30 I=1,3*NNO,3
            KD1 = KD1+ 3*I -6
            KD2 = KD2+ 3*I -3
            KD3 = KD3+ 3*I
            II  = (I+2)/3
            DO 40 J=1,I,3
               JJ  = (J+2)/3
               IJ1 = KD1+ J-2
               IJ2 = KD2+ J-2
               IJ3 = KD3+ J-2
               VFI = ZR (IVF+K+II-1)
               VFJ = ZR (IVF+K+JJ-1)
               MATV(IJ1  ) = MATV(IJ1  ) + VFI * VFJ * POIDS * RM
               MATV(IJ2  ) = 0.0D0
               MATV(IJ2+1) = MATV(IJ1  )
               MATV(IJ3  ) = MATV(IJ3  ) + VFI * VFJ * POIDS * RMF * NY
               MATV(IJ3+1) = MATV(IJ3+1) - VFI * VFJ * POIDS * RMF * NX
               MATV(IJ3+2) = MATV(IJ3+2) + VFI * VFJ * POIDS * RF
 40         CONTINUE
C
            DO 50 J=1,I-3,3
               JJ  = (J+2)/3
               IJ1 = KD1 + J-2
               IJ2 = KD2 + J-2
               IJ3 = KD3 + J-2
               MATV(IJ1+1) = MATV(IJ2  )
               MATV(IJ1+2) = MATV(IJ3  )
               MATV(IJ2+2) = MATV(IJ3+1)
 50         CONTINUE
 30      CONTINUE
 10   CONTINUE
C
      IF ( OPTION .EQ. 'MASS_MECA' ) THEN
C
         CALL JEVECH('PMATUUR','E',IMATUU)
C
         DO 100 I = 1 , NVEC
            ZR(IMATUU+I-1) = MATV(I)
 100     CONTINUE
C
      ELSEIF ( OPTION .EQ. 'M_GAMMA' ) THEN
C
         CALL JEVECH('PDEPLAR','L',IACCE)
         CALL JEVECH('PVECTUR','E',IVECT)
         CALL VECMA(MATV,NVEC,MATP,NDDL)
         CALL PMAVEC('ZERO',NDDL,MATP,ZR(IACCE),ZR(IVECT))
C
      ELSE
         CALL UTMESS ('F' , 'TE0226' , 'OPTION NON TRAITEE')
      ENDIF
C
      END
