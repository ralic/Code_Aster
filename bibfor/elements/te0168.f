      SUBROUTINE TE0168 ( OPTION , NOMTE )
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
C    - FONCTION REALISEE:  CALCUL MATRICE DE MASSE MECABLE
C                          OPTION : 'MASS_MECA'
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
      REAL*8        A,RHO,BILINE,COEF,JACOBI,EN(3,2),R8B
      REAL*8        MATP(6,6), MATV(21)
      INTEGER       NNO,NPG,K,KP,I,II,JJ,KI,KY,NDDL,NVEC,IMATUU,LSECT
      INTEGER       ICARAC,IFF,IPOIDS,IVF,IYTY,IGEOM,IMATE,IACCE,IVECT
C ......................................................................
C
      CALL ELREF1(ELREFE)
C
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
      IYTY   = IVF   +2*NPG*NNO
C
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PMATERC','L',IMATE)
C
      CALL RCVALA (ZI(IMATE),'CABLE',0,' ',R8B,1,'RHO',RHO,CODRET,'FM')
      CALL JEVECH('PCACABL','L',LSECT)
      A = ZR(LSECT)
C
      K = 0
      DO 10 KP = 1,NPG
         DO 12 I = 1,NNO
            K = K + 1
            EN(I,KP) = ZR(IVF-1+K)
 12      CONTINUE
 10   CONTINUE
C
      DO 20 K = 1,NVEC
         MATV(K) = 0.0D0
 20   CONTINUE
C
      DO 30 KP = 1,NPG
         KY = (KP-1) * NDDL * NDDL
         JACOBI = SQRT(BILINE(NDDL,ZR(IGEOM),ZR(IYTY+KY),ZR(IGEOM)))
         COEF = RHO * A * JACOBI * ZR(IPOIDS-1+KP)
         K = 0
         DO 40 II = 1,NNO
            DO 50 KI = 1,3
               K = K + KI - 3
               DO 60 JJ=1,II
                  K = K + 3
                  MATV(K) = MATV(K) + COEF*EN(II,KP)*EN(JJ,KP)
 60            CONTINUE
 50         CONTINUE
 40      CONTINUE
 30   CONTINUE
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
C
         CALL VECMA(MATV,NVEC,MATP,NDDL)
         CALL PMAVEC('ZERO',NDDL,MATP,ZR(IACCE),ZR(IVECT))
C
      ELSE
         CALL UTMESS ('F' , 'TE0168' , 'OPTION NON TRAITEE')
      ENDIF
C
      END
