      SUBROUTINE TE0220 ( OPTION , NOMTE )
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
C    - FONCTION REALISEE:
C                         CALCUL DE L'ENERGIE THERMIQUE A L'EQUILIBRE
C                         OPTION : 'EPOT_ELEM_TEMP'
C
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
C
C ----- DEBUT --- COMMUNS NORMALISES  JEVEUX  --------------------------
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
      CHARACTER*2        CODRET
      CHARACTER*8        NOMPAR,ELREFE
      REAL*8             VALRES, VALPAR
      REAL*8             DFDX(9),DFDY(9),POIDS,FLUX,FLUY,EPOT
      INTEGER            NNO,KP,J,K,ITEMPE,ITEMP,IENER
      INTEGER            ICARAC,IFF,IPOIDS,IVF,IDFDE,IDFDK,IGEOM,IMATE
      INTEGER            NPG
C
      CALL ELREF1(ELREFE)

      CARAC='&INEL.'//ELREFE//'.CARAC'
      CALL JEVETE(CARAC,'L',ICARAC)
      NNO  = ZI(ICARAC)
      NPG  = ZI(ICARAC+2)
C
      FF   ='&INEL.'//ELREFE//'.FF'
      CALL JEVETE(FF,'L',IFF)
      IPOIDS = IFF
      IVF    = IPOIDS + NPG
      IDFDE  = IVF    + NPG*NNO
      IDFDK  = IDFDE  + NPG*NNO
C
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PMATERC','L',IMATE)
      CALL JEVECH('PTEMPER','L',ITEMPE)
      CALL JEVECH('PENERDR','E',IENER)
C
      CALL TECACH('ONN','PTEMPSR',1,ITEMP,IRET)
      IF ( ITEMP .EQ. 0 ) THEN
         NBPAR  = 0
         NOMPAR = ' '
         VALPAR = 0.D0
      ELSE
         NBPAR  = 1
         NOMPAR = 'INST'
         VALPAR = ZR(ITEMP)
      ENDIF
C
      CALL RCVALA ( ZI(IMATE),'THER',NBPAR,NOMPAR,VALPAR,1,'LAMBDA',
     &              VALRES,CODRET,'FM')
C
      EPOT = 0.D0
      DO 101 KP=1,NPG
         K=(KP-1)*NNO
         CALL DFDM2D ( NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IDFDK+K),
     &                 ZR(IGEOM),DFDX,DFDY,POIDS )
         FLUX = 0.D0
         FLUY = 0.D0
         DO 110 J=1,NNO
             FLUX = FLUX + ZR(ITEMPE+J-1)*DFDX(J)
             FLUY = FLUY + ZR(ITEMPE+J-1)*DFDY(J)
 110     CONTINUE
C
         EPOT = EPOT - ( FLUX**2 + FLUY**2 )*POIDS
101   CONTINUE
      ZR(IENER) = EPOT * VALRES / 2.D0
C
      END
