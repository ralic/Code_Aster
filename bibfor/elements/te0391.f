      SUBROUTINE TE0391 ( OPTION , NOMTE )
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
C    - FONCTION REALISEE:  CALCUL MATRICE DE MASSE MEPODTGD
C                          OPTION : 'MASS_MECA'
C                          OPTION : 'M_GAMMA'
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
      REAL*8        EN(3,2),ENPRIM(3,2),X00(3,3),Y0(3),ROT0(3,3),RHO
      REAL*8        GRANI(4),MASS(18,18),R8BID,ZERO
      REAL*8        A,XIY,XIZ,XJX,PJACOB,AJACOB
      INTEGER       NNO,NPG,NORD,IFF,IPOIDS,IVF,IDFDE,KP,NE,IC,LSECT
      INTEGER       IGEOM,K0,IMATE,LORIEN,IMATUU,IMAT,IACCE,IVECT,I,J
      INTEGER       ICARAC
C ......................................................................
C
      CALL ELREF1(ELREFE)
      ZERO = 0.0D0
      CARAC = '&INEL.'//ELREFE//'.CARAC'
      CALL JEVETE(CARAC,'L',ICARAC)
      NNO  = ZI(ICARAC)
      NPG  = ZI(ICARAC+2)
      NORD = 6 * NNO
C
      FF = '&INEL.'//ELREFE//'.FF'
      CALL JEVETE(FF,'L',IFF)
      IPOIDS = IFF
      IVF    = IPOIDS+NPG-1
      IDFDE  = IVF + NPG*NNO
C
      DO 10 KP=1,NPG
         DO 20 NE=1,NNO
            IVF = IVF + 1
            IDFDE = IDFDE + 1
            EN(NE,KP) = ZR(IVF)
            ENPRIM(NE,KP) = ZR(IDFDE)
 20      CONTINUE
 10   CONTINUE
C
      CALL JEVECH('PGEOMER','L',IGEOM)
      K0 = IGEOM - 1
C
      DO 30 NE=1,NNO
         DO 32 IC=1,3
            K0 = K0 + 1
            X00(IC,NE) = ZR(K0)
 32      CONTINUE
 30   CONTINUE
C
      CALL JEVECH('PMATERC','L',IMATE)
      CALL RCVALA(ZI(IMATE),'ELAS',0,' ',R8BID,1,'RHO',RHO,CODRET,'FM')
C
C     --- RECUPERATION DES CARACTERISTIQUES GENERALES DES SECTIONS ---
      CALL JEVECH ('PCAGNPO', 'L',LSECT)
      LSECT = LSECT-1
C
C     --- LA SECTION EST SUPPOSEE CONSTANTE ---
      A     =  ZR(LSECT+1)
      XIY   =  ZR(LSECT+2)
      XIZ   =  ZR(LSECT+3)
      XJX   =  ZR(LSECT+8)
      GRANI(1) = RHO * XJX
      GRANI(2) = RHO * XIY
      GRANI(3) = RHO * XIZ
      GRANI(4) = RHO * A
C
C     --- RECUPERATION DES ORIENTATIONS INITIALES Y0(1), Y0(2), Y0(3)
      CALL JEVECH ('PCAORIE', 'L',LORIEN)
      Y0(1) =  ZR(LORIEN    )
      Y0(2) =  ZR(LORIEN + 1)
      Y0(3) =  ZR(LORIEN + 2)
C
      DO 40 J=1,NORD
         DO 42 I=1,NORD
            MASS(I,J) = ZERO
 42      CONTINUE
 40   CONTINUE
C
C* BOUCLE SUR LES POINTS DE GAUSS
C
      DO 50 KP=1,NPG
         CALL GDJRG0 (KP,NNO,ENPRIM,X00,Y0,   AJACOB,ROT0)
         PJACOB = ZR(IPOIDS-1+KP) * AJACOB
         CALL GDMMAS (KP,NNO,PJACOB,EN,GRANI,ROT0,   MASS)
50    CONTINUE
C
      IF ( OPTION .EQ. 'MASS_MECA' ) THEN
         CALL JEVECH('PMATUNS','E',IMATUU)
         IMAT = IMATUU - 1
         DO 62 I=1,NORD
            DO 61 J=1,NORD
               IMAT = IMAT +1
               ZR(IMAT) = MASS(I,J)
61          CONTINUE
62       CONTINUE
      ELSEIF ( OPTION .EQ. 'M_GAMMA' ) THEN
         CALL JEVECH('PDEPLAR','L',IACCE)
         CALL JEVECH('PVECTUR','E',IVECT)
         CALL PMAVEC('ZERO',NORD,MASS,ZR(IACCE),ZR(IVECT))
      ENDIF
C
      END
