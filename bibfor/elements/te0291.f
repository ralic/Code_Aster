      SUBROUTINE TE0291 ( OPTION , NOMTE )
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
C    - FONCTION REALISEE:  CALCUL DU CHAM_ELEM_ERREUR EN 2D
C                          OPTION : 'CALC_ESTI_ERRE'
C
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
C
      CHARACTER*24       CARAC,FF
      CHARACTER*8        NOMRES(2),ELREFE
      CHARACTER*2        CODRET(2)
      REAL*8             DFDX(9),DFDY(9),POIDS,VALRES(2)
      REAL*8             SIXX,SIYY,SIZZ,SIXY,RR
      REAL*8             SIG11,SIG22,SIG33,SIG12,R,TH,THETA,NORVRA,NORV
      REAL*8             XX,YY,E,NU,TPG,ESIG,EEST,NOR,NORSIG,NU0,NUE
      INTEGER            NNO,KP,NPG1,I,K,IVECTT
      INTEGER            ICARAC,IFF,IPOIDS,IVF,IDFDE,IDFDK,IGEOM
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
      CALL ELREF1(ELREFE)
      CALL JEMARQ()


      CARAC='&INEL.'//ELREFE//'.CARAC'
      CALL JEVETE(CARAC,' ',ICARAC)
      NNO  = ZI(ICARAC)
      NPG1 = ZI(ICARAC+2)
C
      FF   ='&INEL.'//ELREFE//'.FF'
      CALL JEVETE(FF ,' ',IFF   )
      IPOIDS=IFF
      IVF   =IPOIDS+NPG1
      IDFDE =IVF   +NPG1*NNO
      IDFDK =IDFDE +NPG1*NNO
C
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PMATERC','L',IMATE)
      MATER = ZI(IMATE)
      NOMRES(1) = 'E'
      NOMRES(2) = 'NU'
      CALL JEVECH('PSIEF_R','L',ISIEF)
      CALL JEVECH('PTEMPER','L',ITEMP)
      CALL JEVECH('PSIGMA','L',ISIG)
      CALL JEVECH('PERREUR','E',IERR)
C
      NORSIG = 0.D0
      ZR(IERR) = 0.D0
C
C    BOUCLE SUR LES POINTS DE GAUSS
C
      DO 101 KP=1,NPG1
        K=(KP-1)*NNO
        CALL DFDM2D ( NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IDFDK+K),
     &                ZR(IGEOM),DFDX,DFDY,POIDS )
        IF ( NOMTE(3:4) .EQ. 'AX' ) THEN
           R = 0.D0
           DO 103 I=1,NNO
             R = R + ZR(IGEOM+2*(I-1)) * ZR(IVF+K+I-1)
103        CONTINUE
           POIDS = POIDS*R
        ENDIF
        XX = 0.D0
        YY = 0.D0
        DO 106 I=1,NNO
          XX = XX + ZR(IGEOM+2*(I-1)) * ZR(IVF+K+I-1)
          YY = YY + ZR(IGEOM+2*I-1)   * ZR(IVF+K+I-1)
106     CONTINUE
        SIG11 = 0.D0
        SIG22 = 0.D0
        SIG33 = 0.D0
        SIG12 = 0.D0
        TPG   = 0.D0
        DO 102 I=1,NNO
             SIG11 = SIG11 + ZR(ISIG-1+4*(I-1)+1) * ZR(IVF+K+I-1)
             SIG22 = SIG22 + ZR(ISIG-1+4*(I-1)+2) * ZR(IVF+K+I-1)
             SIG33 = SIG33 + ZR(ISIG-1+4*(I-1)+3) * ZR(IVF+K+I-1)
             SIG12 = SIG12 + ZR(ISIG-1+4*(I-1)+4) * ZR(IVF+K+I-1)
             TPG   = TPG   + ZR(ITEMP-1+I) * ZR(IVF+K+I-1)
102     CONTINUE
        CALL RCVALA ( MATER,'ELAS',1,'TEMP',TPG,2,NOMRES,
     &                VALRES, CODRET, 'FM' )
        E  = VALRES(1)
        NU = VALRES(2)
C
C    ESTIMATION DE L'ERREUR EN NORME DE L' ENERGIE
C
        EEST = (SIG11-ZR(ISIEF-1+4*(KP-1)+1))**2
     &        +(SIG22-ZR(ISIEF-1+4*(KP-1)+2))**2
     &        +(SIG33-ZR(ISIEF-1+4*(KP-1)+3))**2
     &        +(1.D0+NU)*(SIG12-ZR(ISIEF-1+4*(KP-1)+4))**2
        ZR(IERR) = ZR(IERR) + EEST * POIDS / E
C
C    NORME DE L' ENERGIE DE LA SOLUTION CALCULEE
C
        NOR    = ZR(ISIEF-1+4*(KP-1)+1)**2
     &         + ZR(ISIEF-1+4*(KP-1)+2)**2
     &         + ZR(ISIEF-1+4*(KP-1)+3)**2
     &         +(1.D0+NU)*ZR(ISIEF-1+4*(KP-1)+4)**2
        NORSIG = NORSIG + NOR * POIDS / E
C
101     CONTINUE
C
        NU0 = 100.D0*SQRT(ZR(IERR)/(ZR(IERR)+NORSIG))
        ZR(IERR  ) = SQRT(ZR(IERR))
        ZR(IERR+1) = NU0
        ZR(IERR+2) = SQRT(NORSIG)
      CALL JEDEMA()
      END
