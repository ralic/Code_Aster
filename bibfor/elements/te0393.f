      SUBROUTINE TE0393 ( OPTION , NOMTE )
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
      IMPLICIT REAL*8(A-H,O-Z)
C
      CHARACTER*16        OPTION , NOMTE
C ......................................................................
C    - FONCTION REALISEE:  CALCUL DES FORCES NODALES DE MECA_POU_D_T_GD
C                          OPTION : 'FORC_NODA        '
C
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
C
      CHARACTER*24 CARAC,FF
      CHARACTER*8  ELREFE
      REAL*8 EN(3,2),ENPRIM(3,2),FINT(6,3),Y0(3),X00(3,3),
     &X0K(3,3),QIK(3,3),X0PG(3),QIG(3),ROT(3,3),ROT0(3,3),ROTABS(3,3),
     &GN(3),GM(3),PN(3),PM(3)
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
      ZERO = 0.D0
      UN   = 1.D0
C
C PARAMETRES EN ENTREE
C
C
      CARAC='&INEL.'//ELREFE//'.CARAC'
      CALL JEVETE(CARAC,'L',ICARAC)
      NNO  = ZI(ICARAC)
      NPG  = ZI(ICARAC+2)
C
      FF   ='&INEL.'//ELREFE//'.FF'
      CALL JEVETE(FF,'L',IFF)
      IPOIDS=IFF
      IVF   =IPOIDS+NPG-1
      IDFDE =IVF + NPG*NNO
C
      DO 2 KP=1,NPG
        DO 1 NE=1,NNO
          IVF = IVF + 1
          IDFDE = IDFDE + 1
          EN(NE,KP) = ZR(IVF)
          ENPRIM(NE,KP) = ZR(IDFDE)
1       CONTINUE
2     CONTINUE
C
C     CALL JEVECH('PTEMPPR','L',ITEMPR)
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PDEPLMR','L',IDEPM)
      CALL JEVECH('PDEPLPR','L',IDEPDE)
      CALL JEVECH('PCONTMR','L',ISIGMA)
C
C     --- RECUPERATION DES ORIENTATIONS INITIALES Y0(1), Y0(2), Y0(3)
      CALL JEVECH ('PCAORIE', 'L',LORIEN)
      Y0(1) =  ZR(LORIEN    )
      Y0(2) =  ZR(LORIEN + 1)
      Y0(3) =  ZR(LORIEN + 2)
C
C PARAMETRES EN SORTIE
C
        CALL JEVECH('PVECTUR','E',JEFINT)
        DO 14 NE=1,NNO
          DO 13 KC=1,6
            FINT(KC,NE) = ZERO
13        CONTINUE
14      CONTINUE
C
C     DO 21 NE=1,NNO
C       TEMPN(NE) = ZR(ITEMPR-1+NE)
C21    CONTINUE
C
      K0 = IGEOM - 1
      K1 = IDEPM - 1
      K2 = IDEPDE - 1
C
      DO 33 NE=1,NNO
        DO 31 KC=1,3
          K0 = K0 + 1
          K1 = K1 + 1
          K2 = K2 + 1
          X00(KC,NE) = ZR(K0)
          X0K(KC,NE) = ZR(K0) + ZR(K1) + ZR(K2)
31      CONTINUE
        DO 32 KC=1,3
          K1 = K1 + 1
          K2 = K2 + 1
          QIK(KC,NE) = ZR(K1) + ZR(K2)
32      CONTINUE
33    CONTINUE
C
C
C* BOUCLE SUR LES POINTS DE GAUSS
C
      DO 51 KP=1,NPG
        CALL GDJRG0 (KP,NNO,ENPRIM,X00,Y0,   AJACOB,ROT0)
        PJACOB = ZR(IPOIDS-1+KP) * AJACOB
C
      DO 41 IC=1,3
      X0PG(IC) = ZERO
      QIG (IC) = ZERO
   41 CONTINUE
C     TEMPG = ZERO
      UNSURJ = UN / AJACOB
      DO 43 IC=1,3
         DO 42 NE=1,NNO
            X0PG(IC)    = X0PG(IC)  + UNSURJ*ENPRIM(NE,KP)*X0K(IC,NE)
            QIG (IC)    = QIG (IC)  +        EN    (NE,KP)*QIK(IC,NE)
42       CONTINUE
43    CONTINUE
C     DO 45 NE=1,NNO
C        TEMPG = TEMPG + EN(NE,KP)*TEMPN(NE)
C45    CONTINUE
C
      CALL MAROTA (QIG,ROT)
      CALL PROMAT (ROT   ,3,3,3,ROT0  ,3,3,3,ROTABS)
      DO 46 IC=1,3
         GN(IC) = ZR(ISIGMA-1+6*(KP-1)+IC)
         GM(IC) = ZR(ISIGMA+2+6*(KP-1)+IC)
46    CONTINUE
      CALL PROMAT (ROTABS,3,3,3,GN    ,3,3,1,PN    )
      CALL PROMAT (ROTABS,3,3,3,GM    ,3,3,1,PM    )
C
      CALL GDFINT (KP,NNO,AJACOB,PJACOB,EN,ENPRIM,X0PG,PN,PM,  FINT)
C
C* FIN DE BOUCLE SUR LES POINTS DE GAUSS
C
51    CONTINUE
C
          IFINT = JEFINT - 1
          DO 72 NE=1,NNO
            DO 71 KC=1,6
              IFINT = IFINT + 1
              ZR(IFINT) = FINT(KC,NE)
71          CONTINUE
72        CONTINUE
      END
