      SUBROUTINE TE0484 ( OPTION , NOMTE )
      IMPLICIT   NONE
      CHARACTER*16        OPTION , NOMTE
C ......................................................................
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 17/02/99   AUTEUR VABHHTS J.PELLET 
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
C    - FONCTION REALISEE:  CALCUL DE LA CHARGE LIMITE POUR
C                          DES ELEMENTS INCOMPRESSIBLES SEG3 PLAN OU AXI
C                          OPTIONS : 'CHAR_LIMITE'
C                                    'CHAR_LIMITE_F'
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C
C VECTEURS DIMENSIONNES POUR  NPG = 4
C ......................................................................
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
      INTEGER       KP,I,K,L,IECHLI,IDEPL,ICARAC,IPREF,IPRES
      INTEGER       ITEMPS,IFORF,IFORC,ICODE,IFF,IPOIDS,IVF1,IGEOM
      INTEGER       NPG1,NNO1,NNO2,IDFDE1,NDIM,I1,J1
C
      REAL*8        VALPAR(3),POIDS,R,NX,NY,FORCX,FORCY
      REAL*8        TFORC,UX,UY,XG,YG,FX,FY,PRES,CISA,VF
C
      CHARACTER*8   NOMPAR(3)
      CHARACTER*24  CARAC,FF
C
      LOGICAL       FONC,AXI
C-----------------------------------------------------------------------
C
      CALL JEMARQ()
C
      AXI = NOMTE(3:4).EQ.'AX'
C
      CARAC='&INEL.'//NOMTE(1:8)//'.CARAC'
      CALL JEVETE(CARAC,' ',ICARAC)
      NNO1 = ZI(ICARAC)
      NNO2 = ZI(ICARAC+1)
      NPG1 = ZI(ICARAC+2)
      NDIM = 2
C
      FF   ='&INEL.'//NOMTE(1:8)//'.FF'
      CALL JEVETE(FF,' ',IFF)
      IPOIDS = IFF
      IVF1   = IPOIDS + NPG1
      IDFDE1 = IVF1   + NPG1*NNO1
C
      CALL JEVECH ( 'PGEOMER', 'L', IGEOM )
      CALL JEVECH ( 'PDEPLAR', 'L', IDEPL )
      IF ( OPTION .EQ. 'CHAR_LIMITE_F' ) THEN
        FONC = .TRUE.
        CALL JEVECH ( 'PFF1D2D', 'L', IFORF  )
        CALL JEVECH ( 'PPRESSF', 'L', IPREF  )
        CALL JEVECH ( 'PTEMPSR', 'L', ITEMPS )
        NOMPAR(1) = 'X'
        NOMPAR(2) = 'Y'
        NOMPAR(3) = 'INST'
        VALPAR(3) = ZR(ITEMPS)
      ELSE
        FONC =.FALSE.
        CALL JEVECH ( 'PFR1D2D', 'L', IFORC )
        CALL JEVECH ( 'PPRESSR', 'L', IPRES )
      ENDIF
      CALL JEVECH ('PECHLI','E',IECHLI)
C
      TFORC   = 0.D0
C
C --- BOUCLE SUR LES POINTS DE GAUSS
C
      DO 800 KP=1,NPG1
        K = (KP-1)*NNO1
C
        CALL VFF2DN (NNO1,ZR(IPOIDS+KP-1),ZR(IDFDE1+K),ZR(IGEOM),NX,NY,
     &               POIDS)
        IF ( AXI ) THEN
          R = 0.D0
          DO 102 I=1,NNO1
            L = (KP-1)*NNO1+I
            R = R + ZR(IGEOM+2*(I-1))*ZR(IVF1+L-1)
102       CONTINUE
          POIDS = POIDS*R
        ENDIF
C
        XG   = 0.D0
        YG   = 0.D0
        UX   = 0.D0
        UY   = 0.D0
        DO 10 I=1,NNO1
          IF (I.LE.NNO2) THEN
            I1 = NDIM+1
            J1 = 0
          ELSE
            I1 = NDIM
            J1 = NNO2
          ENDIF
          VF  = ZR(IVF1+K+I-1)
          XG  = XG + VF*ZR(IGEOM+2*(I-1)  )
          YG  = YG + VF*ZR(IGEOM+2*(I-1)+1)
          UX  = UX + VF*ZR(IDEPL+J1+I1*(I-1)  )
          UY  = UY + VF*ZR(IDEPL+J1+I1*(I-1)+1)
   10   CONTINUE
C
        IF ( FONC ) THEN
           VALPAR(1) = XG
           VALPAR(2) = YG
           CALL FOINTE('FM',ZK8(IFORF  ),3,NOMPAR,VALPAR,FORCX,ICODE)
           CALL FOINTE('FM',ZK8(IFORF+1),3,NOMPAR,VALPAR,FORCY,ICODE)
           CALL FOINTE('FM',ZK8(IPREF)  ,3,NOMPAR,VALPAR,PRES ,ICODE)
           CALL FOINTE('FM',ZK8(IPREF+1),3,NOMPAR,VALPAR,CISA ,ICODE)
        ELSE
           PRES = 0.D0
           CISA = 0.D0
           FORCX = 0.D0
           FORCY = 0.D0
           DO 104 I = 1 , NNO1
              PRES = PRES + ZR(IPRES+2*(I-1))  *ZR(IVF1+K+I-1)
              CISA = CISA + ZR(IPRES+2*(I-1)+1)*ZR(IVF1+K+I-1)
              FORCX = FORCX + ZR(IFORC+2*(I-1))  *ZR(IVF1+K+I-1)
              FORCY = FORCY + ZR(IFORC+2*(I-1)+1)*ZR(IVF1+K+I-1)
 104       CONTINUE
        ENDIF
C
        FX = -NX*PRES -NY*CISA + FORCX
        FY = -NY*PRES +NX*CISA + FORCY
C
C - CALCUL DU TERME ELEMENTAIRE DU AU CHARGEMENT PERMANENT
C
        TFORC = TFORC + (FX*UX+FY*UY)*POIDS
C
800   CONTINUE
C
      ZR(IECHLI  ) = 0.D0
      ZR(IECHLI+1) = 0.D0
      ZR(IECHLI+2) = 0.D0
      ZR(IECHLI+3) = TFORC
C
      CALL JEDEMA()
      END
