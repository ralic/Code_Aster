      SUBROUTINE TE0441 ( OPTION , NOMTE )
      IMPLICIT   NONE
      CHARACTER*16        OPTION , NOMTE
C ......................................................................
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 05/08/98   AUTEUR CIBHHLV L.VIVAN 
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
C    - FONCTION REALISEE:  CALCUL DES VECTEURS ELEMENTAIRES
C                          POUR ELEMENTS INCOMPRESSIBLES
C                          OPTION : 'CHAR_MECA_PRES_R  '
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
      INTEGER        NNO1,NNO2,KP,NPG,ICARAC,IFF,IPOIDS,IGEOM
      INTEGER        IPRES,IVECTU,K,I,IVF1,IDFDE1
      REAL*8         POIDS,R,TX,TY,NX,NY,PRES,CISA
      CHARACTER*24   CARAC,FF
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      CARAC = '&INEL.'//NOMTE(1:8)//'.CARAC'
      CALL JEVETE ( CARAC, 'L', ICARAC )
      NNO1 = ZI(ICARAC)
      NNO2 = ZI(ICARAC+1)
      NPG  = ZI(ICARAC+2)
C
      FF = '&INEL.'//NOMTE(1:8)//'.FF'
      CALL JEVETE ( FF, 'L', IFF )
      IPOIDS = IFF
      IVF1   = IPOIDS +NPG
      IDFDE1 = IVF1   +NPG*NNO1
C
      CALL JEVECH ( 'PGEOMER', 'L', IGEOM  )
      CALL JEVECH ( 'PPRESSR', 'L', IPRES  )
      CALL JEVECH ( 'PVECTUR', 'E', IVECTU )
C
      DO 100 KP = 1 , NPG
         K = (KP-1)*NNO1
         CALL VFF2DN ( NNO1, ZR(IPOIDS+KP-1), ZR(IDFDE1+K), ZR(IGEOM),
     &                 NX, NY, POIDS )
         IF ( NOMTE(3:4) .EQ. 'AX' ) THEN
            R    = 0.D0
            PRES = 0.D0
            CISA = 0.D0
            DO 102 I = 1 , NNO1
               R    = R    + ZR(IGEOM+2*(I-1))  *ZR(IVF1+K+I-1)
               PRES = PRES + ZR(IPRES+2*(I-1))  *ZR(IVF1+K+I-1)
               CISA = CISA + ZR(IPRES+2*(I-1)+1)*ZR(IVF1+K+I-1)
 102        CONTINUE
            POIDS = POIDS*R
         ELSE
            PRES = 0.D0
            CISA = 0.D0
            DO 104 I = 1 , NNO1
               PRES = PRES + ZR(IPRES+2*(I-1))  *ZR(IVF1+K+I-1)
               CISA = CISA + ZR(IPRES+2*(I-1)+1)*ZR(IVF1+K+I-1)
 104        CONTINUE
         ENDIF
C
         TX = -NX*PRES -NY*CISA
         TY = -NY*PRES +NX*CISA
C
         DO 106 I = 1,NNO2
           ZR(IVECTU+3*I-3) = ZR(IVECTU+3*I-3) + TX*ZR(IVF1+K+I-1)*POIDS
           ZR(IVECTU+3*I-2) = ZR(IVECTU+3*I-2) + TY*ZR(IVF1+K+I-1)*POIDS
           ZR(IVECTU+3*I-1) = 0.D0
 106     CONTINUE
C
         DO 108 I = NNO2+1 , NNO1
            ZR(IVECTU+NNO2+2*I-2) = ZR(IVECTU+NNO2+2*I-2)
     &                                + TX*ZR(IVF1+K+I-1)*POIDS
            ZR(IVECTU+NNO2+2*I-1) = ZR(IVECTU+NNO2+2*I-1)
     &                                + TY*ZR(IVF1+K+I-1)*POIDS
 108     CONTINUE
 100  CONTINUE
C
      CALL JEDEMA()
      END
