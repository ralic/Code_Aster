       SUBROUTINE TE0447 ( OPTION , NOMTE )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 26/06/2002   AUTEUR SMICHEL S.MICHEL-PONNELLE 
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
       IMPLICIT NONE
       CHARACTER*16        OPTION , NOMTE

C ......................................................................
C    - FONCTION REALISEE:  CALCUL DES DEFORMATIONS ELEMENTAIRES 2D
C                          DES ELEMENTS ISOPARAMETRIQUES
C                          OPTION : 'EPSI_ELGA_DEPL      '
C                          OPTION : 'EPSI_ELNO_DEPL      '

C
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
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

      LOGICAL       AXI, GRAND
      INTEGER       KPG,KSIG,NNO1,NNO2, NPG, IPOIDS, IVF,  NDIM, NCMP
      INTEGER       IDFDE, IDFDK,ICARAC, IFF, IDEPL, IGEOM, IDEFO, KK
      INTEGER       ICARA2
      REAL*8        RBID, POIDS, DFDI(81), F(3,3), R, EPS(4), VPG(36)
      REAL*8        TMP
      CHARACTER*8  ELREFE,ELREF2
      CHARACTER*24  CARAC,FF,CARAC2
C ......................................................................

      NDIM = 2
      NCMP = 2*NDIM
      CALL ELREF1(ELREFE)
      IF ( NOMTE(5:8).EQ.'TR6') THEN
        ELREF2 = 'TRII3'
      ELSEIF ( NOMTE(5:8)  .EQ. 'QU8'  ) THEN
        ELREF2 = 'QUAI4'
      ELSE
        CALL UTMESS('F','TE0447','ELEMENT:'//NOMTE(5:8)//
     +                'NON IMPLANTE')
      ENDIF   
      CARAC='&INEL.'//ELREFE//'.CARAC'
      CALL JEVETE(CARAC,'L',ICARAC)
      NNO1 = ZI(ICARAC)
      NPG  = ZI(ICARAC+2)
      FF = '&INEL.'//ELREFE//'.FF'
      CALL JEVETE(FF,'L',IFF)
      IPOIDS = IFF
      IVF    = IPOIDS + NPG
      IDFDE  = IVF + NPG*NNO1
      IDFDK  = IDFDE + NPG*NNO1
      
      CARAC2 = '&INEL.'//ELREF2//'.CARAC'
      CALL JEVETE(CARAC2,'L',ICARA2)
      NNO2 = ZI(ICARA2)

      AXI   = NOMTE(3:4).EQ.'AX'
      GRAND = .FALSE.
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PDEPLAR','L',IDEPL)
      CALL JEVECH('PDEFORR','E',IDEFO)

      CALL R8INIR(4,0.D0,EPS,1)
      CALL R8INIR(36,0.D0,VPG,1)

      DO 10 KPG = 1, NPG

        CALL NMGEOM(NDIM,NNO1,AXI,GRAND,ZR(IGEOM),KPG,ZR(IPOIDS-1+KPG),
     &              ZR(IVF+NNO1*(KPG-1)),ZR(IDFDE),RBID,ZR(IDFDK),
     &              ZR(IDEPL),POIDS,DFDI,F,EPS,R)

C       RECUPERATION DE LA DEFORMATION
        DO 20 KSIG=1,NCMP
          IF (KSIG.LE.3) THEN
            TMP=1.D0
          ELSE
            TMP = SQRT(2.D0)
          ENDIF
          VPG(NCMP*(KPG-1)+KSIG)=EPS(KSIG)/TMP
 20     CONTINUE
 10   CONTINUE

C      AFFECTATION DU VECTEUR EN SORTIE
C         (DEFORMATIONS AUX POINTS DE GAUSS OU AUX NOEUDS)
      IF (OPTION(6:9).EQ.'ELGA') THEN
        DO 30 KK = 1,NPG*NCMP
            ZR(IDEFO+KK-1)= VPG(KK)
 30     CONTINUE
      ELSEIF (OPTION(6:9).EQ.'ELNO') THEN
        CALL PPGANO(NNO2,NPG,NCMP,VPG,ZR(IDEFO))
      ENDIF

      END
