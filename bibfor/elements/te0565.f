      SUBROUTINE TE0565 ( OPTION , NOMTE )
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
C ----------------------------------------------------------------------
C     BUT: CALCUL DES CONTRAINTES ALPHA0 (METHODE ZAC) AUX PG
C          ELEMENTS ISOPARAMETRIQUES 3D
C
C     IN   OPTION : OPTIONS DE CALCUL
C                   'ALPH_ELGA_ZAC'
C          NOMTE  : NOM DU TYPE ELEMENT
C ----------------------------------------------------------------------
      PARAMETER         ( NBRES = 4 , NCMP = 6)
      CHARACTER*24       CHVAL,CHCTE
      CHARACTER*8        NOMRES(NBRES),ELREFE
      CHARACTER*2        CODRET(NBRES)
      REAL*8             VALRES(NBRES)
      REAL*8             DEFOPP(162),DEFORP(162),DEFORE(162),DEFOTH
      REAL*8             C1, C2, C, S
      REAL*8             TREF,TRCP
      REAL*8             DFDX(27),DFDY(27),DFDZ(27)
      REAL*8             TPG,POIDS,UP(3,27),UE(3,27)
      INTEGER            NNO,KP,I,K,ITEMPE,ITREF,IDEPLP,IDEPLE,IALPHA
      INTEGER            IPOIDS,IVF,IDFDE,IDFDN,IDFDK,IGEOM,IMATE
      INTEGER            NPG,ICONTP,NBPG(10)
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
      CHCTE = '&INEL.'//ELREFE//'.CARACTE'
      CALL JEVETE(CHCTE,'L',JIN)
      NDIM = ZI(JIN+1-1)
      NNO = ZI(JIN+2-1)
      NBFPG = ZI(JIN+3-1)
      DO 110 I = 1,NBFPG
        NBPG(I) = ZI(JIN+3-1+I)
110   CONTINUE
C
      NPG = NBPG(1)
C
      CHVAL = '&INEL.'//ELREFE//'.FFORMES'
      CALL JEVETE(CHVAL,'L',JVAL)
C
      IPOIDS = JVAL + (NDIM+1)*NNO*NNO

CJMP   PARAIT FAUX =>  ON INTERDIT
C      CALL UTMESS('F','POST_ZAC','NE MARCHE PAS ENCORE EN 3D')

C      IF(ELREFE.EQ.'TETRA10 '.OR.ELREFE.EQ.'HEXA20  ' ) THEN
C        IPOIDS = IPOIDS + NBPG(1)*(1+(NDIM+1)*NNO)
C     &                  + NBPG(2)*(1+(NDIM+1)*NNO)
C       ELSE IF(ELREFE.EQ.'PENTA15 ' ) THEN
C        IPOIDS = IPOIDS + NBPG(1)*(1+(NDIM+1)*NNO)
C      ENDIF

      IVF = IPOIDS + NPG
      IDFDE  = IVF + NPG*NNO
      IDFDN  = IDFDE  + 1
      IDFDK  = IDFDN  + 1
C
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PMATERC','L',IMATE)
      CALL JEVECH('PTEMPER','L',ITEMPE)
      CALL JEVECH('PTEREF' ,'L',ITEREF)
      CALL TECACH(.TRUE.,.FALSE.,'PDEPLAP',1,IDEPLP)
      CALL TECACH(.TRUE.,.FALSE.,'PCONTRP',1,ICONTP)
      CALL JEVECH('PDEPLAE','L',IDEPLE)
      CALL JEVECH('PALPHAR','E',IALPHA)
C
      IF (IDEPLP.NE.0) THEN
C
C    ETAT INITIAL NON NUL (DONNEE ELASTOPLASTIQUE)
C
        NOMRES(1) = 'E'
        NOMRES(2) = 'NU'
        NOMRES(3) = 'ALPHA'
        NOMRES(4) = 'D_SIGM_ESPI'
C
        DO 111 I = 1,NCMP*NPG
          DEFORP(I) = 0.D0
          DEFORE(I) = 0.D0
          DEFOPP(I) = 0.D0
111     CONTINUE
C
      DO 113 I=1,NNO
        UP(1,I) = ZR(IDEPLP + 3 * I - 3)
        UP(2,I) = ZR(IDEPLP + 3 * I - 2)
        UP(3,I) = ZR(IDEPLP + 3 * I - 1)
        UE(1,I) = ZR(IDEPLE + 3 * I - 3)
        UE(2,I) = ZR(IDEPLE + 3 * I - 2)
        UE(3,I) = ZR(IDEPLE + 3 * I - 1)
113   CONTINUE
C
        DO 101 KP = 1 , NPG
C
          K  = (KP-1)* 3 * NNO
          IT = (KP-1) * NNO
          IDPG = (KP-1)*NCMP
C
          CALL DFDM3D ( NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IDFDN+K),
     &                  ZR(IDFDK+K),ZR(IGEOM),DFDX,DFDY,DFDZ,POIDS )
          TPG = 0.D0
          DO 102 I=1,NNO
            TPG = TPG + ZR(ITEMPE+I-1) *ZR(IVF+IT+I-1)
102       CONTINUE
          CALL RCVALA ( ZI(IMATE),'ELAS',1,'TEMP',TPG,2,NOMRES,
     &                  VALRES, CODRET, 'FM' )
          CALL RCVALA ( ZI(IMATE),'ELAS',1,'TEMP',TPG,1,NOMRES(3),
     &                  VALRES(3), CODRET(3), '  ' )
          IF ( CODRET(3) .NE. 'OK' ) VALRES(3) = 0.D0
          CALL RCVALA ( ZI(IMATE),'ECRO_LINE',1,'TEMP',TPG,1,NOMRES(4),
     &                  VALRES(4), CODRET(4), 'FM' )
C
          C = 2.D0/3.D0*(VALRES(1)*VALRES(4))/(VALRES(1)-VALRES(4))
C
          DO 106 I=1,NNO
            DEFORE(IDPG+1) = DEFORE(IDPG+1) + UE(1,I) * DFDX(I)
            DEFORE(IDPG+2) = DEFORE(IDPG+2) + UE(2,I) * DFDY(I)
            DEFORE(IDPG+3) = DEFORE(IDPG+3) + UE(3,I) * DFDZ(I)
            DEFORE(IDPG+4) = DEFORE(IDPG+4) +(UE(2,I) * DFDX(I)
     &                                      + UE(1,I) * DFDY(I))*0.5D0
            DEFORE(IDPG+5) = DEFORE(IDPG+5) +(UE(1,I) * DFDZ(I)
     &                                      + UE(3,I) * DFDX(I))*0.5D0
            DEFORE(IDPG+6) = DEFORE(IDPG+6) +(UE(2,I) * DFDZ(I)
     &                                      + UE(3,I) * DFDY(I))*0.5D0
            DEFORP(IDPG+1) = DEFORP(IDPG+1) + UP(1,I) * DFDX(I)
            DEFORP(IDPG+2) = DEFORP(IDPG+2) + UP(2,I) * DFDY(I)
            DEFORP(IDPG+3) = DEFORP(IDPG+3) + UP(3,I) * DFDZ(I)
            DEFORP(IDPG+4) = DEFORP(IDPG+4) +(UP(2,I) * DFDX(I)
     &                                      + UP(1,I) * DFDY(I))*0.5D0
            DEFORP(IDPG+5) = DEFORP(IDPG+5) +(UP(1,I) * DFDZ(I)
     &                                      + UP(3,I) * DFDX(I))*0.5D0
            DEFORP(IDPG+6) = DEFORP(IDPG+6) +(UP(2,I) * DFDZ(I)
     &                                      + UP(3,I) * DFDY(I))*0.5D0
106       CONTINUE
C
          TREF   = ZR(ITEREF)
          DEFOTH = VALRES(3)*(TPG-TREF)
          TRCP = ZR(ICONTP+IDPG)+ZR(ICONTP+IDPG+1)+ZR(ICONTP+IDPG+2)
          C1  = (1.D0+VALRES(2))/VALRES(1)
          C2  = VALRES(2)/VALRES(1)
C
          TREL = DEFORE(IDPG+1) + DEFORE(IDPG+2) + DEFORE(IDPG+3)
C
          DEFOPP(IDPG+1)=
     &        C*(DEFORP(IDPG+1)-DEFOTH-C1*ZR(ICONTP+IDPG)+C2*TRCP)
     &           - (ZR(ICONTP+IDPG) - TRCP/3.D0)
     &           + (DEFORE(IDPG+1) - TREL/3.D0)/C1
C
          DEFOPP(IDPG+2)=
     &        C*(DEFORP(IDPG+2)-DEFOTH-C1*ZR(ICONTP+IDPG+1)+C2*TRCP)
     &           - (ZR(ICONTP+IDPG+1) - TRCP/3.D0)
     &           + (DEFORE(IDPG+2) - TREL/3.D0)/C1
C
          DEFOPP(IDPG+3)=
     &        C*(DEFORP(IDPG+3)-DEFOTH-C1*ZR(ICONTP+IDPG+2)+C2*TRCP)
     &           - (ZR(ICONTP+IDPG+2) - TRCP/3.D0)
     &           + (DEFORE(IDPG+3) - TREL/3.D0)/C1
C
          DEFOPP(IDPG+4)=C*(DEFORP(IDPG+4)       -C1*ZR(ICONTP+IDPG+3))
     &           - ZR(ICONTP+IDPG+3) + DEFORE(IDPG+4)/C1
C
          DEFOPP(IDPG+5)=C*(DEFORP(IDPG+5)       -C1*ZR(ICONTP+IDPG+4))
     &           - ZR(ICONTP+IDPG+4) + DEFORE(IDPG+5)/C1
C
          DEFOPP(IDPG+6)=C*(DEFORP(IDPG+6)       -C1*ZR(ICONTP+IDPG+5))
     &           - ZR(ICONTP+IDPG+5) + DEFORE(IDPG+6)/C1
C
 101    CONTINUE
C
C       TENSEUR ALPHA0 AUX PG
C
        DO 198 KP = 1,NPG
          DO 198 J  = 1,NCMP
            ZR(IALPHA+(KP-1)*NCMP-1+J) = DEFOPP((KP-1)*NCMP+J)
198     CONTINUE
C
      ELSE
C
C  ETAT INITIAL NUL (PAS DE DONNEE ELASTOPLASTIQUE)
C
        DO 199 KP = 1,NPG
          DO 199 J  = 1,NCMP
            ZR(IALPHA+(KP-1)*NCMP-1+J) = 0.D0
199     CONTINUE
C
      END IF
C
      END
