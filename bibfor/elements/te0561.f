      SUBROUTINE TE0561 ( OPTION , NOMTE )
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
C ----------------------------------------------------------------------
C     BUT: CALCUL DES CONTRAINTES ALPHA0 (METHODE ZAC) AUX PG
C          ELEMENTS ISOPARAMETRIQUES 2D
C
C     IN   OPTION : OPTIONS DE CALCUL
C                   'ALPH_ELGA_ZAC'
C          NOMTE  : NOM DU TYPE ELEMENT
C ----------------------------------------------------------------------
      PARAMETER         ( NBRES = 4 , NCMP = 4)
      CHARACTER*24       CARAC,FF,CHMAT
      CHARACTER*8        NOMRES(NBRES),ELREFE
      CHARACTER*2        CODRET(NBRES)
      REAL*8             VALRES(NBRES)
      REAL*8             DEFOPP(54),DEFORP(54),DEFORE(54),DEFOTH
      REAL*8             C1, C2, C, S
      REAL*8             TREF,TRCP
      REAL*8             DFDX(9),DFDY(9),TPG,POIDS,X,Y,EPSP(5),EPSE(5)
      INTEGER            NNO,KP,I,K,ITEMPE,ITREF,IDEPLP,IDEPLE,IALPHA
      INTEGER            ICARAC,IFF,IPOIDS,IVF,IDFDE,IDFDK,IGEOM,IMATE
      INTEGER            NPG,NPG1,ICONTP
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
C
      CALL ELREF1(ELREFE)
C
      CARAC='&INEL.'//ELREFE//'.CARAC'
      CALL JEVETE(CARAC,'L',ICARAC)
      NNO  = ZI(ICARAC)
      NPG1 = ZI(ICARAC+2)
C
      FF   ='&INEL.'//ELREFE//'.FF'
      CALL JEVETE(FF,'L',IFF)
      IPOIDS = IFF
      IVF    = IPOIDS+NPG1
      IDFDE  = IVF   +NPG1*NNO
      IDFDK  = IDFDE +NPG1*NNO
      NPG    = NPG1
C
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PMATERC','L',IMATE)
      CALL JEVECH('PTEMPER','L',ITEMPE)
      CALL JEVECH('PTEREF' ,'L',ITEREF)
      CALL TECACH('ONN','PDEPLAP',1,IDEPLP,IRET)
      CALL TECACH('ONN','PCONTRP',1,ICONTP,IRET)
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
        DO 101 KP = 1 , NPG
          K=(KP-1)*NNO
          CALL DFDM2D ( NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IDFDK+K),
     &                  ZR(IGEOM),DFDX,DFDY,POIDS )
          X   = 0.D0
          Y   = 0.D0
          TPG = 0.D0
          DO 102 I=1,NNO
            X   = X   + ZR(IGEOM+2*I-2)*ZR(IVF+K+I-1)
            Y   = Y   + ZR(IGEOM+2*I-1)*ZR(IVF+K+I-1)
            TPG = TPG + ZR(ITEMPE+I-1) *ZR(IVF+K+I-1)
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
          DO 103 I=1,5
            EPSP(I) = 0.0D0
            EPSE(I) = 0.0D0
103       CONTINUE
          DO 104 I=1,NNO
            EPSP(1) = EPSP(1) + DFDX(I) * ZR(IDEPLP+2*(I-1)  )
            EPSP(2) = EPSP(2) + DFDY(I) * ZR(IDEPLP+2*(I-1)+1)
            EPSP(3) = EPSP(3) + ZR(IVF+K+I-1)*ZR(IDEPLP+2*(I-1))
            EPSP(4) = EPSP(4) + DFDY(I) * ZR(IDEPLP+2*(I-1)  )
            EPSP(5) = EPSP(5) + DFDX(I) * ZR(IDEPLP+2*(I-1)+1)
            EPSE(1) = EPSE(1) + DFDX(I) * ZR(IDEPLE+2*(I-1)  )
            EPSE(2) = EPSE(2) + DFDY(I) * ZR(IDEPLE+2*(I-1)+1)
            EPSE(3) = EPSE(3) + ZR(IVF+K+I-1)*ZR(IDEPLE+2*(I-1))
            EPSE(4) = EPSE(4) + DFDY(I) * ZR(IDEPLE+2*(I-1)  )
            EPSE(5) = EPSE(5) + DFDX(I) * ZR(IDEPLE+2*(I-1)+1)
104       CONTINUE
C
          IDPG = (KP-1)*NCMP
C
          IF ( NOMTE(3:4) .EQ. 'AX' ) THEN
            IF ( X .NE. 0.D0 ) THEN
              EPSP(3) = EPSP(3) / X
              EPSE(3) = EPSE(3) / X
            ELSE
              EPSP(3) = EPSP(1)
              EPSE(3) = EPSE(1)
            ENDIF
            DEFORP(IDPG+3) = EPSP(3)
            DEFORE(IDPG+3) = EPSE(3)
          ELSE IF (NOMTE(3:4) .EQ. 'CP' ) THEN
            DEFORP(IDPG+3)   = EPSP(3)
            DEFORE(IDPG+3)   = EPSE(3)
          ELSE
            DEFORP(IDPG+3) = 0.D0
            DEFORE(IDPG+3) = 0.D0
          ENDIF
C
          DEFORP(IDPG+1) =  EPSP(1)
          DEFORP(IDPG+2) =  EPSP(2)
          DEFORP(IDPG+4) = (EPSP(4)+EPSP(5))/2.D0
          DEFORE(IDPG+1) =  EPSE(1)
          DEFORE(IDPG+2) =  EPSE(2)
          DEFORE(IDPG+4) = (EPSE(4)+EPSE(5))/2.D0
C
          TREF   = ZR(ITEREF)
          DEFOTH = VALRES(3)*(TPG-TREF)
          IF (NOMTE(3:4) .EQ. 'CP' ) THEN
            TRCP = ZR(ICONTP+IDPG)+ZR(ICONTP+IDPG+1)
          ELSE
            TRCP = ZR(ICONTP+IDPG)+ZR(ICONTP+IDPG+1)+ZR(ICONTP+IDPG+2)
          ENDIF
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
          IF (NOMTE(3:4) .EQ. 'CP' ) THEN
            DEFOPP(IDPG+3)=-999999.D0
          ELSE
            DEFOPP(IDPG+3)=
     &        C*(DEFORP(IDPG+3)-DEFOTH-C1*ZR(ICONTP+IDPG+2)+C2*TRCP)
     &           - (ZR(ICONTP+IDPG+2) - TRCP/3.D0)
     &           + (DEFORE(IDPG+3) - TREL/3.D0)/C1
          ENDIF
C
          DEFOPP(IDPG+4)=C*(DEFORP(IDPG+4)       -C1*ZR(ICONTP+IDPG+3))
     &           - ZR(ICONTP+IDPG+3) + DEFORE(IDPG+4)/C1
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
