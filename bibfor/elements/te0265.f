      SUBROUTINE TE0265 ( OPTION , NOMTE )
      IMPLICIT   NONE
      CHARACTER*16       OPTION , NOMTE
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
C     BUT: CALCUL DES FLUX AUX NOEUDS
C          ELEMENTS ISOPARAMETRIQUES 2D FOURIER
C
C            OPTION : 'FLUX_ELNO_TEMP   '
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
C
      CHARACTER*24       CARAC,FF
      CHARACTER*24       CHMAT
      CHARACTER*8        ELREFE
      CHARACTER*2        CODRET
      REAL*8             VALRES,FLPG(27)
      REAL*8             DFDR(9),DFDZ(9),POIDS,R,XH,FLUXR,FLUXZ,FLUXT
      INTEGER            NNO,KP,I,J,K,ITEMPE,ITEMP,IFLUX
      INTEGER            JMAT
      INTEGER            ICARAC,IFF,IPOIDS,IVF,IDFDE,IDFDK,IGEOM,IMATE
      INTEGER            NPG,NPG1,NPG2,NPG3,NPG4,NNOS,IHARM,NH,NCMP
C
C
      CALL ELREF1(ELREFE)

      CARAC='&INEL.'//ELREFE//'.CARAC'
      CALL JEVETE(CARAC,'L',ICARAC)
      NNO  = ZI(ICARAC)
      NPG1 = ZI(ICARAC+2)
      NPG2 = ZI(ICARAC+3)
      NPG3 = ZI(ICARAC+4)
      NPG4 = ZI(ICARAC+5)
C
      FF   ='&INEL.'//ELREFE//'.FF'
      CALL JEVETE(FF,'L',IFF)
C
      IF(NOMTE(1:8).EQ.'THFOTR3 ' .OR.
     &   NOMTE(1:8).EQ.'THFOQU4 ') THEN
         NNOS = NNO
         IPOIDS = IFF   + NPG1*(1+3*NNO)
         IVF    = IPOIDS+NPG2
         IDFDE  = IVF   +NPG2*NNO
         IDFDK  = IDFDE +NPG2*NNO
         NPG    = NPG2
      ELSE IF(NOMTE(1:8).EQ.'THFOTR6 ' .OR.
     &        NOMTE(1:8).EQ.'THFOQU8 ' .OR.
     &        NOMTE(1:8).EQ.'THFOQU9 ') THEN
         NNOS = NNO/2
         IPOIDS = IFF + (NPG1+NPG2+NPG3)*(1+3*NNO)
         IVF    = IPOIDS+NPG4
         IDFDE  = IVF   +NPG4*NNO
         IDFDK  = IDFDE +NPG4*NNO
         NPG    = NPG4
      ENDIF
C
C
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PHARMON','L',IHARM)
      NH = ZI(IHARM)
      XH = DBLE(NH)
      CALL JEVECH('PMATERC','L',IMATE)
      CALL JEVECH('PTEMPSR','L',ITEMP)
      CALL JEVECH('PTEMPER','L',ITEMPE)
      CALL JEVECH('PFLUX_R','E',IFLUX)
C
      CALL RCVALA(ZI(IMATE),'THER',1,'INST',ZR(ITEMP),1,'LAMBDA',
     &            VALRES, CODRET, 'FM' )
C
       DO 101 KP=1,NPG
        K=(KP-1)*NNO
        CALL DFDM2D ( NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IDFDK+K),
     &                ZR(IGEOM),DFDR,DFDZ,POIDS )
C
           R = 0.D0
           DO 102 I=1,NNO
             R = R + ZR(IGEOM+2*I-2) * ZR(IVF+K+I-1)
102        CONTINUE
C
           FLUXR = 0.0D0
           FLUXZ = 0.0D0
           FLUXT = 0.0D0
           DO 110 J=1,NNO
                FLUXR = FLUXR + ZR(ITEMPE+J-1)*DFDR(J)
                FLUXZ = FLUXZ + ZR(ITEMPE+J-1)*DFDZ(J)
                FLUXT = FLUXT - ZR(ITEMPE+J-1)*ZR(IVF+K+J-1)*XH/R
 110       CONTINUE
C
         FLPG((KP-1)*3+1)   = -VALRES*FLUXR
         FLPG((KP-1)*3+2)   = -VALRES*FLUXZ
         FLPG((KP-1)*3+3)   = -VALRES*FLUXT
C
 101  CONTINUE
C
      NCMP = 3
      CALL PPGANO(NNOS,NPG,NCMP,FLPG,ZR(IFLUX))
C
      END
