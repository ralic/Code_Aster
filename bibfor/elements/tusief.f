      SUBROUTINE TUSIEF(OPTION,ELREFE,NBRDDL,B,VIN,MAT,PASS,VTEMP)
      IMPLICIT NONE
C MODIF ELEMENTS  DATE 03/07/2002   AUTEUR CIBHHPD D.NUNEZ 
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
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
      CHARACTER*16        OPTION , NOMTE
C ......................................................................
C
C    - FONCTION REALISEE:  CALCUL DES OPTIONS EPSI_ELGA_DEPL
C                          ET SIEF_ELGA_DEPL POUR UN TUYAU DROIT
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
C
      INTEGER            NBRES,NBRDDL
      PARAMETER          (NBRES=9)
      CHARACTER*24       CARAC,FF
      CHARACTER*8        NOMRES(NBRES),NOMPAR,NOMPU(2)
      CHARACTER*8        ELREFE
      CHARACTER*2        CODRET(NBRES)
      REAL*8             VALRES(NBRES),VALPAR,VALPU(2)
      REAL*8             H,A,L,E,NU,BETA,CISAIL,G,OMEGA
      REAL*8             SINFI,FI,DEUXPI,R,R8PI,AT1,AT2
      REAL*8             B(4,NBRDDL),C(4,4),ALPHA,TMOY1,TINF1,TSUP1
      REAL*8             TEMPGM(4),TEMPGS(4),TEMPGI(4),TREF,HK,COE1
      REAL*8             TMOY(4),TINF(4),TSUP(4),SIGTH(2),XPG(4)
      REAL*8             PGL(3,3),VIN(NBRDDL),VOUT(4),MAT(4,NBRDDL)
      REAL*8             PGL1(3,3),PGL2(3,3),PGL3(3,3),RAYON,THETA
      REAL*8             VTEMP(NBRDDL),PASS(NBRDDL,NBRDDL),PGL4(3,3)
      INTEGER            NNO,NPG,NBCOU,NBSEC,M,ICARAC,ICOUDE
      INTEGER            IPOIDS,IVF,KPGS,IBID,IER,K,JTREF
      INTEGER            IMATE,ITEMP,ICAGEP,IGEOM,NBPAR
      INTEGER            IGAU,ICOU,ISECT,I,J,JIN,JOUT,IRET,INDICE
      INTEGER            IFF,LORIEN,ICOUD2,MMT,JNBSPI
      INTEGER            NPG1,IXF,ITAB(8)
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
      DEUXPI=2.D0*R8PI()

      CALL JEVECH('PNBSP_I','L',JNBSPI)
      NBCOU=ZI(JNBSPI-1+1)
      NBSEC=ZI(JNBSPI-1+2)

C
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PCAGEPO','L',ICAGEP)
      H  =ZR(ICAGEP+1)
      A  =ZR(ICAGEP  ) - H/2.D0
C A= RMOY, H = EPAISSEUR
      CARAC='&INEL.'//ELREFE//'.CARAC'
      CALL JEVETE(CARAC,'L',ICARAC)
      FF   ='&INEL.'//ELREFE//'.FF'
      CALL JEVETE(FF,'L',IFF)
C
      NNO  = ZI(ICARAC  )
      NPG1 = ZI(ICARAC+2)
      M     = ZI(ICARAC+6)
C
         NPG = NPG1
         IXF= IFF

      DO 55 I=1,NPG
         XPG(I)=ZR(IXF-1+I)
55    CONTINUE
C
      IPOIDS = IXF+NPG
      IVF = IPOIDS + NPG
C
C     --- RECUPERATION DES ORIENTATIONS ---
C
      CALL JEVECH ('PCAORIE', 'L',LORIEN)
      CALL CARCOU(ZR(LORIEN),L,PGL,RAYON,THETA,PGL1,PGL2,PGL3,PGL4,NNO,
     &            OMEGA,  ICOUD2)
      IF (ICOUD2.GE.10) THEN
         ICOUDE = ICOUD2 - 10
         MMT=0
      ELSE
         ICOUDE = ICOUD2
         MMT=1
      ENDIF


      CALL JEVECH('PMATERC','L',IMATE)
      NOMRES(1) = 'E'
      NOMRES(2) = 'NU'
      NOMRES(3) = 'ALPHA'
C
      IF (OPTION(1:14) .EQ. 'SIEF_ELGA_DEPL') THEN
         CALL TECAC2 ('NNN','PTEMPER',8,ITAB,IRET)
         ITEMP=ITAB(1)
         IF ( ITEMP .EQ. 0 ) THEN
            NBPAR  = 0
            NOMPAR = ' '
            VALPAR = 0.D0
         ELSE
            NBPAR  = 1
            NOMPAR = 'TEMP'
            CALL DXTPIF(ZR(ITEMP),ZL(ITAB(8)))
            VALPAR = ZR(ITEMP)
         ENDIF
C
         CALL RCVALA ( ZI(IMATE),'ELAS',NBPAR,NOMPAR,VALPAR,
     &               2,NOMRES,VALRES,CODRET,'FM')
         CALL RCVALA ( ZI(IMATE),'ELAS',NBPAR,NOMPAR,VALPAR,
     &               1,NOMRES(3),VALRES(3),CODRET(3),'  ')
         E   = VALRES(1)
         NU  = VALRES(2)
         IF (CODRET(3).NE.'OK') THEN
            ALPHA = 0.D0
         ELSE
            ALPHA = VALRES(3)
         ENDIF
C
C        DEFINITION DE LA MATRICE DE COMPORTEMENT C
C
         BETA    = E/(1.D0-NU**2)
         G       = E/(2.D0*(1.D0+NU))
         CISAIL  = 1.D0
C
         C(1,1) = BETA
         C(1,2) = NU*BETA
         C(1,3) = 0.D0
         C(1,4) = 0.D0
C
         C(2,1) = NU*BETA
         C(2,2) = BETA
         C(2,3) = 0.D0
         C(2,4) = 0.D0
C
         C(3,1) = 0.D0
         C(3,2) = 0.D0
         C(3,3) = G
         C(3,4) = 0.D0
C
         C(4,1) = 0.D0
         C(4,2) = 0.D0
         C(4,3) = 0.D0
         C(4,4) = G*CISAIL
C
C        FIN DE LA CONSTRUCTION DE LA MATRICE DE COMPORTEMENT C
C
      ENDIF
      CALL JEVECH('PDEPLAR','L',JIN)
      DO 01 I=1, NBRDDL
         VIN(I) = ZR(JIN-1+I)
01     CONTINUE
      IF (ICOUDE.EQ.0) THEN
         CALL VLGGL(NNO,NBRDDL,PGL,VIN,'GL',PASS,VTEMP)
      ELSE
         CALL VLGGLC(NNO,NBRDDL,PGL1,PGL2,PGL3,PGL4,VIN,'GL',PASS,VTEMP)
      ENDIF
C
      IF (OPTION(1:14) .EQ. 'EPSI_ELGA_DEPL') THEN
C
         CALL JEVECH('PDEFORR','E',JOUT)
C
      ELSEIF (OPTION(1:14) .EQ. 'SIEF_ELGA_DEPL') THEN
C
C===============================================================
C        -- RECUPERATION DE LA TEMPERATURE :
C        -- SI LA TEMPERATURE EST CONNUE AUX NOEUDS :
         CALL TECAC2 ('NNN','PTEMPER',8,ITAB,IRET)
         ITEMP=ITAB(1)
         IF (ITEMP.GT.0) THEN
            DO 02 I = 1,NNO
               CALL DXTPIF(ZR(ITEMP+3*(I-1)),ZL(ITAB(8)+3*(I-1)))
               TMOY(I) = ZR(ITEMP+3* (I-1))
               TINF(I) = ZR(ITEMP+3* (I-1)+1)
               TSUP(I) = ZR(ITEMP+3* (I-1)+2)
02          CONTINUE
         END IF
C        -- SI LA TEMPERATURE EST UNE FONCTION DE 'INST' ET 'EPAIS'
         CALL TECACH(.FALSE.,.FALSE.,'PTEMPEF',1,ITEMP)
         IF (ITEMP.GT.0) THEN
            NOMPU(1) = 'INST'
            NOMPU(2) = 'EPAIS'
            CALL JEVECH('PTEMPSR','L',IBID)
            VALPU(1) = ZR(IBID)
            VALPU(2) = 0.D0
            CALL FOINTE('FM',ZK8(ITEMP),2,NOMPU,VALPU,TMOY1,IER)
            VALPU(2) = -H/2.D0
            CALL FOINTE('FM',ZK8(ITEMP),2,NOMPU,VALPU,TINF1,IER)
            VALPU(2) = +H/2.D0
            CALL FOINTE('FM',ZK8(ITEMP),2,NOMPU,VALPU,TSUP1,IER)
            DO 03,I = 1,NNO
               TMOY(I) = TMOY1
               TINF(I) = TINF1
               TSUP(I) = TSUP1
03          CONTINUE
          END IF
C
C         PASSAGE DE LA TEMPERATURE DES NOEUDS AUX POINTS DE GAUSS
C
          DO 05, IGAU=1, NPG
             TEMPGI(IGAU) = 0.D0
             TEMPGM(IGAU) = 0.D0
             TEMPGS(IGAU) = 0.D0
             DO 06, K=1, NNO
                HK   = ZR(IVF-1+NNO*(IGAU-1)+K)
                TEMPGI(IGAU) = HK * TINF(K) + TEMPGI(IGAU)
                TEMPGM(IGAU) = HK * TMOY(K) + TEMPGM(IGAU)
                TEMPGS(IGAU) = HK * TSUP(K) + TEMPGS(IGAU)
06           CONTINUE
05        CONTINUE
C
          CALL JEVECH('PTEREF','L',JTREF)
          TREF = ZR(JTREF)
C===============================================================
C
          CALL JEVECH('PCONTRR','E',JOUT)
C
      ELSE
         CALL UTMESS('F','TE0584',
     &             'L''OPTION "'//OPTION//'" EST NON PREVUE')
      ENDIF
C
C
C BOUCLE SUR LES POINTS DE GAUSS
C
C BOUCLE SUR LES POINTS DE SIMPSON DANS L'EPAISSEUR
C
      KPGS = 0
      SIGTH(1) = 0.D0
      SIGTH(2) = 0.D0
      DO 10 IGAU=1, NPG
         IF (OPTION(1:14) .EQ. 'SIEF_ELGA_DEPL') THEN
            COE1=(TEMPGI(IGAU)+TEMPGS(IGAU)+TEMPGM(IGAU))/3.D0-TREF
            AT1  = (C(1,1)+C(1,2))*COE1*ALPHA
            AT2  = (C(2,1)+C(2,2))*COE1*ALPHA
         ENDIF
C
      DO 20 ICOU = 1, 2*NBCOU+1
C
C BOUCLE SUR LES POINTS DE SIMPSON SUR LA CIRCONFERENCE
C
          IF (MMT.EQ.0) THEN
              R      = A
          ELSE
              R = A + (ICOU-1)*H/ (2.D0*NBCOU) - H/2.D0
          ENDIF

            DO 30 ISECT=1, 2*NBSEC+1
               KPGS = KPGS + 1
C
               IF(ICOUDE.EQ.0) THEN
                  CALL BCOUDE (IGAU,ICOU,ISECT,L,H,A,M,OMEGA,
     &                      NPG,NNO,NBCOU,NBSEC,ZR(IVF),MMT,B)
               ELSEIF(ICOUDE.EQ.1) THEN
                  FI     = (ISECT-1)*DEUXPI/(2.D0*NBSEC)
C                  FI = FI - OMEGA
                  SINFI  = SIN(FI)
                   L = THETA*(RAYON+R*SINFI)
                  CALL BCOUDC (IGAU,ICOU,ISECT,L,H,A,M,OMEGA,
     &              NPG,XPG,NNO,NBCOU,NBSEC,ZR(IVF),RAYON,THETA,MMT,B)
               ENDIF
               IF (OPTION(1:14) .EQ. 'EPSI_ELGA_DEPL') THEN
               DO 40 I=1, 4
                  DO 50 J=1, NBRDDL
                     MAT(I,J)=B(I,J)
50                CONTINUE
40             CONTINUE
               ELSEIF (OPTION(1:14) .EQ. 'SIEF_ELGA_DEPL') THEN
                  SIGTH(1) = AT1
                  SIGTH(2) = AT2
                  CALL PROMAT(C,4,4,4,B,4,4,NBRDDL,MAT)
               ENDIF
               IRET=0
               CALL PRODMV(0,MAT,4,4,NBRDDL,VIN,
     &                  NBRDDL,VOUT,4,IRET)
C
C  STOCKAGE DU VECTEUR VOUT
C
            INDICE = JOUT - 1 + 6*(KPGS-1)
C
            ZR(INDICE+1) = VOUT(1)-SIGTH(1)
            ZR(INDICE+2) = VOUT(2)-SIGTH(2)
            ZR(INDICE+3) = 0.D0
            ZR(INDICE+4) = VOUT(3)
            ZR(INDICE+5) = VOUT(4)
            ZR(INDICE+6) = 0.D0
C
C  FIN STOCKAGE
C
30             CONTINUE
20       CONTINUE
10    CONTINUE
C
C
      END
