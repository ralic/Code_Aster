      SUBROUTINE TE0031 ( OPTION , NOMTE )
      IMPLICIT NONE
      CHARACTER*16        OPTION , NOMTE
C     ----------------------------------------------------------------
C MODIF ELEMENTS  DATE 02/02/2011   AUTEUR PELLET J.PELLET 
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     CALCUL DES OPTIONS DES ELEMENTS DE PLAQUE 
C          -----------------------------------------------------------
C                                              TRIANGLE  QUADRANGLE
C        LINEAIRE          KIRCHOFF  (MINCE)        DKT       DST
C                 AVEC CISAILLEMENT  (EPAISSE)      DKQ    DSQ ET Q4G
C
C        RIGI_MECA       MASS_MECA
C        RIGI_MECA_SENSI RIGI_MECA_SENS_C
C        EPOT_ELEM  ECIN_ELEM
C        MASS_INER
C          -----------------------------------------------------------
C                                              TRIANGLE
C        LINEAIRE          KIRCHOFF  (MINCE)        DKT

C        FORC_NODA
C          -----------------------------------------------------------
C                                              TRIANGLE
C        NON LINEAIRE      KIRCHOFF  (MINCE)        DKT

C        FULL_MECA       RAPH_MECA     RIGI_MECA_TANG
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER         ZI
      COMMON /IVARJE/ ZI(1)
      REAL*8          ZR
      COMMON /RVARJE/ ZR(1)
      COMPLEX*16      ZC
      COMMON /CVARJE/ ZC(1)
      LOGICAL         ZL
      COMMON /LVARJE/ ZL(1)
      CHARACTER*8     ZK8
      CHARACTER*16            ZK16
      CHARACTER*24                     ZK24
      CHARACTER*32                              ZK32
      CHARACTER*80                                       ZK80
      COMMON /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      INTEGER      NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDX,JGANO,IND
      INTEGER      MULTIC,JTAB(7),CODRET,JDEPM,JDEPR,JVAPR
      INTEGER      ICOMPO,I1,I2,J,JVECT
      INTEGER      ICHG,ICHN,NCMP,K,JCRET,JFREQ,IACCE
      INTEGER      JMATE,JGEOM,JMATR,JENER,I,JCARA
      INTEGER      IVECT,NDDL,NVEC,IRET,ICONTP, N1, NI, N2
      INTEGER      ICOU, NBCOU,JNBSPI, IRET1, VALI(2)
      LOGICAL      LCOELA
      CHARACTER*2  CODRE2(33),CODRE1,VAL
      CHARACTER*3  NUM
      CHARACTER*8  NOMRES
      CHARACTER*10 PHENOM
C
      REAL*8        PGL(3,3), XYZL(3,4), BSIGMA(24), EFFGT(32)
      REAL*8        VECLOC(24), ENER(3), MATP(24,24), MATV(300)
      REAL*8        T2EV(4), T2VE(4), T1VE(9)
      REAL*8        EPI,EPTOT,R8BID,R8PREM, VALR(2)
C
C     ---> POUR DKT/DST MATELEM = 3 * 6 DDL = 171 TERMES STOCKAGE SYME
C     ---> POUR DKQ/DSQ MATELEM = 4 * 6 DDL = 300 TERMES STOCKAGE SYME
      REAL*8 MATLOC(300),RHO,EPAIS
C
C     ---> POUR DKT/DST EFFINT = 24
C     ---> POUR DKQ/DSQ EFFINT = 32
      REAL*8  EFFINT(32)
C
C     --->   UML : DEPLACEMENT A L'INSTANT T- (REPERE LOCAL)
C     --->   DUL : INCREMENT DE DEPLACEMENT   (REPERE LOCAL)
      REAL*8 UML(6,4),DUL(6,4)
C DEB ------------------------------------------------------------------
C
      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDX,JGANO)
C
      LCOELA = .FALSE.
      IF ( OPTION.EQ.'FULL_MECA'      .OR.
     &     OPTION.EQ.'RAPH_MECA'      .OR.
     &     OPTION(1:9).EQ.'RIGI_MECA' ) THEN

        CALL JEVECH('PMATERC','L',JMATE)

        IF ( OPTION.EQ.'FULL_MECA'      .OR.
     &     OPTION.EQ.'RAPH_MECA'      .OR.
     &     OPTION.EQ.'RIGI_MECA_TANG') THEN
          CALL RCCOMA(ZI(JMATE),'ELAS',PHENOM,CODRE2)
          IF (PHENOM.EQ.'ELAS_COQUE') LCOELA = .TRUE.
        ENDIF

C
C ---   VERIFICATION DE LA COHERENCE DES INFORMATIONS 
C ---   PROVENANT DE DEFI_COQU_MULT ET DE AFFE_CARA_ELEM
C       ----------------------------------
        JNBSPI = 0
        CALL TECACH('NNN','PNBSP_I',1,JNBSPI,IRET1)
        IF (IRET1.EQ.0) THEN
          NBCOU = ZI(JNBSPI)
          ICOU = 0
          EPTOT = 0.D0
          EPI = 0.D0
          CALL JEVECH('PCACOQU','L',JCARA)
          EPAIS  = ZR(JCARA)
  5       CONTINUE
          ICOU=ICOU+1
          CALL CODENT(ICOU,'G',NUM)
          CALL CODENT(1,'G',VAL)
          NOMRES = 'C'//NUM//'_V'//VAL
          CALL RCVALA(ZI(JMATE),' ','ELAS_COQMU',0,' ',R8BID,
     &         1,NOMRES,EPI,CODRE1,' ')
          IF (CODRE1.EQ.'OK') THEN
            EPTOT=EPTOT+EPI
            GOTO 5
          ENDIF
          IF (EPTOT.NE.0.D0) THEN
            IF ((ICOU-1).NE.NBCOU) THEN
              VALI(1) = ICOU-1
              VALI(2) = NBCOU
              CALL U2MESG('F','ELEMENTS3_51',0,' ',2,VALI,0,0.D0)
            ENDIF
            IF (ABS(EPAIS-EPTOT)/EPAIS.GT.1.D-2) THEN
              VALR(1) = EPTOT
              VALR(2) = EPAIS
              CALL U2MESG('F','ELEMENTS3_52',0,' ',0,0,2,VALR)
            ENDIF
          ENDIF
        ENDIF
      ENDIF
C
      IF ( OPTION.NE.'VARI_ELNO' ) THEN
         CALL JEVECH('PGEOMER','L',JGEOM)
         IF (NNO.EQ.3) THEN
            CALL DXTPGL ( ZR(JGEOM), PGL )
         ELSE IF (NNO.EQ.4) THEN
            CALL DXQPGL ( ZR(JGEOM), PGL )
         END IF
         CALL UTPVGL(NNO,3,PGL,ZR(JGEOM),XYZL)
      END IF
C
      IF ( OPTION.EQ.'RIGI_MECA'      .OR.
     &     OPTION.EQ.'RIGI_MECA_SENSI' .OR.
     &     OPTION.EQ.'RIGI_MECA_SENS_C' .OR.
     &     OPTION.EQ.'EPOT_ELEM' ) THEN
C     --------------------------------------
C
        IF (NOMTE.EQ.'MEDKTR3') THEN
          CALL DKTRIG(NOMTE,XYZL,OPTION,PGL,MATLOC,ENER,MULTIC)
        ELSE IF (NOMTE.EQ.'MEDSTR3') THEN
          CALL DSTRIG(NOMTE,XYZL,OPTION,PGL,MATLOC,ENER)
        ELSE IF (NOMTE.EQ.'MEDKQU4') THEN
          CALL DKQRIG(NOMTE,XYZL,OPTION,PGL,MATLOC,ENER)
        ELSE IF (NOMTE.EQ.'MEDSQU4') THEN
          CALL DSQRIG(NOMTE,XYZL,OPTION,PGL,MATLOC,ENER)
        ELSE IF (NOMTE.EQ.'MEQ4QU4') THEN
          CALL Q4GRIG(NOMTE,XYZL,OPTION,PGL,MATLOC,ENER)
        END IF

        IF (OPTION(11:14).EQ.'SENS') THEN
          NDDL = 6*NNO
          IF (OPTION(15:16).EQ.'_C') THEN
            CALL JEVECH('PVECTUC','E',JVECT)
          ELSE
            CALL JEVECH('PVECTUR','E',JVECT)
          END IF
          CALL JEVECH('PVAPRIN','L',JVAPR)
          CALL UTPSLG(NNO,6,PGL,MATLOC,MATV)
C PASSAGE VECTEUR - MATRICE
          I1 = 0
          DO 120 I = 1 , NDDL
            DO 130 J = 1 , NDDL
              I1 = I1 + 1
              MATP(I,J) = MATV(I1)
              MATP(J,I) = MATV(I1)
              IF(J.EQ.I)GOTO 120
130         CONTINUE
120       CONTINUE
          DO 100 I = 1,NDDL
            IF (OPTION(15:16).EQ.'_C') THEN
              ZC(JVECT-1+I) = DCMPLX(0.D0,0.D0)
            ELSE
              ZR(JVECT-1+I) = 0.D0
            END IF
            DO 110 J = 1,NDDL
              IF (OPTION(15:16).EQ.'_C') THEN
                ZC(JVECT-1+I) = ZC(JVECT-1+I)
     &             - MATP(I,J)*ZC(JVAPR-1+J)
              ELSE
                ZR(JVECT-1+I) = ZR(JVECT-1+I)
     &             - MATP(I,J)*ZR(JVAPR-1+J)
              END IF
110         CONTINUE
100       CONTINUE

        ELSE IF (OPTION.EQ.'RIGI_MECA') THEN
          CALL JEVECH('PMATUUR','E',JMATR)
          CALL UTPSLG(NNO,6,PGL,MATLOC,ZR(JMATR))

        ELSE IF (OPTION.EQ.'EPOT_ELEM') THEN
          CALL JEVECH('PENERDR','E',JENER)
          DO 10 I = 1,3
            ZR(JENER-1+I) = ENER(I)
   10     CONTINUE

        END IF
C
C
      ELSEIF ( OPTION.EQ.'MASS_MECA'      .OR.
     &         OPTION.EQ.'MASS_MECA_DIAG' .OR.
     &         OPTION.EQ.'MASS_MECA_EXPLI' .OR.
     &         OPTION.EQ.'M_GAMMA'        .OR.
     &         OPTION.EQ.'ECIN_ELEM' ) THEN
C     ------------------------------------------
        IF (NOMTE.EQ.'MEDKTR3') THEN
          CALL DKTMAS ( XYZL, OPTION, PGL, MATLOC, ENER, MULTIC)
        ELSE IF (NOMTE.EQ.'MEDSTR3') THEN
          CALL DSTMAS ( XYZL, OPTION, PGL, MATLOC, ENER )
        ELSE IF (NOMTE.EQ.'MEDKQU4') THEN
          CALL DKQMAS ( XYZL, OPTION, PGL, MATLOC, ENER )
        ELSE IF (NOMTE.EQ.'MEDSQU4') THEN
          CALL DSQMAS ( XYZL, OPTION, PGL, MATLOC, ENER )
        ELSE IF (NOMTE.EQ.'MEQ4QU4') THEN
          CALL Q4GMAS ( XYZL, OPTION, PGL, MATLOC, ENER )
        END IF
        IF (OPTION.EQ.'MASS_MECA') THEN
          CALL JEVECH('PMATUUR','E',JMATR)
          CALL UTPSLG(NNO,6,PGL,MATLOC,ZR(JMATR))
        ELSE IF (OPTION.EQ.'ECIN_ELEM') THEN
          CALL JEVECH('PENERCR','E',JENER)
          CALL JEVECH('PFREQR','L',JFREQ)
          DO 20 I = 1,3
            ZR(JENER-1+I) = ZR(JFREQ)*ENER(I)
   20     CONTINUE
        ELSE IF (OPTION.EQ.'M_GAMMA') THEN
          CALL JEVECH('PDEPLAR','L',IACCE)
          CALL JEVECH('PVECTUR','E',IVECT)
          NDDL = 6*NNO
          NVEC = NDDL* (NDDL+1)/2
          CALL UTPSLG(NNO,6,PGL,MATLOC,MATV)
          CALL VECMA(MATV,NVEC,MATP,NDDL)
          CALL PMAVEC('ZERO',NDDL,MATP,ZR(IACCE),ZR(IVECT))
        ELSE IF (OPTION.EQ.'MASS_MECA_DIAG' .OR.
     &           OPTION.EQ.'MASS_MECA_EXPLI' ) THEN
          CALL JEVECH('PMATUUR','E',JMATR)
          NDDL = 6*NNO
          NDIM = NDDL* (NDDL+1)/2
          DO 30 I = 1,NDIM
            ZR(JMATR-1+I) = MATLOC(I)
   30     CONTINUE

        END IF
C
C
      ELSEIF ( OPTION.EQ.'MASS_INER') THEN
C     ------------------------------------
        CALL JEVECH('PMASSINE','E',JMATR)
        CALL DXROEP(RHO,EPAIS)
        CALL DXINER(NNO,ZR(JGEOM),RHO,EPAIS,ZR(JMATR),ZR(JMATR+1),
     &              ZR(JMATR+4))
C
C     -- OPTIONS NON-LINEAIRES :
C     --------------------------
      ELSEIF ( ( OPTION(1:9).EQ.'FULL_MECA'.OR.
     &           OPTION.EQ.'RAPH_MECA'     .OR.
     &           OPTION(1:10).EQ.'RIGI_MECA_' )
     &   .AND. ( OPTION.NE.'RIGI_MECA_SENSI' .OR.
     &           OPTION.NE.'RIGI_MECA_SENS_C' ) ) THEN
C
        CALL JEVECH('PDEPLMR','L',JDEPM)
        CALL JEVECH('PDEPLPR','L',JDEPR)
        CALL JEVECH('PCOMPOR','L',ICOMPO)
        IF ( ZK16(ICOMPO+3) .EQ. 'COMP_ELAS' ) THEN
           IF (.NOT.LCOELA) THEN
             CALL U2MESS('F','ELEMENTS2_71')
           ENDIF
        ENDIF
        IF ((ZK16(ICOMPO+2) (6:10).EQ.'_REAC') .OR. 
     &      (ZK16(ICOMPO+2).EQ.'GROT_GDEP') ) THEN
C            GROT_GDEP CORRESPOND ICI A EULER_ALMANSI   

          IF(ZK16(ICOMPO+2) (6:10).EQ.'_REAC') 
     &     CALL U2MESS('A','ELEMENTS2_72')
          DO 40 I = 1,NNO
            I1 = 3* (I-1)
            I2 = 6* (I-1)
            ZR(JGEOM+I1) = ZR(JGEOM+I1) + ZR(JDEPM+I2) + ZR(JDEPR+I2)
            ZR(JGEOM+I1+1) = ZR(JGEOM+I1+1) + ZR(JDEPM+I2+1) +
     &                       ZR(JDEPR+I2+1)
            ZR(JGEOM+I1+2) = ZR(JGEOM+I1+2) + ZR(JDEPM+I2+2) +
     &                       ZR(JDEPR+I2+2)
   40     CONTINUE
C
          IF (NNO.EQ.3) THEN
            CALL DXTPGL(ZR(JGEOM),PGL)
          ELSE IF (NNO.EQ.4) THEN
            CALL DXQPGL(ZR(JGEOM),PGL)
          END IF
          CALL UTPVGL(NNO,3,PGL,ZR(JGEOM),XYZL)
C
        END IF
C
        CALL UTPVGL(NNO,6,PGL,ZR(JDEPM),UML)
        CALL UTPVGL(NNO,6,PGL,ZR(JDEPR),DUL)
C
        IF (NOMTE.EQ.'MEDKTR3') THEN
          IF (ZK16(ICOMPO+3) (1:9).EQ.'COMP_INCR') THEN
            CALL DKTNLI ( NOMTE, OPTION, XYZL, UML, DUL, VECLOC,
     &                    MATLOC, PGL, CODRET )
          ELSE
            CALL U2MESK('F','ELEMENTS2_73',1,ZK16(ICOMPO+3))
          ENDIF
        ELSE IF (NOMTE.EQ.'MEDKQU4 ') THEN
          IF (ZK16(ICOMPO+3) (1:9).EQ.'COMP_INCR') THEN
            CALL DKTNLI ( NOMTE, OPTION, XYZL, UML, DUL, VECLOC,
     &                    MATLOC, PGL, CODRET )
          ELSE
            CALL U2MESK('F','ELEMENTS2_73',1,ZK16(ICOMPO+3))
          ENDIF
        ELSE
          CALL U2MESK('F','ELEMENTS2_74',1,NOMTE)
        END IF
C
        IF (OPTION(1:9).EQ.'FULL_MECA') THEN
          CALL JEVECH('PMATUUR','E',JMATR)
          CALL JEVECH('PVECTUR','E',JVECT)
          CALL UTPSLG(NNO,6,PGL,MATLOC,ZR(JMATR))
          CALL UTPVLG(NNO,6,PGL,VECLOC,ZR(JVECT))
        ELSE IF (OPTION.EQ.'RAPH_MECA') THEN
          CALL JEVECH('PVECTUR','E',JVECT)
          CALL UTPVLG(NNO,6,PGL,VECLOC,ZR(JVECT))
        ELSE IF (OPTION(1:10).EQ.'RIGI_MECA_') THEN
          CALL JEVECH('PMATUUR','E',JMATR)
          CALL UTPSLG(NNO,6,PGL,MATLOC,ZR(JMATR))
        END IF
C
C
      ELSEIF ( OPTION.EQ.'SIEF_ELNO' ) THEN
C     ------------------------------------------
          CALL TECACH('NNN','PCOMPOR',1,ICOMPO,IRET)
          CALL JEVECH ( 'PCONTRR', 'L', ICONTP )
          IND=6
          CALL DXEFFI ( NOMTE, XYZL, PGL, ZR(ICONTP), IND, EFFINT )
          CALL DXREPE ( PGL, T2EV, T2VE, T1VE )
          CALL DXEFR2 ( NPG, T2EV, EFFINT, EFFGT )
          CALL JEVECH ( 'PSIEFNOR', 'E', ICHN   )
          CALL PPGAN2 ( JGANO, 6, EFFGT, ZR(ICHN) )
C
C
      ELSEIF ( OPTION.EQ.'VARI_ELNO' ) THEN
C     ------------------------------------------
          CALL JEVECH ( 'PVARIGR', 'L', ICHG )
          CALL JEVECH ( 'PVARINR', 'E', ICHN )
          CALL TECACH ( 'OON', 'PVARIGR', 7, JTAB, IRET )
          NCMP = JTAB(6)*JTAB(7)

          CALL PPGAN2 ( JGANO, NCMP, ZR(ICHG), ZR(ICHN))
C
C
      ELSEIF ( OPTION.EQ.'FORC_NODA' ) THEN
C     -------------------------------------

         CALL JEVECH ( 'PCONTMR', 'L', ICONTP )
         IND=8
         CALL DXEFFI ( NOMTE, XYZL, PGL, ZR(ICONTP), IND, EFFGT )
C
C ------ CALCUL DES EFFORTS INTERNES (I.E. SOMME_VOL(BT_SIG))
C        ----------------------------------------------------
C
         CALL DXBSIG ( NOMTE, XYZL, PGL, EFFGT, BSIGMA )
C
C ------ AFFECTATION DES VALEURS DE BSIGMA AU VECTEUR EN SORTIE
C        ------------------------------------------------------
         CALL JEVECH('PVECTUR','E',JVECT)
         K = 0
         DO 90 I = 1,NNO
            DO 80 J = 1,6
               K = K + 1
               ZR(JVECT+K-1) = BSIGMA(K)
   80       CONTINUE
   90    CONTINUE
      ELSE

CC OPTION DE CALCUL INVALIDE
        CALL ASSERT(.FALSE.)
      END IF
C
      IF ( OPTION(1:9).EQ.'FULL_MECA'  .OR.
     &     OPTION(1:9).EQ.'RAPH_MECA'  ) THEN
         CALL JEVECH ( 'PCODRET', 'E', JCRET )
         ZI(JCRET) = CODRET
      ENDIF
C
      END
