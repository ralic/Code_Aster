      SUBROUTINE DXSIEF ( NOMTE, XYZL, DEPL, MATER, PGL, SIGMA )
      IMPLICIT  NONE
      INTEGER         MATER
      REAL*8          XYZL(3,4), DEPL(*), PGL(3,3), SIGMA(*)
      CHARACTER*16    NOMTE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 15/06/2004   AUTEUR MABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C     ------------------------------------------------------------------
C     IN  XYZL : COORDONNEES DES NOEUDS
C     IN  UL   : DEPLACEMENT
C     IN  PGL  : MATRICE DE PASSAGE GLOBAL - LOCAL ELEMENT
C     ------------------------------------------------------------------
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

C     NNO:    NOMBRE DE NOEUDS DE L'ELEMENT
CCC      PARAMETER (NNO=3)  POUR LES DKT
CCC      PARAMETER (NNO=4)  POUR LES DKQ
      INTEGER    NNO
      PARAMETER (NNO=4)
C
C --------------------------------------------------------------------
      INTEGER      I, J, ICACOQ, ICOU, ICPG, IGAUH, INO, IPG,
     +             IRET, ITEMP, LZR, NNOEL, IBID, NBCON, NBCOU,
     +             NPG, NPGH, JNBSPI, ITAB(8), JTREF, NBPAR
      REAL*8       DISTN, ROT(9), DH(9), D(4,4), REPERE(7), INST
      REAL*8       C,S,PI,PHI,EPSL(4),R8PI
      REAL*8       HIC, H, ZIC, ZMIN, VALPU(2), TINF, TMOY, TSUP
      REAL*8       ZERO, DEUX, HYDR, SECH, TEMPG, SIG, C1, C2, C3
      REAL*8       TREF, ALPHA,E,SIGP(4),SIGL(3),R8DGRD,BETA,ALPH
      REAL*8       EPS2D(6), KHI(3), DEPF(12), DEPM(8)
      REAL*8       BF(3,3*NNO), BM(3,2*NNO), EPSM(3), EPSTH(4),EPSG(4)
      LOGICAL      TEMPNO, GRILLE, DKT, DKQ 
      CHARACTER*2  CODRET
      CHARACTER*8  NOMPAR(2)
C     ------------------------------------------------------------------
C --DEB
      ZERO = 0.0D0
      DEUX = 2.0D0
C
      DKT = .FALSE.
      DKQ = .FALSE.
      GRILLE = .FALSE.
      IF (NOMTE(1:8).EQ.'MEGRDKT ') THEN
        NPG=3
        NNOEL=3
        GRILLE = .TRUE.
        CALL JEVETE('&INEL.MEGRDKT .DESR',' ',LZR)
      ELSEIF ( NOMTE(1:8).EQ.'MEDKTR3 ' ) THEN
        NPG=3
        NNOEL=3
        DKT = .TRUE.
        CALL JEVETE('&INEL.MEDKTR3 .DESR',' ',LZR)
      ELSEIF ( NOMTE(1:8).EQ.'MEDSTR3 ' ) THEN
        NPG=3
        NNOEL=3
        DKT = .TRUE.
        CALL JEVETE('&INEL.MEDSTR3 .DESR',' ',LZR)
      ELSEIF ( NOMTE(1:8).EQ.'MEDKQU4 ' ) THEN
        NPG=4
        NNOEL=4
        DKQ = .TRUE.
        CALL JEVETE('&INEL.MEDKQU4 .DESR',' ',LZR)
      ELSEIF ( NOMTE(1:8).EQ.'MEDSQU4 ' ) THEN
        NPG=4
        NNOEL=4
        DKQ = .TRUE.
        CALL JEVETE('&INEL.MEDSQU4 .DESR',' ',LZR)
      ELSEIF ( NOMTE(1:8).EQ.'MEQ4QU4 ' ) THEN
        NPG=4
        NNOEL=4
        DKQ = .TRUE.
        CALL JEVETE('&INEL.MEQ4QU4 .DESR',' ',LZR)
      ELSE
        CALL UTMESS('F','DXSIEF','ELEMENT NON TRAITE '//NOMTE)
      END IF

      CALL TECACH ('ONN','PTEREF',8,ITAB,IRET)
      JTREF = ITAB(1)
      IF (JTREF.GT.0) THEN
         TREF = ZR(JTREF)
      ELSE
         TREF = ZERO
      END IF


C --- VARIABLE D'HYDRATATION ET DE SECHAGE
      HYDR = ZERO
      SECH = ZERO

      REPERE(1) = ZERO
      REPERE(2) = ZERO
      REPERE(3) = ZERO
      REPERE(4) = ZERO
      REPERE(5) = ZERO
      REPERE(6) = ZERO
      REPERE(7) = ZERO

C     -- GRANDEURS GEOMETRIQUES :
C     ---------------------------
      CALL JEVECH ( 'PCACOQU', 'L', ICACOQ )
      H = ZR(ICACOQ)
      DISTN = ZERO
      IF ( GRILLE ) THEN
         CALL GTRIA3(XYZL,ZR(LZR))
         DISTN = ZR(ICACOQ+3)
         CALL GRDMAT(ICACOQ,MATER,PGL,DH,ROT)
         ALPH = ZR(ICACOQ+1) * R8DGRD()
         BETA  = ZR(ICACOQ+2) * R8DGRD()
         CALL GRIROT ( ALPH , BETA ,PGL , ROT, C, S )
         PI = R8PI()
         PHI= 0.D0
C         IF(ABS(C).GT.1.D-14) PHI= (ATAN(S/C)*180.D0/PI)-90.D0
         IF(ABS(C).GT.1.D-14) PHI= (ATAN2(S,C)*180.D0/PI)-90.D0
      ELSEIF ( DKT ) THEN
         CALL GTRIA3(XYZL,ZR(LZR))
      ELSEIF ( DKQ ) THEN
         CALL GQUAD4(XYZL,ZR(LZR))
      END IF
C
      CALL TECACH ( 'ONN', 'PTEMPSR', 8, ITAB, IRET )
      IBID = ITAB(1)
      IF (IBID.GT.0) THEN
         INST = ZR(IBID)
      ELSE
         INST = ZERO
      END IF

      NBPAR = 0
      NOMPAR(1) = ' '
      NOMPAR(2) = ' '
      VALPU(1) = ZERO
      VALPU(2) = ZERO

C --- RECUPERATION DE LA TEMPERATURE :
C     1- SI LA TEMPERATURE EST CONNUE AUX NOEUDS :
C        -----------------------------------------
      CALL TECACH ('ONN','PTEMPER',8,ITAB,IRET)
      ITEMP = ITAB(1)
      IF (ITEMP.GT.0) THEN
        TEMPNO = .TRUE.
        NBPAR = 1
        NOMPAR(1) = 'TEMP'
C       -- CALCUL DES TEMPERATURES INF, SUP ET MOY
C          (MOYENNE DES NNO NOEUDS) ET DES COEF. DES POLY. DE DEGRE 2 :
C          ------------------------------------------------------------
        TINF = ZERO
        TMOY = ZERO
        TSUP = ZERO
        DO 10 , INO = 1,NNOEL
          CALL DXTPIF(ZR(ITEMP+3*(INO-1)),ZL(ITAB(8)+3*(INO-1)))
          TMOY = TMOY + ZR(ITEMP-1+3*(INO-1)+1)/DBLE(NNOEL)
          TINF = TINF + ZR(ITEMP-1+3*(INO-1)+2)/DBLE(NNOEL)
          TSUP = TSUP + ZR(ITEMP-1+3*(INO-1)+3)/DBLE(NNOEL)
 10     CONTINUE
        C1 = TMOY
        C2 = (TSUP-TINF) / H
        C3 = DEUX*(TINF+TSUP-DEUX*TMOY) / (H*H)

C     2- SI LA TEMPERATURE EST UNE FONCTION DE 'INST' ET 'EPAIS'
C        -------------------------------------------------------
      ELSE
        CALL TECACH('ONN','PTEMPEF',1,ITEMP,IRET)
        IF (ITEMP.GT.0) THEN
          TEMPNO = .FALSE.
          NBPAR = 2
          NOMPAR(1) = 'INST'
          NOMPAR(2) = 'EPAIS'
        ELSE
          CALL UTMESS('F','DXSIEF','TEMPERATURE NON TROUVEE.')
        END IF
      END IF

C     -- PARTITION DU DEPLACEMENT EN MEMBRANE/FLEXION :
C     -------------------------------------------------
      DO 20, INO = 1,NNOEL
        DEPM(1+2*(INO-1)) =  DEPL(1+6*(INO-1))
        DEPM(2+2*(INO-1)) =  DEPL(2+6*(INO-1))
        DEPF(1+3*(INO-1)) =  DEPL(1+2+6*(INO-1))
        DEPF(2+3*(INO-1)) =  DEPL(3+2+6*(INO-1))
        DEPF(3+3*(INO-1)) = -DEPL(2+2+6*(INO-1))
 20   CONTINUE

      IF ( GRILLE ) THEN
        NPGH = 1
      ELSE
        NPGH = 3
      END IF
C
      CALL JEVECH('PNBSP_I','L',JNBSPI)
      NBCON = 6
      NBCOU = ZI(JNBSPI-1+1)
      IF (NBCOU.LE.0) CALL UTMESS('F','DXSIEF',
     +                            'NOMBRE DE COUCHES NEGATIF OU NUL')
C
      HIC = H/NBCOU
      IF (GRILLE) THEN
        ZMIN = -H/DEUX + HIC/DEUX + DISTN
      ELSE
        ZMIN = -H/DEUX + DISTN
      END IF

C --- BOUCLE SUR LES POINTS DE GAUSS DE LA SURFACE:
C     ---------------------------------------------
      DO 100, IPG = 1 , NPG

        IF ( DKQ ) THEN
          CALL JQUAD4( IPG, XYZL, ZR(LZR) )
          CALL DXQBM ( IPG, ZR(LZR), BM )
          CALL DKQBF ( IPG, ZR(LZR), BF )
        ELSE
          CALL DXTBM ( ZR(LZR), BM )
          CALL DKTBF ( IPG, ZR(LZR), BF )
        ENDIF
C
        CALL PMRVEC ( 'ZERO', 3, 2*NNOEL, BM, DEPM, EPSM )
        CALL PMRVEC ( 'ZERO', 3, 3*NNOEL, BF, DEPF, KHI )

C ----- CALCUL DE L'ECOULEMENT PLASTIQUE SUR CHAQUE COUCHE
C       PAR INTEGRATION EN TROIS POINTS:
C       --------------------------------
        DO 110, ICOU = 1 , NBCOU

          DO 120, IGAUH = 1 , NPGH

            ICPG = NBCON*NPGH*NBCOU*(IPG-1) + NBCON*NPGH*(ICOU-1) +
     +                                        NBCON*(IGAUH-1)

C       -- COTE DES POINTS D'INTEGRATION
C       --------------------------------
            IF (IGAUH.EQ.1) THEN
              ZIC = ZMIN + (ICOU-1)*HIC
            ELSE IF (IGAUH.EQ.2) THEN
              ZIC = ZMIN + HIC/DEUX + (ICOU-1)*HIC
            ELSE
              ZIC = ZMIN + HIC + (ICOU-1)*HIC
            END IF

C         -- CALCUL DE LA TEMPERATURE SUR LA COUCHE :
C         -------------------------------------------
            IF ( TEMPNO ) THEN
              TEMPG = C3*ZIC*ZIC + C2*ZIC + C1
              VALPU(1) = TEMPG
            ELSE
              VALPU(1) = INST
              VALPU(2) = ZIC
              CALL FOINTE('FM',ZK8(ITEMP),NBPAR,NOMPAR,VALPU,TEMPG,IRET)
            END IF

            CALL RCVALA(MATER,' ', 'ELAS', NBPAR,NOMPAR,VALPU,
     +                                   1,'ALPHA',ALPHA,CODRET, '  ' )
            IF ( CODRET.NE.'OK' ) ALPHA = ZERO

C         -- CALCUL DE EPS2D
C         ------------------
            EPS2D(1) = EPSM(1) + ZIC*KHI(1)
            EPS2D(2) = EPSM(2) + ZIC*KHI(2)
            EPS2D(3) = ZERO
            EPS2D(4) = EPSM(3) + ZIC*KHI(3)
            EPS2D(5) = ZERO
            EPS2D(6) = ZERO
 
C
C         -- INTERPOLATION DE ALPHA EN FONCTION DE LA TEMPERATURE
C         ----------------------------------------------------
            EPSTH(1) = ALPHA*(TEMPG-TREF)
            EPSTH(2) = ALPHA*(TEMPG-TREF)
            EPSTH(3) = ZERO
            EPSTH(4) = ZERO         
            IF (GRILLE) THEN
C
C              DEFORMATIONS  REPERE LOCAL
C
               EPSG(1)=EPS2D(1)-EPSTH(1)
               EPSG(2)=EPS2D(2)-EPSTH(2)
               EPSG(3)=EPS2D(4)
               CALL INSDRF(EPSG,PHI,EPSL)
               CALL RCVALA(MATER,' ','ELAS',1,'TEMP',TEMPG,
     &                     1,'E',E,CODRET,'FM')
               SIGL(1)=E*EPSL(1)
               SIGL(2)=E*EPSL(2)
               SIGL(3)=0.D0
C
C
               SIGL(2) = 0.D0
               CALL R8INIR(4,0.D0,SIGP,1)
               CALL INSCRG ( SIGL , PHI , SIGP)
               SIGMA(ICPG+1)=SIGP(1)
               SIGMA(ICPG+2)=SIGP(2)
               SIGMA(ICPG+3)=0.D0
               SIGMA(ICPG+4)=SIGP(3)
            ELSE

C              -- CALCUL DE LA MATRICE DE HOOKE
C              --------------------------------
               CALL DMATCP ( MATER, TEMPG, HYDR, SECH, INST, REPERE, D )

C              -- CALCUL DE LA CONTRAINTE AU POINT D'INTEGRATION COURANT
C              ---------------------------------------------------------
               DO 130 I = 1, 4 
                  SIG = ZERO
                  DO 132 J = 1, 4
                     SIG = SIG + (EPS2D(J)-EPSTH(J))*D(I,J)
 132              CONTINUE
                  SIGMA(ICPG+I) = SIG
 130           CONTINUE
            ENDIF
 120      CONTINUE
 110    CONTINUE

 100  CONTINUE

      END
