      SUBROUTINE TE0033 ( OPTION, NOMTE )
      IMPLICIT  NONE
      CHARACTER*16        OPTION, NOMTE
C     ------------------------------------------------------------------
C MODIF ELEMENTS  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
C     CALCUL DE CONTRAINTES, DEFORMATIONS, EFFORTS ET DEFORMATIONS
C     GENERALISES POUR LES ELEMENTS DKT, DST, DKQ, DSQ ET Q4G
C     POUR UN MATERIAU ISOTROPE OU MULTICOUCHE
C         OPTIONS TRAITEES  ==>  SIEF_ELGA_DEPL
C                                EFGE_ELNO_DEPL
C                                SIGM_ELNO_DEPL
C                                EPSI_ELNO_DEPL
C                                DEGE_ELNO_DEPL
C     IN   K16   OPTION : NOM DE L'OPTION A CALCULER
C     IN   K16   NOMTE  : NOM DU TYPE_ELEMENT
C     ------------------------------------------------------------------
C     ----- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
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
      INTEGER       NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDX,JGANO
      INTEGER       I, IBID, IC, IER, INIV, IRET, ITEMP, JCARA,
     &              JDEPG, JEFFG, JGEOM, JMATE, JNUMCO, JSIGM,
     &              NP, MULTIC, JTAB(8), IPG,ICONTP,
     &              JNBSPI, NBCOU, NBSP,ISP, IEFF, ICOU
      REAL*8        TINF1, TMOY1, TSUP1, ZERO, EPAIS,X3I,EPI,EPTOT
      REAL*8        PGL(3,3), XYZL(3,4), VALPU(2),R8BID
      REAL*8        DEPL(24), DEPLR(24), DEPLI(24), SIGMR(32),SIGMI(32)
      REAL*8        DEPGR(24), DEPGI(24), SIGMRL(32), SIGMIL(32)
      REAL*8        TMOY(4), TSUP(4), TINF(4), EFFGT(32), SIGTOT(24)
      REAL*8        T2EV(4), T2VE(4), T1VE(9)
      LOGICAL       GRILLE
      CHARACTER*2   CODRET,VAL
      CHARACTER*8   NOMPU(2),NOMRES
      CHARACTER*16  PHENOM
      CHARACTER*3 NUM
C     ------------------------------------------------------------------
C
      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDX,JGANO)
C
      IF (OPTION.NE.'SIEF_ELGA_DEPL  ' .AND.
     &    OPTION.NE.'EFGE_ELNO_DEPL  ' .AND.
     &    OPTION.NE.'SIGM_ELNO_DEPL  ' .AND.
     &    OPTION.NE.'EPSI_ELNO_DEPL  ' .AND.
     &    OPTION.NE.'DEGE_ELNO_DEPL  ') THEN
        CALL U2MESS('F','ELEMENTS2_67')
      END IF
      ZERO   = 0.0D0

      IF (NOMTE(1:8).EQ.'MEGRDKT ') THEN
        GRILLE = .TRUE.
      ELSE
        GRILLE = .FALSE.
      END IF

      DO 10 I = 1,32
        EFFGT(I) = ZERO
   10 CONTINUE

      DO 20 I = 1,24
        SIGTOT(I) = ZERO
   20 CONTINUE

      DO 30 I = 1,4
        TINF(I) = ZERO
        TMOY(I) = ZERO
        TSUP(I) = ZERO
   30 CONTINUE

      CALL JEVECH('PGEOMER','L',JGEOM)
      CALL JEVECH('PMATERC','L',JMATE)
      CALL JEVECH('PCACOQU','L',JCARA)
      EPAIS  = ZR(JCARA)

      IF (OPTION(8:9).EQ.'GA') THEN
        NP = NPG
      ELSE IF (OPTION(8:9).EQ.'NO') THEN
        NP = NNO
      END IF

      IF (NNO.EQ.3) THEN
        CALL DXTPGL(ZR(JGEOM),PGL)
      ELSE IF (NNO.EQ.4) THEN
        CALL DXQPGL(ZR(JGEOM),PGL)
      END IF

      CALL UTPVGL(NNO,3,PGL,ZR(JGEOM),XYZL)
      CALL DXREPE( PGL,T2EV, T2VE, T1VE )

      CALL JEVECH('PDEPLAR','L',JDEPG)
      CALL UTPVGL(NNO,6,PGL,ZR(JDEPG),DEPL)

      IF (OPTION(1:9).EQ.'EFGE_ELNO' .OR.
     &    OPTION(1:9).EQ.'SIGM_ELNO') THEN
C===============================================================
C          -- RECUPERATION DE LA TEMPERATURE :
C          -- SI LA TEMPERATURE EST CONNUE AUX NOEUDS :
      CALL TECACH ('NNN','PTEMPER',8,JTAB,IRET)
      ITEMP=JTAB(1)
        IF (IRET.EQ.0 .OR. IRET.EQ.3) THEN
          DO 50 I = 1,NNO
            CALL DXTPIF(ZR(ITEMP+3*(I-1)),ZL(JTAB(8)+3*(I-1)))
            TMOY(I) = ZR(ITEMP+3* (I-1))
            TINF(I) = ZR(ITEMP+3* (I-1)+1)
            TSUP(I) = ZR(ITEMP+3* (I-1)+2)
   50     CONTINUE
        END IF
C          -- SI LA TEMPERATURE EST UNE FONCTION DE 'INST' ET 'EPAIS'
        CALL TECACH('NNN','PTEMPEF',1,ITEMP,IRET)
        IF (IRET.EQ.0) THEN
          NOMPU(1) = 'INST'
          NOMPU(2) = 'EPAIS'
          CALL JEVECH('PTEMPSR','L',IBID)
          VALPU(1) = ZR(IBID)
          VALPU(2) = 0.D0
          CALL FOINTE('FM',ZK8(ITEMP),2,NOMPU,VALPU,TMOY1,IER)
          VALPU(2) = -EPAIS/2.D0
          CALL FOINTE('FM',ZK8(ITEMP),2,NOMPU,VALPU,TINF1,IER)
          VALPU(2) = +EPAIS/2.D0
          CALL FOINTE('FM',ZK8(ITEMP),2,NOMPU,VALPU,TSUP1,IER)
          DO 60,I = 1,NNO
            TMOY(I) = TMOY1
            TINF(I) = TINF1
            TSUP(I) = TSUP1
   60     CONTINUE
        END IF
C===============================================================
      END IF

C     ---------- CONTRAINTES ET DEFORMATIONS --------------------------

C          ----------------------------
      IF ( OPTION(1:9) .EQ. 'SIGM_ELNO' ) THEN
C          ----------------------------
        CALL JEVECH('PNUMCOR','L',JNUMCO)
        IC = ZI(JNUMCO)
        INIV = ZI(JNUMCO+1)
        CALL JEVECH('PCONTRR','E',JSIGM)

C ---   CALCUL DES CONTRAINTES VRAIES (I.E. SIG_MECA - SIG_THERM)
C ---   AUX POINTS DE CALCUL
C       --------------------
        CALL DXSIGV(NOMTE,OPTION,XYZL,PGL,IC,INIV,DEPL,TSUP,TINF,TMOY,
     &              SIGTOT)

C ---   PASSAGE DES CONTRAINTES DU REPERE INTRINSEQUE
C ---   A L'ELEMENT AU REPERE LOCAL DE LA COQUE
C       ---------------------------------------
        CALL DXSIRO(NP,T2EV,SIGTOT,ZR(JSIGM))

C               ----------------------------
      ELSE IF ( OPTION(1:9) .EQ. 'EPSI_ELNO' ) THEN
C               ----------------------------
        CALL JEVECH('PDEFORR','E',JSIGM)
        CALL JEVECH('PNUMCOR','L',JNUMCO)
        IC = ZI(JNUMCO)
        INIV = ZI(JNUMCO+1)

        IF (NOMTE(1:8).EQ.'MEDKTR3 ' .OR.
     &      NOMTE(1:8).EQ.'MEDKTG3 ' .OR.
     &      NOMTE(1:8).EQ.'MEGRDKT ') THEN
          CALL DKTCOD(XYZL,OPTION,PGL,IC,INIV,DEPL,SIGTOT,
     &                MULTIC,GRILLE)
        ELSE IF (NOMTE(1:8).EQ.'MEDSTR3 ') THEN
          CALL DSTCOD(XYZL,OPTION,PGL,IC,INIV,DEPL,SIGTOT)
        ELSE IF (NOMTE(1:8).EQ.'MEDKQU4 '.OR.
     &           NOMTE(1:8).EQ.'MEDKQG4 ') THEN
          CALL DKQCOD(XYZL,OPTION,PGL,IC,INIV,DEPL,SIGTOT)
        ELSE IF (NOMTE(1:8).EQ.'MEDSQU4 ') THEN
          CALL DSQCOD(XYZL,OPTION,PGL,IC,INIV,DEPL,SIGTOT)
        ELSE IF (NOMTE(1:8).EQ.'MEQ4QU4 ') THEN
          CALL Q4GCOD(XYZL,OPTION,PGL,IC,INIV,DEPL,SIGTOT)
        END IF

C ---     PASSAGE DES DEFORMATIONS  DU REPERE INTRINSEQUE
C ---     A L'ELEMENT AU REPERE LOCAL DE LA COQUE
C         ---------------------------------------
        CALL DXSIRO(NP,T2EV,SIGTOT,ZR(JSIGM))
C
C               ----------------------------
      ELSE IF ( OPTION(1:9) .EQ. 'SIEF_ELGA' ) THEN
C               ----------------------------

        CALL JEVECH('PCONTRR','E',JSIGM)
C
        CALL RCCOMA ( ZI(JMATE), 'ELAS', PHENOM, CODRET )
        IF ( PHENOM.EQ.'ELAS'      .OR.
     &       PHENOM.EQ.'ELAS_ORTH' .OR.
     &       PHENOM.EQ.'ELAS_ISTR' ) THEN
           CALL DXSIEF ( NOMTE, XYZL, DEPL, ZI(JMATE), PGL, ZR(JSIGM) )

        ELSEIF ( PHENOM.EQ.'ELAS_COQUE' ) THEN

           CALL JEVECH('PNBSP_I','L',JNBSPI)
           NBCOU = ZI(JNBSPI)
           IF (NBCOU.LE.0) CALL U2MESS('F','ELEMENTS_46')

           IF ( NOMTE(1:8).EQ.'MEDKTR3 ' .OR.
     &          NOMTE(1:8).EQ.'MEDKTG3 ' ) THEN
              CALL DKTCOL(XYZL,OPTION,PGL,NBCOU,3,DEPL,ZR(JSIGM),
     &                    MULTIC,GRILLE)
           ELSE IF (NOMTE(1:8).EQ.'MEGRDKT ' ) THEN
              CALL DKTCOL(XYZL,OPTION,PGL,NBCOU,1,DEPL,ZR(JSIGM),
     &                    MULTIC,GRILLE)
           ELSE IF (NOMTE(1:8).EQ.'MEDSTR3 ' ) THEN
              CALL DSTCOL(XYZL,OPTION,PGL,NBCOU,3,DEPL,ZR(JSIGM))
           ELSE IF (NOMTE(1:8).EQ.'MEDKQU4 ' .OR.
     &              NOMTE(1:8).EQ.'MEDKQG4 ' ) THEN
              CALL DKQCOL(XYZL,OPTION,PGL,NBCOU,3,DEPL,ZR(JSIGM))
           ELSE IF (NOMTE(1:8).EQ.'MEDSQU4 ' ) THEN
              CALL DSQCOL(XYZL,OPTION,PGL,NBCOU,3,DEPL,ZR(JSIGM))
           ELSE IF (NOMTE(1:8).EQ.'MEQ4QU4 ' ) THEN
              CALL Q4GCOL(XYZL,OPTION,PGL,NBCOU,3,DEPL,ZR(JSIGM))
           END IF
           CALL DXSIR2 ( NP, NBCOU, 3, T2EV, ZR(JSIGM), ZR(JSIGM) )
        ELSEIF ( PHENOM.EQ.'ELAS_COQMU' ) THEN
C
C       EXCEPTION POUR LES MULTICOUCHES COMPÖSITES : ON STOCKE DANS UNE
C       SEULE COUCHE LES CONTRAINTES HOMOGENEISEES
C
            CALL JEVECH('PNBSP_I','L',JNBSPI)
            NBCOU = ZI(JNBSPI)
            IF (NBCOU.LE.0) CALL U2MESS('F','ELEMENTS_46')

            CALL DXEFGV(NOMTE,OPTION,XYZL,PGL,DEPL,TSUP,TINF,TMOY,EFFGT)
            CALL EXCENT(OPTION,NOMTE,NNO,EFFGT)
            CALL TECACH('OON','PCONTRR',7,JTAB,IRET)
            NPG=JTAB(3)
            NBSP=JTAB(7)
C RECALCUL DE L'EPAISSEUR
            ICOU=0
            EPTOT=0.D0
 215        CONTINUE
            ICOU=ICOU+1
            CALL CODENT(ICOU,'G',NUM)
            CALL CODENT(1,'G',VAL)
            NOMRES = 'C'//NUM//'_V'//VAL
            CALL RCVALA(ZI(JMATE),' ','ELAS_COQMU',0,' ',R8BID,
     &         1,NOMRES,EPI,CODRET,' ')
            IF (CODRET.EQ.'OK') THEN
                EPTOT=EPTOT+EPI
                GOTO 215
            ENDIF
C EN PRINCIPE, EPTOT=EPAIS LUE DANS CARA_ELEM. MAIS PAS TOUJOURS
C CAR DANS HPLA100G,H ET SSLS113A, ON DECLARE EPAIS=0
            DO 216 IPG=1,NPG
               IEFF=8*(IPG-1)
               DO 219 I = 1,6
                  SIGTOT(I) = 0.D0
  219          CONTINUE
               DO 217 ISP=1,NBSP
                  IF (ISP.EQ.1) THEN
                     X3I = - EPTOT/2.D0
                  ELSE IF (ISP.EQ.2) THEN
                     X3I = 0.D0
                  ELSE IF (ISP.EQ.3) THEN
                     X3I = EPTOT/2.D0
                  END IF
C                 SIGXX=NXX/E + Z*MXX*12:E**3
C                 SIGYY=NYY/E + Z*MYY*12:E**3
                  DO 218 I = 1,2
                     SIGTOT(I) = EFFGT(IEFF+I)/EPTOT +
     &                       X3I*EFFGT(IEFF+3+I)/EPTOT/EPTOT/EPTOT*12.D0
 218              CONTINUE
C                 SIGXY=NXY/E + Z*MXY*12:E**3
                  SIGTOT(4) = EFFGT(IEFF+3)/EPTOT +
     &                    X3I*EFFGT(IEFF+3+3)/EPTOT/EPTOT/EPTOT*12.D0
                  ICONTP=JTAB(1)+(6*NBSP)*(IPG-1)+6*(ISP-1)
                  CALL DCOPY(6,SIGTOT,1,ZR(ICONTP),1)
217            CONTINUE
216         CONTINUE

        ELSE
           CALL U2MESK('F','ELEMENTS2_76',1,PHENOM)
        END IF
C
C               ----------------------------
      ELSE IF ( OPTION(1:9) .EQ. 'EFGE_ELNO' ) THEN
C               ----------------------------
C ---     CALCUL DES EFFORTS GENERALISES VRAIS
C ---     AUX POINTS DE CALCUL
C         --------------------
        CALL DXEFGV(NOMTE,OPTION,XYZL,PGL,DEPL,TSUP,TINF,TMOY,EFFGT)
C
C ---    PRISE EN COMPTE DE L'EXCENTREMENT SI ON CALCULE LES
C ---    EFFORTS GENERALISES SUR UN FEUILLET DE REFERENCE DIFFERENT
C ---    DU FEUILLET DU MAILLAGE (I.E. EN PEAU SUP, INF OU MOY)
C        ------------------------------------------------------
C
        CALL EXCENT(OPTION,NOMTE,NNO,EFFGT)

C ---   PASSAGE DES EFFORTS GENERALISES DU REPERE INTRINSEQUE
C ---   A L'ELEMENT AU REPERE LOCAL DE LA COQUE
C       ---------------------------------------
        CALL JEVECH('PEFFORR','E',JEFFG)
        CALL DXEFRO(NP,T2EV,EFFGT,ZR(JEFFG))

C
C               ----------------------------
      ELSE IF ( OPTION(1:9) .EQ. 'DEGE_ELNO' ) THEN
C               ----------------------------

        CALL JEVECH('PDEFOGR','E',JEFFG)


        IF (NOMTE(1:8).EQ.'MEDKTR3 ' .OR.
     &      NOMTE(1:8).EQ.'MEGRDKT ' .OR.
     &      NOMTE(1:8).EQ.'MEDKTG3 ' ) THEN
          CALL DKTEDG(XYZL,OPTION,PGL,DEPL,EFFGT,MULTIC,GRILLE)

        ELSE IF (NOMTE(1:8).EQ.'MEDSTR3 ' ) THEN
          CALL DSTEDG(XYZL,OPTION,PGL,DEPL,EFFGT)
        ELSE IF (NOMTE(1:8).EQ.'MEDKQU4 ' .OR.
     &           NOMTE(1:8).EQ.'MEDKQG4 ' ) THEN
          CALL DKQEDG(XYZL,OPTION,PGL,DEPL,EFFGT)
        ELSE IF (NOMTE(1:8).EQ.'MEDSQU4 ' ) THEN
          CALL DSQEDG(XYZL,OPTION,PGL,DEPL,EFFGT)
        ELSE IF (NOMTE(1:8).EQ.'MEQ4QU4 ' ) THEN
          CALL Q4GEDG(XYZL,OPTION,PGL,DEPL,EFFGT)
        END IF
C
C ---    PRISE EN COMPTE DE L'EXCENTREMENT SI ON CALCULE LES
C ---    EFFORTS GENERALISES SUR UN FEUILLET DE REFERENCE DIFFERENT
C ---    DU FEUILLET DU MAILLAGE (I.E. EN PEAU SUP, INF OU MOY)
C        ------------------------------------------------------
          CALL EXCENT(OPTION,NOMTE,NNO,EFFGT)
C
C ---     PASSAGE DES DEFORMATIONS GENERALISEES DU REPERE INTRINSEQUE
C ---     A L'ELEMENT AU REPERE LOCAL DE LA COQUE
C         ---------------------------------------
        CALL DXEFRO(NP,T2EV,EFFGT,ZR(JEFFG))

      END IF
      END
