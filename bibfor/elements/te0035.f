      SUBROUTINE TE0035 ( OPTION , NOMTE )
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
C ......................................................................
C    - FONCTION REALISEE:  CALCUL DES VECTEURS ELEMENTAIRES
C                          POUR LES ELEMENTS DKT, DST, DKQ, DSQ ET Q4G
C                          OPTIONS : 'CHAR_MECA_TEMP_R'
C                                    'CHAR_MECA_EPSI_R'
C
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
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
      CHARACTER*8        ZK8,NOMPU(2)
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
      CHARACTER*24 DESI
      CHARACTER*8  ELREFE
      REAL*8       PGL(3,3) , XYZL(3,4),VALPU(2),EPAIS
C
      REAL*8       TMOY(4), TSUP(4), TINF(4), EPSINI(6)
      REAL*8       BSIGMA(24), SIGT(32)
C ----------------------------------------------------------------------
C
      CALL ELREF1(ELREFE)

      CALL JEVECH ( 'PGEOMER' , 'L' , JGEOM )
      CALL JEVECH ( 'PCACOQU' , 'L' , JCACO )
      EPAIS=ZR(JCACO-1+1)
      CALL JEVECH ( 'PVECTUR' , 'E' , JVECG )
C
      DESI = '&INEL.'//ELREFE//'.DESI'
      CALL JEVETE(DESI,'L',LZI)
      NNO = ZI(LZI)
C
C --- DETERMINATION DE LA MATRICE DE PASSAGE DU REPERE GLOBAL
C --- AU REPERE LOCAL A L'ELEMENT
C     ---------------------------
      IF (     NNO .EQ. 3) THEN
         CALL DXTPGL ( ZR(JGEOM) , PGL )
      ELSE IF( NNO .EQ. 4) THEN
         CALL DXQPGL ( ZR(JGEOM) , PGL )
      ENDIF
C
C --- DETERMINATION DES COORDONNEES DES CONNECTIVITES DE L'ELEMENT
C --- DANS SON REPERE LOCAL
C     ---------------------
      CALL UTPVGL ( NNO , 3 , PGL , ZR(JGEOM) , XYZL )
C
C --- CALCUL DES EFFORTS GENERALISES D'ORIGINE THERMIQUE
C --- AUX POINTS D'INTEGRATION
C     ------------------------
      IF (OPTION .EQ.'CHAR_MECA_TEMP_R') THEN
C===============================================================
C          -- RECUPERATION DE LA TEMPERATURE :
C          -- SI LA TEMPERATURE EST CONNUE AUX NOEUDS :
      CALL TECACH(.TRUE.,.FALSE.,'PTEMPER',1,ITEMP)
           IF (ITEMP.GT.0) THEN
             DO 10 I=1,NNO
                TMOY(I)=ZR(ITEMP+3*(I-1)  )
                TINF(I)=ZR(ITEMP+3*(I-1)+1)
                TSUP(I)=ZR(ITEMP+3*(I-1)+2)
 10           CONTINUE
           ENDIF
C          -- SI LA TEMPERATURE EST UNE FONCTION DE 'INST' ET 'EPAIS'
      CALL TECACH(.FALSE.,.FALSE.,'PTEMPEF',1,ITEMPF)
           IF (ITEMPF.GT.0) THEN
             NOMPU(1)='INST'
             NOMPU(2)='EPAIS'
             CALL JEVECH ( 'PTEMPSR' ,'L', IBID )
             VALPU(1)= ZR(IBID)

             VALPU(2)= 0.D0
             CALL  FOINTE ('FM',ZK8(ITEMPF), 2, NOMPU, VALPU, TMOY1,IER)
             VALPU(2)= -EPAIS/2.D0
             CALL  FOINTE ('FM',ZK8(ITEMPF), 2, NOMPU, VALPU, TINF1,IER)
             VALPU(2)= +EPAIS/2.D0
             CALL  FOINTE ('FM',ZK8(ITEMPF), 2, NOMPU, VALPU, TSUP1,IER)
             DO 11, I=1,NNO
                TMOY(I)=TMOY1
                TINF(I)=TINF1
                TSUP(I)=TSUP1
 11           CONTINUE
           ENDIF
C===============================================================
C
        CALL DXEFGT(NOMTE, XYZL, PGL, TSUP, TINF, TMOY, SIGT)
C
      ELSEIF (OPTION .EQ.'CHAR_MECA_EPSI_R') THEN
C
        CALL JEVECH ( 'PEPSINR' , 'L' , IDEFI )
C
        EPSINI(1) = ZR(IDEFI+1-1)
        EPSINI(2) = ZR(IDEFI+2-1)
        EPSINI(3) = ZR(IDEFI+3-1)
        EPSINI(4) = ZR(IDEFI+4-1)
        EPSINI(5) = ZR(IDEFI+5-1)
        EPSINI(6) = ZR(IDEFI+6-1)
C
        CALL DXEFGI(NOMTE, XYZL, PGL, EPSINI, SIGT)
C
      ENDIF
C
C --- CALCUL DES EFFORTS INTERNES D'ORIGINE THERMIQUE
C --- (I.E. SOMME_VOL(BT_SIG))
C     ------------------------
      CALL DXBSIG(NOMTE, XYZL, PGL, SIGT, BSIGMA)
C
C --- AFFECTATION DU VECTEUR DES FORCES ELEMENTAIRES EN SORTIE DU TE
C     --------------------------------------------------------------
      DO 20 I = 1, NNO*6
         ZR(JVECG+I-1) = BSIGMA(I)
  20  CONTINUE
C
      END
