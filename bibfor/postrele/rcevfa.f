      SUBROUTINE RCEVFA ( NOMMAT, PARA, SM, CNOC, CSNO, CSNE, CSPO,
     &                    CSPE, KEMIXT, CSPTO, CSPTE, CSPMO, CSPME,
     &                    CFAO, CFAE )
      IMPLICIT     NONE
      REAL*8       PARA(3), SM
      CHARACTER*8  NOMMAT
      CHARACTER*24 CNOC, CSNO, CSNE, CSPO, CSPE, CFAO, CFAE,
     &              CSPTO, CSPTE, CSPMO, CSPME
      LOGICAL       KEMIXT
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 20/04/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C     ------------------------------------------------------------------
C     OPERATEUR POST_RCCM, TYPE_RESU_MECA='EVOLUTION'
C     CALCUL DU KE, SALT, NADM ET DOMMAGE
C
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER      NBORDR, JSNO, JSNE, JSPO, JSPE, JFAO, JFAE, IND,
     &             JNOC, NBINST, I1, I2, JSPTO, JSPTE, JSPMO, JSPME
      REAL*8       SNO, SNE, SPO, SPE, KEO, KEE, SALTO, SALTE,
     &             NADMO, NADME, KTH, KETHEO, KETHEE,
     &             SPTO, SPTE, SPMO, SPME, KEMECO, KEMECE, NBID,
     &             SALTMO, SALTME, SALTHO, SALTHE, VALR(2),R8MAEM
      CHARACTER*8  K8B
      INTEGER ICODRE
      LOGICAL       ENDUR
C DEB ------------------------------------------------------------------
      CALL JEMARQ()
C
      CALL JELIRA ( CSNO, 'LONMAX', NBORDR, K8B )
      CALL JEVEUO ( CSNO, 'L', JSNO )
      CALL JEVEUO ( CSNE, 'L', JSNE )
      CALL JEVEUO ( CSPO, 'L', JSPO )
      CALL JEVEUO ( CSPE, 'L', JSPE )
      IF (KEMIXT) THEN
        CALL JEVEUO ( CSPTO, 'L', JSPTO )
        CALL JEVEUO ( CSPTE, 'L', JSPTE )
        CALL JEVEUO ( CSPMO, 'L', JSPMO )
        CALL JEVEUO ( CSPME, 'L', JSPME )
      ENDIF
      CALL JELIRA ( CNOC, 'LONMAX', NBINST, K8B )
      CALL JEVEUO ( CNOC, 'L', JNOC )
C
      CALL WKVECT ( CFAO, 'V V R', 5*NBORDR, JFAO )
      CALL WKVECT ( CFAE, 'V V R', 5*NBORDR, JFAE )
C
      IND = 0
C
C ---     POUR TOUTES COMBINAISONS D INSTANTS : CALCUL AUX DEUX
C ---     EXTREMITES :
C ---     - DU COEFFICIENT DE CONCENTRATION ELASTO-PLASTIQUE KE
C ---     - DE LA CONTRAINTE EQUIVALENTE ALTERNEE SALT
C ---     - DU NBRE DE CYCLES ADMISSIBLE NADM (AVEC LA COURBE DE WOHLER)
C ---     - DU FACTEUR D USAGE
C         --------------------------------------------------------
      DO 10 I1 = 1 , NBINST
C
        IND = IND + 1
        ZR(JFAO-1+5*(IND-1)+4) = 0.D0
        ZR(JFAE-1+5*(IND-1)+4) = 0.D0
C
        DO 12 I2 = I1+1 , NBINST
C
          IND = IND + 1
          SNO = ZR(JSNO+IND-1)
          SNE = ZR(JSNE+IND-1)
C
          IF (.NOT. KEMIXT) THEN
C
C --- 1ER CAS : KE_MECA
C
            SPO = ZR(JSPO+IND-1)
            SPE = ZR(JSPE+IND-1)
C
            CALL PRCCM3(NOMMAT, PARA, SM, SNO, SPO, KEO, SALTO, NADMO)
            CALL PRCCM3(NOMMAT, PARA, SM, SNE, SPE, KEE, SALTE, NADME)
C
            ZR(JFAO-1+5*(IND-1)+1) = KEO
            ZR(JFAO-1+5*(IND-1)+2) = SALTO
            ZR(JFAO-1+5*(IND-1)+3) = NADMO
C
            ZR(JFAE-1+5*(IND-1)+1) = KEE
            ZR(JFAE-1+5*(IND-1)+2) = SALTE
            ZR(JFAE-1+5*(IND-1)+3) = NADME
C
            ZR(JFAO-1+5*(IND-1)+4) = 1.D0 / NADMO
            ZR(JFAE-1+5*(IND-1)+4) = 1.D0 / NADME
          ELSE
C
C --- 2EME CAS : KE_MIXTE
C
            SPMO = ZR(JSPMO+IND-1)
            SPME = ZR(JSPME+IND-1)
            SPTO = ZR(JSPTO+IND-1)
            SPTE = ZR(JSPTE+IND-1)
C
            KTH = 1.86D0*(1.D0-(1.D0/(1.66D0+SNO/SM)))
            KETHEO = MAX(1.D0,KTH)
            SALTHO =  0.5D0 * PARA(3) * KETHEO * SPTO
C
            KTH = 1.86D0*(1.D0-(1.D0/(1.66D0+SNE/SM)))
            KETHEE = MAX(1.D0,KTH)
            SALTHE =  0.5D0 * PARA(3) * KETHEE * SPTE
C
            CALL PRCCM3 ( NOMMAT,PARA,SM,SNO,SPMO,KEMECO,SALTMO,NBID )
            CALL PRCCM3 ( NOMMAT,PARA,SM,SNE,SPME,KEMECE,SALTME,NBID )
            SALTO = SALTMO + SALTHO
            SALTE = SALTME + SALTHE
C
C --- CALCUL DU NOMBRE DE CYCLES ADMISSIBLE NADM
C
            CALL LIMEND( NOMMAT,SALTO,'WOHLER',ENDUR)
            IF (ENDUR) THEN
               NADMO=R8MAEM()
            ELSE
              CALL RCVALE (NOMMAT,'FATIGUE', 1, 'SIGM    ',SALTO, 1,
     &                      'WOHLER  ', NADMO, ICODRE, 2)
              IF ( NADMO .LT. 0 ) THEN
                VALR (1) = SALTO
                VALR (2) = NADMO
                CALL U2MESG('A','POSTRELE_61',0,' ',0,0,2,VALR)
              ENDIF
            ENDIF
C
            CALL LIMEND( NOMMAT,SALTE,'WOHLER',ENDUR)
            IF (ENDUR) THEN
               NADME=R8MAEM()
            ELSE
              CALL RCVALE (NOMMAT,'FATIGUE', 1, 'SIGM    ',SALTE, 1,
     &                      'WOHLER  ', NADME, ICODRE, 2)
              IF ( NADMO .LT. 0 ) THEN
                VALR (1) = SALTE
                VALR (2) = NADME
                CALL U2MESG('A','POSTRELE_61',0,' ',0,0,2,VALR)
              ENDIF
            ENDIF
C
            ZR(JFAO-1+5*(IND-1)+1) = KEMECO
            ZR(JFAO-1+5*(IND-1)+5) = KETHEO
            ZR(JFAO-1+5*(IND-1)+2) = SALTO
            ZR(JFAO-1+5*(IND-1)+3) = NADMO
            ZR(JFAO-1+5*(IND-1)+4) = 1.D0 / NADMO
C
            ZR(JFAE-1+5*(IND-1)+1) = KEMECE
            ZR(JFAE-1+5*(IND-1)+5) = KETHEE
            ZR(JFAE-1+5*(IND-1)+2) = SALTE
            ZR(JFAE-1+5*(IND-1)+3) = NADME
            ZR(JFAE-1+5*(IND-1)+4) = 1.D0 / NADME
C
          ENDIF
C
 12     CONTINUE
C
 10   CONTINUE
C
      CALL JEDEMA()
      END
