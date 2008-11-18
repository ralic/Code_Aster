      SUBROUTINE RCEVFA ( NOMMAT, PARA, SM, CNOC, CSNO, CSNE, CSPO,
     +                    CSPE, CFAO, CFAE )
      IMPLICIT     NONE
      REAL*8       PARA(3), SM
      CHARACTER*8  NOMMAT
      CHARACTER*24 CNOC, CSNO, CSNE, CSPO, CSPE, CFAO, CFAE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 17/11/2008   AUTEUR PROIX J-M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
      CHARACTER*32     JEXNOM, JEXNUM
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER      NBORDR, JSNO, JSNE, JSPO, JSPE, JFAO, JFAE, I, IND,
     +             NBOCC1, NBOCC2, NBCYCL, JNOC, NBINST, I1, I2
      REAL*8       SNO, SNE, SPO, SPE, KEO, KEE, SALTO, SALTE,
     +             NADMO, NADME
      CHARACTER*8  K8B
C DEB ------------------------------------------------------------------
      CALL JEMARQ()
C
      CALL JELIRA ( CSNO, 'LONMAX', NBORDR, K8B )
      CALL JEVEUO ( CSNO, 'L', JSNO )
      CALL JEVEUO ( CSNE, 'L', JSNE )
      CALL JEVEUO ( CSPO, 'L', JSPO )
      CALL JEVEUO ( CSPE, 'L', JSPE )
      CALL JELIRA ( CNOC, 'LONMAX', NBINST, K8B )
      CALL JEVEUO ( CNOC, 'L', JNOC )
C
      CALL WKVECT ( CFAO, 'V V R', 4*NBORDR, JFAO )
      CALL WKVECT ( CFAE, 'V V R', 4*NBORDR, JFAE )
C
      IND = 0
      DO 10 I1 = 1 , NBINST
C
        IND = IND + 1
        NBOCC1 = ZI(JNOC-1+I1)
        NBOCC2 = ZI(JNOC-1+I1)
        SNO = ZR(JSNO+IND-1)
        SNE = ZR(JSNE+IND-1)
        SPO = ZR(JSPO+IND-1)
        SPE = ZR(JSPE+IND-1)
C
C ---   CALCUL DU COEFFICIENT DE CONCENTRATION ELASTO-PLASTIQUE KE
C ---   AUX ORIGINES ET EXTREMITES DU CHEMIN
C ---   CALCUL DE LA CONTRAINTE EQUIVALENTE ALTERNEE SALT
C ---   AUX ORIGINES ET EXTREMITES DU CHEMIN
C ---   CALCUL DU NOMBRE DE CYCLES ADMISSIBLE NADM EN UTILISANT
C ---   LA COURBE DE WOHLER AUX ORIGINES ET EXTREMITES DU CHEMIN
C       --------------------------------------------------------
        CALL PRCCM3(NOMMAT, PARA, SM, SNO, SPO, KEO, SALTO, NADMO)
        CALL PRCCM3(NOMMAT, PARA, SM, SNE, SPE, KEE, SALTE, NADME)
C
        ZR(JFAO-1+4*(IND-1)+1) = KEO
        ZR(JFAO-1+4*(IND-1)+2) = SALTO
        ZR(JFAO-1+4*(IND-1)+3) = NADMO
C
        ZR(JFAE-1+4*(IND-1)+1) = KEE
        ZR(JFAE-1+4*(IND-1)+2) = SALTE
        ZR(JFAE-1+4*(IND-1)+3) = NADME
C
C ---   CALCUL DES FACTEURS D'USAGE AUX EXTREMITES DU CHEMIN
C ---   CORRESPONDANT A LA COMBINAISON DES 2 INSTANTS CONSIDERES
C ---   DU TRANSITOIRE :
C        --------------
        NBCYCL = MIN( NBOCC1 , NBOCC2 )
        ZR(JFAO-1+4*(IND-1)+4) = DBLE(NBCYCL) / NADMO
        ZR(JFAE-1+4*(IND-1)+4) = DBLE(NBCYCL) / NADME
C
        DO 12 I2 = I1+1 , NBINST
C
          IND = IND + 1
          NBOCC2 = ZI(JNOC-1+I2)
          SNO = ZR(JSNO+IND-1)
          SNE = ZR(JSNE+IND-1)
          SPO = ZR(JSPO+IND-1)
          SPE = ZR(JSPE+IND-1)
C
C ---     CALCUL DU COEFFICIENT DE CONCENTRATION ELASTO-PLASTIQUE KE
C ---     AUX ORIGINES ET EXTREMITES DU CHEMIN
C ---     CALCUL DE LA CONTRAINTE EQUIVALENTE ALTERNEE SALT
C ---     AUX ORIGINES ET EXTREMITES DU CHEMIN
C ---     CALCUL DU NOMBRE DE CYCLES ADMISSIBLE NADM EN UTILISANT
C ---     LA COURBE DE WOHLER AUX ORIGINES ET EXTREMITES DU CHEMIN
C         --------------------------------------------------------
          CALL PRCCM3(NOMMAT, PARA, SM, SNO, SPO, KEO, SALTO, NADMO)
          CALL PRCCM3(NOMMAT, PARA, SM, SNE, SPE, KEE, SALTE, NADME)
C
          ZR(JFAO-1+4*(IND-1)+1) = KEO
          ZR(JFAO-1+4*(IND-1)+2) = SALTO
          ZR(JFAO-1+4*(IND-1)+3) = NADMO
C
          ZR(JFAE-1+4*(IND-1)+1) = KEE
          ZR(JFAE-1+4*(IND-1)+2) = SALTE
          ZR(JFAE-1+4*(IND-1)+3) = NADME
C
C ---     CALCUL DES FACTEURS D'USAGE AUX EXTREMITES DU CHEMIN
C ---     CORRESPONDANT A LA COMBINAISON DES 2 INSTANTS CONSIDERES
C ---     DU TRANSITOIRE :
C         --------------
          NBCYCL = MIN( NBOCC1 , NBOCC2 )
          ZR(JFAO-1+4*(IND-1)+4) = DBLE(NBCYCL) / NADMO
          ZR(JFAE-1+4*(IND-1)+4) = DBLE(NBCYCL) / NADME
C
 12     CONTINUE
C
 10   CONTINUE
C
      CALL JEDEMA()
      END
