      SUBROUTINE DXEFIN ( NOMTE, DEPL, EFFGT)
      IMPLICIT  NONE
      REAL*8        DEPL(24), EFFGT(32)
      CHARACTER*16  NOMTE
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
C     ------------------------------------------------------------------
C MODIF ELEMENTS  DATE 08/11/2005   AUTEUR CIBHHLV L.VIVAN 
C
C     CALCUL DES EFFORTS GENERALISES POUR LES ELEMENTS DE PLAQUE
C     DKT, DST, DKQ, DSQ ET Q4G, DANS LE REPERE LOCAL A L'ELEMENT
C         OPTION TRAITEE  ==>  SIEF_ELGA_DEPL
C
C     IN   K16   NOMTE      : NOM DU TYPE_ELEMENT
C     IN   R8    DEPL(24)   : DEPLACEMENT AUX NOEUDS DE L'ELEMENT
C     OUT  R8    EFFGT(32)  : TABLEAU DES EFFORTS GENERALISES
C                             AUX POINTS D'INTEGRATION
C     ---> POUR DKT/DST EFFINT = 24
C     ---> POUR DKQ/DSQ EFFINT = 32
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
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
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER      I, JGEOM, JMATE, NNO
      REAL*8       PGL(3,3), SIGTH(32), TMOY(4), TSUP(4), TINF(4),
     +             XYZL(3,4), ZERO
      LOGICAL      INDITH
      CHARACTER*16 OPTION
C     ------------------------------------------------------------------
C
C --- INITIALISATIONS :
C     ===============
      ZERO   = 0.0D0
      OPTION = 'SIEF_ELGA_DEPL'
C
      DO 10 I = 1,32
        EFFGT(I) = ZERO
        SIGTH(I) = ZERO
   10 CONTINUE
C
      DO 20 I = 1,4
        TINF(I) = ZERO
        TMOY(I) = ZERO
        TSUP(I) = ZERO
   20 CONTINUE
C
      CALL JEVECH('PMATERC','L',JMATE)
      CALL JEVECH('PGEOMER','L',JGEOM)
C
      IF (NOMTE(1:8).EQ.'MEDKTR3 ' .OR. NOMTE(1:8).EQ.'MEDSTR3 ' .OR.
     +    NOMTE(1:8).EQ.'MEGRDKT'  .OR. NOMTE(1:8).EQ.'MEDKTG3 ') THEN
         NNO = 3
         CALL DXTPGL(ZR(JGEOM),PGL)

      ELSE IF (NOMTE(1:8).EQ.'MEDKQU4 ' .OR.
     +         NOMTE(1:8).EQ.'MEDKQG4 ' .OR.
     +         NOMTE(1:8).EQ.'MEDSQU4 ' .OR.
     +         NOMTE(1:8).EQ.'MEQ4QU4 ') THEN
        NNO = 4
        CALL DXQPGL(ZR(JGEOM),PGL)

      ELSE
          CALL UTMESS('F','DXEFIN','LE TYPE D''ELEMENT : '//NOMTE(1:8)//
     +                'N''EST PAS PREVU.')
      END IF
C
C --- CALCUL DES COORDONNEES LOCALES DES CONNECTIVITES :
C     ------------------------------------------------
      CALL UTPVGL(NNO,3,PGL,ZR(JGEOM),XYZL)
C
C --- CACUL DU DEPLACEMENT DANS LE REPERE DE L'ELEMENT :
C     ------------------------------------------------
C      CALL UTPVGL(NNO,6,PGL,DEPL,DEPLOC)
C
C --- CALCUL DES EFFORTS GENERALISES 'VRAIS' AUX POINTS D'INTEGRATION :
C     ===============================================================
C
C --- CALCUL DES EFFORTS GENERALISES D'ORIGINE MECANIQUE
C --- AUX POINTS D'INTEGRATION :
C     ------------------------
      CALL DXEFGM(NOMTE,OPTION,XYZL,PGL,DEPL,EFFGT)
C
C --- RECUPERATION DES TEMPERATURES AUX NOEUDS :
C     ----------------------------------------
      CALL DXTEMP(NOMTE,TSUP,TINF,TMOY,INDITH)
C
C --- CALCUL DES EFFORTS GENERALISES D'ORIGINE THERMIQUE
C --- AUX POINTS D'INTEGRATION :
C     ------------------------
      CALL DXEFGT(NOMTE,XYZL,PGL,TSUP,TINF,TMOY,SIGTH)
C
      DO 30 I = 1, 32
        EFFGT(I) = EFFGT(I) - SIGTH(I)
  30  CONTINUE
C
C --- PRISE EN COMPTE DE L'EXCENTREMENT SI ON CALCULE LES 
C --- EFFORTS GENERALISES SUR UN FEUILLET DE REFERENCE DIFFERENT
C --- DU FEUILLET DU MAILLAGE (I.E. EN PEAU SUP, INF OU MOY) :
C     ------------------------------------------------------
      CALL EXCENT(OPTION,NOMTE,NNO,EFFGT)
C
      END
