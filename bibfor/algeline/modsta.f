      SUBROUTINE MODSTA(MOTCLE,LMATF,LMATM,NUME,IDDL,COEF,NEQ,
     +                                                    NBMODE,ZRMOD)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER                  LMATF,LMATM,    IDDL(*),   NEQ,NBMODE
      REAL*8                                       COEF(*),ZRMOD(NEQ,*)
      CHARACTER*(*)     MOTCLE,            NUME
      COMPLEX*16        CBID
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 28/03/2001   AUTEUR CIBHHLV L.VIVAN 
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
C-----------------------------------------------------------------------
C
C     CALCUL DE MODES STATIQUES
C
C     SI MOTCLE = 'DEPL' : CALCUL DE MODES CONTRAINTS
C                                    ( DEPLACEMENT UNITAIRE )
C                          LE TABLEAU IDDL EST CELUI DES NOEUDS BLOQUES
C                          ON APPLIQUE UNE FORCE UNITAIRE SUR LES LAGR
C     SI MOTCLE = 'FORC' : CALCUL DE MODES D'ATTACHE
C                                    ( FORCE UNITAIRE )
C                          LE TABLEAU IDDL EST CELUI DES NOEUDS ACTIFS
C     SI MOTCLE = 'ACCE' : CALCUL DE DEFORMEES STATIQUES
C                                    ( ACCELERATION VECTEUR UNITAIRE )
C     SI MOTCLE = 'ACCD' : CALCUL DE DEFORMEES STATIQUES
C                                    ( ACCELERATION DDL UNITAIRE )
C-----------------------------------------------------------------------
C  IN  : MOTCLE : CALCUL DE MODES CONTRAINTS OU D'ATTACHE
C  IN  : LMATF  : POINTEUR SUR LA MATRICE DE RAIDEUR FACTORISEE
C  IN  : LMATM  : POINTEUR SUR LE DESCRIPTEUR DE LA MATRICE DE MASSE
C  IN  : NUME   : NOM DU NUME_DDL
C  IN  : IDDL   : TABLEAU DES DDL
C                 IDDL(I) = 0  PAS DE CALCUL DU MODE
C                 IDDL(I) = 1  CALCUL DU MODE
C  IN  : COEF   : COEFFICIENTS A APPLIQUER
C  IN  : NEQ    : NOMBRE D'EQUATIONS DU NUME
C  IN  : NBMODE : NOMBRE DE MODES STATIQUES
C  OUT : ZRMOD  : TABLEAU DES MODES STATIQUES CALCULES
C-----------------------------------------------------------------------
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
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C     ------------------------------------------------------------------
      REAL*8       UN
      CHARACTER*8  NOMCMP(3)
C     ------------------------------------------------------------------
      DATA  NOMCMP / 'DX' , 'DY' , 'DZ' /
C     ------------------------------------------------------------------
      CALL JEMARQ()
      UN   = 1.D0
      IMOD = 0
C
      IF ( MOTCLE(1:4) .EQ. 'ACCE') THEN
         CALL WKVECT('&&MODSTA.POSITION_DDL','V V I',3*NEQ,JDDL)
         CALL PTEDDL('NUME_DDL',NUME,3,NOMCMP,NEQ,ZI(JDDL))
         DO 10 IM = 1,NBMODE
            IMOD = IMOD + 1
            IN2  = 3 * ( IM - 1 )
            CALL WKVECT('&&MODSTA.POSITION_DDR','V V R',NEQ,JDDR)
            DO 12 IC = 1,3
               IND = NEQ * ( IC - 1 )
               DO 14 IN = 0,NEQ-1
                  ZR(JDDR+IN) =  ZR(JDDR+IN) +
     +                                   ZI(JDDL+IND+IN) * COEF(IN2+IC)
 14            CONTINUE
 12         CONTINUE
            CALL MRMULT('ZERO',LMATM,ZR(JDDR),'R',ZRMOD(1,IMOD),1)
            CALL JEDETR('&&MODSTA.POSITION_DDR')
C
 10      CONTINUE
         CALL JEDETR('&&MODSTA.POSITION_DDL')
      ELSE
         DO 20 IE = 1,NEQ
            IF (IDDL(IE).EQ.1) THEN
               IMOD = IMOD + 1
               IF ( MOTCLE(1:4) .EQ. 'DEPL') THEN
                  CALL DDLLAG(NUME,IE,NEQ,ILA1,ILA2)
                  IF (ILA1.EQ.0 .OR. ILA2.EQ.0) THEN
                     CALL UTMESS('F','MODSTA','ERREUR SUR LA RECHER'//
     +                                            'CHE DES LAGRANGES.')
                  ENDIF
                  ZRMOD(ILA1,IMOD) = UN
                  ZRMOD(ILA2,IMOD) = UN
               ELSEIF ( MOTCLE(1:4) .EQ. 'FORC') THEN
                  ZRMOD(IE,IMOD) = UN
               ELSE
                  CALL WKVECT('&&MODSTA.POSITION_DDR','V V R',NEQ,JDDR)
                  CALL DDLLAG(NUME,IE,NEQ,ILA1,ILA2)
                  IF (ILA1.EQ.0 .OR. ILA2.EQ.0) THEN
                     CALL UTMESS('F','MODSTA','ERREUR SUR LA RECHER'//
     +                                            'CHE DES LAGRANGES.')
                  ENDIF
                  ZR(JDDR+ILA1-1) = UN
                  ZR(JDDR+ILA2-1) = UN
                  CALL MRCONL(LMATF,NEQ,' ',ZR(JDDR),1)
                  CALL MRCOND(LMATF,NEQ,ZR(JDDR),1)
                  CALL RLDLGG(LMATF,ZR(JDDR),CBID,1)
                  CALL MRMULT('ZERO',LMATM,ZR(JDDR),'R',ZRMOD(1,IMOD),1)
                  CALL JEDETR('&&MODSTA.POSITION_DDR')
               ENDIF
            ENDIF
 20      CONTINUE
      ENDIF
C
C     --- RESOLUTION ---
      IF ( IMOD .GT. 0 ) THEN
         CALL MRCONL(LMATF,NEQ,' ',ZRMOD,IMOD)
         CALL MRCOND(LMATF,NEQ,ZRMOD,IMOD)
         CALL RLDLGG(LMATF,ZRMOD,CBID,IMOD)
         CALL MRCOND(LMATF,NEQ,ZRMOD,IMOD)
      ENDIF
      CALL JEDEMA()
      END
