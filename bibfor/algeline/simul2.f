      SUBROUTINE SIMUL2(RESU,NOMCMD,MASSE,MODSTA,NBDIR,DIR,NOMNOE,NBNO)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER                                    NBDIR,           NBNO
      REAL*8                                          DIR(*)
      CHARACTER*(*)     RESU,NOMCMD,MASSE,MODSTA,         NOMNOE(*)
      CHARACTER*19      MASS2
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 11/05/2009   AUTEUR NISTOR I.NISTOR 
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
C     ------------------------------------------------------------------
C
C     OPERATEUR :   CALC_CHAR_SEISME
C
C     CREE LE VECTEUR SECOND MEMBRE DANS LE CAS D'UN CALCUL SISMIQUE
C     STRUCTURE : MULTI-APPUI
C
C     ------------------------------------------------------------------
C
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
      CHARACTER*32 JEXNUM, JEXNOM, JEXR8, JEXATR
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER      LMAT, NEQ, IBID, IORDR, IER
      REAL*8       R8B, EPSI
      CHARACTER*8  K8B, CMP(6), CRIT
      CHARACTER*24 VALK(3)
      CHARACTER*14 NUME
      CHARACTER*16 ACCES
      CHARACTER*19 RESU2, CHAMNO
      COMPLEX*16   C16B
C     ------------------------------------------------------------------
      DATA CMP / 'DX' , 'DY' , 'DZ' , 'DRX' , 'DRY' , 'DRZ' /
C     ------------------------------------------------------------------
      CALL JEMARQ()
      RESU2 = RESU
      EPSI = R8PREM()
      IER = 0
C
      CALL MTDSCR(MASSE)
      MASS2=MASSE
      CALL JEVEUO(MASS2//'.&INT','E',LMAT)
      CALL DISMOI('F','NB_EQUA'     ,MASSE,'MATR_ASSE',NEQ ,K8B ,IE)
      CALL DISMOI('F','NOM_NUME_DDL',MASSE,'MATR_ASSE',IBID,NUME,IE)
      CALL WKVECT('&&SIMUL2.VECTEUR','V V R',NEQ,IDVE)
C
C     --- CREATION DU CHAM_NO RESULTAT ---
C
      CALL VTCREM(RESU2,MASSE,'G','R')
      CALL JEVEUO(RESU2//'.VALE','E',IDCHM)
C
C     --- ON BOUCLE SUR LES NOEUDS ---
C
      DO 10 ID = 1,NBDIR
         XD = DIR(ID)
         IF (ABS(XD).GT.EPSI) THEN
            DO 20 IN = 1,NBNO
               ACCES(1:8 ) = NOMNOE(IN)
               ACCES(9:16) = CMP(ID)
C
C              --- ON RECUPERE LE MODE STATIQUE ASSOCIE AU NOEUD ---
               CALL RSORAC(MODSTA,'NOEUD_CMP',IBID,R8B,ACCES,C16B,EPSI,
     +                     CRIT,IORDR,1,NBTROU)
               IF (NBTROU.NE.1) THEN
                  IER = IER + 1
                  VALK (1) = ACCES(1:8)
                  VALK (2) = ACCES(9:16)
                  CALL U2MESG('E','ALGELINE5_41',2,VALK,0,0,0,0.D0)
                  GOTO 20
               ENDIF
               CALL RSVPAR(MODSTA,IORDR,'TYPE_DEFO',IBID,R8B,
     +                                  'DEPL_IMPO',IRET)
               IF (IRET.NE.100) THEN
                  IER = IER + 1
                  VALK (1) = 'MODE_MECA'
                  VALK (2) = ACCES(1:8)
                  VALK (3) = ACCES(9:16)
                  CALL U2MESG('E','ALGELINE5_42',3,VALK,0,0,0,0.D0)
                  GOTO 20
               ENDIF
               CALL RSEXCH(MODSTA,'DEPL',IORDR,CHAMNO,IRET)
               IF (IRET.NE.0) THEN
                  IER = IER + 1
                  VALK (1) = CHAMNO
                  VALK (2) = ACCES(1:8)
                  VALK (3) = ACCES(9:16)
                  CALL U2MESG('E','ALGELINE5_43',3,VALK,0,0,0,0.D0)
                  GOTO 20
               ELSE
                  CALL JEVEUO(CHAMNO//'.VALE','L',IDMST)
C
C                 --- ON EFFECTUE LE PRODUIT  MASSE * CHAM_NO ---
                  DO 22 I = 0,NEQ-1
                     ZR(IDVE+I) = -XD * ZR(IDMST+I)
 22               CONTINUE
                  CALL JELIBE(CHAMNO//'.VALE')
                  CALL MRMULT('CUMU',LMAT,ZR(IDVE),'R',ZR(IDCHM),1)
               ENDIF
 20         CONTINUE
         ENDIF
 10   CONTINUE
      IF (IER.NE.0) THEN
         CALL U2MESS('F','ALGELINE5_40')
      ENDIF
C
      CALL WKVECT('&&SIMUL2.DDL.BLOQUE','V V I',NEQ,IDDL)
      CALL TYPDDL('BLOQ',NUME,NEQ,ZI(IDDL),NBA,NBB,NBL,NBLIAI)
C
      DO 30 IN = 0,NEQ-1
         ZR(IDCHM+IN) = ( 1 - ZI(IDDL+IN) ) * ZR(IDCHM+IN)
 30   CONTINUE
C
      CALL JELIBE(RESU2//'.VALE')
      CALL JEDETC(' ','&&SIMUL2',1)
C
      CALL JEDEMA()
      END
