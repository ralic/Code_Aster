      SUBROUTINE DIAG99 ( NOMRES )
      IMPLICIT  NONE
      CHARACTER*8         NOMRES
C----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 16/09/2008   AUTEUR PELLET J.PELLET 
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
C----------------------------------------------------------------------
C
C         DEFI_BASE_MODALE : DIAG_MASS
C
C CE MOT CLE PERMET DE DIAGONALISER LA MATRICE DE MASSE EN DEUX ETAPES
C
C 1- RETIRER AUX MODES STATIQUES LEUR CONTRIBUTION SUR LES
C    MODES DYNAMIQUES
C
C 2- ORTHOGONALISER LA FAMILLES DES MODES STATIQUES MODIFIES PAR
C    LE PROCEDE DE GRAAM-SCHMIDT
C
C----------------------------------------------------------------------
C-------- DEBUT COMMUNS NORMALISES  JEVEUX ----------------------------
C
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16              ZK16
      CHARACTER*24                        ZK24
      CHARACTER*32                                  ZK32
      CHARACTER*80                                            ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
C-----  FIN  COMMUNS NORMALISES  JEVEUX  ------------------------------
      INTEGER       IAD, JIAD, LLREF, IER, IRET,
     +              IBID, JORDM, IDMODE, LMASSE, IDSTAT, JORDS,
     +              JTRAV1, JTRAV2, JTRAV3, JTRAV4, JNSTA,
     +              I, J, K, IEQ, NBORD, NBMODE, NBSTAT, NEQ, N1,
     +              IORNE, IOROL, JUTIL, JVALE
      REAL*8        ALPHA, R8SCAL,DDOT
      CHARACTER*8   K8B, MECA, STAT
      CHARACTER*14  NU
      CHARACTER*24  MASSE, NUMDDL, MAILLA
      CHARACTER*19  CHAMOL
C----------------------------------------------------------------------
      CALL JEMARQ()
C
C----------------------------------------------------------------------
C --- RECUPERATION DES MODES PROPRES
C-----------------------------------------------------------------------
C
      CALL GETVID ( 'DIAG_MASS', 'MODE_MECA', 1,1,1, MECA, N1 )
C
      CALL JELIRA(  MECA//'           .ORDR','LONUTI',NBMODE,K8B)
      CALL JEVEUO(  MECA//'           .ORDR','L',JORDM)
      CALL JEVEUO(NOMRES//'           .REFD','L',LLREF)
      MASSE  = ZK24(LLREF+1)
      NUMDDL = ZK24(LLREF+3)
      NU     = NUMDDL(1:14)
C
      CALL DISMOI('F','NOM_MAILLA',NUMDDL,'NUME_DDL',IBID,MAILLA,IER)
      CALL DISMOI('F','NB_EQUA',MASSE,'MATR_ASSE',NEQ,K8B,IER)
      CALL WKVECT ('&&DIAG99.MODE_MECA','V V R',NBMODE*NEQ,IDMODE)
      CALL COPMOD (MECA,'DEPL',NEQ,NU,NBMODE,ZR(IDMODE))

C-----------------------------------------------------------------------
C --- RECUPERATION DES MODES STATIQUES
C-----------------------------------------------------------------------
C
      CALL GETVID ( 'DIAG_MASS', 'MODE_STAT', 1,1,1, STAT, N1 )
C
      CALL JELIRA(STAT//'           .ORDR','LONUTI',NBSTAT,K8B)
      CALL JEVEUO(STAT//'           .ORDR','L',JORDS)
      CALL WKVECT ('&&DIAG99.MODE_STAT','V V R',NBSTAT*NEQ,IDSTAT)
      CALL COPMOD (STAT,'DEPL',NEQ,NU,NBSTAT,ZR(IDSTAT))

C-----------------------------------------------------------------------
C --- RECUPERATION DU DESCRIPTEUR DE LA MATRICE DE MASSE
C-----------------------------------------------------------------------
      CALL MTDSCR(MASSE)
      CALL JEVEUO(MASSE(1:19)//'.&INT','L',LMASSE)

C-----------------------------------------------------------------------
C 1- RETIRER AUX MODES STATIQUES LEUR CONTRIBUTION AUX MODES PROPRES
C MODE STAT J =
C MODE STAT J - SOMME (T(MODE STAT J)*MASSE*MODE PROPRE I)*MODE PROPRE I
C OU T(VECTEUR) EST LA TRANSPOSEE DU VECTEUR
C-----------------------------------------------------------------------
      CALL WKVECT ('&&DIAG99.NEW_STAT','V V R',NBSTAT*NEQ,JNSTA )
      CALL WKVECT ('&&DIAG99.TRAV1'   ,'V V R',NEQ       ,JTRAV1)
      CALL WKVECT ('&&DIAG99.TRAV2'   ,'V V R',NEQ       ,JTRAV2)
      CALL WKVECT ('&&DIAG99.TRAV3'   ,'V V R',NBSTAT    ,JTRAV3)
      CALL WKVECT ('&&DIAG99.TRAV4'   ,'V V I',NEQ       ,JTRAV4)
C
      DO 10 J=1,NBSTAT
C
         CALL INITVE ( NEQ, ZR(JTRAV1) )
C
         DO 20 I=1,NBMODE
C
C --------- PRODUIT MASSE*MODE PROPRE I
            CALL MRMULT ( 'ZERO', LMASSE, ZR(IDMODE+(I-1)*NEQ), 'R',
     +                    ZR(JTRAV2), 1 )
C
C --------- (T(MODE STAT J)*MASSE*MODE PROPRE I)
            R8SCAL=DDOT(NEQ,ZR(IDSTAT+(J-1)*NEQ),1,ZR(JTRAV2),1)
C
C --------- PRODUIT (T(MODE STAT J)*MASSE*MODE PROPRE I)*MODE PROPRE I
C --------- PUIS
C --------- SOMME (T(MODE STAT J)*MASSE*MODE PROPRE I)*MODE PROPRE I
            DO 30 K=1,NEQ
               ZR(JTRAV1+(K-1)) = ZR(JTRAV1+(K-1)) +
     +                           R8SCAL * ZR(IDMODE+(I-1)*NEQ+(K-1))
 30         CONTINUE
 20      CONTINUE
C
         DO 40 K=1,NEQ
            ZR(JNSTA+(J-1)*NEQ+(K-1)) = ZR(IDSTAT+(J-1)*NEQ+(K-1)) -
     +                                  ZR(JTRAV1+(K-1))
 40      CONTINUE
C
 10   CONTINUE
C
      DO 50 I = 1,NEQ
         ZI(JTRAV4+I-1) = 1
 50   CONTINUE
      ALPHA = 0.717D0
C
      CALL VPGSKP ( NEQ, NBSTAT, ZR(JNSTA), ALPHA, LMASSE, 2,
     +              ZR(JTRAV1), ZI(JTRAV4), ZR(JTRAV3) )
C
      NBORD = NBMODE + NBSTAT
      CALL RSCRSD('G',NOMRES,'BASE_MODALE',NBORD)
C
      CALL JEEXIN(NOMRES//'           .UTIL',IRET)
      IF (IRET.NE.0) CALL JEDETR(NOMRES//'           .UTIL')
      CALL WKVECT(NOMRES//'           .UTIL','G V I',4,JUTIL)
      ZI(JUTIL  ) = 4
      ZI(JUTIL+1) = NBORD
      ZI(JUTIL+2) = NBMODE
      ZI(JUTIL+3) = NBSTAT
C
      IORNE =0
      DO 80 I=1,NBMODE
         IOROL = ZI(JORDM+I-1)
         IORNE = IORNE+1
C
         CALL RSEXCH(  MECA,'DEPL',IOROL,CHAMOL,IER)
         CALL RSNOCH(NOMRES,'DEPL',IORNE,CHAMOL)
C
         CALL RSADPA(  MECA, 'L',1,'NUME_MODE',IOROL,0, IAD,K8B)
         CALL RSADPA(NOMRES, 'E',1,'NUME_MODE',IORNE,0,JIAD,K8B)
         ZI(JIAD) = ZI(IAD)
C
         CALL RSADPA(  MECA, 'L',1,'FREQ',IOROL,0, IAD,K8B)
         CALL RSADPA(NOMRES, 'E',1,'FREQ',IORNE,0,JIAD,K8B)
         ZR(JIAD) = ZR(IAD)
C
         CALL RSADPA(  MECA, 'L',1,'NORME' ,IOROL,0, IAD,K8B)
         CALL RSADPA(NOMRES, 'E',1,'NORME' ,IORNE,0,JIAD,K8B)
         ZK24(JIAD) = ZK24(IAD)
C
         CALL RSADPA(  MECA, 'L',1,'OMEGA2',IOROL,0, IAD,K8B)
         CALL RSADPA(NOMRES, 'E',1,'OMEGA2',IORNE,0,JIAD,K8B)
         ZR(JIAD) = ZR(IAD)
C
         CALL RSADPA(  MECA, 'L',1,'MASS_GENE',IOROL,0, IAD,K8B)
         CALL RSADPA(NOMRES, 'E',1,'MASS_GENE',IORNE,0,JIAD,K8B)
         ZR(JIAD) = ZR(IAD)
C
         CALL RSADPA(  MECA, 'L',1,'RIGI_GENE',IOROL,0, IAD,K8B)
         CALL RSADPA(NOMRES, 'E',1,'RIGI_GENE',IORNE,0,JIAD,K8B)
         ZR(JIAD) = ZR(IAD)
 80   CONTINUE
C
      DO 90 I=1,NBSTAT
        IOROL = ZI(JORDS+I-1)
        IORNE = IORNE+1
C
        CALL RSADPA(NOMRES, 'E',1,'NUME_MODE',IORNE,0,JIAD,K8B)
        ZI(JIAD) = IOROL+NBMODE
C
        CALL RSADPA(NOMRES, 'E',1,'FREQ',IORNE,0,JIAD,K8B)
        ZR(JIAD) = 0.D0
C
        CALL RSADPA(NOMRES, 'E',1,'OMEGA2'   ,IORNE,0,JIAD,K8B)
        ZR(JIAD) = 0.D0
C
        CALL RSADPA(NOMRES, 'E',1,'MASS_GENE',IORNE,0,JIAD,K8B)
        ZR(JIAD) = 0.D0
C
        CALL RSADPA(NOMRES, 'E',1,'RIGI_GENE',IORNE,0,JIAD,K8B)
        ZR(JIAD) = 0.D0
C
        CALL RSADPA(  STAT, 'L',1,'NOEUD_CMP',IOROL,0, IAD,K8B)
        CALL RSADPA(NOMRES, 'E',1,'NOEUD_CMP',IORNE,0,JIAD,K8B)
        ZK16(JIAD) = ZK16(IAD)
C
        CALL RSADPA(  STAT, 'L',1,'TYPE_DEFO',IOROL,0, IAD,K8B)
        CALL RSADPA(NOMRES, 'E',1,'TYPE_DEFO',IORNE,0,JIAD,K8B)
        ZK16(JIAD) = ZK16(IAD)
C
        CALL RSEXCH(NOMRES,'DEPL',IORNE,CHAMOL,IER)
        CALL VTCREM(CHAMOL,MASSE,'G','R')
        CALL JEVEUO(CHAMOL//'.VALE','E',JVALE)
         DO 111 IEQ = 1 , NEQ
           ZR(JVALE+IEQ-1) = ZR(JNSTA+(I-1)*NEQ+IEQ-1)
 111     CONTINUE
        CALL RSNOCH(NOMRES,'DEPL',IORNE,' ')
 90    CONTINUE
C
      CALL JEDETR('&&DIAG99.TRAV1')
      CALL JEDETR('&&DIAG99.TRAV2')
      CALL JEDETR('&&DIAG99.TRAV3')
      CALL JEDETR('&&DIAG99.TRAV4')
      CALL JEDETR('&&DIAG99.TRAV5')
      CALL JEDETR('&&DIAG99.TRAV6')
      CALL JEDETR('&&DIAG99.MODE_MECA')
      CALL JEDETR('&&DIAG99.MODE_STAT')
C
      CALL JEDEMA()
      END
