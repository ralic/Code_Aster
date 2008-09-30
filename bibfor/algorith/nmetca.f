      SUBROUTINE NMETCA(MODELE,MAILLA,MATE  ,SDDISC,NUMINS,
     &                  VALPLU,VALMOI)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 29/09/2008   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
C NON-LINEAIRE MECANIQUE - ERREUR EN TEMPS - CALCUL
C *            *           *         *       **
C ----------------------------------------------------------------------
C
C COMMANDE STAT_NON_LINE : CALCUL DE L'INDICATEUR D'ERREUR TEMPORELLE
C                          POUR LES MODELISATIONS HM SATUREES AVEC
C                          COMPORTEMENT MECANIQUE ELASTIQUE
C
C ----------------------------------------------------------------------
C      IN  MODELE : NOM DU MODELE
C      IN  MATE   : NOM DU MATERIAU
C      IN  TBGRCA : TABLEAU GRANDEURS CARACTERISTIQUES
C                   POUR ADIMENSIONNEMENT
C      IN  INST   : INST(1) : INSTANT DE CALCUL ACTUEL
C                   INST(2) : DELTAT
C      IN  MAILLA : MAILLAGE SOUS-TENDU PAR LE MAILLAGE
C      IN  VALPLU : ETAT EN T+ (CONTRAINTES A L'INSTANT ACTUEL)
C      IN  VALMOI : ETAT EN T- (CONTRAINTES A L'INSTANT PRECEDENT)
C ----------------------------------------------------------------------
      IMPLICIT NONE

C 0.1. ==> ARGUMENTS

      CHARACTER*8  MAILLA
      CHARACTER*24 MODELE, MATE, VALPLU(8),VALMOI(8)
      INTEGER      NUMINS
      CHARACTER*19 SDDISC
      

C 0.2. ==> COMMUNS

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
      CHARACTER*16               ZK16
      CHARACTER*24                        ZK24
      CHARACTER*32                                 ZK32
      CHARACTER*80                                          ZK80
      COMMON  / KVARJE / ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------

C 0.3. ==> VARIABLES LOCALES

      CHARACTER*6 NOMPRO
      PARAMETER ( NOMPRO = 'NMETCA' )

C NBRIN  = NOMBRE DE PARAMETRES A PASSER DANS CALCUL
      INTEGER NBRIN
      PARAMETER   ( NBRIN = 6 )
      INTEGER IRET
C
      INTEGER      NPARA
      PARAMETER  ( NPARA = 2 )
C
      INTEGER      IAUX,IBID,IERD,ICMP,CODRET
      INTEGER      DIARCH,NUMARC
      INTEGER      IORDR
      INTEGER      LJEVEU(NPARA)
      LOGICAL      LBID,FORCE,INCR
      CHARACTER*1  BASE,KBID
      CHARACTER*8  LPAIN(NBRIN),LPAOUT(1),KCMP,LICMP(2)
      CHARACTER*16 LPARA(NPARA)
      CHARACTER*16 OPTION,CONCEP,NOMCMD
      CHARACTER*19 RESUCO
      CHARACTER*24 LIGRMO,CHGEOM,LCHIN(NBRIN)
      CHARACTER*24 CHTIME,LCHOUT(1)
      CHARACTER*24 CARTCA, CHELEM
      CHARACTER*24 K24BID,SIGMAM,SIGMAP
      COMPLEX*16   CBID,CCMP
      REAL*8       RCMP(2),SOMME(1)
      REAL*8       DIINST,INSTAP,INSTAM,DELTAT
      REAL*8       LONGC, PRESC
      REAL*8       R8BID
      REAL*8       TABERR(NPARA)
      REAL*8       TBGRCA(3)
C
C     ------------------------------------------------------------------
      DATA LPARA / 'ERRE_TPS_LOC', 'ERRE_TPS_GLOB'/
C     ------------------------------------------------------------------
C
C ----------------------------------------------------------------------
      CALL JEMARQ()
C  
C --- NUMERO ARCHIVE
C    
      FORCE = .FALSE.
      INCR  = .FALSE.
      NUMARC = DIARCH(SDDISC,NUMINS,FORCE,INCR  )    
C      
C --- INSTANTS
C     
      INSTAP = DIINST(SDDISC,NUMINS)
      INSTAM = DIINST(SDDISC,NUMINS-1)      
      DELTAT = INSTAP-INSTAM
C
C --- RECUPERATION TABLEAU GRANDEURS
C
      CALL CETULE(MODELE,TBGRCA,CODRET)
C
C --- CONTRAINTES
C
      CALL DESAGG(VALMOI,K24BID,SIGMAM,K24BID,K24BID,
     &            K24BID,K24BID,K24BID,K24BID)
      CALL DESAGG(VALPLU,K24BID,SIGMAP,K24BID,K24BID,
     &            K24BID,K24BID,K24BID,K24BID)     
C
C ===================================================
C 1. CALCUL POUR UN INSTANT > 0
C ===================================================
C
      IF ( NUMARC.GT.0 ) THEN
C
C 1.0. VALEURS INITIALES DE L'ERREUR
C
C
        CALL GETRES(RESUCO,CONCEP,NOMCMD)
C
        IORDR = NUMARC-1
C
        CALL RSADPA(RESUCO,'L',NPARA,LPARA,IORDR,0,LJEVEU,KBID)
C
        DO 10 , IAUX = 1 , NPARA
          TABERR(IAUX) = ZR(LJEVEU(IAUX))
 10     CONTINUE
C
C 1.1. MISE EN FORME DES PARAMETRES EN ARGUMENTS
C
C
        LONGC = TBGRCA(1)
        PRESC = TBGRCA(2)
C
C 1.2. PARAMETRES DU CALCUL
C
        BASE = 'V'
C
        CALL DISMOI('F','NOM_LIGREL',MODELE,'MODELE',
     &            IBID,LIGRMO,IERD)
        CALL MEGEOM(MODELE,' ',LBID,CHGEOM)
C
        OPTION = 'ERRE_TEMPS'
C
C 1.3. CARTE DES PARAMETRES TEMPORELS
C
        CALL MECHTI(MAILLA,INSTAP,R8BID,R8BID,CHTIME)
C
        LICMP(1) = 'X1'
        LICMP(2) = 'X2'
        RCMP(1)  = LONGC
        RCMP(2)  = PRESC
C
C               12   345678   9012345678901234
        CARTCA = '&&'//NOMPRO//'.GRDCA          '
        CALL MECACT ( BASE, CARTCA,'MODELE',LIGRMO,
     &              'NEUT_R',2,LICMP,ICMP,RCMP,CCMP,KCMP)
C
C 1.4. CALCUL DES INDICATEURS LOCAUX PAR ELEMENT
C
        LPAIN(1) = 'PGEOMER'
        LCHIN(1) = CHGEOM
        LPAIN(2) = 'PMATERC'
        LCHIN(2) = MATE
        LPAIN(3) = 'PCONTGP'
        LCHIN(3) = SIGMAP
        LPAIN(4) = 'PCONTGM'
        LCHIN(4) = SIGMAM
        LPAIN(5) = 'PTEMPSR'
        LCHIN(5) = CHTIME
        LPAIN(6) = 'PGRDCA'
        LCHIN(6) = CARTCA
C
C                 12   345678   9012345678901234
        CHELEM = '&&'//NOMPRO//'_ERRE_TEMPS     '
        LPAOUT(1) = 'PERREUR'
        LCHOUT(1) = CHELEM
C
        CALL CALCUL('S',OPTION,LIGRMO,NBRIN,LCHIN,LPAIN,
     &               1,LCHOUT,LPAOUT,BASE)
C
        CALL EXISD('CHAMP_GD',LCHOUT(1),IRET)
        IF (IRET.EQ.0) THEN
          CALL U2MESK('A','CALCULEL2_88',1,OPTION)
          GOTO 9999
        ENDIF
C
C 1.5. PASSAGE A UNE VALEUR GLOBALE EN ESPACE
C
        CALL MESOMM(LCHOUT(1),1,IBID,SOMME,CBID,0,IBID)

C --- INDICATEUR D'ERREUR LOCAL EN TEMPS / GLOBAL EN ESPACE

        TABERR(1) = SQRT(DELTAT*SOMME(1))

C --- INDICATEUR D'ERREUR GLOBAL EN TEMPS / GLOBAL EN ESPACE

        TABERR(2) = SQRT(TABERR(2)**2 + TABERR(1)**2)
C
      ENDIF
C
C ===================================================
C 2. ARCHIVAGE DES RESULTATS DANS LA SD RESULTAT
C ===================================================
C
      IORDR = NUMARC
      CALL RSADPA(RESUCO,'E',NPARA,LPARA,IORDR,0,LJEVEU,KBID)
C
      DO 20 , IAUX = 1 , NPARA
        ZR(LJEVEU(IAUX)) = TABERR(IAUX)
 20   CONTINUE
C
C ===================================================
C 3. MENAGE
C ===================================================
C
 9999 CONTINUE
C
      CALL JEDETC ( BASE, '&&'//NOMPRO, 1 )
C
      CALL JEDEMA()
C
      END
