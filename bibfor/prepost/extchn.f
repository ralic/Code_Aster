      SUBROUTINE EXTCHN ( NCHMNO, NNOEUD, NUMND, NCMP, NBN, NBC, INDIC,
     &                    NSSCHN, MCF, IOCC )
      IMPLICIT   NONE
      INTEGER             NBN, NBC, NUMND(*), IOCC
      CHARACTER*6         INDIC
      CHARACTER*8         NNOEUD(*), NCMP(*)
      CHARACTER*19        NCHMNO, NSSCHN
      CHARACTER*(*)       MCF
C***********************************************************************
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
C
C     OPERATION REALISEE
C     ------------------
C
C       CONSTRUCTION DE LA SD ASSOCIEE A L' EXTRACTION SUR UN CHAM_NO
C
C     ARGUMENTS
C     ---------
C
C       NCHMNO (IN) : NOM DE LA SD DE TYPE CHAM_NO SUR LAQUELLE
C                     ON EXTRAIT
C
C       NNOEUD (IN) : TABLEAU DES NOMS DE NOEUDS SUR LESQUELS
C                     ON EXTRAIT
C
C       NUMND  (IN) : TABLEAU DES NUMEROS DE NOEUDS SUR LESQUELS
C                     ON EXTRAIT
C
C       NCMP   (IN) : TABLEAU DES NOMS DE COMPOSANTES SUR LESQUELS
C                     ON EXTRAIT
C
C       NBN    (IN) : NBR DE NOEUDS DE NNOEUD
C
C       NBC    (IN) : NBR DE COMPOSANTES DE NCMP
C
C       INDIC  (IN) : INDIQUE SI LES NOEUDS MIS EN JEU SONT DONNES
C                     PAR NOMS ('NOMME', ALORS NNOEUDS EST UTILISE)
C                     OU PAR NUMEROS ('NUMERO', ALORS NUMND EST UTILISE)
C
C       NSSCHN (IN) : NOM DE LA SD DE TYPE SOUS_CHAM_NO CONSTRUITE
C
C                      .VALE     : OJB V R8 --> SGT DE VALEURS
C                                  DOCU = 'CHNO'
C
C                      .PADR     : OJB V I  --> POINTEUR DES NOEUDS
C                                               SUR .VALE
C
C                      .PCMP     : OJB V I  --> TABLE DES CMP ACTIVES
C                                               POUR L' EXTRACTION
C
C                      .NOMA     : OJB E K8 --> NOM DU MAILLAGE
C
C                      .NUGD     : OJB E I  --> NUMERO DE LA GRANDEUR
C
C                      .ERRE     : XD  V I  --> VERIFICATION DE LA
C                                               DEFINITION DES CMP SUR
C                                               LES NOEUDS
C
C     ORGANISATION DE LA SD SOUS_CHAM_NO
C     ----------------------------------
C
C       SI PADR(IN) = 0 ALORS
C       --              -----
C
C           LE NOEUD NUMERO IN N' EST PAS CONCERNE PAR L' EXTRACTION
C
C       SINON
C       -----
C
C           PADR(IN) EST L' ADRESSE DU SOUS-SGT DE VALEURS
C           DANS VALE, ASSOCIEE AU NOEUD NUMERO IN
C
C       FSI
C       ---
C       SI PCMP(ICMP) = 0 ALORS
C       --                -----
C
C           LA CMP NUMERO ICMP N' EST PAS CONCERNE PAR L' EXTRACTION
C
C       SINON
C       -----
C
C           PCMP(ICMP) EST L' ADRESSE DE LA VALEUR DE LA CMP
C           NUMERO ICMP (DANS LE CATALOGUE DES GRANDEURS) DANS
C           TOUS LES SOUS-SGT ASSOCIES AUX NOEUDS
C
C       FSI
C       ---
C
C       INDLOC EST LE NUMERO LOCALE D' UN NOEUD DE L' EXTRACTRATION
C       IL LUI CORRESPOND UN VECTEUR V DANS LA XD '.ERRE' :
C
C          V(JLOCCMP) = 0 <=> LA CMP NUMERO JLOCCMP POUR L' EXTRACTION
C                             EST DEFINIE SUR CE NOEUD
C
C
C***********************************************************************
C
C   FONCTIONS EXTERNES
C   ------------------
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
      CHARACTER*32     JEXNUM, JEXNOM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
C   NOMS ET ADRESSES DES OJB ASSOCIES AUX CHAM_NO
C   ---------------------------------------------
C
      INTEGER      ADESCH,AREFCH,AVALCH,ANUEQ
      CHARACTER*3  TYPE
      CHARACTER*24 NDESCH,NREFCH,NVALCH,NOMAUX,NNUEQ, NOMVEC
C
C   NOMS ET ADREESES DES OJB ASSOCIES AUX SOUS_CHAM_NO
C   --------------------------------------------------
C
      INTEGER      APADR,APCMP,APVAL,ANUGD,APERR,ANOMA
      CHARACTER*24 NPADR,NPCMP,NPVAL,NNUGD,NPERR,NNOMA
C
C   NOMS ET ADRESSES DES OJB ASSOCIES AUX PROFCHNO
C   ----------------------------------------------
C
      INTEGER      APRNO
      CHARACTER*24 NPRNO
      CHARACTER*19 NPROF
C
C   VARIABLES ASSOCIEES A LA GRANDEUR
C   ---------------------------------
C
      INTEGER GD,ACMPGD,ADESGD,NBTCMP,NBEC
C
C   VARIABLES ASSOCIEES AU MAILLAGE
C   -------------------------------
C
      INTEGER     NBTND
      CHARACTER*8 NMAILA,CBID
C
C   VARIABLES COMPLEMENTAIRES
C   -------------------------
C
      INTEGER      NUM,ANUMCP,I,IND,N1,IBID,IER
      REAL*8       ANGL(3), PGL(3,3), R8DGRD, ORIG(3), AXEZ(3)
      REAL*8       ZERO, XNORMZ, EPSI
      LOGICAL      UTILI
      CHARACTER*8  K8B, REPERE
      CHARACTER*24 NOMJV
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
      NDESCH = NCHMNO//'.DESC'
      NREFCH = NCHMNO//'.REFE'
      NVALCH = NCHMNO//'.VALE'
      ZERO = 0.0D0
      EPSI = 1.0D-6
C
      CALL JELIRA ( NVALCH, 'TYPE', IBID, TYPE )
      IF ( TYPE(1:1) .EQ. 'R' ) THEN
         CALL JEVEUO(NVALCH,'L',AVALCH)
      ELSEIF ( TYPE(1:1) .EQ. 'C' ) THEN
         NOMVEC = 'EXTCHN.VECTEUR'
         CALL RVRECU ( MCF, IOCC, NCHMNO, NOMVEC )
         CALL JEVEUO(NOMVEC,'L',AVALCH)
      ELSE
         CALL U2MESS('F','PREPOST_37')
      ENDIF
      CALL JEVEUO(NDESCH,'L',ADESCH)
      CALL JEVEUO(NREFCH,'L',AREFCH)
C
C   EST-CE UN REPERE "UTILISATEUR"
C   ------------------------------
C
      UTILI = .FALSE.
      IF ( MCF(1:6) .EQ. 'ACTION' ) THEN
      CALL GETVTX ( MCF, 'REPERE', IOCC,1,1, REPERE, N1 )
      IF ( REPERE .EQ. 'UTILISAT' ) THEN
         UTILI = .TRUE.
         NOMJV = '&&EXTCHN.NEW_CHAMP'
         CALL GETVR8 ( MCF, 'ANGL_NAUT', IOCC,1,3, ANGL, N1 )
         ANGL(1) = ANGL(1) * R8DGRD()
         ANGL(2) = ANGL(2) * R8DGRD()
         ANGL(3) = ANGL(3) * R8DGRD()
         CALL MATROT ( ANGL, PGL )
         CALL RVCHN1 ( NCHMNO, NOMJV, NBN, NUMND, PGL )
         CALL JEVEUO ( NOMJV, 'L', AVALCH )
      ELSEIF ( REPERE .EQ. 'CYLINDRI' ) THEN
         UTILI = .TRUE.
         NOMJV = '&&EXTCHN.NEW_CHAMP'
         CALL GETVR8 ( MCF, 'ORIGINE', IOCC,1,3, ORIG, N1 )
         CALL GETVR8 ( MCF, 'AXE_Z'  , IOCC,1,3, AXEZ, N1 )
         XNORMZ = ZERO
         DO 30 I = 1,3
            XNORMZ = XNORMZ + AXEZ(I)*AXEZ(I)
 30      CONTINUE
         IF ( XNORMZ .LT. EPSI ) THEN
            CALL U2MESS('F','PREPOST_38')
         ENDIF
         XNORMZ =  1.0D0 / SQRT( XNORMZ )
         DO 32 I = 1,3
            AXEZ(I) = AXEZ(I) * XNORMZ
 32      CONTINUE
         CALL RVCHN2 ( NCHMNO, NOMJV, NBN, NUMND, ORIG, AXEZ )
         CALL JEVEUO ( NOMJV, 'L', AVALCH )
      ENDIF
      ENDIF
C
C   RECUPERATION DE L' INFORMATION
C   ------------------------------
C
      NNOMA = NSSCHN//'.NOMA'
      NNUGD = NSSCHN//'.NUGD'
C
      GD = ZI(ADESCH+1-1)
      NUM = ZI(ADESCH+2-1)
C
      NOMAUX = ZK24(AREFCH+1-1)
      NMAILA = NOMAUX(1:8)
C
      CALL JECREO(NNOMA,'V E K8')
      CALL JEVEUO(NNOMA,'E',ANOMA)
C
      ZK8(ANOMA) = NMAILA
C
      CALL JECREO(NNUGD,'V E I')
      CALL JEVEUO(NNUGD,'E',ANUGD)
C
      ZI(ANUGD) = GD
C
C   RECUPERATION DE LA DESCRIPTION DE LA GRANDEUR DANS LE CATALOGUE
C   ---------------------------------------------------------------
C
      CALL JEVEUO(JEXNUM('&CATA.GD.NOMCMP',GD),'L',ACMPGD)
      CALL JELIRA(JEXNUM('&CATA.GD.NOMCMP',GD),'LONMAX',NBTCMP,K8B)
      CALL JEVEUO(JEXNUM('&CATA.GD.DESCRIGD',GD),'L',ADESGD)
C
      NBEC = ZI(ADESGD+3-1)
C
C   RECUPERATION DU NOMBRE TOTAL DE NOEUD DANS LE MAILLAGE
C   ------------------------------------------------------
C
      CALL DISMOI('F','NB_NO_MAILLA',NMAILA,'MAILLAGE',NBTND,CBID,IER)
C
C   TRADUCTION DES NOMS DE NOEUDS ET DE CMP EN NUMEROS
C   --------------------------------------------------
C
      IF (INDIC.EQ.'NOMME') THEN
C
          DO 10,I = 1,NBN,1
C
              CALL JENONU(JEXNOM(NMAILA//'.NOMNOE',NNOEUD(I)),NUMND(I))
C
   10     CONTINUE
C
      END IF
C
      CALL JECREO('&&EXTRCHNNUMCP','V V I')
      CALL JEECRA('&&EXTRCHNNUMCP','LONMAX',NBC,' ')
      CALL JEVEUO('&&EXTRCHNNUMCP','E',ANUMCP)
C
      DO 20,I = 1,NBC,1
C
          ZI(ANUMCP+I-1) = 0
C
   20 CONTINUE
C
      CALL NUMEK8(ZK8(ACMPGD),NCMP,NBTCMP,NBC,ZI(ANUMCP))
C
C   CREATIONS DES OJB DE LA SD SOUS_CHAM_NO
C   ---------------------------------------
C
      NPADR = NSSCHN//'.PADR'
      NPCMP = NSSCHN//'.PCMP'
      NPVAL = NSSCHN//'.VALE'
      NPERR = NSSCHN//'.ERRE'
C
      CALL JECREO(NPADR,'V V I')
      CALL JEECRA(NPADR,'LONMAX',NBTND,' ')
      CALL JEVEUO(NPADR,'E',APADR)
C
      CALL JECREO(NPCMP,'V V I')
      CALL JEECRA(NPCMP,'LONMAX',NBTCMP,' ')
      CALL JEVEUO(NPCMP,'E',APCMP)
C
      CALL JECREO(NPVAL,'V V R')
      CALL JEECRA(NPVAL,'LONMAX',NBC*NBN,' ')
      CALL JEECRA(NPVAL,'DOCU',NBC,'CHNO')
      CALL JEVEUO(NPVAL,'E',APVAL)
C
      CALL JECREC(NPERR,'V V I','NU','DISPERSE','VARIABLE',NBN)
C
C   REMPLISSAGE DU POINTEUR D' ADRESSES
C   -----------------------------------
C
      DO 100,I = 1,NBTND,1
C
          ZI(APADR+I-1) = 0
C
  100 CONTINUE
C
C
      DO 110,I = 1,NBN,1
C
          ZI(APADR+NUMND(I)-1) = NBC* (I-1) + 1
C
  110 CONTINUE
C
C   REMPLISSAGE DU POINTEUR DES POSITIONS DES CMP
C   ---------------------------------------------
C
      DO 200,I = 1,NBTCMP,1
C
          ZI(APCMP+I-1) = 0
C
  200 CONTINUE
C
      DO 210,I = 1,NBC,1
C
CCC          ZI(APCMP+ZI(ANUMCP+I-1)-1) = I
          ZI(APCMP+I-1) = ZI(ANUMCP+I-1)
C
  210 CONTINUE
C
C   REMPISSAGE DU SGT DE VALEURS
C   ----------------------------
C
      IF (NUM.LT.0) THEN
C
C          /* CAS D' UN CHAM_NO A REPRESENTATION CONSTANTE */
C
          DO 300,I = 1,NBN,1
C
              CALL JECROC(JEXNUM(NPERR,I))
              CALL JEECRA(JEXNUM(NPERR,I),'LONMAX',NBC,' ')
              CALL JEVEUO(JEXNUM(NPERR,I),'E',APERR)
C
              IND = NUMND(I)
C
              CALL EXCHNN(ZI(ADESCH+1-1),IND,ZI(ANUMCP),NBC,ZR(AVALCH),
     &                    IBID,.FALSE.,ZR(APVAL+NBC* (I-1)+1-1),
     &                    ZI(APERR))
C
  300     CONTINUE
C
      ELSE
C
C          /* CAS D' UN CHAM_NO A REPRESENTATION PAR NOEUD */
C          /* CONTENUE DANS LE PRNO (OC 1) DU PROF_CHNO    */
C
C        RECUPERATION DU NOM DU PROF_CHNO
C        --------------------------------
C
          NOMAUX = ZK24(AREFCH+2-1)
          NPROF = NOMAUX(1:19)
C
C        ACCES AU DESCRIPTEUR DU CHAM_NO
C        -------------------------------
C
          NPRNO = NPROF//'.PRNO'
          NNUEQ = NPROF//'.NUEQ'
          CALL JEVEUO(NNUEQ,'L',ANUEQ)
C
          CALL JEVEUO(JEXNUM(NPRNO,1),'L',APRNO)
C
C        RECUPERATION DES VALEURS
C        ------------------------
C
C
          DO 400,I = 1,NBN,1
C
              CALL JECROC(JEXNUM(NPERR,I))
              CALL JEECRA(JEXNUM(NPERR,I),'LONMAX',NBC,' ')
              CALL JEVEUO(JEXNUM(NPERR,I),'E',APERR)
C
              IND = NUMND(I)
C
              CALL EXCHNN(ZI(APRNO+ (IND-1)* (2+NBEC)+1-1),0,ZI(ANUMCP),
     &                    NBC,ZR(AVALCH),ZI(ANUEQ),.TRUE.,
     &                    ZR(APVAL+NBC* (I-1)+1-1),ZI(APERR))
C
  400     CONTINUE
C
      END IF
C
C   DESTRUCTION DES OJB TEMPORAIRES
C   -------------------------------
C
      IF ( UTILI ) CALL JEDETR ( NOMJV )
      CALL JEDETR ( '&&EXTRCHNNUMCP' )
      IF ( TYPE(1:1) .EQ. 'C' ) CALL JEDETR ( NOMVEC )
C
      CALL JEDEMA()
      END
