      SUBROUTINE NUROTA (NU, COMPO, NURO, REAROT)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 15/04/99   AUTEUR CIBHHLV L.VIVAN 
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
      CHARACTER*14 NU
      CHARACTER*19 COMPO, NURO
      LOGICAL      REAROT
C ----------------------------------------------------------------------
C     CREATION DE LA S.D. DE NOM NURO QUI INDIQUE QUELLES SONT LES     -
C     ADRESSES DANS L'OBJET .VALE D'UN CHAM_NO S'APPUYANT SUR NU       -
C     DES ROTATIONS DRX, DRY, DRZ DES NOEUDS DES MAILLES POUR          -
C     LESQUELLES LA CARTE DE COMPORTEMENT INDIQUE : POUTRE_GD          -
C     (POUTRES EN GRANDES ROTATIONS)                                   -
C ----------------------------------------------------------------------
C  NU            - IN    - K14  - : NOM NUME_DDL                       -
C                - JXIN  -      -                                      -
C ----------------------------------------------------------------------
C  COMPO         - IN    - K19  - : NOM DE LA CARTE COMPOR             -
C                - JXIN  -      -                                      -
C ----------------------------------------------------------------------
C  NURO          - IN    - K19  - : NOM DE LA S.D. NUME_DDL_ROTA       -
C                - JXVAR -      -                                      -
C ----------------------------------------------------------------------
C  REAROT        - OUT   - L    - : VRAI S' IL Y A DES NOEUDS EN       -
C                -       -      -              GRANDES ROTATIONS       -
C                -       -      -   FAUX SINON                         -
C ----------------------------------------------------------------------
C
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
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*17                 ZK17
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
C --------- VARIABLES LOCALES ------------------------------------------
      INTEGER       DG
      CHARACTER*8   NOCMP, NOMGD, MODELE, NOMA, K8BID, DEFORM
      CHARACTER*16  COMPT
      CHARACTER*19  LIGRMO
      CHARACTER*24  NOLILI
      LOGICAL       EXISDG
C --------- FIN  DECLARATIONS  VARIABLES LOCALES ----------------------
C
C
C --- INITIALISATIONS ---
C
      CALL JEMARQ()
      REAROT = .FALSE.
      DEFORM = 'GREEN_GR'
      NOMGD  = 'COMPOR  '
C
      CALL DISMOI('F','NB_EC',NOMGD,'GRANDEUR',NEC,K8BID,IER)
      IF (NEC.GT.1) THEN
         CALL UTMESS('F','NUROTA',
     &                   'LE DESCRIPTEUR_GRANDEUR DE COMPOR'//
     &                    ' NE TIENT PAS SUR UN SEUL ENTIER_CODE')
      ENDIF
C
C --- MODELE ASSOCIE AU NUME_DDL ---
C
      CALL DISMOI('F','NOM_MODELE',NU,'NUME_DDL',IBID,MODELE,IER)
C
C --- NOM DU MAILLAGE ---
C
      CALL DISMOI('F','NOM_MAILLA',MODELE,'MODELE',IBID,NOMA,IER)
C
C --- NOMBRE DE MAILLES DU MAILLAGE ---
C
      CALL DISMOI('F','NB_MA_MAILLA',NOMA,'MAILLAGE',NBMA,K8BID,IER)
C
C --- NOMBRE DE NOEUDS DU MAILLAGE ---
C
      CALL DISMOI('F','NB_NO_MAILLA',NOMA,'MAILLAGE',NBNOEU,K8BID,IER)
C
      LIGRMO = MODELE//'.MODELE'
C
C --- CREATION DU TABLEAU DESCRIPTEUR DE LA CARTE COMPOR ---
C
      CALL ETENCA(COMPO, LIGRMO, IRET)
      IF (IRET.NE.0) THEN
         CALL UTMESS('F','NUROTA',
     &                   'ERREUR DANS ETENCA')
      ENDIF
C
C --- CREATION D'UN VECTEUR DESTINE A CONTENIR LES NUMEROS ---
C --- DES NOEUDS EN GRANDES ROTATIONS                      ---
C
      CALL WKVECT('&&NUROTA.NOEUDS.GR','V V I',NBNOEU,ITRAV)
C
C --- RECUPERATION DE LA GRANDEUR (ICI COMPOR)  ---
C --- REFERENCEE PAR LA CARTE COMPO             ---
C
      CALL JEVEUO(COMPO//'.DESC','L',IDESC)
      NGDMAX = ZI(IDESC+2-1)
C
C --- NOMBRE DE COMPOSANTES ASSOCIEES A LA GRANDEUR  ---
C
      CALL JELIRA(JEXNOM('&CATA.GD.NOMCMP',NOMGD),'LONMAX',NCMPMX,K8BID)
C
C --- TABLEAU DE VALEURS DE LA CARTE COMPO     ---
C --- (CONTENANT LES VALEURS DU COMPORTEMENT)  ---
C
      CALL JEVEUO(COMPO//'.VALE','L',IVALE)
C
C --- RECUPERATION DU VECTEUR D'ADRESSAGE DANS LA CARTE  ---
C --- CREE PAR ETENCA                                    ---
C
      CALL JEVEUO(COMPO//'.PTMA','L',IPTMA)
C
C --- AFFECTATION DU TABLEAU DES NOEUDS EN GRANDES ROTATIONS  ---
C
      DO 10 IMA = 1, NBMA
         IF (ZI(IPTMA+IMA-1).NE.0) THEN
             IGD   = ZI(IPTMA+IMA-1)
             IDEBGD = (IGD-1)*NCMPMX
             DG = ZI(IDESC+3+2*NGDMAX+ZI(IPTMA+IMA-1)-1)
C
C ---     ON S'ASSURE QUE LA PREMIERE COMPOSANTE DE LA GRANDEUR
C ---     QUI EST RELCOM A BIEN ETE AFFECTEE
C
             IF (.NOT.EXISDG(DG,1)) THEN
                 CALL UTMESS('F','NUROTA',
     &                       'LA COMPOSANTE RELCOM N''A PAS ETE'//
     &                       ' AFFECTEE POUR LA GRANDEUR COMPOR')
             ENDIF
C ---     RECUPERATION DU COMPORTEMENT AFFECTE A LA MAILLE
             COMPT = ZK16(IVALE+IDEBGD+3-1)
             IF ( COMPT(1:8) .NE. DEFORM ) GOTO 10
C ---     RECUPERATION DES NUMEROS DES NOEUDS DE LA MAILLE
             CALL JEVEUO(JEXNUM(NOMA//'.CONNEX',IMA),'L',ICONEX)
             CALL JELIRA(JEXNUM(NOMA//'.CONNEX',IMA),'LONMAX',
     &                   NBNO,K8BID)
             DO 20 INO = 1, NBNO
                ZI(ITRAV+ZI(ICONEX+INO-1)-1) = 1
  20         CONTINUE
          ENDIF
  10  CONTINUE
C
C --- NOMBRE DE NOEUDS EN GRANDES ROTATIONS  ---
C
      NBNOC = 0
      DO 30 INO = 1, NBNOEU
         IF (ZI(ITRAV+INO-1).EQ.1) THEN
             NBNOC = NBNOC + 1
         ENDIF
  30  CONTINUE
C
C --- CREATION DU TABLEAU DES NUMEROS D'EQUATIONS CORRESPONDANT   ---
C --- AUX DDLS DE ROTATION POUR LES NOEUDS EN GRANDES ROTATIONS   ---
C
C --- RECUPERATION DU NOMBRE D'INCONNUES DU MODELE  ---
      CALL JELIRA(NU(1:14)//'.NUME.NUEQ','LONUTI',NEQUA,K8BID)
      IF (NBNOC.GT.0) THEN
          CALL WKVECT(NURO//'.NDRO','V V I',NEQUA,INDRO)
          REAROT = .TRUE.
      ELSE
          REAROT = .FALSE.
          GOTO 9999
      ENDIF
C
      NOMGD = 'DEPL_R'
      CALL DISMOI('F','NB_EC',NOMGD,'GRANDEUR',NEC,K8BID,IER)
C
C --- NOMBRE DE COMPOSANTES ASSOCIEES A LA GRANDEUR DEPL_R ---
C
      CALL JELIRA(JEXNOM('&CATA.GD.NOMCMP',NOMGD),'LONMAX',NCMPMX,K8BID)
      CALL JEVEUO(JEXNOM('&CATA.GD.NOMCMP',NOMGD),'L',IANCMP)
C
      NOCMP = 'DRZ'
C
C --- LOCALISATION DE DRZ DANS LA LISTE DES DDLS ASSOCIES  ---
C ---  A LA GRANDEUR DEPL_R                                ---
C
      IDRZ = INDIK8(ZK8(IANCMP),NOCMP,1,NCMPMX)
      IF (IDRZ.EQ.0) THEN
         CALL UTMESS('F','NUROTA','LE DDL : '//NOCMP//
     &                   'N''EXISTE PAS DANS LA GRANDEUR : '//NOMGD)
      ENDIF
C
C --- RECUPERATION DU .PRNO ASSOCIE AU MAILLAGE  ---
C
      CALL JELIRA(NU(1:14)//'.NUME.PRNO','NMAXOC',NLILI,K8BID)
      K = 0
      DO 40 I = 1, NLILI
         CALL JENUNO(JEXNUM(NU(1:14)//'.NUME.LILI',I),NOLILI)
         IF (NOLILI(1:8).NE.'&MAILLA ') GOTO 40
         K = I
  40  CONTINUE
      IF (K.EQ.0) THEN
         CALL UTMESS('F','NUROTA',
     &                   'ERREUR DANS LA RECUPERATION DU NUME.PRNO')
      ENDIF
      CALL JEVEUO(JEXNUM(NU(1:14)//'.NUME.PRNO',K),'L',IAPRNO)
C
C --- TABLEAU DES NUMEROS D'EQUATIONS  ---
C
      CALL JEVEUO(NU(1:14)//'.NUME.NUEQ','L',IANUEQ)
C
C --- AFFECTATION DU TABLEAU DES NUMEROS DES INCONNUES ROTATIONS  ---
C --- DES NOEUDS EN GRANDES ROTATIONS                             ---
C
      INOC = 0
      DO 50 INO = 1, NBNOEU
         IF (ZI(ITRAV+INO-1).EQ.0) GOTO 50
         INOC = INOC + 1
C ---  IVAL  : ADRESSE DU DEBUT DU NOEUD INO DANS .NUEQ
         IVAL = ZI(IAPRNO+(INO-1)*(NEC+2)+1-1)
C ---  NCMP  : NOMBRE DE COMPOSANTES SUR LE NOEUD INO
C ---  IADG  : DEBUT DU DESCRIPTEUR GRANDEUR DU NOEUD INO
         IADG = IAPRNO+(INO-1)*(NEC+2)+3-1
C
         DO 60 I = IDRZ-2, IDRZ
             IF (.NOT.EXISDG(ZI(IADG),I)) THEN
                 CALL UTMESS('F','NUROTA',
     &                       'INCOMPATIBILITE SUR LA DESCRIPTION '//
     &                       'DES DDLS DE LA GRANDEUR')
             ENDIF
  60     CONTINUE
         ICO = 0
         DO 70 I = 1, IDRZ-3
             IF (EXISDG(ZI(IADG),I)) THEN
                  ICO = ICO + 1
             ENDIF
  70     CONTINUE
C
         ZI(INDRO-1+IVAL-1+ICO+1) = 1
         ZI(INDRO-1+IVAL-1+ICO+2) = 1
         ZI(INDRO-1+IVAL-1+ICO+3) = 1
C
  50  CONTINUE
C
      CALL JEDETC('V','&&NUROTA',1)
C
9999  CONTINUE
C
      CALL JEDEMA()
      END
