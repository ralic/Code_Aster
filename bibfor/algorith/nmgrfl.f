      SUBROUTINE NMGRFL(NUMEDD,SDDYNA,DEPMOI,DEPDEL,VITESS,
     &                  ACCELE,DT    ,CNGRFL)

C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 01/12/2008   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C TOLE CRP_20
C
      IMPLICIT NONE
      REAL*8       DT
      CHARACTER*24 NUMEDD,DEPMOI, DEPDEL, VITESS, ACCELE
      CHARACTER*24 CNGRFL
      CHARACTER*19 SDDYNA
C 
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (CALCUL)
C
C CALCUL DES FORCES FLUIDES NON LINEAIRES S'EXERCANT SUR LES
C GRAPPES LORS DE LA CHUTE DE CELLES-CI
C      
C ----------------------------------------------------------------------
C 
C
C   ARGUMENT        E/S  TYPE         ROLE
C    NUMEDD         IN    K24       NUME_DDL
C    CHARGE         IN    K24       CHARGEMENT CONTENANT LES DONNEES
C                                   POUR LE CALCUL DES FORCES FLUIDES
C                                   S'EXERCANT SUR LA GRAPPE
C    DEPMOI         IN    K24       DEPLACEMENT A L'INSTANT PRECEDENT
C    DEPDEL         IN    K24       INCREMENT DE DEPLACEMENT
C    VITESS         IN    K24       VITESSES A L'INSTANT COURANT
C    ACCELE         IN    K24       ACCELERATIONS A L'INSTANT COURANT
C    DT             IN    R         PAS DE TEMPS
C    IT             IN    I         NUMERO D'ITERATION
C    CNGRFL        VAR    K24       VECTEUR ASSEMBLE DU CHARGEMENT
C                                   DU AUX FORCES FLUIDES
C
C
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C
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
      CHARACTER*32  JEXNUM
C
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      INTEGER      NBNO, NLILI, K, I, II, IT, NDIM, IER, IVA1, IVA2
      INTEGER      VALI(2)
      INTEGER      IDDEPD, IDEPMO, IAPRNO, JIFL, JFFL, IDIM, NEC
      INTEGER      IVAL, INO, INO1, INO2, IDVALE, IDVITE, IDACCE
      INTEGER      IZONE, IPNOEU, IDNOEU
      INTEGER      K1, K2, K3, I8, I11, I14
      INTEGER      I7
      INTEGER      I17, I18, I20, I21
      INTEGER      IARCH, IPLAQ, IFMEC, IFTG, ICDG, IIMPF, IIMPN
      INTEGER      IMIL, DISTND
      REAL*8       FPMEC, FFMEC, FFPLAQ, FPTG1, FFTG1, FPTG2, FFTG2
      REAL*8       VALR(4)
      REAL*8       FRTG2, ZERO, Z, DZ, D2Z, MA, G, VDIR(3)
      REAL*8       XA12, XA1, XA2, COTE, Z1, Z2, AA12, AA1, AA2
      REAL*8       LDOME, LGDC, HGC, LCHUT, ZMAX, Z0, ZM1
      REAL*8       ZINTER,L1,ZINTE1,ZMAXM1
      REAL*8       FARCHI, FPLAQ, FMEC, FTG, FFTG
      REAL*8       DIST(100), FF, R8VIDE, VDGC(3)
      LOGICAL      LDIGC
      CHARACTER*8  K8BID, NOMGD
      CHARACTER*24 NOLILI, CHGRF2
      CHARACTER*24 NDYNKK,CHGRFL
      REAL*8       PCMAX, PRESCR,P0
      INTEGER      I23
C      
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- INIT
C
      ZERO   = 0.D0
      NOMGD  = 'DEPL_R'
      LDIGC  = .FALSE.
      CHGRFL = NDYNKK(SDDYNA,'CHGRFL')             
C
      CALL DISMOI('F','NB_EC',NOMGD,'GRANDEUR',NEC,K8BID,IER)

      CALL JEVEUO ( '&&GFLECT.INDICE', 'E', JIFL )
      II    = 5 + ZI(JIFL-1+5)
      I7    = ZI(JIFL-1+II+7)
      I8    = ZI(JIFL-1+II+8)
      I11   = ZI(JIFL-1+II+11)
      I14   = ZI(JIFL-1+II+14)
      I17   = ZI(JIFL-1+II+17)
      I18   = ZI(JIFL-1+II+18)
      I20   = ZI(JIFL-1+II+20)
      I21   = ZI(JIFL-1+II+21)
      IARCH = ZI(JIFL-1+II+23)
      IPLAQ = ZI(JIFL-1+II+24)
      IFMEC = ZI(JIFL-1+II+25)
      IFTG  = ZI(JIFL-1+II+26)
      IMIL  = ZI(JIFL-1+II+27)
      ICDG  = ZI(JIFL-1+II+28)
      IIMPF = ZI(JIFL-1+II+29)
      IIMPN = ZI(JIFL-1+II+30)
C
C --- RECUPERATION DU NOMBRE DE NOEUDS MODELISANT LA GRAPPE :
C     -----------------------------------------------------
      NBNO = ZI(JIFL-1+5)

C --- COPIE DE LA SD DE FORCE FLUIDE DANS UNE SD DE TRAVAIL
C     -----------------------------------------------------
      CHGRF2 = '&&OP0070.GRAPPE_FLUIDE_C'
      CALL GFCOPY(CHGRFL,CHGRF2)
      CALL JEVEUO ( CHGRF2, 'E', JFFL )


C     SI ZMAXM1=666.666 C'EST QU'ON A DEPASSE LA BUTEE 
C          => COURT-CIRCUIT DE GRAPPE FLUIDE
      IF (ZR(JFFL-1+I11+4).EQ.666.666D0) THEN
         GOTO 9999
       ENDIF

C
C --- RECUPERATION DU VECTEUR UNITAIRE ORIENTANT LE CRAYON :
C     ----------------------------------------------------
      VDIR(1) = ZR(JFFL-1+I18+3*(NBNO-1)+1)
      VDIR(2) = ZR(JFFL-1+I18+3*(NBNO-1)+2)
      VDIR(3) = ZR(JFFL-1+I18+3*(NBNO-1)+3)
      VDGC(1) = ZR(JFFL-1+I18+3*NBNO+1)
      VDGC(2) = ZR(JFFL-1+I18+3*NBNO+2)
      VDGC(3) = ZR(JFFL-1+I18+3*NBNO+3)
      IF ( VDGC(1) .NE. R8VIDE() ) LDIGC = .TRUE.
C
C --- RECUPERATION DE L'ADRESSAGE DES CHAMPS :
C ---   .PRNO ASSOCIE AU MAILLAGE :
C       -------------------------
      CALL JELIRA(NUMEDD(1:14)//'.NUME.PRNO','NMAXOC',NLILI,K8BID)
C
      K = 0
      DO 10 I = 1, NLILI
        CALL JENUNO(JEXNUM(NUMEDD(1:14)//'.NUME.LILI',I),NOLILI)
        IF (NOLILI(1:8).NE.'&MAILLA ') GOTO 10
        K = I
  10  CONTINUE
      IF (K.EQ.0) THEN
        CALL U2MESG('F','GRAPPEFLUIDE_8',0,' ',0,0,1,Z)
      ENDIF
      CALL JEVEUO(JEXNUM(NUMEDD(1:14)//'.NUME.PRNO',K),'L',IAPRNO)
C
C --- RECUPERATION DE L'INCREMENT DU CHAMP DE DEPLACEMENT :
C     ---------------------------------------------------
      CALL JEVEUO(DEPDEL(1:19)//'.VALE','L',IDDEPD)
C
C --- RECUPERATION DU CHAMP DE DEPLACEMENT A L'INSTANT PRECEDENT :
C     ----------------------------------------------------------
      CALL JEVEUO(DEPMOI(1:19)//'.VALE','L',IDEPMO)
C
C --- RECUPERATION DU CHAMP DE VITESSE :
C     --------------------------------
      CALL JEVEUO(VITESS(1:19)//'.VALE','L',IDVITE)
C
C --- RECUPERATION DU CHAMP D'ACCELERATION :
C     ------------------------------------
      CALL JEVEUO(ACCELE(1:19)//'.VALE','L',IDACCE)
C
C --- DIMENSION DU PROBLEME :
C     ---------------------
      NDIM = 3
C
C --- REACTUALISATION DES ABSCISSES CURVILIGNES :
C     -----------------------------------------
      Z   = ZERO
      DZ  = ZERO
      D2Z = ZERO
      DO 20  INO = 1, NBNO
         ZR(JFFL-1+I21+INO) = 0.0D0
         IVAL = ZI(IAPRNO+(ZI(JIFL-1+5+INO)-1)*(NEC+2)+1-1) - 1
         DO 30 IDIM = 1, NDIM
           ZR(JFFL-1+I21+INO) = ZR(JFFL-1+I21+INO) +
     &     (ZR(IDDEPD+IVAL+IDIM-1) + ZR(IDEPMO+IVAL+IDIM-1))*VDIR(IDIM)
            DZ  = DZ  + ZR(IDVITE+IVAL+IDIM-1)*VDIR(IDIM)
            D2Z = D2Z + ZR(IDACCE+IVAL+IDIM-1)*VDIR(IDIM)
  30     CONTINUE
         ZR(JFFL-1+I21+INO) = ZR(JFFL-1+I21+INO) + ZR(JFFL-1+I20+INO)
         Z  = Z + ( ZR(JFFL-1+I21+INO) - ZR(JFFL-1+I20+INO) )
  20  CONTINUE
C
      Z   = Z   / NBNO
      DZ  = DZ  / NBNO
      D2Z = D2Z / NBNO
C
C --- ON VERIFIE SI L'ON ARRIVE EN FIN DE CHUTE
C ET ON CALCULE T6
C     -----------------------------------------
      LCHUT = ZR(JFFL-1+I8+4)
      Z0    = ZR(JFFL-1+I11+4)
      ZM1   = ZR(JFFL-1+I14+1)
      ZMAX  = LCHUT - Z0 - Z
      ZMAXM1  = LCHUT - Z0 - ZM1
      IF ( ZMAX .LE. 0.D0 .AND. ZMAXM1.GT.0.D0) THEN
         IT = ZI(JIFL-1+3)
         VALI (1) = IT-1
         VALI (2) = IT
         VALR (1) = ZM1
         VALR (2) = Z
         VALR (3) = ( IT - 1 ) * DT
         VALR (4) = IT * DT
C MESSAGE T6
         CALL U2MESG('I', 'GRAPPEFLUIDE_11',0,' ',2,VALI,4,VALR)
C ON MET Z0 A 666.666 POUR INDIQUER QU'ON A DEPASSE LA BUTEE
C ET NE PLUS PASSER DANS LES ROUTINES DE GRAPPE FLUIDE
         Z0 = 666.666D0
         ZR(JFFL-1+I11+4) = Z0
C ON NE CHANGE RIEN AU VECTEUR DES FORCES EXTERIEURES ET ON VA
C DIRECTEMENT A LA FIN
         GOTO 9999

      ENDIF
C
C ON CALCULE T5 ENTREE DANS LE RETREINT
C ------------------------------------
      L1 = ZR(JFFL-1+I7+14)
      ZINTER = L1 - Z0 - Z
      ZINTE1 = L1 - Z0 - ZM1
      IF ( ZINTER .LE. 0.D0 .AND. ZINTE1.GT.0.D0) THEN
         IT = ZI(JIFL-1+3)
         VALI (1) = IT-1
         VALI (2) = IT
         VALR (1) = ZM1
         VALR (2) = Z
         VALR (3) = ( IT - 1 ) * DT
         VALR (4) = IT * DT
         CALL U2MESG('I', 'GRAPPEFLUIDE_14',0,' ',2,VALI,4,VALR)
      ENDIF
C ON TESTE SI LA GRAPPE EST  BLOQUEE
C -------------------------------------------
         IT = ZI(JIFL-1+3)
      IF (IT.GT.2.D0.AND.DZ .LE. 0.D0 ) THEN
         VALI (1) = IT-1
         VALI (2) = IT
         VALR (1) = ZM1
         VALR (2) = Z
         VALR (3) = ( IT - 1 ) * DT
         VALR (4) = IT * DT
         CALL U2MESG('A', 'GRAPPEFLUIDE_15',0,' ',2,VALI,4,VALR)
C COMMENTAIRE EL  RAJOUTER UN ARRET  DU CALCUL AUTOMATIQUE       
         GOTO 9999
      ENDIF

C --- POUR LES ABSCISSES CURVILIGNES, ON PREND L'ORIGINE AU SOMMET
C --- DU COEUR.
C --- Z1 VA ETRE L'ABSCISSE CURVILIGNE DU SOMMET DU MECANISME DE
C --- COMMANDE
C --- Z2 VA ETRE L'ABSCISSE CURVILIGNE DU BAS DU MECANISME DE COMMANDE
C --- Z3 VA ETRE L'ABSCISSE CURVILIGNE DU SOMMET DU GUIDAGE CONTINU
C --- L1 EST LA LONGUEUR DU TUBE GUIDE HORS DASHPOT
C --- L2 EST LA LONGUEUR DES RETREINTS :
C     --------------------------------
      LDOME = ZR(JFFL-1+I17+4)
      LGDC  = ZR(JFFL-1+I17+5)
      HGC   = ZR(JFFL-1+I17+6)
CCC      L1    = ZR(JFFL-1+I7+14)
C
      Z1 = -LDOME -LGDC -HGC
      Z2 =              -HGC
C
C --- DETERMINATION DU NOMBRE DE NOEUDS ET DES NOEUDS SE TROUVANT
C --- DANS LES 3 ZONES DECRITES CI-DESSUS OU SE TROUVENT LES FORCES
C --- S'APPLIQUANT SUR LA GRAPPE :
C     --------------------------
      K1 = 0
      K2 = 0
      K3 = 0
C
      DO 50 INO = 1, NBNO
C
        COTE = ZR(JFFL-1+I21+INO)
C
C ---   ZONE 1 (MECANISME DE COMMANDE) :
C       ------------------------------
        IF ( COTE.LE.Z1 ) THEN
          K1 = K1 + 1
C
C ---   ZONE 2 (GUIDAGE CONTINU) :
C       ------------------------
        ELSEIF ( COTE.GE.Z2 .AND. COTE.LE.ZERO ) THEN
          K2 = K2 + 1
C
C ---   ZONE 3 (TUBE GUIDE, DASHPOT, RETREINT) :
C       ---------------------------------
        ELSEIF ( COTE.GT.ZERO ) THEN
          K3 = K3 + 1
C
        ENDIF
C
  50  CONTINUE
C
C --- CALCUL DES FORCES FLUIDES S'EXERCANT SUR LES DIFFERENTES PARTIES
C --- DE LA GRAPPE :
C     ------------
      CALL GFFORC ( CHGRF2, Z, DZ, D2Z, DT, FPMEC,
     &              FFMEC, FFPLAQ, FPTG1, FFTG1, FPTG2, FFTG2, FRTG2,
     &              PRESCR,P0)


C CALCUL DE LA PRESSION MAXIMALE DANS LE RETREINT
      I23 = ZI(JIFL-1+II+33)
      PCMAX = ZR(JFFL-1+I23)
      IF (PRESCR.GT.PCMAX) THEN
        PCMAX = PRESCR
      ENDIF
      ZR(JFFL-1+I23) = PCMAX

      IF ( ZMAX .LE. 0.D0 .AND. ZMAXM1.GT.0.D0) THEN
         IT = ZI(JIFL-1+3)
         VALI (1) = IT-1
         VALI (2) = IT
         VALR (1) = PCMAX
         VALR (2) = P0
         VALR (3) = ( IT - 1 ) * DT
         VALR (4) = PCMAX - P0
         CALL U2MESG('I', 'GRAPPEFLUIDE_16',0,' ',2,VALI,4,VALR)
      ENDIF
C
C
C --- AFFECTATION DU VECTEUR DE CHARGEMENT :
C     ====================================
      CALL JEVEUO (CNGRFL(1:19)//'.VALE','E',IDVALE)
C
      MA = ZR(JFFL-1+I8+2)
      G  = ZR(JFFL-1+I8+3)
C
C      FTOT = -MA*G - 24*(FPTG1+FFTG1) - (FPMEC+FFMEC) - FFPLAQ
C     +             - 24*(FPTG2+FFTG2+FRTG2)
C
      IF ( IIMPF .GT. 0 ) THEN
         IT = ZI(JIFL-1+3)
         IF ( IT .EQ. 1 ) THEN
            WRITE(IIMPF,1000)
            WRITE(IIMPF,1002)
         ENDIF
         WRITE(IIMPF,1100) IT, Z, DZ, D2Z, MA, FFPLAQ, FPMEC, FFMEC,
     &               FPTG1, FFTG1, FPTG2, FFTG2, FRTG2
     &               , PRESCR,P0, PRESCR-P0
      ENDIF
C
C     =================================================================
C     -------------- APPLICATION DE LA FORCE D'ARCHIMEDE --------------
C
C     FORCE REPARTIE SUR TOUTE LA GRAPPE
C     ----------------------------------
      IF ( IARCH .EQ. 1 ) THEN
         XA1  = ZR(JFFL-1+I20+1)
         XA2  = ZR(JFFL-1+I20+NBNO)
         XA12 = ABS ( XA2 - XA1 )
         FARCHI = MA*G / XA12
         DO 100  I = 1, NBNO-1
            INO1  = ZI(JIFL-1+5+I)
            INO2  = ZI(JIFL-1+5+I+1)
            IVA1  = ZI(IAPRNO+(INO1-1)*(NEC+2)+1-1) - 1
            IVA2  = ZI(IAPRNO+(INO2-1)*(NEC+2)+1-1) - 1
            XA1 = ZR(JFFL-1+I20+I)
            XA2 = ZR(JFFL-1+I20+I+1)
            XA12 = ABS ( XA2 - XA1 )
            DO 102 IDIM = 1, NDIM
               FF = (FARCHI*XA12/2)*ZR(JFFL-1+I18+3*(I-1)+IDIM)
               ZR(IDVALE+IVA1+IDIM-1) = ZR(IDVALE+IVA1+IDIM-1) - FF
               ZR(IDVALE+IVA2+IDIM-1) = ZR(IDVALE+IVA2+IDIM-1) - FF
 102        CONTINUE
 100     CONTINUE
C
C     FORCE AU CENTRE DE GRAVITE
C     --------------------------
      ELSEIF (IARCH .EQ. 2 ) THEN
         INO = ZI(IAPRNO+(ICDG - 1) * (NEC + 2) + 1 - 1) - 1
         DO 110 IDIM = 1, NDIM
            FF = MA*G*VDIR(IDIM)
            ZR(IDVALE+INO+IDIM-1) = ZR(IDVALE+INO+IDIM-1) - FF
 110    CONTINUE
      ENDIF
C
C     =================================================================
C     - APPLICATION DE LA FORCE DE PLAQUAGE AU NIVEAU GUIDAGE CONTINU -
C
C     FORCE REPARTIE SUR TOUTE LA GRAPPE
C     ----------------------------------
      IF ( IPLAQ .EQ. 1 ) THEN
         XA1  = ZR(JFFL-1+I20+1)
         XA2  = ZR(JFFL-1+I20+NBNO)
         XA12 = ABS ( XA2 - XA1 )
         FPLAQ = FFPLAQ / XA12
         DO 200  I = 1, NBNO-1
            INO1  = ZI(JIFL-1+5+I)
            INO2  = ZI(JIFL-1+5+I+1)
            IVA1  = ZI(IAPRNO+(INO1-1)*(NEC+2)+1-1) - 1
            IVA2  = ZI(IAPRNO+(INO2-1)*(NEC+2)+1-1) - 1
            XA1 = ZR(JFFL-1+I20+I)
            XA2 = ZR(JFFL-1+I20+I+1)
            XA12 = ABS ( XA2 - XA1 )
            DO 202 IDIM = 1, NDIM
               IF ( LDIGC ) THEN
                  FF = (FPLAQ*XA12/2)*VDGC(IDIM)
               ELSE
                  FF = (FPLAQ*XA12/2)*ZR(JFFL-1+I18+3*(I-1)+IDIM)
               ENDIF
               ZR(IDVALE+IVA1+IDIM-1) = ZR(IDVALE+IVA1+IDIM-1) - FF
               ZR(IDVALE+IVA2+IDIM-1) = ZR(IDVALE+IVA2+IDIM-1) - FF
 202        CONTINUE
 200     CONTINUE
C
C     FORCE AU CENTRE DE GRAVITE
C     --------------------------
      ELSEIF ( IPLAQ .EQ. 2 ) THEN
         INO = ZI(IAPRNO+(ICDG - 1) * (NEC + 2) + 1 - 1) - 1
         DO 210 IDIM = 1, NDIM
            IF ( LDIGC ) THEN
               FF = FFPLAQ*VDGC(IDIM)
            ELSE
               FF = FFPLAQ*VDIR(IDIM)
            ENDIF
            ZR(IDVALE+INO+IDIM-1) = ZR(IDVALE+INO+IDIM-1) - FF
 210     CONTINUE
C
C     FORCE REPARTIE DANS LE GUIDAGE CONTINU
C     --------------------------------------
      ELSEIF ( IPLAQ .EQ. 3 ) THEN
         IF ( K2 .EQ. 0) THEN
            CALL U2MESG('F','GRAPPEFLUIDE_8',0,' ',0,0,1,Z)
         ENDIF
         IZONE = 0
         DO 220 INO = 1, NBNO
            COTE = ZR(JFFL-1+I21+INO)
            IF ( COTE.GE.Z2 .AND. COTE.LE.ZERO )  IZONE = IZONE + 1
            IF ( IZONE.EQ.1 )  IPNOEU = INO
 220     CONTINUE
         IDNOEU = IPNOEU + IZONE
         XA1  = ZR(JFFL-1+I20+IPNOEU)
         XA2  = ZR(JFFL-1+I20+IDNOEU)
         XA12 = ABS ( XA2 - XA1 )
         FPLAQ = FFPLAQ / XA12
         DO 222 I = IPNOEU, IDNOEU-1
            INO1 = ZI(JIFL-1+5+I)
            INO2 = ZI(JIFL-1+5+I+1)
            IVA1 = ZI(IAPRNO+(INO1-1)*(NEC+2)+1-1) - 1
            IVA2 = ZI(IAPRNO+(INO2-1)*(NEC+2)+1-1) - 1
            XA1 = ZR(JFFL-1+I20+I)
            XA2 = ZR(JFFL-1+I20+I+1)
            XA12 = ABS ( XA2 - XA1 )
            DO 224 IDIM = 1, NDIM
               IF ( LDIGC ) THEN
                  FF = FFPLAQ*VDGC(IDIM)
               ELSE
                  FF = FFPLAQ*ZR(JFFL-1+I18+3*(I-1)+IDIM)
               ENDIF
               ZR(IDVALE+IVA1+IDIM-1) = ZR(IDVALE+IVA1+IDIM-1) - FF
               ZR(IDVALE+IVA2+IDIM-1) = ZR(IDVALE+IVA2+IDIM-1) - FF
 224        CONTINUE
 222     CONTINUE
C
C     FORCE APPLIQUEE AU MILIEU DU GUIDAGE CONTINU
C     --------------------------------------------
      ELSEIF ( IPLAQ .EQ. 4 ) THEN
         IF ( K2 .EQ. 0) THEN
            CALL U2MESG('F','GRAPPEFLUIDE_8',0,' ',0,0,1,Z)
         ENDIF
         IZONE = 0
         DO 230 INO = 1, NBNO
            COTE = ZR(JFFL-1+I21+INO)
            IF ( COTE.GE.Z2 .AND. COTE.LE.ZERO )  IZONE = IZONE + 1
            IF ( IZONE.EQ.1 )  IPNOEU = INO
 230     CONTINUE
         IDNOEU = IPNOEU + IZONE
         XA1  = ZR(JFFL-1+I20+IPNOEU)
         XA2  = ZR(JFFL-1+I20+IDNOEU)
         XA12 = ABS ( XA2 - XA1 )
         XA1  = XA1 + ( XA12 / 2 )
         DO 232 I = IPNOEU, IDNOEU-1
            XA2 = ZR(JFFL-1+I20+I+1)
            IF ( XA2 .GT. XA1 ) GOTO 234
 232     CONTINUE
 234     CONTINUE
         IMIL = ZI(JIFL-1+5+I-1)
         INO = ZI(IAPRNO+(IMIL-1)*(NEC+2)+1-1) - 1
         DO 236 IDIM = 1, NDIM
            IF ( LDIGC ) THEN
               FF = FFPLAQ*VDGC(IDIM)
            ELSE
               FF = FFPLAQ*VDIR(IDIM)
            ENDIF
            ZR(IDVALE+INO+IDIM-1) = ZR(IDVALE+INO+IDIM-1) - FF
 236     CONTINUE
         ZI(JIFL-1+II+27) = IMIL
C
C     FORCE DISTRIBUEE AU NIVEAU DU GUIDAGE CONTINU
C     ---------------------------------------------
      ELSEIF ( IPLAQ .EQ. 5 ) THEN
         IF ( K2 .EQ. 0) THEN
            CALL U2MESG('F','GRAPPEFLUIDE_8',0,' ',0,0,1,Z)
         ENDIF
         IZONE = 0
         DO 240 INO = 1, NBNO
            COTE = ZR(JFFL-1+I21+INO)
            IF ( COTE.GE.Z2 .AND. COTE.LE.ZERO )  IZONE = IZONE + 1
            IF ( IZONE.EQ.1 )  IPNOEU = INO
 240     CONTINUE
         CALL DISTRI ( IZONE, DIST )
         IDNOEU = IPNOEU + IZONE
         DISTND = 1
         DO 242 I = IPNOEU, IDNOEU
            INO = ZI(IAPRNO+(I - 1) * (NEC + 2) + 1 - 1) - 1
            DO 244 IDIM = 1, NDIM
               IF ( LDIGC ) THEN
                  FF = FFPLAQ*DIST(DISTND)*VDGC(IDIM)
               ELSE
                  FF = FFPLAQ*DIST(DISTND)*ZR(JFFL-1+I18+3*(I-1)+IDIM)
               ENDIF
               ZR(IDVALE+INO+IDIM-1) = ZR(IDVALE+INO+IDIM-1) - FF
 244        CONTINUE
            DISTND = DISTND + 1
 242     CONTINUE
      ENDIF
C
C     =================================================================
C     ---- APPLICATION DE LA FORCE AN NIVEAU DU MECANISME DE LEVEE ----
C
C     FORCE REPARTIE SUR TOUTE LA GRAPPE
C     ----------------------------------
      IF ( IFMEC .EQ. 1 ) THEN
         XA1  = ZR(JFFL-1+I20+1)
         XA2  = ZR(JFFL-1+I20+NBNO)
         XA12 = ABS ( XA2 - XA1 )
         FMEC = ( FPMEC + FFMEC )  / XA12
         DO 300  I = 1, NBNO-1
            INO1  = ZI(JIFL-1+5+I)
            INO2  = ZI(JIFL-1+5+I+1)
            IVA1  = ZI(IAPRNO+(INO1-1)*(NEC+2)+1-1) - 1
            IVA2  = ZI(IAPRNO+(INO2-1)*(NEC+2)+1-1) - 1
            XA1 = ZR(JFFL-1+I20+I)
            XA2 = ZR(JFFL-1+I20+I+1)
            XA12 = ABS ( XA2 - XA1 )
            DO 302 IDIM = 1, NDIM
               FF = (FMEC*XA12/2)*ZR(JFFL-1+I18+3*(I-1)+IDIM)
               ZR(IDVALE+IVA1+IDIM-1) = ZR(IDVALE+IVA1+IDIM-1) - FF
               ZR(IDVALE+IVA2+IDIM-1) = ZR(IDVALE+IVA2+IDIM-1) - FF
 302        CONTINUE
 300     CONTINUE
C
C     FORCE AU CENTRE DE GRAVITE
C     --------------------------
      ELSEIF ( IFMEC .EQ. 2 ) THEN
         INO = ZI(IAPRNO+(ICDG - 1) * (NEC + 2) + 1 - 1) - 1
         DO 310 IDIM = 1, NDIM
            FF = ( FPMEC + FFMEC )*VDIR(IDIM)
            ZR(IDVALE+INO+IDIM-1) = ZR(IDVALE+INO+IDIM-1) - FF
 310     CONTINUE
C
C     FORCE REPARTIE SUR LA ZONE DU MECANISME DE LEVEE
C     ------------------------------------------------
      ELSEIF ( IFMEC .EQ. 3 ) THEN
         IF ( K1 .EQ. 0) THEN
            CALL U2MESG('F','GRAPPEFLUIDE_9',0,' ',0,0,1,Z)
         ENDIF
         IZONE = 0
         DO 320 INO = 1, NBNO
            COTE = ZR(JFFL-1+I21+INO)
            IF ( COTE.LE.Z1 )  IZONE = IZONE + 1
            IF ( IZONE.EQ.1)  IPNOEU = INO
 320     CONTINUE
         IDNOEU = IPNOEU + IZONE
         XA1  = ZR(JFFL-1+I20+IPNOEU)
         XA2  = ZR(JFFL-1+I20+IDNOEU)
         XA12 = ABS ( XA2 - XA1 )
         FMEC = ( FPMEC + FFMEC )  / XA12
         DO 322 I = IPNOEU, IDNOEU-1
            INO1 = ZI(JIFL-1+5+I)
            INO2 = ZI(JIFL-1+5+I+1)
            IVA1  = ZI(IAPRNO+(INO1-1)*(NEC+2)+1-1) - 1
            IVA2  = ZI(IAPRNO+(INO2-1)*(NEC+2)+1-1) - 1
            XA1 = ZR(JFFL-1+I20+I)
            XA2 = ZR(JFFL-1+I20+I+1)
            XA12 = ABS ( XA2 - XA1 )
            DO 324 IDIM = 1, NDIM
               FF = (FMEC*XA12/2)*ZR(JFFL-1+I18+3*(I-1)+IDIM)
               ZR(IDVALE+IVA1+IDIM-1) = ZR(IDVALE+IVA1+IDIM-1) - FF
               ZR(IDVALE+IVA2+IDIM-1) = ZR(IDVALE+IVA2+IDIM-1) - FF
 324        CONTINUE
 322     CONTINUE
C
C     FORCE PONCTUELLE A L'EXTREMITE DU TUBE
C     FORCE REPARTIE SUR LA ZONE DU MECANISME DE LEVEE
C     ------------------------------------------------
      ELSEIF ( IFMEC .EQ. 4 ) THEN
         INO = ZI(IAPRNO+(1 - 1) * (NEC + 2) + 1 - 1) - 1
         DO 330 IDIM = 1, NDIM
            FF = FPMEC*ZR(JFFL-1+I18+IDIM)
            ZR(IDVALE+INO+IDIM-1) = ZR(IDVALE+INO+IDIM-1) - FF
 330     CONTINUE
C
         IF ( K1 .EQ. 0) THEN
            CALL U2MESG('F','GRAPPEFLUIDE_9',0,' ',0,0,1,Z)
         ENDIF
         IZONE = 0
         DO 332 INO = 1, NBNO
            COTE = ZR(JFFL-1+I21+INO)
            IF ( COTE.LE.Z1 )  IZONE = IZONE + 1
            IF ( IZONE.EQ.1)  IPNOEU = INO
 332     CONTINUE
         IDNOEU = IPNOEU + IZONE
         AA1  = ZR(JFFL-1+I20+IPNOEU)
         AA2  = ZR(JFFL-1+I20+IDNOEU)
         AA12 = ABS ( AA2 - AA1 )
         FMEC = FFMEC / AA12
         DO 334 I = IPNOEU, IDNOEU-1
            INO1 = ZI(JIFL-1+5+I)
            INO2 = ZI(JIFL-1+5+I+1)
            IVA1  = ZI(IAPRNO+(INO1-1)*(NEC+2)+1-1) - 1
            IVA2  = ZI(IAPRNO+(INO2-1)*(NEC+2)+1-1) - 1
            AA1 = ZR(JFFL-1+I20+I)
            AA2 = ZR(JFFL-1+I20+I+1)
            AA12 = ABS ( AA2 - AA1 )
            DO 336 IDIM = 1, NDIM
               FF = (FMEC*AA12/2)*ZR(JFFL-1+I18+3*(I-1)+IDIM)
               ZR(IDVALE+IVA1+IDIM-1) = ZR(IDVALE+IVA1+IDIM-1) - FF
               ZR(IDVALE+IVA2+IDIM-1) = ZR(IDVALE+IVA2+IDIM-1) - FF
 336        CONTINUE
 334     CONTINUE
      ENDIF
C
C     =================================================================
C     ------- APPLICATION DE LA FORCE AVANT ET APRES LE RETREINT ------
C
C     FORCE REPARTIE SUR TOUTE LA GRAPPE
C     ----------------------------------
      IF ( IFTG .EQ. 1 ) THEN
         XA1  = ZR(JFFL-1+I20+1)
         XA2  = ZR(JFFL-1+I20+NBNO)
         XA12 = ABS ( XA2 - XA1 )
         FFTG = FPTG1 + FFTG1 + FPTG2 + FFTG2 +FRTG2
         FTG = 24 * FFTG  / XA12
         DO 400  I = 1, NBNO-1
            INO1  = ZI(JIFL-1+5+I)
            INO2  = ZI(JIFL-1+5+I+1)
            IVA1  = ZI(IAPRNO+(INO1-1)*(NEC+2)+1-1) - 1
            IVA2  = ZI(IAPRNO+(INO2-1)*(NEC+2)+1-1) - 1
            XA1 = ZR(JFFL-1+I20+I)
            XA2 = ZR(JFFL-1+I20+I+1)
            XA12 = ABS ( XA2 - XA1 )
            DO 402 IDIM = 1, NDIM
               FF = (FTG*XA12/2)*ZR(JFFL-1+I18+3*(I-1)+IDIM)
               ZR(IDVALE+IVA1+IDIM-1) = ZR(IDVALE+IVA1+IDIM-1) - FF
               ZR(IDVALE+IVA2+IDIM-1) = ZR(IDVALE+IVA2+IDIM-1) - FF
 402        CONTINUE
 400     CONTINUE
C
C     FORCE AU CENTRE DE GRAVITE
C     --------------------------
      ELSEIF ( IFTG .EQ. 2 ) THEN
         FFTG = 24 * ( FPTG1 + FFTG1 + FPTG2 + FFTG2 + FRTG2 )
         INO = ZI(IAPRNO+(ICDG - 1) * (NEC + 2) + 1 - 1) - 1
         DO 410 IDIM = 1, NDIM
            FF = FFTG * VDIR(IDIM)
            ZR(IDVALE+INO+IDIM-1) = ZR(IDVALE+INO+IDIM-1) - FF
 410     CONTINUE
C
C     FORCE REPARTIE SUR LA ZONE AVANT ET APRES LE RETREINT
C     -----------------------------------------------------
      ELSEIF ( IFTG .EQ. 3 ) THEN
         IF ( K3 .EQ. 0) THEN
            CALL U2MESG('F','GRAPPEFLUIDE_10',0,' ',0,0,1,Z)
         ENDIF
         IZONE = 0
         DO 420 INO = 1, NBNO
            COTE = ZR(JFFL-1+I21+INO)
            IF ( COTE.GT.ZERO ) IZONE = IZONE + 1
            IF ( IZONE.EQ.1 )  IPNOEU = INO
 420     CONTINUE
         IDNOEU = IPNOEU + IZONE
         XA1  = ZR(JFFL-1+I20+IPNOEU)
         XA2  = ZR(JFFL-1+I20+IDNOEU)
         XA12 = ABS ( XA2 - XA1 )
         FTG = 24 * ( FPTG1 + FFTG1 + FPTG2 + FFTG2 + FRTG2 ) / XA12
         DO 422 I = IPNOEU, IDNOEU-1
            INO1 = ZI(JIFL-1+5+I)
            INO2 = ZI(JIFL-1+5+I+1)
            IVA1  = ZI(IAPRNO+(INO1-1)*(NEC+2)+1-1) - 1
            IVA2  = ZI(IAPRNO+(INO2-1)*(NEC+2)+1-1) - 1
            XA1 = ZR(JFFL-1+I20+I)
            XA2 = ZR(JFFL-1+I20+I+1)
            XA12 = ABS ( XA2 - XA1 )
            DO 424 IDIM = 1, NDIM
               FF = (FTG*XA12/2)*ZR(JFFL-1+I18+3*(I-1)+IDIM)
               ZR(IDVALE+IVA1+IDIM-1) = ZR(IDVALE+IVA1+IDIM-1) - FF
               ZR(IDVALE+IVA2+IDIM-1) = ZR(IDVALE+IVA2+IDIM-1) - FF
 424        CONTINUE
 422     CONTINUE
C
C     FORCE PONCTUELLE A L'EXTREMITE DU CRAYON
C     FORCE REPARTIE SUR LA ZONE AVANT ET APRES LE RETREINT
C     -----------------------------------------------------
      ELSEIF ( IFTG .EQ. 4 ) THEN
C
         INO = ZI(IAPRNO+(NBNO - 1) * (NEC + 2) + 1 - 1) - 1
         DO 430 IDIM = 1, NDIM
            FF = 24 * ( FPTG1 + FPTG2 )*ZR(JFFL-1+I18+3*(NBNO-1)+IDIM)
            ZR(IDVALE+INO+IDIM-1) = ZR(IDVALE+INO+IDIM-1) - FF
 430     CONTINUE
C
         IF ( K3 .EQ. 0) THEN
            CALL U2MESG('F','GRAPPEFLUIDE_10',0,' ',0,0,1,Z)
         ENDIF
         IZONE = 0
         DO 432 INO = 1, NBNO
            COTE = ZR(JFFL-1+I21+INO)
            IF ( COTE.GT.ZERO )  IZONE = IZONE + 1
            IF ( IZONE.EQ.1 )   IPNOEU = INO
 432     CONTINUE
         IDNOEU = IPNOEU + IZONE
         XA1  = ZR(JFFL-1+I20+IPNOEU)
         XA2  = ZR(JFFL-1+I20+IDNOEU)
         XA12 = ABS ( XA2 - XA1 )
         FTG = 24 * ( FFTG1 + FFTG2 + FRTG2 ) / XA12
         DO 434 I = IPNOEU, IDNOEU-1
            INO1 = ZI(JIFL-1+5+I)
            INO2 = ZI(JIFL-1+5+I+1)
            IVA1 = ZI(IAPRNO+(INO1-1)*(NEC+2)+1-1) - 1
            IVA2 = ZI(IAPRNO+(INO2-1)*(NEC+2)+1-1) - 1
            XA1  = ZR(JFFL-1+I20+I)
            XA2  = ZR(JFFL-1+I20+I+1)
            XA12 = ABS ( XA2 - XA1 )
            DO 436 IDIM = 1, NDIM
               FF = (FTG*XA12/2)*ZR(JFFL-1+I18+3*(I-1)+IDIM)
               ZR(IDVALE+IVA1+IDIM-1) = ZR(IDVALE+IVA1+IDIM-1) - FF
               ZR(IDVALE+IVA2+IDIM-1) = ZR(IDVALE+IVA2+IDIM-1) - FF
 436        CONTINUE
 434        CONTINUE
      ENDIF
C
      IF ( IIMPN .GT. 0 ) THEN
         IT = ZI(JIFL-1+3)
         IF ( IT .EQ. 1 ) THEN
            WRITE(IIMPN,1010)
            WRITE(IIMPN,1012)
         ENDIF
         WRITE(IIMPN,1110) IT, Z, K1, K2, K3
      ENDIF
C
 9999 CONTINUE
C
      CALL JEDEMA()
C
 1000 FORMAT('  IT       Z             DZ            D2Z          ',
     &       ' MA         PLAQUAGE     -----FORCE MECANISME-----   ',
     &       '--------TUBE GUIDE-------   ',
     &       '----------------DASHPOT----------------','PRESSION')
 1002 FORMAT(78X,'PRESSION      VISQUEUX      PRESSION      VISQUEUX',
     &           '      PRESSION      VISQUEUX      RETREINT')
 1100 FORMAT(1P,I4,15(2X,E12.5))
 1010 FORMAT('NOMBRE DE NOEUDS DANS LES DIFFERENTES ZONES')
 1012 FORMAT('  IT       Z        MECANISME_COMMANDE  GUIDAGE_CONTINU',
     &       '   TUBE_GUIDE/DASHPOT/RETREINT')
 1110 FORMAT(1P,I4,2X,E12.5,6X,I10,8X,I10,10X,I10)
C
C.============================ FIN DE LA ROUTINE ======================
      END
