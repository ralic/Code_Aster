      SUBROUTINE NMGRFL ( NUMEDD, CHGRFL, DEPMOI, DEPDEL, VITPLU,
     +                    ACCPLU, DT, CNFEDO)
      IMPLICIT NONE
      REAL*8         DT
      CHARACTER*24   NUMEDD, CHGRFL , DEPMOI, DEPDEL, VITPLU, ACCPLU
      CHARACTER*24   CNFEDO
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 29/10/2003   AUTEUR BOYERE E.BOYERE 
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
C
C BUT : CALCUL DES FORCES FLUIDES NON LINEAIRES S'EXERCANT SUR LES
C       GRAPPES LORS DE LA CHUTE DE CELLES-CI 
C
C   ARGUMENT        E/S  TYPE         ROLE
C    NUMEDD         IN    K24       NUME_DDL
C    CHARGE         IN    K24       CHARGEMENT CONTENANT LES DONNEES 
C                                   POUR LE CALCUL DES FORCES FLUIDES
C                                   S'EXERCANT SUR LA GRAPPE
C    DEPMOI         IN    K24       DEPLACEMENT A L'INSTANT PRECEDENT
C    DEPDEL         IN    K24       INCREMENT DE DEPLACEMENT 
C    VITPLU         IN    K24       VITESSES A L'INSTANT COURANT
C    ACCPLU         IN    K24       ACCELERATIONS A L'INSTANT COURANT
C    DT             IN    R         PAS DE TEMPS
C    IT             IN    I         NUMERO D'ITERATION
C    CNFEDO        VAR    K24       VECTEUR ASSEMBLE DU CHARGEMENT 
C                                   DU AUX FORCES FLUIDES
C
C.========================= DEBUT DES DECLARATIONS ====================
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
      INTEGER      IDDEPD, IDEPMO, IAPRNO, JIFL, JFFL, IDIM, NEC
      INTEGER      IVAL, INO, INO1, INO2, IDVALE, IDVITE, IDACCE
      INTEGER      K1, K2, K3, I8, I11, I14, I17, I18, I20, I21
      REAL*8       FPMEC, FFMEC, FFPLAQ, FPTG1, FFTG1, FPTG2, FFTG2
      REAL*8       FRTG2, ZERO, Z, DZ, D2Z, MA, G, FTOT, VDIR(3)
      REAL*8       XA12, XA1, XA2, COTE, Z1, Z2
      REAL*8       LDOME, LGDC, HGC, LCHUT, ZMAX, Z0, ZM1, INST
      LOGICAL      DEBUG, FAPLIQ
      CHARACTER*8  K8BID, NOMGD
      CHARACTER*24 NOLILI

C.========================= DEBUT DU CODE EXECUTABLE ==================
C
      CALL JEMARQ()
C
      ZERO   = 0.0D0
      NOMGD  = 'DEPL_R'
      DEBUG  = .FALSE.
CCC      DEBUG  = .TRUE.
C
      CALL DISMOI('F','NB_EC',NOMGD,'GRANDEUR',NEC,K8BID,IER)

      CALL JEVEUO ( '&&GFLECT.INDICE', 'E', JIFL ) 
      CALL JEVEUO ( CHGRFL, 'E', JFFL ) 
      II = 5 + ZI(JIFL-1+5)
      I8  = ZI(JIFL-1+II+8)
      I11 = ZI(JIFL-1+II+11)
      I14 = ZI(JIFL-1+II+14)
      I17 = ZI(JIFL-1+II+17)
      I18 = ZI(JIFL-1+II+18)
      I20 = ZI(JIFL-1+II+20)
      I21 = ZI(JIFL-1+II+21)
C
C --- RECUPERATION DU NOMBRE DE NOEUDS MODELISANT LA GRAPPE :
C     -----------------------------------------------------
      NBNO = ZI(JIFL-1+5)
C
C --- RECUPERATION DU VECTEUR UNITAIRE ORIENTANT LE CRAYON :
C     ----------------------------------------------------
      VDIR(1) = ZR(JFFL-1+I18+1)
      VDIR(2) = ZR(JFFL-1+I18+2)
      VDIR(3) = ZR(JFFL-1+I18+3)
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
        CALL UTMESS('F','NMGRFL','ERREUR DANS LA RECUPERATION DU '//
     +                           'NUME.PRNO .')
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
      CALL JEVEUO(VITPLU(1:19)//'.VALE','L',IDVITE)
C
C --- RECUPERATION DU CHAMP D'ACCELERATION :
C     ------------------------------------
      CALL JEVEUO(ACCPLU(1:19)//'.VALE','L',IDACCE)
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
     +     (ZR(IDDEPD+IVAL+IDIM-1) + ZR(IDEPMO+IVAL+IDIM-1))*VDIR(IDIM)
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
C --- ON VERIFIE QUE L'ON CHUTE TOUJOURS
C     ----------------------------------
      LCHUT = ZR(JFFL-1+I8+4)
      Z0    = ZR(JFFL-1+I11+4)
      ZM1   = ZR(JFFL-1+I14+1)
      ZMAX  = LCHUT - Z0 - Z
      IF ( ZMAX .LE. 0.D0 ) THEN
         IT = ZI(JIFL-1+3)
         CALL UTDEBM('A','FORCE_FLUIDE','GRAPPE BLOQUEE')
         CALL UTIMPI('L','   ITERATION ', 1, IT-1)
         CALL UTIMPR('S',', Z = ', 1, ZM1)
         CALL UTIMPI('L','   ITERATION ', 1, IT)
         CALL UTIMPR('S',', Z = ', 1, Z)
         INST = ( IT - 1 ) * DT
         CALL UTIMPR('L','   TEMPS DE CHUTE COMPRIS ENTRE ', 1, INST)
         INST = IT * DT
         CALL UTIMPR('S',' ET ', 1, INST)
         CALL UTFINM()
         GOTO 9999
      ENDIF
C
C --- CALCUL DES FORCES FLUIDES S'EXERCANT SUR LES DIFFERENTES PARTIES
C --- DE LA GRAPPE :
C     ------------
      CALL GFFORC ( CHGRFL, Z, DZ, D2Z, DT, FPMEC,
     +              FFMEC, FFPLAQ, FPTG1, FFTG1, FPTG2, FFTG2, FRTG2)
C
C --- AFFECTATION DU VECTEUR DE CHARGEMENT :
C     ====================================
      CALL JEVEUO (CNFEDO(1:19)//'.VALE','E',IDVALE)
C
      MA = ZR(JFFL-1+I8+2)
      G  = ZR(JFFL-1+I8+3)
C
      FTOT = MA*G - 24*(FPTG1+FFTG1) - (FPMEC+FFMEC) - FFPLAQ
     +            - 24*(FPTG2+FFTG2+FRTG2)
C
      IF ( DEBUG ) THEN
         IT = ZI(JIFL-1+3)
         IF ( IT .EQ. 1 ) THEN
            WRITE(38,1000)
            WRITE(38,1002)
         ENDIF
         WRITE(38,1100) IT, Z, DZ, D2Z, MA, FFPLAQ, FPMEC, FFMEC,
     +               FPTG1, FFTG1, FPTG2, FFTG2, FRTG2
      
C         WRITE(6,*)' F_FLUIDE, FTOT = ', FTOT
C         WRITE(6,*)' F_FLUIDE, MA*G = ', MA*9.81D0
C         WRITE(6,*)' F_FLUIDE, 24*(FPTG1+FFTG1) = ', 24*(FPTG1+FFTG1)
C         WRITE(6,*)' F_FLUIDE,    (FPMEC+FFMEC) = ', (FPMEC+FFMEC)
C         WRITE(6,*)' F_FLUIDE,           FFPLAQ = ', FFPLAQ
C         WRITE(6,*)' F_FLUIDE, 24*(FPTG2+FFTG2+FRTG2) = ', 
C     +                                          24*(FPTG2+FFTG2+FRTG2)
      ENDIF
C
C --- ON APPLIQUE LA FORCE EN CHAQUE NOEUD
C
CCC      FAPLIQ = .TRUE.
      FAPLIQ = .FALSE.
      IF ( FAPLIQ ) THEN
         FTOT = FTOT / NBNO
         DO 140  I = 1, NBNO
            INO1 = ZI(JIFL-1+5+I)
            IVA1 = ZI(IAPRNO+(INO1-1)*(NEC+2)+1-1) - 1
            DO 142 IDIM = 1, NDIM
               ZR(IDVALE+IVA1+IDIM-1) = ZR(IDVALE+IVA1+IDIM-1) +
     +                                                  FTOT*VDIR(IDIM)
 142        CONTINUE
 140     CONTINUE
      ELSE
         XA1  = ZR(JFFL-1+I20+1)
         XA2  = ZR(JFFL-1+I20+NBNO)
         XA12 = ABS ( XA2 - XA1 ) 
         FTOT = FTOT / XA12
         DO 240  I = 1, NBNO-1
            INO1  = ZI(JIFL-1+5+I)
            INO2  = ZI(JIFL-1+5+I+1)
            IVA1  = ZI(IAPRNO+(INO1-1)*(NEC+2)+1-1) - 1
            IVA2  = ZI(IAPRNO+(INO2-1)*(NEC+2)+1-1) - 1
            XA1 = ZR(JFFL-1+I20+I)
            XA2 = ZR(JFFL-1+I20+I+1)
            XA12 = ABS ( XA2 - XA1 ) 
            DO 242 IDIM = 1, NDIM
               ZR(IDVALE+IVA1+IDIM-1) = ZR(IDVALE+IVA1+IDIM-1) +
     +                                         (FTOT*XA12/2)*VDIR(IDIM)
               ZR(IDVALE+IVA2+IDIM-1) = ZR(IDVALE+IVA2+IDIM-1) +
     +                                         (FTOT*XA12/2)*VDIR(IDIM)
 242        CONTINUE
 240     CONTINUE
      ENDIF
C
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

      IF ( DEBUG ) THEN
         IT = ZI(JIFL-1+3)
         IF ( IT .EQ. 1 ) THEN
            WRITE(37,1010)
            WRITE(37,1012)
         ENDIF
         WRITE(37,1110) IT, Z, K1, K2, K3
      ENDIF
C
 9999 CONTINUE
C
      CALL JEDEMA()
C
 1000 FORMAT('  IT       Z             DZ            D2Z          ',
     +       ' MA         PLAQUAGE     -----FORCE MECANISME-----   ',
     +       '--------TUBE GUIDE-------   ',
     +       '----------------DASHPOT----------------')
 1002 FORMAT(78X,'PRESSION      VISQUEUX      PRESSION      VISQUEUX',
     +           '      PRESSION      VISQUEUX      RETREINT')
 1100 FORMAT(1P,I4,12(2X,E12.5))
 1010 FORMAT('NOMBRE DE NOEUDS DANS LES DIFFERENTES ZONES')
 1012 FORMAT('  IT       Z        MECANISME_COMMANDE  GUIDAGE_CONTINU',
     +       '   TUBE_GUIDE/DASHPOT/RETREINT')
 1110 FORMAT(1P,I4,2X,E12.5,6X,I10,8X,I10,10X,I10)
C
C.============================ FIN DE LA ROUTINE ======================
      END
