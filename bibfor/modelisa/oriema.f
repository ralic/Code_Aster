      SUBROUTINE ORIEMA ( NOMAIZ, MODELZ, REORIE, NORIEN )
C.======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 21/05/2002   AUTEUR PABHHHH N.TARDIEU 
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
      IMPLICIT   NONE
      INTEGER                                     NORIEN
      LOGICAL                             REORIE
      CHARACTER*(*)       NOMAIZ, MODELZ
C
C     ORIEMA  --  ORIENTATION DE LA MAILLE DE NOM NOMAIL
C                 DE TELLE MANIERE A CE QUE LA NORMALE A CETTE MAILLE
C                 SOIT EXTERIEURE AU VOLUME.
C                 CETTE MAILLE EST UNE MAILLE DE PEAU :
C                  .EN 2D C'EST UNE MAILLE DE TYPE SEG2 OU SEG3
C                  .EN 3D C'EST UNE MAILLE DE TYPE TRIA3, TRIA6,
C                   QUAD4, QUAD8 OU QUAD9.
C
C IN  : NOMAIZ : NOM DE LA MAILLE
C IN  : MODELZ : NOM DU MODELE
C IN  : REORIE : = .FALSE.  ON VERIFIE L'ORIENTATION
C                = .TRUE.   ON REORIENTE
C OUT : NORIEN : = 0  LA MAILLE EST BIEN ORIENTEE
C                = 1  LA MAILLE EST A REORIENTER
C
C.========================= DEBUT DES DECLARATIONS ====================
C ----- COMMUNS NORMALISES  JEVEUX
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
      CHARACTER*32     JEXNUM, JEXNOM, JEXATR
C
      INTEGER       NBTYP, NBNOMX
      PARAMETER    (NBTYP  = 3)
      PARAMETER    (NBNOMX = 27)
      INTEGER       LTYP(NBTYP), LISNOE(NBNOMX), LISNO1(NBNOMX)
      REAL*8        N(3), N1N2(3), N1N3(3), XG(3),COON1(3),COON2(3)
      REAL*8        COON3(3),N1G(3),NORMEN,NORME1,XGN,ZERO,XG1(3)
      CHARACTER*1   K1BID
      CHARACTER*8   K8BID, NOMA, MODELE, TYPEL, NOMAIL
      CHARACTER*8   LISTYP(NBTYP), TYPE
      CHARACTER*19  LIGRMO
      CHARACTER*24  MAILMA
      INTEGER IGR,IEL,IALIEL,ILLIEL,IMA,INO,IAMACO,ILMACO,JNOMA,I
      INTEGER NDIM,IER,IDTYMA,IRET,NUMA1,NUTYMA,JDES,NBNO1,JCOOR
      INTEGER N1,N2,N3,IC,NTYP,NBNOSO,NBGREL,NUMA,NUMAIL,ITYP,LT,NEL
      INTEGER NBELEM,J,NNOE,K,NUNOEL,NUMGLM,INDI,INDIIS
      INTEGER NBNOE,IFM,NIV,J55,ILCNX1,NBSO,COMP,NVOL,JJ
C -----  FONCTIONS FORMULES
C     NUMAIL(IGR,IEL)=NUMERO DE LA MAILLE ASSOCIEE A L'ELEMENT IEL
      NUMAIL(IGR,IEL)=ZI(IALIEL-1+ZI(ILLIEL+IGR-1)+IEL-1)
C     NUMGLM(IMA,INO)=NUMERO GLOBAL DU NOEUD INO DE LA MAILLE IMA
C                     IMA ETANT UNE MAILLE DU MAILLAGE.
      NUMGLM(IMA,INO)=ZI(IAMACO-1+ZI(ILMACO+IMA-1)+INO-1)
C
C ========================= DEBUT DU CODE EXECUTABLE ==================
C
      CALL JEMARQ ( )
      CALL INFNIV ( IFM , NIV )
C
C --- INITIALISATIONS :
C     ---------------
      MODELE = MODELZ
      NOMAIL = NOMAIZ
      NORIEN = 0
      ZERO   = 0.0D0
C
C --- MAILLAGE ASSOCIE AU MODELE :
C     --------------------------
      CALL JEVEUO(MODELE(1:8)//'.MODELE    .NOMA','L',JNOMA)
      NOMA = ZK8(JNOMA)
      MAILMA = NOMA//'.NOMMAI'
C
      DO 10 I = 1, 3
        N(I)    = ZERO
        N1N2(I) = ZERO
        N1N3(I) = ZERO
        XG(I)   = ZERO
        XG1(I)  = ZERO
  10  CONTINUE
C
      DO 20 I = 1, NBNOMX
        LISNOE(I) = 0
        LISNO1(I) = 0
  20  CONTINUE
      DO 21 I = 1, NBTYP
        LISTYP(I) = '        '
  21  CONTINUE
C
C --- LIGREL DU MODELE :
C     ----------------
      LIGRMO = MODELE//'.MODELE'
C
C --- RECUPERATION DE LA DIMENSION (2 OU 3) DU PROBLEME :
C     -------------------------------------------------
      CALL DISMOI('F','DIM_GEOM',MODELE,'MODELE',NDIM,K8BID,IER)
      IF ( NDIM .GT. 1000 )  NDIM = 3
C
C --- VECTEUR DU TYPE DES MAILLES DU MAILLAGE :
C     ---------------------------------------
      CALL JEVEUO(NOMA//'.TYPMAIL','L',IDTYMA)
C
      CALL JEEXIN(NOMA//'.CONNEX',IRET)
      IF (IRET.GT.0) THEN
        CALL JEVEUO(NOMA//'.CONNEX','L',IAMACO)
        CALL JEVEUO(JEXATR(NOMA//'.CONNEX','LONCUM'),'L',ILMACO)
      END IF
      CALL JEVEUO(LIGRMO//'.LIEL','L',IALIEL)
      CALL JEVEUO(JEXATR(LIGRMO//'.LIEL','LONCUM'),'L',ILLIEL)
C
C --- NUMERO DE LA MAILLE NOMAIL :
C     --------------------------
      CALL JENONU(JEXNOM(MAILMA,NOMAIL),NUMA1)
C
C --- TYPE DE LA MAILLE :
C     -----------------
      NUTYMA = ZI(IDTYMA+NUMA1-1)
      CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',NUTYMA),TYPEL)
C
C --- COORDONNEES DES CONNECTIVITES :
C     -----------------------------
      CALL JEVEUO(JEXNUM(NOMA//'.CONNEX',NUMA1),'E',JDES)
      CALL JELIRA(JEXNUM(NOMA//'.CONNEX',NUMA1),'LONMAX',NBNO1,K1BID)
      DO 30 I = 1, NBNO1
        LISNO1(I) = ZI(JDES+I-1)
  30  CONTINUE
C
C --- RECUPERATION DU TABLEAU DES COORDONNEES :
C     ---------------------------------------
      CALL JEVEUO (NOMA//'.COORDO    .VALE','L',JCOOR)
C
C --- NUMERO DES 2 (3 EN 3D) PREMIERS NOEUDS DE LA MAILLE :
C     ---------------------------------------------------
      N1 = ZI(JDES+1-1)
      N2 = ZI(JDES+2-1)
      IF (NDIM.EQ.3)  N3 = ZI(JDES+3-1)
C
      DO 56 IC=1,3
         COON1(IC)=ZR(JCOOR+3*(N1-1)+IC-1)
         COON2(IC)=ZR(JCOOR+3*(N2-1)+IC-1)
56    CONTINUE
      CALL VDIFF(3,COON2,COON1,N1N2)
C
      IF (NDIM.EQ.2) THEN
        N(1) =  N1N2(2)
        N(2) = -N1N2(1)
      ELSEIF (NDIM.EQ.3) THEN
         DO 57 IC=1,3
            COON3(IC)=ZR(JCOOR+3*(N3-1)+IC-1)
57       CONTINUE
         CALL VDIFF(3,COON3,COON1,N1N3)
         CALL PROVEC(N1N2, N1N3, N)
      ENDIF
C
      CALL NORMEV(N,NORMEN)

      IF (TYPEL(1:4).EQ.'QUAD') THEN
        LISTYP(1)(1:4) = 'HEXA'
        LISTYP(2)(1:5) = 'PENTA'
        LISTYP(3)(1:5) = 'PYRAM'
C
        LTYP(1) = 4
        LTYP(2) = 5
        LTYP(3) = 5
        NTYP   = 3
        NBNOSO = 4
C
      ELSEIF (TYPEL(1:4).EQ.'TRIA') THEN
        LISTYP(1)(1:5) = 'PENTA'
        LISTYP(2)(1:5) = 'TETRA'
        LISTYP(3)(1:5) = 'PYRAM'
C
        LTYP(1) = 5
        LTYP(2) = 5
        LTYP(3) = 5
C
        NTYP   = 3
        NBNOSO = 3
C
      ELSEIF (TYPEL(1:3).EQ.'SEG') THEN
       IF (NDIM.EQ.3) GOTO 9999
        LISTYP(1)(1:4) = 'TRIA'
        LISTYP(2)(1:4) = 'QUAD'
C
        LTYP(1) = 4
        LTYP(2) = 4
C
        NTYP   = 2
        NBNOSO = 2
C
      ELSE
        CALL UTMESS('F','ORIEMA','IMPOSSIBILITE, LA MAILLE '//
     +              NOMAIL//' DOIT ETRE UNE MAILLE DE PEAU, I.E. '//
     +              'DE TYPE "QUAD" OU "TRIA" EN 3D OU DE TYPE "SEG" '
     +            //'EN 2D, ET ELLE EST DE TYPE : '//TYPEL)
      ENDIF
C
C     CENTRE DE GRAVITE DE CETTE MAILLE
C
      DO 130 K = 1,NBNOSO
         NUNOEL = NUMGLM(NUMA1,K)
         XG1(1)  = XG1(1) + ZR(JCOOR+3*(NUNOEL-1)+1-1)
         XG1(2)  = XG1(2) + ZR(JCOOR+3*(NUNOEL-1)+2-1)
         XG1(3)  = XG1(3) + ZR(JCOOR+3*(NUNOEL-1)+3-1)
 130  CONTINUE
      XG1(1) = XG1(1) / NBNOSO
      XG1(2) = XG1(2) / NBNOSO
      XG1(3) = XG1(3) / NBNOSO
   
      COMP=0
      NVOL=0

C
C --- RECHERCHE DE L'ELEMENT DE VOLUME SUR LEQUEL S'APPUIE LA MAILLE
C --- DE NOM NOMAIL :
C     =============
C
C --- BOUCLE SUR LE NOMBRE DE GRELS DU LIGREL DU MODELE :
C     -------------------------------------------------
      DO 40 IGR = 1,NBGREL(LIGRMO)
C
C ---   RECUPERATION DU TYPE DU PREMIER ELEMENT DU GREL :
C       ----------------------------------------------
         NUMA = NUMAIL(IGR,1)
         IF ( NUMA .GT. 0 ) THEN
            NUTYMA = ZI(IDTYMA+NUMA-1)
            CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',NUTYMA),TYPE)
C
            DO 50 ITYP = 1, NTYP
               LT=LTYP(ITYP)
               IF (TYPE(1:LT).EQ.LISTYP(ITYP)(1:LT)) GOTO 60
  50        CONTINUE
         ENDIF
         GOTO 40
  60     CONTINUE
C
         IF (TYPE(1:4).EQ.'HEXA') THEN
            NBSO=8
         ELSEIF (TYPE(1:4).EQ.'PENT') THEN
            NBSO=6
         ELSEIF (TYPE(1:4).EQ.'PYRA') THEN
            NBSO=5
         ELSEIF (TYPE(1:4).EQ.'TETR') THEN
            NBSO=4
         ELSEIF (TYPE(1:4).EQ.'QUAD') THEN
            NBSO=4
         ELSEIF (TYPE(1:4).EQ.'TRIA') THEN
            NBSO=3
         ENDIF
C
         NEL  = NBELEM(LIGRMO,IGR)
         DO 70 J = 1,NEL
            NUMA = NUMAIL(IGR,J)
            IF ( NUMA .LE. 0 ) GOTO 70
C
C ---       IL S'AGIT D'UNE MAILLE PHYSIQUE DU MAILLAGE :
C           -------------------------------------------
C
            CALL JEVEUO(JEXATR(NOMA//'.CONNEX','LONCUM'),'L',ILCNX1)
            NNOE = ZI(ILCNX1+NUMA)-ZI(ILCNX1-1+NUMA)
            DO 80 K = 1,NNOE
               LISNOE(K) = NUMGLM(NUMA,K)
  80        CONTINUE
C
C ---       ON REGARDE SI LES NOEUDS DE LA MAILLE NOMAIL
C ---       APPARTIENNENT A LISNOE :
C           ----------------------
            DO 90 K = 1,NBNOSO
               INDI = INDIIS(LISNOE,ZI(JDES+K-1),1,NNOE)
               IF ( INDI .EQ. 0 )   GOTO 70
  90        CONTINUE

C
C ---       ON EST SUR LA MAILLE DE VOLUME A LAQUELLE APPARTIENT
C ---       LA MAILLE DE PEAU
C ---       DETERMINATION DU CENTRE DE GRAVITE DE CETTE MAILLE
C ---       DE VOLUME :
C           ---------
            DO 100 K = 1,NBSO
               NUNOEL = NUMGLM(NUMA,K)
C ---   ON COMPTE LE NOMBRE DE MAILLES VOLUMES AYANT NOMAIL COMME PEAU
C ---   SI CE NOMBRE EST SUPERIEUR A UN ON S ARRETE EN FATAL
               DO 140 JJ=1, NBNOSO
                  IF (NUNOEL.EQ.ZI(JDES+JJ-1))   COMP=COMP+1 
 140           CONTINUE
               IF (COMP.EQ.NBNOSO) THEN
                  COMP=0
                  NVOL=NVOL+1 
               ENDIF
               IF (NVOL.GE.2) THEN 
                  CALL UTMESS('F','ORIEMA','LA MAILLE DE PEAU '//
     +              NOMAIL//' S''APPUIE SUR PLUS D''UNE MAILLE '//
     +              'VOLUMIQUE')
               ENDIF
               XG(1)  = XG(1) + ZR(JCOOR+3*(NUNOEL-1)+1-1)
               XG(2)  = XG(2) + ZR(JCOOR+3*(NUNOEL-1)+2-1)
               XG(3)  = XG(3) + ZR(JCOOR+3*(NUNOEL-1)+3-1)
 100        CONTINUE
            XG(1) = XG(1) / NBSO
            XG(2) = XG(2) / NBSO
            XG(3) = XG(3) / NBSO

            CALL VDIFF  ( 3, XG, XG1, N1G )
            CALL NORMEV ( N1G, NORME1 )
            CALL PSCAL  ( 3, N1G, N, XGN )
C
C ---       SI XGN > 0, LA NORMALE A LA MAILLE DE PEAU
C ---       EST DIRIGEE VERS L'INTERIEUR DU VOLUME, IL FAUT
C ---       LA REORIENTER :
C           -------------
            IF ( XGN .GT. ZERO ) THEN
               NORIEN = NORIEN + 1
               IF ( .NOT. REORIE ) GOTO 9999
               IF ( TYPEL(1:5).EQ.'QUAD9' .OR.
     &              TYPEL(1:5).EQ.'TRIA7' ) THEN
                  NBNOE = NBNO1 - 1
               ELSE
                  NBNOE = NBNO1
               ENDIF
               K = 0
               DO 110 I = NBNOSO, 1 , -1
                  K = K+1
                  ZI(JDES+I-1) = LISNO1(K)
 110           CONTINUE
               IF (NBNOSO.NE.NBNOE) THEN
                  K = 0
                  DO 120 I = NBNOE-1, NBNOSO+1 , -1
                     K = K+1
                     ZI(JDES+I-1) = LISNO1(K+NBNOSO)
 120              CONTINUE
                  ZI(JDES+NBNOE-1) = LISNO1(NBNOE)
               ENDIF
               IF (NIV.EQ.2) THEN
                  WRITE(IFM,*) ' REORIENTATION MAILLE ',NOMAIL,
     &                         ' NOEUDS ',(ZI(JDES+J55-1),J55=1,NBNO1),
     &                         ' PRODUIT SCALAIRE ',XGN
               ENDIF
            ENDIF
  70     CONTINUE
  40  CONTINUE
 9999 CONTINUE
C
      CALL JEDEMA()
C
      END
