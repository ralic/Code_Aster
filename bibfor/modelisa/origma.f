      SUBROUTINE ORIGMA ( MODELZ, GMAZ, NORIEN, REORIE,
     .                    ORIVEC, VECT, NOEUD, BAVARD )
      IMPLICIT REAL*8 (A-H,O-Z)
C.======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 06/02/2001   AUTEUR CIBHHLV L.VIVAN 
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
C     ORIGMA  --  LE BUT EST DE REORIENTER, SI C'EST NECESSAIRE,
C                 LES MAILLES DE PEAU DU GROUPE DE MAILLES GMAT
C                 CE PROGRAMME EST OPTIMISE : LE NOMBRE D'APPELS
C                 A ORIEMA EST REDUIT AU MINIMUM. 
C                 LA NORMALE A LA MAILLE DE PEAU DOIT ETRE
C                 EXTERIEURE AU VOLUME.
C                 DANS LE CAS OU REORIE EST FAUX, L'ORIENTATION
C                 GEOMETRIQUE N'EST PAS UTILISEE, CECI PERMET DE
C                 TESTER UNE SURFACE POUR UNE CONDITION AUX LIMITES
C                 DE PRESSION
C
C   ARGUMENT        E/S  TYPE         ROLE
C    MODELZ         IN    K*      NOM DU MODELE
C    GMAZ           IN    K*      NOM DU GROUP_MA DE MAILLES DE PEAU
C                                 A REORIENTER
C    NORIEN        VAR           NOMBRE DE MAILLES REORIENTEES
C    REORIE         IN    L       INDIQUE SI L'ON DOIT APPELER ORIEMA
C    ORIVEC         IN    L       INDIQUE SI L'ON DOIT ORIENTER
C                                 SELON LE VECTEUR VECT
C    BAVARD         IN    L       INDIQUE SI L'ON DOIT INFORMER SUR
C                                 L'ORIENTATION DES MAILLES EN CAS
C                                 DE PROBLEME
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
      CHARACTER*32     JEXNUM, JEXNOM
C -----  ARGUMENTS
      CHARACTER*(*)  MODELZ, GMAZ
      INTEGER NORIEN
      LOGICAL REORIE,ORIVEC,BAVARD
      REAL*8 VECT(*)
C -----  VARIABLES LOCALES
      INTEGER       I,JNOMA,IDTYMA,NBMAIL,JGRO,IMA
      INTEGER       NUMAIL,NUMA,NUTYMA,NIV,IFM,NORIEG
      CHARACTER*1   K1BID
      CHARACTER*8   NOMA, MODELE, TYPEL, NOMAIL
      CHARACTER*8   GMAT
      CHARACTER*24  GRMAMA, MAILMA
      LOGICAL       BAVAR2
C
      LOGICAL PASORI,DIME1,DIME2
      INTEGER JORI,LORI
      PASORI(I)=ZI(LORI-1+I).EQ.0
C
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
      CALL JEMARQ ( )
      IF (ORIVEC.AND.REORIE) CALL UTMESS('F','ORIGMA', 'ERREUR DE '//
     .  'PROGRAMMATION : ON NE PEUT PAS AVOIR REORIE ET ORIVEC VRAIS')
C
C
C --- INITIALISATIONS :
C     ---------------
      MODELE = MODELZ
      GMAT   = GMAZ
      CALL INFNIV ( IFM , NIV )
C
C --- RECUPERATION DU MAILLAGE ASSOCIE AU MODELE :
C     ------------------------------------------
      CALL JEVEUO(MODELE(1:8)//'.MODELE    .NOMA','L',JNOMA)
      NOMA = ZK8(JNOMA)
C
      GRMAMA = NOMA//'.GROUPEMA'
      MAILMA = NOMA//'.NOMMAI'
C
C --- VECTEUR DU TYPE DES MAILLES DU MAILLAGE :
C     ---------------------------------------
      CALL JEVEUO(NOMA//'.TYPMAIL','L',IDTYMA)
C
C --- RECUPERATION DU NOMBRE DE MAILLES DU GROUP_MA :
C     ---------------------------------------------
      GRMAMA = NOMA//'.GROUPEMA'
      CALL JELIRA (JEXNOM(GRMAMA,GMAT),'LONMAX',NBMAIL,K1BID)
      CALL JEVEUO (JEXNOM(GRMAMA,GMAT),'L',JGRO)
C
      CALL JEVEUO (NOMA//'.COORDO    .VALE','L',JCOOR)
C
C     ALLOCATIONS
      CALL WKVECT('&&ORIGMA.ORI1','V V I',NBMAIL,LORI)
      CALL WKVECT('&&ORIGMA.ORI2','V V I',NBMAIL,JORI)
      CALL WKVECT('&&ORIGMA.ORI3','V V I',NBMAIL,NORI)
      CALL WKVECT('&&ORIGMA.ORI4','V V I',NBMAIL,KORI)
C --- VERIFICATION DU TYPE DES MAILLES
C --- (ON DOIT AVOIR DES MAILLES DE PEAU) :
C     -----------------------------------
      NETAPE=0
    1 CONTINUE
      NETAPE=NETAPE+1
      BAVAR2=BAVARD
      IF (NETAPE.GT.1) THEN
C     C'EST LE DEUXIEME PASSAGE AVEC IMPRESSION DES MAILLES
        NIV=2
        BAVAR2=.FALSE.
      ENDIF
      DIME1=.FALSE.
      DIME2=.FALSE.
      DO 20 IMA = 1, NBMAIL
        ZI(LORI-1+IMA)=0
        NUMAIL = ZI(JGRO-1+IMA)
        CALL JENUNO(JEXNUM(MAILMA,NUMAIL),NOMAIL)
        CALL JENONU(JEXNOM(NOMA//'.NOMMAI',NOMAIL),NUMA)
        CALL JEVEUO(JEXNUM(NOMA//'.CONNEX',NUMA),'E',JDES)
        CALL JELIRA(JEXNUM(NOMA//'.CONNEX',NUMA),'LONMAX',NBNO1,K1BID)
        ZI(NORI-1+IMA)=NBNO1
        ZI(KORI-1+IMA)=JDES
C
C ---   TYPE DE LA MAILLE COURANTE :
C       --------------------------
        NUTYMA = ZI(IDTYMA+NUMA-1)
        CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',NUTYMA),TYPEL)
C
        IF (TYPEL(1:4).EQ.'QUAD') THEN
          DIME2=.TRUE.
        ELSEIF (TYPEL(1:4).EQ.'TRIA') THEN
          DIME2=.TRUE.
        ELSEIF (TYPEL(1:3).EQ.'SEG') THEN
          DIME1=.TRUE.
        ELSE
          CALL UTMESS('F','ORIGMA','IMPOSSIBILITE, LA MAILLE '//
     +                NOMAIL//' DOIT ETRE UNE MAILLE DE PEAU, I.E. '//
     +                'DE TYPE "QUAD" OU "TRIA" EN 3D OU DE TYPE "SEG" '
     +              //'EN 2D, ET ELLE EST DE TYPE : '//TYPEL)
        ENDIF
        IF (DIME1.AND.DIME2) CALL UTMESS('F','ORIGMA',
     +  'IMPOSSIBILITE DE MELANGER DES "SEG" ET DES "TRIA" OU "QUAD" !')
C
  20  CONTINUE
C
      NORIEG=0
C     ITERATION
      DO 300 IMA=1,NBMAIL
C     SI LA MAILLE N'EST PAS ORIENTEE ON L'ORIENTE
C     CAS AVEC VECTEUR
        IF (PASORI(IMA)) THEN
          IF (ORIVEC) THEN
C           VERIFICATION QUE LE NOEUD EST DANS LA MAILLE
            IF (DIME1)
     .        ICO=IORIV1(ZI(ZI(KORI-1+IMA)),
     .                   NOEUD,VECT,ZR(JCOOR))
            IF (DIME2)
     .        ICO=IORIV2(ZI(ZI(KORI-1+IMA)),ZI(NORI-1+IMA),
     .                   NOEUD,VECT,ZR(JCOOR))
C           SI MAILLE CORRECTE
            IF (ICO.NE.0) THEN
              IF (ICO.LT.0) NORIEG = NORIEG+1
            ELSE
              GOTO 300
            ENDIF
          ENDIF
          ZI(LORI-1+IMA)=1
          LLISTE=0
          ILISTE=0
          ZI(JORI+LLISTE)=IMA
          NUMAIL = ZI(JGRO-1+IMA)
          CALL JENUNO(JEXNUM(MAILMA,NUMAIL),NOMAIL)
          CALL ORIEMA ( NOMAIL, MODELE, REORIE, NORIEM )
          NORIEG = NORIEG + NORIEM
          IF (NIV.EQ.2) THEN
            WRITE (IFM,*) 'LA MAILLE ',NOMAIL,
     .                    ' SERT A ORIENTER UN NOUVEAU GROUPE CONNEXE'
            IF ( REORIE ) THEN
              IF ( NORIEG .NE. 0 ) THEN
                WRITE (IFM,*) 'ELLE A ETE REORIENTEE PAR RAPPORT AU '//
     .                        'MAILLAGE SUR LEQUEL ELLE S''APPUIE'
              ELSE
                WRITE (IFM,*) 'ELLE N''A PAS ETE REORIENTEE PAR '//
     .                 'RAPPORT AU MAILLAGE SUR LEQUEL ELLE S''APPUIE'
              ENDIF
            ELSEIF (ORIVEC) THEN
              IF ( NORIEG .NE. 0 ) THEN
                WRITE (IFM,*) 'ELLE A ETE REORIENTEE PAR RAPPORT AU '//
     .                        'VECTEUR'
              ELSE
                WRITE (IFM,*) 'ELLE N''A PAS ETE REORIENTEE PAR '//
     .                 'RAPPORT AU VECTEUR'
              ENDIF
            ELSE
              WRITE (IFM,*) 'ELLE EST SUPPOSEE BIEN ORIENTEE'
            ENDIF
          ENDIF
  200     CONTINUE
          IM1=ZI(JORI+ILISTE)
C         ON ESSAYE D'ORIENTER D'AUTRES MAILLES
          IDEB=IMA+1
          IF (ORIVEC) IDEB=1
          DO 210 IM2=IDEB,NBMAIL
            IF (PASORI(IM2)) THEN
C             VERIFICATION DE LA CONNEXITE ET REORIENTATION EVENTUELLE
              IF (DIME1)
     .        ICO=IORIM1(ZI(ZI(KORI-1+IM1)),
     .                   ZI(ZI(KORI-1+IM2)),
     .                   BAVAR2)
              IF (DIME2)
     .        ICO=IORIM2(ZI(ZI(KORI-1+IM1)),ZI(NORI-1+IM1),
     .                   ZI(ZI(KORI-1+IM2)),ZI(NORI-1+IM2),
     .                   BAVAR2)
C             SI MAILLES CONNEXES
              IF (ICO.NE.0) THEN
                ZI(LORI-1+IM2)=1
                LLISTE=LLISTE+1
                ZI(JORI+LLISTE)=IM2
              ENDIF
C              
              IF (ICO.LT.0.AND.BAVAR2) GOTO 1
C             SI ORIENTATIONS CONTRAIRES
              IF (ICO.LT.0) NORIEG=NORIEG+1
                IF (NIV.EQ.2.AND.ICO.NE.0) THEN
                  NUMAIL = ZI(JGRO-1+IM2)
                  CALL JENUNO(JEXNUM(MAILMA,NUMAIL),NOMAIL)
                  IF (ICO.LT.0) THEN
                    WRITE (IFM,*) 'LA MAILLE ',NOMAIL,
     .                    ' EST D''ORIENTATION CONTRAIRE'
                  ELSE IF (ICO.GT.0) THEN
                    WRITE (IFM,*) 'LA MAILLE ',NOMAIL,
     .                    ' EST DE MEME ORIENTATION'
                  ENDIF
                ENDIF 
            ENDIF
  210     CONTINUE
          ILISTE=ILISTE+1
          IF (ILISTE.LE.LLISTE) GOTO 200
          IF ( REORIE ) 
     .       WRITE(IFM,*)GMAT,' GROUPE CONNEXE DE ',ILISTE,' MAILLES'
C     ON NE TRAITE Q'UNE MAILLE SI UN VECTEUR EST FOURNI
          IF (ORIVEC) GOTO 400
        ENDIF
  300 CONTINUE
      IF (ORIVEC) WRITE(IFM,*) 'AUCUNE MAILLE '//
     .         ' N''A ETE ORIENTEE PAR RAPPORT AU VECTEUR'
  400 CONTINUE
C     CAS AVEC VECTEUR
      IF (ORIVEC) THEN
        DO 500 IMA=1,NBMAIL
          IF (PASORI(IMA)) THEN
             CALL JENUNO(JEXNUM(MAILMA,ZI(JGRO-1+IMA)),NOMAIL)
             IF (NIV.EQ.2) THEN
                WRITE(IFM,*) 'LA MAILLE '//NOMAIL//
     &         ' N''A PAS ETE ORIENTEE PAR RAPPORT AU VECTEUR'
             ENDIF
          ENDIF
C         VERIFICATION QUE LE NOEUD EST DANS LA MAILLE
          IF (DIME1)
     .      ICO=IORIV1(ZI(ZI(KORI-1+IMA)),
     .                 NOEUD,VECT,ZR(JCOOR))
          IF (DIME2)
     .      ICO=IORIV2(ZI(ZI(KORI-1+IMA)),ZI(NORI-1+IMA),
     .                 NOEUD,VECT,ZR(JCOOR))
C         SI MAILLE CORRECTE
          IF (ICO.LT.0) THEN
             CALL JENUNO(JEXNUM(MAILMA,ZI(JGRO-1+IMA)),NOMAIL)
             IF (NIV.EQ.2) THEN
                WRITE(IFM,*) 'LA MAILLE '//NOMAIL//
     .         ' EST MAL ORIENTEE PAR RAPPORT AU VECTEUR'
             ENDIF
          ENDIF
  500   CONTINUE
      ENDIF
C
      NORIEN=NORIEN+NORIEG
      IF ( REORIE ) WRITE(IFM,*)'    ',NORIEG,' MAILLES REORIENTEES'
C
      CALL JEDETR('&&ORIGMA.ORI1')
      CALL JEDETR('&&ORIGMA.ORI2')
      CALL JEDETR('&&ORIGMA.ORI3')
      CALL JEDETR('&&ORIGMA.ORI4')
      CALL JEDEMA()
      END
