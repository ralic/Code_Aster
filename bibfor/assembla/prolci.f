      SUBROUTINE PROLCI(NUZ,RTBLOC,TYPMAT,BASE)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*) NUZ
      REAL*8 RTBLOC
      CHARACTER*1 TYPMAT,BASE
      CHARACTER*8 KBID
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ASSEMBLA  DATE 29/02/2000   AUTEUR VABHHTS J.PELLET 
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
C TOLE CRP_20
C-----------------------------------------------------------------------
C --- DESCRIPTION DES PARAMETRES
C OUT K*14 NU     :L'OBJET NU DE S.D. NUME_DDL EST COMPLETE D'UN .SLC*
C IN  K*14 NU     : NOM DE L'OBJET DE S.D. NUME_DDL A COMPLETER.
C IN  R   RTBLOC  : TAILLE D'UN BLOC DU PROFIL
C                          1 --> ECRITURES ( RESERVE AU PROGRAMMEUR)
C IN  K1  TYPMAT  : TYPE DES MATRICES A STOCKER 'S':SYMETRIQUE
C                  'A':ANTISYMETRIQUE(PAS IMPLEMENTE)
C                  'H':HERMITIENNE(PAS IMPLEMENTE)
C IN  K1  BASE    : BASE DE CREATION DU STOCKAGE
C ----------------------------------------------------------------------
C-----------------------------------------------------------------------
C     FONCTIONS JEVEUX
C-----------------------------------------------------------------------
      CHARACTER*32 JEXNUM,JEXNOM,JEXATR
C-----------------------------------------------------------------------
C     COMMUNS   JEVEUX
C-----------------------------------------------------------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      CHARACTER*8 ZK8,EXIELE,MA,MO
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C----------------------------------------------------------------------
C     VARIABLES LOCALES
C----------------------------------------------------------------------
      CHARACTER*14 NU
      CHARACTER*24 KNULIL,NOMLI,KHCOL,KADIA,KABLO,KIABL,KVALE,KDESC,
     &             KTEMP1,KTEMP2,KREFE
      REAL*8 X
      INTEGER HMAX,HMOY,ITBLOC
C-----------------------------------------------------------------------
C     FONCTIONS LOCALES D'ACCES AUX DIFFERENTS CHAMPS DES
C     S.D. MANIPULEES DANS LE SOUS PROGRAMME
C-----------------------------------------------------------------------
      INTEGER ZZCONX,ZZNBNE,ZZLIEL,ZZNGEL,ZZNSUP,ZZNELG,ZZNELS
      INTEGER ZZNEMA,ZZPRNO,IZZPRN

C---- FONCTION D ACCES AU CHAMP CONNEX DE LA S.D. MAILLA DE TYPE
C     MAILLAGE
C     ZZCONX(IMAIL,J) = NUMERO DANS LA NUMEROTATION DU MAILLAGE
C         DU NOEUD J DE LA MAILLE IMAIL
      ZZCONX(IMAIL,J) = ZI(ICONX1-1+ZI(ICONX2+IMAIL-1)+J-1)

C---- NBRE DE NOEUDS DE LA MAILLE IMAIL DU MAILLAGE

      ZZNBNE(IMAIL) = ZI(ICONX2+IMAIL) - ZI(ICONX2+IMAIL-1)

C---- FONCTION D ACCES AUX ELEMENTS DES CHAMPS LIEL DES S.D. LIGREL
C     REPERTORIEES DANS LE REPERTOIRE TEMPORAIRE .MATAS.LILI
C     ZZLIEL(ILI,IGREL,J) =
C      SI LA JIEME MAILLE DU LIEL IGREL DU LIGREL ILI EST:
C          -UNE MAILLE DU MAILLAGE : SON NUMERO DANS LE MAILLAGE
C          -UNE MAILLE TARDIVE : -POINTEUR DANS LE CHAMP .NEMA

      ZZLIEL(ILI,IGREL,J) = ZI(ZI(IADLIE+3* (ILI-1)+1)-1+
     &                      ZI(ZI(IADLIE+3* (ILI-1)+2)+IGREL-1)+J-1)

C---- NBRE DE GROUPES D'ELEMENTS (DE LIEL) DU LIGREL ILI

      ZZNGEL(ILI) = ZI(IADLIE+3* (ILI-1))

C---- NBRE DE NOEUDS DE LA MAILLE TARDIVE IEL ( .NEMA(IEL))
C     DU LIGREL ILI REPERTOIRE .LILI
C     (DIM DU VECTEUR D'ENTIERS .LILI(ILI).NEMA(IEL) )

      ZZNSUP(ILI,IEL) = ZI(ZI(IADNEM+3* (ILI-1)+2)+IEL) -
     &                  ZI(ZI(IADNEM+3* (ILI-1)+2)+IEL-1) - 1

C---- NBRE D ELEMENTS DU LIEL IGREL DU LIGREL ILI DU REPERTOIRE TEMP.
C     .MATAS.LILI(DIM DU VECTEUR D'ENTIERS .LILI(ILI).LIEL(IGREL) )

      ZZNELG(ILI,IGREL) = ZI(ZI(IADLIE+3* (ILI-1)+2)+IGREL) -
     &                    ZI(ZI(IADLIE+3* (ILI-1)+2)+IGREL-1) - 1

C---- NBRE D ELEMENTS SUPPLEMENTAIRE (.NEMA) DU LIGREL ILI DU
C     REPERTOIRE TEMPORAIRE .MATAS.LILI

      ZZNELS(ILI) = ZI(IADNEM+3* (ILI-1))

C---- FONCTION D ACCES AUX ELEMENTS DES CHAMPS NEMA DES S.D. LIGREL
C     REPERTORIEES DANS LE REPERTOIRE TEMPO. .MATAS.LILI
C     ZZNEMA(ILI,IEL,J) =  1.LE. J .GE. ZZNELS(ILI)
C      SI LE J IEME NOEUD DE LA MAILE TARDIVE IEL DU LIGREL ILI EST:
C          -UN NOEUD DU MAILLAGE : SON NUMERO DANS LE MAILLAGE
C          -UN NOEUD TARDIF : -SON NUMERO DANS LA NUMEROTATION LOCALE
C                              AU LIGREL ILI
C     ZZNEMA(ILI,IEL,ZZNELS(ILI)+1)=NUMERO DU TYPE_MAILLE DE LA MAILLE
C                                   IEL DU LIGREL ILI

      ZZNEMA(ILI,IEL,J) = ZI(ZI(IADNEM+3* (ILI-1)+1)-1+
     &                    ZI(ZI(IADNEM+3* (ILI-1)+2)+IEL-1)+J-1)

C---- FONCTION D ACCES AUX ELEMENTS DES CHAMPS PRNO DES S.D. LIGREL
C     REPERTORIEES DANS NU.LILI DE LA S.D. NUME_DDL ET A LEURS ADRESSES
C     ZZPRNO(ILI,NUNOEL,1) = NUMERO DE L'EQUATION ASSOCIEES AU 1ER DDL
C                            DU NOEUD NUNOEL DANS LA NUMEROTATION LOCALE
C                            AU LIGREL ILI DE .LILI
C     ZZPRNO(ILI,NUNOEL,2) = NOMBRE DE DDL PORTES PAR LE NOEUD NUNOEL
C     ZZPRNO(ILI,NUNOEL,2+1) = 1ER CODE
C     ZZPRNO(ILI,NUNOEL,2+NEC) = NEC IEME CODE

      IZZPRN(ILI,NUNOEL,L) = (IDPRN1-1+ZI(IDPRN2+ILI-1)+
     &                       (NUNOEL-1)* (NEC+2)+L-1)
      ZZPRNO(ILI,NUNOEL,L) = ZI(IDPRN1-1+ZI(IDPRN2+ILI-1)+
     &                       (NUNOEL-1)* (NEC+2)+L-1)
C----------------------------------------------------------------------
C                DEBUT DES INSTRUCTIONS
C----------------------------------------------------------------------

      CALL JEMARQ()

C----RECUPERATION DU NIVEAU D'IMPRESSION

      CALL INFNIV(IFM,NIV)
C----------------------------------------------------------------------

      NU = NUZ

      CALL DISMOI('F','NUM_GD_SI',NU,'NUME_DDL',IGD,KBID,IER)
      NEC = NBEC(IGD)
      CALL DISMOI('F','NOM_MAILLA',NU,'NUME_DDL',IBID,MA,IER)
      CALL JEEXIN(MA//'.CONNEX',IRET)
      IF (IRET.GT.0) THEN
        CALL JEVEUO(MA//'.CONNEX','L',ICONX1)
        CALL JEVEUO(JEXATR(MA//'.CONNEX','LONCUM'),'L',ICONX2)
      END IF


      ITBLOC = NINT(RTBLOC*1024)

      CALL DISMOI('F','NOM_MODELE',NU,'NUME_DDL',IBID,MO,IERD)
      CALL DISMOI('F','NB_NO_SS_MAX',MA,'MAILLAGE',NBNOSS,KBID,IERD)
      CALL JEVEUO(NU//'     .ADNE','E',IADNEM)
      CALL JEVEUO(NU//'     .ADLI','E',IADLIE)
      CALL JEVEUO(NU//'.NUME.PRNO','E',IDPRN1)
      CALL JEVEUO(JEXATR(NU//'.NUME.PRNO','LONCUM'),'L',IDPRN2)

C     100 EST SUPPOSE ETRE LA + GDE DIMENSION D'UNE MAILLE STANDARD:
      NBNOSS = MAX(NBNOSS,100)
      CALL JEEXIN('&&PROLCI.NUMLOC',IRET)
      IF (IRET.GT.0) CALL JEDETR('&&PROLCI.NUMLOC')
C     -- NUMLOC(K,INO) (K=1,4)(INO=1,NBNO(MAILLE))
      CALL WKVECT('&&PROLCI.NUMLOC','V V I',4*NBNOSS,IANULO)

C---- NOMS DES PRINCIPAUX OBJETS JEVEUX LIES A L'OBJET PROF_LIGN_CIEL
C     LIE A LA NUMEROTATION NU

      KHCOL = NU//'.SLC'//TYPMAT//'.HCOL'
      KADIA = NU//'.SLC'//TYPMAT//'.ADIA'
      KABLO = NU//'.SLC'//TYPMAT//'.ABLO'
      KIABL = NU//'.SLC'//TYPMAT//'.IABL'
      KDESC = NU//'.SLC'//TYPMAT//'.DESC'
      KREFE = NU//'.SLC'//TYPMAT//'.REFE'


C---- ALLOCATION DE .SLC*.REFE

      CALL WKVECT(KREFE,BASE//' V K24',1,IDREFE)
      CALL JEECRA(KREFE,'DOCU',0,'SLC'//TYPMAT)
      ZK24(IDREFE) = NU

C---- NOM DES PRINCIPAUX OBJETS JEVEUX LIES AU CHAMP .NUME DE NU

      KNULIL = NU//'.NUME.LILI'

C---- RECUPERATION DU NOMBRE D'EQUATIONS

      CALL JEVEUO(NU//'.NUME.NEQU','L',IADEQU)
      CALL JEVEUO(NU//'.NUME.NUEQ','L',IANUEQ)
      CALL JELIRA(NU//'.NUME.PRNO','NMAXOC',NLILI,KBID)
      NEQU = ZI(IADEQU)

C---- ALLOCATION DE .PLC*.HCOL ET .PLC*.ADIA

      CALL WKVECT(KHCOL,BASE//' V I',NEQU,IDHCOL)
      CALL WKVECT(KADIA,BASE//' V I',NEQU,IDADIA)

C---- CALCUL DES HAUTEURS DE COLONNE : .MATAS.HCOL(J)
C     ON SUPPOSE QUE '&MAILLA' EST LE 1ER NOM DU REPERTOIRE .LILI
C     CE QUI A PRIORI EST VRAI PAR CONSTRUCTION
      ILIMNU = 1

C---- BOUCLE SUR LES LIGREL DU REPERTOIRE KNULIL

      DO 160 ILI = 2,NLILI
        CALL JENUNO(JEXNUM(KNULIL,ILI),NOMLI)
        IF (NOMLI(1:8).EQ.MO) THEN
          CALL DISMOI('F','NB_SS_ACTI',MO,'MODELE',NBSS,KBID,IERD)
        ELSE
          NBSS = 0
        END IF

C         -- TRAITEMENT DES SOUS-STRUCTURES:
C         ----------------------------------
        IF (NBSS.GT.0) THEN

          CALL DISMOI('F','NB_SM_MAILLA',MO,'MODELE',NBSMA,KBID,IERD)
          CALL JEVEUO(MO//'.SSSA','L',IASSSA)

          DO 50,IMA = 1,NBSMA
            IF (ZI(IASSSA-1+IMA).EQ.0) GO TO 50
            CALL JEVEUO(JEXNUM(MA//'.SUPMAIL',IMA),'L',IAMAIL)
            CALL JELIRA(JEXNUM(MA//'.SUPMAIL',IMA),'LONMAX',NNOE,KBID)
            DO 40 K1 = 1,NNOE
              N1 = ZI(IAMAIL-1+K1)
              IF (N1.EQ.0) CALL JXABOR()
              IAD1 = ZZPRNO(ILIMNU,N1,1)
              NDDL1 = ZZPRNO(ILIMNU,N1,2)
              ZI(IANULO-5+4*K1+1) = N1
              ZI(IANULO-5+4*K1+2) = IAD1
              ZI(IANULO-5+4*K1+3) = NDDL1

C               -- ON CHERCHE QUEL LE DDL DU NOEUD K1 QUI A LE
C                  NUMERO D'EQUATION LE PLUS PETIT
C                  ON STOCKE CE NUMERO EN POSITION 4.
              IEQMIN = ZI(IANUEQ-1+IAD1)
              DO 10 I = 2,NDDL1
                IEQMIN = MIN(IEQMIN,ZI(IANUEQ-1+IAD1+I-1))
   10         CONTINUE
              ZI(IANULO-5+4*K1+4) = IEQMIN

C              ---- BOUCLE SUR LE NOEUD J DE LA MAILLE
              DO 30 K2 = 1,K1
                IF (ZI(IANULO-5+4*K2+4).LE.ZI(IANULO-5+4*K1+4)) THEN
                  KLI = K2
                  KCO = K1
                ELSE
                  KLI = K1
                  KCO = K2
                END IF
                IADC = ZI(IANULO-5+4*KCO+2)
                NDDLC = ZI(IANULO-5+4*KCO+3)
                IAD21 = ZI(IANULO-5+4*KLI+4)

C                    ---- BOUCLE SUR LES DDLS
                DO 20 J = 1,NDDLC
                  IAD11 = ZI(IANUEQ-1+IADC+J-1)
                  IF (ZI(IDHCOL+IAD11-1).LT. (IAD11-IAD21+1)) THEN
                    ZI(IDHCOL+IAD11-1) = (IAD11-IAD21+1)
                  END IF
   20           CONTINUE
   30         CONTINUE
   40       CONTINUE
   50     CONTINUE

        END IF


C         -- TRAITEMENT DES ELEMENTS FINIS CLASSIQUES:
C         --------------------------------------------
        CALL DISMOI('F','EXI_ELEM',NOMLI,'LIGREL',IBID,EXIELE,IERD)
        IF (EXIELE(1:3).EQ.'NON') GO TO 160
        DO 150 IGR = 1,ZZNGEL(ILI)

C---- BOUCLE SUR LES OCCURENCES DES NOMLI.LIEL

          NEL = ZZNELG(ILI,IGR)

C---- BOUCLE SUR LES MAILLES

          DO 140 IEL = 1,NEL
            NUMA = ZZLIEL(ILI,IGR,IEL)
            IF (NUMA.GT.0) THEN
              NNOE = ZZNBNE(NUMA)

C---- BOUCLE SUR LE NOEUD I DE LA MAILLE

              DO 90 K1 = 1,NNOE
                N1 = ZZCONX(NUMA,K1)
                IAD1 = ZZPRNO(ILIMNU,N1,1)
                IF (IAD1.EQ.0) THEN
                  CALL UTDEBM('F','PROLCI','---')
                  CALL UTIMPI('L',' LE NOEUD  :',1,N1)
                  CALL UTIMPK('S',' DU LIGREL :',1,NOMLI)
                  CALL UTIMPK('L',' N"A PAS D"ADRESSE DANS  :',1,NU)
                  CALL UTFINM()
                END IF
                NDDL1 = ZZPRNO(ILIMNU,N1,2)
                ZI(IANULO-5+4*K1+1) = N1
                ZI(IANULO-5+4*K1+2) = IAD1
                ZI(IANULO-5+4*K1+3) = NDDL1
C    -- ON CHERCHE QUEL LE DDL DU NOEUD K1 QUI A LE NUMERO D'EQUATION
C    -- LE PLUS PETIT, ON STOCKE CE NUMERO EN POSITION 4.
                IEQMIN = ZI(IANUEQ-1+IAD1)
                DO 60 I = 2,NDDL1
                  IEQMIN = MIN(IEQMIN,ZI(IANUEQ-1+IAD1+I-1))
   60           CONTINUE
                ZI(IANULO-5+4*K1+4) = IEQMIN

C---- BOUCLE SUR LE NOEUD J DE LA MAILLE

                DO 80 K2 = 1,K1
                  IF (ZI(IANULO-5+4*K2+4).LE.ZI(IANULO-5+4*K1+4)) THEN
                    KLI = K2
                    KCO = K1
                  ELSE
                    KLI = K1
                    KCO = K2
                  END IF
                  IADC = ZI(IANULO-5+4*KCO+2)
                  NDDLC = ZI(IANULO-5+4*KCO+3)
                  IAD21 = ZI(IANULO-5+4*KLI+4)

C---- BOUCLE SUR LES DDLS

                  DO 70 J = 1,NDDLC
                    IAD11 = ZI(IANUEQ-1+IADC+J-1)
                    IF (ZI(IDHCOL+IAD11-1).LT. (IAD11-IAD21+1)) THEN
                      ZI(IDHCOL+IAD11-1) = (IAD11-IAD21+1)
                    END IF
   70             CONTINUE
   80           CONTINUE
   90         CONTINUE

C---- SI ON EST SUR UNE MAILLE TARDIVE

            ELSE
              NUMA = -NUMA
              NNOE = ZZNSUP(ILI,NUMA)

C---- BOUCLE SUR LE NOEUD I DE LA MAILLE

              DO 130 K1 = 1,NNOE
                N1 = ZZNEMA(ILI,NUMA,K1)
                IF (N1.LT.0) THEN

C---- SI LE NOEUD N1 EST TARDIF LUI AUSSI

                  N1 = -N1
                  IAD1 = ZZPRNO(ILI,N1,1)
                  IF (IAD1.EQ.0) THEN
                    CALL UTDEBM('F','PROLCI','---')
                    CALL UTIMPI('L',' LE NOEUD  :',1,N1)
                    CALL UTIMPK('S',' DU LIGREL :',1,NOMLI)
                    CALL UTIMPK('L',' N"A PAS D"ADRESSE DANS : ',1,NU)
                    CALL UTFINM()
                  END IF
                  NDDL1 = ZZPRNO(ILI,N1,2)
                ELSE

C---- SI LE NOEUD N1 EST UN NOEUD DU MAILLAGE

                  IAD1 = ZZPRNO(ILIMNU,N1,1)
                  IF (IAD1.EQ.0) THEN
                    CALL UTDEBM('F','PROLCI','---')
                    CALL UTIMPI('L',' LE NOEUD  :',1,N1)
                    CALL UTIMPK('S',' DU LIGREL :',1,NOMLI)
                    CALL UTIMPK('L',' N"A PAS D"ADRESSE DANS : ',1,NU)
                    CALL UTFINM()
                  END IF
                  NDDL1 = ZZPRNO(ILIMNU,N1,2)
                END IF
                ZI(IANULO-5+4*K1+1) = N1
                ZI(IANULO-5+4*K1+2) = IAD1
                ZI(IANULO-5+4*K1+3) = NDDL1
C    -- ON CHERCHE QUEL LE DDL DU NOEUD K1 QUI A LE NUMERO D'EQUATION
C    -- LE PLUS PETIT, ON STOCKE CE NUMERO EN POSITION 4.
                IEQMIN = ZI(IANUEQ-1+IAD1)
                DO 100 I = 2,NDDL1
                  IEQMIN = MIN(IEQMIN,ZI(IANUEQ-1+IAD1+I-1))
  100           CONTINUE
                ZI(IANULO-5+4*K1+4) = IEQMIN

C---- BOUCLE SUR LE NOEUD J DE LA MAILLE

                DO 120 K2 = 1,K1
                  IF (ZI(IANULO-5+4*K2+4).LE.ZI(IANULO-5+4*K1+4)) THEN
                    KLI = K2
                    KCO = K1
                  ELSE
                    KLI = K1
                    KCO = K2
                  END IF
                  IADC = ZI(IANULO-5+4*KCO+2)
                  NDDLC = ZI(IANULO-5+4*KCO+3)
                  IAD21 = ZI(IANULO-5+4*KLI+4)

C---- BOUCLE SUR LES DDLS

                  DO 110 J = 1,NDDLC
                    IAD11 = ZI(IANUEQ-1+IADC+J-1)
                    IF (ZI(IDHCOL+IAD11-1).LT. (IAD11-IAD21+1)) THEN
                      ZI(IDHCOL+IAD11-1) = (IAD11-IAD21+1)
                    END IF
  110             CONTINUE
  120           CONTINUE
  130         CONTINUE
            END IF
  140     CONTINUE
  150   CONTINUE
  160 CONTINUE


C---- CALCUL DES ADIA

C---- HMAX = HAUTEUR MAXIMUM DES COLONNES

      HMAX = 0
      HMOY = 0
      DO 170 IEQUA = 1,NEQU
        HMAX = MAX(HMAX,ZI(IDHCOL+IEQUA-1))
        HMOY = HMOY + ZI(IDHCOL+IEQUA-1)
  170 CONTINUE
      IF (NIV.GE.1) THEN
        WRITE (IFM,*) '--- HAUTEUR MAXIMUM D''UNE COLONNE : ',HMAX
        HMOY = HMOY/NEQU
        WRITE (IFM,*) '--- HAUTEUR MOYENNE D''UNE COLONNE : ',HMOY
      END IF
      IF (2*HMAX.GT.ITBLOC) THEN
        CALL UTDEBM('F','PROLCI','---')
        CALL UTIMPI('L',' LA TAILLE BLOC  :',1,ITBLOC)
        CALL UTIMPI('S','EST < 2*HAUTEUR_MAX :',1,2*HMAX)
        CALL UTIMPK('L','  CHANGEZ LA TAILLE_BLOC DES PROFILS:',0,' ')
        CALL UTIMPI('S',' PRENEZ AU MOINS :',1,2*HMAX/1024+1)
        CALL UTFINM()
      END IF
      NBLC = 1
      ZI(IDADIA) = ZI(IDHCOL)
      NTBLC = ZI(IDHCOL)
      DO 180 IEQUA = 2,NEQU
        NTBLC = NTBLC + ZI(IDHCOL+IEQUA-1)
        IF (NTBLC.LE.ITBLOC) THEN
          ZI(IDADIA+IEQUA-1) = ZI(IDADIA+IEQUA-2) + ZI(IDHCOL+IEQUA-1)
        ELSE
          NTBLC = ZI(IDHCOL+IEQUA-1)
          ZI(IDADIA+IEQUA-1) = ZI(IDHCOL+IEQUA-1)
          NBLC = NBLC + 1
        END IF
  180 CONTINUE
      IF (NIV.GE.1) WRITE (IFM,*) '--- NOMBRE DE BLOCS UTILISES '//
     &    'POUR LE STOCKAGE DU PROFIL SUP (OU INF) : ',NBLC

C---- CALCUL ET ALLOCATION DES ABLO

      CALL WKVECT(KABLO,BASE//' V I',NBLC+1,IDABLO)
      ZI(IDABLO) = 0
      IBLC = 1
      NTBLC = ZI(IDHCOL)
      DO 190 IEQUA = 2,NEQU
        NTBLC = NTBLC + ZI(IDHCOL+IEQUA-1)
        IF (NTBLC.GT.ITBLOC) THEN
          NTBLC = ZI(IDHCOL+IEQUA-1)
          ZI(IDABLO+IBLC) = IEQUA - 1
          IBLC = IBLC + 1
        END IF
  190 CONTINUE
      ZI(IDABLO+NBLC) = NEQU
      IF (NBLC.EQ.1) THEN
        ITBLOC = NTBLC
      END IF

C---- CREATION DE L'OBJET .IABL (POUR LES PERFS DE CHARGER):

      CALL WKVECT(KIABL,BASE//' V I',NEQU,IDIABL)
      ICOMPT = 0
      DO 210 I = 1,NBLC
        NBCOL = ZI(IDABLO+I) - ZI(IDABLO+I-1)
        DO 200 J = 1,NBCOL
          ICOMPT = ICOMPT + 1
          ZI(IDIABL-1+ICOMPT) = I
  200   CONTINUE
  210 CONTINUE
      IF (ICOMPT.NE. (NEQU)) THEN
        CALL UTMESS('F','PROLCI','ERREUR PGMEUR 1')
      END IF

C---- CREATION ET REMPLISSAGE DE DESC

      CALL WKVECT(KDESC,BASE//' V I',6,IDDESC)
      ZI(IDDESC) = NEQU
      ZI(IDDESC+1) = ITBLOC
      ZI(IDDESC+2) = NBLC
      ZI(IDDESC+3) = HMAX
      CALL JEDETR('&&PROLCI.NUMLOC')
  220 CONTINUE
      CALL JEDEMA()
      END
