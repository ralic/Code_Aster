      SUBROUTINE RECHNO(PHASE,IZONE,ESCL,REAAPP,REACTU,NEWGEO,DEFICO,
     &                  RESOCO,IESCL,CMULT)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 02/09/2002   AUTEUR BSERRE B.SERRE 
C TOLE CRP_20
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
      IMPLICIT NONE

      INTEGER IZONE,ESCL,REAAPP,REACTU,IESCL
      REAL*8 CMULT
      CHARACTER*6 PHASE
      CHARACTER*24 NEWGEO,DEFICO,RESOCO

C ----------------------------------------------------------------------
C ROUTINE APPELEE PAR : RECHCO / RECHME
C ----------------------------------------------------------------------

C APPARIEMENT NODAL : RECHERCHE POUR CHAQUE NOEUD "ESCLAVE"
C DU NOEUD "MAITRE" LE PLUS PROCHE DANS LA MEME SURFACE DE CONTACT,
C POUR UNE ZONE DE CONTACT DONNEE. LES NOTIONS DE "MAITRE" ET
C D'"ESCLAVE" SONT FICTIVES ICI : ON PREND COMME SURFACE ESCLAVE
C CELLE QUI A LE MOINS DE NOEUDS POUR AVOIR PLUS DE CHANCES D'AVOIR
C UN APPARIEMENT INJECTIF.

C APPARIEMENT MAITRE-ESCLAVE : CETTE ROUTINE EST APPELEE LORSQU'ON
C CHERCHE LA MAILLE LA PLUS PROCHE EN CHERCHANT PREALABLEMENT LE
C NOEUD LE PLUS PROCHE. LE NUMERO DE LA SURFACE ESCLAVE EST DONNE
C EN ARGUMENT (ESCL) : 2 TOUJOURS POUR 'MAIT_ESCL', 1 OU 2 POUR
C 'NODAL_SYME' OU 'MAIT_ESCL_SYME' (IL N'EST PAS MODIFIE ICI).

C DANS LES 2 CAS ON NE PREND PAS COMME NOEUDS ESCLAVES CEUX STOCKES
C DANS LE TABLEAU SANSNO ET PROVENANT DES MOTS-CLES SANS_NO ET
C SANS_GROUP_NO.

C IN  PHASE  : 'FINALE' SI CONTACT NODAL -> ON STOCKE LE JEU, LES
C              NUMEROS DE NOEUDS MAITRES, LES DDLS, LES COEFFICIENTS
C              ET ON INCREMENTE LE POINTEUR
C              'PRELIM' SINON : ON NE STOCKE QUE LE NOEUD LE + PROCHE
C IN  IZONE  : NUMERO DE LA ZONE DE CONTACT
C IN  ESCL   : NUMERO DE LA SURFACE ESCLAVE (1 OU 2, 0 SI INDETERMINE)
C IN  REAAPP : -1 SI PAS DE RECHERCHE MAIS REMPLISSAGE INITIAL DES SD
C              +1 SI ON CHERCHE LE NOEUD LE + PROCHE PAR "BRUTE FORCE"
C              +2 SI ON CHERCHE PAR VOISINAGE   (GRACE AU "PASSE")
C              +3 SI ON CHERCHE AVEC DES BOITES (SANS LE "PASSE")
C IN  REACTU : INDICATEUR DE REACTUALISATION POUR TOUTE LA ZONE
C              DES NORMALES ET DU JEU
C IN  NEWGEO : GEOMETRIE ACTUALISEE EN TENANT COMPTE DU CHAMP DE
C              DEPLACEMENTS COURANT
C IN  DEFICO : SD DE DEFINITION DU CONTACT (ISSUE D'AFFE_CHAR_MECA)
C VAR RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
C VAR IESCL  : NUMERO DU DERNIER NOEUD ESCLAVE CONNU
C IN  CMULT  : COEFFICIENT DE LA RELATION UNILATERALE (ISSU DU
C              MOT-CLE COEF_MULT_2 OU 1)

C -------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------------

      CHARACTER*32 JEXNUM,JEXNOM
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)

C ---------------- FIN DECLARATIONS NORMALISEES JEVEUX -----------------

      LOGICAL MULNOR
      INTEGER ISURF1,ISURF2,NBNO1,NBNO2,JDEC1,JDEC2,K1,K2,K
      INTEGER POSMIN,POSNO1,NUMNO1,POSNO2,NUMNO2,ISURF,IFM,NIV
      INTEGER OLDPOS,JDECMA,POSMA,NBMA,JDEC,NBNO,NUMMIN
      INTEGER JDIM,JZONE,JMACO,JNOCO,JSUNO,JMANO,JPOMA,JNOMA,JPONO
      INTEGER JNORM,JDDL,JCOOR,JAPPAR,JAPMEM,JAPNOR,JSANS,JPSANS
      INTEGER JPDDL,JCHAM,JTANG,JAPJFX,JMETH,JAPJFY,JAPTAN
      INTEGER JAPPTR,JAPCOE,JAPDDL,JAPJEU,NSANS,JAPCOF
      INTEGER NBDDL1,NBDDL2,JDECAL,JDECDL,NESMAX,NDIM,JTGDEF
      REAL*8 COOR1(3),COOR2(3),R8GAEM,DMIN,DIST,PADIST
      REAL*8 XNORM(3),XTANG(6),NORME
      CHARACTER*24 NDIMCO,PZONE,CONTMA,CONTNO,PSURNO,SANSNO,PSANS
      CHARACTER*24 MANOCO,PMANO,NOMACO,PNOMA,NORMCO,DDLCO,PDDL
      CHARACTER*24 CHAMCO,TANGCO,METHCO,TANDEF
      CHARACTER*24 APPARI,APPOIN,APMEMO,APJEFY,APTANG
      CHARACTER*24 APCOEF,APDDL,APNORM,APJEU,APJEFX,APCOFR

C ----------------------------------------------------------------------

      CALL JEMARQ()
      CALL INFNIV(IFM,NIV)

C ======================================================================
C               RECUPERATION D'ADRESSES ET DE DIMENSIONS
C ======================================================================

C --- LECTURE DES SD POUR LE CONTACT POTENTIEL

      NDIMCO = DEFICO(1:16)//'.NDIMCO'
      PZONE = DEFICO(1:16)//'.PZONECO'
      CONTMA = DEFICO(1:16)//'.MAILCO'
      CONTNO = DEFICO(1:16)//'.NOEUCO'
      PSURNO = DEFICO(1:16)//'.PSUNOCO'
      SANSNO = DEFICO(1:16)//'.SSNOCO'
      PSANS = DEFICO(1:16)//'.PSSNOCO'
      MANOCO = DEFICO(1:16)//'.MANOCO'
      PMANO = DEFICO(1:16)//'.PMANOCO'
      NOMACO = DEFICO(1:16)//'.NOMACO'
      PNOMA = DEFICO(1:16)//'.PNOMACO'
      NORMCO = DEFICO(1:16)//'.NORMCO'
      TANGCO = DEFICO(1:16)//'.TANGCO'
      CHAMCO = DEFICO(1:16)//'.CHAMCO'
      METHCO = DEFICO(1:16)//'.METHCO'
      TANDEF = DEFICO(1:16)//'.TANDEF'

      CALL JEVEUO(NDIMCO,'E',JDIM)
      CALL JEVEUO(PZONE,'L',JZONE)
      CALL JEVEUO(CONTMA,'L',JMACO)
      CALL JEVEUO(CONTNO,'L',JNOCO)
      CALL JEVEUO(PSURNO,'L',JSUNO)
      CALL JEVEUO(SANSNO,'L',JSANS)
      CALL JEVEUO(PSANS,'L',JPSANS)
      CALL JEVEUO(MANOCO,'L',JMANO)
      CALL JEVEUO(PMANO,'L',JPOMA)
      CALL JEVEUO(NOMACO,'L',JNOMA)
      CALL JEVEUO(PNOMA,'L',JPONO)
      CALL JEVEUO(NORMCO,'L',JNORM)
      CALL JEVEUO(TANGCO,'L',JTANG)
      CALL JEVEUO(CHAMCO,'L',JCHAM)
      CALL JEVEUO(METHCO,'L',JMETH)

      CALL JEVEUO(NEWGEO(1:19)//'.VALE','L',JCOOR)
      IF (ZI(JMETH+9* (IZONE-1)+2).EQ.1) THEN
        CALL JEVEUO(TANDEF,'L',JTGDEF)
        XTANG(1) = ZR(JTGDEF+3* (IZONE-1))
        XTANG(2) = ZR(JTGDEF+3* (IZONE-1)+1)
        XTANG(3) = ZR(JTGDEF+3* (IZONE-1)+2)
      END IF

C MULNOR : LOGIQUE QUI VAUT 1 LORSQU'ON MULTIPLIE LES COEFFICIENTS
C DE LA RELATION UNILATERALE PAR LES COMPOSANTES DES NORMALES
C (I.E. : ON EST DANS UNE ZONE OU LA RELATION UNILATERALE PORTE
C SUR LE DEPLACEMENT)

      MULNOR = (ABS(ZI(JCHAM+IZONE-1)).EQ.1)

C --- ADRESSES DES SD A LIRE ET / OU REMPLIR POUR LE CONTACT EFFECTIF

      APPARI = RESOCO(1:14)//'.APPARI'
      APMEMO = RESOCO(1:14)//'.APMEMO'

      CALL JEVEUO(APPARI,'E',JAPPAR)
      CALL JEVEUO(APMEMO,'E',JAPMEM)

      NDIM = ZI(JDIM)
      NESMAX = ZI(JDIM+8)

      IF (PHASE.EQ.'FINALE') THEN
        CHAMCO = DEFICO(1:16)//'.CHAMCO'
        DDLCO = DEFICO(1:16)//'.DDLCO'
        PDDL = DEFICO(1:16)//'.PDDLCO'
        APPOIN = RESOCO(1:14)//'.APPOIN'
        APCOEF = RESOCO(1:14)//'.APCOEF'
        APCOFR = RESOCO(1:14)//'.APCOFR'
        APDDL = RESOCO(1:14)//'.APDDL'
        APNORM = RESOCO(1:14)//'.APNORM'
        APTANG = RESOCO(1:14)//'.APTANG'
        APJEU = RESOCO(1:14)//'.APJEU'
        APJEFX = RESOCO(1:14)//'.APJEFX'
        APJEFY = RESOCO(1:14)//'.APJEFY'
        CALL JEVEUO(CHAMCO,'L',JCHAM)
        CALL JEVEUO(DDLCO,'L',JDDL)
        CALL JEVEUO(PDDL,'L',JPDDL)
        CALL JEVEUO(APPOIN,'E',JAPPTR)
        CALL JEVEUO(APCOEF,'E',JAPCOE)
        CALL JEVEUO(APCOFR,'E',JAPCOF)
        CALL JEVEUO(APDDL,'E',JAPDDL)
        CALL JEVEUO(APNORM,'E',JAPNOR)
        CALL JEVEUO(APTANG,'E',JAPTAN)
        CALL JEVEUO(APJEU,'E',JAPJEU)
        CALL JEVEUO(APJEFX,'E',JAPJFX)
        CALL JEVEUO(APJEFY,'E',JAPJFY)
      END IF

C ======================================================================
C REPERAGE DANS LES 2 SURFACES DE LA ZONE NUMERO IZONE
C ISURF1 : NUMERO DE LA SURFACE "ESCLAVE", ISURF2 : SURFACE "MAITRE"
C ======================================================================

C --- NUMEROS DES SURFACES

      IF (ESCL.EQ.1) THEN
        ISURF1 = ZI(JZONE+IZONE-1) + 1
        ISURF2 = ZI(JZONE+IZONE)
      ELSE IF ((ESCL.EQ.2) .OR. (ESCL.EQ.0)) THEN
        ISURF1 = ZI(JZONE+IZONE)
        ISURF2 = ZI(JZONE+IZONE-1) + 1
      END IF

C --- NOMBRE DE NOEUDS DES SURFACES : SI NBNO1 DIFF NBNO2, ON ALARME

      NBNO1 = ZI(JSUNO+ISURF1) - ZI(JSUNO+ISURF1-1)
      NBNO2 = ZI(JSUNO+ISURF2) - ZI(JSUNO+ISURF2-1)
      IF (NBNO1.NE.NBNO2) THEN
C        CALL UTMESS ('I','RECHNO_01','LE NOMBRE DE NOEUDS DES DEUX '
C     &               //'SURFACES EST DIFFERENT : ON SIGNALE QUE '
C     &               //'L''APPARIEMENT NE SERA PAS BIJECTIF')
      END IF

C --- SI SURFACE ESCLAVE INDIFFERENTE, ON SE DEBROUILLE POUR QUE ISURF1
C --- (SURFACE "ESCLAVE") SOIT CELLE AVEC LE MOINS DE NOEUDS

      IF ((ESCL.EQ.0) .AND. (NBNO2.LT.NBNO1)) THEN
        NBNO = NBNO1
        NBNO1 = NBNO2
        NBNO2 = NBNO
        ISURF = ISURF1
        ISURF1 = ISURF2
        ISURF2 = ISURF
      END IF

C --- DECALAGE DANS CONTNO POUR TROUVER LES NOEUDS DES SURFACES

      JDEC1 = ZI(JSUNO+ISURF1-1)
      JDEC2 = ZI(JSUNO+ISURF2-1)

C --- NOMBRE DE NOEUDS ESCLAVES DE LA ZONE A PRIORI

      ZI(JDIM+8+IZONE) = NBNO1

C ======================================================================
C APPARIEMENT PAR METHODE "BRUTE FORCE" : DOUBLE BOUCLE SUR LES NOEUDS
C ======================================================================

      IF ((REAAPP.EQ.1) .OR. (REAAPP.EQ.-1)) THEN

        DO 50 K1 = 1,NBNO1

C --- INDICE DANS CONTNO, NUMERO ABSOLU DU NOEUD DE LA 1ERE SURFACE
C --- ET COORDONNEES ACTUELLES

          POSNO1 = JDEC1 + K1
          NUMNO1 = ZI(JNOCO+POSNO1-1)
          COOR1(1) = ZR(JCOOR+3* (NUMNO1-1))
          COOR1(2) = ZR(JCOOR+3* (NUMNO1-1)+1)
          COOR1(3) = ZR(JCOOR+3* (NUMNO1-1)+2)
          DMIN = R8GAEM()

C --- ON REGARDE SI LE NOEUD EST INTERDIT COMME ESCLAVE

          NSANS = ZI(JPSANS+IZONE) - ZI(JPSANS+IZONE-1)
          JDEC = ZI(JPSANS+IZONE-1)
          DO 10 K = 1,NSANS
            IF (NUMNO1.EQ.ZI(JSANS+JDEC+K-1)) THEN
              ZI(JDIM+8+IZONE) = ZI(JDIM+8+IZONE) - 1
              GO TO 50
            END IF
   10     CONTINUE

C --- RECHERCHE DU NOEUD LE PLUS PROCHE DANS LA 2EME SURFACE
C --- SI PAS D'APPARIEMENT (REAAPP = -1) ON NE LE FAIT PAS

          IF (REAAPP.EQ.-1) THEN
            POSMIN = 0
            DMIN = 0.D0
            NUMMIN = 0
          ELSE
            DO 20 K2 = 1,NBNO2
              POSNO2 = JDEC2 + K2
              NUMNO2 = ZI(JNOCO+POSNO2-1)
              ZI(JAPMEM+4* (POSNO2-1)) = 0
              COOR2(1) = ZR(JCOOR+3* (NUMNO2-1))
              COOR2(2) = ZR(JCOOR+3* (NUMNO2-1)+1)
              COOR2(3) = ZR(JCOOR+3* (NUMNO2-1)+2)
              DIST = PADIST(3,COOR1,COOR2)
              IF (DIST.LT.DMIN) THEN
                POSMIN = POSNO2
                DMIN = DIST
                NUMMIN = NUMNO2
              END IF
   20       CONTINUE
          END IF

C --- STOCKAGE DANS APPARI ET APMEMO.
C --- SI L'APPARIEMENT EST NODAL, ON STOCKE LES NOEUDS, LES DDLS,
C --- LES COEFFICIENTS, LA DIRECTION DE PROJECTION ET LE JEU.
C --- NB : ON SOUSTRAIT AU VRAI JEU LA VALEUR DU JEU FICTIF JEUSUP
C --- RANGE DANS APJEU AUPARAVANT.

          IESCL = IESCL + 1

          ZI(JAPPAR+3* (IESCL-1)+1) = POSNO1
          ZI(JAPPAR+3* (IESCL-1)+2) = -POSMIN
          ZI(JAPPAR+3* (IESCL-1)+3) = REACTU
          

          ZI(JAPMEM+4* (POSNO1-1)) = 1
          ZI(JAPMEM+4* (POSNO1-1)+1) = POSMIN
          ZI(JAPMEM+4* (POSNO1-1)+2) = 0
          ZI(JAPMEM+4* (POSNO1-1)+3) = 0

          IF (POSMIN.NE.0) ZI(JAPMEM+4* (POSMIN-1)) = 0

          IF (PHASE.EQ.'FINALE') THEN
            NBDDL1 = ZI(JPDDL+POSNO1) - ZI(JPDDL+POSNO1-1)
            NBDDL2 = 0
            IF (REAAPP.GT.0) THEN
              NBDDL2 = ZI(JPDDL+POSMIN) - ZI(JPDDL+POSMIN-1)
            END IF
            IF ((NBDDL1.GT.3) .OR. (NBDDL2.GT.3)) THEN
              CALL UTMESS('F','RECHNO_01','ON NE PEUT PAS AVOIR PLUS'//
     &                    ' DE 3 DDLS IMPLIQUES DANS LA MEME RELATION'//
     &                    ' UNILATERALE')
            END IF
            ZI(JAPPAR+3* (IESCL-1)+3) = 0
            IF (ZI(JMETH+8).EQ.0) THEN
              ZR(JAPNOR+3* (IESCL-1)) = ZR(JNORM+3* (POSNO1-1))
              ZR(JAPNOR+3* (IESCL-1)+1) = ZR(JNORM+3* (POSNO1-1)+1)
              ZR(JAPNOR+3* (IESCL-1)+2) = ZR(JNORM+3* (POSNO1-1)+2)
              IF (ZI(JMETH+6).GE.2) THEN
                ZR(JAPTAN+6* (IESCL-1)) = ZR(JTANG+6* (POSNO1-1))
                ZR(JAPTAN+6* (IESCL-1)+1) = ZR(JTANG+6* (POSNO1-1)+1)
                ZR(JAPTAN+6* (IESCL-1)+2) = ZR(JTANG+6* (POSNO1-1)+2)
                IF (NDIM.EQ.3) THEN
                  ZR(JAPTAN+6* (IESCL-1)+3) = ZR(JTANG+6* (POSNO1-1)+3)
                  ZR(JAPTAN+6* (IESCL-1)+4) = ZR(JTANG+6* (POSNO1-1)+4)
                  ZR(JAPTAN+6* (IESCL-1)+5) = ZR(JTANG+6* (POSNO1-1)+5)
                END IF
              END IF
            ELSE
              XNORM(1) = (ZR(JNORM+3* (POSNO1-1))-
     &                   ZR(JNORM+3* (POSMIN-1)))/2
              XNORM(2) = (ZR(JNORM+3* (POSNO1-1)+1)-
     &                   ZR(JNORM+3* (POSMIN-1)+1))/2
              XNORM(3) = (ZR(JNORM+3* (POSNO1-1)+2)-
     &                   ZR(JNORM+3* (POSMIN-1)+2))/2
              CALL NORMEV(XNORM,NORME)
              CALL CATANG(NDIM,XNORM,XTANG,ZI(JMETH+9* (IZONE-1)+2))
              CALL R8COPY(6,XTANG,1,ZR(JTANG+6* (POSNO1-1)),1)
              ZR(JAPNOR+3* (IESCL-1)) = XNORM(1)
              ZR(JAPNOR+3* (IESCL-1)+1) = XNORM(2)
              ZR(JAPNOR+3* (IESCL-1)+2) = XNORM(3)
              IF (ZI(JMETH+6).GE.2) THEN
                ZR(JAPTAN+6* (IESCL-1)) = XTANG(1)
                ZR(JAPTAN+6* (IESCL-1)+1) = XTANG(2)
                ZR(JAPTAN+6* (IESCL-1)+2) = XTANG(3)
                IF (NDIM.EQ.3) THEN
                  ZR(JAPTAN+6* (IESCL-1)+3) = XTANG(4)
                  ZR(JAPTAN+6* (IESCL-1)+4) = XTANG(5)
                  ZR(JAPTAN+6* (IESCL-1)+5) = XTANG(6)
                END IF
              END IF
            END IF
            IF (REAAPP.GT.0) THEN
              ZR(JAPJEU+IESCL-1) = (ZR(JCOOR+3* (NUMMIN-1))-COOR1(1))*
     &                             ZR(JAPNOR+3* (IESCL-1)) +
     &                             (ZR(JCOOR+3* (NUMMIN-1)+1)-COOR1(2))*
     &                             ZR(JAPNOR+3* (IESCL-1)+1) +
     &                             (ZR(JCOOR+3* (NUMMIN-1)+2)-COOR1(3))*
     &                             ZR(JAPNOR+3* (IESCL-1)+2)
              IF (ZI(JMETH+6).GE.3) THEN
                ZR(JAPJFX+IESCL-1) = 0
                IF (NDIM.EQ.3) ZR(JAPJFY+IESCL-1) = 0
              END IF
            ELSE
              ZR(JAPJEU+IESCL-1) = 0.D0
            END IF
            JDECAL = ZI(JAPPTR+IESCL-1)
            JDECDL = ZI(JPDDL+POSNO1-1)
            DO 30 K = 1,NBDDL1
              ZR(JAPCOE+JDECAL+K-1) = 1.D0*CMULT
              IF (MULNOR) THEN
                ZR(JAPCOE+JDECAL+K-1) = ZR(JAPCOE+JDECAL+K-1)*
     &                                  ZR(JAPNOR+3* (IESCL-1)+K-1)
              END IF
              IF (ZI(JMETH+6).GE.2) THEN
                ZR(JAPCOF+JDECAL+K-1) = 1.D0*CMULT
                ZR(JAPCOF+JDECAL+30*NESMAX+K-1) = 1.D0*CMULT
                IF (MULNOR) THEN
                  ZR(JAPCOF+JDECAL+K-1) = ZR(JAPCOF+JDECAL+K-1)*
     &                                    ZR(JAPTAN+6* (IESCL-1)+K-1)
                  IF (NDIM.EQ.3) THEN
                    ZR(JAPCOF+JDECAL+30*NESMAX+K-1) = ZR(JAPCOF+JDECAL+
     &                30*NESMAX+K-1)*ZR(JAPTAN+6* (IESCL-1)+K+2)
                  END IF
                END IF
              END IF
              ZI(JAPDDL+JDECAL+K-1) = ZI(JDDL+JDECDL+K-1)
   30       CONTINUE
            IF (REAAPP.GT.0) THEN
              JDECAL = JDECAL + NBDDL1
              JDECDL = ZI(JPDDL+POSMIN-1)
              DO 40 K = 1,NBDDL2
                ZR(JAPCOE+JDECAL+K-1) = -1.D0*CMULT
                IF (MULNOR) THEN
                  ZR(JAPCOE+JDECAL+K-1) = ZR(JAPCOE+JDECAL+K-1)*
     &                                    ZR(JAPNOR+3* (IESCL-1)+K-1)
                END IF
                IF (ZI(JMETH+6).GE.2) THEN
                  ZR(JAPCOF+JDECAL+K-1) = -1.D0*CMULT
                  ZR(JAPCOF+JDECAL+30*NESMAX+K-1) = -1.D0*CMULT
                  IF (MULNOR) THEN
                    ZR(JAPCOF+JDECAL+K-1) = ZR(JAPCOF+JDECAL+K-1)*
     &                                      ZR(JAPTAN+6* (IESCL-1)+K-1)
                    IF (NDIM.EQ.3) THEN
                      ZR(JAPCOF+JDECAL+30*NESMAX+K-1) = ZR(JAPCOF+
     &                  JDECAL+30*NESMAX+K-1)*ZR(JAPTAN+6* (IESCL-1)+K+
     &                  2)
                    END IF
                  END IF
                END IF
                ZI(JAPDDL+JDECAL+K-1) = ZI(JDDL+JDECDL+K-1)
   40         CONTINUE
            END IF
            ZI(JAPPTR+IESCL) = ZI(JAPPTR+IESCL-1) + NBDDL1 + NBDDL2
          END IF
   50   CONTINUE

C ======================================================================
C APPARIEMENT PAR VOISINAGE : ON CHERCHE UN NOEUD CONNECTE A L'ANCIEN
C NOEUD LE PLUS PROCHE PAR UNE MAILLE DE CONTACT
C ======================================================================

      ELSE IF (REAAPP.EQ.2) THEN

        DO 110 K1 = 1,NBNO1

C --- INDICE DANS CONTNO, NUMERO ABSOLU DU NOEUD DE LA 1ERE SURFACE
C --- ET COORDONNEES ACTUELLES

          POSNO1 = JDEC1 + K1
          NUMNO1 = ZI(JNOCO+POSNO1-1)
          COOR1(1) = ZR(JCOOR+3* (NUMNO1-1))
          COOR1(2) = ZR(JCOOR+3* (NUMNO1-1)+1)
          COOR1(3) = ZR(JCOOR+3* (NUMNO1-1)+2)
          DMIN = R8GAEM()

C --- ON REGARDE SI LE NOEUD EST INTERDIT COMME ESCLAVE

          NSANS = ZI(JPSANS+IZONE) - ZI(JPSANS+IZONE-1)
          JDEC = ZI(JPSANS+IZONE-1)
          DO 60 K = 1,NSANS
            IF (NUMNO1.EQ.ZI(JSANS+JDEC+K-1)) THEN
              ZI(JDIM+8+IZONE) = ZI(JDIM+8+IZONE) - 1
              GO TO 110
            END IF
   60     CONTINUE

C --- ANCIEN NOEUD MAITRE LE PLUS PROCHE

          OLDPOS = ZI(JAPMEM+4* (POSNO1-1)+1)

C --- BOUCLE SUR LES MAILLES CONTENANT CET ANCIEN NOEUD

          JDECMA = ZI(JPOMA+OLDPOS-1)
          NBMA = ZI(JPOMA+OLDPOS) - ZI(JPOMA+OLDPOS-1)

          DO 80 K2 = 1,NBMA

            POSMA = ZI(JMANO+JDECMA+K2-1)

C - BOUCLE SUR LES NOEUDS DE LA MAILLE ET CALCUL DE LA DISTANCE

            JDEC = ZI(JPONO+POSMA-1)
            NBNO = ZI(JPONO+POSMA) - ZI(JPONO+POSMA-1)
            DO 70 K = 1,NBNO
              POSNO2 = ZI(JNOMA+JDEC+K-1)
              NUMNO2 = ZI(JNOCO+POSNO2-1)
              ZI(JAPMEM+4* (POSNO2-1)) = 0
              COOR2(1) = ZR(JCOOR+3* (NUMNO2-1))
              COOR2(2) = ZR(JCOOR+3* (NUMNO2-1)+1)
              COOR2(3) = ZR(JCOOR+3* (NUMNO2-1)+2)
              DIST = PADIST(3,COOR1,COOR2)
              IF (DIST.LT.DMIN) THEN
                POSMIN = POSNO2
                DMIN = DIST
                NUMMIN = NUMNO2
              END IF
   70       CONTINUE

   80     CONTINUE

C --- STOCKAGE DANS APPARI ET APMEMO.
C --- SI L'APPARIEMENT EST NODAL, ON STOCKE LES NOEUDS, LES DDLS,
C --- LES COEFFICIENTS, LA DIRECTION DE PROJECTION ET LE JEU.
C --- NB : ON SOUSTRAIT AU VRAI JEU LA VALEUR DU JEU FICTIF JEUSUP
C --- RANGE DANS APJEU AUPARAVANT.

          IESCL = IESCL + 1

          ZI(JAPPAR+3* (IESCL-1)+1) = POSNO1
          ZI(JAPPAR+3* (IESCL-1)+2) = -POSMIN
          ZI(JAPPAR+3* (IESCL-1)+2) = 0

          ZI(JAPPAR+3* (IESCL-1)+3) = REACTU

          ZI(JAPMEM+4* (POSNO1-1)) = 1
          ZI(JAPMEM+4* (POSNO1-1)+1) = POSMIN
          ZI(JAPMEM+4* (POSNO1-1)+2) = 0
          ZI(JAPMEM+4* (POSNO1-1)+3) = 0

          ZI(JAPMEM+4* (POSMIN-1)) = 0

          IF (PHASE.EQ.'FINALE') THEN
            NBDDL1 = ZI(JPDDL+POSNO1) - ZI(JPDDL+POSNO1-1)
            NBDDL2 = ZI(JPDDL+POSMIN) - ZI(JPDDL+POSMIN-1)
            IF ((NBDDL1.GT.3) .OR. (NBDDL2.GT.3)) THEN
              CALL UTMESS('F','RECHNO_02','ON NE PEUT PAS AVOIR PLUS'//
     &                    ' DE 3 DDLS IMPLIQUES DANS LA MEME RELATION'//
     &                    ' UNILATERALE')
            END IF
            ZI(JAPPAR+3* (IESCL-1)+3) = 0
            IF (ZI(JMETH+8).EQ.0) THEN
              ZR(JAPNOR+3* (IESCL-1)) = ZR(JNORM+3* (POSNO1-1))
              ZR(JAPNOR+3* (IESCL-1)+1) = ZR(JNORM+3* (POSNO1-1)+1)
              ZR(JAPNOR+3* (IESCL-1)+2) = ZR(JNORM+3* (POSNO1-1)+2)
              IF (ZI(JMETH+6).GE.2) THEN
                ZR(JAPTAN+6* (IESCL-1)) = ZR(JTANG+6* (POSNO1-1))
                ZR(JAPTAN+6* (IESCL-1)+1) = ZR(JTANG+6* (POSNO1-1)+1)
                ZR(JAPTAN+6* (IESCL-1)+2) = ZR(JTANG+6* (POSNO1-1)+2)
                IF (NDIM.EQ.3) THEN
                  ZR(JAPTAN+6* (IESCL-1)+3) = ZR(JTANG+6* (POSNO1-1)+3)
                  ZR(JAPTAN+6* (IESCL-1)+4) = ZR(JTANG+6* (POSNO1-1)+4)
                  ZR(JAPTAN+6* (IESCL-1)+5) = ZR(JTANG+6* (POSNO1-1)+5)
                END IF
              END IF
            ELSE
              XNORM(1) = (ZR(JNORM+3* (POSNO1-1))-
     &                   ZR(JNORM+3* (POSMIN-1)))/2
              XNORM(2) = (ZR(JNORM+3* (POSNO1-1)+1)-
     &                   ZR(JNORM+3* (POSMIN-1)+1))/2
              XNORM(3) = (ZR(JNORM+3* (POSNO1-1)+2)-
     &                   ZR(JNORM+3* (POSMIN-1)+2))/2
              CALL NORMEV(XNORM,NORME)
              CALL CATANG(NDIM,XNORM,XTANG,ZI(JMETH+9* (IZONE-1)+2))
              CALL R8COPY(6,XTANG,1,ZR(JTANG+6* (POSNO1-1)),1)
              ZR(JAPNOR+3* (IESCL-1)) = XNORM(1)
              ZR(JAPNOR+3* (IESCL-1)+1) = XNORM(2)
              ZR(JAPNOR+3* (IESCL-1)+2) = XNORM(3)
              IF (ZI(JMETH+6).GE.2) THEN
                ZR(JAPTAN+6* (IESCL-1)) = XTANG(1)
                ZR(JAPTAN+6* (IESCL-1)+1) = XTANG(2)
                ZR(JAPTAN+6* (IESCL-1)+2) = XTANG(3)
                IF (NDIM.EQ.3) THEN
                  ZR(JAPTAN+6* (IESCL-1)+3) = XTANG(4)
                  ZR(JAPTAN+6* (IESCL-1)+4) = XTANG(5)
                  ZR(JAPTAN+6* (IESCL-1)+5) = XTANG(6)
                END IF
              END IF
            END IF
            ZR(JAPJEU+IESCL-1) = (ZR(JCOOR+3* (NUMMIN-1))-COOR1(1))*
     &                           ZR(JAPNOR+3* (IESCL-1)) +
     &                           (ZR(JCOOR+3* (NUMMIN-1)+1)-COOR1(2))*
     &                           ZR(JAPNOR+3* (IESCL-1)+1) +
     &                           (ZR(JCOOR+3* (NUMMIN-1)+2)-COOR1(3))*
     &                           ZR(JAPNOR+3* (IESCL-1)+2)
            IF (ZI(JMETH+6).GE.3) THEN
              ZR(JAPJFX+IESCL-1) = 0
              IF (NDIM.EQ.3) ZR(JAPJFY+IESCL-1) = 0
            END IF
            JDECAL = ZI(JAPPTR+IESCL-1)
            JDECDL = ZI(JPDDL+POSNO1-1)
            DO 90 K = 1,NBDDL1
              ZR(JAPCOE+JDECAL+K-1) = 1.D0*CMULT
              IF (MULNOR) THEN
                ZR(JAPCOE+JDECAL+K-1) = ZR(JAPCOE+JDECAL+K-1)*
     &                                  ZR(JAPNOR+3* (IESCL-1)+K-1)
              END IF
              IF (ZI(JMETH+6).GE.2) THEN
                ZR(JAPCOF+JDECAL+K-1) = 1.D0*CMULT
                ZR(JAPCOF+JDECAL+30*NESMAX+K-1) = 1.D0*CMULT
                IF (MULNOR) THEN
                  ZR(JAPCOF+JDECAL+K-1) = ZR(JAPCOF+JDECAL+K-1)*
     &                                    ZR(JAPTAN+6* (IESCL-1)+K-1)
                  IF (NDIM.EQ.3) THEN
                    ZR(JAPCOF+JDECAL+30*NESMAX+K-1) = ZR(JAPCOF+JDECAL+
     &                30*NESMAX+K-1)*ZR(JAPTAN+6* (IESCL-1)+K+2)
                  END IF
                END IF
              END IF
              ZI(JAPDDL+JDECAL+K-1) = ZI(JDDL+JDECDL+K-1)
   90       CONTINUE
            JDECAL = JDECAL + NBDDL1
            JDECDL = ZI(JPDDL+POSMIN-1)
            DO 100 K = 1,NBDDL2
              ZR(JAPCOE+JDECAL+K-1) = -1.D0*CMULT
              IF (MULNOR) THEN
                ZR(JAPCOE+JDECAL+K-1) = ZR(JAPCOE+JDECAL+K-1)*
     &                                  ZR(JAPNOR+3* (IESCL-1)+K-1)
              END IF
              IF (ZI(JMETH+6).GE.2) THEN
                ZR(JAPCOF+JDECAL+K-1) = -1.D0*CMULT
                ZR(JAPCOF+JDECAL+30*NESMAX+K-1) = -1.D0*CMULT
                IF (MULNOR) THEN
                  ZR(JAPCOF+JDECAL+K-1) = ZR(JAPCOF+JDECAL+K-1)*
     &                                    ZR(JAPTAN+6* (IESCL-1)+K-1)
                  IF (NDIM.EQ.3) THEN
                    ZR(JAPCOF+JDECAL+30*NESMAX+K-1) = ZR(JAPCOF+JDECAL+
     &                30*NESMAX+K-1)*ZR(JAPTAN+6* (IESCL-1)+K+2)
                  END IF
                END IF
              END IF
              ZI(JAPDDL+JDECAL+K-1) = ZI(JDDL+JDECDL+K-1)
  100       CONTINUE
            ZI(JAPPTR+IESCL) = ZI(JAPPTR+IESCL-1) + NBDDL1 + NBDDL2
          END IF

  110   CONTINUE

C ======================================================================
C APPARIEMENT PAR BOITES : ON PARCOURT L'ENSEMBLE DES NOEUDS APPARTENANT
C A DES BOITES VOISINES DU NOEUD ESCLAVE CONSIDERE
C ======================================================================

      ELSE IF (REAAPP.EQ.3) THEN

C        CALL BOITNO (NEWGEO,CONTNO,APBOIT,IESCL)
C        CALL CHNOBO (NEWGEO,NDIMCO,PZONE,CONTNO,PSURNO,APBOIT,APPARI,
C    &                IESCL)

      END IF

C ----------------------------------------------------------------------

      CALL JEDEMA()
      END
