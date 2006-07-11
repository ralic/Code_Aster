      SUBROUTINE EXNOEL(CHAR,NOMA,MOTCLE,NGR,CALCMA,NBMA,NBNO,NBNOQU,
     &                  LISTMA,LISTNO,LISTQU,IPMA,IPNO,IPQU)

C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 11/07/2006   AUTEUR PABHHHH N.TARDIEU 
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

C  IN  <-
      CHARACTER*8 CHAR
      CHARACTER*8 NOMA
      CHARACTER*8 MOTCLE
      INTEGER NGR
      CHARACTER*8 CALCMA(*)
      INTEGER NBMA
      INTEGER NBNO
      INTEGER NBNOQU
C  IO  <->
      INTEGER LISTMA(NBMA)
      INTEGER LISTNO(NBNO)
      INTEGER LISTQU(3*NBNOQU)
      INTEGER IPMA
      INTEGER IPNO
      INTEGER IPQU

C ----------------------------------------------------------------------
C ROUTINE APPELEE PAR : EXNOCO
C ----------------------------------------------------------------------
C
C EXTRACTION DES NOEUDS ET DES NOEUDS QUADRATIQUES DE GROUPES DE MAILLE
C OU DE MAILLES
C
C IN  CHAR   : NOM UTILISATEUR DU CONCEPT DE CHARGE
C IN  NOMA   : NOM DU MAILLAGE
C IN  MOTCLE : MOT-CLE (GROUP_MA ou MAILLE)
C IN  NGR    : NOMBRE DE GROUPES
C IN  CALCMA : VECTEUR DE CHARACTER*8 DE TRAVAIL
C -- LES TROIS VALEURS SUIVANTES (NBMA, NBNO,NBNOQU) ONT ETE DETERMINEES
C -- VIA LA ROUTINE NBNOEL
C IN  NBMA   : NOMBRE DE MAILLES
C              / POUR TAILLE DE LA LISTE LISTMA
C              / UTILISE LORSQUE MOTCLE = "MAILLE"
C IN  NBNO   : NOMBRE DE NOEUDS
C              / POUR TAILLE DE LA LISTE LISTNO
C IN  NBNOQU : NOMBRE DE NOEUDS QUADRATIQUES
C              / POUR TAILLE DE LA LISTE LISTQU
C I/O LISTMA : LISTE DES NUMEROS DES MAILLES DE CONTACT
C I/O LISTNO : LISTE DES NUMEROS DES NOEUDS DE CONTACT
C I/O LISTQU : LISTE DES NUMEROS DES NOEUDS QUADRATIQUES DE CONTACT
C I/O IPMA   : INDICE POUR LA LISTE DES NUMEROS DES MAILLES DE CONTACT
C I/O IPNO   : INDICE POUR LA LISTE DES NUMEROS DES NOEUDS DE CONTACT
C I/O IPQU   : INDICE POUR LA LISTE DES NUMEROS DES NOEUDS QUADRATIQUES
C              DE CONTACT
C
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


      CHARACTER*1 K1BID
      CHARACTER*8 NOMOB,NOMTM
      CHARACTER*16 NOMTE
      CHARACTER*19 NOLIG
      CHARACTER*24 MAILMA,GRMAMA
      INTEGER IER,IRET
      INTEGER II1,II2,II3,INO
      INTEGER NBMAIL,N1,NBNOMI,NBGREL,NEL
      INTEGER NUTYP,NUMAIL,NUMAI2
      INTEGER NOEUSO,NOEUMI
      INTEGER JGRO,JDES,IBID
      INTEGER IATYMA,IALIEL,ITYPEL,IGREL,ITYP

C ----------------------------------------------------------------------

      CALL JEMARQ()

      MAILMA = NOMA//'.NOMMAI'
      GRMAMA = NOMA//'.GROUPEMA'
      CALL JEVEUO(NOMA//'.TYPMAIL','L',IATYMA)

      CALL DISMOI('F','NOM_MODELE',CHAR(1:8),'CHARGE',IBID,NOMOB,IER)
      NOLIG = NOMOB(1:8)//'.MODELE'
      CALL JEEXIN(NOLIG//'.LIEL',IRET)

C --- GROUPE DE MAILLES

      IF (MOTCLE.EQ.'GROUP_MA') THEN
        DO 80 II1 = 1,NGR
          CALL JEVEUO(JEXNOM(GRMAMA,CALCMA(II1)),'L',JGRO)
          CALL JELIRA(JEXNOM(GRMAMA,CALCMA(II1)),'LONMAX',NBMAIL,K1BID)
          DO 70 II2 = 1,NBMAIL
            IPMA = IPMA + 1
            NUMAIL = ZI(JGRO-1+II2)
            ITYP = IATYMA - 1 + NUMAIL
            NUTYP = ZI(ITYP)
            CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',NUTYP),NOMTM)
            LISTMA(IPMA) = NUMAIL
            CALL JEVEUO(JEXNUM(NOMA//'.CONNEX',NUMAIL),'L',JDES)
            CALL JELIRA(JEXNUM(NOMA//'.CONNEX',NUMAIL),'LONMAX',N1,
     &                  K1BID)
            IF (NBNOQU.EQ.0) THEN
              NOEUSO = N1

            ELSE IF (NOMTM(1:5).EQ.'QUAD8') THEN
              NOEUMI = NBNOMI(NOMTM(1:5))
              NOEUSO = N1 - NOEUMI
              DO 10 INO = 1,NOEUMI - 1
                IPQU = IPQU + 1
                LISTQU((IPQU-1)*3+1) = ZI(JDES+ (NOEUSO+INO)-1)
                LISTQU((IPQU-1)*3+2) = ZI(JDES+INO-1)
                LISTQU((IPQU-1)*3+3) = ZI(JDES+ (INO+1)-1)
   10         CONTINUE
              IPQU = IPQU + 1
              LISTQU((IPQU-1)*3+1) = ZI(JDES+N1-1)
              LISTQU((IPQU-1)*3+2) = ZI(JDES+NOEUMI-1)
              LISTQU((IPQU-1)*3+3) = ZI(JDES+1-1)

            ELSE IF (NOMTM(1:5).EQ.'QUAD9') THEN
              IF (IRET.EQ.0) GO TO 50
              CALL JELIRA(NOLIG//'.LIEL','NUTIOC',NBGREL,K1BID)
              DO 40,IGREL = 1,NBGREL
                CALL JEVEUO(JEXNUM(NOLIG//'.LIEL',IGREL),'L',IALIEL)
                CALL JELIRA(JEXNUM(NOLIG//'.LIEL',IGREL),'LONMAX',NEL,
     &                      K1BID)
                ITYPEL = ZI(IALIEL-1+NEL)
                CALL JENUNO(JEXNUM('&CATA.TE.NOMTE',ITYPEL),NOMTE)
                IF (NOMTE.EQ.'MEC3QU9H') THEN
                  DO 30 II3 = 1,NEL - 1
                    NUMAI2 = ZI(IALIEL-1+II3)
                    IF (NUMAI2.EQ.NUMAIL) THEN
                      NOEUMI = NBNOMI(NOMTM(1:5))
                      NOEUSO = N1 - NOEUMI
                      DO 20 INO = 1,NOEUMI - 2
                        IPQU = IPQU + 1
                        LISTQU((IPQU-1)*3+1) = ZI(JDES+ (NOEUSO+INO)-1)
                        LISTQU((IPQU-1)*3+2) = ZI(JDES+INO-1)
                        LISTQU((IPQU-1)*3+3) = ZI(JDES+ (INO+1)-1)
   20                 CONTINUE
                      IPQU = IPQU + 1
                      LISTQU((IPQU-1)*3+1) = ZI(JDES+N1-2)
                      LISTQU((IPQU-1)*3+2) = ZI(JDES+NOEUMI-2)
                      LISTQU((IPQU-1)*3+3) = ZI(JDES+1-1)
                      GO TO 50

                    END IF
   30             CONTINUE

                ELSE IF (NOMTE.EQ.'MECA_FACE9') THEN
                  NOEUMI = 0
                  NOEUSO = N1
                END IF
   40         CONTINUE
   50         CONTINUE

            ELSE
              NOEUSO = N1
            END IF
            DO 60 INO = 1,NOEUSO
              IPNO = IPNO + 1
              LISTNO(IPNO) = ZI(JDES+INO-1)
   60       CONTINUE
   70     CONTINUE
   80   CONTINUE

C --- MAILLES

      ELSE IF (MOTCLE.EQ.'MAILLE') THEN
C              ------------------
        NBMAIL = NBMA
        DO 150 II1 = 1,NBMAIL
          IPMA = IPMA + 1
          CALL JENONU(JEXNOM(MAILMA,CALCMA(II1)),NUMAIL)
          CALL JEVEUO(JEXNUM(NOMA//'.CONNEX',NUMAIL),'L',JDES)
          CALL JELIRA(JEXNUM(NOMA//'.CONNEX',NUMAIL),'LONMAX',N1,K1BID)
          LISTMA(IPMA) = NUMAIL
          ITYP = IATYMA - 1 + NUMAIL
          NUTYP = ZI(ITYP)
          CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',NUTYP),NOMTM)
          IF (NBNOQU.EQ.0) THEN
            NOEUSO = N1

          ELSE IF (NOMTM(1:5).EQ.'QUAD8') THEN
            NOEUMI = NBNOMI(NOMTM(1:5))
            NOEUSO = N1 - NOEUMI
            DO 90 INO = 1,NOEUMI - 1
              IPQU = IPQU + 1
              LISTQU((IPQU-1)*3+1) = ZI(JDES+ (NOEUSO+INO)-1)
              LISTQU((IPQU-1)*3+2) = ZI(JDES+INO-1)
              LISTQU((IPQU-1)*3+3) = ZI(JDES+ (INO+1)-1)
   90       CONTINUE
            IPQU = IPQU + 1
            LISTQU((IPQU-1)*3+1) = ZI(JDES+N1-1)
            LISTQU((IPQU-1)*3+2) = ZI(JDES+NOEUMI-1)
            LISTQU((IPQU-1)*3+3) = ZI(JDES+1-1)

          ELSE IF (NOMTM(1:5).EQ.'QUAD9') THEN
            IF (IRET.EQ.0) GO TO 130
            CALL JELIRA(NOLIG//'.LIEL','NUTIOC',NBGREL,K1BID)
            DO 120,IGREL = 1,NBGREL
              CALL JEVEUO(JEXNUM(NOLIG//'.LIEL',IGREL),'L',IALIEL)
              CALL JELIRA(JEXNUM(NOLIG//'.LIEL',IGREL),'LONMAX',NEL,
     &                    K1BID)
              ITYPEL = ZI(IALIEL-1+NEL)
              CALL JENUNO(JEXNUM('&CATA.TE.NOMTE',ITYPEL),NOMTE)
              IF (NOMTE.EQ.'MEC3QU9H') THEN
                DO 110 II3 = 1,NEL - 1
                  NUMAI2 = ZI(IALIEL-1+II3)
                  IF (NUMAI2.EQ.NUMAIL) THEN
                    NOEUMI = NBNOMI(NOMTM(1:5))
                    NOEUSO = N1 - NOEUMI
                    DO 100 INO = 1,NOEUMI - 2
                      IPQU = IPQU + 1
                      LISTQU((IPQU-1)*3+1) = ZI(JDES+ (NOEUSO+INO)-1)
                      LISTQU((IPQU-1)*3+2) = ZI(JDES+INO-1)
                      LISTQU((IPQU-1)*3+3) = ZI(JDES+ (INO+1)-1)
  100               CONTINUE
                    IPQU = IPQU + 1
                    LISTQU((IPQU-1)*3+1) = ZI(JDES+N1-2)
                    LISTQU((IPQU-1)*3+2) = ZI(JDES+NOEUMI-2)
                    LISTQU((IPQU-1)*3+3) = ZI(JDES+1-1)
                    GO TO 130

                  END IF
  110           CONTINUE

              ELSE IF (NOMTE.EQ.'MECA_FACE9') THEN
                NOEUMI = 0
                NOEUSO = N1
              END IF
  120       CONTINUE
  130       CONTINUE

          ELSE
            NOEUSO = N1
          END IF
          DO 140 INO = 1,NOEUSO
            IPNO = IPNO + 1
            LISTNO(IPNO) = ZI(JDES+INO-1)
  140     CONTINUE
  150   CONTINUE
      END IF

      CALL JEDEMA()
      END
