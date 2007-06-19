      SUBROUTINE MMMRES(DEFICO,DEPDEL,NUMEDD,NOMA,CNSINR)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 19/06/2007   AUTEUR VIVAN L.VIVAN 
C TOLE CRP_20
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.

C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.

C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C TOLE CRP_20

      IMPLICIT     NONE
      CHARACTER*8 NOMA
      CHARACTER*19 CNSINR
      CHARACTER*24 DEFICO,DEPDEL,NUMEDD

C   BUT : PREPARER LE CHAM_NO_S POUR L ARCHIVAGE DU CONTACT PAR NMARCH
C         POUR LA METHODE CONTINUE ( ROUTINE JUMELLE DE CFRESU POUR LES
C         AUTRES TYPES DE CONTACT )

C   IN       DEFICO : SD DU CONTACT
C   IN       DEPDEL : DEPLACEMENT AU PAS DE TEMPS COURANT
C   IN       NUMEDD : NUME_DDL POUR LA CREATION DES CHAM_NO
C   IN       NOMA   : NOM DU MAILLAGE
C   IN/OUT   CNSINR : CHAM_NO_S POUR L ARCHIVAGE DU CONTACT


C ======================================================================
C ------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------------
      CHARACTER*32 JEXATR
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
C --------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------
C ======================================================================


      CHARACTER*8 LICMPR(20),LICMP4(4),LICMP6(6),LICNT2(2),LICNT3(3)
      CHARACTER*8 ALIAS,KBID
      INTEGER IRET,JNOESC,INO,JTABF,JDIM,NUNOE
      INTEGER NBNO,NBNOC,NBNOT,NNOM,NNOE,JCOODE,INOE,IN
      INTEGER JCNSVR,JCNSLR,JCONT,JFROT,JNOCO
      INTEGER JJEU,ICONEX,IATYMA,NUMAMA,NUMAES,NUTYP,NUNO,JUSU
      INTEGER JDEPDE,I,DIM,IZONE,JCMCF,ILONG,JCOOR
      INTEGER NTPC,IMA,NTMAE,JMAESC,JMACO,JEUTEM,JGLIE,JGLIM
      INTEGER JCONTA,JPREMI,JZOCO,NBZONE,INTEGR,JFROL,JCONL,JDEPDL
      INTEGER NBNOM,IBID,CFMMVD,ZTABF
      REAL*8 LICOEF,RN,RNX,RNY,RNZ,GLI1,GLI2,GLI,RTAX,RTAY,RTAZ,RTGX
      REAL*8 RTGY,RTGZ,X(2),FF(9)
      REAL*8 DEPLPM(3),DEPLPE(3),CONT,LAGSF,VECT1(3),VECT2(3)
      REAL*8 COOR(3),COORE(3),ERR(3),EPS
      CHARACTER*19 FCONT,FFROT,FCONTS,FFROTS,DEPDES,DEPCN
      CHARACTER*19 FCTCN,FFROCN
      CHARACTER*24 JEU,GLIE,GLIM,CONTAC,PREMIE
      LOGICAL   LFROT
      INTEGER   IFROT

      PARAMETER (EPS=1.D-6)

      CALL JEMARQ()

      FCONT  = '&&MMMRES.CONT'
      FCONTS = '&&MMMRES.CONT_S'
      FCTCN  = '&&MMMRES.FCTCN'
      FFROT  = '&&MMMRES.FROT'
      FFROTS = '&&MMMRES.FROT_S'
      FFROCN = '&&MMMRES.FROTCN'
      DEPDES = '&&MMMRES.DEPDES'
      DEPCN  = '&&MMMRES.DEPCN'
      CNSINR = '&&MMMRES.CNSRINR'
      JEU    = '&&MMMRES.JEU'
      GLIE   = '&&MMMRES.GLIE'
      GLIM   = '&&MMMRES.GLIM'
      CONTAC = '&&MMMRES.CONTAC'
      PREMIE = '&&MMMRES.PREMIE'

      CALL JEEXIN('&&CFMVEL.LISTE_RESU',IRET)

C-----EST CE QU ON EST EN CONTACT CONTINUE ?
      IF (IRET.GT.0) THEN

C ------ RECUPERATION DES DIVERSES DONNEES RELATIVES AU CONTACT

        CALL JEVEUO(DEFICO(1:16)//'.TABFIN','L',JTABF)
        CALL JEVEUO(DEFICO(1:16)//'.NDIMCO','L',JDIM)
        CALL JEVEUO(DEFICO(1:16)//'.NOESCL','L',JNOESC)
        CALL JEVEUO(DEFICO(1:16)//'.NOEUCO','L',JNOCO)
        CALL JEVEUO(DEFICO(1:16)//'.JEUCON','L',JJEU)
        CALL JEVEUO(DEFICO(1:16)//'.JEUSUR','L',JUSU)
        CALL JEVEUO(DEFICO(1:16)//'.CARACF','L',JCMCF)
        CALL JEVEUO(DEFICO(1:16)//'.MAESCL','L',JMAESC)
        CALL JEVEUO(NOMA(1:8)//'.TYPMAIL','L',IATYMA)
        CALL JEVEUO(DEFICO(1:16)//'.MAILCO','L',JMACO)
        CALL JEVEUO(DEFICO(1:16)//'.NOZOCO','L',JZOCO)

C---------RECHERCHE DE LA METHODE D INTEGRATION

        ZTABF = CFMMVD('ZTABF')

        LFROT = .FALSE.
        NBZONE = INT(ZR(JCMCF))
        DO 10 I = 1,NBZONE
          IF (ZR(JCMCF+22* (I-1)+1).EQ.2.D0) THEN
            CALL U2MESS('A','ALGORITH6_12')
            GO TO 190
          END IF
          IFROT = INT(ZR(JCMCF+22*(I-1)+5))
          IF ( IFROT .EQ. 3 ) LFROT = .TRUE.
   10   CONTINUE

        LICMPR(1) = 'CONT'
        LICMPR(2) = 'JEU'
        LICMPR(3) = 'RN'
        LICMPR(4) = 'RNX'
        LICMPR(5) = 'RNY'
        LICMPR(6) = 'RNZ'
        LICMPR(7) = 'GLIX'
        LICMPR(8) = 'GLIY'
        LICMPR(9) = 'GLI'
        LICMPR(10) = 'RTAX'
        LICMPR(11) = 'RTAY'
        LICMPR(12) = 'RTAZ'
        LICMPR(13) = 'RTGX'
        LICMPR(14) = 'RTGY'
        LICMPR(15) = 'RTGZ'
        LICMPR(16) = 'RX'
        LICMPR(17) = 'RY'
        LICMPR(18) = 'RZ'
        LICMPR(19) = 'R'
        LICMPR(20) = 'HN'

C ------ TRANSFORMATION DU CHAM_NO DES DDL EN UN CHAM_NO_S POUR
C ------ UNE LECTURE FACILE

        DIM = ZI(JDIM)
        CALL CNOCNS(DEPDEL,'V',DEPDES)

        LICMP6(1) = 'DX'
        LICMP6(2) = 'DY'
        LICMP6(3) = 'DZ'
        LICMP6(4) = 'LAGS_C'
        LICMP6(5) = 'LAGS_F1'
        LICMP6(6) = 'LAGS_F2'

        LICMP4(1) = 'DX'
        LICMP4(2) = 'DY'
        LICMP4(3) = 'LAGS_C'
        LICMP4(4) = 'LAGS_F1'

        IF (DIM.EQ.3) THEN
           IF ( LFROT ) THEN
              CALL CNSRED(DEPDES,0,0,6,LICMP6,'V',DEPCN)
           ELSE
              CALL CNSRED(DEPDES,0,0,4,LICMP6,'V',DEPCN)
           END IF
        ELSE IF (DIM.EQ.2) THEN
           IF ( LFROT ) THEN
              CALL CNSRED(DEPDES,0,0,4,LICMP4,'V',DEPCN)
           ELSE
              CALL CNSRED(DEPDES,0,0,3,LICMP4,'V',DEPCN)
           END IF
        END IF

        CALL JEVEUO(DEPCN//'.CNSV','L',JDEPDE)
        CALL JEVEUO(DEPCN//'.CNSL','L',JDEPDL)

C ------ CREATION DU CHAM_NO_S POUR L ARCHIVAGE DU CONTACT

        CALL CNSCRE(NOMA,'INFC_R',20,LICMPR,'V',CNSINR)
        CALL JEVEUO(CNSINR//'.CNSV','E',JCNSVR)
        CALL JEVEUO(CNSINR//'.CNSL','E',JCNSLR)
        LICOEF = 1.D0

C ------ ASSEMBLAGE DES SECONDS MEMBRES DU AUX CONTACTS
C ------ POUR RECUPERER LES FORCES NODALES DE CONTACT

        LICNT3(1) = 'DX'
        LICNT3(2) = 'DY'
        LICNT3(3) = 'DZ'

        LICNT2(1) = 'DX'
        LICNT2(2) = 'DY'

        CALL ASSVEC('V',FCONT,1,'&&MMCCON',LICOEF,NUMEDD,' ','ZERO',1)

        CALL CNOCNS(FCONT,'V',FCONTS)
        IF (DIM.EQ.3) THEN
          CALL CNSRED(FCONTS,0,0,3,LICNT3,'V',FCTCN)
        ELSE IF (DIM.EQ.2) THEN
          CALL CNSRED(FCONTS,0,0,2,LICNT2,'V',FCTCN)
        END IF
        CALL JEVEUO(FCTCN//'.CNSV','L',JCONT)
        CALL JEVEUO(FCTCN//'.CNSL','L',JCONL)

        CALL ASSVEC('V',FFROT,1,'&&MMCFROT',LICOEF,NUMEDD,' ','ZERO',1)

        CALL CNOCNS(FFROT,'V',FFROTS)
        IF (DIM.EQ.3) THEN
          CALL CNSRED(FFROTS,0,0,3,LICNT3,'V',FFROCN)
        ELSE IF (DIM.EQ.2) THEN
          CALL CNSRED(FFROTS,0,0,2,LICNT2,'V',FFROCN)
        END IF
        CALL JEVEUO(FFROCN//'.CNSV','L',JFROT)
        CALL JEVEUO(FFROCN//'.CNSL','L',JFROL)

        CALL JEVEUO(JEXATR(NOMA(1:8)//'.CONNEX','LONCUM'),'L',ILONG)
        CALL JEVEUO(NOMA(1:8)//'.CONNEX','L',ICONEX)
        CALL JEVEUO(NOMA(1:8)//'.COORDO    .VALE','L',JCOOR)
        CALL JEVEUO(NOMA(1:8)//'.COORDO    .DESC','L',JCOODE)

C ---- NBNO = NOMBRE DE POINTS D'INTEGRATION DE CONTACT

        NBNO = 0
        NTMAE = ZI(JMAESC)
        DO 20 IMA = 1,NTMAE
          NBNOC = ZI(JMAESC+3* (IMA-1)+3)
          NBNO = NBNO + NBNOC
   20   CONTINUE
C         WRITE(6,*)'NBNO=',NBNO
C--------NOMBRE TOTAL DE NOEUDS DU MAILALGE
        CALL DISMOI('F','NB_NO_MAILLA',NOMA,'MAILLAGE',NBNOM,KBID,IBID)
C--------CREATION D OBJETS DE TRAVAIL
C--------JEU DES POINTS DE CONTACT
        CALL WKVECT(JEU,'V V R',NBNO,JEUTEM)
C--------VECTEUR DE GLISSEMENT DU NOEUD ESCLAVE
        CALL WKVECT(GLIE,'V V R',2*NBNO,JGLIE)
C--------VECTEUR DE GLISSEMENT DU NOEUD MAITRE
        CALL WKVECT(GLIM,'V V R',2*NBNO,JGLIM)
C--------INDICATEUR DE CONTACT
        CALL WKVECT(CONTAC,'V V R',NBNO,JCONTA)
C--------VECTEUR LOGIQUE INDIQUANT UN PREMIER PASSAGE SUR UN NOEUD
C--------CE VECTEUR EST DIMENSIONNE AU NBRE TOTAL DE NOEUD DE
C--------/TOUT\ LE MAILLAGE CAR ON Y ACCEDE PAR LE NUMERO ABSOLU DU
C--------NOEUD
        CALL WKVECT(PREMIE,'V V L',NBNOM,JPREMI)
C ---- NBNOT: NOMBRE DE NOEUDS DE CONTACT
        NBNOT = ZI(JDIM+4)
C         WRITE(6,*)'NBNOT =',NBNOT

C ---- TRAITEMENT EN DIMENSION TROIS
C      ____________________________

        IF (DIM.EQ.3) THEN

C------CALCUL DU JEU AUX NOEUDS EN PRENANT LE MIN DES JEUX AUX NOEUDS,
C------DU GLISSEMENT EN PRENANT LE MAX DU GLISSEMENT AUX NOEUDS
C------DE L INDICATEUR DE CONTACT EN CONSIDERANT QU UN NOEUD EST EN
C------CONTACT A PARTIR DU MOMENT OU IL L EST SUR AU MOINS UNE
C------DES MAILLES.

          NTPC = 0
          NTMAE = ZI(JMAESC)
          DO 60 IMA = 1,NTMAE
            NBNOC = ZI(JMAESC+3* (IMA-1)+3)
            IZONE = ZI(JMAESC+3* (IMA-1)+2)
CCCCCCCCC   ??????
            INTEGR = INT(ZR(JCMCF+22*(IZONE-1)+1))
            IFROT  = INT(ZR(JCMCF+22*(IZONE-1)+5))

C ---- INTEGR>2 : INTEGRATION DE SIMPSON OU DE NEWTON COTES
            IF (INTEGR.EQ.3 .OR. INTEGR.EQ.4 .OR. INTEGR.EQ.5 .OR.
     &         INTEGR.EQ.6 .OR. INTEGR.EQ.7 .OR. INTEGR.EQ.8) THEN
              NBNOC = NBNOC/2
            END IF

            DO 50 INO = 1,NBNOC
              NUMAES = ZR(JTABF+ZTABF* (NTPC+INO-1)+1)
              NUMAMA = ZR(JTABF+ZTABF* (NTPC+INO-1)+2)
C            NUNOE=ZI(ICONEX+ZI(ILONG-1+NUMAES)+INO-2)
C            WRITE(6,*)'NUNOE=',NUNOE

C ------ VECTEURS DIRECTEURS DU PLAN DE CONTACT

              VECT1(1) = ZR(JTABF+ZTABF* (NTPC+INO-1)+6)
              VECT1(2) = ZR(JTABF+ZTABF* (NTPC+INO-1)+7)
              VECT1(3) = ZR(JTABF+ZTABF* (NTPC+INO-1)+8)
              VECT2(1) = ZR(JTABF+ZTABF* (NTPC+INO-1)+9)
              VECT2(2) = ZR(JTABF+ZTABF* (NTPC+INO-1)+10)
              VECT2(3) = ZR(JTABF+ZTABF* (NTPC+INO-1)+11)

C ------ DEPLACEMENT DU NOEUD ESCLAVE DE LA MAILLE ESCLAVE

              NUTYP = ZI(IATYMA-1+NUMAES)
              IF (NUTYP.EQ.2) THEN
                ALIAS = 'SG2'
                NNOM = 2
                X(1) = ZR(JTABF+ZTABF* (NTPC+INO-1)+3)
              ELSE IF (NUTYP.EQ.4) THEN
                ALIAS = 'SG3'
                NNOM = 3
                X(1) = ZR(JTABF+ZTABF* (NTPC+INO-1)+3)
              ELSE IF (NUTYP.EQ.7) THEN
                ALIAS = 'TR3'
                NNOM = 3
                X(1) = ZR(JTABF+ZTABF* (NTPC+INO-1)+3)
                X(2) = ZR(JTABF+ZTABF* (NTPC+INO-1)+12)
              ELSE IF (NUTYP.EQ.9) THEN
                ALIAS = 'TR6'
                NNOM = 6
                X(1) = ZR(JTABF+ZTABF* (NTPC+INO-1)+3)
                X(2) = ZR(JTABF+ZTABF* (NTPC+INO-1)+12)
              ELSE IF (NUTYP.EQ.12) THEN
                ALIAS = 'QU4'
                NNOM = 4
                X(1) = ZR(JTABF+ZTABF* (NTPC+INO-1)+3)
                X(2) = ZR(JTABF+ZTABF* (NTPC+INO-1)+12)
              ELSE IF (NUTYP.EQ.14) THEN
                ALIAS = 'QU8'
                NNOM = 8
                X(1) = ZR(JTABF+ZTABF* (NTPC+INO-1)+3)
                X(2) = ZR(JTABF+ZTABF* (NTPC+INO-1)+12)
              ELSE IF (NUTYP.EQ.16) THEN
                ALIAS = 'QU9'
                NNOM = 9
                X(1) = ZR(JTABF+ZTABF* (NTPC+INO-1)+3)
                X(2) = ZR(JTABF+ZTABF* (NTPC+INO-1)+12)
              ELSE
                CALL U2MESS('F','ELEMENTS_16')
              END IF

              CALL CALFFX(ALIAS,X(1),X(2),FF)

              DEPLPE(1) = 0.D0
              DEPLPE(2) = 0.D0
              DEPLPE(3) = 0.D0

C----DEPLACEMENT DE LA PROJECTION DU NOEUD ESCLAVE SUR LA MAILLE MAITRE
C----POUR LE CALCUL DU GLISSEMENT

              DO 30 I = 1,NNOM
                NUNO = ZI(ICONEX+ZI(ILONG-1+NUMAES)+I-2)

                IF ( IFROT .EQ. 3 ) THEN
                CALL ASSERT(ZL(JDEPDL-1+6*(NUNO-1)+1))
                CALL ASSERT(ZL(JDEPDL-1+6*(NUNO-1)+2))
                CALL ASSERT(ZL(JDEPDL-1+6*(NUNO-1)+3))
                DEPLPE(1) = DEPLPE(1) + ZR(JDEPDE-1+6*(NUNO-1)+1)*FF(I)
                DEPLPE(2) = DEPLPE(2) + ZR(JDEPDE-1+6*(NUNO-1)+2)*FF(I)
                DEPLPE(3) = DEPLPE(3) + ZR(JDEPDE-1+6*(NUNO-1)+3)*FF(I)
                ELSE
                CALL ASSERT(ZL(JDEPDL-1+4*(NUNO-1)+1))
                CALL ASSERT(ZL(JDEPDL-1+4*(NUNO-1)+2))
                CALL ASSERT(ZL(JDEPDL-1+4*(NUNO-1)+3))
                DEPLPE(1) = DEPLPE(1) + ZR(JDEPDE-1+4*(NUNO-1)+1)*FF(I)
                DEPLPE(2) = DEPLPE(2) + ZR(JDEPDE-1+4*(NUNO-1)+2)*FF(I)
                DEPLPE(3) = DEPLPE(3) + ZR(JDEPDE-1+4*(NUNO-1)+3)*FF(I)
                ENDIF

   30         CONTINUE

C ------ DEPLACEMENT DU NOEUD MAITRE,
C        PROJETE DU NOEUD ESCLAVE SUR LA MAILLE MAITRE

              NUTYP = ZI(IATYMA-1+NUMAMA)

              IF (NUTYP.EQ.2) THEN
                ALIAS = 'SG2'
                NNOM = 2
                X(1) = ZR(JTABF+ZTABF* (NTPC+INO-1)+4)
              ELSE IF (NUTYP.EQ.4) THEN
                ALIAS = 'SG3'
                NNOM = 3
                X(1) = ZR(JTABF+ZTABF* (NTPC+INO-1)+4)
              ELSE IF (NUTYP.EQ.7) THEN
                ALIAS = 'TR3'
                NNOM = 3
                X(1) = ZR(JTABF+ZTABF* (NTPC+INO-1)+4)
                X(2) = ZR(JTABF+ZTABF* (NTPC+INO-1)+5)
              ELSE IF (NUTYP.EQ.9) THEN
                ALIAS = 'TR6'
                NNOM = 6
                X(1) = ZR(JTABF+ZTABF* (NTPC+INO-1)+4)
                X(2) = ZR(JTABF+ZTABF* (NTPC+INO-1)+5)
              ELSE IF (NUTYP.EQ.12) THEN
                ALIAS = 'QU4'
                NNOM = 4
                X(1) = ZR(JTABF+ZTABF* (NTPC+INO-1)+4)
                X(2) = ZR(JTABF+ZTABF* (NTPC+INO-1)+5)
              ELSE IF (NUTYP.EQ.14) THEN
                ALIAS = 'QU8'
                NNOM = 8
                X(1) = ZR(JTABF+ZTABF* (NTPC+INO-1)+4)
                X(2) = ZR(JTABF+ZTABF* (NTPC+INO-1)+5)
              ELSE IF (NUTYP.EQ.16) THEN
                ALIAS = 'QU9'
                NNOM = 9
                X(1) = ZR(JTABF+ZTABF* (NTPC+INO-1)+4)
                X(2) = ZR(JTABF+ZTABF* (NTPC+INO-1)+5)
              ELSE
                CALL U2MESS('F','ELEMENTS_16')
              END IF

              CALL CALFFX(ALIAS,X(1),X(2),FF)

              DEPLPM(1) = 0.D0
              DEPLPM(2) = 0.D0
              DEPLPM(3) = 0.D0

C----DEPLACEMENT DE LA PROJECTION DU NOEUD ESCLAVE SUR LA MAILLE MAITRE

              DO 40 I = 1,NNOM
                NUNO = ZI(ICONEX+ZI(ILONG-1+NUMAMA)+I-2)

                IF ( IFROT .EQ. 3 ) THEN
                CALL ASSERT(ZL(JDEPDL-1+6* (NUNO-1)+1))
                CALL ASSERT(ZL(JDEPDL-1+6* (NUNO-1)+2))
                CALL ASSERT(ZL(JDEPDL-1+6* (NUNO-1)+3))
                DEPLPM(1) = DEPLPM(1) + ZR(JDEPDE-1+6* (NUNO-1)+1)*FF(I)
                DEPLPM(2) = DEPLPM(2) + ZR(JDEPDE-1+6* (NUNO-1)+2)*FF(I)
                DEPLPM(3) = DEPLPM(3) + ZR(JDEPDE-1+6* (NUNO-1)+3)*FF(I)
                ELSE
                CALL ASSERT(ZL(JDEPDL-1+4*(NUNO-1)+1))
                CALL ASSERT(ZL(JDEPDL-1+4*(NUNO-1)+2))
                CALL ASSERT(ZL(JDEPDL-1+4*(NUNO-1)+3))
                DEPLPM(1) = DEPLPM(1) + ZR(JDEPDE-1+4*(NUNO-1)+1)*FF(I)
                DEPLPM(2) = DEPLPM(2) + ZR(JDEPDE-1+4*(NUNO-1)+2)*FF(I)
                DEPLPM(3) = DEPLPM(3) + ZR(JDEPDE-1+4*(NUNO-1)+3)*FF(I)
                ENDIF

   40         CONTINUE

C----ECRITURE SUR LES VECTEURS DE TRAVAIL DES JEUX, DU GLISSEMENT ET DE
C----L INDICATEUR DE CONTACT

              ZR(JGLIE+2* (NTPC+INO-1)) = DEPLPE(1)*VECT1(1) +
     &                                    DEPLPE(2)*VECT1(2) +
     &                                    DEPLPE(3)*VECT1(3)

              ZR(JGLIE+2* (NTPC+INO-1)+1) = DEPLPE(1)*VECT2(1) +
     &          DEPLPE(2)*VECT2(2) + DEPLPE(3)*VECT2(3)

              ZR(JGLIM+2* (NTPC+INO-1)) = DEPLPM(1)*VECT1(1) +
     &                                    DEPLPM(2)*VECT1(2) +
     &                                    DEPLPM(3)*VECT1(3)

              ZR(JGLIM+2* (NTPC+INO-1)+1) = DEPLPM(1)*VECT2(1) +
     &          DEPLPM(2)*VECT2(2) + DEPLPM(3)*VECT2(3)

C                 WRITE(6,*)'JEU=',ZR(JJEU-1+NTPC+INO)
C                 WRITE(6,*)'CONT=',ZR(JTABF+20*(NTPC+INO-1)+13)

   50       CONTINUE
            NTPC = NTPC + NBNOC
   60     CONTINUE

C ------ BOUCLE SUR TOUS LES NOEUDS DE CONTACT

          DO 100 INO = 1,NBNOT

C ------ TRAITEMENT DES NOEUDS ESCLAVES SEULEMENT

            IF (ZR(JNOESC+10* (INO-1)+1).EQ.-1.D0) THEN

              GLI1 = 0.D0
              GLI2 = 0.D0
              GLI = 0.D0
              RTAX = 0.D0
              RTAY = 0.D0
              RTAZ = 0.D0
              RTGX = 0.D0
              RTGY = 0.D0
              RTGZ = 0.D0
              RN = 0.D0
              RNX = 0.D0
              RNY = 0.D0
              RNZ = 0.D0

C ------ COORD ABSOLUES DU NOEUD TRAITE: COOR

              NUNOE = ZI(JNOCO+INO-1)
              ZL(JPREMI-1+NUNOE) = .FALSE.
              COOR(1) = ZR(JCOOR-1+3* (NUNOE-1)+1)
              COOR(2) = ZR(JCOOR-1+3* (NUNOE-1)+2)
              COOR(3) = ZR(JCOOR-1+3* (NUNOE-1)+3)

C ------ ON REBOUCLE SUR LES POINTS D INTEGRATION POUR TESTER
C        LEUR COINCIDENCE AVEC LES NOEUDS DE CONTACT

              NTPC = 0
              INOE = 0
              NTMAE = ZI(JMAESC)
              DO 90 IMA = 1,NTMAE
                NBNOC = ZI(JMAESC+3* (IMA-1)+3)
                IZONE = ZI(JMAESC+3* (IMA-1)+2)
                INTEGR = INT(ZR(JCMCF+22*(IZONE-1)+1))
                IFROT  = INT(ZR(JCMCF+22*(IZONE-1)+5))

                IF (INTEGR.EQ.3 .OR. INTEGR.EQ.4 .OR. INTEGR.EQ.5 .OR.
     &         INTEGR.EQ.6 .OR. INTEGR.EQ.7 .OR. INTEGR.EQ.8) THEN
                  NBNOC = NBNOC/2
                END IF

                DO 80 IN = 1,NBNOC
                  NUMAES = ZR(JTABF+ZTABF* (NTPC+IN-1)+1)

C ------ COORD ABSOLUES DU POINT D INTEGRATION: COORE

                  NUTYP = ZI(IATYMA-1+NUMAES)

                  IF (NUTYP.EQ.2) THEN
                    ALIAS = 'SG2'
                    NNOM = 2
                    X(1) = ZR(JTABF+ZTABF* (NTPC+IN-1)+3)
                  ELSE IF (NUTYP.EQ.4) THEN
                    ALIAS = 'SG3'
                    NNOM = 3
                    X(1) = ZR(JTABF+ZTABF* (NTPC+IN-1)+3)
                  ELSE IF (NUTYP.EQ.7) THEN
                    ALIAS = 'TR3'
                    NNOM = 3
                    X(1) = ZR(JTABF+ZTABF* (NTPC+IN-1)+3)
                    X(2) = ZR(JTABF+ZTABF* (NTPC+IN-1)+12)
                  ELSE IF (NUTYP.EQ.9) THEN
                    ALIAS = 'TR6'
                    NNOM = 6
                    X(1) = ZR(JTABF+ZTABF* (NTPC+IN-1)+3)
                    X(2) = ZR(JTABF+ZTABF* (NTPC+IN-1)+12)
                  ELSE IF (NUTYP.EQ.12) THEN
                    ALIAS = 'QU4'
                    NNOM = 4
                    X(1) = ZR(JTABF+ZTABF* (NTPC+IN-1)+3)
                    X(2) = ZR(JTABF+ZTABF* (NTPC+IN-1)+12)
                  ELSE IF (NUTYP.EQ.14) THEN
                    ALIAS = 'QU8'
                    NNOM = 8
                    X(1) = ZR(JTABF+ZTABF* (NTPC+IN-1)+3)
                    X(2) = ZR(JTABF+ZTABF* (NTPC+IN-1)+12)
                  ELSE IF (NUTYP.EQ.16) THEN
                    ALIAS = 'QU9'
                    NNOM = 9
                    X(1) = ZR(JTABF+ZTABF* (NTPC+IN-1)+3)
                    X(2) = ZR(JTABF+ZTABF* (NTPC+IN-1)+12)
                  ELSE
                    CALL U2MESS('F','ELEMENTS_16')
                  END IF

                  CALL CALFFX(ALIAS,X(1),X(2),FF)

                  COORE(1) = 0.D0
                  COORE(2) = 0.D0
                  COORE(3) = 0.D0

C----DEPLACEMENT DE LA PROJECTION DU NOEUD ESCLAVE SUR LA MAILLE MAITRE
C----POUR LE CALCUL DU GLISSEMENT

                  DO 70 I = 1,NNOM
                    NUNO = ZI(ICONEX+ZI(ILONG-1+NUMAES)+I-2)

                    IF ( IFROT .EQ. 3 ) THEN
                      CALL ASSERT(ZL(JDEPDL-1+6*(NUNO-1)+1))
                      CALL ASSERT(ZL(JDEPDL-1+6*(NUNO-1)+2))
                      CALL ASSERT(ZL(JDEPDL-1+6*(NUNO-1)+3))
                    ELSE
                      CALL ASSERT(ZL(JDEPDL-1+4*(NUNO-1)+1))
                      CALL ASSERT(ZL(JDEPDL-1+4*(NUNO-1)+2))
                      CALL ASSERT(ZL(JDEPDL-1+4*(NUNO-1)+3))
                    ENDIF

                    COORE(1) = COORE(1) + ZR(JCOOR-1+3* (NUNO-1)+1)*
     &                         FF(I)
                    COORE(2) = COORE(2) + ZR(JCOOR-1+3* (NUNO-1)+2)*
     &                         FF(I)
                    COORE(3) = COORE(3) + ZR(JCOOR-1+3* (NUNO-1)+3)*
     &                         FF(I)

   70             CONTINUE

                  IF (COOR(1).NE.0) THEN
                    ERR(1) = ABS((COOR(1)-COORE(1))/COOR(1))
                  ELSE
                    ERR(1) = ABS(COORE(1))
                  END IF
                  IF (COOR(2).NE.0) THEN
                    ERR(2) = ABS((COOR(2)-COORE(2))/COOR(2))
                  ELSE
                    ERR(2) = ABS(COORE(2))
                  END IF
                  IF (COOR(3).NE.0) THEN
                    ERR(3) = ABS((COOR(3)-COORE(3))/COOR(3))
                  ELSE
                    ERR(3) = ABS(COORE(3))
                  END IF
                  INOE = INOE + 1

                  IF (ERR(1).LE.EPS .AND. ERR(2).LE.EPS .AND.
     &                ERR(3).LE.EPS) THEN

C______ POST-TRAITEMENT DES RESULTATS
C              WRITE(6,*)'NUNOE=',NUNOE
C ____ RECUPERATION DES DONNES AUX NOEUDS

                    IF (.NOT.ZL(JPREMI-1+NUNOE)) THEN

                      IF ( IFROT .EQ. 3 ) THEN
                        CALL ASSERT(ZL(JDEPDL-1+6*(NUNOE-1)+1))
                        CALL ASSERT(ZL(JDEPDL-1+6*(NUNOE-1)+2))
                        CALL ASSERT(ZL(JDEPDL-1+6*(NUNOE-1)+3))
                      ELSE
                        CALL ASSERT(ZL(JDEPDL-1+4*(NUNOE-1)+1))
                        CALL ASSERT(ZL(JDEPDL-1+4*(NUNOE-1)+2))
                        CALL ASSERT(ZL(JDEPDL-1+4*(NUNOE-1)+3))
                      ENDIF

                      ZL(JPREMI-1+NUNOE) = .TRUE.
                      ZL(JCNSLR-1+20* (NUNOE-1)+1) = .TRUE.

                      ZR(JCNSVR-1+20* (NUNOE-1)+2) = ZR(JJEU-1+INOE)
                      ZR(JCNSVR-1+20* (NUNOE-1)+20)= ZR(JUSU-1+INOE)
                      ZR(JCNSVR-1+20* (NUNOE-1)+1) = ZR(JTABF+
     &                  ZTABF* (INOE-1)+13)

C        WRITE(6,*)'JEU1=',ZR(JCNSVR-1+20*(NUNOE-1)+2)
C        WRITE(6,*)'CONT1=',ZR(JTABF+20*(INOE-1)+13)

                      GLI1 = ZR(JGLIE+2* (INOE-1)) -
     &                       ZR(JGLIM+2* (INOE-1))
                      GLI2 = ZR(JGLIE+2* (INOE-1)+1) -
     &                       ZR(JGLIM+2* (INOE-1)+1)
                      ZR(JCNSVR-1+20* (NUNOE-1)+7) = GLI1
                      ZR(JCNSVR-1+20* (NUNOE-1)+8) = GLI2
                      ZR(JCNSVR-1+20* (NUNOE-1)+9) = SQRT(GLI1**2+
     &                  GLI2**2)

                    ELSE
C                      CALL ASSERT(ZL(JCNSLR-1+20* (NUNOE-1)+1))
                      ZR(JCNSVR-1+20* (NUNOE-1)+
     &                  2) = MIN(ZR(JCNSVR-1+20* (NUNOE-1)+2),
     &                  ZR(JJEU-1+INOE))

C        WRITE(6,*)'CONT2=',ZR(JTABF+20*(INOE-1)+13)
C        WRITE(6,*)'JEU2=',ZR(JCNSVR-1+20*(NUNOE-1)+2)

                      ZR(JCNSVR-1+20* (NUNOE-1)+
     &                  1) = MAX(ZR(JCNSVR-1+20* (NUNOE-1)+1),
     &                  ZR(JTABF+ZTABF* (INOE-1)+13))

                      GLI1 = ZR(JGLIE+2* (INOE-1)) -
     &                       ZR(JGLIM+2* (INOE-1))
                      GLI2 = ZR(JGLIE+2* (INOE-1)+1) -
     &                       ZR(JGLIM+2* (INOE-1)+1)
                      GLI = SQRT(GLI1**2+GLI2**2)
                      IF (GLI.GT.ZR(JCNSVR-1+20* (NUNOE-1)+9)) THEN
                        ZR(JCNSVR-1+20* (NUNOE-1)+9) = GLI
                        ZR(JCNSVR-1+20* (NUNOE-1)+7) = GLI1
                        ZR(JCNSVR-1+20* (NUNOE-1)+8) = GLI2
                      END IF

                    END IF

                    CONT = ZR(JCNSVR-1+20* (NUNOE-1)+1)
                    IF (CONT.GE.1.D0) THEN

C ------ RECUPERATION DES FORCES NODALES DE CONTACT
                      CALL ASSERT(ZL(JCONL-1+3* (NUNOE-1)+1))
                      CALL ASSERT(ZL(JCONL-1+3* (NUNOE-1)+2))
                      CALL ASSERT(ZL(JCONL-1+3* (NUNOE-1)+3))

                      RNX = ZR(JCONT-1+3* (NUNOE-1)+1)
                      RNY = ZR(JCONT-1+3* (NUNOE-1)+2)
                      RNZ = ZR(JCONT-1+3* (NUNOE-1)+3)
                      RN = SQRT(RNX**2+RNY**2+RNZ**2)

C ----- NORME DU MULTIPLICATEUR DE LAGRANGE DU FROTTEMENT

                      IF ( IFROT .EQ. 3 ) THEN
                      LAGSF = SQRT((ZR(JDEPDE-1+6*(NUNOE-1)+5))**2+
     &                        (ZR(JDEPDE-1+6*(NUNOE-1)+6))**2)
                      ELSE
                        LAGSF = 0.D0
                      ENDIF

C ----- Y-A-T-IL DU FROTTEMENT ?

                      IZONE = ZI(JZOCO-1+INO)

                      IF (ZR(JCMCF+22* (IZONE-1)+5).EQ.3.D0) THEN

                        CALL ASSERT(ZL(JFROL-1+3* (NUNOE-1)+1))
                        CALL ASSERT(ZL(JFROL-1+3* (NUNOE-1)+2))
                        CALL ASSERT(ZL(JFROL-1+3* (NUNOE-1)+3))

                        IF (LAGSF.GE.0.999D0) THEN
C ------ LE NOEUD EST GLISSANT

                          RTGX = ZR(JFROT-1+3* (NUNOE-1)+1)
                          RTGY = ZR(JFROT-1+3* (NUNOE-1)+2)
                          RTGZ = ZR(JFROT-1+3* (NUNOE-1)+3)
                          ZR(JCNSVR-1+20* (NUNOE-1)+1) = 2.D0
                        ELSE

C ------ LE NOEUD EST ADHERENT

                          RTAX = ZR(JFROT-1+3* (NUNOE-1)+1)
                          RTAY = ZR(JFROT-1+3* (NUNOE-1)+2)
                          RTAZ = ZR(JFROT-1+3* (NUNOE-1)+3)
                        END IF
                      END IF
                    END IF
                  END IF
C______ FIN POST-TRAITEMENT DES RESULTATS
   80           CONTINUE
                NTPC = NTPC + NBNOC
   90         CONTINUE


C ------ ARCHIVAGE DES RESULTATS DANS LE CHAM_NO_S CREE

              ZR(JCNSVR-1+20* (NUNOE-1)+3) = RN
              ZR(JCNSVR-1+20* (NUNOE-1)+4) = RNX
              ZR(JCNSVR-1+20* (NUNOE-1)+5) = RNY
              ZR(JCNSVR-1+20* (NUNOE-1)+6) = RNZ
              ZR(JCNSVR-1+20* (NUNOE-1)+10) = RTAX
              ZR(JCNSVR-1+20* (NUNOE-1)+11) = RTAY
              ZR(JCNSVR-1+20* (NUNOE-1)+12) = RTAZ
              ZR(JCNSVR-1+20* (NUNOE-1)+13) = RTGX
              ZR(JCNSVR-1+20* (NUNOE-1)+14) = RTGY
              ZR(JCNSVR-1+20* (NUNOE-1)+15) = RTGZ
              ZR(JCNSVR-1+20* (NUNOE-1)+16) = RNX + RTAX + RTGX
              ZR(JCNSVR-1+20* (NUNOE-1)+17) = RNY + RTAY + RTGY
              ZR(JCNSVR-1+20* (NUNOE-1)+18) = RNZ + RTAZ + RTGZ
              ZR(JCNSVR-1+20* (NUNOE-1)+19) = SQRT((RNX+RTAX+RTGX)**2+
     &          (RNY+RTAY+RTGY)**2+ (RNZ+RTAZ+RTGZ)**2)

              ZL(JCNSLR-1+ (NUNOE-1)*20+1) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*20+2) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*20+3) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*20+4) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*20+5) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*20+6) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*20+7) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*20+8) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*20+9) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*20+10) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*20+11) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*20+12) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*20+13) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*20+14) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*20+15) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*20+16) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*20+17) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*20+18) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*20+19) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*20+20) = .TRUE.
            END IF

  100     CONTINUE

C ---- TRAITEMENT EN DIMENSION DEUX
C      ____________________________

        ELSE IF (DIM.EQ.2) THEN

C------CALCUL DU JEU AUX NOEUDS EN PRENANT LE MIN DES JEUX AUX NOEUDS,
C------DU GLISSEMENT EN PRENANT LE MAX DU GLISSEMENT AUX NOEUDS
C------DE L INDICATEUR DE CONTACT EN CONSIDERANT QU UN NOEUD EST
C------EN CONTACT A PARTIR DU MOMENT OU IL L EST SUR AU MOINS
C------UNE DES MAILLES.

          NTPC = 0
          NTMAE = ZI(JMAESC)
          DO 140 IMA = 1,NTMAE
            IZONE = ZI(JMAESC+3* (IMA-1)+2)
            INTEGR = INT(ZR(JCMCF+22*(IZONE-1)+1))
            IFROT  = INT(ZR(JCMCF+22*(IZONE-1)+5))

C----NBNOC= NOMBRE DE POINTS D'INTEGRATION DE CONTACT DE LA MAILLE IMA
            NBNOC = ZI(JMAESC+3* (IMA-1)+3)

C----INTEGR>2 : INTEGRATION DE SIMPSON OU DE NEWTON COTES
            IF (INTEGR.EQ.3 .OR. INTEGR.EQ.4 .OR. INTEGR.EQ.5 .OR.
     &         INTEGR.EQ.6 .OR. INTEGR.EQ.7 .OR. INTEGR.EQ.8) THEN
              NBNOC = NBNOC/2
            END IF

            DO 130 INO = 1,NBNOC

              NUMAES = ZR(JTABF+ZTABF* (NTPC+INO-1)+1)
              NUMAMA = ZR(JTABF+ZTABF* (NTPC+INO-1)+2)
              NUNOE = ZI(ICONEX+ZI(ILONG-1+NUMAES)+INO-2)
C                WRITE(6,*)'NUNOE =',NUNOE

C ---- VECTEURS DIRECTEURS DU PLAN DE CONTACT

              VECT1(1) = ZR(JTABF+ZTABF* (NTPC+INO-1)+6)
              VECT1(2) = ZR(JTABF+ZTABF* (NTPC+INO-1)+7)

              NUTYP = ZI(IATYMA-1+NUMAES)
              IF (NUTYP.EQ.2) THEN
                ALIAS = 'SG2'
                NNOE = 2
                X(1) = ZR(JTABF+ZTABF* (NTPC+INO-1)+3)
              ELSE IF (NUTYP.EQ.4) THEN
                ALIAS = 'SG3'
                NNOE = 3
                X(1) = ZR(JTABF+ZTABF* (NTPC+INO-1)+3)
              ELSE
                CALL U2MESS('F','ELEMENTS_16')
              END IF

              CALL CALFFX(ALIAS,X(1),X(2),FF)

              DEPLPE(1) = 0.D0
              DEPLPE(2) = 0.D0

C ---- DEPLACEMENT TANGENTIEL DU NOEUD ESCLAVE DE LA MAILLE ESCLAVE

              DO 110 I = 1,NNOE
                NUNO = ZI(ICONEX+ZI(ILONG-1+NUMAES)+I-2)
                IF ( IFROT .EQ. 3 ) THEN
                CALL ASSERT(ZL(JDEPDL-1+4*(NUNO-1)+1))
                CALL ASSERT(ZL(JDEPDL-1+4*(NUNO-1)+2))
                DEPLPE(1) = DEPLPE(1) + ZR(JDEPDE-1+4*(NUNO-1)+1)*FF(I)
                DEPLPE(2) = DEPLPE(2) + ZR(JDEPDE-1+4*(NUNO-1)+2)*FF(I)
                ELSE
                CALL ASSERT(ZL(JDEPDL-1+3*(NUNO-1)+1))
                CALL ASSERT(ZL(JDEPDL-1+3*(NUNO-1)+2))
                DEPLPE(1) = DEPLPE(1) + ZR(JDEPDE-1+3*(NUNO-1)+1)*FF(I)
                DEPLPE(2) = DEPLPE(2) + ZR(JDEPDE-1+3*(NUNO-1)+2)*FF(I)
                ENDIF
  110         CONTINUE


              NUTYP = ZI(IATYMA-1+NUMAMA)
              IF (NUTYP.EQ.2) THEN
                ALIAS = 'SG2'
                NNOM = 2
                X(1) = ZR(JTABF+ZTABF* (NTPC+INO-1)+4)
              ELSE IF (NUTYP.EQ.4) THEN
                ALIAS = 'SG3'
                NNOM = 3
                X(1) = ZR(JTABF+ZTABF* (NTPC+INO-1)+4)
              ELSE
                CALL U2MESS('F','ELEMENTS_16')
              END IF

              CALL CALFFX(ALIAS,X(1),X(2),FF)

              DEPLPM(1) = 0.D0
              DEPLPM(2) = 0.D0

C ---- DEPLACEMENT DU NOEUD MAITRE,
C      PROJETE DU NOEUD ESJCNSLRCLAVE SUR LA MAILLE MAITRE

              DO 120 I = 1,NNOM
                NUNO = ZI(ICONEX+ZI(ILONG-1+NUMAMA)+I-2)
                IF ( IFROT .EQ. 3 ) THEN
                CALL ASSERT(ZL(JDEPDL-1+4*(NUNO-1)+1))
                CALL ASSERT(ZL(JDEPDL-1+4*(NUNO-1)+2))
                DEPLPM(1) = DEPLPM(1) + ZR(JDEPDE-1+4*(NUNO-1)+1)*FF(I)
                DEPLPM(2) = DEPLPM(2) + ZR(JDEPDE-1+4*(NUNO-1)+2)*FF(I)
                ELSE
                CALL ASSERT(ZL(JDEPDL-1+3*(NUNO-1)+1))
                CALL ASSERT(ZL(JDEPDL-1+3*(NUNO-1)+2))
                DEPLPM(1) = DEPLPM(1) + ZR(JDEPDE-1+3*(NUNO-1)+1)*FF(I)
                DEPLPM(2) = DEPLPM(2) + ZR(JDEPDE-1+3*(NUNO-1)+2)*FF(I)
                ENDIF
  120         CONTINUE

C----ECRITURE SUR LES VECTEURS DE TRAVAIL DES JEUX, DU GLISSEMENT ET DE
C----L INDICATEUR DE CONTACT

C                   WRITE(6,*)'JEU=',ZR(JJEU-1+NTPC+INO)
C                   WRITE(6,*)'CONT=',ZR(JTABF+20*(NTPC+INO-1)+13)

              ZR(JGLIE+NTPC+INO-1) = DEPLPE(1)*VECT1(1) +
     &                               DEPLPE(2)*VECT1(2)

              ZR(JGLIM+NTPC+INO-1) = DEPLPM(1)*VECT1(1) +
     &                               DEPLPM(2)*VECT1(2)

  130       CONTINUE
            NTPC = NTPC + NBNOC
  140     CONTINUE

C ______ NOMBRE DE NOEUDS DE CONTACT = NBNOT
C        NOMBRE DE POINTS D'INTEGRATION DE CONTACT = NBNO

          DO 180 INO = 1,NBNOT

C ------ TRAITEMENT DES NOEUDS ESCLAVES SEULEMENT

            IF (ZR(JNOESC+10* (INO-1)+1).EQ.-1.D0) THEN

C ____ INITIALISATION DES DONNEES DE SORTIE

              GLI1 = 0.D0
              GLI = 0.D0
              RTAX = 0.D0
              RTAY = 0.D0
              RTGX = 0.D0
              RTGY = 0.D0
              RNX = 0.D0
              RNY = 0.D0
              RN = 0.D0

C ---- NUMERO ABSOLU DU NOEUD ESCL: NUNOE ET COORD ABSOLUES: COOR

              NUNOE = ZI(JNOCO+INO-1)
              COOR(1) = ZR(JCOOR-1+3* (NUNOE-1)+1)
              COOR(2) = ZR(JCOOR-1+3* (NUNOE-1)+2)
              COOR(3) = ZR(JCOOR-1+3* (NUNOE-1)+3)

C              WRITE(6,*)'NUNOE =',NUNOE
C              WRITE(6,*)'COORD =',(COOR(K),K=1,2)

C ------ ON REBOUCLE SUR LES POINTS D INTEGRATION POUR TESTER
C        LEUR COINCIDENCE AVEC LES NOEUDS DU MAILLAGE

              NTPC = 0
              INOE = 0
              NTMAE = ZI(JMAESC)
              ZL(JPREMI-1+NUNOE) = .FALSE.
              DO 170 IMA = 1,NTMAE
                IZONE = ZI(JMAESC+3* (IMA-1)+2)
                INTEGR = INT(ZR(JCMCF+22*(IZONE-1)+1))
                IFROT  = INT(ZR(JCMCF+22*(IZONE-1)+5))
                NBNOC = ZI(JMAESC+3* (IMA-1)+3)

                IF (INTEGR.EQ.3 .OR. INTEGR.EQ.4 .OR. INTEGR.EQ.5 .OR.
     &         INTEGR.EQ.6 .OR. INTEGR.EQ.7 .OR. INTEGR.EQ.8) THEN
                  NBNOC = NBNOC/2
                END IF

                DO 160 IN = 1,NBNOC
                  NUMAES = ZR(JTABF+ZTABF* (NTPC+IN-1)+1)
                  NUTYP = ZI(IATYMA-1+NUMAES)
                  IF (NUTYP.EQ.2) THEN
                    ALIAS = 'SG2'
                    NNOE = 2
                    X(1) = ZR(JTABF+ZTABF* (NTPC+IN-1)+3)
                  ELSE IF (NUTYP.EQ.4) THEN
                    ALIAS = 'SG3'
                    NNOE = 3
                    X(1) = ZR(JTABF+ZTABF* (NTPC+IN-1)+3)
                  ELSE
                    CALL U2MESS('F','ELEMENTS_16')
                  END IF

                  CALL CALFFX(ALIAS,X(1),X(2),FF)
                  COORE(1) = 0.D0
                  COORE(2) = 0.D0
                  DO 150 I = 1,NNOE
                    NUNO = ZI(ICONEX+ZI(ILONG-1+NUMAES)+I-2)
                    COORE(1) = COORE(1) + ZR(JCOOR-1+3* (NUNO-1)+1)*
     &                         FF(I)
                    COORE(2) = COORE(2) + ZR(JCOOR-1+3* (NUNO-1)+2)*
     &                         FF(I)
  150             CONTINUE

                  IF (COOR(1).NE.0) THEN
                    ERR(1) = ABS((COOR(1)-COORE(1))/COOR(1))
                  ELSE
                    ERR(1) = ABS(COORE(1))
                  END IF
                  IF (COOR(2).NE.0) THEN
                    ERR(2) = ABS((COOR(2)-COORE(2))/COOR(2))
                  ELSE
                    ERR(2) = ABS(COORE(2))
                  END IF
                  INOE = INOE + 1

                  IF (ERR(1).LE.EPS .AND. ERR(2).LE.EPS) THEN

C ____ POST-TRAITEMENT DES RESULTATS

                    IF (.NOT.ZL(JPREMI-1+NUNOE)) THEN
                      ZL(JPREMI-1+NUNOE) = .TRUE.
                      ZR(JCNSVR-1+20* (NUNOE-1)+2) = ZR(JJEU-1+INOE)
                      ZR(JCNSVR-1+20* (NUNOE-1)+20)= ZR(JUSU-1+INOE)
                      ZR(JCNSVR-1+20* (NUNOE-1)+1) = ZR(JTABF+
     &                  ZTABF* (INOE-1)+13)

C               WRITE(6,*)'JEU1=',ZR(JJEU-1+INOE)
C               WRITE(6,*)'CONT1=',ZR(JTABF+20*(INOE-1)+13)


                      GLI1 = ZR(JGLIE+INOE-1) - ZR(JGLIM+INOE-1)
                      GLI = SQRT(GLI1**2)
                      ZR(JCNSVR-1+20* (NUNOE-1)+7) = GLI1
                      ZR(JCNSVR-1+20* (NUNOE-1)+9) = GLI

                    ELSE

                      ZR(JCNSVR-1+20* (NUNOE-1)+
     &                  2) = MIN(ZR(JCNSVR-1+20* (NUNOE-1)+2),
     &                  ZR(JJEU-1+INOE))
                      ZR(JCNSVR-1+20* (NUNOE-1)+
     &                  1) = MAX(ZR(JCNSVR-1+20* (NUNOE-1)+1),
     &                  ZR(JTABF+ZTABF* (INOE-1)+13))

C               WRITE(6,*)'JEU2=',ZR(JJEU-1+INOE)
C               WRITE(6,*)'CONT2=',ZR(JTABF+20*(INOE-1)+13)

                      GLI1 = ZR(JGLIE+INOE-1) - ZR(JGLIM+INOE-1)
                      GLI = SQRT(GLI1**2)
                      IF (GLI.GT.ZR(JCNSVR-1+20* (NUNOE-1)+9)) THEN
                        ZR(JCNSVR-1+20* (NUNOE-1)+9) = GLI
                        ZR(JCNSVR-1+20* (NUNOE-1)+7) = GLI1
                      END IF

                    END IF

C ---- ETAT DU CONTACT: CONT

                    CONT = ZR(JCNSVR-1+20* (NUNOE-1)+1)
                    IF (CONT.GE.1.D0) THEN

C ------ RECUPERATION DES FORCES NODALES DE CONTACT
                      RNX = ZR(JCONT-1+2* (NUNOE-1)+1)
                      RNY = ZR(JCONT-1+2* (NUNOE-1)+2)
                      RN = SQRT(RNX**2+RNY**2)

C ----- NORME DU MULTIPLICATEUR DE LAGRANGE DU FROTTEMENT
                      IF ( IFROT .EQ. 3 ) THEN
                        LAGSF = ABS(ZR(JDEPDE-1+4* (NUNOE-1)+4))
                      ELSE
                        LAGSF = 0.D0
                      ENDIF
C ----- Y-A-T-IL DU FROTTEMENT ?

                      IZONE = ZI(JZOCO-1+INO)

                      IF (ZR(JCMCF+22* (IZONE-1)+5).EQ.3.D0) THEN
                        IF (LAGSF.GE.0.999D0) THEN
C ------ LE NOEUD EST GLISSANT
                          RTGX = ZR(JFROT-1+2* (NUNOE-1)+1)
                          RTGY = ZR(JFROT-1+2* (NUNOE-1)+2)
                          ZR(JCNSVR-1+20* (NUNOE-1)+1) = 2.D0
                        ELSE
C ------ LE NOEUD EST ADHERENT
                          RTAX = ZR(JFROT-1+2* (NUNOE-1)+1)
                          RTAY = ZR(JFROT-1+2* (NUNOE-1)+2)
                        END IF
                      END IF
                    END IF

                  END IF

  160           CONTINUE
                NTPC = NTPC + NBNOC
  170         CONTINUE

C ------ ARCHIVAGE DES RESULTATS DANS LE CHAM_NO_S CREE
              ZR(JCNSVR-1+20* (NUNOE-1)+3) = RN
              ZR(JCNSVR-1+20* (NUNOE-1)+4) = RNX
              ZR(JCNSVR-1+20* (NUNOE-1)+5) = RNY
              ZR(JCNSVR-1+20* (NUNOE-1)+10) = RTAX
              ZR(JCNSVR-1+20* (NUNOE-1)+11) = RTAY
              ZR(JCNSVR-1+20* (NUNOE-1)+13) = RTGX
              ZR(JCNSVR-1+20* (NUNOE-1)+14) = RTGY
              ZR(JCNSVR-1+20* (NUNOE-1)+16) = RNX + RTAX + RTGX
              ZR(JCNSVR-1+20* (NUNOE-1)+17) = RNY + RTAY + RTGY
              ZR(JCNSVR-1+20* (NUNOE-1)+19) = SQRT((RNX+RTAX+RTGX)**2+
     &          (RNY+RTAY+RTGY)**2)

              ZL(JCNSLR-1+ (NUNOE-1)*20+1) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*20+2) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*20+3) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*20+4) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*20+5) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*20+7) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*20+9) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*20+10) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*20+11) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*20+13) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*20+14) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*20+16) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*20+17) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*20+19) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*20+20) = .TRUE.

            END IF
  180     CONTINUE

        ELSE
          CALL U2MESS('F','ALGORITH6_13')
        END IF
      ELSE
        CALL U2MESS('F','ALGORITH6_14')

      END IF

      CALL JEDETR(FCONT)
      CALL DETRSD('CHAMP',FCONTS)
      CALL DETRSD('CHAMP',FCTCN)
      CALL JEDETR(FFROT)
      CALL DETRSD('CHAMP',FFROTS)
      CALL DETRSD('CHAMP',FFROCN)
      CALL DETRSD('CHAMP',DEPDES)
      CALL DETRSD('CHAMP',DEPCN)
      CALL JEDETR(JEU)
      CALL JEDETR(GLIE)
      CALL JEDETR(GLIM)
      CALL JEDETR(CONTAC)
      CALL JEDETR(PREMIE)

  190 CONTINUE
      CALL JEDEMA()
      END
