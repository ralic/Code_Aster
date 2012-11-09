      SUBROUTINE CADDLP ( FONREE, CHAR )
      IMPLICIT NONE
      INCLUDE 'jeveux.h'

      CHARACTER*32 JEXNUM,JEXNOM
      CHARACTER*4         FONREE
      CHARACTER*8                 CHAR
C ---------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     CREER LES CARTES CHAR.CHME.CMULT ET CHAR.CHME.CIMPO
C          ET REMPLIR LIGRCH, POUR LE MOT-CLE 'DDL_POUTRE'
C
C IN  : FONREE : 'REEL' OU 'FONC'
C IN  : CHARGE : NOM UTILISATEUR DU RESULTAT DE CHARGE
C ---------------------------------------------------------------------
C
      INTEGER       NDDLA
      PARAMETER    ( NDDLA = 6)
      INTEGER      DDLIMP(NDDLA), NDDLI,N1,N2,IOC,I,J,K,IBID
      INTEGER      IER, NBEC, JNOMA, NBNOEU, JPRNM, JVAL, IFM, NIV
      INTEGER      JDIREC, JDIMEN, NBNO, IALINO, INO, NBMA, IALIMA
      INTEGER      INOM, NBCMP, JCOMPT,NN1(3)
      REAL*8       VALIMR(NDDLA),PGL(3,3),DLOC(3),DGLO(3),ZERO
      REAL*8       RLN1(3),RGN1(3)
      COMPLEX*16   VALIMC(NDDLA)

      CHARACTER*1   K1BID
      CHARACTER*8   MOD, NOMA, K8BID, NOMG
      CHARACTER*8   NOMN, VALIMF(NDDLA), DDL(NDDLA)
      CHARACTER*16  MOTFAC, MOTCLE(NDDLA), MOTCL1(2), TYMOC1(2),
     &              MOTCL2(2), TYMOC2(2), NOMCMD
      CHARACTER*19  LIGRMO, LISREL, K19BID
      CHARACTER*24  NOMNOE, NCNCIN
      INTEGER      IARG
C ----------------------------------------------------------------------
      DATA MOTCLE / 'DX' , 'DY' , 'DZ' , 'DRX' , 'DRY' , 'DRZ' /
C ----------------------------------------------------------------------
      CALL JEMARQ()
C
      MOTFAC = 'DDL_POUTRE      '
      CALL GETFAC ( MOTFAC, NDDLI )
      IF ( NDDLI .EQ. 0 ) GOTO 9999
      CALL GETRES(K8BID,K8BID,NOMCMD)
C
      CALL INFNIV ( IFM, NIV )

      IF (NOMCMD(11:14).EQ.'MECA') THEN
        NOMG='DEPL_R'
      ELSE IF (NOMCMD(11:14).EQ.'THER') THEN
        NOMG='TEMP_R'
      ELSE IF (NOMCMD(11:14).EQ.'ACOU') THEN
        NOMG='PRES_C'
      ELSE
        CALL ASSERT(.FALSE.)
      END IF
      CALL JEVEUO(JEXNOM('&CATA.GD.NOMCMP',NOMG),'L',INOM)
      CALL JELIRA(JEXNOM('&CATA.GD.NOMCMP',NOMG),'LONMAX',NBCMP,K1BID)
C
      LISREL = '&&CADDLP.RLLISTE'
      MOTCL1(1) = 'NOEUD'
      TYMOC1(1) = 'NOEUD'
      MOTCL1(2) = 'GROUP_NO'
      TYMOC1(2) = 'GROUP_NO'
C
      MOTCL2(1) = 'MAILLE'
      TYMOC2(1) = 'MAILLE'
      MOTCL2(2) = 'GROUP_MA'
      TYMOC2(2) = 'GROUP_MA'
C
      ZERO = 0.D0
C
C --- MODELE ASSOCIE AU LIGREL DE CHARGE ---
C
      CALL DISMOI('F','NOM_MODELE',CHAR(1:8),'CHARGE',IBID,MOD,IER)
      LIGRMO = MOD(1:8)//'.MODELE'

      CALL DISMOI('F','NB_NO_MAILLA',LIGRMO,'LIGREL',N1,K8BID,IER)
      CALL JELIRA(LIGRMO//'.PRNM','LONMAX',N2,K1BID)
      NBEC = N2/N1
      IF (NBEC.GT.10) THEN
        CALL U2MESS('F','MODELISA2_46')
      END IF
C
C --- MAILLAGE ASSOCIE AU MODELE ---
C
      CALL JEVEUO(LIGRMO//'.LGRF','L',JNOMA)
      NOMA = ZK8(JNOMA)

      NOMNOE = NOMA//'.NOMNOE'
      CALL JELIRA(NOMNOE,'NOMMAX',NBNOEU,K1BID)

      CALL JEVEUO(LIGRMO//'.PRNM','L',JPRNM)
C
      NCNCIN = '&&CADDLP.CONNECINVERSE  '
      CALL JEEXIN ( NCNCIN, N1 )
      IF ( N1 .EQ. 0 ) CALL CNCINV ( NOMA, IBID, 0, 'V', NCNCIN )
C
C ---------------------------------------------------
C 2   ALLOCATION DE TABLEAUX DE TRAVAIL
C ---------------------------------------------------
C   OBJETS INTERMEDIAIRES PERMETTANT D'APPLIQUER LA REGLE DE SURCHARGE

C        -  VECTEUR (K8) CONTENANT LES NOMS DES NOEUDS
C        -  TABLEAU DES VALEURS DES DDLS DES NOEUDS BLOQUES
C                         DIM NBNOEU * NBCOMP
C        -  VECTEUR (IS) CONTENANT LE DESCRIPTEUR GRANDEUR ASSOCIE AUX
C                         DDLS IMPOSES PAR NOEUD

      IF (FONREE.EQ.'REEL') THEN
        CALL WKVECT('&&CADDLP.VALDDL','V V R' ,NDDLA*NBNOEU,JVAL)
      ELSE
        CALL U2MESK('F','MODELISA2_47',1,FONREE)
      END IF
      CALL WKVECT('&&CADDLP.DIRECT'   ,'V V R',3*NBNOEU,JDIREC)
      CALL WKVECT('&&CADDLP.DIMENSION','V V I',  NBNOEU,JDIMEN)
C --------------------------------------------------------------
C 3   BOUCLE SUR LES OCCURENCES DU MOT-CLE FACTEUR DDL IMPOSE
C --------------------------------------------------------------

      IF ( NIV .GE. 2 )  WRITE(IFM,1020)

      DO 100 IOC = 1, NDDLI

C ------ RECUPERATION DE LA LISTE DES NOEUDS :
         CALL RELIEM ( ' ', NOMA, 'NU_NOEUD', MOTFAC, IOC, 2, MOTCL1,
     &                                TYMOC1, '&&CADDLP.NOEUD', NBNO )
         CALL JEVEUO ( '&&CADDLP.NOEUD','L', IALINO )
         CALL RELIEM ( MOD, NOMA, 'NU_MAILLE', MOTFAC, IOC, 2, MOTCL2,
     &                              TYMOC2, '&&CADDLP.MAILLE', NBMA )
         IF ( NBMA .NE. 0 ) THEN
            CALL JEVEUO ( '&&CADDLP.MAILLE','L', IALIMA )
         ELSE
            IALIMA = 1
         ENDIF

         CALL WKVECT('&&CADDLP.ICOMPT','V V I',NDDLA,JCOMPT)
         DO 110 K = 1 , NBNO
            INO = ZI(IALINO-1+K)
            CALL JENUNO ( JEXNUM(NOMNOE,INO), NOMN )
C
C --------- MATRICE DE PASSAGE AU REPERE GLOBAL ---
C
            CALL MATLOC ( NOMA, NCNCIN, MOTFAC, IOC, INO, NBMA,
     &                    ZI(IALIMA), PGL )
C
C --------- RECUPERATION DE LA VALEUR IMPOSEE  (MOCLE(J)):
C           ----------------------------------------------
            IF (FONREE.EQ.'REEL') THEN
               DO 120 J = 1 , 3
                  DDLIMP(J) = 0
                  DDL(J)    = ' '
                  DLOC(J)   = ZERO
                  RLN1(J)   = ZERO
                  CALL GETVR8(MOTFAC,MOTCLE(J),IOC,IARG,1,
     &                        DLOC(J),NN1(J))
                  IF ( NN1(J) .GE. 1 ) RLN1(J) = 1.0D0
120            CONTINUE
               CALL UTPVLG(1,3,PGL,DLOC,DGLO)
               CALL UTPVLG(1,3,PGL,RLN1,RGN1)
               DO 122 J = 1 , 3
                  IF ( RGN1(J) .NE. ZERO ) THEN
                     DDL(J)    = MOTCLE(J)
                     VALIMR(J) = DGLO(J)
                     DDLIMP(J) = 1
                  ENDIF
122            CONTINUE

               DO 130 J = 1 , 3
                  DDLIMP(J+3) = 0
                  DDL(J+3)    = ' '
                  DLOC(J)     = ZERO
                  RLN1(J)     = ZERO
                  CALL GETVR8(MOTFAC,MOTCLE(J+3),IOC,IARG,1,
     &                        DLOC(J),NN1(J))
                  IF ( NN1(J) .GE. 1 ) RLN1(J) = 1.0D0
130            CONTINUE
               CALL UTPVLG(1,3,PGL,DLOC,DGLO)
               CALL UTPVLG(1,3,PGL,RLN1,RGN1)
               DO 132 J = 1 , 3
                  IF ( RGN1(J) .NE. ZERO ) THEN
                     DDL(J+3)    = MOTCLE(J+3)
                     VALIMR(J+3) = DGLO(J)
                     DDLIMP(J+3) = 1
                  ENDIF
132            CONTINUE
            END IF
            IF ( NIV .GE. 2 )  THEN
               I = 0
               DO 77 J = 1,NDDLA
                  IF ( DDLIMP(J) .EQ. 0 ) GOTO 77
                  IF ( I .EQ. 0 ) THEN
                     WRITE(IFM,1000) NOMN, DDL(J), VALIMR(J)
                  ELSE
                     WRITE(IFM,1010)       DDL(J), VALIMR(J)
                  ENDIF
                  I = I + 1
77             CONTINUE
            ENDIF

            CALL AFDDLI ( ZR(JVAL), ZK8(JVAL), ZC(JVAL),
     &                    ZI(JPRNM-1+(INO-1)*NBEC+1), NDDLA,
     &                    FONREE, NOMN, INO, DDLIMP, VALIMR, VALIMF,
     &                    VALIMC, MOTCLE, ZR(JDIREC+3*(INO-1)),
     &                    ZI(JDIMEN+INO-1), MOD,LISREL,
     &                    ZK8(INOM), NBCMP, ZI(JCOMPT), .FALSE., IBID,
     &                    IBID, K19BID, K19BID, K19BID, K19BID)
110      CONTINUE
         DO 111,K=1,NDDLA
            IF (ZI(JCOMPT-1+K) .EQ. 0 )
     &               CALL U2MESK('F','MODELISA2_45',1,MOTCLE(K))
111      CONTINUE
         CALL JEDETR('&&CADDLP.ICOMPT')
100   CONTINUE
C
      CALL AFLRCH ( LISREL, CHAR )
      CALL JEDETR('&&CADDLP.VALDDL')
      CALL JEDETR('&&CADDLP.DIRECT')
      CALL JEDETR('&&CADDLP.DIMENSION')
      CALL JEDETR('&&CADDLP.NUNOTMP')
      CALL JEEXIN ( NCNCIN, N1 )
      IF ( N1 .NE. 0 ) CALL JEDETR( NCNCIN )
C
9999  CONTINUE
C
1020  FORMAT( '"DDL_POUTRE" DANS LE REPERE GLOBAL : ' )
1000  FORMAT( /,'NOEUD = ',A8,', ',A8,' = ',1P,E12.5 )
1010  FORMAT(                  18X,A8,' = ',1P,E12.5 )
C
      CALL JEDEMA()
C
      END
