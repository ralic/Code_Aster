      SUBROUTINE CAFACI( FONREE, CHAR )
      IMPLICIT NONE
      CHARACTER*4         FONREE
      CHARACTER*8                 CHAR
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 05/05/2009   AUTEUR LAVERNE J.LAVERNE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C TOLE CRP_20

C     BUT: CREER LES CARTES CHAR.CHME.CMULT ET CHAR.CHME.CIMPO
C          ET REMPLIR LIGRCH POUR FACE_IMPO

C ARGUMENTS D'ENTREE:
C      FONREE  : TYPE DE LA VALEUR IMPOSEE :
C                REEL OU FONC OU COMP
C      CHAR  : NOM UTILISATEUR DU RESULTAT DE CHARGE

C ROUTINES APPELEES:
C---------------- COMMUNS NORMALISES  JEVEUX  --------------------------
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
      CHARACTER*32 ZK32 ,JEXNOM,JEXNUM
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C---------------- FIN COMMUNS NORMALISES  JEVEUX  ----------------------

      INTEGER NMOCL
      INTEGER VALI(2)
      PARAMETER (NMOCL=300)

      INTEGER I,J,K,N,JLISTI
      INTEGER NBNOEU,JVAL,NDDLA,JDIREC,NBNO
      INTEGER IDIM,IN,JNORM,JTANG,JNONO,NFACI
      INTEGER IBID,JNOMA,IER,NDIM,NBMA2,JCOMPT
      INTEGER N1,N2,INO,JPRNM,NBEC,NMCL,INOR,ICMP,ICMPX,INDIK8
      INTEGER IE,NBMA,NBCMP,INOM
      INTEGER JLIST2,JLIST3,JLIST1,NBMA3,NBNO1,NBNO3
      INTEGER DDLIMP(NMOCL),NBNO2,JLINO2,JLINO1,JLINO,JLINU
      REAL*8 VALIMR(NMOCL),COEF(3),DIRECT(3)
      COMPLEX*16 VALIMC(NMOCL),COEFC(3)
      CHARACTER*1 K1BID
      CHARACTER*2 TYPLAG
      CHARACTER*3 TYMOCL(NMOCL)
      CHARACTER*3 CDIM
      CHARACTER*4 TYPCOE
      CHARACTER*8 K8B,NOMA,MOD,NOMG
      CHARACTER*8 VALIMF(NMOCL),NOMNOE,DDL(3)
      CHARACTER*16 MOTFAC,MOTCLE(NMOCL),NOMCMD,TYPMCL(2),MOCLM(2)
      CHARACTER*16 MOCLM2(2),MOCLM3(2),TYPMC3(2)
      CHARACTER*24 MESMAI,MESMA2,LNOEU2,LNOEU1,MESNO3
      CHARACTER*19 LIGRMO
      CHARACTER*19 LISREL
      LOGICAL EXISDG,EXIDX

      CALL JEMARQ()
      CALL GETFAC('FACE_IMPO',NFACI)
      IF (NFACI.EQ.0) GO TO 220
      CALL GETRES(K8B,K8B,NOMCMD)

      LISREL = '&&CAFACI.RLLISTE'

      MESMAI = '&&CAFACI.MES_MAILLES'
      MESMA2 = '&&CAFACI.MAILLES_2'
      MESNO3 = '&&CAFACI.NOEUDS_2'
      MOTFAC = 'FACE_IMPO     '

      MOCLM(1) = 'MAILLE'
      MOCLM(2) = 'GROUP_MA'
      MOCLM2(1) = 'SANS_MAILLE'
      MOCLM2(2) = 'SANS_GROUP_MA'
      TYPMCL(1) = 'MAILLE'
      TYPMCL(2) = 'GROUP_MA'

      MOCLM3(1) = 'SANS_NOEUD'
      MOCLM3(2) = 'SANS_GROUP_NO'
      TYPMC3(1) = 'NOEUD'
      TYPMC3(2) = 'GROUP_NO'

      TYPLAG = '12'
      DDL(1) = 'DX      '
      DDL(2) = 'DY      '
      DDL(3) = 'DZ      '

      COEFC(1) = (1.0D0,0.0D0)
      COEFC(2) = (1.0D0,0.0D0)
      COEFC(3) = (1.0D0,0.0D0)

      TYPCOE = 'REEL'
      IF (FONREE.EQ.'COMP')  TYPCOE = 'COMP'

C --- MODELE ASSOCIE AU LIGREL DE CHARGE ---

      CALL DISMOI('F','NOM_MODELE',CHAR(1:8),'CHARGE',IBID,MOD,IER)

C ---  LIGREL DU MODELE ---

      LIGRMO = MOD(1:8)//'.MODELE'

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

C --- MAILLAGE ASSOCIE AU MODELE ---
      CALL JEVEUO(LIGRMO//'.LGRF','L',JNOMA)
      NOMA = ZK8(JNOMA)

C ---------------------------------------------------
C *** RECUPERATION DU DESCRIPTEUR GRANDEUR .PRNM
C *** DU MODELE
C ---------------------------------------------------
      CALL DISMOI('F','NB_NO_MAILLA',LIGRMO,'LIGREL',N1,K8B,IER)
      CALL JELIRA(LIGRMO//'.PRNM','LONMAX',N2,K1BID)
      NBEC = N2/N1
      IF (NBEC.GT.10) THEN
        CALL U2MESS('F','MODELISA_94')
      ELSE
        CALL JEVEUO(LIGRMO//'.PRNM','L',JPRNM)
      END IF

C     --------------------------------------------------------
C     RECUPERATION DE LA DIMENSION DE L'ESPACE DES COORDONNEES
C     --------------------------------------------------------
      CALL DISMOI('F','Z_CST',MOD,'MODELE',IBID,CDIM,IE)
      IF (CDIM.EQ.'OUI') THEN
        NDIM = 2
      ELSE
        NDIM = 3
      END IF

      DO 210 I = 1,NFACI
        ICMP = 0
        INOR = 0
        NBMA2= 0
        NBNO3= 0
C
C ----- RECUPERATION DES MAILLES
        CALL RELIEM ( ' ', NOMA, 'NU_MAILLE', MOTFAC, I, 2,
     &                     MOCLM, TYPMCL, MESMAI, NBMA )
        CALL JEVEUO ( MESMAI, 'L', JLISTI )

C ----- RECUPERATION DES MAILLES (A EXCLURE)
        CALL RELIEM ( ' ', NOMA, 'NU_MAILLE', MOTFAC, I, 2,
     &                     MOCLM2, TYPMCL, MESMA2, NBMA2 )

C ----- RECUPERATION DES NOEUDS (A EXCLURE)
        CALL RELIEM ( ' ', NOMA, 'NO_NOEUD', MOTFAC, I, 2,
     &                     MOCLM3, TYPMC3, MESNO3, NBNO3 )

C ---------------------------------------------------
C     RECUPERATION DES MOTS-CLES DDL SOUS FACE_IMPO
C     MOTCLE(J) : K8 CONTENANT LE J-EME MOT-CLE DDL
C     NDDLA     : NOMBRE DE MOTS CLES DU TYPE DDL
C ---------------------------------------------------
        CALL GETMJM('FACE_IMPO',I,0,MOTCLE,TYMOCL,N)
        NMCL = -N
        IF (NMCL.GT.NMOCL) THEN
          VALI (1) = NMOCL
          VALI (2) = NMCL
          CALL U2MESG('F', 'MODELISA8_31',0,' ',2,VALI,0,0.D0)
        END IF
        CALL GETMJM('FACE_IMPO',I,NMCL,MOTCLE,TYMOCL,N)
        NDDLA = 0
        DO 50 J = 1,NMCL
          IF ( MOTCLE(J).NE.'MAILLE' .AND.
     &         MOTCLE(J).NE.'GROUP_MA' .AND.
     &         MOTCLE(J).NE.'SANS_MAILLE' .AND.
     &         MOTCLE(J).NE.'SANS_GROUP_MA' .AND.
     &         MOTCLE(J).NE.'SANS_NOEUD' .AND.
     &         MOTCLE(J).NE.'SANS_GROUP_NO' .AND.
     &         MOTCLE(J).NE.'DNOR' .AND.
     &         MOTCLE(J).NE.'DTAN') THEN
            NDDLA = NDDLA + 1
            MOTCLE(NDDLA) = MOTCLE(J)
          END IF
   50   CONTINUE
        MOTCLE(NDDLA+1) = 'DNOR'
        MOTCLE(NDDLA+2) = 'DTAN'
        IF (FONREE.EQ.'REEL') THEN
          DO 60 J = 1,NDDLA + 2
            CALL GETVR8('FACE_IMPO',MOTCLE(J),I,1,1,VALIMR(J),DDLIMP(J))
            IF (J.LE.NDDLA) THEN
              ICMP = ICMP + DDLIMP(J)
            ELSE
              INOR = INOR + DDLIMP(J)
            END IF
   60     CONTINUE
          IF (NDIM.EQ.3 .AND. DDLIMP(NDDLA+2).NE.0) THEN
            CALL U2MESS('F','MODELISA2_63')
          END IF
        ELSE
          DO 70 J = 1,NDDLA + 2
            CALL GETVID('FACE_IMPO',MOTCLE(J),I,1,1,VALIMF(J),DDLIMP(J))
            IF (J.LE.NDDLA) THEN
              ICMP = ICMP + DDLIMP(J)
            ELSE
              INOR = INOR + DDLIMP(J)
            END IF
   70     CONTINUE
          IF (NDIM.EQ.3 .AND. DDLIMP(NDDLA+2).NE.0) THEN
            CALL U2MESS('F','MODELISA2_63')
          END IF
        END IF

C      ***************************************
C      TRAITEMENT DES COMPOSANTES DNOR ET DTAN
C      ***************************************
C    -------------------------------------
C    RECUPERATION DES NOEUDS (A CONSERVER)
C    -------------------------------------
          IF ((NBMA2.NE.0) .OR. (NBNO3.NE.0)) THEN

C           LISTE DES NOMS DES NOEUDS
            LNOEU1='&&CAFACI.NOEU_MAIL.TOTAL'
            CALL JEDETR(LNOEU1)
            CALL MAINOE(NOMA,NBMA,ZI(JLISTI),'NO',NBNO1,LNOEU1)
            CALL JEVEUO(LNOEU1,'L',JLINO1)
C           LISTE DES NOMS DES NOEUDS DES MAILLES EXCLUES
            LNOEU2='&&CAFACI.NOEU_MAIL.EXCL'
            CALL JEDETR(LNOEU2)
            IF (NBMA2.NE.0) THEN
C              LISTE DES NUM DES MAILLES EXCLUES
               CALL JEVEUO ( MESMA2, 'L', JLIST2 )
C              LISTE DES NOMS DES NOEUDS DES MAILLES EXCLUES
               CALL MAINOE(NOMA,NBMA2,ZI(JLIST2),'NO',NBNO2,LNOEU2)
            ENDIF
            IF (NBNO3.NE.0) THEN
C              LISTE DES NOMS DES NOEUDS EXCLUS : SANS_(NOEUD,GROUP_NO)
               CALL JEVEUO ( MESNO3, 'L', JLIST3 )
C              Si NBMA2<>0
C                 AGRANDIR LNOEU2 de NBNO3 et ajouter MESNO3
C              Sinon
C                 CREER LNOEU2 et copier MESNO3
               IF ( NBMA2.NE.0 ) THEN
                  CALL JUVECA(LNOEU2,NBNO2+NBNO3)
                  CALL JEVEUO(LNOEU2,'E',JLINO2)
C                 COMPLETER AVEC LES NOEUDS
                  DO 300 J = 0 , NBNO3-1
                     ZK8(JLINO2+NBNO2+J) = ZK8(JLIST3+J)
300               CONTINUE
                  NBNO2=NBNO2+NBNO3
               ELSE
                  CALL WKVECT(LNOEU2,'V V K8',NBNO3,JLINO2)
                  DO 310 J=0,NBNO3-1
                     ZK8(JLINO2+J) = ZK8(JLIST3+J)
310               CONTINUE
                  NBNO2=NBNO3
               ENDIF
            ENDIF
            CALL JEVEUO(LNOEU2,'L',JLINO2)

C             WRITE(*,*) 'NOM DES NOEUDS DE LA LISTE 1'
C             WRITE(*,*) (' '//ZK8(JLINO1+J),J=0,NBNO1-1)
C             WRITE(*,*) 'NOM DES NOEUDS DE LA LISTE 2'
C             WRITE(*,*) (' '//ZK8(JLINO2+J),J=0,NBNO2-1)

C           LISTE DES NOMS DES NOEUDS A CONSERVER
            CALL JEDETR('&&CAFACI.NOEU_NOM')
            CALL WKVECT('&&CAFACI.NOEU_NOM','V V K8',NBNO1,JLINO)
            NBNO = NBNO1
            CALL KNDIFF(8,ZK8(JLINO1),NBNO1,ZK8(JLINO2),NBNO2,
     &                    ZK8(JLINO),NBNO)

C             WRITE(*,*) 'NOM DES NOEUDS DE LA LISTE 1 - 2'
C             WRITE(*,*) (' '//ZK8(JLINO+J),J=0,NBNO)

C           LISTE DES NUMEROS DES NOEUDS A CONSERVER
            CALL JEDETR('&&CAFACI.NOEU_NUM')
            CALL WKVECT ( '&&CAFACI.NOEU_NUM','V V I',NBNO,JLINU)
            DO 71 J=1,NBNO
               CALL JENONU(JEXNOM(NOMA//'.NOMNOE',ZK8(JLINO+J-1)),
     &              ZI(JLINU+J-1))
 71         CONTINUE

          ELSE
C           LISTE DES NUMEROS DES NOEUDS
            LNOEU1='&&CAFACI.NOEU_MAIL.TOTAL'
            CALL JEDETR(LNOEU1)
            CALL MAINOE(NOMA,NBMA,ZI(JLISTI),'NU',NBNO,LNOEU1)
            CALL JEVEUO(LNOEU1,'L',JLINU)
          ENDIF

C   -------------------------------------------
C   CALCUL DES NORMALES ET TANGENTES AUX NOEUDS
C   -------------------------------------------
          IF (DDLIMP(NDDLA+1).NE.0) THEN
            CALL CANORT(NOMA,NBMA,ZI(JLISTI),K8B,NDIM,NBNO,
     &                                       ZI(JLINU),1)
            CALL JEVEUO('&&CANORT.NORMALE','L',JNORM)
          END IF
          IF (DDLIMP(NDDLA+2).NE.0) THEN
            CALL CANORT(NOMA,NBMA,ZI(JLISTI),K8B,NDIM,NBNO,
     &                                       ZI(JLINU),2)
            CALL JEVEUO('&&CANORT.TANGENT','L',JTANG)
          END IF
C   ----------------------
C   AFFECTATION DES COMPOSANTES DE LA RELATION A LA RELATION COURANTE
C   ----------------------
          COEF(1) = 1.0D0
          DDL(1) = 'DEPL'
          DO 120 INO = 1,NBNO
            IN = ZI(JLINU+INO-1)

C           TEST SI DDL DX EXISTE : OUI AFRELA CLASSIQUE, NON XDDLIM
            EXIDX = .TRUE.   
            ICMPX = INDIK8(ZK8(INOM),'DX',1,NBCMP)
            IF (.NOT.EXISDG(ZI(JPRNM-1+(IN-1)*NBEC+1),ICMPX)) 
     &         EXIDX=.FALSE.
            
            CALL JENUNO(JEXNUM(NOMA//'.NOMNOE',IN),NOMNOE)
            
            IF (DDLIMP(NDDLA+1).NE.0) THEN
              DO 100 IDIM = 1,NDIM
                DIRECT(IDIM) = ZR(JNORM-1+NDIM* (INO-1)+IDIM)
  100         CONTINUE

              IF (EXIDX) THEN
              
                CALL AFRELA(COEF,COEFC,DDL,NOMNOE,NDIM,DIRECT,1,
     &                      VALIMR(NDDLA+1),VALIMC(NDDLA+1),
     &                      VALIMF(NDDLA+1),TYPCOE,FONREE,TYPLAG,0.D0,
     &                      LISREL)
                                  
              ELSE
              
                CALL XDDLIM(MOD,DDL,ZK8(INOM),NBCMP,NOMNOE,IN,
     &                      VALIMR(NDDLA+1),VALIMC(J),VALIMF(J),
     &                      ZI(JPRNM-1+(IN-1)*NBEC+1),FONREE,
     &                      IBID,LISREL,NDIM,DIRECT)
              
              ENDIF
            END IF
            
            IF (DDLIMP(NDDLA+2).NE.0) THEN
              DO 110 IDIM = 1,NDIM
                DIRECT(IDIM) = ZR(JTANG-1+NDIM* (INO-1)+IDIM)
  110         CONTINUE
  
              IF (EXIDX) THEN
              
                CALL AFRELA(COEF,COEFC,DDL,NOMNOE,NDIM,DIRECT,1,
     &                      VALIMR(NDDLA+2),VALIMC(NDDLA+2),
     &                      VALIMF(NDDLA+2),TYPCOE,FONREE,TYPLAG,0.D0,
     &                      LISREL)
                            
              ELSE
              
                CALL XDDLIM(MOD,DDL,ZK8(INOM),NBCMP,NOMNOE,IN,
     &                      VALIMR(NDDLA+2),VALIMC(J),VALIMF(J),
     &                      ZI(JPRNM-1+(IN-1)*NBEC+1),FONREE,
     &                      IBID,LISREL,NDIM,DIRECT)
     
              ENDIF
            END IF
            
  120     CONTINUE

C      ***************************************************
C      TRAITEMENT DES COMPOSANTES DX DY DZ DRX DRY DRZ ...
C      ***************************************************

        IF (ICMP.EQ.0) GO TO 200

        CALL JELIRA(NOMA//'.NOMNOE','NOMMAX',NBNOEU,K1BID)

C    ALLOCATION DE 3 OBJETS INTERMEDIAIRES PERMETTANT D'APPLIQUER
C    LA REGLE DE SURCHARGE :

C               - VECTEUR (K8) CONTENANT LES NOMS DES NOEUDS
C               - TABLEAU DES VALEURS DES DDLS DES NOEUDS BLOQUES
C                 DIM NBNOEU * NBCOMP
C               - VECTEUR (IS) CONTENANT LE DESCRIPTEUR GRANDEUR
C                 ASSOCIE AUX DDLS IMPOSES PAR NOEUD

        CALL WKVECT('&&CAFACI.NOMS_NOEUDS','V V K8',NBNOEU,JNONO)
        IF (FONREE.EQ.'REEL') THEN
          CALL WKVECT('&&CAFACI.VALDDL','V V R',NDDLA*NBNOEU,JVAL)
        ELSE
          CALL WKVECT('&&CAFACI.VALDDL','V V K8',NDDLA*NBNOEU,JVAL)
        END IF

        CALL WKVECT('&&CAFACI.DIRECT','V V R',3*NBNOEU,JDIREC)

          CALL WKVECT('&&CAFACI.ICOMPT','V V I',NDDLA,JCOMPT)
          DO 180 INO = 1,NBNO
            IN = ZI(JLINU-1+INO)
            CALL JENUNO(JEXNUM(NOMA//'.NOMNOE',IN),NOMNOE)
            ZK8(JNONO-1+IN) = NOMNOE
            CALL AFDDLI(ZR(JVAL),ZK8(JVAL),ZC(JVAL),
     &                    ZI(JPRNM-1+ (IN-1)*NBEC+1),NDDLA,FONREE,
     &                    NOMNOE,IN,DDLIMP,VALIMR,VALIMF,VALIMC,MOTCLE,
     &                    NBEC,ZR(JDIREC+3* (IN-1)),0,MOD,LISREL,
     &                    ZK8(INOM),NBCMP,ZI(JCOMPT))
  180     CONTINUE
          DO 181,K=1,NDDLA
             IF (ZI(JCOMPT-1+K) .EQ. 0 ) CALL U2MESK('F','MODELISA2_45',
     &1,MOTCLE(K))
  181     CONTINUE
          CALL JEDETR('&&CAFACI.ICOMPT')

        CALL JEDETR('&&CAFACI.NOMS_NOEUDS')
        CALL JEDETR('&&CAFACI.VALDDL')
        CALL JEDETR('&&CAFACI.DIRECT')

  200   CONTINUE

        CALL JEDETR ( MESMAI )
        CALL JEDETR ( MESMA2 )

  210 CONTINUE
C        -------------------------------------
C        AFFECTATION DE LA LISTE DE RELATIONS A LA CHARGE
C        (I.E. AFFECTATION DES OBJETS .CMULT, .CIMPO,
C        LIGRCH ET .NEMA)
C        -------------------------------------
      CALL AFLRCH(LISREL,CHAR)

  220 CONTINUE
      CALL JEDETR('&&NBNLMA.LN')
      CALL JEDETR('&&NBNLMA.NBN')
      CALL JEDETR('&&CANORT.NORMALE')
      CALL JEDETR('&&CANORT.TANGENT')

      CALL JEDEMA()
      END
