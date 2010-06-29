      SUBROUTINE CTDATA (MESNOE,MESMAI,NKCHA,TYCH,TOUCMP,NKCMP,NBCMP,
     &                   NDIM,CHPGS,NOMA,NBNO,NBMA,NBVAL,TYPGD)
      IMPLICIT   NONE
      INTEGER      NBCMP,NDIM,NBNO,NBMA,NBVAL
      CHARACTER*1  TYPGD
      CHARACTER*4  TYCH
      CHARACTER*8  NOMA
      CHARACTER*24 MESNOE,MESMAI,NKCHA,NKCMP
      CHARACTER*19 CHPGS
      LOGICAL      TOUCMP
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 28/06/2010   AUTEUR FLEJOU J-L.FLEJOU 
C ======================================================================
C COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     ----- OPERATEUR CREA_TABLE , MOT-CLE FACTEUR RESU   --------------
C
C        BUT : RECUPERER LES DONNEES UTILES POUR CONSTRUIRE LA TABLE
C              (COMPOSANTES,NOEUDS,MAILLES,...)
C
C        IN     : NKCHA  (K24) : OBJET DES NOMS DE CHAMP
C                 NBVAL (I)    : NOMBRE DE VALEURS D'ACCES
C        IN/OUT : MESNOE (K24) : OBJET DES NOMS DE NOEUD
C                 MESMAI (K24) : OBJET DES NOMS DE MAILLE
C                 NKCMP  (K24) : OBJET DES NOMS DE COMPOSANTES
C                 NCHSPG (K24) : NOM DU CHAM_ELEM_S DES COORDONNES DES
C                                POINTS DE GAUSS (REMPLI SI TYCH='ELGA')
C        OUT    : TYCH   (K4)  : TYPE DE CHAMP (=NOEU,ELNO,ELGA)
C                 TOUCMP (L)   : INDIQUE SI TOUT_CMP EST RENSEIGNE
C                 NBCMP  (I)   : NOMBRE DE COMPOSANTES LORSQUE
C                                NOM_CMP EST RENSEIGNE, 0 SINON
C                 NDIM   (I)   : DIMENSION GEOMETRIQUE (=2 OU 3)
C                 NOMA   (K8)  : NOM DU MAILLAGE
C                 NBNO   (I)   : NOMBRE DE NOEUDS UTILISATEUR
C                 NBMA   (I)   : NOMBRE DE MAILLES UTILISATEUR
C                 TYPGD  (K1)  : TYPE DE LA GRANDEUR (REEL/COMPLEXE)
C
C ----------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER           ZI
      COMMON / IVARJE / ZI(1)
      REAL*8            ZR
      COMMON / RVARJE / ZR(1)
      COMPLEX*16        ZC
      COMMON / CVARJE / ZC(1)
      LOGICAL           ZL
      COMMON / LVARJE / ZL(1)
      CHARACTER*8       ZK8
      CHARACTER*16              ZK16
      CHARACTER*24                       ZK24
      CHARACTER*32                                ZK32
      CHARACTER*80                                         ZK80
      COMMON / KVARJE / ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
      CHARACTER*8 K8B
      INTEGER JKCHA,I,IBID,IRET,N,JLNO,JCMP,N1,JLMA,N2,N3
      REAL*8  R8B,VALR,EPSI
      COMPLEX*16 CBID
      CHARACTER*8 NOMO,NOMGD
      CHARACTER*8 TYPMCL(4),LPAIN(1),LPAOUT(1)
      CHARACTER*16 MOTCLE(4),CONCEP
      CHARACTER*19 LIGREL
      CHARACTER*24 CHGEOM,LCHIN(1),LCHOUT(1)
      LOGICAL EXIGEO
C     ------------------------------------------------------------------

      CALL JEMARQ()
C
C
C  --- 1. DETERMINATION DU TYPE DE CHAMP 
C
      CALL JEVEUO(NKCHA,'L',JKCHA)
      TYCH=' '
      LIGREL = ' '
      NOMO=' '
      TYPGD=' '
      DO 60 I=1,NBVAL
         IF(ZK24(JKCHA+I-1)(1:18).NE.'&&CHAMP_INEXISTANT')THEN
             CALL DISMOI('F','TYPE_CHAMP',ZK24(JKCHA+I-1)(1:19),
     &                     'CHAMP',IBID,TYCH,IRET)
             CALL DISMOI('F','NOM_MAILLA',ZK24(JKCHA+I-1)(1:19),
     &                     'CHAMP',IBID,NOMA,IRET)
             CALL DISMOI('F','NB_NO_MAILLA',NOMA,'MAILLAGE',NBNO,
     &                     K8B,IRET)
             CALL DISMOI('F','NB_MA_MAILLA',NOMA,'MAILLAGE',NBMA,
     &                     K8B,IRET)
             CALL DISMOI('F','DIM_GEOM',NOMA,'MAILLAGE',NDIM,K8B,IRET)
             CALL DISMOI('F','NOM_GD',ZK24(JKCHA+I-1)(1:19),
     &                   'CHAMP',IBID,NOMGD,IRET)
             TYPGD=NOMGD(6:6)
             IF(TYPGD.EQ.'C')CALL U2MESS('F','TABLE0_42')
             IF(TYCH(1:2).EQ.'EL')THEN
                CALL DISMOI('F','NOM_MODELE',ZK24(JKCHA+I-1)(1:19),
     &                     'CHAMP',IBID,NOMO,IRET)
                LIGREL=NOMO//'.MODELE'
             ENDIF 
             GOTO 61
         ENDIF
 60   CONTINUE
 61   CONTINUE
C
C  --- 2. RECUPERATION DES NOEUDS,MAILLES
C
      IF(TYCH.EQ.'NOEU')THEN

           MOTCLE(1) = 'NOEUD'
           MOTCLE(2) = 'GROUP_NO'
           MOTCLE(3) = 'MAILLE'
           MOTCLE(4) = 'GROUP_MA'
           TYPMCL(1) = 'NOEUD'
           TYPMCL(2) = 'GROUP_NO'
           TYPMCL(3) = 'MAILLE'
           TYPMCL(4) = 'GROUP_MA'
           CALL GETVTX ('RESU','TOUT',1,1,0, K8B, N1)
           IF(N1.NE.0)THEN
               CALL WKVECT(MESNOE,'V V I',NBNO,JLNO)
               DO 70 I=1,NBNO
                   ZI(JLNO+I-1)=I
 70            CONTINUE
           ELSE
               CALL RELIEM(' ', NOMA, 'NU_NOEUD', 'RESU', 1, 4,
     &                 MOTCLE, TYPMCL, MESNOE, NBNO )
               CALL JEVEUO ( MESNOE, 'L', JLNO )
           ENDIF
           NBMA=0

      ELSEIF(TYCH(1:2).EQ.'EL')THEN
           
C          VERIFICATIONS
           CALL GETVTX('RESU','NOEUD',1,1,0,K8B, N1)
           CALL GETVTX('RESU','GROUP_NO',1,1,0,K8B, N2)
           N3=-N1-N2
           IF(N3.NE.0)CALL U2MESS('F','TABLE0_41')

           MOTCLE(1) = 'MAILLE'
           MOTCLE(2) = 'GROUP_MA'
           TYPMCL(1) = 'MAILLE'
           TYPMCL(2) = 'GROUP_MA'
           CALL GETVTX ('RESU','TOUT',1,1,0, K8B, N1)
           IF(N1.NE.0)THEN
               CALL WKVECT(MESMAI,'V V I',NBMA,JLMA)
               DO 80 I=1,NBMA
                   ZI(JLMA+I-1)=I
 80            CONTINUE
           ELSE
               CALL RELIEM(' ', NOMA, 'NU_MAILLE', 'RESU', 1, 2,
     &                 MOTCLE, TYPMCL, MESMAI, NBMA )
               CALL JEVEUO ( MESMAI, 'L', JLMA )
           ENDIF
           NBNO=0

           IF(TYCH.EQ.'ELGA')THEN

              CALL MEGEOM(NOMO,' ',EXIGEO,CHGEOM)
              LCHIN(1)=CHGEOM(1:19)
              LPAIN(1)='PGEOMER'
              LCHOUT(1)='&&PEECAL.PGCOOR'
              LPAOUT(1)='PCOORPG'

              CALL CALCUL('S','COOR_ELGA',LIGREL,1,LCHIN,LPAIN,1,
     &                  LCHOUT,LPAOUT,'V')
           
              CALL CELCES(LCHOUT(1),'V',CHPGS)
           
           ENDIF

      ENDIF
C
C  --- 3. RECUPERATION DES COMPOSANTES
C
      CALL GETVTX('RESU','NOM_CMP' ,1,1,0,K8B ,N1)
      IF(N1.NE.0)THEN
           NBCMP=-N1
           TOUCMP=.FALSE.
           CALL WKVECT(NKCMP,'V V K8',NBCMP,JCMP)
           CALL GETVTX('RESU','NOM_CMP' ,1,1,NBCMP,ZK8(JCMP),N1)
      ELSE
           NBCMP=0
           TOUCMP=.TRUE.
           CALL WKVECT(NKCMP,'V V K8',1,JCMP)
           ZK8(JCMP)=' '
      ENDIF

      CALL JEDEMA()

      END
