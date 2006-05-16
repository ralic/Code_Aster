      SUBROUTINE XORIPE(MODELE,FISS)
      IMPLICIT NONE 

      CHARACTER*8   MODELE,FISS


C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 09/05/2006   AUTEUR JMBHH01 J.M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE JMBHH01 J.M.PROIX
C
C        ORIENTER LES SOUS-ELEMENTS DE PEAU DES ELEMENTS X-FEM
C                    
C  IN         MODELE    : NOM DE L'OBJET MODELE	 
C  IN/OUT     FISS      : NOM DE LA SD FISS_XFEM
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
      CHARACTER*32     JEXNUM, JEXNOM, JEXATR
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      REAL*8        R8B,ARMIN,PREC,G2(3),G3(3),NEXT(3),NORME
      REAL*8        CO(3,3),AB(3),AC(3),N2D(3),DDOT
      COMPLEX*16    CBID
      INTEGER       JNOMA,NGR,IGR,JGR,N1,NBELT,ITYPEL,IEL,IMA,NBMA,J
      INTEGER       JMAIL,CPT,NBMAIL,IRET,NBPAR,JCOOR,JM3D,IBID,I,JVECNO
      INTEGER       NUMA3D,NUMA2D,NBNOM3,NBNOM2,JCONX1,JCONX2,INO,NUNO
      INTEGER       ICH,JCESD(3),JCESV(3),JCESL(3),IAD,NIT,IT,NSE,ISE,IN
      INTEGER       NDIME,ICMP,NDIM,ID(3),INTEMP,NSEORI,IFM,NIV,NNCP
      CHARACTER*8   TYPMA,NOMA,K8BID,NOMAIL,K8B
      CHARACTER*2   KDIM
      CHARACTER*16  NOTYPE
      CHARACTER*19  LIGREL,NOMT19,CHS(3)
      CHARACTER*24  MAMOD,LIEL,GRMAPE,NOMMAI,NOMOB,PARA,VECNOR
C ----------------------------------------------------------------------

      CALL JEMARQ()

C     ATTENTION, NE PAS CONFONDRE NDIM ET NDIME  !!
C     NDIM EST LA DIMENSION DU MAILLAGE
C     NDIME EST DIMENSION DE LA MAILLE DE PEAU
      NDIME=2
      NDIM=3
      
      LIGREL = MODELE//'.MODELE'
      LIEL=LIGREL//'.LIEL'

C     RECUPERATION DU MAILLAGE ASSOCIE AU MODELE :
      MAMOD = MODELE(1:8)//'.MODELE    .NOMA'
      CALL JEVEUO(MAMOD,'L',JNOMA)
      NOMA = ZK8(JNOMA)
      NOMMAI = NOMA//'.NOMMAI'
      CALL DISMOI('F','NB_MA_MAILLA',NOMA,'MAILLAGE',NBMA,K8BID,IBID)
      CALL JEVEUO(NOMA//'.CONNEX','L',JCONX1)
      CALL JEVEUO(JEXATR(NOMA//'.CONNEX','LONCUM'),'L',JCONX2)
      CALL JEVEUO(NOMA//'.COORDO    .VALE','L',JCOOR)


C     RECUPERATION DE L'ARETE MINIMUM DU MAILLAGE :
      CALL JEEXIN ( NOMA//'           .LTNT', IRET )
      IF ( IRET .NE. 0 ) THEN
         CALL LTNOTB ( NOMA , 'CARA_GEOM' , NOMT19 )
         NBPAR = 0
         PARA = 'AR_MIN                  '
         CALL TBLIVA (NOMT19, NBPAR, ' ', IBID, R8B, CBID, K8B,
     +                K8B, R8B , PARA, K8B, IBID, ARMIN, CBID,
     +                K8B, IRET )
         IF ( IRET .EQ. 0 ) THEN
            PREC = ARMIN*1.D-06
         ELSEIF ( IRET .EQ. 1 ) THEN
            PREC = 1.D-10
         ELSE
            CALL UTMESS('F','XORIPE',
     + 'PROBLEME POUR RECUPERER UNE GRANDEUR DANS LA TABLE "CARA_GEOM"')
         ENDIF
      ELSE
         CALL UTMESS('F','XORIPE',
     +            'LA TABLE "CARA_GEOM" N''EXISTE PAS DANS LE MAILLAGE')
      ENDIF


C     ------------------------------------------------------------------
C     I°) CREATION DE LA LISTE DES NUMEROS DES MAILLES DE PEAU
C     ------------------------------------------------------------------

      GRMAPE='&&XORIPE.GRMAPE'
      CALL WKVECT(GRMAPE,'V V I',NBMA,JMAIL)

      CPT=0

      CALL JELIRA(LIEL,'NMAXOC',NGR,K8BID)
      DO 100 IGR=1,NGR
        CALL JEVEUO(JEXNUM(LIEL,IGR),'L',JGR)
        CALL JELIRA(JEXNUM(LIEL,IGR),'LONMAX',N1,K8BID)
        NBELT=N1-1
        ITYPEL=ZI(JGR-1+N1)
        CALL JENUNO(JEXNUM('&CATA.TE.NOMTE',ITYPEL),NOTYPE)
        DO 110 IEL=1,NBELT
          IMA=ZI(JGR-1+IEL)
          IF (NOTYPE(1:12).EQ.'MECA_XH_FACE') THEN
            CPT=CPT+1
            ZI(JMAIL-1+CPT)=IMA
            CALL JENUNO(JEXNUM(NOMMAI,IMA),NOMAIL)
C            WRITE(6,*)'ON STOCKE ',IMA,NOMAIL
          ENDIF
 110    CONTINUE
 100  CONTINUE      

C     NOMBRE DE MAILLES DE LA LISTE
      NBMAIL=CPT
      IF (NBMAIL.EQ.0) GOTO 999

C     ------------------------------------------------------------------
C     II°) RECHERCHE DES MAILLES SUPPORT
C     ------------------------------------------------------------------

      KDIM ='3D'
      NOMOB = '&&XORIPE.NU_MAILLE_3D'
      CALL UTMASU(MODELE(1:8),NOMA,KDIM,NBMAIL,ZI(JMAIL),NOMOB,PREC,R8B)
      CALL JEVEUO (NOMOB,'L',JM3D)

C      DO 200 IMA=1,NBMAIL
C        WRITE(6,*)'NUMA3D ',ZI(JM3D-1+IMA)
C 200  CONTINUE

C     ------------------------------------------------------------------
C     III°) CREATION DU VECTEUR DES NORMALES SORTANTES
C     ------------------------------------------------------------------

      VECNOR='&&XORIPE.VECNOR'
      CALL WKVECT(VECNOR,'V V R',NBMAIL*3,JVECNO)
    
      DO 300 IMA=1,NBMAIL

C       NUMEROS DES MAILLES 2D ET 3D
        NUMA2D=ZI(JMAIL-1+IMA)
        NUMA3D=ZI(JM3D-1+IMA)

C       NOMBRES DE NOEUDS DES MAILLES 2D ET 3D
        NBNOM2=ZI(JCONX2+NUMA2D) - ZI(JCONX2+NUMA2D-1)
        NBNOM3=ZI(JCONX2+NUMA3D) - ZI(JCONX2+NUMA3D-1)

C       G2 : CENTRE DE GRAVITÉ DE LA MAILLE 2D
        CALL LCINVN(3,0.D0,G2) 
        DO 310 INO=1,NBNOM2
          NUNO=ZI(JCONX1-1+ZI(JCONX2+NUMA2D-1)+INO-1)
          DO 311 J=1,3
            G2(J)=G2(J)+ZR(JCOOR-1+3*(NUNO-1)+J)/NBNOM2
 311      CONTINUE
 310    CONTINUE

C        WRITE(6,*)'G2 ',G2

C       G3 : CENTRE DE GRAVITÉ DE LA MAILLE 3D
        CALL LCINVN(3,0.D0,G3) 
        DO 320 INO=1,NBNOM3
          NUNO=ZI(JCONX1-1+ZI(JCONX2+NUMA3D-1)+INO-1)
          DO 321 J=1,3
            G3(J)=G3(J)+ZR(JCOOR-1+3*(NUNO-1)+J)/NBNOM3
 321      CONTINUE
 320    CONTINUE

C        WRITE(6,*)'G3 ',G3

C       NORMALE EXTERIEURE : Next = G2 - G3
        CALL VDIFF(NDIM,G2,G3,NEXT)
        CALL NORMEV(NEXT,NORME)
        DO 330 J=1,NDIM
          ZR(JVECNO-1+3*(IMA-1)+J)=NEXT(J)
 330    CONTINUE

C        WRITE(6,*)'NEXT ',NEXT

 300  CONTINUE

C     ------------------------------------------------------------------
C     IV°) ORIENTATION DES SOUS-TRIA
C     ------------------------------------------------------------------

      CHS(1)  = '&&XORIPE.PINTTO'
      CHS(2)  = '&&XORIPE.CNSETO'
      CHS(3)  = '&&XORIPE.LONCHA'

      CALL CELCES(FISS//'.TOPOSE.PINTTO','V', CHS(1))
      CALL CELCES(FISS//'.TOPOSE.CNSETO','V', CHS(2))
      CALL CELCES(FISS//'.TOPOSE.LONCHAM','V',CHS(3))

      DO 40 ICH=1,3
        CALL JEVEUO(CHS(ICH)//'.CESD','L',JCESD(ICH))
        CALL JEVEUO(CHS(ICH)//'.CESV','E',JCESV(ICH))
        CALL JEVEUO(CHS(ICH)//'.CESL','L',JCESL(ICH))
 40   CONTINUE

C     NOMBRE DE SOUS-TRIA RE-ORIENTES
      NSEORI=0.D0

      DO 400 IMA=1,NBMAIL

        DO 401 J=1,NDIM
          NEXT(J)=ZR(JVECNO-1+3*(IMA-1)+J)
 401    CONTINUE

        NUMA2D=ZI(JMAIL-1+IMA)

C       RECUPERATION DE LA SUBDIVISION LA MAILLE DE PEAU EN NIT TRI
        CALL CESEXI('S',JCESD(3),JCESL(3),NUMA2D,1,1,1,IAD)
        NIT=ZI(JCESV(3)-1+IAD)

        CPT=0
C       BOUCLE SUR LES NIT TRI
        DO 410 IT=1,NIT

C         RECUPERATION DU DECOUPAGE EN NSE SOUS-ELEMENTS
          CALL CESEXI('S',JCESD(3),JCESL(3),NUMA2D,1,1,1+IT,IAD)
          NSE=ZI(JCESV(3)-1+IAD)

C         BOUCLE SUR LES NSE SOUS-ELEMENTS
          DO 420 ISE=1,NSE

            CPT=CPT+1

C           CO(J,IN) : Jeme COORDONNEE DU INeme SOMMET DU SOUS-TRIA
            DO 421 IN=1,3
              ICMP=(NDIME+1)*(CPT-1)+IN
              CALL CESEXI('S',JCESD(2),JCESL(2),NUMA2D,1,1,ICMP,ID(IN))
              INO=ZI(JCESV(2)-1+ID(IN))
              IF (INO.LT.1000) THEN
                NUNO=ZI(JCONX1-1+ZI(JCONX2+NUMA2D-1)+INO-1)
                DO 422 J=1,NDIM
                  CO(J,IN)=ZR(JCOOR-1+NDIM*(NUNO-1)+J)
 422            CONTINUE
              ELSE
                DO 423 J=1,NDIM
                  ICMP=NDIM*(INO-1000-1)+J
                  CALL CESEXI('S',JCESD(1),JCESL(1),NUMA2D,1,1,ICMP,IAD)
                  CO(J,IN)=ZR(JCESV(1)-1+IAD)
 423            CONTINUE
              ENDIF
 421        CONTINUE

            CALL VDIFF(NDIM,CO(1,2),CO(1,1),AB)
            CALL VDIFF(NDIM,CO(1,3),CO(1,1),AC)

C           NORMALE AU SOUS-TRIA 2D
            CALL PROVEC(AB,AC,N2D)
            CALL NORMEV(N2D,NORME)

C            WRITE(6,*)'N2D ',N2D

C           PRODUIT SCALAIRE DES NORMALES : N2D.NEXT
            IF (DDOT(NDIM,N2D,1,NEXT,1).LT.0.D0) THEN
C             ON INVERSE LES SOMMETS 2 ET 3
              NSEORI=NSEORI+1
              INTEMP=ZI(JCESV(2)-1+ID(2))
              ZI(JCESV(2)-1+ID(2))=ZI(JCESV(2)-1+ID(3))
              ZI(JCESV(2)-1+ID(3))=INTEMP
            ENDIF
            
 420      CONTINUE

 410    CONTINUE

 400  CONTINUE

C     ON SAUVE LE NOUVEAU CHAM_ELEM MODIFIE A LA PLACE DE L'ANCIEN
      CALL CESCEL(CHS(2),LIGREL,'TOPOSE','PCNSETO','OUI',
     &                               NNCP,'G',FISS//'.TOPOSE.CNSETO')
      
C     ------------------------------------------------------------------
C     FIN
C     ------------------------------------------------------------------

      CALL JEDETR('&&XORIPE.NU_MAILLE_3D')
      CALL JEDETR('&&XORIPE.VECNOR')

 999  CONTINUE

      CALL INFNIV(IFM,NIV)
      WRITE(IFM,*)'NOMBRE DE SOUS-ELEMENTS DE PEAU RE-ORIENTES =',NSEORI

      CALL JEDETR('&&XORIPE.GRMAPE')

      CALL JEDEMA()
      END
