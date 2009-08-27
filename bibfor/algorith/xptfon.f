      SUBROUTINE XPTFON(NOMA,NMAFON,CNSLT,CNSLN,CNXINV,
     &                  JMAFON,NXPTFF,JFON,NFON,JBAS,JBORD,NPTBOR,
     &                  ARMIN,FISS)
      IMPLICIT NONE

      INTEGER       NMAFON,JMAFON,JFON,NFON,NXPTFF,JBORD,NPTBOR
      REAL*8        ARMIN
      CHARACTER*8   NOMA,FISS
      CHARACTER*19  CNSLT,CNSLN,CNXINV
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 24/08/2009   AUTEUR GENIAUT S.GENIAUT 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE GENIAUT S.GENIAUT
C
C       RECHERCHE DES POINTS DU FOND DE FISSURE DANS LE CADRE DE XFEM
C
C  ENTREES :
C     NOMA         :    NOM DE L'OBJET MAILLAGE
C     NMAFON       :    NOMBRE DE MAILLES DE LA ZONE FOND DE FISSURE
C     JMAFON       :    MAILLES DE LA ZONE FOND DE FISSURE
C     NXPTFF       :    NOMBRE MAXIMUM DE POINTS DU FOND DE FISSURE
C     CNSLT,CNSLN  :    LEVEL-SETS
C     CNXINV       :    CONNECTIVITE INVERSE
C     FISS         :    SD FISS_XFEM (POUR RECUP DES GRADIENTS)  
C
C  SORTIES :
C     JFON         :   ADRESSE DES POINTS DU FOND DE FISSURE
C     JBAS         :   ADRESSE DES DIRECTIONS DE PROPAGATION
C     NFON         :   NOMBRE DE POINTS DU FOND DE FISSURE
C     JBORD        :   ADRESSE DE L'ATTRIBUT LOGIQUE 'POINT DE BORD'
C     NPTBOR       :   NOMBRE DE POINTS 'DE BORD' DU FOND DE FISSURE
C     ARMIN        :   ARETE MINIMALE DU MAILLAGE
C
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
      CHARACTER*32    JEXNUM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      INTEGER         IN,IMA,IFT,I,J,IBID,IRET,ITYPMA,IBID2(6,4),NDIM
      INTEGER         NMAABS,NBF,NA,NB,NC,ND,NUNOA,NUNOB,NUNOC,NUNOD
      INTEGER         JCONX1,JCONX2,JCOOR,JLTSV,JLNSV,JMA,IPT,ADDIM
      INTEGER         JGT,JBAS,JGN,K,FA(6,4),IFQ,NUNOI,FT(12,3),NBFT
      INTEGER         NUNOJ,NNE,JDIRN,JDIRT,IBID3(12,3)
      REAL*8          LSTA,LSNA,LSTB,LSNB,LSTC,LSNC,L(2,2),DETL,LL(2,2)
      REAL*8          R8PREM,EPSI(2),A(3),B(3),C(3),D(3),MP(2),M(3),P(3)
      REAL*8          R8B,EPS3,PREC,PADIST,COORMA(8),RBID(3),RBID1
      REAL*8          G1A,G1B,G1C,G1D,G2A,G2B,G2C,G2D,XE(3),FF(27)
      REAL*8          EPS1,EPS2
      COMPLEX*16      C16B
      CHARACTER*8     K8BID,TYPMA,ELREF,ALIAS
      CHARACTER*19    NOMT19,MAI,GRLT,CHGRT,GRLN,CHGRN,DIRT,DIRN
      CHARACTER*24    PARA
      CHARACTER*32    JEXATR
      LOGICAL         DEJA,FABORD,CHGSGN
C ----------------------------------------------------------------------

      CALL JEMARQ()

C     PRÉCISION :
C      PREC=1.D-3
      PREC=100.D0*R8PREM()
      
      CALL JEVEUO(NOMA//'.COORDO    .VALE','L',JCOOR)
      CALL JEVEUO(NOMA//'.CONNEX','L',JCONX1)
      CALL JEVEUO(JEXATR(NOMA//'.CONNEX','LONCUM'),'L',JCONX2)
      CALL JEVEUO(CNSLT//'.CNSV','L',JLTSV)
      CALL JEVEUO(CNSLN//'.CNSV','L',JLNSV)
      MAI=NOMA//'.TYPMAIL'
      CALL JEVEUO(MAI,'L',JMA)

C     DIRECTION NORMALE ET TANGENTIELLE POUR LA PROPAGATION
      DIRT = '&&XPTFON.DIRT'
      DIRN = '&&XPTFON.DIRN'
      CALL WKVECT(DIRT,'V V R',3*NXPTFF,JDIRT)
      CALL WKVECT(DIRN,'V V R',3*NXPTFF,JDIRN)

C     GRADIENT LST
      GRLT = FISS//'.GRLTNO'
      CHGRT = '&&XPTFON.GRLN'
      CALL CNOCNS(GRLT,'V',CHGRT)
      CALL JEVEUO(CHGRT//'.CNSV','L',JGT)

C     GRADIENT LSN
      GRLN = FISS//'.GRLNNO'
      CHGRN = '&&XPTFON.GRLT'
      CALL CNOCNS(GRLN,'V',CHGRN)
      CALL JEVEUO(CHGRN//'.CNSV','L',JGN)

      CALL JEVEUO(NOMA//'.DIME','L',ADDIM)
      NDIM=ZI(ADDIM-1+6)

C     ON RÉCUPÈRE LA VALEUR DE LA PLUS PETITE ARETE DU MAILLAGE : ARMIN
      CALL LTNOTB(NOMA,'CARA_GEOM',NOMT19)
      PARA = 'AR_MIN                  '
      CALL TBLIVA(NOMT19,0,' ',IBID,R8B,C16B,K8BID,K8BID,R8B,PARA,K8BID,
     &            IBID,ARMIN,C16B,K8BID,IRET)
C     PROBLEME POUR RECUPERER AR_MIN DANS LA TABLE "CARA_GEOM"
      CALL ASSERT(IRET.EQ.0)
C     ARMIN NEGATIF OU NUL
      CALL ASSERT(ARMIN.GT.0)

      DO 100 I=1,NXPTFF
         ZL(JBORD-1+I)=.FALSE.
 100  CONTINUE

C     COMPTEUR : NOMBRE DE POINTS DE FONFIS TROUVÉS
      IN=0
C     COMPTEUR : NOMBRE DE POINTS DE FONFIS DE BORD TROUVES
      NPTBOR=0

C     BOUCLE SUR LES MAILLES DE MAFOND
      
      DO 400 IMA=1,NMAFON

        NMAABS=ZI(JMAFON-1+(IMA-1)+1)
        ITYPMA=ZI(JMA-1+NMAABS)
        CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ITYPMA),TYPMA)



        IF (TYPMA.NE.'HEXA8'.AND.TYPMA.NE.'HEXA20') THEN

        CALL CONFAC(TYPMA,FT,NBFT,IBID2,IBID)
        
C       BOUCLE SUR LES FACES TRIANGULAIRES
        DO 410 IFT=1,NBFT
          NA=FT(IFT,1)
          NB=FT(IFT,2)
          NC=FT(IFT,3)
          NUNOA=ZI(JCONX1-1+ZI(JCONX2+NMAABS-1)+NA-1)
          NUNOB=ZI(JCONX1-1+ZI(JCONX2+NMAABS-1)+NB-1)
          NUNOC=ZI(JCONX1-1+ZI(JCONX2+NMAABS-1)+NC-1)
          LSNA=ZR(JLNSV-1+(NUNOA-1)+1)
          LSNB=ZR(JLNSV-1+(NUNOB-1)+1)
          LSNC=ZR(JLNSV-1+(NUNOC-1)+1)
          LSTA=ZR(JLTSV-1+(NUNOA-1)+1)
          LSTB=ZR(JLTSV-1+(NUNOB-1)+1)
          LSTC=ZR(JLTSV-1+(NUNOC-1)+1)
          L(1,1)=LSNB-LSNA
          L(1,2)=LSNC-LSNA
          L(2,1)=LSTB-LSTA
          L(2,2)=LSTC-LSTA
          DETL=L(1,1)*L(2,2)-L(2,1)*L(1,2)
          IF (ABS(DETL).GT.R8PREM()) THEN
            LL(1,1)=L(2,2)/DETL
            LL(2,2)=L(1,1)/DETL
            LL(1,2)=-1*L(1,2)/DETL
            LL(2,1)=-1*L(2,1)/DETL
            EPS1=-LL(1,1)*LSNA-LL(1,2)*LSTA
            EPS2=-LL(2,1)*LSNA-LL(2,2)*LSTA
            EPS3=1.D0-(EPS1+EPS2)
            DO 411 I=1,NDIM
              A(I)=ZR(JCOOR-1+3*(NUNOA-1)+I)
              B(I)=ZR(JCOOR-1+3*(NUNOB-1)+I)
              C(I)=ZR(JCOOR-1+3*(NUNOC-1)+I)
              M(I)=A(I)+EPS1*(B(I)-A(I))+EPS2*(C(I)-A(I))
 411        CONTINUE
            IF (NDIM.LT.3) THEN
              A(3)=0.D0
              B(3)=0.D0
              C(3)=0.D0
              M(3)=0.D0
              P(3)=0.D0
            ENDIF   
C           ON CONTINUE SSI M EST DANS LE TRIANGLE
            IF (-PREC.LE.EPS1.AND.EPS1.LE.1.D0+PREC.AND.
     &          -PREC.LE.EPS2.AND.EPS2.LE.1.D0+PREC.AND.
     &          -PREC.LE.EPS3.AND.EPS3.LE.1.D0+PREC) THEN

C             VÉRIFICATION SI CE POINT A DÉJÀ ÉTÉ TROUVÉ
              DEJA=.FALSE.
               DO 412 J=1,IN
                  DO 4120 I=1,3
                     P(I)=ZR(JFON-1+4*(J-1)+I)
 4120             CONTINUE
                IF (PADIST(3,P,M).LT.(ARMIN*1.D-2)) THEN
                  DEJA=.TRUE.
                  IPT=J
                ENDIF
 412          CONTINUE
              IF (.NOT.DEJA) THEN
C               CE POINT N'A PAS DÉJÀ ÉTÉ TROUVÉ, ON LE GARDE
                IN=IN+1
                IPT=IN
C               AUGMENTER NXPTFF
                CALL ASSERT(IN.LT.NXPTFF)

                ZR(JFON-1+4*(IN-1)+1)=M(1)
                ZR(JFON-1+4*(IN-1)+2)=M(2)
                ZR(JFON-1+4*(IN-1)+3)=M(3)

C          DIRECTION DE PROPA
                DO 425 K=1,NDIM
                  G1A = ZR(JGT-1+NDIM*(NUNOA-1)+K)
                  G1B = ZR(JGT-1+NDIM*(NUNOB-1)+K)
                  G1C = ZR(JGT-1+NDIM*(NUNOC-1)+K)
                  ZR(JDIRT-1+(IN-1)*NDIM+K)=
     &                            G1A+EPS1*(G1B-G1A)+EPS2*(G1C-G1A)
                  G2A = ZR(JGN-1+NDIM*(NUNOA-1)+K)
                  G2B = ZR(JGN-1+NDIM*(NUNOB-1)+K)
                  G2C = ZR(JGN-1+NDIM*(NUNOC-1)+K)
                  ZR(JDIRN-1+(IN-1)*NDIM+K)=
     &                            G2A+EPS1*(G2B-G2A)+EPS2*(G2C-G2A)
 425            CONTINUE

              ENDIF

C             ON VERIFIE SI LA FACE COURANTE EST UNE FACE DE BORD
C             CELA N'A DE SENS QU'EN 3D
              IF (NDIM.EQ.3) THEN
                 CALL XFABOR(NOMA,CNXINV,NUNOA,NUNOB,NUNOC,FABORD)
                 IF (FABORD) THEN
                    ZL(JBORD-1+IPT)=.TRUE.
                    NPTBOR=NPTBOR+1
                 ENDIF
              ENDIF
            ENDIF

          ENDIF
 410    CONTINUE



      ELSE

C     Interpolation bilinéaire
        CALL CONFAC(TYPMA,IBID3,IBID,FA,NBF)
C     BOUCLE SUR LES FACES QUADRANGULAIRES
        DO 610 IFQ=1,NBF
         NA=FA(IFQ,1)
         NB=FA(IFQ,2)
         NC=FA(IFQ,3)
         ND=FA(IFQ,4)
         NUNOA=ZI(JCONX1-1+ZI(JCONX2+NMAABS-1)+NA-1)
         NUNOB=ZI(JCONX1-1+ZI(JCONX2+NMAABS-1)+NB-1)
         NUNOC=ZI(JCONX1-1+ZI(JCONX2+NMAABS-1)+NC-1)
         NUNOD=ZI(JCONX1-1+ZI(JCONX2+NMAABS-1)+ND-1)
C     On introduit un booléen pour ne s'intéresser qu'aux faces ou lst
C     et lsn changent conjointement de signe         
         CHGSGN = .FALSE.

           
         DO 611 I=1,4
          NUNOI=ZI(JCONX1-1+ZI(JCONX2+NMAABS-1)+FA(IFQ,I)-1)
          COORMA(2*I-1)=ZR(JLTSV-1+(NUNOI-1)+1)
          COORMA(2*I)=ZR(JLNSV-1+(NUNOI-1)+1)          
          IF (I.GT.1) THEN
           DO 6100 J=1,(I-1)
            IF (((ABS(COORMA(2*I-1)-COORMA(2*J-1)).LT.PREC).AND.
     &      ((COORMA(2*I-1)*COORMA(2*J-1)).GE.0.D0)).AND.
     &      ((ABS(COORMA(2*I)-COORMA(2*J)).LT.PREC).AND.
     &       ((COORMA(2*I)*COORMA(2*J)).GE.0.D0))) GOTO 610
            IF (((COORMA(2*I-1)*COORMA(2*J-1)).LE.0.D0).AND.
     &      ((COORMA(2*I)*COORMA(2*J)).LE.0.D0)) THEN
               CHGSGN = .TRUE.   
            ENDIF   
 6100      CONTINUE    
          ENDIF   
 611     CONTINUE
 
C        Controle pour verifier qu'on n'est pas sur l'iso-zero de lsn
         IF ((COORMA(2).EQ.0.D0).AND.(COORMA(4).EQ.0.D0).AND.
     &(COORMA(6).EQ.0.D0).AND.(COORMA(8).EQ.0.D0)) GOTO 610
              
         IF (.NOT. CHGSGN) GOTO 610
C        On cherche sur la maille le point correspondant à lsn=lst=0
C        Mais, attention! Il faut chercher dans les mailles sécantes
C        au plan de fissure, sinon, on aura par ex ls(A)=Ls(D)
C        D'où le test de la boucle 6100.
         MP(1)=0.D0
         MP(2)=0.D0
         ALIAS='QU4'

        CALL REEREG('C',ALIAS,4,COORMA,MP,2,EPSI,IRET)
        IF (IRET.EQ.1) GOTO 610
        IF ((ABS(EPSI(1)).GT.1.D0).OR.(ABS(EPSI(2)).GT.1.D0)) GOTO 610
C       Si MP est dans le quadrangle, 
C       on lui affecte les coordonnées paramétriques trouvées        
        EPS1=EPSI(1)
        EPS2=EPSI(2)                      
C       On doit maintenant multiplier les coord. param. de M par chacune
C       des FF des noeuds de l'élément pour obtenir les coord. cart.
         CALL ELRFVF('QU4',EPSI,4,FF,NNE)
         DO 6120 I=1,NDIM
          A(I)=ZR(JCOOR-1+3*(NUNOA-1)+I)
          B(I)=ZR(JCOOR-1+3*(NUNOB-1)+I)
          C(I)=ZR(JCOOR-1+3*(NUNOC-1)+I)
          D(I)=ZR(JCOOR-1+3*(NUNOD-1)+I)
          M(I)=A(I)*FF(1)+B(I)*FF(2)+C(I)*FF(3)+D(I)*FF(4)              
 6120    CONTINUE          

C             VÉRIFICATION SI CE POINT A DÉJÀ ÉTÉ TROUVÉ
              DEJA=.FALSE.
               DO 612 J=1,IN
                P(1)=ZR(JFON-1+4*(J-1)+1)
                P(2)=ZR(JFON-1+4*(J-1)+2)
                P(3)=ZR(JFON-1+4*(J-1)+3)
C                IF (PADIST(3,P,M).LT.(ARMIN*1.D-2)) THEN
                IF (PADIST(3,P,M).LT.(ARMIN*0.3D0)) THEN
                  DEJA=.TRUE.
                  IPT=J
                ENDIF
 612          CONTINUE
              IF (.NOT.DEJA) THEN
C               CE POINT N'A PAS DÉJÀ ÉTÉ TROUVÉ, ON LE GARDE
                IN=IN+1
                IPT=IN
C               AUGMENTER NXPTFF
                CALL ASSERT(IN.LT.NXPTFF)

                ZR(JFON-1+4*(IN-1)+1)=M(1)
                ZR(JFON-1+4*(IN-1)+2)=M(2)
                ZR(JFON-1+4*(IN-1)+3)=M(3)

C          DIRECTION DE PROPA
                DO 625 K=1,NDIM
                  G1A = ZR(JGT-1+NDIM*(NUNOA-1)+K)
                  G1B = ZR(JGT-1+NDIM*(NUNOB-1)+K)
                  G1C = ZR(JGT-1+NDIM*(NUNOC-1)+K)
                  G1D = ZR(JGT-1+NDIM*(NUNOD-1)+K)
                  ZR(JDIRT-1+(IN-1)*NDIM+K)=
     &                            G1A+EPS1*(G1B-G1A)+EPS2*(G1D-G1A)
                  G2A = ZR(JGN-1+NDIM*(NUNOA-1)+K)
                  G2B = ZR(JGN-1+NDIM*(NUNOB-1)+K)
                  G2C = ZR(JGN-1+NDIM*(NUNOC-1)+K)
                  G2D = ZR(JGN-1+NDIM*(NUNOD-1)+K)
                  ZR(JDIRN-1+(IN-1)*NDIM+K)=
     &                            G2A+EPS1*(G2B-G2A)+EPS2*(G2D-G2A)
 625            CONTINUE

              ENDIF

C             ON VERIFIE SI LA FACE COURANTE EST UNE FACE DE BORD
C             CELA N'A DE SENS QU'EN 3D
              IF (NDIM.EQ.3) THEN
                 CALL XFABOR(NOMA,CNXINV,NUNOA,NUNOB,NUNOC,FABORD)
                 IF (FABORD) THEN
                    ZL(JBORD-1+IPT)=.TRUE.
                    NPTBOR=NPTBOR+1
                 ENDIF
              ENDIF
 610    CONTINUE

      ENDIF

 400  CONTINUE
      
      NFON=IN

C     STOCKAGE DE LA DIRECTION DE PROPA
      DO 555 I=1,NFON
        DO 556 K=1,NDIM
          ZR(JBAS-1+2*NDIM*(I-1)+K)     =ZR(JDIRN-1+(I-1)*NDIM+K)
          ZR(JBAS-1+2*NDIM*(I-1)+K+NDIM)=ZR(JDIRT-1+(I-1)*NDIM+K)
 556    CONTINUE
 555  CONTINUE
     
      CALL JEDETR(DIRN)
      CALL JEDETR(DIRT)      
      CALL JEDEMA()
      END
