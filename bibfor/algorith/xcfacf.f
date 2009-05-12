      SUBROUTINE XCFACF(JPTINT,PTMAX,IPT,JAINT,LSN,LST,IGEOM,NNO,NDIM,
     &                                                          TYPMA)
      IMPLICIT NONE

      INTEGER       JPTINT,PTMAX,IPT,JAINT,IGEOM,NNO,NDIM
      REAL*8        LSN(NNO),LST(NNO)
      CHARACTER*8   TYPMA

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 12/05/2009   AUTEUR MAZET S.MAZET 
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
C RESPONSABLE GENIAUT S.GENIAUT
C              TROUVER LES PTS D'INTERSECTION ENTRE LE FOND DE FISSURE
C                 ET LES FACES POUR LES ELEMENTS EN FOND DE FISSURE
C
C     ENTREE
C       JPTINT   : ADRESSE DES COORDONNEES DES POINTS D'INTERSECTION
C       PTMAX    : NOMBRE MAX DE POINTS D'INTERSECTION
C       IPT      : COMPTEUR DE NOMBRE DE POINTS D'INTERSECTION
C       JAINT    : ADRESSE DES INFOS SUR LES ARETES ASSOCIEES
C       LSN      : VALEURS DE LA LEVEL SET NORMALE
C       LST      : VALEURS DE LA LEVEL SET TANGENTE
C       IGEOM    : ADRESSE DES COORDONNEES DES NOEUDS DE L'ELT PARENT
C       NNO      : NOMBRE DE NOEUDS DE L'ELEMENT
C       NDIM     : DIMENSION DE L'ESPACE
C       TYPMA    : TYPE DE LA MAILLE ASSOCIEE A L'ELEMENT
C
C     SORTIE
C       JPTINT   : ADRESSE DES COORDONNEES DES POINTS D'INTERSECTION
C       IPT      : COMPTEUR DE NOMBRE DE POINTS D'INTERSECTION
C       JAINT    : ADRESSE DES INFOS SUR LES ARETES ASSOCIEES
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
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      CHARACTER*8   ELREF,ALIAS
      CHARACTER*1   CBID
      REAL*8        R8MAEM,MAXLSN,MINLSN,MAXLST,MINLST,L(2,2),DETL
      REAL*8        R8PREM,LL(2,2),EPSI(2),A(3),B(3),C(3),D(3),MP(2)
      REAL*8        LONCAR,PADIST,DMIN,DIST,COORMA(8),RBID(3),RBID1,DST
      REAL*8        XE(3),EPS(6),GRAD(3,3),FF(27),M(3),PREC,EPS1,EPS2
      REAL*8        LSTA,LSTB,LSNA,LSNB
      INTEGER       I,NBF,IBID2(12,3),IBID,IFQ,NA,NB,NC,ND,J,JMIN
      INTEGER       FA(6,4),IFM,IRET,FT(12,3),NBFT,IFT,NNE,IBID3(12,3) 
      LOGICAL       CHGSGN     
      INTEGER       ZXAIN,XXMMVD
C ----------------------------------------------------------------------

      CALL JEMARQ()
      ZXAIN = XXMMVD('ZXAIN')

      CALL ELREF1(ELREF)
C     INITIALISATION DES MIN ET MAX
      MAXLSN=-1.D0*R8MAEM()
      MINLSN=R8MAEM()
      MAXLST=-1.D0*R8MAEM()
      MINLST=R8MAEM()
      PREC=1.D-3
    
C     RECHERCHE DU MIN ET MAX DES LEVEL SETS SUR LES NOEUDS
      DO 100 I=1,NNO
        MAXLSN=MAX(LSN(I),MAXLSN)
        MAXLST=MAX(LST(I),MAXLST)
        MINLSN=MIN(LSN(I),MINLSN)
        MINLST=MIN(LST(I),MINLST)
 100  CONTINUE

C     SI CE N'EST PAS UN ELEMENT EN FOND DE FISSURE, ON SORT
      IF (MINLSN*MAXLSN.GE.0.D0.OR.MINLST*MAXLST.GE.0.D0) GOTO 9999

C     On introduit un test permettant de beneficier des
C     FF bilinéaires du quadrangle et non plus des FF linéaires du
C     triangle lorsque la maille est hexaédrique

C     RECHERCHE DES INTERSECTION ENTRE LE FOND DE FISSURE ET LES FACES
C     (COMME DANS XPTFON.F)
C


      IF (TYPMA.NE.'HEXA8'.AND.TYPMA.NE.'HEXA20') THEN

       CALL CONFAC(TYPMA,FT,NBFT,IBID2,IBID)
      
C     BOUCLE SUR LES FACES TRIANGULAIRES
       DO 410 IFT=1,NBFT

        NA=FT(IFT,1)
        NB=FT(IFT,2)
        NC=FT(IFT,3)
        L(1,1)=LSN(NB)-LSN(NA)
        L(1,2)=LSN(NC)-LSN(NA)
        L(2,1)=LST(NB)-LST(NA)
        L(2,2)=LST(NC)-LST(NA)
        DETL=L(1,1)*L(2,2)-L(2,1)*L(1,2)
        IF (ABS(DETL).LE.R8PREM()) GOTO 410
        LL(1,1)=L(2,2)/DETL
        LL(2,2)=L(1,1)/DETL
        LL(1,2)=-1*L(1,2)/DETL
        LL(2,1)=-1*L(2,1)/DETL
        EPS1=-LL(1,1)*LSN(NA)-LL(1,2)*LST(NA)
        EPS2=-LL(2,1)*LSN(NA)-LL(2,2)*LST(NA)
        DO 411 I=1,NDIM
          A(I)=ZR(IGEOM-1+NDIM*(NA-1)+I)
          B(I)=ZR(IGEOM-1+NDIM*(NB-1)+I)
          C(I)=ZR(IGEOM-1+NDIM*(NC-1)+I)
          M(I)=A(I)+EPS1*(B(I)-A(I))+EPS2*(C(I)-A(I))
 411    CONTINUE

C       ON SORT SI M N'EST DANS LE TRIANGLE
        IF (0.D0.GT.EPS1.OR.EPS1.GT.1.D0 .OR.
     &      0.D0.GT.EPS2.OR.EPS2.GT.1.D0 .OR.
     &      0.D0.GT.(EPS1+EPS2).OR.(EPS1+EPS2).GT.1.D0) GOTO 410

C       Modif pour ignorer les points confondus avec ceux 
C       detectes dans xcface lorsque le pt est exactt sur une arete
        DO 412 J=1,IPT
         DST=PADIST(NDIM,M,ZR(JPTINT-1+NDIM*(J-1)+1))
         IF (DST.LE.R8PREM()) GOTO 410
 412    CONTINUE
C       Fin modif Julien          

C       LONGUEUR CARACTERISTIQUE
         LONCAR=(PADIST(NDIM,A,B)+PADIST(NDIM,A,C))/2.D0
C         WRITE(IFM,*) 'Coordonnées cartésiennes de M', M

C       ON AJOUTE A LA LISTE LE POINT M
         CALL XAJPIN(JPTINT,PTMAX,IPT,IBID,M,LONCAR,JAINT,-1,-1,0.D0)

 410   CONTINUE

      ELSE
        
        CALL CONFAC(TYPMA,IBID3,IBID,FA,NBF)
C     BOUCLE SUR LES FACES QUADRANGULAIRES
        DO 610 IFQ=1,NBF
C     On introduit un compteur pour ne s'interesser qu'aux faces ou lst
C     et lsn changent conjointement de signe..  
         CHGSGN = .FALSE.         
         NA=FA(IFQ,1)
         NB=FA(IFQ,2)
         NC=FA(IFQ,3)
         ND=FA(IFQ,4)

         DO 611 I=1,4
          COORMA(2*I-1)=LST(FA(IFQ,I))
          COORMA(2*I)=LSN(FA(IFQ,I))
          IF (I.GT.1) THEN


           DO 6100 J=1,(I-1)         
            IF (((ABS(LST(FA(IFQ,I))-LST(FA(IFQ,J))).LT.PREC).AND.
     &       (LST(FA(IFQ,I))*LST(FA(IFQ,J))).GE.0.D0).AND.
     &      ((ABS(LSN(FA(IFQ,I))-LSN(FA(IFQ,J))).LT.PREC).AND.
     &      (LSN(FA(IFQ,I))*LSN(FA(IFQ,J))).GE.0.D0)) GOTO 610
            IF (((LST(FA(IFQ,I))*LST(FA(IFQ,J))).LE.0.D0).AND.
     &      ((LSN(FA(IFQ,I))*LSN(FA(IFQ,J))).LE.0.D0)) THEN
               CHGSGN = .TRUE.
            ENDIF   
 6100      CONTINUE    
          ENDIF   
 611     CONTINUE

              
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
        MP(1)=EPSI(1)
        MP(2)=EPSI(2)                    
C       On doit maintenant multiplier les coord. param. de M par chacune
C       des FF des noeuds de l'élément pour obtenir les coord. cart.
         CALL ELRFVF('QU4',MP,4,FF,NNE)
         DO 612 I=1,NDIM
          A(I)=ZR(IGEOM-1+NDIM*(NA-1)+I)
          B(I)=ZR(IGEOM-1+NDIM*(NB-1)+I)
          C(I)=ZR(IGEOM-1+NDIM*(NC-1)+I)
          D(I)=ZR(IGEOM-1+NDIM*(ND-1)+I)
          M(I)=A(I)*FF(1)+B(I)*FF(2)+C(I)*FF(3)+D(I)*FF(4)    
 612     CONTINUE          

C       Modif pour ignorer les points confondus avec ceux 
C       detectes dans xcface lorsque le pt est exactt sur une arete
        DO 613 J=1,IPT
         DST=PADIST(NDIM,M,ZR(JPTINT-1+NDIM*(J-1)+1))
         IF (DST.LE.R8PREM()) THEN
          GOTO 610
         ENDIF
 613    CONTINUE  

C       LONGUEUR CARACTERISTIQUE
         LONCAR=(PADIST(NDIM,A,B)+PADIST(NDIM,A,C))/2.D0

C       ON AJOUTE A LA LISTE LE POINT M
         CALL XAJPIN(JPTINT,PTMAX,IPT,IBID,M,LONCAR,JAINT,-1,-1,0.D0)

 610  CONTINUE
  
      ENDIF
  
C     POUR UN RACCORD CONTACT (BOOK VI 17/03/2006)
C     ON MET LES LAMBDAS DU FOND EGAUX AU LAMBDA NODAL LE PLUS PROCHE
      DO 510 I=1,IPT

C       ON SE RESTREINT AUX LAMBDA DU FOND
        IF (ZR(JAINT-1+ZXAIN*(I-1)+1).NE.-1.D0) GOTO 510
 
C       RECHERCHE DU LAMBDA NODAL LE PLUS PROCHE 
        DMIN=R8MAEM()
        DO 511 J=1,IPT
          IF (J.EQ.I) GOTO 511
          IF (ZR(JAINT-1+ZXAIN*(J-1)+1).EQ.-1.D0 .OR.
     &        ZR(JAINT-1+ZXAIN*(J-1)+2).EQ.-1.D0 )  GOTO 511
          DIST=PADIST(NDIM,ZR(JPTINT-1+NDIM*(I-1)+1),
     &                  ZR(JPTINT-1+NDIM*(J-1)+1))
          IF (DIST.LE.DMIN) THEN
             DMIN=DIST
             JMIN=J
          ENDIF
 511    CONTINUE
  
C       COPIE DU VECTEUR JAINT 
C       MAIS ON LAISSE UNE INDIC -1 POUR POUVOIR ENCORE RECONNAITRE
C       UN POINT DU FOND
        IF (ZR(JAINT-1+ZXAIN*(JMIN-1)+1).GT.0)
     &    ZR(JAINT-1+ZXAIN*(I-1)+1)=ZR(JAINT-1+ZXAIN*(JMIN-1)+1)
        IF (ZR(JAINT-1+ZXAIN*(JMIN-1)+2).GT.0)
     &    ZR(JAINT-1+ZXAIN*(I-1)+2)=ZR(JAINT-1+ZXAIN*(JMIN-1)+2)
        ZR(JAINT-1+ZXAIN*(I-1)+3)=ZR(JAINT-1+ZXAIN*(JMIN-1)+3)
        ZR(JAINT-1+ZXAIN*(I-1)+4)=ZR(JAINT-1+ZXAIN*(JMIN-1)+4)

 510  CONTINUE
 
C --------------------------- FIN ------------------------------------- 
 
C      ON SUPPRIME LES -1 RESTANT
       DO 512 I=1,IPT
          DO 513 J=1,2
             IF (ZR(JAINT-1+ZXAIN*(I-1)+J).EQ.-1.D0) THEN
               ZR(JAINT-1+ZXAIN*(I-1)+J)=0.D0
             ENDIF 
 513      CONTINUE
 512   CONTINUE

 9999 CONTINUE

      CALL JEDEMA()
      END
