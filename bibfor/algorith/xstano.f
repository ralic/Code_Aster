      SUBROUTINE XSTANO(NOMA,LISNO,NMAFIS,JMAFIS,CNSLT,CNSLN,RAYON,
     &                  STANO)
      IMPLICIT NONE 

      REAL*8        RAYON
      INTEGER       NMAFIS,JMAFIS
      CHARACTER*8   NOMA
      CHARACTER*19  CNSLT,CNSLN
      CHARACTER*24  LISNO,STANO
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 22/08/2006   AUTEUR MASSIN P.MASSIN 
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
C                DETERMINER LE STATUT (ENRICHISSEMENT) DES NOEUDS
C                    1 : ENRICHISSEMENT HEAVISIDE
C                    2 : ENRICHISSEMENT CRACK TIP
C                    3 : ENRICHISSEMENT HEAVISIDE ET CRACK TIP
C
C     ENTREE
C         NOMA   : NOM DE L'OBJET MAILLAGE
C         LISNO  : LISTE DES NOEUDS DE GROUP_MA_ENRI
C         NMAFIS : NOMBRE DE MAILLES DE LA ZONE FISSURE
C         JMAFIS : ADRESSE DES MAILLES DE LA ZONE FISSURE
C         CNSLT  : LEVEL SET TANGENTE
C         CNSLN  : LEVEL SET NORMALE
C         RAYON  : RAYON DE LA ZONE D'ENRICHISSEMENT DES NOEUDS EN FOND
C                  DE FISSURE
C         STANO  : VECTEUR STATUT DES NOEUDS INITIALISE À 0
C
C     SORTIE
C         STANO  : VECTEUR STATUT DES NOEUDS
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
      INTEGER         INO,IMAE,IN,AR(12,2),IA,I,NNOS,NBNOE
      INTEGER         PARE,NABS,NMAABS,NBNOMA,NUNO,NRIEN,NBAR,NA
      INTEGER         NB,NUNOA,NUNOB,ENR,ENR1,ENR2,JDLINO,JMA,JSTANO
      INTEGER         JCONX1,JCONX2,JLTSV,JLNSV,JCOOR,ITYPMA,NDIM,ADDIM
      REAL*8          MINLSN,MINLST,MAXLSN,MAXLST,LSNA,LSNB,LSTA,LSTB
      REAL*8          LSNC,LSTC,LSN,A(3),B(3),C(3),R8MAEM,LST
      CHARACTER*8     TYPMA,K8B
      CHARACTER*19    MAI
      CHARACTER*32    JEXATR
      LOGICAL         MARE
C ----------------------------------------------------------------------

      CALL JEMARQ()

      CALL JEVEUO(NOMA//'.CONNEX','L',JCONX1)
      CALL JEVEUO(JEXATR(NOMA//'.CONNEX','LONCUM'),'L',JCONX2)
      
      CALL JELIRA(LISNO,'LONMAX',NBNOE,K8B)
      CALL JEVEUO(LISNO,'L',JDLINO)
      
      CALL JEVEUO(STANO,'E',JSTANO)
      
      CALL JEVEUO(CNSLT//'.CNSV','L',JLTSV)
      CALL JEVEUO(CNSLN//'.CNSV','L',JLNSV)
      
      CALL JEVEUO(NOMA//'.COORDO    .VALE','L',JCOOR)      
      MAI=NOMA//'.TYPMAIL'
      CALL JEVEUO(MAI,'L',JMA)
      CALL JEVEUO(NOMA//'.DIME','L',ADDIM)
      NDIM=ZI(ADDIM-1+6)

C     BOUCLE SUR LES NOEUDS DE GROUP_ENRI
      DO 200 INO=1,NBNOE
        PARE=0
        MAXLSN=-1*R8MAEM()
        MINLSN=R8MAEM()
        MAXLST=-1*R8MAEM()
        MINLST=R8MAEM()
        NABS=ZI(JDLINO-1+(INO-1)+1)

C       BOUCLE SUR LES MAILLES DE MAFIS
        DO 210 IMAE=1,NMAFIS
          MARE=.FALSE.
          NMAABS=ZI(JMAFIS-1+(IMAE-1)+1)
C         NOMBRE DE NOEUDS DE LA MAILLE TRAITEE
          NBNOMA=ZI(JCONX2+NMAABS) - ZI(JCONX2+NMAABS-1)

          DO 211 IN=1,NBNOMA
            NUNO=ZI(JCONX1-1+ZI(JCONX2+NMAABS-1)+IN-1)
            IF (NABS.EQ.NUNO) THEN
              MARE=.TRUE.
            ENDIF
 211      CONTINUE
C         SI MAILLE RETENUE : ON CALCULE LST QUE SUR LES PTS OÙ LSN=0
C                             ON CALCULE LSN SUR LES NOEUDS
          IF (MARE) THEN
            NRIEN=0
C           BOUCLE SUR LES ARETES DE LA MAILLE RETENUE
            ITYPMA=ZI(JMA-1+NMAABS)
            CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ITYPMA),TYPMA)
            CALL CONARE(TYPMA,AR,NBAR)
            DO 212 IA=1,NBAR
              NA=AR(IA,1)
              NB=AR(IA,2)
              NUNOA=ZI(JCONX1-1+ZI(JCONX2+NMAABS-1)+NA-1)
              NUNOB=ZI(JCONX1-1+ZI(JCONX2+NMAABS-1)+NB-1)             
              LSNA=ZR(JLNSV-1+(NUNOA-1)+1)
              LSNB=ZR(JLNSV-1+(NUNOB-1)+1)
              LSTA=ZR(JLTSV-1+(NUNOA-1)+1)
              LSTB=ZR(JLTSV-1+(NUNOB-1)+1)
              

              IF (LSNA.EQ.0.D0.AND.LSNB.EQ.0.D0) THEN
C               ON RETIENT LES 2 POINTS A ET B
C               ET ACTUALISATION DE MIN ET MAX POUR LST
                IF (LSTA.LT.MINLST) MINLST=LSTA
                IF (LSTA.GT.MAXLST) MAXLST=LSTA
                IF (LSTB.LT.MINLST) MINLST=LSTB
                IF (LSTB.GT.MAXLST) MAXLST=LSTB
              ELSEIF((LSNA*LSNB).LE.0.D0) THEN
C              CA VEUT DIRE QUE LSN S'ANNULE SUR L'ARETE AU PT C
C              (RETENU) ET ACTUALISATION DE MIN ET MAX POUR LST EN CE PT
                DO 21 I=1,NDIM
                  A(I)=ZR(JCOOR-1+3*(NUNOA-1)+I)
                  B(I)=ZR(JCOOR-1+3*(NUNOB-1)+I)
C                 INTERPOLATION DES COORDONNÉES DE C ET DE LST EN C
                  C(I)=A(I)-LSNA/(LSNB-LSNA)*(B(I)-A(I))
 21             CONTINUE
                IF (A(1).NE.B(1)) THEN
                  LSTC=LSTA+(LSTB-LSTA)*(C(1)-A(1))/(B(1)-A(1))
                ELSEIF (A(2).NE.B(2)) THEN
                  LSTC=LSTA+(LSTB-LSTA)*(C(2)-A(2))/(B(2)-A(2))
                ELSEIF (NDIM .EQ. 3) THEN
                  IF (A(3).NE.B(3))
     &            LSTC=LSTA+(LSTB-LSTA)*(C(3)-A(3))/(B(3)-A(3))
                ENDIF
                IF (LSTC.LT.MINLST) MINLST=LSTC
                IF (LSTC.GT.MAXLST) MAXLST=LSTC
              ELSE
C               AUCUN POINT DE L'ARETE N'A LSN = 0,ALORS ON RETIENT RIEN
                NRIEN=NRIEN+1
              ENDIF
              IF (NRIEN.EQ.NBAR) CALL UTMESS('E','XSTANO','AUCUNE '//
     &                               'ARETE SUR LAQUELLE LSN S ANNULE')
 212        CONTINUE

            IF (TYPMA(1:6).EQ.'HEXA20'.OR.TYPMA(1:7).EQ.'PENTA15'.
     &          OR.TYPMA(1:7).EQ.'TETRA10') THEN
C             MAILLE QUADRAT : NB NOEUDS SOMMETS = 2/5 x NB NOEUDS TOTAL
              NNOS=2*NBNOMA/5
            ELSEIF (TYPMA(1:5).EQ.'QUAD8'.OR.TYPMA(1:5).EQ.'TRIA6') THEN
C             MAILLE QUADRAT 2D:NB NOEUDS SOMMETS =1/2 x NB NOEUDS TOTAL
              NNOS=NBNOMA/2
            ELSE
              NNOS=NBNOMA
            ENDIF
C           BOUCLE SUR LES NOEUDS SOMMETS DE LA MAILLE RETENUE
            DO 213 IN=1,NNOS
              NUNO=ZI(JCONX1-1+ZI(JCONX2+NMAABS-1)+IN-1)
              LSN=ZR(JLNSV-1+(NUNO-1)+1)
              IF (LSN.LT.MINLSN) MINLSN=LSN
              IF (LSN.GT.MAXLSN) MAXLSN=LSN
 213        CONTINUE          
          ELSE
C           MAILLE PAS RETENUE
            PARE=PARE+1
          ENDIF
 210    CONTINUE

        ENR=0
        ENR1=0
        ENR2=0

C       TEST S'IL Y A EU UNE MAILLE DE VOISINAGE TROUVÉE
        IF (PARE.LT.NMAFIS) THEN
         IF ((MINLSN*MAXLSN.LT.0.D0).AND.(MAXLST.LE.0.D0))        ENR1=1
         IF ((MINLSN*MAXLSN.LE.0.D0).AND.(MINLST*MAXLST.LE.0.D0)) ENR2=2
        ENDIF
        
C       SI ON DEFINIT UN RAYON POUR LA ZONE D'ENRICHISSEMENT SINGULIER
        IF (RAYON.GT.0.D0) THEN
          LSN=ZR(JLNSV-1+(NABS-1)+1)
          LST=ZR(JLTSV-1+(NABS-1)+1)
          IF (SQRT(LSN**2+LST**2) .LE . RAYON) ENR2=2       
        ENDIF

        ENR=ENR1+ENR2

C       ENREGISTREMENT DU STATUT DU NOEUD
        ZI(JSTANO-1+(NABS-1)+1)=ENR

 200  CONTINUE


      CALL JEDEMA()
      END
