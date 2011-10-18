      SUBROUTINE   XPRVIR(FISS,COVIR,BAVIR,VITVIR,ANGVIR,NUMVIR,NUMFON,
     &            NVIT,NBETA,NBPTFF,RADIMP,RADTOR,DAMAX,NOMA,LOCDOM)  

      IMPLICIT NONE
      CHARACTER*8    FISS,NOMA

      CHARACTER*19   COVIR,BAVIR,VITVIR,ANGVIR,NUMVIR            
      CHARACTER*24   NVIT,NBETA
      INTEGER        NUMFON,NBPTFF
      REAL*8         RADIMP,RADTOR,DAMAX
      LOGICAL        LOCDOM

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 18/10/2011   AUTEUR COLOMBO D.COLOMBO 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE COLOMBO D.COLOMBO
C TOLE CRP_20

C
C      XPRVIR   : X-FEM PROPAGATION :CREATION DU FOND DE FISSURE VIRTUEL
C      ------     -     --                                  ---
C    CREATION D'UN FOND DE FISSURE VIRTUEL DANS LE CAS DE L'UTILISATION 
C    DE LA METHODE UPWIND  
C
C    ENTREE
C        NOMA    : NOM DU CONCEPT MAILLAGE
C        FISS    : NOM DU CONCEPT FISSURE X-FEM
C                  (FISSURE INITIALE DONT ON EXTRAIT LE FOND DE FISSURE)
C       NVIT     : VECTEUR DES VITESSES DE PROPAGATION POUR CHAQUE POINT
C                  DU FOND DE LA FISSURE (NOM DU CONCEPT)
C       NBETA    : VECTEUR DES ANGLES DE PROPAGATION POUR CHAQUE POINT
C                  DU FOND DE LA FISSURE (NOM DU CONCEPT)
C       NUMFON   : NOMBRE DE POINT DU FOND DE FISSURE
C       RADIMP   : RAYON DE LA ZONE DE REACTUALISATION DES LEVELS SETS
C       RADTOR   : RAYON DE LA ZONE DE REPROJECTION DES LEVELS SETS
C       DAMAX    : AVANCEMENT MAXIMUM DU FRONT DE FISSURE
C       LOCDOM   : LOCALISATION DES LEVELS SETS ACTIVEE
C       NBPTFF   : NOMBRE DE POINT DU FOND DE FISSURE
C
C    SORTIE
C        NUMVIR  : VECTEUR CONTENANT LES POINTS DELIMITANT LES 
C                  DIFFERENTS FRONTS DE FISSURE
C        COVIR   : VECTEUR CONTENANT LES COORDONNEES DES POINTS DU FOND 
C                   VIRTUEL
C        BAVIR   : VECTEUR CONTENANT LES BASES LOCALES DES POINTS DU 
C                  FOND  VIRTUEL
C        VIRVIR  : VECTEUR CONTENANT LES VITESSES DES POINTS DU FOND 
C                   VIRTUEL
C        ANGVIR  : VECTEUR CONTENANT LES ANGLES DE PROPAGATION DES 
C                  POINTS DU FOND VIRTUEL
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

      INTEGER        I,J,JCOOR,IRET,NBNO,JFONF,IFM,NIV,
     &               JVIT,JBETA,CFV,BFV,VFV,AFV,CFVPR,BFVPR,
     &               VFVPR,AFVPR,NFV,NPOIN,NFVPR
      REAL*8         V1,V2,AI,AJ,AK,AL,DA1,DA2,A1,A2
      CHARACTER*8    K8B
      INTEGER        JBASEF,K

      REAL*8         PI(3),PJ(3),PK(3),PL(3),PIJ(3),PKL(3),P1(3),P2(3),
     &               VI,VJ,VK,VL,DV1,DV2,NORMIJ,
     &               NORMKL,NORMJ1,NORMK2,NORMJK
                  
C     MULTIPLE CRACK FRONTS
      INTEGER        JFMULT

C-----------------------------------------------------------------------
C     DEBUT
C-----------------------------------------------------------------------
     
C     Recuperation des caracteristique du maillage et du fond de fissure

      CALL JEMARQ()
      CALL INFMAJ()
      CALL INFNIV(IFM,NIV)

C     RECUPERATION DES CARACTERISTIQUES DU MAILLAGE
      CALL DISMOI('F','NB_NO_MAILLA',NOMA,'MAILLAGE',NBNO,K8B,IRET)
      CALL JEVEUO(NOMA//'.COORDO    .VALE','L',JCOOR)

C     RECUPERATION DU FOND DE FISSURE
      CALL JEVEUO(FISS//'.FONDFISS','L',JFONF)
      CALL DISMOI('F','NB_POINT_FOND',FISS,'FISS_XFEM',NBPTFF,K8B,IRET)

C     RETRIEVE THE DIFFERENT PIECES OF THE CRACK FRONT
      CALL JEVEUO(FISS//'.FONDMULT','L',JFMULT)

C     RETRIEVE THE LOCAL REFERENCE SYSTEM FOR EACH NODE ON THE FRONT
      CALL JEVEUO(FISS//'.BASEFOND','E',JBASEF)

C     RETRIEVE THE CRACK'S SPEED AND PROPAGATION ANGLE FOR EACH NODE
C     ON THE FRONT
      CALL JEVEUO(NVIT,'L',JVIT)
      CALL JEVEUO(NBETA,'L',JBETA)

C     CREATION DES VECTEURS OU SONT STOCKES LES CARACTERISTIQUES
C     DU FRONT VIRTUEL
      CALL JEVEUO(COVIR,'E',CFV)
      CALL JEVEUO(BAVIR,'E',BFV)
      CALL JEVEUO(VITVIR,'E',VFV)
      CALL JEVEUO(ANGVIR,'E',AFV)
      CALL JEVEUO(NUMVIR,'E',NFV)

C     ON CREE DES VARIABLES DE GARAGES
          CALL WKVECT('&&XPRVIR.CO_FON_VIRPR','V V R8',
     &                4*(NBPTFF+2*NUMFON),CFVPR)
          CALL WKVECT('&&XPRVIR.BA_FON_VIRPR','V V R8',
     &             6*(NBPTFF+2*NUMFON),BFVPR)
          CALL WKVECT('&&XPRVIR.VIT_FON_VIRPR','V V R8',
     &               (NBPTFF+2*NUMFON),VFVPR)
          CALL WKVECT('&&XPRVIR.ANG_FON_VIRPR','V V R8',
     &                   (NBPTFF+2*NUMFON),AFVPR)

C        creation du vecteur ou l on stocke les distances entre les
C        differents fond de fissure
          CALL WKVECT('&&XPRVIR.NUM_FON_VIRPR','V V I',
     &                   (2*NUMFON),NFVPR)

C      On initialise les fonds de fissure actuels et precedents

        DO 1 I=1,(4*NBPTFF)
             ZR(CFV+I-1)=ZR(JFONF+I-1)
             ZR(CFVPR+I-1)=ZR(JFONF+I-1) 
1       CONTINUE     

        DO 2 I=1,6*(NBPTFF)
             ZR(BFV+I-1)=ZR(JBASEF+I-1)
             ZR(BFVPR+I-1)=ZR(JBASEF+I-1)      
2       CONTINUE

        DO 3 I=1,(NBPTFF)
             ZR(VFV+I-1)=ZR(JVIT+I-1)
             ZR(VFVPR+I-1)=ZR(JVIT+I-1)      
3       CONTINUE
      
        DO 4 I=1,(NBPTFF)
             ZR(AFV+I-1)=ZR(JBETA+I-1)
             ZR(AFVPR+I-1)=ZR(JBETA+I-1)    
4       CONTINUE
     
        DO 5 I=1,(2*NUMFON)
             ZI(NFV+I-1)=ZI(JFMULT+I-1)
             ZI(NFVPR+I-1)=ZI(JFMULT+I-1)
5       CONTINUE  

C      PARTIE 1: CREATION DE POINTS VIRTUELS ENTRE LES DIFFERENTS
C      FRONTS DE FISSURE

        IF (NUMFON.GT.1) THEN

            DO 10  K=1,(NUMFON-1)
C             On repere le dernier point du kieme fond de fissure
              NPOIN=ZI(NFV-1+2*K)

C On extrait les numeros des noueuds delimitants le fond de fissure

C             Coordonne des deux derniers points du fond de fissure  
              PI(1)=ZR(CFVPR-1+4*(NPOIN-2)+1)
              PI(2)=ZR(CFVPR-1+4*(NPOIN-2)+2)
              PI(3)=ZR(CFVPR-1+4*(NPOIN-2)+3)

              PJ(1)=ZR(CFVPR-1+4*(NPOIN-1)+1)
              PJ(2)=ZR(CFVPR-1+4*(NPOIN-1)+2)
              PJ(3)=ZR(CFVPR-1+4*(NPOIN-1)+3)

C             coordonne des deux premiers points du fond de
C             fissure suivant
              PK(1)=ZR(CFVPR-1+4*(NPOIN)+1)
              PK(2)=ZR(CFVPR-1+4*(NPOIN)+2)
              PK(3)=ZR(CFVPR-1+4*(NPOIN)+3)

              PL(1)=ZR(CFVPR-1+4*(NPOIN+1)+1)
              PL(2)=ZR(CFVPR-1+4*(NPOIN+1)+2)
              PL(3)=ZR(CFVPR-1+4*(NPOIN+1)+3)

C             Calcul des vecteurs IJ et KL
              PIJ(1)=PJ(1)-PI(1)
              PIJ(2)=PJ(2)-PI(2)
              PIJ(3)=PJ(3)-PI(3)
              PKL(1)=PL(1)-PK(1)
              PKL(2)=PL(2)-PK(2)
              PKL(3)=PL(3)-PK(3)

C             Calcul des normes des vecteur IJ ET KL
              NORMIJ=SQRT(PIJ(1)**2+PIJ(2)**2+PIJ(3)**2)
              NORMKL=SQRT(PKL(1)**2+PKL(2)**2+PKL(3)**2)  
C             On norme les vecteurs PIJ et PKL
              PIJ(1)=PIJ(1)/NORMIJ 
              PIJ(2)=PIJ(2)/NORMIJ  
              PIJ(3)=PIJ(3)/NORMIJ  
              PKL(1)=PKL(1)/NORMKL  
              PKL(2)=PKL(2)/NORMKL  
              PKL(3)=PKL(3)/NORMKL  

              IF (LOCDOM) THEN
C        LE rayon d'actualisation des levels sets a bien etait defini
                IF (RADIMP.GT.0.D0) THEN
                   P1(1)=PJ(1)+RADIMP*PIJ(1)
                   P1(2)=PJ(2)+RADIMP*PIJ(2)
                   P1(3)=PJ(3)+RADIMP*PIJ(3)
                   P2(1)=PK(1)-RADIMP*PKL(1)
                   P2(2)=PK(2)-RADIMP*PKL(2)
                   P2(3)=PK(3)-RADIMP*PKL(3)
                ELSE
                   P1(1)=PJ(1)+RADTOR*PIJ(1)
                   P1(2)=PJ(2)+RADTOR*PIJ(2)
                   P1(3)=PJ(3)+RADTOR*PIJ(3)
                   P2(1)=PK(1)-RADTOR*PKL(1)
                   P2(2)=PK(2)-RADTOR*PKL(2)
                   P2(3)=PK(3)-RADTOR*PKL(3)
                 ENDIF
               ELSE
C          Sinon on utilise l'avancememt maximal de la fissure
                P1(1)=PJ(1)+7*DAMAX*PIJ(1)
                P1(2)=PJ(2)+7*DAMAX*PIJ(2)
                P1(3)=PJ(3)+7*DAMAX*PIJ(3)
                P2(1)=PK(1)-7*DAMAX*PKL(1)
                P2(2)=PK(2)-7*DAMAX*PKL(2)
                P2(3)=PK(3)-7*DAMAX*PKL(3)
              ENDIF  

C              distance entre les fonts de fissure
                NORMJK=SQRT((PK(1)-PJ(1))**2+(PK(2)-PJ(2))**2+
     &         (PK(3)-PJ(3))**2)
           
C          On verifie que les deux nouveauX points sont bien eloignes

C           DISTANCE ENTRE LES POINTS 1 ET J 
                NORMJ1=SQRT((P1(1)-PJ(1))**2+(P1(2)-PJ(2))**2+
     &        (P1(3)-PJ(3))**2)
  
C           SI DISTANE EST TROP GRANDE ON RAPPROCHE LE POINT VIRTUEL
                IF((NORMJ1).GT.(NORMJK/3.D0)) THEN
                  NORMJ1=NORMJK/3.D0
                  P1(1)=PJ(1)+(NORMJ1*PIJ(1))
                  P1(2)=PJ(2)+(NORMJ1*PIJ(2))
                  P1(3)=PJ(3)+(NORMJ1*PIJ(3))
                ENDIF          
            
C              distance entre le point 2 et K
                NORMK2=SQRT((P2(1)-PK(1))**2+(P2(2)-PK(2))**2+
     &          (P2(3)-PK(3))**2)

C           SI DISTANCE EST TROP GRANDE ON RAPPROCHE LE POINT VIRTUEL
                IF((NORMK2).GT.(NORMJK/3.D0)) THEN
                  NORMK2=NORMJK/3.D0
                  P2(1)=PK(1)-(NORMK2*PKL(1))
                  P2(2)=PK(2)-(NORMK2*PKL(2))
                  P2(3)=PK(3)-(NORMK2*PKL(3))
                ENDIF 

C          On s assure que la vitesse de propagation est positive

C             calcul des vitesses de propagations des levels sets
C             aux points virtuels

              VI=ZR(VFVPR+NPOIN-2)
              VJ=ZR(VFVPR+NPOIN-1)
              VK=ZR(VFVPR+NPOIN)
              VL=ZR(VFVPR+NPOIN+1) 
         
              DV1=(VJ-VI)/NORMIJ
              DV2=(VL-VK)/NORMKL  

              V1=VJ+DV1*NORMJ1
              V2=VK-DV2*NORMK2         

                NORMJ1=SQRT((P1(1)-PJ(1))**2+(P1(2)-PJ(2))**2+
     &        (P1(3)-PJ(3))**2)

C         SI LES VITESSES SONT TROP FAIBLES, ON ELOIGNE OU ON
C         RAPPROCHE LES POINTS POUR LE POINT 1
              IF (ABS(V1).LT.ABS(2.D0*VJ/3.D0).OR.(ABS(V1).GT.
     &         ABS((4.D0*VJ/3.D0)))) THEN
                      NORMJ1=ABS(VJ/(3.D0*DV1))
                      P1(1)=PJ(1)+(PIJ(1)*NORMJ1)              
                      P1(2)=PJ(2)+(PIJ(2)*NORMJ1) 
                      P1(3)=PJ(3)+(PIJ(3)*NORMJ1)                
                      V1=VJ+(DV1*NORMJ1)
              ENDIF

C          POUR LE POINT 2                      
              IF ((ABS(V2).LT.ABS(2.D0*VK/3.D0)).OR.(ABS(V2).GT.
     &            ABS(4.D0*VK/3.D0))) THEN
                      NORMK2=ABS(VK/(3.D0*DV2))
                      P2(1)=PK(1)-(PKL(1)*NORMK2)              
                       P2(2)=PK(2)-(PKL(2)*NORMK2) 
                      P2(3)=PK(3)-(PKL(3)*NORMK2)               
                      V2=VK-(DV2*NORMK2)
              ENDIF

              AI=ZR(AFVPR+(NPOIN-2))
              AJ=ZR(AFVPR+(NPOIN-1))
              AK=ZR(AFVPR+(NPOIN))
              AL=ZR(AFVPR+(NPOIN+1))
              DA1=(AI-AJ)/NORMIJ
              DA2=(AL-AK)/NORMKL
              A1=AJ+DA1*NORMJ1
              A2=AK-DA2*NORMK2

C            On verifie que l angle de propagation au front
C            virtuel est compris entre -90 et 90 degre sinon
C            on rappoche le point virtuel du front physique
       
C             POUR LE POINT 1
              IF ((A1.GT.(1.51D0)).OR.(A1.LT.(-1.51D0))) THEN
                      NORMJ1=(1.51D0-ABS(AI))/ABS(DA1)
                      P1(1)=PJ(1)+(PIJ(1)*NORMJ1)              
                      P1(2)=PJ(2)+(PIJ(2)*NORMJ1) 
                      P1(3)=PJ(3)+(PIJ(3)*NORMJ1)                 
                      V1=VJ+(DV1*NORMJ1)
                      A1=AJ+DA1*NORMJ1
              ENDIF

C             POUR LE POINT 2
              IF ((A2.GT.(1.51D0)).OR.(A2.LT.(-1.51D0)))  THEN
                      NORMK2=(1.51D0-ABS(AL))/ABS(DA2)
                      P2(1)=PK(1)-(PKL(1)*NORMK2)              
                      P2(2)=PK(2)-(PKL(2)*NORMK2) 
                      P2(3)=PK(3)-(PKL(3)*NORMK2)                  
                      V2=VK-(DV2*NORMK2)
                      A2=AK-DA2*NORMK2
              ENDIF

C             On rentre les coordonnes des vecteurs dans le vecteur
C             coor contenant les coordonne de la fissure virtuel
              ZR(CFV-1+4*NPOIN+1)=P1(1)
              ZR(CFV-1+4*NPOIN+2)=P1(2)
              ZR(CFV-1+4*NPOIN+3)=P1(3)

              ZR(CFV-1+4*(NPOIN+1)+1)=P2(1)
              ZR(CFV-1+4*(NPOIN+1)+2)=P2(2)
              ZR(CFV-1+4*(NPOIN+1)+3)=P2(3)

C             On alloue une abscisse curviligne  
              ZR(CFV-1+4*NPOIN+4)=(ZR(CFVPR-1+4*(NPOIN-2)+4)+
     &         ZR(CFVPR-1+4*(NPOIN-1)+4))/2.D0
              ZR(CFV-1+4*(NPOIN+1)+4)=(ZR(CFVPR-1+4*(NPOIN)+4)+
     &         ZR(CFVPR-1+4*(NPOIN+1)+4))/2.D0
              
              DO 11 I=(NPOIN+1),NBPTFF
                ZR(CFV-1+4*(I+1)+1)=ZR(CFVPR-1+4*(I-1)+1)
                ZR(CFV-1+4*(I+1)+2)=ZR(CFVPR-1+4*(I-1)+2)
                ZR(CFV-1+4*(I+1)+3)=ZR(CFVPR-1+4*(I-1)+3)
                ZR(CFV-1+4*(I+1)+4)=ZR(CFVPR-1+4*(I-1)+4)
11            CONTINUE

C            ON ECRIE LES VITESSES DANS LES EMPLACEMENTS RESERVES
              ZR(VFV+NPOIN)=V1
              ZR(VFV+NPOIN+1)=V2

              DO 12 I=(NPOIN+1),NBPTFF
                ZR(VFV+(I+1))=ZR(VFVPR+(I-1))
12            CONTINUE
              
C            ON ECRIE LES VECTEURS DE LA BASE LOCALE DANS LES 
C            EMPLACEMENTS RESERVES
              DO 13 J=1,6
                 ZR(BFV-1+6*NPOIN+J)=ZR(BFVPR-1+6*(NPOIN-1)+J)
                 ZR(BFV-1+6*(NPOIN+1)+J)=ZR(BFVPR-1+6*(NPOIN)+J)
                 DO 130 I=(NPOIN+1),NBPTFF
                    ZR(BFV+6*(I+1)+J)=ZR(BFVPR+6*(I-1)+J)
130               CONTINUE                
13            CONTINUE           

C            ON ECRIE LES ANGLES DE PROPAGATION DANS EMPLACEMENTS
C            RESERVES                
              DO 14 I=(NPOIN+1),NBPTFF
                ZR(AFV+(I+1))=ZR(AFVPR+(I-1))
14            CONTINUE

C             REACTUALISATION DE LA FISSURE PRECEDENTE

C             reactualisation des cooordonnes            
              DO 15 I=1,4*(NBPTFF+2)
                 ZR(CFVPR+I-1)=ZR(CFV+I-1)      
15            CONTINUE

C             reactualisation de la base locale             
              DO 16 I=1,6*(NBPTFF+2)
                ZR(BFVPR+I-1)=ZR(BFV+I-1)      
16            CONTINUE

C             reactualisation de la vitesse de propagation
              DO 17 I=1,(NBPTFF+2)
                 ZR(VFVPR+I-1)=ZR(VFV+I-1)      
17           CONTINUE

              DO 18 I=1,(NBPTFF+2)
                  ZR(AFVPR+I-1)=ZR(AFV+I-1)      
18            CONTINUE

C            reactualisation du nombre de points du fond de
C            fissure virtuel
              NBPTFF=NBPTFF+2
         
C             reactualisatuion du vecteur FONDMULT
              ZI(NFV+2*K-1)=ZI(NFVPR+2*K-1)+1
              ZI(NFV+2*K)=ZI(NFVPR+2*K)+1
              
              DO 19, I=(2*(K+1)),(2*NUMFON)
                   ZI(NFV+I-1)=ZI(NFVPR+I-1)+2
19            CONTINUE

              DO 191, I=1,(2*NUMFON)
                   ZI(NFVPR+I-1)=ZI(NFV+I-1)
191            CONTINUE

10          CONTINUE
    
          ENDIF

C      PARTIE 2: CREATION DE POINTS VIRTUELS AUX EXTREMITES DU
C                FRONTS DE FISSURE

C         coordonne du premier point du fond de fissure
          PI(1)=ZR(CFVPR-1+1) 
          PI(2)=ZR(CFVPR-1+2)
          PI(3)=ZR(CFVPR-1+3)

C         coordonne du deuxieme point
          PJ(1)=ZR(CFVPR-1+4+1) 
          PJ(2)=ZR(CFVPR-1+4+2)
          PJ(3)=ZR(CFVPR-1+4+3)

C         coordonne de l avant dernier point
          PK(1)=ZR(CFVPR-1+4*(NBPTFF-2)+1) 
          PK(2)=ZR(CFVPR-1+4*(NBPTFF-2)+2)
          PK(3)=ZR(CFVPR-1+4*(NBPTFF-2)+3)

C         coordonne du dernier point
          PL(1)=ZR(CFVPR-1+4*(NBPTFF-1)+1) 
          PL(2)=ZR(CFVPR-1+4*(NBPTFF-1)+2)
          PL(3)=ZR(CFVPR-1+4*(NBPTFF-1)+3)

C         Calcul des vecteurs IJ et KL
          PIJ(1)=PJ(1)-PI(1)
          PIJ(2)=PJ(2)-PI(2)
          PIJ(3)=PJ(3)-PI(3)
          PKL(1)=PL(1)-PK(1)
          PKL(2)=PL(2)-PK(2)
          PKL(3)=PL(3)-PK(3)

C         Calcul des normes des vecteur IJ ET KL
          NORMIJ=SQRT(PIJ(1)**2+PIJ(2)**2+PIJ(3)**2)
          NORMKL=SQRT(PKL(1)**2+PKL(2)**2+PKL(3)**2)
          
C         On norme les vecteurs PIJ et PKL
          PIJ(1)=PIJ(1)/NORMIJ 
          PIJ(2)=PIJ(2)/NORMIJ  
          PIJ(3)=PIJ(3)/NORMIJ  
          PKL(1)=PKL(1)/NORMKL  
          PKL(2)=PKL(2)/NORMKL  
          PKL(3)=PKL(3)/NORMKL  

C         CALCUL DES POINTS VIRTUELS A GAUCHE ET A DROITE

              IF (LOCDOM) THEN
C        LE rayon d actualisation des levels sets a bien etait defini
                IF (RADIMP.GT.0.D0) THEN
                P1(1)=PI(1)-RADIMP*PIJ(1)
                P1(2)=PI(2)-RADIMP*PIJ(2)
                P1(3)=PI(3)-RADIMP*PIJ(3)
                P2(1)=PL(1)+RADIMP*PKL(1)
                P2(2)=PL(2)+RADIMP*PKL(2)
                P2(3)=PL(3)+RADIMP*PKL(3)
                ELSE
                 
                P1(1)=PI(1)-RADTOR*PIJ(1)
                P1(2)=PI(2)-RADTOR*PIJ(2)
                P1(3)=PI(3)-RADTOR*PIJ(3)
                P2(1)=PL(1)+RADTOR*PKL(1)
                P2(2)=PL(2)+RADTOR*PKL(2)
                P2(3)=PL(3)+RADTOR*PKL(3)
                ENDIF
            ELSE

C          SInon on utilise l avancememt maximal de la fissure
                P1(1)=PI(1)-7*DAMAX*PIJ(1)
                P1(2)=PI(2)-7*DAMAX*PIJ(2)
                P1(3)=PI(3)-7*DAMAX*PIJ(3)
                P2(1)=PL(1)+7*DAMAX*PKL(1)
                P2(2)=PL(2)+7*DAMAX*PKL(2)
                P2(3)=PL(3)+7*DAMAX*PKL(3)
          ENDIF

C         Distance entre le premier et le dernier fond de fissure
          NORMJK=SQRT((PI(1)-PL(1))**2+(PI(2)-PL(2))**2+
     &         (PI(3)-PL(3))**2)
           
C          On verifie que les deux nouveaux points sont bien
C          eloignes

C              distance entre le point 1 et J
          NORMJ1=SQRT((P1(1)-PI(1))**2+(P1(2)-PI(2))**2+
     &     (P1(3)-PI(3))**2)

C         SI DISTANCE EST TROP GRANDE ON RAPPROCHE LE POINT VIRTUEL
                IF((NORMJ1).GT.(NORMJK/3.D0)) THEN
                  NORMJ1=NORMJK/3.D0
                  P1(1)=PI(1)-(NORMJ1*PIJ(1))
                  P1(2)=PI(2)-(NORMJ1*PIJ(2))
                  P1(3)=PI(3)-(NORMJ1*PIJ(3))
                ENDIF          
            
C              distance entre le point 2 et K
          NORMK2=SQRT((P2(1)-PL(1))**2+(P2(2)-PL(2))**2+
     &     (P2(3)-PL(3))**2)

C         SI DISTANCE EST TROP GRANDE ON RAPPROCHE LE POINT VIRTUEL
                IF((NORMK2).GT.(NORMJK/3.D0)) THEN
                  NORMK2=NORMJK/3.D0
                  P2(1)=PL(1)+(NORMK2*PKL(1))
                  P2(2)=PL(2)+(NORMK2*PKL(2))
                  P2(3)=PL(3)+(NORMK2*PKL(3))
                ENDIF 

C        calcul de la vitesse de la fissure au point virtuel

          VI=ZR(VFVPR)
          VJ=ZR(VFVPR+1)
          VK=ZR(VFVPR+(NBPTFF-2))
          VL=ZR(VFVPR+(NBPTFF-1))           
          DV1=(VJ-VI)/NORMIJ 
          DV2=(VL-VK)/NORMKL          
          V1=VI-DV1*NORMJ1
          V2=VL+DV2*NORMK2
          
C       On verifie que la vitesse des points virtuels ne sont ni
C       trop grandes ni trop petites
        IF ((ABS(V1).LT.ABS(1.D0*VI/3.D0)).OR.(ABS(V1).GT.ABS(
     &      5.D0*VI/3.D0))) THEN
                      NORMJ1=ABS(2.D0*VI/(3.D0*DV1))
                      P1(1)=PI(1)-(PIJ(1)*NORMJ1)              
                      P1(2)=PI(2)-(PIJ(2)*NORMJ1) 
                      P1(3)=PI(3)-(PIJ(3)*NORMJ1)                
                      V1=VI-(DV1*NORMJ1)
         ENDIF

         IF ((ABS(V2).LT.ABS(1.D0*VL/3.D0)).OR.(ABS(V2).GT.ABS(5.D0
     &             *VL/3.D0))) THEN
                      NORMK2=ABS(2.D0*VL/(3.D0*DV2))
                      P2(1)=PL(1)+(PKL(1)*NORMK2)              
                      P2(2)=PL(2)+(PKL(2)*NORMK2) 
                      P2(3)=PL(3)+(PKL(3)*NORMK2)                
                      V2=VL+(DV2*NORMK2)
         ENDIF

              AI=ZR(AFVPR)
              AJ=ZR(AFVPR+1)
              AK=ZR(AFVPR+NBPTFF-2)
              AL=ZR(AFVPR+NBPTFF-1)
              DA1=(AJ-AI)/NORMIJ
              DA2=(AL-AK)/NORMKL

              A1=AI-DA1*NORMJ1
              A2=AL+DA2*NORMK2

C            On verifie que l angle de propagation au front virtuel
C            est compris entre -90 et 90 degre sinon
C            on rappoche le point virtuel du front physique

          IF ((A1.GT.(1.51D0)).OR.(A1.LT.(-1.51D0))) THEN
                      NORMJ1=(1.51D0-ABS(AI))/ABS(DA1)
                      P1(1)=PI(1)-(PIJ(1)*NORMJ1)              
                      P1(2)=PI(2)-(PIJ(2)*NORMJ1) 
                      P1(3)=PI(3)-(PIJ(3)*NORMJ1)                
                      V1=VI-(DV1*NORMJ1)
                      A1=AI-DA1*NORMJ1
         ENDIF

          IF ((A2.GT.(1.51D0)).OR.(A2.LT.(-1.51D0)))  THEN
                      NORMK2=(1.51D0-ABS(AL))/ABS(DA2)
                      P2(1)=PL(1)+(PKL(1)*NORMK2)              
                      P2(2)=PL(2)+(PKL(2)*NORMK2) 
                      P2(3)=PL(3)+(PKL(3)*NORMK2)                
                      V2=VL+(DV2*NORMK2)
                      A2=AL+DA2*NORMK2
          ENDIF

C        On rentre les nouveaux points virtuels de la fissure
C        dans la place reserve

          ZR(CFV-1+1)=P1(1)
          ZR(CFV-1+2)=P1(2)
          ZR(CFV-1+3)=P1(3)
          ZR(CFV-1+4)=-100.D0
          ZR(CFV-1+4*(1+NBPTFF)+1)=P2(1)
          ZR(CFV-1+4*(1+NBPTFF)+2)=P2(2)
          ZR(CFV-1+4*(1+NBPTFF)+3)=P2(3)
          ZR(CFV-1+4*(1+NBPTFF)+4)=100.D0

C        On rentre les vitesses de la fissure au point virtuel
        ZR(VFV)=V1
        ZR(VFV+1+NBPTFF)=V2 

C        IDEM POUR LA BASE LOCALE ET L'ANGLE DE PROPAGATION
          DO 25 , J=1,6
             ZR(BFV-1+J)=ZR(BFVPR-1+J)
             ZR(BFV-1+6*(1+NBPTFF)+J)=ZR(BFVPR-1+6*(NBPTFF-1)+J)
25        CONTINUE

              ZR(AFV)=AI-DA1*NORMJ1
              ZR(AFV+NBPTFF+1)=AL+DA2*NORMK2
C         On copie les coordonnes,  du front physique dans
C         l emplacememt dans les emplacements reserves au 
C         front virtuel

          DO 26 I=1,NBPTFF
                ZR(CFV-1+4*I+1)=ZR(CFVPR-1+4*(I-1)+1)
                ZR(CFV-1+4*I+2)=ZR(CFVPR-1+4*(I-1)+2)
                ZR(CFV-1+4*I+3)=ZR(CFVPR-1+4*(I-1)+3)
                ZR(CFV-1+4*I+4)=ZR(CFVPR-1+4*(I-1)+4)

                DO 261,J=1,6
                   ZR(BFV-1+6*I+J)=ZR(BFVPR-1+6*(I-1)+J)
261             CONTINUE
      
               ZR(VFV+I)=ZR(VFVPR+I-1)
               ZR(AFV+I)=ZR(AFVPR+I-1)  
26             CONTINUE
          
         DO 27 I=1,(2*NUMFON-1)
               ZI(NFV+I)=1+ZI(NFV+I)
27       CONTINUE
         ZI(NFV+2*(NUMFON-1)+1)=1+ZI(NFV+2*(NUMFON-1)+1)

       NBPTFF=NBPTFF+2

      CALL JEDETR('&&XPRVIR.NUM_FON_VIRPR')
      CALL JEDETR('&&XPRVIR.CO_FON_VIRPR')
      CALL JEDETR('&&XPRVIR.BA_FON_VIRPR')
      CALL JEDETR('&&XPRVIR.VIT_FON_VIRPR')
      CALL JEDETR('&&XPRVIR.ANG_FON_VIRPR')
C-----------------------------------------------------------------------
C     FIN
C-----------------------------------------------------------------------
      CALL JEDEMA()
      END
