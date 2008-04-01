      SUBROUTINE XMREMA(MODELE,NOMA  ,NDIM  ,DEFICO,IZONE ,
     &                  MMAIT ,PMAIT ,AMAIT ,NMAIT ,ZMAIT ,
     &                  GEOM  ,JMAESC,POSMIN,JEUMIN,T1MIN ,
     &                  T2MIN ,XIMIN ,YIMIN ,PROJIN)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 01/04/2008   AUTEUR ABBAS M.ABBAS 
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
C
      IMPLICIT NONE
      CHARACTER*8  NOMA,MODELE
      INTEGER      NDIM,ZMAIT,MMAIT,PMAIT,NMAIT,AMAIT,JMAESC
      CHARACTER*24 DEFICO
      INTEGER      IZONE
      REAL*8       GEOM(3)
      INTEGER      POSMIN
      REAL*8       JEUMIN
      REAL*8       T1MIN(3),T2MIN(3)
      REAL*8       XIMIN,YIMIN
      LOGICAL      PROJIN
C      
C ----------------------------------------------------------------------
C
C ROUTINE XFEM (CONTACT - GRANDS GLISSEMENTS)
C
C RECHERCHER LA MAILLE MAITRE LA PLUS PROCHE CONNAISSANT LE POINT 
C D'INTERSECTION MAITRE LE PLUS PROCHE DU POINT DE CONTACT ET FAIRE
C LA PROJECTION 
C
C TRAVAIL EFFECTUE EN COLLABORATION AVEC I.F.P.
C
C ----------------------------------------------------------------------
C
C
C IN  NOMA   : NOM DU MAILLAGE
C IN  MODELE : NOM DU MODELE
C IN  PMAIT  : NUMERO LOCAL DU POINT D'INTERSECTION LE PLUS PROCHE
C IN  AMAIT  : NUMERO LOCAL DE L'ARETE INTERSECTÉ
C IN  NMAIT  : NUMERO LOCAL DU NOEUD INTERSECTÉ
C IN  MMAIT  : NUMERO DE LA MAILLE MAITRE CONTENANT LE PMAIT
C IN  ZMAIT  : NUMERO DE LA ZONE DE CONTACT CONTENANT LE PMAIT
C IN  GEOM   : COORDONNEES DU POINT DE CONTACT
C OUT POSMIN : POSITION DE LA MAILLE MAITRE LA PLUS PROCHE
C OUT JEUMIN : JEU MINIMUM
C OUT T1MIN  : PREMIER VECTEUR TANGENT
C OUT T2MIN  : DEUXIEME VECTEUR TANGENT
C OUT XIMIN  : COORDONNEE X DE LE PROJECTION MINIMALE DU POINT DE 
C              CONTACT SUR LA MAILLE MAITRE
C OUT YIMIN  : COORDONNEE Y DE LE PROJECTION MINIMALE DU POINT DE 
C              CONTACT SUR LA MAILLE MAITRE
C OUT PROJIN : VAUT .TRUE. SI LA PROJECTION DU POINT DE CONTACT N'EST
C              PAS LE RESULTAT DU RABATTEMENT 
C              .FALSE. S'IL Y A EU RABATTEMENT PARCE QU'ELLE SERAIT
C              TOMBEEE HORS DE LA MAILLE MAITRE (A LA TOLERANCE PRES)
C
C -------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------------
C
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
C
C ---------------- FIN DECLARATIONS NORMALISEES JEVEUX -----------------
C
      INTEGER      CFDISI,ITEMAX
      INTEGER      CFMMVD,ZMESX
      INTEGER      INI,IAD,STATUT
      INTEGER      JCSD1,JCSV1,JCSL1
      INTEGER      JCONX1,JCONX2      
      INTEGER      POSMA,NUNOIN,NUNOG,NUGLA,NUGLB
      INTEGER      N1,N2,NBNOS,NTMA
      INTEGER      INO,NIVERR
      INTEGER      I,J,K,IA,IMA
      INTEGER      AR(12,2),NBAR,NA,NB,NUNOA,NUNOB
      REAL*8       JEU,TAU1(3),TAU2(3)
      REAL*8       CFDISR,TOLEOU,EPSMAX
      REAL*8       COORMA(27),XI,YI
      REAL*8       R8GAEM,R3BID(3)
      CHARACTER*8  ALIAS,TYPMA,FFORME
      CHARACTER*19 CHS1
      LOGICAL      LDIST,LDMIN,DIRAPP        
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- INITIALISATIONS
C
      CHS1   = '&&XMREMA.CHS1'
      JEUMIN = R8GAEM()
      PROJIN = .TRUE.
      POSMIN = 0      
      DO 10 I=1,27
        COORMA(I)=0.D0
10    CONTINUE    
      FFORME = 'CONTINUE'
      DIRAPP = .FALSE. 
C
C --- ALIAS ET TYPMA VALABLE POUR LE CAS 2D (IL FAUDRA CHANGER POUR 3D)
C
      ALIAS  = 'SG2'
      TYPMA  = 'QUAD4'   
C
C --- INFOS GENERIQUES POUR L'ALGORITHME D'APPARIEMENT
C         
      TOLEOU = CFDISR(DEFICO,'TOLE_PROJ_EXT' ,IZONE)
      EPSMAX = CFDISR(DEFICO,'PROJ_NEWT_RESI',IZONE)
      ITEMAX = CFDISI(DEFICO,'PROJ_NEWT_ITER',IZONE) 

C----------------------------------------------------------------------
      CALL JEVEUO(NOMA//'.CONNEX','L',JCONX1)
      CALL JEVEUO(JEXATR(NOMA//'.CONNEX','LONCUM'),'L',JCONX2)
      
C------SI LE POINT DE CONTACT EST SUR UN ARRÊTE----------------------
      IF (AMAIT.GT.0) THEN            
        CALL CONARE(TYPMA,AR,NBAR)
        NA=AR(AMAIT,1)
        NB=AR(AMAIT,2)
        NUNOA=ZI(JCONX1-1+ZI(JCONX2+MMAIT-1)+NA-1)
        NUNOB=ZI(JCONX1-1+ZI(JCONX2+MMAIT-1)+NB-1)
C------SI LE POINT DE CONTACT EST UN NOEUD---------------------------	
      ELSE
        NUNOG=ZI(JCONX1-1+ZI(JCONX2+MMAIT-1)+NMAIT-1)
C-------attention!!! nbnos est introduit en dur ici pour quad4!!!!!!!!!
        NBNOS=4
      ENDIF

      ZMESX = CFMMVD('ZMESX')
      NTMA=ZI(JMAESC)

C-----ON RECUPERE LA GEOMETRIE DES FACETTES MAITRES
      CALL CELCES(MODELE//'.TOPOFAC.GM','V',CHS1)
      CALL JEVEUO(CHS1//'.CESD','L',JCSD1)
      CALL JEVEUO(CHS1//'.CESV','L',JCSV1)
      CALL JEVEUO(CHS1//'.CESL','L',JCSL1)

C-----BOUCLE SUR LES MAILLES FISSURÉES--------------------------------
      DO 100 IMA=1,NTMA
        POSMA  = ZI(JMAESC+ZMESX*(IMA-1)+1)
        STATUT = ZI(JMAESC+ZMESX*(IMA-1)+4)
C        NBN    = ZI(JMAESC+ZMESX*(IMA-1)+3)

C-----UNE BOUCLE SUR LES FACETTES DE CONTACT EST A INTRODUIRE POUR 3D!!!

        IF (STATUT.EQ.-1) GO TO 100

C------SI LE POINT DE CONTACT EST SUR UN ARRÊTE----------------------
        IF (AMAIT.GT.0) THEN     
C------ BOUCLE SUR LES ARETES DE LA MAILLE COURANTE
          DO 110 IA=1,NBAR

            N1=AR(IA,1)
            N2=AR(IA,2)

            NUGLA=ZI(JCONX1-1+ZI(JCONX2+POSMA-1)+N1-1)
            NUGLB=ZI(JCONX1-1+ZI(JCONX2+POSMA-1)+N2-1)

            IF (((NUGLA.EQ.NUNOA).AND.(NUGLB.EQ.NUNOB)).OR.
     &         ((NUGLA.EQ.NUNOB).AND.(NUGLB.EQ.NUNOA))) THEN

C----RECUPERATION GEOMETRIE DES POINTS D'INTERS. DE LA FACETTE MAITRE
              DO 120 INI=1,NDIM
                DO 130 J=1,NDIM
                  CALL CESEXI('S',JCSD1,JCSL1,POSMA,1,1,
     &                        NDIM*(INI-1)+J,IAD)
                  CALL ASSERT(IAD.GT.0)
                  COORMA(3*(INI-1)+J)=ZR(JCSV1-1+IAD)
  130           CONTINUE
  120         CONTINUE
C-----------PROJECTION SUR LA FACETTE MAITRE-------------------------
              CALL MMPROJ(ALIAS ,NDIM  ,NDIM  ,COORMA,GEOM  ,
     &                    ITEMAX,EPSMAX,TOLEOU,DIRAPP,R3BID ,
     &                    FFORME,XI    ,YI    ,TAU1  ,TAU2  ,
     &                    LDIST ,NIVERR)              
C
C --- ECHEC DE NEWTON
C      
              IF (NIVERR.EQ.1) THEN
                CALL ASSERT(.FALSE.)
              ENDIF    
C
C --- CALCUL DU JEU
C
              CALL MMJEUX(ALIAS ,NDIM  ,NDIM  ,COORMA,FFORME,
     &                    XI    ,YI    ,GEOM  ,JEU   )              
              
C
C --- CHOIX DE LA MAILLE 
C               
              IF (JEU.LT.JEUMIN) THEN
                POSMIN = POSMA
                JEUMIN = JEU
                LDMIN  = LDIST
                DO 40 K = 1,3
                  T1MIN(K) = TAU1(K)
                  T2MIN(K) = TAU2(K)
   40           CONTINUE
                XIMIN = XI
                YIMIN = YI
              ENDIF
            ENDIF
 110      CONTINUE
 
C------SI LE POINT DE CONTACT EST UN NOEUD----------------------
        ELSE
C---------BOUCLE SUR LES NOEUDS DE LA MAILLE COURANTE	
          DO 210 INO=1,NBNOS
            NUNOIN=ZI(JCONX1-1+ZI(JCONX2+POSMA-1)+INO-1)
            IF (NUNOIN.EQ.NUNOG) THEN
              DO 220 INI=1,NDIM
                DO 230 J=1,NDIM
                  CALL CESEXI('S',JCSD1,JCSL1,POSMA,1,1,
     &                        NDIM*(INI-1)+J,IAD)
                  CALL ASSERT(IAD.GT.0)
                  COORMA(3*(INI-1)+J)=ZR(JCSV1-1+IAD)
  230           CONTINUE
  220         CONTINUE
C-----------PROJECTION SUR LA FACETTE MAITRE-------------------------
     
              CALL MMPROJ(ALIAS ,NDIM  ,NDIM  ,COORMA,GEOM  ,
     &                    ITEMAX,EPSMAX,TOLEOU,DIRAPP,R3BID ,
     &                    FFORME,XI    ,YI    ,TAU1  ,TAU2  ,
     &                    LDIST ,NIVERR) 
C
C --- ECHEC DE NEWTON
C      
              IF (NIVERR.EQ.1) THEN
                CALL ASSERT(.FALSE.)
              ENDIF  
C
C --- CALCUL DU JEU
C
              CALL MMJEUX(ALIAS ,NDIM  ,NDIM  ,COORMA,FFORME,
     &                    XI    ,YI    ,GEOM  ,JEU   )
C
C --- CHOIX DE LA MAILLE 
C                                
              IF (JEU.LT.JEUMIN) THEN
                POSMIN = POSMA
                JEUMIN = JEU
                LDMIN  = LDIST
                DO 240 K = 1,3
                  T1MIN(K) = TAU1(K)
                  T2MIN(K) = TAU2(K)
 240            CONTINUE
                XIMIN = XI
                YIMIN = YI
              ENDIF
            ENDIF
 210      CONTINUE
        ENDIF

 100  CONTINUE

      IF (.NOT.LDMIN) PROJIN = .FALSE.
      IF (TOLEOU.EQ.-1.D0) PROJIN = .TRUE.


C ----------------------------------------------------------------------

      CALL JEDEMA()
      END
