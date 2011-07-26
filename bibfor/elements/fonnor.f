      SUBROUTINE FONNOR ( RESU, NOMA)
      IMPLICIT NONE
      CHARACTER*8         RESU, NOMA
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 26/07/2011   AUTEUR MACOCCO K.MACOCCO 
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
C-----------------------------------------------------------------------
C FONCTION REALISEE:
C
C     CALCUL DE LA NORMALE AU FOND DE FISSURE POUR DEFI_FOND_FISS 
C     EN 2D ET 3D DE LA BASE LOCALE
C
C     ENTREES:
C        RESU   : NOM DU CONCEPT RESULTAT DE L'OPERATEUR
C        NOMA   : NOM DU MAILLAGE
C-----------------------------------------------------------------------
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
      CHARACTER*32     JEXNOM, JEXNUM,JEXATR
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER       JNOE1,JVECT,JTYP
      INTEGER       I,J,INA,INB,ISEG,IND1,IRET,NSOMMT,NBNOSE,NBNOFF
      INTEGER       NA,NB,NRET,COMPTE,NDIM,NBNOEL,NSEG,NBMAX,NBMAC
      INTEGER       INDIC(4),NOE(4,4),INDR(2),TABLEV(2)
      REAL*8        S,NORM1,NORM2,X1,Y1
      REAL*8        VNORM(2,3),VTANG(2,3)
      CHARACTER*8   K8B, TYPE,TYPFON,NOEUA
      CHARACTER*16  CASFON
C     -----------------------------------------------------------------
C
      CALL JEMARQ()

C     ------------------------------------------------------------------
C     INITIALISATIONS
C     ------------------------------------------------------------------

C
C     RECUPERATION DES INFORMATIONS RELATIVES AU MAILLAGE
C
C
C     RECUPERATION DU CONCEPT DU MAILLAGE
      CALL GETVID ( ' ', 'MAILLAGE', 1,1,1, NOMA , NRET )
C
C     RECUPERATION DU NOMBRE DE NOEUDS DU MAILLAGE
      CALL DISMOI('F','DIM_GEOM',NOMA,'MAILLAGE',NDIM,K8B,IRET)
C
C     RECUPERATION DES NOEUDS DU FOND DE FISSURE
C
      CALL JEEXIN(RESU//'.FOND      .NOEU',IRET)
      IF (IRET.NE.0) THEN
C       RECUPERATION DE L'ADRESSE DES NOEUDS DE FOND DE FISSURE
        CALL JEVEUO (RESU//'.FOND      .NOEU', 'L', JNOE1 )     
C       RECUPERATION DU NOMBRE DE NOEUD
        CALL JELIRA (RESU//'.FOND      .NOEU' , 'LONUTI', NBNOFF, K8B)
      ELSE
C       RECUPERATION DE L'ADRESSE DES NOEUDS DE FOND DE FISSURE
        CALL JEVEUO (RESU//'.FOND_SUP  .NOEU', 'L', JNOE1 )     
C       RECUPERATION DU NOMBRE DE NOEUD
        CALL JELIRA (RESU//'.FOND_SUP  .NOEU' , 'LONUTI', NBNOFF, K8B)
      ENDIF
C
C     RECUPERATION DU TYPE DE MAILLE EN FOND DE FISSURE EN 3D
      IF (NDIM.EQ.3) THEN
        CALL JEVEUO (RESU//'.FOND      .TYPE', 'L', JTYP )
        TYPFON = ZK8(JTYP)
C       SUIVANT LE CAS QUADRATIQUE/LINEAIRE EN 3D DEUX MAILLES SONT 
C       CONNECTEES SI ELLES ONT AU MOINS NBMAX NOEUDS EN COMMUN
C       NBNOSE : NOMBRE DE NOEUDS PAR "SEGMENT" DE FOND DE FISSURE 
        IF (TYPFON.EQ.'NOE2'.OR.TYPFON.EQ.'SEG2') THEN
          CASFON = 'LINEAIRE'
          NBNOSE = 2
          NBMAX  = 3
        ELSEIF (TYPFON.EQ.'NOE3'.OR.TYPFON.EQ.'SEG3') THEN
          CASFON = 'QUADRATIQUE'
          NBNOSE = 3
          NBMAX  = 6
        ELSE
          NBNOSE = 2
          NBMAX  = 3

        ENDIF
      ELSE
          NBNOSE = 2
          NBMAX  = 3
      ENDIF
      IF (NDIM.EQ.2) CASFON = '2D'
C     
C
C     ALLOCATION DU VECTEUR DES BASES LOCALES      
      CALL WKVECT (RESU//'.BASEFOND'  , 'G V R', 6*NBNOFF, JVECT )
C
C
C     ALLOCATION DU VECTEUR CONTENANT LES MAILLES CONNECTEES AU FOND
C     DE FISSURE ET AYANT UN BORD LIBRE 
      IF (NDIM.EQ.2) THEN
        CALL ASSERT(NBNOFF.EQ.1)
C       NOMBRE DE SOMMETS EN FOND DE FISSURE
        NSOMMT = 1
C       NOMBRE DE SOMMETS A TRAITER
        NSEG = 1
      ELSEIF (NDIM.EQ.3) THEN
        CALL ASSERT(NBNOFF.GT.1)
C       NOMBRE DE SOMMETS EN FOND DE FISSURE
        IF (CASFON.EQ.'LINEAIRE')    NSOMMT =  NBNOFF
        IF (CASFON.EQ.'QUADRATIQUE') NSOMMT = (NBNOFF-1)/2+1
C       NOMBRE DE SOMMETS A TRAITER = NB SEG ?
        NSEG = NSOMMT-1
      ENDIF      

C
      IND1 = 0
C
C     ------------------------------------------------------------------
C     BOUCLE SUR LES "SEGMENTS" DU FOND DE FISSURE
C     ------------------------------------------------------------------
C
      DO 100 ISEG=1,NSEG
        
C       INDICES DES NOEUDS SOMMETS DU SEGMENT
        IF (CASFON.EQ.'LINEAIRE'.OR.CASFON.EQ.'2D') THEN
          INA = ISEG
          INB = ISEG+1
        ELSEIF (CASFON.EQ.'QUADRATIQUE') THEN
          INA = 2*ISEG-1
          INB = 2*ISEG+1
        ENDIF

C       NUMEROS DES NOEUDS SOMMETS DU SEGMENT
        NOEUA = ZK8(JNOE1-1+INA)
        CALL JENONU (JEXNOM(NOMA//'.NOMNOE',NOEUA),NA)
        IF (NDIM.EQ.3) THEN
          CALL JENONU (JEXNOM(NOMA//'.NOMNOE',ZK8(JNOE1-1+INB)),NB)
        ENDIF

C
C       1) RECUP DES NUMEROS DES MAILLES CONNECTEES AU SEGMENT DU FOND
C       ----------------------------------------------------------------
C
        CALL FONNO1 (NOMA,NDIM,NA,NB,NBMAC)
C
C       2) PARMI LES MAILLES CONNECTEES AU SEGMENT DU FOND, FILTRAGE DES
C          MAILLES CONNECTEES A 1 LEVRE (CAD AYANT UNE FACE LIBRE) 
C          -> REMPLISSAGE DE JMALEV
C       ----------------------------------------------------------------
C
        CALL FONNO2 (NOMA,NBMAC,NBNOFF,NBNOSE,
     %               NBMAX,NOEUA,TABLEV)
     
C
C       3) RECUP DES FACES CONNECTEES AU FOND 
C          POUR CHACUNE DES 2 MAILLES
C       ----------------------------------------------------

        CALL FONNO3 (NOMA,TABLEV,NDIM,NA,NB,NOE)


C       4) FILTRE DES FACES LIBRES
C       ----------------------------------------------------
C
        CALL FONNO4 (NOMA,NBMAC,TABLEV,NOE,NBNOFF,INDIC)

C       5) CALCUL DES VECTEURS DE LA BASE LOCALE
C       ----------------------------------------------------
        CALL FONNO5 (NOMA,INDIC,NBNOFF,NOE,NA,NB,NDIM,
     %         NBNOEL,INDR,COMPTE,VNORM,VTANG)

C       6) VERIF
C       ----------------------------------------------------
        CALL FONNO6 (RESU,NOMA,NDIM,NSOMMT,INA,NBNOSE,COMPTE,ISEG,
     %        NOE,INDR,NBNOEL,IND1,VNORM,VTANG)


 100  CONTINUE

C     ------------------------------------------------------------------

      IF (NDIM.EQ.3) THEN
C       CALCUL DE LA BASE LOCALE AU DERNIER SOMMET
        DO 200 J=1,6
          ISEG = (NSOMMT-1)*(NBNOSE-1)+1
          ZR(JVECT-1+6*(ISEG-1)+J) = 
     &           ZR(JVECT-1+6*(NSOMMT-2)*(NBNOSE-1)+J)
 200     CONTINUE
C       LA BASE LOCALE AUX NOEUDS SOMMET EST CALCULEE 
C       COMME MOYENNE DES BASES LOCALES VOISINES
        DO 400 ISEG=2,NSOMMT-1,-1          
          DO 410 J=1,6
            X1 = ZR(JVECT-1+6*((ISEG-2)*(NBNOSE-1))+J)
            Y1 = ZR(JVECT-1+6*((ISEG-1)*(NBNOSE-1))+J)
            ZR(JVECT-1+6*((ISEG-1)*(NBNOSE-1))+J) = (X1+Y1)/2.D0
 410     CONTINUE
 400    CONTINUE
C       NORMALISATION
        DO 500 ISEG=1,NBNOFF
          NORM1 = 0.D0
          NORM2 = 0.D0
          DO 510 J=1,3
            NORM1 = NORM1 + ZR(JVECT-1+6*(ISEG-1)+J)**2
            NORM2 = NORM2 + ZR(JVECT-1+6*(ISEG-1)+J+3)**2
 510      CONTINUE                 
          DO 520 J=1,3
            ZR(JVECT-1+6*(ISEG-1)+J) = ZR(JVECT-1+6*(ISEG-1)+J)
     &                                                    /SQRT(NORM1)
            ZR(JVECT-1+6*(ISEG-1)+J+3) = ZR(JVECT-1+6*(ISEG-1)+J+3)
     &                                                    /SQRT(NORM2)
 520      CONTINUE
 500    CONTINUE
      ENDIF
     

      CALL JEDEMA()
      END
