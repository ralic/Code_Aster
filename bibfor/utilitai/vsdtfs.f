      SUBROUTINE VSDTFS(NOMZ,M80,NVU,NOBJ)

      IMPLICIT NONE
      CHARACTER*(*) NOMZ,M80
      INTEGER NVU,NOBJ

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 05/02/2007   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ----------------------------------------------------------------------
C BUT : VERISD/TYPE_FLUI_STRU
C ----------------------------------------------------------------------
C TOLE CRS_512
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
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
C     ----- FIN COMMUNS NORMALISES  JEVEUX  --------------------------
C -DEB------------------------------------------------------------------

      CHARACTER*8    NOM,KBID
      INTEGER JFSIC,JFSVI,JFSVK,NBGRIL,NBTUBE
      INTEGER TYPFAI,NBZONE,VERIOB,IER,NBTYP,NBTOT,I,IRET,IZONE,LONG
      LOGICAL COUPLA,SIMP

      CALL JEMARQ()
      
      NOM=NOMZ(1:8)


C---- 1. VERIFICATION DE L OBJET FSIC

      IER=VERIOB('O',NOM//'           .FSIC','EXIS_V',0,' ')
      IER=VERIOB('O',NOM//'           .FSIC','TYPE_I',0,' ')
      IER=VERIOB('O',NOM//'           .FSIC','LONMAX',2,' ')
      CALL JEVEUO(NOM//'           .FSIC','L',JFSIC)

C ON RECUPERE LE TYPE DE CONFIGURATION ET LE FAIT D AVOIR DU COUPLAGE
C CAR C EST NECESSAIRE POUR LES VERIFICATIONS ULTERIEURES
      TYPFAI=ZI(JFSIC)
      COUPLA=ZI(JFSIC+1).EQ.1
      CALL ASSERT((TYPFAI.EQ.1).OR.(TYPFAI.EQ.2).OR.
     &            (TYPFAI.EQ.3).OR.(TYPFAI.EQ.4))

      CALL ASSERT((ZI(JFSIC+1).EQ.0).OR.(ZI(JFSIC+1).EQ.1))

C ----------------------------------------------------------------------

C---- 2. VERIFICATION DE L OBJET FSVI

      IF (TYPFAI.EQ.1) THEN
C   TYPE FAISCEAU_TRANS
        IER=VERIOB('O',NOM//'           .FSVI','EXIS_V',0,' ')
        IER=VERIOB('O',NOM//'           .FSVI','TYPE_I',0,' ')
        CALL JEVEUO(NOM//'           .FSVI','L',JFSVI)
        NBZONE=ZI(JFSVI+1)
C   SI COUPLAGE
        IF (COUPLA) THEN
          IER=VERIOB('O',NOM//'           .FSVI','LONMAX',
     &               2+2*NBZONE,' ')
          CALL ASSERT((ZI(JFSVI).EQ.1).OR.(ZI(JFSVI).EQ.2))
        ELSE
          IER=VERIOB('O',NOM//'           .FSVI','LONMAX',2,' ')
        ENDIF

      ELSE IF (TYPFAI.EQ.3) THEN
C   TYPE FAISCEAU_AXIAL

        IER=VERIOB('O',NOM//'           .FSVI','EXIS_V',0,' ')
        IER=VERIOB('O',NOM//'           .FSVI','TYPE_I',0,' ')
        CALL JEVEUO(NOM//'           .FSVI','L',JFSVI)

C S AGIT-IL D UNE CONFIGURATION SIMPLIFIE OU D UN FAISCEAU COMPLET?
        SIMP=ZI(JFSVI).EQ.1
        NBZONE=ZI(JFSVI+3)

        IF (ZI(JFSVI).EQ.0) THEN
C  CALCUL SUR FAISCEAU COMPLET  
          
          CALL ASSERT((ZI(JFSVI+1).EQ.1).OR.(ZI(JFSVI+1).EQ.2).OR.
     &                (ZI(JFSVI+1).EQ.3))
          CALL ASSERT((ZI(JFSVI+2).EQ.1).OR.(ZI(JFSVI+2).EQ.2))

          NBTYP=ZI(JFSVI+4)

          IF (NBTYP.EQ.0) THEN
C PAS DE PRISE EN COMPTE DE GRILLE SUR LE FAISCEAU
            IER=VERIOB('O',NOM//'           .FSVI','LONMAX',5,' ')
          ELSE
C PRISE EN COMPTE DE GRILLE SUR LE FAISCEAU
            IER=VERIOB('O',NOM//'           .FSVI','LONMAX',6+NBTYP,' ')
          ENDIF

C  ON AIMERAIT AUSSI VERIFIER LE TYPE DE GRILLE MAIS ON NE CONNAIT PAS
C  CEUX QUI SONT LICITES DE CEUX QUI SONT ILLICITES

        ELSE IF (ZI(JFSVI).EQ.1) THEN
C  CALCUL SUR FAISCEAU SIMPLIFIE  
          CALL ASSERT((ZI(JFSVI+1).EQ.1).OR.(ZI(JFSVI+1).EQ.2).OR.
     &                (ZI(JFSVI+1).EQ.3))
          CALL ASSERT((ZI(JFSVI+2).EQ.1).OR.(ZI(JFSVI+2).EQ.2))

          NBTYP=ZI(JFSVI+4)
          NBTOT=0
          DO 20 I=1,NBZONE
           NBTOT=NBTOT+ZI(JFSVI+5+I)
20        CONTINUE
          CALL ASSERT(NBTOT.EQ.ZI(JFSVI+5))
          IF (NBTYP.EQ.0) THEN
C PAS DE PRISE EN COMPTE DE GRILLE SUR LE FAISCEAU
            IER=VERIOB('O',NOM//'           .FSVI','LONMAX',
     &                 6+NBZONE,' ')
          ELSE
C PRISE EN COMPTE DE GRILLE SUR LE FAISCEAU
            IER=VERIOB('O',NOM//'           .FSVI','LONMAX',
     &                 7+NBTYP+NBZONE,' ')
          ENDIF
        ELSE
          CALL U2MESS ('F','SDVERI_32')
        ENDIF
      ELSE IF (TYPFAI.EQ.4) THEN
        IER=VERIOB('O',NOM//'           .FSVI','EXIS_V',0,' ')
        IER=VERIOB('O',NOM//'           .FSVI','TYPE_I',0,' ')
        IER=VERIOB('O',NOM//'           .FSVI','LONMAX',2,' ')
        CALL JEVEUO(NOM//'           .FSVI','L',JFSVI)
        CALL ASSERT((ZI(JFSVI).EQ.0).OR.(ZI(JFSVI).EQ.1))
        CALL ASSERT((ZI(JFSVI+1).EQ.1).OR.(ZI(JFSVI+1).EQ.2).OR.
     &              (ZI(JFSVI+1).EQ.3))
      ENDIF



C ----------------------------------------------------------------------

C---- 3. VERIFICATION DE L OBJET .FSVK

      IF (TYPFAI.EQ.1) THEN
C        FAISCEAU_TRANS
        IER=VERIOB('O',NOM//'           .FSVK','EXIS_V',0,' ')
        IER=VERIOB('O',NOM//'           .FSVK','TYPE_K8',0,' ')
        IER=VERIOB('O',NOM//'           .FSVK','LONMAX',4+NBZONE,' ')
        CALL JEVEUO(NOM//'           .FSVK','L',JFSVK)

        CALL VERIJA('O','CARA_ELEM',ZK8(JFSVK),M80,NVU,NOBJ)

        CALL ASSERT((ZK8(JFSVK+1).EQ.'DX').OR.
     &              (ZK8(JFSVK+1).EQ.'DY').OR.
     &              (ZK8(JFSVK+1).EQ.'DZ'))

        CALL JEEXIN(ZK8(JFSVK+2)//'           .NOVA',IRET)
C    CONCEPT DE TYPE FONCTION OU FORMULE
        IF (IRET.EQ.0) THEN
          CALL VERIJA('O','FONCTION',ZK8(JFSVK+2),
     &                M80,NVU,NOBJ)
        ELSE
          CALL VERIJA('O','FORMULE',ZK8(JFSVK+2),
     &                M80,NVU,NOBJ)
        ENDIF


        CALL JEEXIN(ZK8(JFSVK+3)//'           .NOVA',IRET)
C    CONCEPT DE TYPE FONCTION OU FORMULE
        IF (IRET.EQ.0) THEN
          CALL VERIJA('O','FONCTION',ZK8(JFSVK+3),
     &                M80,NVU,NOBJ)
        ELSE
          CALL VERIJA('O','FORMULE',ZK8(JFSVK+3),
     &                M80,NVU,NOBJ)
        ENDIF

        DO 30 IZONE=1,NBZONE
          CALL JEEXIN(ZK8(JFSVK+3+IZONE)//'           .NOVA',IRET)
C    CONCEPT DE TYPE FONCTION OU FORMULE
          IF (IRET.EQ.0) THEN
            CALL VERIJA('O','FONCTION',ZK8(JFSVK+3+IZONE),
     &                  M80,NVU,NOBJ)
          ELSE
            CALL VERIJA('O','FORMULE',ZK8(JFSVK+3+IZONE),
     &                  M80,NVU,NOBJ)
          ENDIF
 30     CONTINUE
      ELSE IF (TYPFAI.EQ.2) THEN
C     GRAPPE

C     L OBJET N EXISTE QUE S IL Y A COUPLAGE
        IF (COUPLA) THEN
          IER=VERIOB('O',NOM//'           .FSVK','EXIS_V',0,' ')
          IER=VERIOB('O',NOM//'           .FSVK','TYPE_K8',0,' ')
          IER=VERIOB('O',NOM//'           .FSVK','LONMAX',4,' ')
          CALL JEVEUO(NOM//'           .FSVK','L',JFSVK)
          CALL ASSERT((ZK8(JFSVK).EQ.'ASC_CEN').OR.
     &                (ZK8(JFSVK).EQ.'ASC_EXC').OR.
     &                (ZK8(JFSVK).EQ.'DES_CEN').OR.
     &                (ZK8(JFSVK).EQ.'DES_EXC'))

          CALL VERIJA('O','CARA_ELEM',ZK8(JFSVK+2),M80,NVU,NOBJ)
          CALL VERIJA('O','MODELE',ZK8(JFSVK+3),M80,NVU,NOBJ)
        ENDIF

      ELSE IF (TYPFAI.EQ.3) THEN
C     FAISCEAU_AXIAL
        CALL JEVEUO(NOM//'           .FSVK','L',JFSVK)
        IF (ZI(JFSVI).EQ.0) THEN
C  CALCUL SUR FAISCEAU COMPLET  
          IER=VERIOB('O',NOM//'           .FSVK','EXIS_V',0,' ')
          IER=VERIOB('O',NOM//'           .FSVK','TYPE_K8',0,' ')
          IER=VERIOB('O',NOM//'           .FSVK','LONMAX',3,' ')
          CALL VERIJA('O','CARA_ELEM',ZK8(JFSVK+2),M80,NVU,NOBJ)
        ELSE IF (ZI(JFSVI).EQ.1) THEN
C  CALCUL SUR FAISCEAU SIMPLIFIE
          IER=VERIOB('O',NOM//'           .FSVK','EXIS_V',0,' ')
          IER=VERIOB('O',NOM//'           .FSVK','TYPE_K8',0,' ')
          IER=VERIOB('O',NOM//'           .FSVK','LONMAX',2,' ')
        ENDIF

C  OBJET COMMUN
        CALL JEEXIN(ZK8(JFSVK)//'           .NOVA',IRET)
C    CONCEPT DE TYPE FONCTION OU FORMULE
        IF (IRET.EQ.0) THEN
          CALL VERIJA('O','FONCTION',ZK8(JFSVK),
     &                M80,NVU,NOBJ)
        ELSE
          CALL VERIJA('O','FORMULE',ZK8(JFSVK),
     &                M80,NVU,NOBJ)
        ENDIF

        CALL JEEXIN(ZK8(JFSVK+1)//'           .NOVA',IRET)
C    CONCEPT DE TYPE FONCTION OU FORMULE
        IF (IRET.EQ.0) THEN
          CALL VERIJA('O','FONCTION',ZK8(JFSVK+1),
     &               M80,NVU,NOBJ)
        ELSE
          CALL VERIJA('O','FORMULE',ZK8(JFSVK+1),
     &               M80,NVU,NOBJ)
        ENDIF

      ELSE IF (TYPFAI.EQ.4) THEN
C     COQUE_COAX
        IER=VERIOB('O',NOM//'           .FSVK','EXIS_V',0,' ')
        IER=VERIOB('O',NOM//'           .FSVK','TYPE_K8',0,' ')
        IER=VERIOB('O',NOM//'           .FSVK','LONMAX',3,' ')
        CALL JEVEUO(NOM//'           .FSVK','L',JFSVK)

        CALL VERIJA('O','CARA_ELEM',ZK8(JFSVK),M80,NVU,NOBJ)
        CALL VERIJA('O','MATER',ZK8(JFSVK+1),M80,NVU,NOBJ)
        CALL VERIJA('O','MATER',ZK8(JFSVK+2),M80,NVU,NOBJ)
      ENDIF


C ----------------------------------------------------------------------

C---- 4. VERIFICATION DE L OBJET .FSVR

      IF (TYPFAI.EQ.1) THEN
C     FAISCEAU_TRANS

        IER=VERIOB('O',NOM//'           .FSVR','EXIS_V',0,' ')
        IER=VERIOB('O',NOM//'           .FSVR','TYPE_R',0,' ')
        IF (COUPLA) THEN
          IER=VERIOB('O',NOM//'           .FSVR','LONMAX',
     &               3+2*NBZONE,' ')
        ELSE
          IER=VERIOB('O',NOM//'           .FSVR','LONMAX',1,' ')
        ENDIF

      ELSE IF (TYPFAI.EQ.2) THEN
C     GRAPPE
        IF (COUPLA) THEN
          IER=VERIOB('O',NOM//'           .FSVR','EXIS_V',0,' ')
          IER=VERIOB('O',NOM//'           .FSVR','TYPE_R',0,' ')
          IER=VERIOB('O',NOM//'           .FSVR','LONMAX',2,' ')
        ENDIF
      ELSE IF (TYPFAI.EQ.3) THEN
C     FAISCEAU_AXIAL
        IER=VERIOB('O',NOM//'           .FSVR','EXIS_V',0,' ')
        IER=VERIOB('O',NOM//'           .FSVR','TYPE_R',0,' ')
        CALL JELIRA(NOM//'           .FSVR','LONMAX',LONG,KBID)
        IF (SIMP) THEN
          CALL ASSERT((LONG.EQ.8+NBZONE).OR.(LONG.EQ.10+NBZONE))
        ELSE
          CALL ASSERT((LONG.EQ.8).OR.(LONG.EQ.10))
        ENDIF

      ELSE IF (TYPFAI.EQ.4) THEN
C     COQUE_COAX
        IER=VERIOB('O',NOM//'           .FSVR','EXIS_V',0,' ')
        IER=VERIOB('O',NOM//'           .FSVR','TYPE_R',0,' ')
        IER=VERIOB('O',NOM//'           .FSVR','LONMAX',7,' ')
      ENDIF


C ----------------------------------------------------------------------

C---- 5. VERIFICATION DE L OBJET .UNIT_FAISCEAU
C         N EXISTE QUE POUR L OPTION FAISCEAU_TRANS
      IF (TYPFAI.EQ.1) THEN
C     FAISCEAU_TRANS
        IER=VERIOB('O',NOM//'.UNIT_FAISCEAU','EXIS_V',0,' ')
        IER=VERIOB('O',NOM//'.UNIT_FAISCEAU','TYPE_I',0,' ')
        IER=VERIOB('O',NOM//'.UNIT_FAISCEAU','LONMAX',2,' ')
      ENDIF


C ----------------------------------------------------------------------

C---- 6. VERIFICATION DE L OBJET .FSGM
C         N EXISTE QUE POUR L OPTION FAISCEAU_AXIAL OU COQUE_COAX

      IF (TYPFAI.EQ.3) THEN
C     FAISCEAU_AXIAL
        IER=VERIOB('O',NOM//'           .FSGM','EXIS_V',0,' ')
        IER=VERIOB('O',NOM//'           .FSGM','TYPE_K8',0,' ')
C      SI ON UTILISE TRI_GROUP_MA AVEC UN FAISCEAU COMPLET,
C      NBZONE VAUT 0 ET ON DIMENSIONNE L OBJET FSGM A 1
        IF (NBZONE.EQ.0) THEN
          IER=VERIOB('O',NOM//'           .FSGM','LONMAX',1,' ')
        ELSE
          IER=VERIOB('O',NOM//'           .FSGM','LONMAX',NBZONE,' ')
        ENDIF

      ELSE IF (TYPFAI.EQ.4) THEN
        IER=VERIOB('O',NOM//'           .FSGM','EXIS_V',0,' ')
        IER=VERIOB('O',NOM//'           .FSGM','TYPE_K8',0,' ')
        IER=VERIOB('O',NOM//'           .FSGM','LONMAX',2,' ')
      ENDIF


C ----------------------------------------------------------------------

C---- 7. VERIFICATION DE L OBJET .FSGR
C         N EXISTE QUE POUR L OPTION FAISCEAU_TRANS EN PRESENCE
C         DE GRILLE

      IF ((TYPFAI.EQ.3).AND.(NBTYP.GT.0)) THEN
        IER=VERIOB('O',NOM//'           .FSGR','EXIS_V',0,' ')
        IER=VERIOB('O',NOM//'           .FSGR','TYPE_R',0,' ')
        IF (SIMP) THEN
          NBGRIL=ZI(JFSVI+6+NBZONE)
          IER=VERIOB('O',NOM//'           .FSGR','LONMAX',
     &               NBGRIL+6*NBTYP,' ')
        ELSE
          NBGRIL=ZI(JFSVI+5)
          IER=VERIOB('O',NOM//'           .FSGR','LONMAX',
     &               NBGRIL+6*NBTYP,' ')
        ENDIF
      ENDIF


C ----------------------------------------------------------------------

C---- 8. VERIFICATION DE L OBJET .FSCR
C         N EXISTE QUE POUR L OPTION FAISCEAU_TRANS REPRESENTATION 
C         SIMPLIFIE
      IF ((TYPFAI.EQ.3).AND. SIMP) THEN
        IER=VERIOB('O',NOM//'           .FSCR','EXIS_V',0,' ')
        IER=VERIOB('O',NOM//'           .FSCR','TYPE_R',0,' ')
        NBTUBE=ZI(JFSVI+5)
        IER=VERIOB('O',NOM//'           .FSCR','LONMAX',2*NBTUBE,' ')
      ENDIF


C ----------------------------------------------------------------------

C---- 9. VERIFICATION DE L OBJET .UNIT_GRAPPES
C         N EXISTE QUE POUR L OPTION GRAPPE AVEC COUPLAGE
      IF ((TYPFAI.EQ.2).AND.COUPLA) THEN
        IER=VERIOB('O',NOM//'.UNIT_GRAPPES','EXIS_V',0,' ')
        IER=VERIOB('O',NOM//'.UNIT_GRAPPES','TYPE_I',0,' ')
        IER=VERIOB('O',NOM//'.UNIT_GRAPPES','LONMAX',2,' ')
      ENDIF


      CALL JEDEMA()

      END
