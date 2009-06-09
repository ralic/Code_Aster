      SUBROUTINE CGMFTM(TYMAZ,NOMAZ,LISMAZ,NBMA,IERR)
      IMPLICIT   NONE
      INTEGER             NBMA,IERR
      CHARACTER*(*)       NOMAZ, LISMAZ, TYMAZ
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 09/06/2009   AUTEUR REZETTE C.REZETTE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2009  EDF R&D                  WWW.CODE-ASTER.ORG
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
C       OPERATEUR: DEFI_GROUP/CREA_GROUP_MA
C
C       CGMFTM -- TRAITEMENT DU FILTRE DES MAILLES
C                 EN FONCTION DE LEUR TYPE.
C
C -------------------------------------------------------
C  TYMA          - IN    - K8   - : TYPE DE MAILLE RETENU
C                                   ("TOUT","1D","2D","3D")
C  NOMAZ         - IN    - K8   - : NOM DU MAILLAGE
C  LISMAZ        - INOUT - K24  - : NOM DE LA LISTE DE MAILLES A FILTRER
C                                   ET FILTREE
C  NBMA          - INOUT -  I   - : LONGUEUR DE CETTE LISTE
C  IERR          - OUT   -  I   - : CODE RETOUR (=0 OU 1)
C                                   
C
C  REMARQUES : 
C     IERR=0 : OK
C        => ON A OBTENU DES MAILLES EN FILTRANT LA LISTE DE MAILLES, 
C           LA LISTE DE MAILLES RETOURNEE EST LA LISTE FILTREE,
C           LE NOMBRE DE MAILLES RETOURNE EST LA LONGUEUR DE LA LISTE
C           FILTREE.
C     IERR=1 : NOOK
C        => AUCUNE MAILLE N'A ETE RETENUE CAR LE TYPE DE MAILLE SOUHAITE
C           PAR L'UTILISATEUR NE CORRESPOND PAS AUX TYPES DES MAILLES
C           DE LA LISTE.
C           LA LISTE DE MAILLE RETOURNEE EST LA LISTE INITIALE,
C           LE NOMBRE DE MAILLES RETOURNE EST LA LONGUEUR DE LA LISTE
C           INITIALE.
C           
C -------------------------------------------------------
C
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
      CHARACTER*32     JEXNOM, JEXNUM, JEXATR
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------

      INTEGER ITYP,I,NN,JMALA2,JMALA,IMA
      CHARACTER*8  NOMA, TYMA, TYPMA
      CHARACTER*24 LISMA,LISMA2

      CALL JEMARQ()

      NOMA=NOMAZ
      LISMA=LISMAZ
      TYMA=TYMAZ

      CALL JEVEUO( NOMA//'.TYPMAIL', 'L', ITYP )
      CALL JEVEUO( LISMA, 'E', JMALA)

      LISMA2='&&CGMFTP.MA_FILTREES'
      CALL WKVECT(LISMA2,'V V I',NBMA,JMALA2)

      NN=0
      IERR=1

      IF(TYMA(1:2).EQ.'1D')THEN

        DO 10 I=1,NBMA
          IMA=ZI(JMALA+I-1)
          CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ZI(ITYP+IMA-1)),TYPMA)

          IF (TYPMA(1:3).EQ.'SEG' )THEN
              ZI(JMALA2+NN)=IMA
              NN=NN+1
          ENDIF

 10     CONTINUE

      ELSE IF(TYMA.EQ.'2D')THEN

        DO 20 I=1,NBMA
          IMA=ZI(JMALA+I-1)
          CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ZI(ITYP+IMA-1)),TYPMA)

          IF (TYPMA(1:4).EQ.'TRIA' .OR.
     &        TYPMA(1:4).EQ.'QUAD')THEN
              ZI(JMALA2+NN)=IMA
              NN=NN+1
          ENDIF

 20     CONTINUE

      ELSE IF(TYMA.EQ.'3D')THEN

        DO 30 I=1,NBMA
          IMA=ZI(JMALA+I-1)
          CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ZI(ITYP+IMA-1)),TYPMA)

          IF (TYPMA(1:5).EQ.'TETRA' .OR.
     &        TYPMA(1:5).EQ.'PENTA' .OR.
     &        TYPMA(1:5).EQ.'PYRAM' .OR.
     &        TYPMA(1:4).EQ.'HEXA')THEN
              ZI(JMALA2+NN)=IMA
              NN=NN+1
          ENDIF

 30     CONTINUE

      ENDIF

      IF(NN.NE.0)THEN
        IERR=0
        NBMA=NN
        DO 40 I=1,NBMA
          ZI(JMALA+I-1)=ZI(JMALA2+I-1)
 40     CONTINUE
      ENDIF

      CALL JEDETR(LISMA2)

      CALL JEDEMA()

      END
