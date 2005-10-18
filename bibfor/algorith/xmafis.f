      SUBROUTINE XMAFIS(NOMA,CNSLN,NXMAFI,MAFIS,NMAFIS)
      IMPLICIT NONE 

      INTEGER       NXMAFI,NMAFIS
      CHARACTER*8   NOMA
      CHARACTER*19  CNSLN
      CHARACTER*24  MAFIS  
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/10/2005   AUTEUR GENIAUT S.GENIAUT 
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
C                      TROUVER LES MAILLES OÙ LSN CHANGE DE SIGNE
C
C     ENTREE
C       NOMA     :  NOM DE L'OBJET MAILLAGE
C       CNSLN    :  LEVEL-SETS
C       NXMAFI   :  NOMBRE MAX DE MAILLES DE LA ZONE FISSURE
C
C     SORTIE
C       MAFIS    :  MAILLES DE LA ZONE FISSURE
C       NMAFIS   :  NOMBRE DE MAILLES DE LA ZONE FISSURE
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
      INTEGER       JDLIMA,JCONX1,JCONX2,JLNSV,JMAFIS
      INTEGER       I,IMAE,IN
      INTEGER       NMAABS,NBNOMA,NUNO,NBMAE
      REAL*8        LSNP,LSN
      CHARACTER*24  LISMA
      CHARACTER*32  JEXATR

C ----------------------------------------------------------------------

      CALL JEMARQ()
      LISMA = '&&XMAFIS.LISTE_MA_ENRICH'
      CALL RELIEM(' ',NOMA,'NU_MAILLE',' ',1,1,
     &                     'GROUP_MA_ENRI','GROUP_MA',LISMA,NBMAE)
      CALL JEVEUO(LISMA,'L',JDLIMA)

      CALL JEVEUO(NOMA//'.CONNEX','L',JCONX1)
      CALL JEVEUO(JEXATR(NOMA//'.CONNEX','LONCUM'),'L',JCONX2)

C     RÉCUPÉRATION DES LEVEL-SETS
      CALL JEVEUO(CNSLN//'.CNSV','L',JLNSV)

      CALL JEVEUO(MAFIS,'L',JMAFIS)

      I=0
C     BOUCLE SUR LES MAILLES DE GROUP_ENRI
      DO 100 IMAE=1,NBMAE
        NMAABS=ZI(JDLIMA-1+(IMAE-1)+1)
        NBNOMA=ZI(JCONX2+NMAABS) - ZI(JCONX2+NMAABS-1)
        IN=1
        NUNO=ZI(JCONX1-1+ZI(JCONX2+NMAABS-1)+IN-1)
        LSNP=ZR(JLNSV-1+(NUNO-1)+1)
        DO 101 IN=2,NBNOMA
          NUNO=ZI(JCONX1-1+ZI(JCONX2+NMAABS-1)+IN-1)
          LSN=ZR(JLNSV-1+(NUNO-1)+1)
          IF ((LSNP*LSN).LE.0) THEN
C           LSN A CHANGÉ DE SIGNE DONC ON STOCKE LA MAILLE DANS MAFIS
            I=I+1
            ZI(JMAFIS-1+I)=NMAABS
            IF ((I-1).GE.NXMAFI) THEN
              CALL UTMESS('E','XMAFIS','AUGMENTER NXMAFI')
            ENDIF
            GOTO 100
          ENDIF
 101    CONTINUE
 100  CONTINUE
      NMAFIS=I

      CALL JEDEMA()
      END
