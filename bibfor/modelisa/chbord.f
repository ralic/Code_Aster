      SUBROUTINE CHBORD ( NOMO, NBMAIL, LISTMA, MABORD, NBMAPR, NBMABO )
      IMPLICIT   NONE
      INTEGER             NBMAIL, LISTMA(*), MABORD(*), NBMAPR, NBMABO
      CHARACTER*(*)       NOMO
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 26/06/2006   AUTEUR CIBHHLV L.VIVAN 
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
C      OPERATEURS :     AFFE_CHAR_MECA ET AFFE_CHAR_MECA_C
C                                      ET AFFE_CHAR_MECA_F
C
C IN  : NOMO   : NOM DU MODELE
C IN  : NBMAIL : NOMBRE DE MAILLES
C IN  : LISTMA : LISTE DES NUMEROS DE MAILLE
C OUT : MABORD : MABORD(IMA) = 0 , MAILLE "PRINCIPAL"
C                MABORD(IMA) = 1 , MAILLE "BORD"
C OUT : NBMAPR : NOMBRE DE MAILLES "PRINCIPAL"
C OUT : NBMABO : NOMBRE DE MAILLES "BORD"
C
C-----------------------------------------------------------------------
C --------------- COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER           ZI
      COMMON / IVARJE / ZI(1)
      REAL*8            ZR
      COMMON / RVARJE / ZR(1)
      COMPLEX*16        ZC
      COMMON / CVARJE / ZC(1)
      LOGICAL           ZL
      COMMON / LVARJE / ZL(1)
      CHARACTER*8       ZK8
      CHARACTER*16              ZK16
      CHARACTER*24                       ZK24
      CHARACTER*32                                ZK32
      CHARACTER*80                                         ZK80
      COMMON / KVARJE / ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
      CHARACTER*32      JEXNOM, JEXNUM
C     ----------- COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER       IRET, NBGREL, IGREL, IALIEL, NEL, ITYPEL, IMA, IER,
     +              NUMAIL, IEL, TRAITE
      CHARACTER*1   K1B
      CHARACTER*8   K8B, MODELE, DMO, DMA
      CHARACTER*16  NOMTE
      CHARACTER*19  NOLIG
C     ------------------------------------------------------------------
C
      MODELE = NOMO
      NBMAPR = 0
      NBMABO = 0
      NOLIG = MODELE//'.MODELE'
C
      CALL JEEXIN ( NOLIG//'.LIEL', IRET )
      IF (IRET.EQ.0) GO TO 999
C
      CALL JELIRA(NOLIG//'.LIEL','NUTIOC',NBGREL,K1B)
      IF (NBGREL.LE.0) GO TO 999
C
      TRAITE = 0
      DO 10 IGREL = 1 , NBGREL
         CALL JEVEUO (JEXNUM(NOLIG//'.LIEL',IGREL),'L',IALIEL)
         CALL JELIRA (JEXNUM(NOLIG//'.LIEL',IGREL),'LONMAX',NEL,K1B)
         ITYPEL = ZI(IALIEL -1 +NEL)
         CALL JENUNO(JEXNUM('&CATA.TE.NOMTE',ITYPEL),NOMTE)
         DO 20 IMA = 1 , NBMAIL
            NUMAIL = LISTMA(IMA)                    
            DO 30 IEL = 1 , NEL-1
               IF ( NUMAIL .EQ. ZI(IALIEL-1+IEL) ) THEN
                  TRAITE = TRAITE + 1
                  CALL TEATTR (NOMTE,'C','DIM_TOPO_MODELI',DMO,IER)
                  CALL TEATTR (NOMTE,'C','DIM_TOPO_MAILLE',DMA,IER)
                  IF ( DMO .EQ. DMA ) THEN
C                    on a un element principal
                     MABORD(IMA) = 0
                     NBMAPR = NBMAPR + 1
                  ELSE
C                    on a un element de bord 
                     MABORD(IMA) = 1
                     NBMABO = NBMABO + 1
                  ENDIF
                  GO TO 20
               ENDIF
 30         CONTINUE
 20      CONTINUE
         IF ( TRAITE .EQ. NBMAIL ) GO TO 999
 10   CONTINUE
C
 999  CONTINUE
C
      END
