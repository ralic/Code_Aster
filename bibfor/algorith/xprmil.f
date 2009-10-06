      SUBROUTINE XPRMIL(NOMA,CNSLT,CNSLN)
      IMPLICIT NONE
      CHARACTER*19   CNSLT,CNSLN
      CHARACTER*8    NOMA
  
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 06/10/2009   AUTEUR GENIAUT S.GENIAUT 
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
C RESPONSABLE MASSIN P.MASSIN
C     ------------------------------------------------------------------
C
C       XPRMIL   : X-FEM PROPAGATION ; EXTENSION AUX NOEUDS MILIEUX
C       ------     -     --                                 ---
C    EXTENSION DES CHAM_NO_S LEVEL SETS AUX NOEUDS MILIEUX
C     AFIN DE RESTITUER, APRES PROPAGATION, UNE FISSURE DANS LA MEME
C     CONFIGURATION QU'APRES LES 2 PREMIERES PARTIES DE OP0041
C
C    ENTREE
C        NOMA   : NOM DU MAILLAGE
C        CNSLT  : CHAM_NO_S LST
C        CNSLN  : CHAM_NO_S LSN
C
C    SORTIE
C        CNSLT  : CHAM_NO_S LST
C        CNSLN  : CHAM_NO_S LSN
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
      CHARACTER*32    JEXNUM,JEXATR
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------

      INTEGER        IFM,NIV,IRET,NBMA,JMA,JCONX1,JCONX2,JLNNO,JLTNO,
     &               IMA,AR(12,2),NBAR,IA,NA,NB,NUNOA,NUNOB,NMIL,NOMIL,
     &               NUNOM
      REAL*8         LSNA,LSNB,LSTA,LSTB
      CHARACTER*8    K8BID,TYPMA
      CHARACTER*19   MAI
      LOGICAL        ISMALI
      
C-----------------------------------------------------------------------
C     DEBUT
C-----------------------------------------------------------------------

      CALL JEMARQ()
      CALL INFMAJ()
      CALL INFNIV(IFM,NIV)

      CALL DISMOI('F','NB_MA_MAILLA',NOMA,'MAILLAGE',NBMA,K8BID,IRET)
      MAI=NOMA//'.TYPMAIL'
      CALL JEVEUO(MAI,'L',JMA)
      CALL JEVEUO(NOMA//'.CONNEX','L',JCONX1)
      CALL JEVEUO(JEXATR(NOMA//'.CONNEX','LONCUM'),'L',JCONX2)
      
      CALL JEVEUO(CNSLN//'.CNSV','E',JLNNO)
      CALL JEVEUO(CNSLT//'.CNSV','E',JLTNO)

C     BOUCLE SUR TOUTES LES MAILLES DU MAILLAGE
      DO 100 IMA=1,NBMA
        CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ZI(JMA-1+IMA)),TYPMA)
        
        IF (ISMALI(TYPMA)) GOTO 100
        
        CALL CONARE(TYPMA,AR,NBAR)
        
C       BOUCLE SUR LES ARETES DE LA MAILLE
        DO 110 IA=1,NBAR
C       ON RECUPERE LES NUMEROS DES 2 NOEUDS DE L'ARETE
          NA=AR(IA,1)
          NB=AR(IA,2)
          NUNOA=ZI(JCONX1-1+ZI(JCONX2+IMA-1)+NA-1)
          NUNOB=ZI(JCONX1-1+ZI(JCONX2+IMA-1)+NB-1)

C       ON CALCULE LES LEVEL SETS AUX 2 NOEUDS
          LSNA=ZR(JLNNO-1+(NUNOA-1)+1)
          LSNB=ZR(JLNNO-1+(NUNOB-1)+1)
          LSTA=ZR(JLTNO-1+(NUNOA-1)+1)
          LSTB=ZR(JLTNO-1+(NUNOB-1)+1)

C       ON RECUPERE LE NUMERO DU NOEUD MILIEU
          NMIL=NOMIL(TYPMA,IA)
          NUNOM=ZI(JCONX1-1+ZI(JCONX2+IMA-1)+NMIL-1)
          
C       ON REMPLI LES CHAM_NO_S AVEC LES VALEUR DE LEVEL SETS MOYENNES
          ZR(JLNNO-1+(NUNOM-1)+1) = (LSNA+LSNB)/2.D0
          ZR(JLTNO-1+(NUNOM-1)+1) = (LSTA+LSTB)/2.D0

 110    CONTINUE
 100  CONTINUE
 
C-----------------------------------------------------------------------
C     FIN
C-----------------------------------------------------------------------
      CALL JEDEMA()
      END   
