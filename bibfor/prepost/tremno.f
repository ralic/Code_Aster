      SUBROUTINE TREMNO ( DIM, NCMP, NSSCHE, NOMSD )
      IMPLICIT NONE
      CHARACTER*2         DIM
      CHARACTER*8         NCMP, CBID
      CHARACTER*19        NSSCHE, NOMSD
C*********************************************************************
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 03/08/2001   AUTEUR CIBHHLV L.VIVAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C ======================================================================
C
C   OPERATION REALISEE
C   ------------------
C
C     REORGANISATION PAR NOEUDS D' UN SOUS_CHAMELEM
C
C   ARGUMENTS
C   ---------
C
C     DIM    (IN) : '2D' OU '3D' DIMENSION DE OMEGA
C                    POUR DISTINGUER LES MAILLES SURFACIQUES DES AUTRES
C
C     NOMSDL (IN) : NOM DE LA SD IMPLEMENTANT LA REORGANISATION
C
C     NCMP   (IN) : NOM DE LA COMPOSANTE TRAITEE
C
C     NSSCHE (IN) : NOM DE LA SD DE TYPE SOUS_CHAMPELEM A TRAITER
C
C     DESCRIPTION DE LA SD PRODUITES
C     ------------------------------
C
C         .VACP : XD V R8, UN OC CORRESPOND A LA LISTE DES VALEURS
C                 DE LA CMP SUR UN NOEUDS VU DES MAILLES LE CONTENANT
C
C         .NUMA : XD V I , UN OC CORRESPOND A LA LISTE DES MAILLES
C                 CONTENANT LE NOEUD DE L' OC DE VACP CORRESPONDANT
C
C         .NUND : S V I, VECTEUR DES NUMEROS DE NOEUDS CONCERNES
C
C         .NOCP : S E K8, NOM DE LA CMP
C
C         .NUCP : S E I, NUMERO DE LA CMP
C
C     L' OC NUMERO I CONTIENT LES VALEURS DE LA CMP SUR LE NOEUD
C     NUMERO NUND(I) (DANS LA NUMEROTATION DU MAILLAGE)
C
C*********************************************************************
C
C   FONCTIONS EXTERNES
C   ------------------
C
      CHARACTER*32 JEXNUM,JEXNOM
C
C   COMMUNS NORMALISES JEVEUX
C   -------------------------
C
      INTEGER         ZI
      COMMON /IVARJE/ ZI(1)
      REAL*8          ZR
      COMMON /RVARJE/ ZR(1)
      COMPLEX*16      ZC
      COMMON /CVARJE/ ZC(1)
      LOGICAL         ZL
      COMMON /LVARJE/ ZL(1)
      CHARACTER*8     ZK8
      CHARACTER*16    ZK16
      CHARACTER*24    ZK24
      CHARACTER*32    ZK32
      CHARACTER*80    ZK80
      COMMON /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
C   NOMS ET ADRESSES DES OJB ASSOCIES AU SOUS CHAMELEM
C   --------------------------------------------------
C
      CHARACTER*24 NPNBN,NPADR,NPCMP,NVALE,NNOMA,NNUGD
      INTEGER      APNBN,APADR,APCMP,AVALE,ANOMA,ANUGD,APNCO,APNSP
C
C   NOMS ET ADRESSES DES OJB ASSOCIES A LA SD
C   -----------------------------------------
C
      CHARACTER*24 NVACP,NNUND,NNUMA,NNUCP,NNOCP
      INTEGER      AVACP,ANUND,ANUMA,ANUCP,ANOCP
      INTEGER      PTM,PTV,TCO,TSP,NBCO,NBSP,LNGM,LNGV,ICO,ISP,IER
C
C   ADRESSE DE NUMERO DE CMP CONCERNEES PAR L' EXTRACTION
C   -----------------------------------------------------
C
      INTEGER  LIBRE,NBNM,NBTCMP,NUMCP,GD,ACONEC,ACMPGD,ALISTE
C
C   DIVERS
C   ------
C
      INTEGER      I,M,N,IN,IM,NBTND,NBTMAI,ADRM,NBN,NBM,NDLOC,NBCPAC
      CHARACTER*24 NCONEC,NCNCIN
      CHARACTER*8  TK8(1),NMAILA
      LOGICAL      TROUVE
      CHARACTER*1  K1BID
C
C================= FIN DES DECLARATIONS ============================
C
C   RECUPERATION DES NOMS ET DES OJB DU SOUS_CHAMELEM
C   -------------------------------------------------
C
      CALL JEMARQ()
      NPNBN  = NSSCHE//'.PNBN'
      NPADR  = NSSCHE//'.PADR'
      NPCMP  = NSSCHE//'.PCMP'
      NVALE  = NSSCHE//'.VALE'
      NNOMA  = NSSCHE//'.NOMA'
      NNUGD  = NSSCHE//'.NUGD'
C
      CALL JEVEUO(NPNBN,'L',APNBN)
      CALL JEVEUO(NPADR,'L',APADR)
      CALL JEVEUO(NPCMP,'L',APCMP)
      CALL JEVEUO(NVALE,'L',AVALE)
      CALL JEVEUO(NNOMA,'L',ANOMA)
      CALL JEVEUO(NNUGD,'L',ANUGD)
      CALL JEVEUO(NSSCHE//'.PNCO','L',APNCO)
      CALL JEVEUO(NSSCHE//'.PNSP','L',APNSP)
C
      NMAILA = ZK8(ANOMA)
C
      GD = ZI(ANUGD)
C
      NCONEC = NMAILA//'.CONNEX'
      NCNCIN = '&&OP0051.CONNECINVERSE'
C
C   CONSTRUCTION DES NOM DES OJB DE LA SD PRODUITE
C   ----------------------------------------------
C
      NVACP = NOMSD//'.VACP'
      NNUND = NOMSD//'.NUND'
      NNOCP = NOMSD//'.NOCP'
      NNUCP = NOMSD//'.NUCP'
      NNUMA = NOMSD//'.NUMA'
C
C   CREATION REMPLISSAGE DE .NOCP
C   -----------------------------
C
      CALL JECREO(NNOCP,'V E K8')
      CALL JEVEUO(NNOCP,'E',ANOCP)
C
      ZK8(ANOCP) = NCMP
C
C   CREATION REMPLISSAGE DE .NUCP
C   -----------------------------
C
      TK8(1) = NCMP
C
      CALL JECREO(NNUCP,'V E I')
      CALL JEVEUO(NNUCP,'E',ANUCP)
C
      CALL JEVEUO(JEXNUM('&CATA.GD.NOMCMP',GD),'L',ACMPGD)
      CALL JELIRA(NPCMP,'LONMAX',NBTCMP,K1BID)
      CALL NUMEK8 (ZK8(ACMPGD),TK8,NBTCMP,1,ZI(ANUCP))
C
      NUMCP = ZI(ANUCP)
C
C   RECUPERATION DU NBR TOTAL DE MAILLES
C   ------------------------------------
C
      CALL JELIRA(NPNBN,'LONMAX',NBTMAI,K1BID)
C
C   CALCUL DU NBR DE CMP ACTIVES
C   ----------------------------
C
      NBCPAC = 0
C
      DO 300, I = 1, NBTCMP, 1
C
         NBCPAC = NBCPAC + MIN(ZI(APCMP + I-1),1)
C
300   CONTINUE
C
C   CONSTRUCTION DU .NUND
C   ---------------------
C
C-DEL CALL JELIRA(NMAILA//'.NOMNOE','NOMMAX',NBTND,K1BID)
      CALL DISMOI('F','NB_NO_MAILLA',NMAILA,'MAILLAGE',NBTND,CBID,IER)
C
      CALL JECREO('&&TREMNO.LISTE.ENTIER','V V I')
      CALL JEECRA('&&TREMNO.LISTE.ENTIER','LONMAX',NBTND,' ')
      CALL JEVEUO('&&TREMNO.LISTE.ENTIER','E',ALISTE)
C
      LIBRE = 1
C
      DO 100, IM = 1, NBTMAI, 1
C
         IF ( ZI(APADR + IM-1) .NE. 0 ) THEN
C
            CALL JEVEUO(JEXNUM(NCONEC,IM),'L',ADRM)
C
            NBN = ZI(APNBN + IM-1)
C
            CALL I2TRGI(ZI(ALISTE),ZI(ADRM),NBN,LIBRE)
C
         ENDIF
C
100   CONTINUE
C
      NBTND = LIBRE - 1
C
      CALL JECREO(NNUND,'V V I')
      CALL JEECRA(NNUND,'LONMAX',NBTND,' ')
      CALL JEVEUO(NNUND,'E',ANUND)
C
      DO 110, IN = 1, NBTND, 1
C
         ZI(ANUND + IN-1) = ZI(ALISTE + IN-1)
C
110   CONTINUE
C
      CALL JEDETR('&&TREMNO.LISTE.ENTIER')
C
C   CONSTRUCTION DU .VACP
C   ---------------------
C
      CALL JECREC(NVACP,'V V R','NU','DISPERSE','VARIABLE',NBTND)
      CALL JECREC(NNUMA,'V V I','NU','DISPERSE','VARIABLE',NBTND)
C
      DO 200, IN = 1, NBTND, 1
C
         N = ZI(ANUND + IN-1)
C
         CALL JEVEUO(JEXNUM(NCNCIN,N),'L',ADRM)
         CALL JELIRA(JEXNUM(NCNCIN,N),'LONMAX',NBM,K1BID)
C
         LNGM = 0
         LNGV = 0
C
         DO 250, IM = 1, NBM, 1
C
            M = ZI(ADRM + IM-1)
C
            IF ( M .NE. 0 ) THEN
C
               LNGM = LNGM + MIN(ZI(APADR + M-1),1)
               LNGV = LNGV + MIN(ZI(APADR + M-1),1)*ZI(APNCO + M-1)*
     +                                              ZI(APNSP + M-1)
C
            ENDIF
C
250      CONTINUE
C
         IF ( LNGM .EQ. 0 ) THEN
C
            CALL UTDEBM('F','TREMNO','NOEUD NON CONTENU DANS UNE '//
     +                  'MAILLE SACHANT CALCULER L" OPTION')
            CALL UTIMPI('L','NOEUD NUMERO : ',1,N)
            CALL UTFINM()
C
         ENDIF
C
         CALL JECROC(JEXNUM(NVACP,IN))
         CALL JEECRA(JEXNUM(NVACP,IN),'LONMAX',LNGV,' ')
         CALL JEVEUO(JEXNUM(NVACP,IN),'E',AVACP)
C
         CALL JECROC(JEXNUM(NNUMA,IN))
         CALL JEECRA(JEXNUM(NNUMA,IN),'LONMAX',LNGM,' ')
         CALL JEVEUO(JEXNUM(NNUMA,IN),'E',ANUMA)
C
CDEL     WRITE(6,*)'N = ', N
CDEL
         PTM = 1
         PTV = 1
C
         DO 210, IM = 1, NBM, 1
C
            M = ZI(ADRM + IM-1)
CDEL     WRITE(6,*)'IM   = ', IM
CDEL     WRITE(6,*)'M    = ', M
CDEL     WRITE(6,*)'PADR = ', ZI(APADR + M-1)
CDEL     WRITE(6,*)'NBCO = ', ZI(APNCO + M-1)
CDEL     WRITE(6,*)'NBSP = ', ZI(APNSP + M-1)
C
            IF ( M .NE. 0 ) THEN
C
               IF ( ZI(APADR + M-1) .NE. 0 ) THEN
C
                  NBCO   = ZI(APNCO + M-1)
                  NBSP   = ZI(APNSP + M-1)
                  NDLOC  = 1
                  TROUVE = .FALSE.
C
                  CALL JEVEUO(JEXNUM(NCONEC,M),'L',ACONEC)
                  CALL JELIRA(JEXNUM(NCONEC,M),'LONMAX',NBNM,K1BID)
C
220               CONTINUE
                  IF ( (.NOT. TROUVE) .AND. (NDLOC .LE. NBNM) ) THEN
C
                     IF ( ZI(ACONEC + NDLOC-1) .EQ. N ) THEN
C
                        TROUVE = .TRUE.
C
                     ELSE
C
                        NDLOC = NDLOC + 1
C
                     ENDIF
C
                     GOTO 220
C
                  ENDIF
C
                  TSP = NBCPAC*NBSP
C
                  DO 231, ICO = 1, NBCO, 1
C
                     TCO = NBSP*NBCPAC*NBNM*(ICO-1)
C
                     DO 232, ISP = 1, NBSP, 1
C
                        ZR(AVACP + PTV-1) =
     +                  ZR(AVALE + ZI(APADR+M-1) + TCO + (NDLOC-1)*TSP +
     +                        (ISP-1)*NBCPAC + ZI(APCMP + NUMCP-1) - 2)
C
                        PTV = PTV + 1
C
232                  CONTINUE
C
231               CONTINUE
C
C
                  ZI(ANUMA + PTM-1) = M
C
                  PTM = PTM + 1
C
               ENDIF
C
            ENDIF
C
210      CONTINUE
C
200   CONTINUE
C
      CALL JEDEMA()
      END
