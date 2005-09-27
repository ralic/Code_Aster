      SUBROUTINE RVPSTM ( SDLIEU, SDEVAL, SDMOYE )
      IMPLICIT REAL*8 (A-H,O-Z)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 23/09/2005   AUTEUR CIBHHLV L.VIVAN 
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
      CHARACTER*19 SDEVAL
      CHARACTER*24 SDMOYE,SDLIEU
C
C**********************************************************************
C
C  OPERATION REALISEE
C  ------------------
C
C     OPERATION MOYENNE DU POST-TRAITEMENT POUR UN LIEU
C
C  ARGUMENTS EN ENTREE
C  -------------------
C
C     SDLIEU : SD DU LIEU TRAITE
C     SDEVAL : SD DE L' EVALUATION DE LA QUANTITE SUR CE LIEU
C
C  ARGUMENTS EN SORTIE
C  -------------------
C
C     SDMOYE : NOM DE LA SD CONSERVANT LES MOYENNES
C
C              XD V R, UN OC PAR OC DE .ABSC DU LIEU
C                      DIM(V) = 6*NB_CMP*NB_COUCHE*NB_SS_PT
C
C                      1 --> MOYENNE DE TYPE 1
C                      2 --> MOYENNE DE TYPE 2
C                      3 --> MINIMUM
C                      4 --> MAXIMUM
C                      5 --> MINIMUM LINEAIRE
C                      6 --> MAXIMUM LINEAIRE
C
C**********************************************************************
C
C  FONCTIONS EXTERNES
C  ------------------
C
      CHARACTER*32 JEXNOM,JEXNUM
C
C  DECLARATION DES COMMUNS NORMALISES JEVEUX
C  -----------------------------------------
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
C  FIN DES COMMUNS NORMALISES JEVEUX
C  ---------------------------------
C
C  VARIABLES LOCALES
C  -----------------
C
      CHARACTER*24 NVALE,NPCMP,NPNBN,NPADR,NABSC,NNOCP,NTAB
      CHARACTER*4  DOCUL,DOCU
C
      INTEGER AVALE,APNBN,APADR,AMOYE,ADESC,AABSC,ATAB,ADR1,ADR2
      INTEGER DEB,FIN,LMOYE,NBCP,IFR,NBCO,NBSP,NBOC,NBSGT
      INTEGER L1,L2,L3,L4,L5,L6,L7,IOC,ICO,ISGT,ISP,K,I,N
C
C
      REAL*8 M1,M2,MA,MI,S1,S2,T1,T2,S12,L,T12,SMIL
      CHARACTER*1 K1BID,BL
C
C==================== CORPS DE LA ROUTINE =============================
C
      CALL JEMARQ()
      BL  = ' '
C
      NTAB = '&&RVPSTM.VECT.INTER'
C
      NVALE = SDEVAL//'.VALE'
      NPNBN = SDEVAL//'.PNBN'
      NNOCP = SDEVAL//'.NOCP'
      NPADR = SDEVAL//'.PADR'
C
      NABSC = SDLIEU(1:19)//'.ABSC'
C
      CALL JELIRA(SDLIEU(1:19)//'.REFE','DOCU',N,DOCUL)
      CALL JELIRA(NVALE,'DOCU',N,DOCU)
      CALL JELIRA(NABSC,'NMAXOC',NBOC,K1BID)
      CALL JEEXIN ( NNOCP , IRET )
      IF ( IRET .EQ. 0 ) THEN
        CALL UTMESS('F','RVPSTM','MANQUE LE VECTEUR "NOCP".')
      ENDIF
      CALL JELIRA(NNOCP,'LONMAX',NBCP,K1BID)
      CALL JECREC(SDMOYE,'V V R','NU','DISPERSE','VARIABLE',NBOC)
C
      CALL JEVEUO(SDEVAL//'.PNCO','L',AVALE)
C
      NBCO = ZI(AVALE)
C
      CALL JEVEUO(SDEVAL//'.PNSP','L',AVALE)
C
      NBSP = ZI(AVALE)
C
      CALL JEVEUO(NVALE,'L',AVALE)
      CALL JEVEUO(NPADR,'L',APADR)
      CALL JEVEUO(NPNBN,'L',APNBN)
C
      L2 = NBSP*NBCP
      L1 = NBCO*L2
      L3 = 2*L1
C
      LMOYE = 6*NBCP*NBCO*NBSP
      FIN   = 0
C
      DO 100, IOC = 1, NBOC, 1
C
         CALL JECROC(JEXNUM(SDMOYE,IOC))
         CALL JEECRA(JEXNUM(SDMOYE,IOC),'LONMAX',LMOYE,BL)
         CALL JEVEUO(JEXNUM(SDMOYE,IOC),'E',AMOYE)
         CALL JELIRA(JEXNUM(NABSC ,IOC),'LONMAX',NBSGT,K1BID)
         CALL JEVEUO(JEXNUM(NABSC ,IOC),'L',AABSC)
C
         NBSGT = NBSGT - 1
         IF ( NBSGT .EQ. 0 ) THEN
           CALL UTMESS('F','RVPSTM','CHEMIN NUL OU DEFINI EN UN NOEUD')
         ENDIF
         DEB   = FIN + 1
         FIN   = DEB + NBSGT
C
         IF ( (DOCU .EQ. 'CHLM') .OR. (DOCUL .NE. 'LSTN') ) THEN
C
            FIN = FIN - 1
C
         ENDIF
C
C     /* LONGEUR */
C
C
         IF ( DOCUL .NE. 'SGTD' ) THEN
C
            IF ( DOCUL .EQ. 'ARCC' ) THEN
C
               CALL JEVEUO(SDLIEU(1:19)//'.DESC','L',ADESC)
C
               L = 1.0D0/(ZR(ADESC + 3-1)*(ZR(ADESC+5-1)-ZR(ADESC+4-1)))
C
            ELSE
C
               L = 1.0D0/( ZR(AABSC+NBSGT) - ZR(AABSC) )
C
            ENDIF
C
         ELSE
C
            L = 1.0D0/( ZR(AABSC+NBSGT) - ZR(AABSC) )
C
         ENDIF
C
         IF (L2.GT.6) CALL UTMESS('F','RVPSTM','LE NOMBRE DE '//
     +   'COMPOSANTES A TRAITER EST LIMITE A 6 POUR OPERATION '//
     +   '"MOYENNE". UTILISER "NOM_CMP" AVEC AU PLUS 6 COMPOSANTES')
C
C     /* VECTEUR INTER */
C
         CALL WKVECT(NTAB,'V V R',L3*(NBSGT+1),ATAB)
C
         IF ( (DOCUL .EQ. 'LSTN') .OR. (DOCU .EQ. 'CHNO') ) THEN
C
            DO 200, ISGT = 1, NBSGT+1, 1
C
               ADR1 = ZI(APADR + DEB + ISGT-2)
               N    = ZI(APNBN + DEB + ISGT-2)
C
               DO 210, ICO = 1, NBCO, 1
C
                  L5 = (ICO-1)*N*L2
C
                  DO 220, K= 1, L2, 1
C
                     T1 = 0.0D0
C
                     LLL = 0
                     DO 230, I = 1, N, 1
C
              IF(ZR(AVALE-1+ADR1+L5+(I-1)*L2+K-1).EQ.R8VIDE()) GOTO 230
                        LLL = LLL + 1
                        T1 = T1 + ZR(AVALE-1+ADR1+L5+(I-1)*L2+K-1)
C
230                  CONTINUE
C
                     IF ( LLL .EQ. 0 ) THEN
                        T1 = 0.D0
                     ELSE
                        T1 = T1/LLL
                     ENDIF
C
                     ADR2 = (ISGT-1)*L3 + (ICO-1)*L2 + K
C
                     ZR(ATAB + ADR2   -1) = T1
                     ZR(ATAB + ADR2+L1-1) = T1
C
220               CONTINUE
C
210            CONTINUE
C
200         CONTINUE
C
         ELSE
C
            DO 240, ISGT = 1, NBSGT, 1
C
               ADR1 = ZI(APADR + DEB + ISGT-2)
C
               DO 250, ICO = 1, NBCO, 1
C
                  L5 = (ICO-1)*L2
C
                  DO 260, K= 1, L2, 1
C
                     ADR2 = (ISGT-1)*L3 + L5 + L1 + K
C
                     IF(ZR(AVALE+ADR1+2*L5+K-2).EQ.R8VIDE()) THEN
                        ZR(ATAB + ADR2   -1) = 0.D0
                     ELSE
                        ZR(ATAB + ADR2   -1) = ZR(AVALE+ADR1+2*L5+K-2)
                     ENDIF
                     IF(ZR(AVALE+ADR1+2*L5+L2+K-2).EQ.R8VIDE()) THEN
                        ZR(ATAB + ADR2+L1-1) = 0.D0
                     ELSE
                        ZR(ATAB+ADR2+L1-1) = ZR(AVALE+ADR1+2*L5+L2+K-2)
                     ENDIF
C
260               CONTINUE
C
250            CONTINUE
C
240         CONTINUE
C
         ENDIF
C
C
C     /* CONTRIBUTION ELEMENTAIRE */
C
         DO 110, ICMP = 1, NBCP, 1
C
            DO 120, ICO = 1, NBCO, 1
C
               L5 = L2*(ICO-1)
C
               DO 130, ISP = 1, NBSP, 1
C
                  L6 = NBCP*(ISP-1)
                  M1 =  0.0D0
                  M2 =  0.0D0
                  MA = -1.0D50
                  MI =  1.0D50
C
                  DO 140, ISGT = 1, NBSGT, 1
C
                     S1  = ZR(AABSC + ISGT-1) - ZR(AABSC)
                     S2  = ZR(AABSC + ISGT+1-1) - ZR(AABSC)
                     S12 = S2 - S1
C
                     ADR1 = L3*(ISGT-1) + L5 + L6 + ICMP
                     ADR2 = ADR1 + L1
C
                     T1 = ZR(ATAB-1 + L1 + ADR1)
                     T2 = ZR(ATAB-1 + L1 + ADR2)
                     T12  = (T1+T2)/2.0D0
                     SMIL = (S1+S2)/2.0D0
                  M1 = M1 + S12*(T1 + T2)
                  M2 = M2 + S12/3.0D0 * (T1*S1 + 4.0D0*T12*SMIL + T2*S2)
                     MA = MAX(MA,T1,T2)
                     MI = MIN(MI,T1,T2)
C
140               CONTINUE
C
                  M1 = M1*L
                  M2 = M2*L*L
C
                  M1 = 0.5D0*M1
                  M2 = 6.0D0*(M2 - M1)
C
                  L7 = 6*(NBSP*(NBCO*(ICMP-1) + ICO-1) + ISP-1)
C
                  ZR(AMOYE + L7 + 1-1) = M1
                  ZR(AMOYE + L7 + 2-1) = M2
                  ZR(AMOYE + L7 + 3-1) = MI
                  ZR(AMOYE + L7 + 4-1) = MA
                  ZR(AMOYE + L7 + 5-1) = M1 - 0.5D0*M2
                  ZR(AMOYE + L7 + 6-1) = M1 + 0.5D0*M2
C
130            CONTINUE
C
120         CONTINUE
C
110      CONTINUE
C
         CALL JEDETR(NTAB)
C
100   CONTINUE
C
      CALL JEDEMA()
      END
