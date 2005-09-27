      SUBROUTINE RVPSTS(IOCC,SDLIEU,SDEVAL,SDMOYE)
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
      INTEGER      IOCC
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
C     SDLIEU : SD DU LIEU TRAITEE
C     SDEVAL : SD DE L' EVALUATION DE LA QUANTITE SUR CE LIEU
C
C  ARGUMENTS EN SORTIE
C  -------------------
C
C     SDMOYE : NOM DE LA SD CONSERVANT LA SOMME ::= RECORD
C
C      .VALE : XD V R, UN OC PAR OC DE .ABSC DU LIEU
C                      DIM(V) = NB_CMP_SOMME*NB_COUCHE*NB_SS_PT
C      .NOCP : S V K8  NOM DES CMP
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
      CHARACTER*24 NABSC,NTAB
      CHARACTER*24 NSOCP,NSOVA
      CHARACTER*4  DOCUL,DOCU
C
      INTEGER AVALE,APNBN,APADR,AMOYE,AABSC,ATAB,ADR1,ADR2
      INTEGER DEB,FIN,LMOYE,NBCP,IFR,NBCO,NBSP,NBOC,NBSGT,NRES,NMOM
      INTEGER L1,L2,L3,L4,L5,IOC,I,J,K,L,N,NBPT
C
      REAL*8 T1,T(3),X,Y,Z,XPI,YPI,ZPI,RX,RY,RZ,MX,MY,MZ,ZERO
      CHARACTER*1 K1BID
C
C==================== CORPS DE LA ROUTINE =============================
C
      CALL JEMARQ()
      ZERO = 0.0D0
C
      NTAB  = '&&RVPSTM.VECT.INTER'
      NABSC = SDLIEU(1:19)//'.ABSC'
      NSOVA = SDMOYE(1:19)//'.VALE'
      NSOCP = SDMOYE(1:19)//'.NOCP'
C
      CALL GETVTX('ACTION','RESULTANTE',IOCC,1,0,ZK80,NRES)
      CALL GETVTX('ACTION','MOMENT'    ,IOCC,1,0,ZK80,NMOM)
      NRES = -NRES
      NMOM = -NMOM
C
      CALL JELIRA(SDLIEU(1:19)//'.REFE','DOCU',N,DOCUL)
      CALL JELIRA(SDEVAL//'.VALE','DOCU',N,DOCU)
      CALL JELIRA(NABSC,'NMAXOC',NBOC,K1BID)
      CALL JELIRA(SDEVAL//'.NOCP','LONMAX',NBCP,K1BID)
      CALL JECREC(NSOVA,'V V R','NU','DISPERSE','VARIABLE',NBOC)
C
      CALL JEVEUO(SDEVAL//'.PNCO','L',I)
      NBCO = ZI(I)
      CALL JEVEUO(SDEVAL//'.PNSP','L',I)
      NBSP = ZI(I)
C
      CALL JEVEUO(SDEVAL//'.VALE','L',AVALE)
      CALL JEVEUO(SDEVAL//'.PADR','L',APADR)
      CALL JEVEUO(SDEVAL//'.PNBN','L',APNBN)
C
      L2 = NBSP*NBCP
      L1 = NBCO*L2
      L3 = 2*L1
C
      IF ( NMOM .EQ. 0 ) THEN
         CALL WKVECT(NSOCP,'V V K8',NBCP,DEB)
         CALL JEVEUO(SDEVAL//'.NOCP','L',FIN)
         DO 15, IOC = 1, NBCP, 1
            ZK8(DEB + IOC-1) = ZK8(FIN + IOC-1)
15       CONTINUE
         LMOYE = NBCP*NBCO*NBSP
      ELSE
         CALL WKVECT(NSOCP,'V V K8',6,DEB)
         ZK8(DEB-1 + 1) = 'RESULT_X'
         ZK8(DEB-1 + 2) = 'RESULT_Y'
         ZK8(DEB-1 + 3) = 'RESULT_Z'
         ZK8(DEB-1 + 4) = 'MOMENT_X'
         ZK8(DEB-1 + 5) = 'MOMENT_Y'
         ZK8(DEB-1 + 6) = 'MOMENT_Z'
         LMOYE = 6*NBCO*NBSP
      ENDIF
C
      NBSGT = 0
      DO 99, IOC = 1, NBOC, 1
         CALL JELIRA(JEXNUM(NABSC ,IOC),'LONMAX',FIN,K1BID)
         NBSGT = MAX(NBSGT,FIN)
99    CONTINUE
      CALL WKVECT(NTAB,'V V R',L3*(NBSGT+1),ATAB)
C
      FIN   = 0
C
      DO 100, IOC = 1, NBOC, 1
C
         CALL JECROC(JEXNUM(NSOVA,IOC))
         CALL JEECRA(JEXNUM(NSOVA,IOC),'LONMAX',LMOYE,' ')
         CALL JEVEUO(JEXNUM(NSOVA,IOC),'E',AMOYE)
         CALL JELIRA(JEXNUM(NABSC,IOC),'LONMAX',NBPT,K1BID)
         CALL JEVEUO(JEXNUM(NABSC,IOC),'L',AABSC)
C
         NBSGT = NBPT - 1
         DEB   = FIN + 1
         FIN   = DEB + NBSGT
C
         IF ( (DOCU .EQ. 'CHLM') .OR. (DOCUL .NE. 'LSTN') ) THEN
C
            FIN = FIN - 1
C
         ENDIF
C
C     /* VECTEUR INTER IE : PASSAGE A UN SS_CHAM_NO */
C
         IF ( (DOCUL .EQ. 'LSTN') .OR. (DOCU .EQ. 'CHNO') ) THEN
C
            DO 200, I = 1, NBPT, 1
C
               ADR1 = ZI(APADR + DEB + I-2)
               N    = ZI(APNBN + DEB + I-2)
C
               DO 210, J = 1, NBCO, 1
C
                  L5 = (J-1)*N*L2
C
                  DO 220, K= 1, L2, 1
C
                     T1 = 0.0D0
C
                     LLL = 0
                     DO 230, L = 1, N, 1
C
                IF(ZR(AVALE-1+ADR1+L5+(L-1)*L2+K-1).EQ.R8VIDE())GOTO 230
                        LLL = LLL + 1
                        T1 = T1 + ZR(AVALE-1+ADR1+L5+(L-1)*L2+K-1)
C
230                  CONTINUE
C
                     IF(LLL.EQ.0) THEN
                       T1 = 0.D0
                     ELSE
                       T1 = T1/LLL
                     ENDIF
C
                     ADR2 = (I-1)*L1 + (J-1)*L2 + K - 1
C
                     ZR(ATAB + ADR2) = T1
C
220               CONTINUE
C
210            CONTINUE
C
200         CONTINUE
C
         ELSE
C
            ADR1 = ZI(APADR + DEB-1)
            DO 241, I = 1, NBCO, 1
               L5 = (I-1)*L2
               DO 242, J = 1, L2, 1
                 IF(ZR(AVALE+ADR1+2*L5+J-2).EQ.R8VIDE()) THEN
                   ZR(ATAB + L5 + J-1) = 0.D0
                 ELSE
                   ZR(ATAB + L5 + J-1) = ZR(AVALE+ADR1+2*L5+J-2)
                 ENDIF
242            CONTINUE
241         CONTINUE
C
            DO 240, I = 1, NBSGT-1, 1
C
               ADR1 = ZI(APADR + DEB + I-2)
C
               DO 250, J = 1, NBCO, 1
C
                  L5   = (J-1)*L2 + I*L1
                  ADR2 = AVALE + ADR1-1 + (J-1)*L3 + L2
C
                  DO 260, K= 1, L2, 1
C
                    IF (ZR(ADR2+K-1).EQ.R8VIDE() .AND.
     +                  ZR(ADR2+L2*(NBCO*2-1)+K-1).EQ.R8VIDE() ) THEN
                      ZR(ATAB+L5+K-1) = 0.D0
                    ELSEIF (ZR(ADR2+K-1).EQ.R8VIDE() ) THEN
                      ZR(ATAB+L5+K-1) = ZR(ADR2+L2*(NBCO*2-1)+K-1)
                    ELSEIF (ZR(ADR2+L2*(NBCO*2-1)+K-1).EQ.R8VIDE()) THEN
                      ZR(ATAB+L5+K-1) = ZR(ADR2 + K-1)
                    ELSE
                      ZR(ATAB+L5+K-1) = 0.5D0*(ZR(ADR2 + K-1) +
     +                                      ZR(ADR2+L2*(NBCO*2-1)+K-1))
                    ENDIF
C
260               CONTINUE
C
250            CONTINUE
C
240         CONTINUE
C
            ADR1 = AVALE + ZI(APADR + DEB + NBSGT-2)-1
            ADR2 = ATAB  + NBSGT*L1
            DO 243, J = 1, NBCO, 1
               L5 = (J-1)*L2
               DO 244, K = 1, L2, 1
                 IF (ZR(ADR1+(I-1)*L3+K-1).EQ.R8VIDE() ) THEN
                   ZR(ADR2 + L5 + K-1) = 0.D0
                 ELSE
                   ZR(ADR2 + L5 + K-1) = ZR(ADR1+(I-1)*L3+K-1)
                 ENDIF
244            CONTINUE
243         CONTINUE
C
         ENDIF
C
C
C     /* CALCUL DES SOMMES SUR LE SS_CHAM_NO */
C
         IF ( NMOM .EQ. 0 ) THEN
            DO 110, I = 1, L1, 1
               T1 = ZERO
               DO 120, J = 1, NBPT, 1
                  T1 = T1 + ZR(ATAB + (J-1)*L1 + I-1)
120            CONTINUE
               ZR(AMOYE + I-1) = T1
110         CONTINUE
         ELSE
            DO 140, I = 1, L1, 1
               ZR(AMOYE + I-1) = ZERO
140         CONTINUE
            CALL GETVR8('ACTION','POINT',IOCC,1,0,T,N)
            N = -N
            CALL GETVR8('ACTION','POINT',IOCC,1,N,T,I)
            X =  T(1)
            Y =  T(2)
            IF ( N .EQ. 2 ) THEN
               Z = ZERO
            ELSE
               Z = T(3)
            ENDIF
            CALL JEVEUO(JEXNUM(SDLIEU(1:19)//'.COOR',IOC),'L',K)
            DO 150, I = 1, NBCO*NBSP, 1
               ADR1 = (I-1)*NBCP
               ADR2 = (I-1)*6
               DO 151, J = 1, NBPT, 1
                  L   = (J-1)*3 + K-1
                  L5  = (J-1)*L1
                  XPI = ZR(L+1) - X
                  YPI = ZR(L+2) - Y
                  ZPI = ZR(L+3) - Z
C
                  IF ( NMOM .EQ. 3 ) THEN
                     RX  = ZR(ATAB + L5 + ADR1 + 1-1)
                     RY  = ZR(ATAB + L5 + ADR1 + 2-1)
                     RZ  = ZR(ATAB + L5 + ADR1 + 3-1)
                     MX  = ZR(ATAB + L5 + ADR1 + 4-1)
                     MY  = ZR(ATAB + L5 + ADR1 + 5-1)
                     MZ  = ZR(ATAB + L5 + ADR1 + 6-1)
                  ELSE
                     RX  = ZR(ATAB + L5 + ADR1 + 1-1)
                     RY  = ZR(ATAB + L5 + ADR1 + 2-1)
                     RZ  = ZERO
                     MX  = ZR(ATAB + L5 + ADR1 + 3-1)
                     MY  = ZR(ATAB + L5 + ADR1 + 4-1)
                     MZ  = ZERO
                  ENDIF
C
                  MX  = YPI*RZ - ZPI*RY + MX
                  MY  = ZPI*RX - XPI*RZ + MY
                  MZ  = XPI*RY - YPI*RX + MZ
C
                  ZR(AMOYE+ADR2+1-1) = ZR(AMOYE+ADR2+1-1) + RX
                  ZR(AMOYE+ADR2+2-1) = ZR(AMOYE+ADR2+2-1) + RY
                  ZR(AMOYE+ADR2+3-1) = ZR(AMOYE+ADR2+3-1) + RZ
                  ZR(AMOYE+ADR2+4-1) = ZR(AMOYE+ADR2+4-1) + MX
                  ZR(AMOYE+ADR2+5-1) = ZR(AMOYE+ADR2+5-1) + MY
                  ZR(AMOYE+ADR2+6-1) = ZR(AMOYE+ADR2+6-1) + MZ
151            CONTINUE
150         CONTINUE
         ENDIF
C
100   CONTINUE
C
      CALL JEDETR(NTAB)
C
      CALL JEDEMA()
      END
