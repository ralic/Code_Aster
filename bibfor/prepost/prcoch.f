      SUBROUTINE PRCOCH(NOCHE8,NOCHS8,NOCMP,KTYPE,ITOPO,NGROUP,GROUP)
      
      IMPLICIT NONE
      INTEGER     ITOPO,NGROUP
      CHARACTER*8 NOCHE8,NOCHS8,NOCMP,KTYPE
      CHARACTER*8 GROUP(NGROUP)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 24/04/2007   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C --------------- COMMUNS NORMALISES  JEVEUX  --------------------------
      COMMON /IVARJE/ZI(1)
      COMMON /RVARJE/ZR(1)
      COMMON /CVARJE/ZC(1)
      COMMON /LVARJE/ZL(1)
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      INTEGER    ZI
      REAL*8     ZR
      COMPLEX*16 ZC
      LOGICAL    ZL
      CHARACTER*8  ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32,JEXNUM,JEXNOM,JEXATR
      CHARACTER*80 ZK80
C
      CHARACTER*24 LITROU
      CHARACTER*24 VALK
      CHARACTER*19 CELZ, CESZ
      CHARACTER*8  NOMMA,K8BID
      INTEGER      JCESD,JCESL,JCESC,JCESV,JCESK,NCMPMX,ICMP,NUMCMP
      INTEGER      NBMA,IMA,NBPT,NBSP,NBCMP,IPT,ISP,IAD,JVAL,JMA,JPO,JSP
      INTEGER     IVAL,IRET,K,ITRMA,ITRNO,IGR,NBTROU,ITBMA,LMA,NBVAL
      INTEGER     JCNSD,JCNSL,JCNSC,JCNSV,NBNO,INO,JNO,NBN,IN
      LOGICAL     LTOPO
      

      CALL JEMARQ()     
      LTOPO=(ITOPO.EQ.1)
      CELZ=NOCHE8
      CESZ=NOCHS8
      LITROU='&&PRCOCH.NUM_MAILLE'
      


C -------------------------------------------
C CHAM_ELEM
C -------------------------------------------
      IF (KTYPE(1:2).EQ.'EL') THEN
C -- ETAPE 0 : SI NOCHE8='__DETR__' : ON DETRUIT LES VECTEURS
        IF (NOCHE8.EQ.'__DETR__') THEN
          CALL JEDETR(CESZ // '.V')
          IF (LTOPO) THEN
            CALL JEDETR(CESZ // '.M')
            CALL JEDETR(CESZ // '.P')
            CALL JEDETR(CESZ // '.SP')
          ENDIF
          GOTO 6666
        ENDIF

C -- 1ERE ETAPE : CREATION CHAM_ELEM_S BEAUCOUP PLUS FACILE A MANIPULER
C                 ET DANS LA FOULEE REDUCTION SUR LES GROUP_MA 
C                 QUI NOUS INTERESSENT

        CALL CELCES(CELZ,'V',CESZ)

C  --- TRAITEMENT DES LISTES DE GROUPES DE MAILLES---
        IF(NGROUP.NE.0) THEN
          CALL JEVEUO(CESZ//'.CESK','L',JCESK)
          NOMMA=ZK8(JCESK-1+1)
          CALL JEEXIN(LITROU,IRET)
          IF (IRET.NE.0) CALL JEDETR(LITROU)
C     --- RECUPERATION DU NUMERO DE MAILLE----
          CALL DISMOI('F','NB_MA_MAILLA',NOMMA,'MAILLAGE',NBMA,
     &                K8BID,IRET)
          CALL WKVECT('&&PRCOCH.INDIC_MAILLE','V V I',NBMA,ITRMA)

          DO 13 IGR=1,NGROUP
            CALL JEEXIN(JEXNOM(NOMMA//'.GROUPEMA',GROUP(IGR)),IRET)
            IF (IRET.EQ.0) THEN
              VALK = GROUP(IGR)
              CALL U2MESG('A', 'PREPOST5_31',1,VALK,0,0,0,0.D0)
            ELSE
              CALL JELIRA(JEXNOM(NOMMA//'.GROUPEMA',GROUP(IGR)),
     +                       'LONMAX',NBN,K8BID)
              CALL JEVEUO(JEXNOM(NOMMA//'.GROUPEMA',GROUP(IGR)),'L',IAD)
              DO 14 IN=1,NBN
                IMA = ZI(IAD-1+IN)
                ZI(ITRMA-1+IMA) = 1
  14          CONTINUE
            ENDIF
  13      CONTINUE
                    
          NBTROU = 0
          DO 100 IMA = 1,NBMA
            IF (ZI(ITRMA-1+IMA).NE.0) NBTROU = NBTROU + 1
  100     CONTINUE
          IF (NBTROU.EQ.0) THEN
            CALL U2MESS('S', 'CHAMPS_4')
          ENDIF


          CALL WKVECT(LITROU,'V V I',NBTROU,ITBMA)
C         --- RANGEMENT DES NUMEROS DE MAILLES ---
          LMA = 0
          DO 110 IMA = 1,NBMA
            IF (ZI(ITRMA-1+IMA).NE.0) THEN
              LMA = LMA + 1
              ZI(ITBMA-1+LMA) = IMA
            END IF
  110     CONTINUE
          CALL CESRED(CESZ,NBTROU,ZI(ITBMA),0,K8BID,'V',CESZ)
          CALL JEDETR('&&PRCOCH.INDIC_MAILLE')
        ENDIF
      
C -- 2EME ETAPE : RECUPERATION DU NUMCMP QUI NOUS INTERESSE      
     
        CALL JEVEUO(CESZ//'.CESD','L',JCESD)
        CALL JEVEUO(CESZ//'.CESL','L',JCESL)
        CALL JEVEUO(CESZ//'.CESC','L',JCESC)
        CALL JEVEUO(CESZ//'.CESV','L',JCESV)
        
        NCMPMX = ZI(JCESD-1+2)
        
        DO 5 ICMP=1,NCMPMX
          IF (ZK8(JCESC-1+ICMP).EQ.NOCMP) THEN
            NUMCMP=ICMP
            GOTO 6
          ENDIF
5       CONTINUE

6       CONTINUE      

C -- 3EME ETAPE : RECUPERATION DE LA LONGUEUR DU VECTEUR DE VALEURS
C                   UTILES - ON GARDE QUE LES VALEURS DE LA CMP REMPLIES
C                   ET CREATION DES VECTEURS

        NBVAL=0
        NBMA = ZI(JCESD-1+1)

        DO 10 IMA=1,NBMA
          

          NBPT=ZI(JCESD-1+5+4*(IMA-1)+1)
          NBSP=ZI(JCESD-1+5+4*(IMA-1)+2)

          DO 10 IPT=1,NBPT
            DO 10 ISP=1,NBSP
              CALL CESEXI('C',JCESD,JCESL,IMA,IPT,ISP,NUMCMP,IAD)
              IF (IAD.GT.0) NBVAL=NBVAL+1
10      CONTINUE              


        IF (NBVAL.EQ.0) THEN
          CALL U2MESS('S', 'CHAMPS_1')
        ENDIF
        CALL WKVECT(CESZ // '.V','G V R',NBVAL,JVAL)
        IF (LTOPO) THEN
          CALL WKVECT(CESZ // '.M','G V I',NBVAL,JMA)
          CALL WKVECT(CESZ // '.P','G V I',NBVAL,JPO)
          CALL WKVECT(CESZ // '.SP','G V I',NBVAL,JSP)
        ENDIF  
        

C -- 4EME ETAPE : REMPLISSAGE DES VECTEURS
        IVAL=0
        DO 20 IMA=1,NBMA
          

          NBPT=ZI(JCESD-1+5+4*(IMA-1)+1)
          NBSP=ZI(JCESD-1+5+4*(IMA-1)+2)
          
          DO 20 IPT=1,NBPT
            DO 20 ISP=1,NBSP
              CALL CESEXI('C',JCESD,JCESL,IMA,IPT,ISP,NUMCMP,IAD)
              IF (IAD.GT.0) THEN
                IVAL=IVAL+1
                ZR(JVAL-1+IVAL)=ZR(JCESV-1+IAD)
                IF (LTOPO) THEN
                  ZI(JMA-1+IVAL)=IMA
                  ZI(JPO-1+IVAL)=IPT
                  ZI(JSP-1+IVAL)=ISP
                ENDIF
              ENDIF              
20      CONTINUE              

C -------------------------------------------
C CHAM_NO
C -------------------------------------------


      ELSE IF (KTYPE(1:2).EQ.'NO') THEN
C -- ETAPE 0 : SI NOCHE8='__DETR__' : ON DETRUIT LES VECTEURS
        IF (NOCHE8.EQ.'__DETR__') THEN
          CALL JEDETR(CESZ // '.V')
          IF (LTOPO) THEN
            CALL JEDETR(CESZ // '.N')
          ENDIF
          GOTO 6666
        ENDIF

C -- 1ERE ETAPE : CREATION CHAM_NO_S BEAUCOUP PLUS FACILE A MANIPULER
C                 DANS LA FOULEE ON LE REDUIT SUR LA LISTE DE GROUP_NO

        CALL CNOCNS(CELZ,'V',CESZ)
      
C  --- TRAITEMENT DES LISTES DE GROUPES DE MAILLES---
        IF(NGROUP.NE.0) THEN
          CALL JEVEUO(CESZ//'.CNSK','L',JCESK)
          NOMMA=ZK8(JCESK-1+1)
          CALL JEEXIN(LITROU,IRET)
          IF (IRET.NE.0) CALL JEDETR(LITROU)
C     --- RECUPERATION DU NUMERO DE NOEUDS----
          CALL DISMOI('F','NB_NO_MAILLA',NOMMA,'MAILLAGE',NBMA,
     &                K8BID,IRET)
          CALL WKVECT('&&PRCOCH.INDIC_NOEUD','V V I',NBMA,ITRNO)

          DO 130 IGR=1,NGROUP
            CALL JEEXIN(JEXNOM(NOMMA//'.GROUPENO',GROUP(IGR)),IRET)
            IF (IRET.EQ.0) THEN
              VALK = GROUP(IGR)
              CALL U2MESG('A', 'PREPOST5_31',1,VALK,0,0,0,0.D0)
            ELSE
              CALL JELIRA(JEXNOM(NOMMA//'.GROUPENO',GROUP(IGR)),
     +                       'LONMAX',NBN,K8BID)
              CALL JEVEUO(JEXNOM(NOMMA//'.GROUPENO',GROUP(IGR)),'L',IAD)
              DO 140 IN=1,NBN
                IMA = ZI(IAD-1+IN)
                ZI(ITRNO-1+IMA) = 1
 140          CONTINUE
            ENDIF
 130      CONTINUE
                    
          NBTROU = 0
          DO 1000 IMA = 1,NBMA
            IF (ZI(ITRNO-1+IMA).NE.0) NBTROU = NBTROU + 1
 1000     CONTINUE
          IF (NBTROU.EQ.0) THEN
            CALL U2MESS('S', 'CHAMPS_5')
          ENDIF


          CALL WKVECT(LITROU,'V V I',NBTROU,ITBMA)
C         --- RANGEMENT DES NUMEROS DE NOEUDS ---
          LMA = 0
          DO 1100 IMA = 1,NBMA
            IF (ZI(ITRNO-1+IMA).NE.0) THEN
              LMA = LMA + 1
              ZI(ITBMA-1+LMA) = IMA
            END IF
 1100     CONTINUE
          CALL CNSRED(CESZ,NBTROU,ZI(ITBMA),0,K8BID,'V',CESZ)
          CALL JEDETR('&&PRCOCH.INDIC_NOEUD')
        ENDIF


C -- 2EME ETAPE : RECUPERATION DU NUMCMP QUI NOUS INTERESSE      
     
        CALL JEVEUO(CESZ//'.CNSD','L',JCNSD)
        CALL JEVEUO(CESZ//'.CNSC','L',JCNSC)
        CALL JEVEUO(CESZ//'.CNSL','L',JCNSL)
        CALL JEVEUO(CESZ//'.CNSV','L',JCNSV)
        
        NCMPMX = ZI(JCNSD-1+2)
        
        DO 50 ICMP=1,NCMPMX
          IF (ZK8(JCNSC-1+ICMP).EQ.NOCMP) THEN
            NUMCMP=ICMP
            GOTO 60
          ENDIF
50      CONTINUE

C       SI LA COMPOSANTE DEMANDEE N EXISTE PAS, ERREUR FATALE
        CALL U2MESK('F','CHAMPS_3',1,NOCMP)

60      CONTINUE      


C -- 3EME ETAPE : RECUPERATION DE LA LONGUEUR DU VECTEUR DE VALEURS
C                   UTILES - ON GARDE QUE LES VALEURS DE LA CMP REMPLIES
C                   ET CREATION DES VECTEURS

        NBVAL=0
        NBNO = ZI(JCNSD-1+1)

        DO 70 INO=1,NBNO          
          IF (ZL(JCNSL-1+(INO-1)*NCMPMX+NUMCMP)) NBVAL=NBVAL+1
70     CONTINUE              

        IF (NBVAL.EQ.0) THEN
          CALL U2MESS('S','CHAMPS_1')
        ENDIF

        CALL WKVECT(CESZ // '.V','G V R',NBVAL,JVAL)
        IF (LTOPO) THEN
          CALL WKVECT(CESZ // '.N','G V I',NBVAL,JNO)
        ENDIF  

C -- 4EME ETAPE : REMPLISSAGE DES VECTEURS
        IVAL=0
        DO 200 INO=1,NBNO
          IF (ZL(JCNSL-1+(INO-1)*NCMPMX+NUMCMP)) THEN
            IVAL=IVAL+1
            ZR(JVAL-1+IVAL)=ZR(JCNSV-1+(INO-1)*NCMPMX+NUMCMP)
            IF (LTOPO) THEN
              ZI(JNO-1+IVAL)=INO
            ENDIF
          ENDIF              
200      CONTINUE              
      ENDIF

      
6666  CONTINUE
      CALL JEDEMA()
      END
