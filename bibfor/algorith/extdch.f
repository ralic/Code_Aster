      SUBROUTINE EXTDCH(TYPEXT,VALINC,NOCHAM,NOCMP,DVAL)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 28/02/2011   AUTEUR BARGELLI R.BARGELLINI 
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
C RESPONSABLE GENIAUT S.GENIAUT
C
      IMPLICIT     NONE
      REAL*8       DVAL
      CHARACTER*8  TYPEXT
      CHARACTER*16 NOCHAM,NOCMP
      CHARACTER*19 VALINC(*)
C
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (ALGORITHME)
C
C    CALCUL D'UN EXTREMUM (MIN, MAX, EN VALEUR ABSOLUE OU NON)
C    DE L'INCREMENT D'UN CHAMP ('DEPL', 'SIEL_ELGA', OU 'VARI_ELGA')
C
C ----------------------------------------------------------------------
C
C
C IN  TYPEXT : TYPE D'EXTREMUM : MIN(), MAX(), MIN(ABS()), MAX(ABS())
C IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
C IN  NOCHAM : NOM DU CHAMP
C IN  NOCMP  : NOM DE LA COMPOSANTE
C OUT DVAL   : EXTREMUM

C
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      INTEGER      JCNSV,JCNSL,JCNSD
      INTEGER      NBNO,INO,IB
      INTEGER      JCESD,JCESL,JCESV
      INTEGER      NBMA,IMA,IPT,ISP,ICMP,NBPT,NBSP,NBCMP,IAD
      REAL*8       VALEUR,R8MAEM,R8MIEM,R8PREM
      CHARACTER*6  NOMPRO
      CHARACTER*16 TYPCH
      CHARACTER*19 DCH,DCHS,CHPLU,CHMOI
      PARAMETER   (NOMPRO = 'EXTDCH')
      
C      REAL*8  TMP
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C     
      CALL ASSERT(TYPEXT.EQ.'MIN'.OR.
     &            TYPEXT.EQ.'MAX'.OR.
     &            TYPEXT.EQ.'MIN_ABS'.OR.
     &            TYPEXT.EQ.'MAX_ABS'.OR.
     &            TYPEXT.EQ.'MIN_VAR')

      CALL ASSERT(NOCHAM.EQ.'VARI_ELGA'.OR.
     &            NOCHAM.EQ.'SIEF_ELGA'.OR.
     &            NOCHAM.EQ.'DEPL')

C     DECOMPACTION DES VARIABLES CHAPEAUX
      IF (NOCHAM.EQ.'VARI_ELGA') THEN
        CALL NMCHEX(VALINC,'VALINC','VARMOI',CHMOI)
        CALL NMCHEX(VALINC,'VALINC','VARPLU',CHPLU)          
        TYPCH = 'CHAM_ELGA'
      ELSEIF (NOCHAM.EQ.'SIEF_ELGA') THEN
        CALL NMCHEX(VALINC,'VALINC','SIGMOI',CHMOI)
        CALL NMCHEX(VALINC,'VALINC','SIGPLU',CHPLU)
        TYPCH = 'CHAM_ELGA'
      ELSEIF (NOCHAM.EQ.'DEPL') THEN
        CALL NMCHEX(VALINC,'VALINC','DEPMOI',CHMOI)            
        CALL NMCHEX(VALINC,'VALINC','DEPPLU',CHPLU)            
        TYPCH = 'CHAM_NO'
      ENDIF

C     INITIALISATION DE L'EXTREMUM
      IF (TYPEXT.EQ.'MIN'.OR.TYPEXT.EQ.'MIN_ABS'.
     &                    OR.TYPEXT.EQ.'MIN_VAR') DVAL = R8MAEM()
    
      IF (TYPEXT.EQ.'MAX')                        DVAL = R8MIEM()
      IF (TYPEXT.EQ.'MAX_ABS')                    DVAL = 0.D0

C     CALCUL DE L'INCREMENT DU CHAMP
C     DCH = CHPLU - CHMOI
      DCH  = '&&'//NOMPRO//'.DELTACH   '
      DCHS = '&&'//NOMPRO//'.DELTACHS  '
      CALL BARYCH(CHPLU,CHMOI,1.D0,-1.D0,DCH,'V')

C     ON APPELLERA MEMAX QUAND CETTE ROUTINE SERA MIEUX PROGRAMMEE
      IF (TYPCH(1:7).EQ.'CHAM_EL') THEN

        CALL CELCES(DCH,'V',DCHS)
        CALL CESRED(DCHS,0,IB,1,NOCMP,'V',DCHS)
        CALL JEVEUO(DCHS//'.CESD','L',JCESD)
        CALL JEVEUO(DCHS//'.CESL','L',JCESL)
        CALL JEVEUO(DCHS//'.CESV','L',JCESV)
        NBMA = ZI(JCESD-1+1)
        DO 40,IMA = 1,NBMA
          NBPT  = ZI(JCESD-1+5+4*(IMA-1)+1)
          NBSP  = ZI(JCESD-1+5+4*(IMA-1)+2)
          NBCMP = ZI(JCESD-1+5+4*(IMA-1)+3)
          DO 30,IPT = 1,NBPT
            DO 20,ISP = 1,NBSP
              DO 10,ICMP = 1,NBCMP
                CALL CESEXI('C',JCESD,JCESL,IMA,IPT,ISP,ICMP,IAD)
                IF (IAD.GT.0) THEN
                  VALEUR = ZR(JCESV-1+IAD)

                  
                  IF (TYPEXT(5:7).EQ.'ABS') VALEUR = ABS(VALEUR)
                  IF (TYPEXT(5:7).EQ.'VAR') THEN
                    IF (ABS(VALEUR).GT.R8PREM()) THEN
                      VALEUR =1.D-3/ABS(VALEUR)
C                      DVAL = MIN(DVAL,TMP)                     
                    ELSE 
                      VALEUR=R8MAEM()
                    ENDIF  
           
                  ENDIF
                  IF (TYPEXT(1:3).EQ.'MIN') THEN
                    DVAL = MIN(DVAL,VALEUR)
                    
                  ELSEIF (TYPEXT(1:3).EQ.'MAX') THEN
                    DVAL = MAX(DVAL,VALEUR)
                    
                  ENDIF
  
                ENDIF
   10         CONTINUE
   20       CONTINUE
   30     CONTINUE
   40   CONTINUE
        
      ELSEIF (TYPCH.EQ.'CHAM_NO') THEN

        CALL CNOCNS(DCH,'V',DCHS)
        CALL CNSRED(DCHS,0,IB,1,NOCMP,'V',DCHS)
        CALL JEVEUO(DCHS//'.CNSV','L',JCNSV)
        CALL JEVEUO(DCHS//'.CNSL','L',JCNSL)
        CALL JEVEUO(DCHS//'.CNSD','L',JCNSD)
        NBNO = ZI(JCNSD-1+1)
        DO 60,INO=1,NBNO
          IF (ZL(JCNSL-1+INO)) THEN
            VALEUR = ABS(ZR(JCNSV-1+INO))
            IF (TYPEXT(5:7).EQ.'ABS') VALEUR = ABS(VALEUR)
            IF (TYPEXT(1:3).EQ.'MIN') THEN
              DVAL = MIN(DVAL,VALEUR)
            ELSEIF (TYPEXT(1:3).EQ.'MAX') THEN
              DVAL = MAX(DVAL,VALEUR)
            ENDIF
          ENDIF
   60   CONTINUE

      ENDIF

      CALL JEDEMA()
      END
