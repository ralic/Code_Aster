      SUBROUTINE CCVRPU(RESUIN,LISORD,NBORDR)
      IMPLICIT NONE
      INTEGER      NBORDR
      CHARACTER*8  RESUIN
      CHARACTER*19 LISORD
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 04/01/2012   AUTEUR SELLENET N.SELLENET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE SELLENET N.SELLENET
C ----------------------------------------------------------------------
C  CALC_CHAMP - VERIFICATION DES PARAMETRES UTILISATEURS :
C  -    -       - -              -          -
C    MODELE, CARA_ELEM, ...
C ----------------------------------------------------------------------
C
C  LE BUT DE CETTE ROUTINE EST DE VERIFIER QUE L'UTILISATEUR NE
C    SURCHARGE PAS LE MODELE, CARA_ELEM, CHAM_MATER OU LE CHARGEMENT
C  SI UN DE CES ELEMENTS EST PRESENT DANS LA SD ET QUE L'UTILISATEUR
C    LE DONNE EN ENTREE DE CALC_CHAMP, ON VERIFIE QUE C'EST LE MEME
C    SINON ON INTERDIT LA REENTRANCE
C
C IN  :
C   RESUIN K8   NOM DE LA SD IN
C   LISORD K19  NOM DE LA LISTE DES NUMEROS D'ORDRE
C   NBORDR I    NOMBRE DE NUMEROS D'ORDRE
C ----------------------------------------------------------------------
C   ----- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER        ZI
      COMMON /IVARJE/ZI(1)
      REAL*8         ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16     ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL        ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8    ZK8
      CHARACTER*16          ZK16
      CHARACTER*24                  ZK24
      CHARACTER*32                          ZK32
      CHARACTER*80                                  ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     ----- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      INTEGER      JORDR,IORDR,NUMORD,JPARA,N1,N2,N3,NCHALU,ICHARG
      INTEGER      LCHALU,FCHALU,NCHASD,JLCHA,JINFC,JFCHA,ILU,ISD
      INTEGER      GETEXM,IARG
C
      CHARACTER*8  K8B,MODELU,CARELU,CHMATU,MODELR,CARELR,CHMATR
      CHARACTER*8  FONCLU
      CHARACTER*16 VALK(3)
      CHARACTER*19 KCHA,KFON,EXCIT
      CHARACTER*24 EXCISD
C
      KCHA = '&&CCVRPU.CHARGE    '
      KFON = '&&CCVRPU.FONC_MULT '
C
      CALL JEVEUO(LISORD,'L',JORDR)
C
      MODELU = ' '
      CARELU = ' '
      CHMATU = ' '
      CALL GETVID(' ','MODELE'    ,0,IARG,1,MODELU,N1)
      CALL GETVID(' ','CARA_ELEM' ,0,IARG,1,CARELU,N2)
      CALL GETVID(' ','CHAM_MATER',0,IARG,1,CHMATU,N3)
C
      NCHALU=0
      IF (GETEXM('EXCIT','CHARGE').EQ.1) THEN
        CALL GETFAC('EXCIT',NCHALU)
C
        IF ( NCHALU.NE.0 ) THEN
          CALL WKVECT(KCHA,'V V K8',NCHALU,LCHALU)
          CALL WKVECT(KFON,'V V K8',NCHALU,FCHALU)
C
          DO 10 ICHARG = 1, NCHALU
            CALL GETVID('EXCIT','CHARGE',ICHARG,IARG,1,
     &                  ZK8(LCHALU+ICHARG-1),N1)
C
            CALL GETVID('EXCIT','FONC_MULT',ICHARG,IARG,1,FONCLU,N2)
C
            IF ( N2.NE.0 ) THEN
              ZK8(FCHALU+ICHARG-1) = FONCLU
            ENDIF
  10      CONTINUE
        ENDIF
      ENDIF
C
      IF ( MODELU.NE.' '.OR.CARELU.NE.' '.OR.
     &     CHMATU.NE.' '.OR.NCHALU.NE.0 ) THEN
        DO 20, IORDR = 1,NBORDR
          NUMORD = ZI(JORDR-1+IORDR)
C
C         VERIFICATION DU MODELE
          IF ( MODELU.NE.' ' ) THEN
            CALL RSADPA(RESUIN,'L',1,'MODELE',NUMORD,0,JPARA,K8B)
            MODELR = ZK8(JPARA)
            IF ( MODELR.NE.' '.AND.MODELR.NE.MODELU ) THEN
              VALK(1) = 'MODELE'
              VALK(2) = MODELR
              VALK(3) = MODELU
              CALL U2MESK('F','CALCULEL_33',3,VALK)
              CALL ASSERT(.FALSE.)
            ENDIF
          ENDIF
C
C         VERIFICATION DU CARAELEM
          IF ( CARELU.NE.' ' ) THEN
            CALL RSADPA(RESUIN,'L',1,'CARAELEM',NUMORD,0,JPARA,K8B)
            CARELR=ZK8(JPARA)
            IF ( CARELR.NE.' '.AND.CARELR.NE.CARELU ) THEN
              VALK(1) = 'CARA_ELEM'
              VALK(2) = CARELR
              VALK(3) = CARELU
              CALL U2MESK('F','CALCULEL_33',3,VALK)
              CALL ASSERT(.FALSE.)
            ENDIF
          ENDIF
C
C         VERIFICATION DU CHAMATER
          IF ( CHMATU.NE.' ' ) THEN
            CALL RSADPA(RESUIN,'L',1,'CHAMPMAT',NUMORD,0,JPARA,K8B)
            CHMATR=ZK8(JPARA)
            IF ( CHMATR.NE.' '.AND.CHMATR.NE.CHMATU ) THEN
              VALK(1) = 'CHAM_MATER'
              VALK(2) = CHMATR
              VALK(3) = CHMATU
              CALL U2MESK('F','CALCULEL_33',3,VALK)
              CALL ASSERT(.FALSE.)
            ENDIF
          ENDIF
C
C         VERIFICATION DU CHARGEMENT
          IF ( NCHALU.NE.0 ) THEN
            CALL RSADPA(RESUIN,'L',1,'EXCIT',NUMORD,0,JPARA,K8B)
            EXCISD=ZK24(JPARA)
            NCHASD=0
            IF (EXCISD.NE.' ') THEN
              EXCIT=EXCISD(1:19)
              CALL JEVEUO(EXCIT//'.LCHA','L',JLCHA)
              CALL JEVEUO(EXCIT//'.INFC','L',JINFC)
              CALL JEVEUO(EXCIT//'.FCHA','L',JFCHA)
              NCHASD = ZI(JINFC)
              IF ( NCHASD.NE.NCHALU ) THEN
                CALL U2MESS('F','CALCULEL_39')
                CALL ASSERT(.FALSE.)
              ENDIF
              DO 40 ILU = 1,NCHALU
                DO 50 ISD = 1,NCHASD
                  IF(ZK8(LCHALU-1+ILU).EQ.ZK24(JLCHA-1+ISD)(1:8))
     &              GOTO 30
  50            CONTINUE
                CALL U2MESS('F','CALCULEL_39')
  30            CONTINUE
  40          CONTINUE
            ENDIF
          ENDIF
  20    CONTINUE
      ENDIF
C
      CALL JEDETR(KCHA)
      CALL JEDETR(KFON)
C
      END
