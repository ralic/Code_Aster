      SUBROUTINE CFMXR0(DEFICO,RESOCO,NOMA  ,FONACT)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 22/12/2009   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT     NONE
      CHARACTER*24 DEFICO,RESOCO
      CHARACTER*8  NOMA
      INTEGER      FONACT(*)
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (TOUTES METHODES - POST-TRAITEMENT)
C
C CREER LE CHAM_NO_S POUR L ARCHIVAGE DU CONTACT 
C
C ----------------------------------------------------------------------
C
C
C IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
C IN  NOMA   : NOM DU MAILLAGE
C IN  FONACT : FONCTIONNALITES ACTIVEES (VOIR NMFONC)
C
C ------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------------
C
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
C --------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------
C 
      INTEGER      NBCMP,NBCMX
      PARAMETER    (NBCMP = 24,NBCMX = 27)
      CHARACTER*8  NOMCMP(NBCMP),NOMCMX(NBCMX)
      INTEGER      NBPER
      PARAMETER    (NBPER = 4)
      CHARACTER*8  NOMPER(NBPER)            
C
      INTEGER      CFMMVD,ZRESU,ZPERC,ZRESX,CFDISD
      INTEGER      IFM,NIV
      INTEGER      IZONE,ISURF,ILIAI,ICMP,IRET,IER
      INTEGER      NBLIAI,JDECNO,NBNOE,POSNOE,NUMNOE,NBNO
      INTEGER      CFDISI,NZOCO
      CHARACTER*24 CONTNO
      INTEGER      JNOCO
      INTEGER      JCNSVR,JCNSLR
      CHARACTER*19 CNSINR
      INTEGER      JCNSVP,JCNSLP
      CHARACTER*8  K8BID
      LOGICAL      ISFONC,LCTCC,LCTCD,LXFCM
      
      CHARACTER*19 CNSPER      
C ----------------------------------------------------------------------
      DATA NOMCMP
     &   / 'CONT','JEU' ,'RN'  ,
     &     'RNX' ,'RNY' ,'RNZ' ,
     &     'GLIX','GLIY','GLI' ,
     &     'RTAX','RTAY','RTAZ',
     &     'RTGX','RTGY','RTGZ',
     &     'RX'  ,'RY'  ,'RZ'  ,
     &     'R'   ,'HN'  ,'I'   ,
     &     'IX'  ,'IY'  ,'IZ'  /
C ----------------------------------------------------------------------
      DATA NOMCMX
     &   / 'CONT','JEU' ,'RN'  ,
     &     'RNX' ,'RNY' ,'RNZ' ,
     &     'GLIX','GLIY','GLI' ,
     &     'RTAX','RTAY','RTAZ',
     &     'RTGX','RTGY','RTGZ',
     &     'RX'  ,'RY'  ,'RZ'  ,
     &     'R'   ,'HN'  ,'I'   ,
     &     'IX'  ,'IY'  ,'IZ'  ,
     &     'PT_X','PT_Y','PT_Z'/     
C ----------------------------------------------------------------------
      DATA NOMPER
     &   / 'V1','V2','V3','V4'/     
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFNIV(IFM,NIV)
C
C --- ACCES SD CONTACT
C    
      CALL DISMOI('F','NB_NO_MAILLA',NOMA,'MAILLAGE',NBNO,K8BID,IER)
      CNSINR = RESOCO(1:14)//'.VALE'
      CNSPER = RESOCO(1:14)//'.PERC'    
      ZRESU  = CFMMVD('ZRESU')
      ZPERC  = CFMMVD('ZPERC')
      ZRESX  = CFMMVD('ZRESX')
      IF (ZRESU.NE.NBCMP) THEN
        CALL ASSERT(.FALSE.)
      ENDIF
      IF (ZRESX.NE.NBCMX) THEN
        CALL ASSERT(.FALSE.)
      ENDIF      
      IF (ZPERC.NE.NBPER) THEN
        CALL ASSERT(.FALSE.)
      ENDIF   
C       
      LCTCC  = ISFONC(FONACT,'CONT_CONTINU')
      LCTCD  = ISFONC(FONACT,'CONT_DISCRET')
      LXFCM  = ISFONC(FONACT,'CONT_XFEM')               
C
C -- LONGUEUR DU CHAM_NO_S VALE_CONT 
C      
      IF (LCTCD) THEN
        NBLIAI = CFDISD(RESOCO,'NBLIAI')
        NZOCO  = CFDISI(DEFICO,'NZOCO')
        CONTNO = DEFICO(1:16)//'.NOEUCO'
        CALL JEVEUO(CONTNO,'L',JNOCO )
      ELSEIF (LCTCC) THEN  
        NBLIAI = NBNO
      ENDIF                  
C
C --- CREATION DU CHAM_NO_S VALE_CONT
C
      IF (LXFCM) THEN
        CALL JEEXIN(CNSINR(1:19)//'.CNSV',IRET)
        IF (IRET.EQ.0) THEN
          CALL CNSCRE(NOMA,'INFC_R',ZRESX,NOMCMX,'V',CNSINR)
        ENDIF       
      ELSEIF (LCTCC.OR.LCTCD) THEN
        CALL JEEXIN(CNSINR(1:19)//'.CNSV',IRET)
        IF (IRET.EQ.0) THEN
          CALL CNSCRE(NOMA,'INFC_R',ZRESU,NOMCMP,'V',CNSINR)
        ENDIF  
        CALL JEVEUO(CNSINR(1:19)//'.CNSV','E',JCNSVR)
        CALL JEVEUO(CNSINR(1:19)//'.CNSL','E',JCNSLR)
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF
C
C --- INITIALISATION DU CHAM_NO_S VALE_CONT - CAS DISCRET
C
      IF (LCTCD) THEN
        DO 10 IZONE = 1,NZOCO
          CALL CFZONE(DEFICO,IZONE,'ESCL',ISURF)
          CALL CFNBSF(DEFICO,ISURF,'NOEU',NBNOE,JDECNO)
          DO 11 POSNOE = 1,NBNOE
            NUMNOE = ZI(JNOCO-1+JDECNO+POSNOE)
            DO 12 ICMP = 1,ZRESU
              ZR(JCNSVR-1+ZRESU*(NUMNOE-1)+ICMP) = 0.D0
              ZL(JCNSLR-1+ZRESU*(NUMNOE-1)+ICMP) = .TRUE.
 12         CONTINUE
 11       CONTINUE
 10     CONTINUE
      ENDIF
C
C --- INITIALISATION DU CHAM_NO_S VALE_CONT - CAS CONTINU
C
      IF (LCTCC) THEN
        DO 20 ILIAI = 1,NBLIAI
          DO 21 ICMP = 1,ZRESU
            ZR(JCNSVR-1+ZRESU*(ILIAI-1)+ICMP) = 0.D0
            ZL(JCNSLR-1+ZRESU*(ILIAI-1)+ICMP) = .TRUE.
 21       CONTINUE
          ZR(JCNSVR-1+ZRESU*(ILIAI-1)+2 ) = 0.0D0
 20     CONTINUE
      ENDIF
C
C --- CREATION DU CHAM_NO_S PERCUSSION
C 
      CALL JEEXIN(CNSPER(1:19)//'.CNSV',IRET)
      IF (IRET.EQ.0) THEN
        CALL CNSCRE(NOMA,'VARI_R',ZPERC,NOMPER,'V',CNSPER)
      ENDIF  
      CALL JEVEUO(CNSPER(1:19)//'.CNSV','E',JCNSVP)
      CALL JEVEUO(CNSPER(1:19)//'.CNSL','E',JCNSLP)   
C
C --- INITIALISATION DU CHAM_NO_S PERCUSSION
C --- ON NE REMET PAS A ZERO D'UN PAS A L'AUTRE
C
      IF (LCTCD) THEN
        IF (IRET.EQ.0) THEN    
          DO 1 IZONE = 1,NZOCO
            CALL CFZONE(DEFICO,IZONE,'ESCL',ISURF)
            CALL CFNBSF(DEFICO,ISURF,'NOEU',NBNOE,JDECNO)
            DO 2 POSNOE = 1,NBNOE
              NUMNOE = ZI(JNOCO-1+JDECNO+POSNOE)
              DO 3 ICMP = 1,ZPERC
                ZR(JCNSVP-1+ZPERC*(NUMNOE-1)+ICMP) = 0.D0
                ZL(JCNSLP-1+ZPERC*(NUMNOE-1)+ICMP) = .FALSE.
 3            CONTINUE
 2          CONTINUE
 1        CONTINUE
        ENDIF
      ENDIF

      IF (LCTCC) THEN
        IF (IRET.EQ.0) THEN    
          DO 4 ILIAI = 1,NBLIAI     
            DO 5 ICMP = 1,ZPERC
              ZR(JCNSVP-1+ZPERC*(ILIAI-1)+ICMP) = 0.D0
              ZL(JCNSLP-1+ZPERC*(ILIAI-1)+ICMP) = .FALSE.
 5          CONTINUE
 4        CONTINUE
        ENDIF
      ENDIF
C
      CALL JEDEMA()
      END
