      SUBROUTINE CHRPAN( MODELE, CARTE, CHELEM )
      IMPLICIT       NONE
      CHARACTER*(*)  MODELE, CARTE, CHELEM
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 21/09/2011   AUTEUR COURTOIS M.COURTOIS 
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
C ======================================================================
C     COMMANDE MODI_REPERE :
C     SURCHARGE ALPHA ET BETA DANS LA CARTE '.CARORIE'
C     ------------------------------------------------------------------
C IN  : MODELE : MODELE
C IN  : CARTE  : CARTE A TRANSFORMER EN CHAM ELEM
C OUT : CHELEM : CHAM ELEM AVEC ANGLES EVENTUELLEMENT VARIABLES
C     ------------------------------------------------------------------
C     ----- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
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
      INTEGER        IBID,IOC,N1,N2,NA,NVEC,IRET,NREP,
     &               NBMA,NBMAIL,JMAIL,IALPHA,IBETA,IAD1,IAD2,IMA,
     &               NUMMA,NCMAX,ICESK,ICESL,ICESV,ICESC,ICESD,
     &               INDIK8,IE,NNCP
      REAL*8         ANG(2), VECT(3), R8PI
      LOGICAL        LTOUT
      CHARACTER*8    K8B, NOMA, MOTCLS(2), TYPMCL(2)
      CHARACTER*19   CHELMS
      CHARACTER*24   MESMAI, LIGRMO
      INTEGER      IARG
C --- ------------------------------------------------------------------
      CALL GETFAC ( 'AFFE', NREP )
      IF ( NREP .EQ. 0 ) GOTO 9999
C --- ------------------------------------------------------------------
C --- PASSAGE PAR UN CHAM_ELEM_S
      K8B = ' '
      CHELMS = '&&CHRPAN.ELEM_S  '
      CALL CARCES ( CARTE, 'ELEM', K8B, 'V', CHELMS, IRET )
C
      CALL JEVEUO ( CHELMS//'.CESK', 'L', ICESK )
      CALL JEVEUO ( CHELMS//'.CESC', 'L', ICESC )
      CALL JEVEUO ( CHELMS//'.CESD', 'L', ICESD )
      CALL JEVEUO ( CHELMS//'.CESL', 'E', ICESL )
      CALL JEVEUO ( CHELMS//'.CESV', 'E', ICESV )
C
      NOMA   = ZK8(ICESK)
      NBMAIL =  ZI(ICESD)
      NCMAX  =  ZI(ICESD+1)
C --- ------------------------------------------------------------------
C --- INDICE DE 'ALPHA' ET 'BETA' DANS LA CARTE
      IALPHA = INDIK8 ( ZK8(ICESC), 'ALPHA   ', 1, NCMAX )
      IBETA  = INDIK8 ( ZK8(ICESC), 'BETA    ', 1, NCMAX )
      CALL ASSERT (IALPHA.EQ.1.AND.IBETA.EQ.2)
C
      MOTCLS(1) = 'GROUP_MA'
      MOTCLS(2) = 'MAILLE'
      TYPMCL(1) = 'GROUP_MA'
      TYPMCL(2) = 'MAILLE'
      MESMAI = '&&CHRPAN.MES_MAILLES'
C
C --- ------------------------------------------------------------------
      DO 10 IOC = 1 , NREP
         CALL GETVTX ( 'AFFE', 'MAILLE'  , IOC,IARG,0, K8B, N1 )
         CALL GETVTX ( 'AFFE', 'GROUP_MA', IOC,IARG,0, K8B, N2 )
         IF ( N1+N2 .EQ. 0 ) THEN
            LTOUT = .TRUE.
            NBMA  = NBMAIL
         ELSE
            CALL RELIEM(' ', NOMA, 'NU_MAILLE', 'AFFE', IOC, 2,
     &                                  MOTCLS, TYPMCL, MESMAI, NBMA )
            IF ( NBMA.NE.0 )  CALL JEVEUO ( MESMAI, 'L', JMAIL )
            LTOUT = .FALSE.
         ENDIF
C
         ANG(1) = 0.D0
         ANG(2) = 0.D0
         CALL GETVR8 ( 'AFFE', 'ANGL_REP', IOC,IARG,2, ANG , NA   )
         CALL GETVR8 ( 'AFFE', 'VECTEUR' , IOC,IARG,3, VECT, NVEC )
         IF (NVEC.NE.0) THEN
            CALL ANGVX ( VECT, ANG(1), ANG(2) )
            ANG(1)=  ANG(1)*180.D0/R8PI()
            ANG(2)= -ANG(2)*180.D0/R8PI()
         ENDIF
C
         DO 30 IMA = 1 , NBMA
            IF ( LTOUT ) THEN
               NUMMA = IMA
            ELSE
               NUMMA = ZI(JMAIL+IMA-1)
            ENDIF
C
            CALL CESEXI ( 'C', ICESD, ICESL, NUMMA, 1, 1, IALPHA, IAD1 )
            IF ( IAD1 .GT. 0 ) THEN
               ZR(ICESV-1+IAD1) = ANG(1)
            ELSEIF ( IAD1 .LT. 0 ) THEN
               IAD1 = -IAD1
               ZL(ICESL-1+IAD1) = .TRUE.
               ZR(ICESV-1+IAD1) = ANG(1)
            ENDIF
C
            CALL CESEXI ( 'C', ICESD, ICESL, NUMMA, 1, 1, IBETA, IAD2 )
            IF ( IAD2 .GT. 0 ) THEN
               ZR(ICESV-1+IAD2) = -ANG(2)
            ELSEIF  ( IAD2 .LT. 0 ) THEN
               IAD2 = -IAD2
               ZL(ICESL-1+IAD2) = .TRUE.
               ZR(ICESV-1+IAD2) = -ANG(2)
            ENDIF
C
 30      CONTINUE
C
         IF (.NOT. LTOUT)  CALL JEDETR ( MESMAI )
C
 10   CONTINUE
C
      CALL DISMOI('F','NOM_LIGREL',MODELE,'MODELE',IBID,LIGRMO,IE)
      CALL CESCEL(CHELMS,LIGRMO,'REPE_TENS','PANGREP','NON',NNCP,'V',
     &            CHELEM,'F',IBID)
C
      CALL DETRSD('CHAM_ELEM_S',CHELMS )
C
 9999 CONTINUE
      END
