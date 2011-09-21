      SUBROUTINE NMMOAM(SDAMMZ,NBMODA)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 21/09/2011   AUTEUR COURTOIS M.COURTOIS 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C
      IMPLICIT NONE
      CHARACTER*(*) SDAMMZ
      INTEGER       NBMODA
C
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (ALGORITHME)
C
C CREATION SD AMORTISSEMENT MODAL
C
C ----------------------------------------------------------------------
C
C
C IN  SDAMMO : SD DEDIEE A L'AMORTISSEMENT MODAL
C               OUT - VALMOD - VALEURS MODALES
C                       1/ MASSES GENERALISEES
C                       2/ PULSATIONS PROPRES
C                       3/ AMORTISSEMENT MODAL
C               OUT - BASMOD - BASE MODALE
C OUT NBMODA : NOMBRE DE MODES PRIS POUR l'AMORTISSEMENT
C
C -------------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ----------------
C      
      
C
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ---------------------------
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
C --- FIN DECLARATIONS NORMALISEES JEVEUX -----------------------------
C
      CHARACTER*8  K8BID,MODMEC,LISTAM
      CHARACTER*14 NUMDDL
      CHARACTER*24 DEEQ
      CHARACTER*24 MATRIC,NOMCHA
      CHARACTER*24 SDAMMO
      REAL*8       PI,R8PI,R8BID
      INTEGER      IRET,IAM,IMODE,VALI(3),IADRIF
      INTEGER      NA,NB,N,NM
      INTEGER      NBMD,NEQ,NBMAX,NBAMOR
      INTEGER      IDDEEQ,LMAT,IAMOR,LTVEC
      INTEGER      JVALMO,JBASMO,JAMOR,JVAL,JAMO2,JMASG,JFREQ
      INTEGER      EXIAM,GETEXM
      INTEGER      IARG
C
C ---------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- INITIALISATIONS
C
      EXIAM  = 0
      PI     = R8PI()
      NBMODA = 0
      SDAMMO = SDAMMZ
C
C --- MATRICE DES MODES MECA
C      
      CALL GETVID('AMOR_MODAL','MODE_MECA',1,IARG,1,MODMEC,NBMD)      
      IF (NBMD.EQ.0) THEN
        CALL ASSERT(.FALSE.)
      ENDIF       
C
C --- INFORMATIONS SUR MATRICE DES MODES MECANIQUES
C
      CALL MGINFO(MODMEC,NUMDDL,NBMODA,NEQ   )      
      DEEQ = NUMDDL//'.NUME.DEEQ'
      CALL JEVEUO(DEEQ,'L',IDDEEQ)   
C
C --- ALLOCATION DESCRIPTEUR DE LA MATRICE
C
      CALL JEVEUO(MODMEC//'           .REFD','L',IADRIF)
      MATRIC =  ZK24(IADRIF)(1:8)
      CALL MTDSCR(MATRIC(1:8))
      CALL JEVEUO(MATRIC(1:19)//'.&INT','E',LMAT)
C
C --- NOMBRE DE MODES
C  
      CALL GETVIS('AMOR_MODAL','NB_MODE',1,IARG,1,NBMAX,NM)
      IF (NBMAX.NE.NBMODA) THEN
        VALI(1) = NBMODA
        VALI(2) = NBMAX
        VALI(3) = MIN(NBMODA,NBMAX)
        CALL U2MESG('I','MECANONLINE5_30',0,K8BID,3,VALI,0,R8BID)
        NBMODA = MIN(NBMODA,NBMAX)
      ENDIF     
C
C --- RECUPERATION DES AMORTISSEMENTS 
C
      CALL WKVECT('&&NMMOAM.AMORTISSEMENT','V V R',NBMODA,JAMOR)

      NA = 0
      NB = 0
      CALL GETVR8('AMOR_MODAL','AMOR_REDUIT',1,IARG,0,R8BID,NA)
      EXIAM  = GETEXM('AMOR_MODAL','LIST_AMOR')
      IF (EXIAM.EQ.1) THEN
        CALL GETVID('AMOR_MODAL','LIST_AMOR',1,IARG,0,K8BID,NB)
      ENDIF
C  
      IF (NA.NE.0 .OR. NB.NE.0) THEN
        IF (NA.NE.0) THEN
          NBAMOR = -NA
          CALL GETVR8('AMOR_MODAL','AMOR_REDUIT',1,IARG,NBAMOR,
     &                ZR(JAMOR),NA)
        ELSE
          CALL GETVID('AMOR_MODAL','LIST_AMOR',1,IARG,1,LISTAM,N)
          CALL JELIRA(LISTAM//'           .VALE','LONMAX',NBAMOR,K8BID)
          CALL JEVEUO(LISTAM//'           .VALE','L',IAMOR)
          DO 30 IAM = 1,NBMODA
            ZR(JAMOR+IAM-1) = ZR(IAMOR+IAM-1)
   30     CONTINUE
        ENDIF
C
        IF (NBAMOR.GT.NBMODA) THEN
          CALL U2MESS('A','MECANONLINE5_19')
        ENDIF
        IF (NBAMOR.LT.NBMODA) THEN
          CALL WKVECT('&&NMMOAM.AMORTISSEMEN2','V V R',NBMODA,JAMO2)
          DO 40 IAM = 1,NBAMOR
            ZR(JAMO2+IAM-1) = ZR(JAMOR+IAM-1)
 40       CONTINUE
          DO 42 IAM = NBAMOR+1,NBMODA
            ZR(JAMO2+IAM-1) = ZR(JAMOR+NBAMOR-1)
 42       CONTINUE
          NBAMOR = NBMODA
          JAMOR  = JAMO2
        ENDIF
      ENDIF
C
C --- CREATION VALEURS MODALES
C ---  1/ MASSES GENERALISEES
C ---  2/ PULSATIONS PROPRES
C ---  3/ AMORTISSEMENT MODAL
C    
      CALL WKVECT(SDAMMO(1:19)//'.VALM','V V R',3*NBMODA,JVALMO)
      DO 10 IMODE = 1,NBMODA
        CALL RSADPA(MODMEC,'L',1,'MASS_GENE',IMODE,0,JMASG,K8BID)
        ZR(JVALMO+3*(IMODE-1)+1-1) = ZR(JMASG)
        CALL RSADPA(MODMEC,'L',1,'FREQ'     ,IMODE,0,JFREQ,K8BID)
        ZR(JVALMO+3*(IMODE-1)+2-1) = ZR(JFREQ)*2.D0*PI
        ZR(JVALMO+3*(IMODE-1)+3-1) = ZR(JAMOR+IMODE-1)
  10  CONTINUE
C
C --- CREATION BASE MODALE
C   
      CALL WKVECT(SDAMMO(1:19)//'.BASM','V V R',NBMODA*NEQ,JBASMO)
      CALL WKVECT('&&NMMOAM.VECT1','V V R',NEQ,LTVEC) 
      DO 11 IMODE = 1,NBMODA        
        CALL RSEXCH(MODMEC,'DEPL',IMODE,NOMCHA,IRET)
        CALL JEVEUO(NOMCHA(1:19)//'.VALE','L',JVAL)
        CALL DCOPY(NEQ,ZR(JVAL),1,ZR(LTVEC),1)
        CALL ZERLAG(ZR(LTVEC),NEQ,ZI(IDDEEQ))
        CALL MRMULT('ZERO',LMAT,ZR(LTVEC),'R',
     &              ZR(JBASMO+(IMODE-1)*NEQ),1)
        CALL ZERLAG(ZR(JBASMO+(IMODE-1)*NEQ),NEQ,ZI(IDDEEQ))
 11   CONTINUE
C
C --- MENAGE
C
      CALL JEDETC('V','&&NMMOAM',1)
C
      CALL JEDEMA()
      END
