      SUBROUTINE NMEVIN(SDDISC,RESOCO,IECHEC,IEVDAC)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 19/09/2011   AUTEUR ABBAS M.ABBAS 
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT     NONE
      CHARACTER*24 RESOCO
      INTEGER      IECHEC,IEVDAC
      CHARACTER*19 SDDISC
C
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (ALGORITHME - EVENEMENTS)
C
C GESTION DE L'EVENEMENT INTERPENETRATION
C
C ----------------------------------------------------------------------
C
C
C IN  SDDISC : SD DISCRETISATION TEMPORELLE
C IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
C IN  IECHEC : OCCURRENCE DE L'ECHEC
C OUT IEVDAC : VAUT IECHEC SI EVENEMENT DECLENCHE
C                   0 SINON
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
      INTEGER      IFM,NIV
      INTEGER      CFDISD,NBLIAI
      INTEGER      ILIAI
      CHARACTER*24 JEUITE
      INTEGER      JJEUIT
      REAL*8       JEUFIN,PNMAXI
      LOGICAL      LEVENT
      REAL*8       PENMAX
      INTEGER      IBID
      CHARACTER*8  K8BID
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFDBG('MECA_NON_LINE',IFM,NIV)
C
C --- AFFICHAGE
C
      IF (NIV.GE.2) THEN
        WRITE (IFM,*) '<MECANONLINE> ... INTERPENETRATION'
      ENDIF
C
C --- INITIALISATIONS
C
      CALL UTDIDT('L'   ,SDDISC,'ECHE',IECHEC,'PENE_MAXI',
     &            PENMAX,IBID  ,K8BID )
      IEVDAC = 0
      LEVENT = .FALSE.
      PNMAXI = 0.D0
C
C --- PARAMETRES
C
      NBLIAI = CFDISD(RESOCO,'NBLIAI')
C
C --- ACCES OBJETS DU CONTACT
C
      JEUITE = RESOCO(1:14)//'.JEUITE'
      CALL JEVEUO(JEUITE,'L',JJEUIT)
C
C --- DETECTION PENETRATION
C
      DO 10 ILIAI = 1,NBLIAI
        JEUFIN = ZR(JJEUIT+3*(ILIAI-1)+1-1)
        IF (JEUFIN.LE.0.D0) THEN
          IF (ABS(JEUFIN).GT.PENMAX) THEN
            IF (ABS(JEUFIN).GT.PNMAXI) PNMAXI = ABS(JEUFIN)
            LEVENT = .TRUE.
          ENDIF
        ENDIF
  10  CONTINUE
C
C --- ACTIVATION EVENEMENT
C
      IF (LEVENT) THEN
        IEVDAC = IECHEC
      ENDIF
C
      CALL JEDEMA()
      END
