      SUBROUTINE VSDMCD(NOM)
      IMPLICIT NONE
      CHARACTER*(*) NOM
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 10/10/2006   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE VABHHTS J.PELLET

C ----------------------------------------------------------------------
C BUT : VERISD/MACR_ELEM_DYNA
C ----------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
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
C     ----- FIN COMMUNS NORMALISES  JEVEUX  --------------------------
C -DEB------------------------------------------------------------------

      INTEGER NBDEF,JMASS,IOC,LLREFB,NBVAL,IRET,ICHK,N1CHK,N2CHK,N3CHK
      INTEGER VERIOB,IEX1
      CHARACTER*1 CMES
      CHARACTER*8 K8BID,BASMOD
      CHARACTER*19 AMORB

      CMES = 'F'

      CALL JEMARQ()

      IEX1 = VERIOB('O',NOM(1:8)//'.MAEL_MASS_DESC','EXIS_V',0,' ')
      IEX1 = VERIOB('O',NOM(1:8)//'.MAEL_MASS_DESC','LONMAX',3,' ')
      IEX1 = VERIOB('O',NOM(1:8)//'.MAEL_MASS_DESC','TYPE_I',0,' ')
      CALL ASSERT(IEX1.EQ.1)

C ----VERIFICATION COMPOSANTES
      CALL JEVEUO(NOM(1:8)//'.MAEL_MASS_DESC','L',ICHK)
      N1CHK = ZI(ICHK)
      N2CHK = ZI(ICHK+1)
      N3CHK = ZI(ICHK+2)

      IF (N1CHK.NE.2) THEN
        CALL U2MESS(CMES,'SDVERI_1')
      ENDIF

      IF (N2CHK.LE.0) THEN
        CALL U2MESS(CMES,'SDVERI_2')
      ENDIF

      IF ((N3CHK.NE.2) .AND. (N3CHK.NE.3)) THEN
        CALL U2MESS(CMES,'SDVERI_3')
      ENDIF

C ---------------------------------------------------

      CALL JEVEUO(NOM(1:8)//'.MAEL_MASS_DESC','L',JMASS)
      NBDEF = ZI(JMASS+1)

C ----------------------------------------------------
      IEX1 = VERIOB('O',NOM(1:8)//'.MAEL_DESC','EXIS_V',0,' ')
      IEX1 = VERIOB('O',NOM(1:8)//'.MAEL_DESC','LONMAX',3,' ')
      IEX1 = VERIOB('O',NOM(1:8)//'.MAEL_DESC','TYPE_I',0,' ')

C ----VERIFICATION COMPOSANTES
      CALL JEVEUO(NOM(1:8)//'.MAEL_DESC','L',ICHK)
      N1CHK = ZI(ICHK)
      N2CHK = ZI(ICHK+1)
      N3CHK = ZI(ICHK+2)

      IF (N1CHK.LT.0) THEN
        CALL U2MESS(CMES,'SDVERI_4')
      ENDIF

      IF (N2CHK.LT.0) THEN
        CALL U2MESS(CMES,'SDVERI_5')
      ENDIF

      IF (N3CHK.LT.0) THEN
        CALL U2MESS(CMES,'SDVERI_6')
      ENDIF
C ----------------------------------------------------
      IEX1 = VERIOB('O',NOM(1:8)//'.MAEL_REFE','EXIS_V',0,' ')
      IEX1 = VERIOB('O',NOM(1:8)//'.MAEL_REFE','LONMAX',2,' ')
      IEX1 = VERIOB('O',NOM(1:8)//'.MAEL_REFE','TYPE_K24',0,' ')

C ---------------------------------------------------
      IEX1 = VERIOB('O',NOM(1:8)//'.MAEL_INER_REFE','EXIS_V',0,' ')
      IEX1 = VERIOB('O',NOM(1:8)//'.MAEL_INER_REFE','LONMAX',2,' ')
      IEX1 = VERIOB('O',NOM(1:8)//'.MAEL_INER_REFE','TYPE_K24',0,' ')

C ---------------------------------------------------
      IEX1 = VERIOB('O',NOM(1:8)//'.MAEL_INER_VALE','EXIS_V',0,' ')
      IEX1 = VERIOB('O',NOM(1:8)//'.MAEL_INER_VALE','LONMAX',3*NBDEF,
     &       ' ')
      IEX1 = VERIOB('O',NOM(1:8)//'.MAEL_INER_VALE','TYPE_R',0,' ')

C ----------------------------------------------------
      IEX1 = VERIOB('O',NOM(1:8)//'.MAEL_MASS_REFE','EXIS_V',0,' ')
      IEX1 = VERIOB('O',NOM(1:8)//'.MAEL_MASS_REFE','LONMAX',2,' ')
      IEX1 = VERIOB('O',NOM(1:8)//'.MAEL_MASS_REFE','TYPE_K24',0,' ')

C ----------------------------------------------------
      IEX1 = VERIOB('O',NOM(1:8)//'.MAEL_MASS_VALE','EXIS_V',0,' ')
      IEX1 = VERIOB('O',NOM(1:8)//'.MAEL_MASS_VALE','LONMAX',
     &       NBDEF* (NBDEF+1)/2,' ')
      IEX1 = VERIOB('O',NOM(1:8)//'.MAEL_MASS_VALE','TYPE_R',0,' ')

C ----------------------------------------------------
      IEX1 = VERIOB('O',NOM(1:8)//'.MAEL_RAID_DESC','EXIS_V',0,' ')
      IEX1 = VERIOB('O',NOM(1:8)//'.MAEL_RAID_DESC','LONMAX',3,' ')
      IEX1 = VERIOB('O',NOM(1:8)//'.MAEL_RAID_DESC','TYPE_I',0,' ')

C ----VERIFICATION COMPOSANTES
      CALL JEVEUO(NOM(1:8)//'.MAEL_RAID_DESC','L',ICHK)
      N1CHK = ZI(ICHK)
      N2CHK = ZI(ICHK+1)
      N3CHK = ZI(ICHK+2)

      IF (N1CHK.NE.2) THEN
        CALL U2MESS(CMES,'SDVERI_7')
      ENDIF

      IF (N2CHK.LE.0) THEN
        CALL U2MESS(CMES,'SDVERI_8')
      ENDIF

      IF ((N3CHK.NE.2) .AND. (N3CHK.NE.3)) THEN
        CALL U2MESS(CMES,'SDVERI_9')
      ENDIF

C ----------------------------------------------------
      IEX1 = VERIOB('O',NOM(1:8)//'.MAEL_RAID_REFE','EXIS_V',0,' ')
      IEX1 = VERIOB('O',NOM(1:8)//'.MAEL_RAID_REFE','LONMAX',2,' ')
      IEX1 = VERIOB('O',NOM(1:8)//'.MAEL_RAID_REFE','TYPE_K24',0,' ')

C ----------------------------------------------------
      IEX1 = VERIOB('O',NOM(1:8)//'.MAEL_RAID_VALE','EXIS_V',0,' ')
      IEX1 = VERIOB('O',NOM(1:8)//'.MAEL_RAID_VALE','LONMAX',
     &       NBDEF* (NBDEF+1)/2,' ')
      IEX1 = VERIOB('O',NOM(1:8)//'.MAEL_RAID_VALE','TYPE_R',0,' ')



C ----------------------------------------------------
C-----CONDITION D'EXISTENCE DE LA MATRICE D'AMORTISSEMENT
      CALL JEEXIN(NOM(1:8)//'.MAEL_AMOR_REFE',IRET)

C----- ATTENTION ON NE VERIFIE PAS LA COHERENCE ENTRE L EXISTENCE
C      DE LA MATRICE D AMORTISSEMENT ET LES DONNEES UTILISATEURS
C      ON VERIFIE SEULEMENT QUE LA MATRICE CREEE EST LICITE
      IF (IRET.NE.0) THEN

C ----------------------------------------------------
        IEX1 = VERIOB('O',NOM(1:8)//'.MAEL_AMOR_DESC','EXIS_V',0,' ')

C ----VERIFICATION DE LA LONGUEUR DE L'OBJET MAEL_AMOR_DESC
C       IEX1=VERIOB('O',NOM(1:8)//'.MAEL_AMOR_DESC','LONMAX',3,' ')

C ----VERIFICATION DE LA NATURE DE L'OBJET MAEL_AMOR_DESC
C       IEX1=VERIOB('O',NOM(1:8)//'.MAEL_AMOR_DESC','TYPE_I',0,' ')

C ----------------------------------------------------
        IEX1 = VERIOB('O',NOM(1:8)//'.MAEL_AMOR_REFE','EXIS_V',0,' ')
        IEX1 = VERIOB('O',NOM(1:8)//'.MAEL_AMOR_REFE','LONMAX',2,' ')
        IEX1 = VERIOB('O',NOM(1:8)//'.MAEL_AMOR_REFE','TYPE_K24',0,' ')

C ----------------------------------------------------
        IEX1 = VERIOB('O',NOM(1:8)//'.MAEL_AMOR_VALE','EXIS_V',0,' ')
        IEX1 = VERIOB('O',NOM(1:8)//'.MAEL_AMOR_VALE','LONMAX',
     &         NBDEF* (NBDEF+1)/2,' ')
        IEX1 = VERIOB('O',NOM(1:8)//'.MAEL_AMOR_VALE','TYPE_R',0,' ')

      ENDIF

      CALL JEDEMA()
      END
