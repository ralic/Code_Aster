      SUBROUTINE XCORDO(IFISS,NFIS,DEFICO)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 09/02/2007   AUTEUR MARKOVIC D.MARKOVIC 
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

      IMPLICIT NONE
      CHARACTER*24  DEFICO
      INTEGER       IFISS,NFIS

C--------------------------------------------------------------------   
C  BUT: VERIFIER LA COHERENCE ENTRE LES DONNEES DU CONTACT DANS 
C       DIFFERENTES FISSURES X-FEM ET LES SYNCHRONISER
C
C--------------------------------------------------------------------
C
C     ARGUMENTS/
C  NFIS    IN     I  : NOMBRE DE FISSURES POINTÉES PAR IFISS
C  IFISS   IN/OUT I  : ADRESSE DU TABLEAU DES FISSURES (EN SORTIE ON
C                     PEUT MODIFIER LES DONNEES CONTACT)
C  DEFICO  OUT   K24 : SD CONTACT
C
C--------------------------------------------------------------------
C---- COMMUNS NORMALISES  JEVEUX
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
C-----FIN COMMUN NORMALISES  JEVEUX------------------------------------

      INTEGER II,CITMAX,FITMAX,GITMAX,JECPD,VALI(2),ICONTX
      LOGICAL LCONTX
      CHARACTER*24  ECPDON
      CHARACTER*16   VALK(2)
      REAL*8   RBID
C     ------------------------------------------------------------------

      CALL JEVEUO(ZK8(IFISS)//'.CONTACT.XFEM','L',ICONTX)
      LCONTX = ZI(ICONTX-1 +2) .NE. 1

C     VERIFIER SI ON NE MELANGE PAS LES FISSURES AVEC ET SANS CONTACT
      DO 100 II = 2,NFIS
        CALL JEVEUO(ZK8(IFISS-1 +II)//'.CONTACT.XFEM','L',ICONTX)
        LCONTX = (ZI(ICONTX-1 +2) .NE. 1) .EQV. LCONTX 

        IF(.NOT. LCONTX) CALL U2MESS('F','XFEM_4') 
              
        LCONTX = ZI(ICONTX-1 +2) .NE. 1      
 100  CONTINUE   
  
C     RECHERCHE DES VALEURS MAXIMALES DE ITER_CONT_MAXI,ITER_FROT_MAXI
C     ET ITER_GEOM_MAXI
      IF (LCONTX) THEN
        CITMAX = 0
        FITMAX = 0
        GITMAX = 0
        DO 200 II = 1,NFIS
          ECPDON = ZK8(IFISS-1 +II)//'.CONTACT.ECPDON'
          CALL JEVEUO(ECPDON,'L',JECPD)
          IF (ZI(JECPD-1 +3) .GT. CITMAX)  CITMAX = ZI(JECPD-1 +3)
          IF (ZI(JECPD-1 +4) .GT. FITMAX)  FITMAX = ZI(JECPD-1 +4)
          IF (ZI(JECPD-1 +5) .GT. GITMAX)  GITMAX = ZI(JECPD-1 +5)
 200    CONTINUE  
      
C       SYNCHRONISATION DES VALEURS ITER_...
        DO 300 II = 1,NFIS
          ECPDON = ZK8(IFISS-1 +II)//'.CONTACT.ECPDON'
          CALL JEVEUO(ECPDON,'E',JECPD)
          
          IF(ZI(JECPD-1 +3) .NE. CITMAX)  THEN
            VALK(1) = 'ITER_CONT_MAXI'
            VALK(2) = ZK8(IFISS-1 +II)
            VALI(1) = ZI(JECPD-1 +3)
            VALI(2) = CITMAX
            CALL U2MESG ('A', 'XFEM_5', 1, VALK, 2, VALI, 0, RBID)
          ENDIF  
          
          IF(ZI(JECPD-1 +4) .NE. FITMAX)  THEN
            VALK(1) = 'ITER_FROT_MAXI'
            VALK(2) = ZK8(IFISS-1 +II)
            VALI(1) = ZI(JECPD-1 +4)
            VALI(2) = FITMAX
            CALL U2MESG ('A', 'XFEM_5', 1, VALK, 2, VALI, 0, RBID)
          ENDIF  
          
          IF(ZI(JECPD-1 +5) .NE. GITMAX)  THEN
            VALK(1) = 'ITER_GEOM_MAXI'
            VALK(2) = ZK8(IFISS-1 +II)
            VALI(1) = ZI(JECPD-1 +5)
            VALI(2) = GITMAX
            CALL U2MESG ('A', 'XFEM_5', 1, VALK, 2, VALI, 0, RBID)
          ENDIF  
          
          ZI(JECPD-1 +3) = CITMAX
          ZI(JECPD-1 +4) = FITMAX
          ZI(JECPD-1 +5) = GITMAX
 300    CONTINUE  
C       VAL OUT
        ICONTX = 1
      ENDIF 

C     DESORMAIS TOUS LES PARAMETRES GLOBAUX DES CONTACTS SONT COHERENTS
C     ON SORT LE PREMIER
      DEFICO(1:16)=ZK8(IFISS)//'.CONTACT'

      END
