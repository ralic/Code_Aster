      SUBROUTINE FETSCA(NBI,VI,VO,SCALIN,SDFETI)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 22/11/2004   AUTEUR BOITEAU O.BOITEAU 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
C-----------------------------------------------------------------------
C    - FONCTION REALISEE:  CALCUL DE LA PHASE DE MISE A L'ECHELLE
C                         AU SENS FETI
C
C      IN    NBI: IN   : NOMBRE DE NOEUDS D'INTERFACE
C      IN     VI: VR8  : VECTEUR INPUT DE TAILLE NBI
C      OUT    VO: VR8  : VECTEUR OUTPUT DE TAILLE NBI
C      IN SCALIN: CH24 : PARAMETRE DE SCALING
C      IN SDFETI: CH19 : SD DECRIVANT LE PARTIONNEMENT FETI
C   -------------------------------------------------------------------
C     ASTER INFORMATIONS:
C       28/01/04 (OB): CREATION.
C----------------------------------------------------------------------
C RESPONSABLE BOITEAU O.BOITEAU
C CORPS DU PROGRAMME
      IMPLICIT NONE

C DECLARATION PARAMETRES D'APPELS
      INTEGER      NBI
      REAL*8       VI(NBI),VO(NBI)
      CHARACTER*19 SDFETI
      CHARACTER*24 SCALIN

C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
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
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
      
C DECLARATION VARIABLES LOCALES
      INTEGER      I,IFETI,IAUX,IFM,NIV,IMULT,NBDDL,NBDDLC,IAUXJ,J,
     &             IDIME,NBNI,IINF
      REAL*8       RMULT
      CHARACTER*24 INFOFE
      
C CORPS DU PROGRAMME
      CALL JEMARQ()

C RECUPERATION DU NIVEAU D'IMPRESSION
      CALL INFNIV(IFM,NIV)
      CALL JEVEUO('&&'//SDFETI(1:17)//'.FINF','L',IINF)
      INFOFE=ZK24(IINF)
            
C MONITORING
      IF (INFOFE(1:1).EQ.'T') THEN
        WRITE(IFM,*)
        WRITE(IFM,*)'DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD'
        IF (SCALIN(1:4).EQ.'SANS') THEN
          WRITE(IFM,*)'<FETI/FETSCA> SANS SCALING'
         ELSE
          WRITE(IFM,*)'<FETI/FETSCA> SCALING PAR MULTIPLICITE'
         ENDIF
      ENDIF

      IF (SCALIN(1:4).EQ.'SANS') THEN
C ----------------------------------------------------------------------
C ----  PAS DE SCALING
C ----------------------------------------------------------------------
        DO 10 I=1,NBI
          VO(I)=VI(I)
   10   CONTINUE
         
      ELSE IF (SCALIN(1:4).EQ.'MULT') THEN
C ----------------------------------------------------------------------
C ----  SCALING PAR MULTPLICITE
C ----------------------------------------------------------------------

C ADRESSE NBRE DE NOEUDS D'INTERFACE (NBNI)
        CALL JEVEUO(SDFETI(1:19)//'.FDIM','L',IDIME)
        NBNI=ZI(IDIME+1)
              
C ADRESSE JEVEUX DE L'OBJET DECRIVANT LE VECTEUR D'INTERFACE
        CALL JEVEUO(SDFETI//'.FETI','L',IFETI)      
        IAUX=IFETI+1

C ---------------------------------------------------
C BOUCLE SUR LES NOEUDS D'INTERFACE
C ---------------------------------------------------   
        DO 20 I=1,NBNI
        
C MULTIPLICITE DU IEME NOEUD D'INTERFACE        
          IMULT=ZI(IAUX)
          IF (IMULT.EQ.0) THEN
            CALL UTMESS('F','FETSCA',
     &       'DONNEE ERRONNEE, MULTIPLICITE NULLE !')
          ELSE
C NOMBRE DE DDLS CUMULES AVANT LE IEME NOEUD D'INTERFACE (NBDDLC)
C NOMBRE DE DDLS DU IEME NOEUD D'INTERFACE (NBDDL)
            NBDDLC=ZI(IAUX+1)     
            IF (I.EQ.1) THEN
              NBDDL=NBDDLC
            ELSE
              NBDDL=NBDDLC-ZI(IAUX-3)
            ENDIF
            NBDDLC=NBDDLC-NBDDL
            RMULT=1.D0/IMULT        
            DO 15 J=1,NBDDL
              IAUXJ=NBDDLC+J                      
              VO(IAUXJ)=VI(IAUXJ)*RMULT
   15       CONTINUE                
          ENDIF
          IAUX=IAUX+4     
   20   CONTINUE
      ELSE
        CALL UTMESS('F','FETSCA','OPTION DE CALCUL NON PREVUE !')
      ENDIF
C ---------------------------------------------------
C FIN BOUCLE SUR LES NOEUDS D'INTERFACE
C ---------------------------------------------------

C MONITORING
      IF (INFOFE(4:4).EQ.'T') THEN
        WRITE(IFM,*)'<FETI/FETSCA> INPUT I VI(I)'      
        DO 30 I=1,NBI
          WRITE(IFM,*)I,'  ',VI(I)
   30   CONTINUE
        WRITE(IFM,*)'OUTPUT I VO(I)'         
        DO 31 I=1,NBI
          WRITE(IFM,*)I,'  ',VO(I)
   31   CONTINUE          
      ENDIF
      IF (INFOFE(1:1).EQ.'T') THEN        
        WRITE(IFM,*)
        WRITE(IFM,*)'DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD'
      ENDIF          
      CALL JEDEMA()
      END
