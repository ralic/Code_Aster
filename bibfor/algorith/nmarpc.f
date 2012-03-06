      SUBROUTINE NMARPC(RESULT,SDENER,INSTAN)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 05/03/2012   AUTEUR IDOUX L.IDOUX 
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT     NONE
      REAL*8       INSTAN
      CHARACTER*8  RESULT
      CHARACTER*19 SDENER
C
C ----------------------------------------------------------------------
C
C ROUTINE *_NON_LINE (STRUCTURES DE DONNES)
C
C ARCHIVAGE DES PARAMETRES DANS LA TABLE DES PARAMETRES CALCULES
C
C ----------------------------------------------------------------------
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
C IN  RESULT : NOM SD RESULTAT
C IN  SDENER : NOM SD ENERGIE
C IN  INSTAN : VALEUR DE L'INSTANT DE CALCUL
C
C
      INTEGER      IFM,NIV,IENER,IAUX
      INTEGER      NBPAR
      PARAMETER   (NBPAR=7)
      CHARACTER*10 NOMPAR(NBPAR)
      CHARACTER*19 TABLPC

      INTEGER      IBID
      CHARACTER*8  K8BID
      COMPLEX*16   C16BID
      REAL*8       VR(NBPAR)

      DATA         NOMPAR / 'INST'      ,'TRAV_EXT  ','ENER_CIN'  ,
     &                      'ENER_TOT'  ,'TRAV_AMOR ','TRAV_LIAI' ,
     &                      'DISS_SCH'/
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFDBG('MECA_NON_LINE',IFM,NIV)
C
C --- RECUPERATION DU NOM DE LA TABLE CORRESPONDANT
C     AUX PARAMETRE CALCULES
C
      CALL LTNOTB(RESULT,'PARA_CALC',TABLPC)
C
C --- CONSTRUCTION DES LISTES DE PARAMETRES A SAUVEGARDER PAR TYPE
C
C     TYPE 'I'
C     TYPE 'R'
      CALL JEVEUO(SDENER//'.VALE','L',IENER)
      VR(1)=INSTAN
      DO 10 IAUX=1,6 
        VR(1+IAUX)=ZR(IENER-1+IAUX)
 10   CONTINUE
C     TYPE 'C'
C     TYPE 'K' 
      CALL TBAJLI(TABLPC,NBPAR,NOMPAR,IBID,VR,C16BID,K8BID,0)

      CALL JEDEMA()

      END
