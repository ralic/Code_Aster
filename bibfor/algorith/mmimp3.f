      SUBROUTINE MMIMP3(IFM   ,NOMA  ,LIGRCF,IPC   ,JVALV ,
     &                  JTABF ,JJEU)
C      
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 08/10/2007   AUTEUR NISTOR I.NISTOR 
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT     NONE
      INTEGER      IFM
      CHARACTER*19 LIGRCF                   
      CHARACTER*8  NOMA
      INTEGER      IPC
      INTEGER      JVALV,JTABF,JJEU
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE - IMPRESSIONS)
C
C AFFICHAGE DE LA CARTE DES ELEMENTS DE CONTACT 
C      
C ----------------------------------------------------------------------
C
C
C IN  IFM    : UNITE D'IMPRESSION DU MESSAGE
C IN  NOMA   : NOM DU MAILLAGE
C IN  LIGRCF : LIGREL POUR LES ELEMENTS DE CONTACT
C IN  IPC    : NUMERO DU POINT DE CONTACT
C IN  JVALV  : POINTEUR VERS LA CARTE
C IN  JTABF  : POINTEUR VERS DEFICO(1:16)//'.CARACF'
C IN  JJEU   : POINTEUR VERS DEFICO(1:16)//'.JEUSUP'
C
C -------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------------
C
      CHARACTER*32 JEXNUM
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
C ---------------- FIN DECLARATIONS NORMALISEES JEVEUX -----------------
C
      INTEGER      CFMMVD,ZTABF  
      INTEGER      NUMMAE,NUMMAM
      CHARACTER*8  NOMESC,NOMMAI
      INTEGER      JAD      
      REAL*8       LAMBDA,COEFCA,COEFFA,COEFFF
      REAL*8       DELTAT,ASPERI,COEASP,CN,ALPHA,GAMMA,JEUSUP
      INTEGER      IFROTT,IAXIS
      INTEGER      IFORM,ICOMPL,IMAE     
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()     
C
      ZTABF = CFMMVD('ZTABF')          
C                
      LAMBDA = ZR(JVALV-1+1+11)
      COEFCA = ZR(JVALV-1+1+12)
      COEFFA = ZR(JVALV-1+1+13)
      COEFFF = ZR(JVALV-1+1+14)
      IFROTT = NINT(ZR(JVALV-1+1+15))
      IAXIS  = NINT(ZR(JVALV-1+1+17))
      DELTAT = ZR(JVALV-1+1+19)
      IFORM  = NINT(ZR(JVALV-1+1+20))
      ICOMPL = NINT(ZR(JVALV-1+1+26))  
      ASPERI = ZR(JVALV-1+1+27) 
      COEASP = ZR(JVALV-1+1+28)   
      CN     = ZR(JVALV-1+1+29)   
      ALPHA  = ZR(JVALV-1+1+30)
      GAMMA  = ZR(JVALV-1+1+31)
      JEUSUP = ZR(JVALV-1+1+32)
      IMAE   = NINT(ZR(JVALV-1+1+33))                      
C
C --- ACCES A L'ELEMENT EN COURS            
C
      CALL JEVEUO(JEXNUM(LIGRCF//'.NEMA',IMAE),'L',JAD)     
      NUMMAE  = NINT(ZR(JTABF+ZTABF*(IPC-1)+1))
C      NUMMAE  = NINT(ZR(JTABF+ZTABF*(IMAE-1)+1))
      CALL JENUNO(JEXNUM(NOMA//'.NOMMAI',NUMMAE),NOMESC) 
C      NUMMAM  = NINT(ZR(JTABF+ZTABF*(IMAE-1)+2))
      NUMMAM  = NINT(ZR(JTABF+ZTABF*(IPC-1)+2))
      CALL JENUNO(JEXNUM(NOMA//'.NOMMAI',NUMMAM),NOMMAI)         
      WRITE(IFM,1000) IPC,NOMESC,NOMMAI
C
C --- POINT DE CONTACT EN COURS
C             
      IF (IAXIS.EQ.1) THEN
        WRITE(IFM,1011)            
      ELSE 
        WRITE(IFM,1001) 
      ENDIF  
      WRITE(IFM,1002) LAMBDA,COEFCA,JEUSUP
      IF (IFORM.EQ.2) THEN
        WRITE(IFM,1003) DELTAT  
      ELSE
        WRITE(IFM,1004) ALPHA,GAMMA                         
      ENDIF    
      IF (ICOMPL.EQ.1) THEN
        WRITE(IFM,1005) ASPERI,COEASP,CN              
      ENDIF  
      IF (IFROTT.EQ.3) THEN
        WRITE(IFM,1006) COEFFF,COEFFA              
      ENDIF            
C
C --- FORMATS AFFICHAGE
C
 1000 FORMAT (' <CONTACT>     * LA MAILLE DE CONTACT ',I5,
     &        '(',A8,'/',A8,')')    
 1001 FORMAT (' <CONTACT>        A POUR PROPRIETES') 
 1011 FORMAT (' <CONTACT>        A POUR PROPRIETES '//
     &                       '(MODELE AXISYMETRIQUE)') 
  
 1002 FORMAT (' <CONTACT>          - SEUIL_INIT     : ',E10.3,
     &        ' - COEF_REGU_CONT :  ',E10.3,
     &        ' - JEU SUPP.      :  ',E10.3) 
 1003 FORMAT (' <CONTACT>          AVEC FORMULATION EN VITESSE  ',
     &        ' - INC. DE TEMPS  :  ',E10.3,
     &        ' (THETA-SCHEMA)') 
 1004 FORMAT (' <CONTACT>          AVEC FORMULATION EN DEPLACEMENT  ',
     &        ' - NEWMARK ALPHA  :  ',E10.3,
     &        ' - NEWMARK DELTA  :  ',E10.3)  
 1005 FORMAT (' <CONTACT>          AVEC PRISE EN COMPTE DE LA '//
     &        'COMPLIANCE',
     &        ' - ASPERITE       :  ',E10.3,
     &        ' - E_N            :  ',E10.3,
     &        ' - E_V            :  ',E10.3)      
 1006 FORMAT (' <CONTACT>          AVEC FROTTEMENT DE COULOMB',
     &        ' - COEFFICIENT    :  ',E10.3,
     &        ' - COEF_REGU_FROT :  ',E10.3)      
C    
 999  CONTINUE 
      CALL JEDEMA()
C
      END
