      SUBROUTINE PONDIM(UNIT,NOMPOI)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 09/01/2007   AUTEUR ABBAS M.ABBAS 
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
      IMPLICIT NONE
      CHARACTER*24  NOMPOI     
      INTEGER       UNIT
C      
C ----------------------------------------------------------------------
C
C ROUTINE ARLEQUIN
C
C IMPRESSION DU VECTEUR PONDERATION 
C
C ----------------------------------------------------------------------
C
C
C NB: N'AFFICHE QUE LES VALEURS DIFFERENTES DE 1
C
C IN  UNIT   : UNITE D'IMPRESSION 
C IN  NOMPOI : NOM DU VECTEUR DE PONDERATION DES MAILLES
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
C
      CHARACTER*32       JEXNUM
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
      INTEGER      IMA,NMA
      CHARACTER*8  K8BID,NOMO,NOMA,NOMMAI
      INTEGER      IOCC,JNOMA,JPOIM
      REAL*8       VALPOI
C      
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- RECUPERATION MAILLAGE 
C
      CALL GETVID(' ','MODELE',0,1,1,NOMO,IOCC)
      IF (IOCC.EQ.0) THEN
        WRITE(UNIT,*) '<PONDERAT> AFFICHAGE IMPOSSIBLE (VOIR PONDIM) '
        GOTO 999  
      ELSE
        WRITE(UNIT,*) '<PONDERAT> VECTEUR DE '//
     &                  'PONDERATION DES MAILLES...'        
      ENDIF
      CALL JEVEUO(NOMO(1:8)//'.MODELE    .NOMA','L',JNOMA)
      NOMA = ZK8(JNOMA)       
C     
      CALL JEVEUO(NOMPOI(1:21),'L',JPOIM)  
      CALL JELIRA(NOMPOI(1:21),'LONMAX',NMA,K8BID) 
C
      DO 10 IMA = 1,NMA
        VALPOI = ZR(JPOIM+IMA-1)
        CALL JENUNO(JEXNUM(NOMA(1:8)//'.NOMMAI',IMA),NOMMAI)
        IF (VALPOI.NE.1.D0) THEN
          WRITE(UNIT,*) '<PONDERAT> ...MAILLE : '//NOMMAI//
     &                  ' - VALEUR : ',VALPOI  
        ENDIF    
  10  CONTINUE    
C
  999 CONTINUE      
C
      CALL JEDEMA()
      END
