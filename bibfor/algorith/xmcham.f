      SUBROUTINE XMCHAM(MODELE,RESOCO,TYPCHA,NOMCHA)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 14/09/2010   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT      NONE
      CHARACTER*8   MODELE
      CHARACTER*(*) TYPCHA
      CHARACTER*24  RESOCO
      CHARACTER*19  NOMCHA
C 
C ----------------------------------------------------------------------
C
C ROUTINE XFEM (METHODE XFEM - UTILITAIRE)
C
C RETOURNE LE NOM D'UN CHAM
C      
C ----------------------------------------------------------------------
C
C
C IN  MODELE : NOM DU MODELE
C IN  RESOCO : SD POUR LA RESOLUTION DE CONTACT
C IN  TYPCHA : TYPE DU CHAMP
C                'XINDCO' - INDICATEUR DE CONTACT
C                'XDONCO' - DONNNES POUR LE CONTACT
C                'XSEUCO' - SEUIL DE TRESCA POUR LE FROTTEMENT
C                'XCOHES' - DONNEES POUR LE MODELE COHESIF
C                'LNNO'   - LEVEL-SET NORMALE
C                'LTNO'   - LEVEL-SET TANGENTE
C                'PINTER' - COORDONNEES DES POINTS D'INTERSECTION
C                'AINTER' - INFOS ARETES DES POINTS D'INTERSECTION
C                'CFACE'  - MATRICE DE CONECTIVITE DES FACETTES
C                           DE CONTACT
C                'FACLON' - INFOS SUR LES FACETTES DE CONTACT
C                'BASECO' - BASE DU CONTACT
C                'STANO'  - STATUT DES NOEUDS               
C OUT NOMCHA : NOM DU CHAMP
C
C -------------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ----------------
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
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------
C
      CALL JEMARQ()
C
      NOMCHA = ' '      
C
      IF (TYPCHA.EQ.'XINDCO') THEN
        NOMCHA = RESOCO(1:14)//'.XFIN'
      
      ELSEIF (TYPCHA.EQ.'XDONCO') THEN
        NOMCHA = RESOCO(1:14)//'.XFDO'

      ELSEIF (TYPCHA.EQ.'XSEUCO') THEN
        NOMCHA = RESOCO(1:14)//'.XFSE'
        
      ELSEIF (TYPCHA.EQ.'XCOHES') THEN
        NOMCHA = RESOCO(1:14)//'.XCOH'   
        
      ELSEIF (TYPCHA.EQ.'LNNO') THEN
        NOMCHA = MODELE(1:8)//'.LNNO'
        
      ELSEIF (TYPCHA.EQ.'LTNO') THEN
        NOMCHA = MODELE(1:8)//'.LTNO'
        
      ELSEIF (TYPCHA.EQ.'STNO') THEN
        NOMCHA = MODELE(1:8)//'.STNO'        
        
      ELSEIF (TYPCHA.EQ.'PINTER') THEN
        NOMCHA = MODELE(1:8)//'.TOPOFAC.OE'
        
      ELSEIF (TYPCHA.EQ.'AINTER') THEN
        NOMCHA = MODELE(1:8)//'.TOPOFAC.AI'
                   
      ELSEIF (TYPCHA.EQ.'CFACE') THEN
        NOMCHA = MODELE(1:8)//'.TOPOFAC.CF'
        
      ELSEIF (TYPCHA.EQ.'BASECO') THEN
        NOMCHA = MODELE(1:8)//'.TOPOFAC.BA'
        
      ELSEIF (TYPCHA.EQ.'FACLON') THEN
        NOMCHA = MODELE(1:8)//'.TOPOFAC.LO'
        
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF
C
      CALL JEDEMA()
C   
      END
