      SUBROUTINE ARLCRT(NOMA  ,NOMO,NOMGRP,
     &                  CARTAR)

C 
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 12/02/2008   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
      CHARACTER*8  NOMA,NOMO
      CHARACTER*19 CARTAR
      CHARACTER*10 NOMGRP
C          
C ----------------------------------------------------------------------
C
C ROUTINE ARLEQUIN
C
C CREATION DE LA CARTE POUR CALCUL TERME COUPLAGE ARLEQUIN
C
C ----------------------------------------------------------------------
C
C
C IN  NOMA   : NOM DU MAILLAGE
C IN  NOMO   : NOM DU MODELE
C IN  NOMGRP : NOM DU GROUPE DE MAILLES POUR FABRIQUER LE LIGREL
C OUT CARTAR : CARTE POUR ELEMENTS ARLEQUIN
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
      INTEGER      NCMP
      PARAMETER    (NCMP=2)
C      
      INTEGER      IFM,NIV
      INTEGER      JNCMP,JVALV
      INTEGER      ICMP,NBMA
      CHARACTER*8  K8BID
      INTEGER      JGRMA    
      CHARACTER*19 NGRMA       
      CHARACTER*2  CMPCOD
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFDBG('ARLEQUIN',IFM,NIV)
C
C --- AFFICHAGE
C      
      IF (NIV.GE.2) THEN
        WRITE (IFM,*) '<ARLEQUIN> CREATION DE LA CARTE POUR LES '//
     &        ' ELEMENTS ARLEQUIN' 
      ENDIF            
C
C --- LECTURE DONNEES GROUPE DE MAILLES
C         
      NGRMA = NOMGRP(1:10)//'.GROUPEMA'      
      CALL JEVEUO(NGRMA(1:19),'L',JGRMA) 
      CALL JELIRA(NGRMA(1:19),'LONMAX',NBMA  ,K8BID)
C
C --- DESTRUCTION DE LA CARTE SI ELLE EXISTE
C 
      CALL DETRSD('CARTE',CARTAR)
C
C --- CREATION DE LA CARTE 
C 
      CALL ALCART('V',CARTAR,NOMA,'NEUT_K8')
C
C --- ACCES A LACARTE
C      
      CALL JEVEUO(CARTAR//'.NCMP','E',JNCMP)
      CALL JEVEUO(CARTAR//'.VALV','E',JVALV)   
C
C --- NOMS DES COMPOSANTES DE LA CARTE
C      
      DO 100 ICMP = 1,NCMP
        CALL CODENT(ICMP,'G',CMPCOD)
        ZK8(JNCMP-1+ICMP) = 'X'//CMPCOD
  100 CONTINUE
C
C --- VALEURS DES COMPOSANTES DE LA CARTE
C     
      DO 110 ICMP = 1,NCMP     
        ZK8(JVALV-1+1)  = ' '
        ZK8(JVALV-1+2)  = ' '
        CALL NOCART(CARTAR,3,' ','NUM',NBMA,' ',ZI(JGRMA),
     &              ' ',NCMP) 
  110 CONTINUE       
C
C --- MENAGE 
C
      CALL JEDETR(CARTAR//'.NCMP')
      CALL JEDETR(CARTAR//'.VALV')  
C
      CALL JEDEMA()
      END
