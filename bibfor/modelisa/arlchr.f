      SUBROUTINE ARLCHR(DIME  ,NOMARL,CINE  ,NOM1  ,NOM2  ,
     &                  CHARGE)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 09/01/2007   AUTEUR ABBAS M.ABBAS 
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
      INTEGER       DIME
      CHARACTER*10  NOM1,NOM2
      CHARACTER*8   CINE(3),CHARGE,NOMARL    
C      
C ----------------------------------------------------------------------
C
C ROUTINE ARLEQUIN
C
C TRANSFORMATION DES MATRICES ARLEQUIN MORSE EN RELATIONS LINEAIRES
C ROUTINE ENTETE
C
C ----------------------------------------------------------------------
C
C
C IN  DIME   : DIMENSION DE L'ESPACE
C IN  NOMARL : NOM SD ARLEQUIN
C IN  NOM1   : NOM DE LA SD DE STOCKAGE MAILLES GROUP_MA_1 
C IN  NOM2   : NOM DE LA SD DE STOCKAGE MAILLES GROUP_MA_2 
C IN  CINE   : CINEMATIQUES DES GROUPES DE MAILLE
C OUT CHARGE : NOM UTILISATEUR DE LA CHARGE
C
C SD EN SORTIE:
C
C CHARGE.CHME.LIGRE : LIGREL DE CHARGE
C CHARGE.CHME.CIMPO : CARTE COEFFICIENTS IMPOSES
C CHARGE.CHME.CMULT : CARTE COEFFICIENTS MULTIPLICATEURS
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
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
      INTEGER      JARLEQ
C      
C ----------------------------------------------------------------------
C
      CALL JEMARQ()    
C
      CALL JEVEUO(NOMARL(1:8)//'.EXCLU','E',JARLEQ)
      CALL ARLCHA(DIME  ,NOMARL,CINE  ,NOM1  ,NOM2  ,
     &            CHARGE,ZL(JARLEQ))
C
      CALL JEDEMA()    
      END
