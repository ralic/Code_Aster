      SUBROUTINE CRCMEL(NBMO1,MOCLEF,COMPOR,NOMA,NCMPMA,NOMCMP,IRET)
C RESPONSABLE PROIX J-M.PROIX
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 30/06/2008   AUTEUR PROIX J-M.PROIX 
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
C     CREATION D'UNE CARTE DE COMPOREMENT ELAS SUR TOUT LE MAILLAGE
C
C IN   NBMO1   : NOMBRE de mots cles facteurs (COMP_ELAS / COMP_INCR)
C IN   MOCLEF  : mots cles facteurs
C OUT  IRET    : 0 si on n'a rien fait, 1 si si on a créé la carte
C ----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IRET,NBMO1,I,NT,N1,NCMPMA,IBID
      CHARACTER*16 VALCMP(NCMPMA),MOCLEF(2)
      CHARACTER*8  NOMGRD,NOMCMP(NCMPMA),NOMA
      CHARACTER*19 COMPOR
      REAL*8 RBID  
      COMPLEX*16   CBID
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
      INTEGER        ZI
      COMMON /IVARJE/ZI(1)
      REAL*8         ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16     ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL        ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8    ZK8
      CHARACTER*16          ZK16
      CHARACTER*24                  ZK24
      CHARACTER*32                          ZK32
      CHARACTER*80                                  ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32     JEXNUM, JEXNOM
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C ----------------------------------------------------------------------
      DATA NOMGRD/'COMPOR  '/
      
      CALL JEMARQ()
C    -----------------------------------------------------------
C     UTILISE EN PARTICULIER POUR LA COMMANDE CALC_G
C     SI AUCUN DES DEUX COMPORTEMENTS COMP_ELAS ET COMP_INCR N'EST
C     SPECIFIE PAR L'UTILISATEUR, ON CREE UNE CARTE PAR DEFAUT
C     AVEC LES CARACTERISTIQUES SUIVANTES :
C          COMP_ELAS ( RELATION    : ELAS
C                      DEFORMATION : PETIT
C                      TOUT        : OUI)
      IRET=0
      NT=0
      DO 10 I=1,NBMO1
         CALL GETFAC(MOCLEF(I),N1)
         NT=MAX(NT,N1)
   10 CONTINUE      
      IF (NT.EQ.0) THEN
        VALCMP(1) = 'ELAS'
        VALCMP(2) = '1'
        VALCMP(3) = 'PETIT'
        VALCMP(4) = 'COMP_ELAS'
        VALCMP(5) = '??'
        VALCMP(6) = '0'
        VALCMP(7) = '0'
        CALL MECACT('V',COMPOR,'MAILLA',NOMA,NOMGRD,NCMPMA,NOMCMP,
     &              IBID,RBID,CBID,VALCMP)
        IRET=1
      END IF

      CALL JEDEMA()
      END
