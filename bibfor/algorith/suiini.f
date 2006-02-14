      SUBROUTINE SUIINI(SUIVCO)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 14/02/2006   AUTEUR MABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
C
      IMPLICIT     NONE
      CHARACTER*24 SUIVCO
C
C ----------------------------------------------------------------------
C ROUTINE APPELEE PAR : NMINIT
C ----------------------------------------------------------------------
C
C INITIALISATION DES SUIVIS
C
C IN  SUIVCO : NOM DE LA SD CONTENANT INFOS DE SUIVIS DDL
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
      INTEGER       NBOCC,IBID,NBSUIV
      CHARACTER*16  MOTCLE
      CHARACTER*8   MAILLA,MODELE
C
C ----------------------------------------------------------------------
C       
      CALL JEMARQ()
C
C --- INITIALISATION
C
      MOTCLE = 'SUIVI_DDL        '      
C
C --- NOM DU MAILLAGE
C
      CALL GETVID(' ','MODELE',0,1,1,MODELE,IBID)
      CALL DISMOI('F','NOM_MAILLA',MODELE,'MODELE',IBID,MAILLA,IBID)
C
C --- LECTURE INFOS DANS 'SUIVI'
C     
      CALL SUILEC(SUIVCO,MAILLA,MOTCLE,
     &            NBOCC,NBSUIV)
C 
C --- CREATION DE LA STRUCTURE DE DONNEES 
C 
      CALL SUICSD(SUIVCO,MAILLA,MOTCLE,NBOCC,NBSUIV)      
C
      CALL JEDEMA()
      END
