      SUBROUTINE PMMACO ( NOMMAT, CODI )
      IMPLICIT   NONE
      CHARACTER*8        NOMMAT
      CHARACTER*19       CODI
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 23/03/2010   AUTEUR PROIX J-M.PROIX 
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
C RESPONSABLE PROIX J-M.PROIX
C-----------------------------------------------------------------------
C OPERATEUR CALC_POINT_MAT : MATERIAU CODE COMME RCMACO MAIS SANS MODELE
C-----------------------------------------------------------------------
C
C     BUT: CREER L'OBJET NOMMAT//'      .CODI' ,LE REMPLIR ET RENVOYER
C          SON ADRESSE PAR RAPPORT A ZI
C
C IN   NOMMAT : NOM DU MATERIAU
C OUT  CODI   : OBJET MATERIAU CODE (VOIR DESCRIPTION DANS MATCOD)
C
C ----------------------------------------------------------------------
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
      CHARACTER*32       JEXNUM , JEXNOM , JEXATR
C
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      INTEGER            INDMAT,NBMAT,IMATE,IGRP,INGRP
      CHARACTER*8        K8B
C ----------------------------------------------------------------------
C
      CALL JEMARQ()

      CALL JEDETR(NOMMAT//'.MATE_CODE.GRP')
      CALL JEDETR(NOMMAT//'.MATE_CODE.NGRP')
      CALL WKVECT(NOMMAT//'.MATE_CODE.GRP','V V K8',1,IGRP)
      CALL WKVECT(NOMMAT//'.MATE_CODE.NGRP','V V I',1,INGRP)
      ZK8(IGRP)=NOMMAT
      ZI(INGRP)=1
      
      CALL JEVEUT(NOMMAT//'.MATE_CODE.GRP' ,'L',IGRP)
      
      CODI=' '
      INDMAT=0
      NBMAT=1
      IMATE=1
      K8B = ' '
      CALL MATCOD (K8B,INDMAT,NBMAT,IMATE,IGRP,NOMMAT,CODI)

      CALL JEDEMA()
C
      END
