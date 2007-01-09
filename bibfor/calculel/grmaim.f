      SUBROUTINE GRMAIM(UNIT  ,GRMAMA,DEGMAX)
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
      CHARACTER*24 GRMAMA
      INTEGER      UNIT,DEGMAX
C      
C ----------------------------------------------------------------------
C
C ROUTINE ARLEQUIN
C
C IMPRESSION DU GRAPHE MAILLE/MAILLE
C
C ----------------------------------------------------------------------
C
C
C IN  UNIT   : UNITE D'IMPRESSION 
C IN  GRMAMA : NOM DE LA STRUCTURE DE DONNEES GRAPHE MAILLE/MAILLE*
C IN  DEGMAX : DEGRE DU GRAPHE MAILLE/MAILLE
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
      INTEGER      JAPP
      CHARACTER*8  K8BID,NOMO,NOMA,NOMMAI
      INTEGER      NM1,NM2,IM1,IM2,IRET,JNOMA,IOCC
C      
C ----------------------------------------------------------------------
C
      CALL JEMARQ()    
C
C --- RECUPERATION MAILLAGE 
C
      CALL GETVID(' ','MODELE',0,1,1,NOMO,IOCC)
      IF (IOCC.EQ.0) THEN
        WRITE(UNIT,*) '<GRMAMA  > AFFICHAGE IMPOSSIBLE (VOIR GRMAIM) '
        GOTO 999      
      ENDIF
      CALL JEVEUO(NOMO(1:8)//'.MODELE    .NOMA','L',JNOMA)
      NOMA = ZK8(JNOMA)   
C      
      CALL JEEXIN(GRMAMA,IRET)
      IF (IRET.EQ.0) THEN
        WRITE(UNIT,*) '<GRMAMA  > SD GRAPHE MAILLE/MAILLE <',
     &           GRMAMA(1:24),'> N''EXISTE PAS'        
      ELSE
        CALL JELIRA(GRMAMA(1:24),'NUTIOC',NM1,K8BID)
        WRITE(UNIT,*) '<GRMAMA  > ... NOMBRE DE MAILLES          : ',
     &                 NM1
        WRITE(UNIT,*) '<GRMAMA  > ... DEGRE MAXIMUM DU GRAPHE    : ',
     &                 DEGMAX
C        
        DO 60 IM1 = 1 , NM1
          CALL JEVEUO(JEXNUM(GRMAMA,IM1),'L',JAPP)
          CALL JELIRA(JEXNUM(GRMAMA,IM1),'LONMAX',NM2,K8BID)
          CALL JENUNO(JEXNUM(NOMA(1:8)//'.NOMMAI',IM1),NOMMAI) 
          WRITE(UNIT,*) '<GRMAMA  > ... LA MAILLE ',NOMMAI,
     &                  ' A ',NM2,' MAILLES VOISINES: '
          DO 61 IM2 = 1 , NM2
            CALL JENUNO(JEXNUM(NOMA(1:8)//'.NOMMAI',ZI(JAPP-1+IM2)),
     &                  NOMMAI) 
            WRITE(UNIT,*) '<GRMAMA  > ...... ',NOMMAI
 61       CONTINUE       
 60     CONTINUE                    
      ENDIF 
C
 999  CONTINUE      
C
      CALL JEDEMA()
      END
