      SUBROUTINE APPAIM(UNIT,NOMAPP)
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
      CHARACTER*24 NOMAPP
      INTEGER      UNIT
C      
C ----------------------------------------------------------------------
C
C APPARIEMENT DE DEUX GROUPES DE MAILLE PAR LA METHODE
C BOITES ENGLOBANTES + ARBRE BSP
C
C IMPRESSION DU GRAPHE
C
C ----------------------------------------------------------------------
C
C
C IN  UNIT   : UNITE D'IMPRESSION 
C IN  NOMAPP : NOM DE LA STRUCTURE DE DONNEES GRAPHE APPARIEMENT
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
      INTEGER      IRET 
      LOGICAL      DEBUG
      INTEGER      IM1,NM1,IM2,NM2,JAPP
      CHARACTER*8  K8BID,NOMO,NOMA,NOMMAI
      INTEGER      NUMMAI,IOCC,JNOMA
C      
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- RECUPERATION MAILLAGE 
C
      CALL GETVID(' ','MODELE',0,1,1,NOMO,IOCC)
      IF (IOCC.EQ.0) THEN
        WRITE(UNIT,*) '<APPARIE > AFFICHAGE IMPOSSIBLE (VOIR APPAIM) '
        GOTO 999
      ELSE
        WRITE(UNIT,*) '<APPARIE > GRAPHE D''APPARIEMENT...'
      ENDIF
      CALL JEVEUO(NOMO(1:8)//'.MODELE    .NOMA','L',JNOMA)
      NOMA = ZK8(JNOMA)       
C
      DEBUG = .TRUE.
C
      CALL JEEXIN(NOMAPP,IRET)
      IF (IRET.EQ.0) THEN
        WRITE(UNIT,*) '<APPARIE > SD GRAPHE APPARIEMENT INCORRECTE <',
     &           NOMAPP(1:24),'> N''EXISTE PAS'  
      ELSE
        CALL JELIRA(NOMAPP(1:24),'NUTIOC',NM1,K8BID)
        WRITE(UNIT,*) '<APPARIE > ... NOMBRE DE MAILLES APPARIEES: ',
     &                 NM1
        
        DO 60 IM1 = 1 , NM1
          CALL JEVEUO(JEXNUM(NOMAPP,IM1),'L',JAPP)
          CALL JELIRA(JEXNUM(NOMAPP,IM1),'LONMAX',NM2,K8BID)
          CALL JENUNO(JEXNUM(NOMA(1:8)//'.NOMMAI',IM1),NOMMAI)
          WRITE(UNIT,*) '<APPARIE > ... (',IM1,' ) LA MAILLE ',NOMMAI,
     &                  ' EST APPARIEE AVEC ',NM2,
     &                  ' MAILLES: '
          IF (DEBUG) THEN
            DO 61 IM2 = 1 , NM2
              NUMMAI = ZI(JAPP-1+IM2)
              IF (NUMMAI.NE.0) THEN
                CALL JENUNO(JEXNUM(NOMA(1:8)//'.NOMMAI',NUMMAI),NOMMAI)
                WRITE(UNIT,*) '<APPARIE > ...... ',NOMMAI
              ELSE
                WRITE(UNIT,*) '<APPARIE > ...... MAILLE FILTREE'
              ENDIF  
 61         CONTINUE 
          ENDIF                
 60     CONTINUE                    
      ENDIF 
C
  999 CONTINUE      
C
      CALL JEDEMA()
      END
