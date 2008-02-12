      SUBROUTINE APPAIM(UNIT,NGRMA1,NGRMA2,NOMAPP)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 12/02/2008   AUTEUR ABBAS M.ABBAS 
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
      CHARACTER*14 NOMAPP
      CHARACTER*10 NGRMA1,NGRMA2
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
      INTEGER      IM1,NM1,IM2,NM2
      CHARACTER*8  K8BID,NOMO,NOMA,NOMMA1,NOMMA2
      INTEGER      NUMMA1,NUMMA2,IOCC,LONMAX
      INTEGER      JAPP,JLGRF,JLIST,JGRMA1,JGRMA2
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
      CALL JEVEUO(NOMO(1:8)//'.MODELE    .LGRF','L',JLGRF)
      NOMA = ZK8(JLGRF)       
C      
      CALL JEVEUO(NGRMA1(1:10)//'.GROUPEMA','L',JGRMA1)
      CALL JEVEUO(NGRMA2(1:10)//'.GROUPEMA','L',JGRMA2)
C
      CALL JEEXIN(NOMAPP,IRET)
      IF (IRET.EQ.0) THEN
        WRITE(UNIT,*) '<APPARIE > SD GRAPHE APPARIEMENT INCORRECTE <',
     &           NOMAPP,'> N''EXISTE PAS'  
      ELSE
        CALL JELIRA(NOMAPP,'NUTIOC',NM1,K8BID)
        WRITE(UNIT,*) '<APPARIE > ... NOMBRE DE MAILLES APPARIEES: ',
     &                 NM1
C
C --- CREATION SD TEMPORAIRE POUR AFFICHAGE MAILLES
C
        LONMAX = 0
        DO 50 IM1 = 1 , NM1
          CALL JELIRA(JEXNUM(NOMAPP,IM1),'LONMAX',NM2,K8BID)
          LONMAX = MAX(LONMAX,NM2)
   50   CONTINUE       
C
        IF (LONMAX.LE.0) THEN
         WRITE(UNIT,*) '<APPARIE  > AFFICHAGE IMPOSSIBLE (VOIR GRMAIM) '
          GOTO 999   
        ELSE
          CALL WKVECT('&&APPAIM.LIST','V V K8',LONMAX,JLIST)
        ENDIF     
C
C --- AFFICHAGE MAILLES
C    
        DO 60 IM1 = 1 , NM1
          CALL JEVEUO(JEXNUM(NOMAPP,IM1),'L',JAPP)
          CALL JELIRA(JEXNUM(NOMAPP,IM1),'LONMAX',NM2,K8BID)
          NUMMA1 = ZI(JGRMA1+IM1-1)
          CALL JENUNO(JEXNUM(NOMA(1:8)//'.NOMMAI',NUMMA1),NOMMA1)
C          
          WRITE(UNIT,*) '<APPARIE > ... LA MAILLE ',NOMMA1,
     &                  ' EST APPARIEE AVEC ',NM2,
     &                  ' MAILLES: '
C
          DO 61 IM2 = 1 , NM2
            NUMMA2 = ZI(JAPP-1+IM2)
            IF (NUMMA2.NE.0) THEN
              CALL JENUNO(JEXNUM(NOMA(1:8)//'.NOMMAI',NUMMA2),NOMMA2)
              ZK8(JLIST+IM2-1) = NOMMA2
            ELSE
              ZK8(JLIST+IM2-1) = '-FILTRE-'
            ENDIF  
 61       CONTINUE 
          WRITE(UNIT,1050) (ZK8(JLIST+IM2-1), IM2 = 1,NM2)     
 60     CONTINUE                    
      ENDIF 
C
 1050 FORMAT ((' <APPARIE > ...... ',17X,5(A8,1X)))       
C
  999 CONTINUE      
C
      CALL JEDEMA()
      END
