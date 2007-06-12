      SUBROUTINE MORSIM(UNIT,NGRMAC,NGRMA)
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
      CHARACTER*10  NGRMAC,NGRMA      
      INTEGER       UNIT
C      
C ----------------------------------------------------------------------
C
C ROUTINE ARLEQUIN
C
C IMPRESSION DE LA SD POUR LES MATRICES MORSES
C
C ----------------------------------------------------------------------
C
C
C IN  UNIT   : UNITE D'IMPRESSION 
C IN  NGRMA  : NOM DE LA LISTE DES MAILLES DU GROUPE
C IN  NGRMAC : NOM DE LA LISTE DES MAILLES DU GROUPE DE COLLAGE
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
      INTEGER      NLIG,NCOL,ILIG,ICOL,DIME,IVAL,IDIM
      INTEGER      JDIME,JGRMA,JGRMAC,JVALE
      CHARACTER*8  K8BID,NOMO,NOMA,NOMNO1,NOMNO2
      INTEGER      NUMNO1,NUMNO2,IOCC,JNOMA
      REAL*8       VALE
C      
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- RECUPERATION MAILLAGE 
C
      CALL GETVID(' ','MODELE',0,1,1,NOMO,IOCC)
      IF (IOCC.EQ.0) THEN
        WRITE(UNIT,*) '<MORSE   > AFFICHAGE IMPOSSIBLE (VOIR MORSIM) '
        GOTO 999  
      ELSE
        WRITE(UNIT,*) '<MORSE   > SD DE LA MATRICE MORSE...'
      ENDIF
      CALL JEVEUO(NOMO(1:8)//'.MODELE    .NOMA','L',JNOMA)
      NOMA = ZK8(JNOMA)                       
C          
      CALL JEEXIN(NGRMAC(1:10)//'.INO',IRET)
      IF (IRET.EQ.0) THEN
        WRITE(UNIT,*) '<MORSE   > SD MORSE: <',
     &          NGRMAC(1:10)//'.INO','> N''EXISTE PAS'        
      ELSE
        CALL JEVEUO(NGRMAC(1:10)//'.INO','L',JGRMAC)
        CALL JELIRA(NGRMAC(1:10)//'.INO','LONMAX',NLIG,K8BID)      
C        
        CALL JEEXIN(NGRMA(1:10)//'.MORSE.DIME',IRET)
        IF (IRET.EQ.0) THEN
          WRITE(UNIT,*) '<MORSE   > SD MORSE: <',
     &            NGRMA(1:10)//'.MORSE.DIME','> N''EXISTE PAS'  
          GOTO 999       
        ELSE
          CALL JEVEUO(NGRMA(1:10)//'.MORSE.DIME','L',JDIME)
          DIME = ZI(JDIME)
        ENDIF
C
        CALL JEEXIN(NGRMA(1:10)//'.MORSE.INO',IRET)
        IF (IRET.EQ.0) THEN
          WRITE(UNIT,*) '<MORSE   > SD MORSE: <',
     &            NGRMA(1:10)//'.MORSE.INO','> N''EXISTE PAS'  
          GOTO 999       
        ENDIF
C        
        WRITE(UNIT,*) '<MORSE   > ... DIMENSION DES MATRICES NODALES: ',
     &                 DIME
C   
        CALL JEEXIN(NGRMA(1:10)//'.MORSE.VALE',IRET)
        IF (IRET.EQ.0) THEN
          WRITE(UNIT,*) '<MORSE   > SD MORSE: <',
     &            NGRMA(1:10)//'.MORSE.VALE','> N''EXISTE PAS'  
          GOTO 999       
        ELSE
          CALL JEVEUO(NGRMA(1:10)//'.MORSE.VALE','L',JVALE)
          IVAL = 1
        ENDIF
C
        DO 70 ILIG = 1 , NLIG
          CALL JEVEUO(JEXNUM(NGRMA(1:10)//'.MORSE.INO',ILIG),'L',JGRMA)
          NUMNO1 = ZI(JGRMAC+ILIG-1) 
          CALL JENUNO(JEXNUM(NOMA(1:8)//'.NOMNOE',NUMNO1),NOMNO1)       
          IF (NUMNO1.NE.0) THEN
            CALL JELIRA(JEXNUM(NGRMA(1:10)//'.MORSE.INO',ILIG),'LONMAX',
     &                  NCOL,K8BID)
            CALL JEVEUO(JEXNUM(NGRMA(1:10)//'.MORSE.INO',ILIG),'L',
     &                  JGRMA)     
            DO 80 ICOL = 1 , NCOL 
              NUMNO2 = ZI(JGRMA+ICOL-1)
              CALL JENUNO(JEXNUM(NOMA(1:8)//'.NOMNOE',NUMNO2),NOMNO2)
              WRITE(UNIT,*) '<MORSE   > ...... COUPLE (',
     &                      ILIG,',',ICOL,' ) : ',
     &                    NOMNO1,'/',NOMNO2
              DO 90 IDIM = 1,DIME
                VALE = ZR(JVALE-1+IVAL)
                WRITE(UNIT,*) '<MORSE   > ...... VALE  : ',IDIM,
     &                        ' - ',VALE   
                IVAL = IVAL + 1           
  90          CONTINUE                        
C               
 80         CONTINUE 
          ENDIF            
 70     CONTINUE 

      ENDIF       
C
  999 CONTINUE      
C
      CALL JEDEMA()
      END
