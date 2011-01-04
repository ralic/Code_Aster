      SUBROUTINE APREND(SDAPPA)
C     
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 03/01/2011   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
      CHARACTER*19 SDAPPA
C      
C ----------------------------------------------------------------------
C
C ROUTINE APPARIEMENT (ALGO)
C
C RECHERCHE DU NOEUD MAITRE LE PLUS PROCHE DU POINT COURANT
C
C ----------------------------------------------------------------------
C
C
C IN  SDAPPA : NOM DE LA SD APPARIEMENT
C
C -------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------------
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
C ---------------- FIN DECLARATIONS NORMALISEES JEVEUX -----------------
C
      INTEGER      IFM,NIV
      CHARACTER*24 APINFP,CONTNO,COORDO,DEFICO,NEWGEO
      INTEGER      JINFP ,JNOCO ,JCOOR
      INTEGER      INOM,IZONE,I,IP
      INTEGER      NBZONE,NTPT
      INTEGER      NBNOM,NBPT
      REAL*8       COORNM(3),COORPT(3)
      REAL*8       DISTM,DIST
      REAL*8       R8GAEM
      REAL*8       NORMD,NORMV,DIR(3),TOLEAP
      REAL*8       VECPML(3),VECPMM(3)
      INTEGER      JDECNM,NUMNOM,POSNOM
      INTEGER      POSMIN,TYPAPP
      LOGICAL      DIRAPP,PRTOLE,LEXCL
      INTEGER      VALI(2)
      REAL*8       VALR(4)
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFDBG('APPARIEMENT',IFM,NIV)
C
C --- AFFICHAGE
C
      IF (NIV.GE.2) THEN
        WRITE (IFM,*) '<APPARIEMENT> RECH. NOEUD PLUS PROCHE' 
      ENDIF
C
C --- ACCES SDAPPA
C
      APINFP = SDAPPA(1:19)//'.INFP' 
      CALL JEVEUO(APINFP,'L',JINFP )
      CALL APNOMK(SDAPPA,'DEFICO',DEFICO)
      CONTNO = DEFICO(1:16)//'.NOEUCO'
      CALL JEVEUO(CONTNO,'L',JNOCO)
      CALL APNOMK(SDAPPA,'NEWGEO',NEWGEO)
      COORDO = NEWGEO(1:19)//'.VALE'
      CALL JEVEUO(COORDO,'L',JCOOR)
C
C --- PARAMETRES
C
      CALL APPARI(SDAPPA,'APPARI_NBZONE',NBZONE)
      CALL APPARI(SDAPPA,'APPARI_NTPT'  ,NTPT  )    
C
C --- BOUCLE SUR LES ZONES
C
      IP     = 1
      DO 10 IZONE = 1,NBZONE
C
C ----- INFORMATION SUR LA ZONE 
C   
        CALL APZONI(SDAPPA,IZONE ,'NBPT'          ,NBPT  )
        CALL APZONI(SDAPPA,IZONE ,'NBNOM'         ,NBNOM )
        CALL APZONL(SDAPPA,IZONE ,'DIRE_APPA_FIXE',DIRAPP)
        CALL APZONI(SDAPPA,IZONE ,'JDECNM'        ,JDECNM)
        IF (DIRAPP) THEN
          CALL APZONV(SDAPPA,IZONE ,'DIRE_APPA_VECT',DIR   )
        ENDIF
        CALL APZONR(SDAPPA,IZONE ,'TOLE_APPA',TOLEAP)  
C
C ----- BOUCLE SUR LES POINTS
C      
        DO 20 I = 1,NBPT 
C
C ------- INITIALISATIONS
C
          DISTM  = R8GAEM()
          PRTOLE = .FALSE.
          LEXCL  = .FALSE.
          POSMIN = 0
          TYPAPP = 0
C
C ------- COORDONNEES DU POINT
C         
          CALL APCOPT(SDAPPA,IP   ,COORPT)
C
C ------- NOEUD EXCLU OU PAS ?
C          
          IF (ZI(JINFP+IP-1).EQ.1) THEN
            LEXCL  = .TRUE.
          ENDIF         
C
C ------- BOUCLE SUR LES NOEUDS MAITRES DE LA ZONE
C          
          DO 30 INOM = 1,NBNOM
C
C --------- POSITION DU NOEUD
C      
            POSNOM = JDECNM + INOM
C
C --------- NUMERO ABSOLU DU NOEUD
C        
            NUMNOM = ZI(JNOCO-1+POSNOM)
C
C --------- COORDONNEES DU NOEUD MAITRE
C  
            COORNM(1) = ZR(JCOOR+3*(NUMNOM-1))
            COORNM(2) = ZR(JCOOR+3*(NUMNOM-1)+1)
            COORNM(3) = ZR(JCOOR+3*(NUMNOM-1)+2)
C
C --------- DISTANCE
C  
            IF (DIRAPP) THEN
              NORMD    = SQRT(DIR(1)*DIR(1)+
     &                        DIR(2)*DIR(2)+
     &                        DIR(3)*DIR(3))
              NORMV    = SQRT((COORPT(1)-COORNM(1))**2+
     &                        (COORPT(2)-COORNM(2))**2+
     &                        (COORPT(3)-COORNM(3))**2)
              IF (NORMV.EQ.0.D0) THEN
                DIST = 1.D0
              ELSE
                DIST = ABS((COORPT(1)-COORNM(1))*DIR(1)+
     &                     (COORPT(2)-COORNM(2))*DIR(2)+
     &                     (COORPT(3)-COORNM(3))*DIR(3))/(NORMD*NORMV)
              ENDIF
            ELSE
              DIST = SQRT((COORPT(1)-COORNM(1))**2+
     &                    (COORPT(2)-COORNM(2))**2+
     &                    (COORPT(3)-COORNM(3))**2)
            ENDIF
            VECPML(1) = COORNM(1) - COORPT(1)
            VECPML(2) = COORNM(2) - COORPT(2)
            VECPML(3) = COORNM(3) - COORPT(3)         
C
C --------- SELECTION
C
            IF (DIST.LT.DISTM) THEN
              POSMIN = POSNOM
              DISTM  = DIST
              CALL DCOPY(3,VECPML,1,VECPMM,1)
              IF (TOLEAP.GT.0.D0) THEN
                IF (DIST.LE.TOLEAP) THEN
                  PRTOLE = .TRUE.
                ENDIF
              ELSE
                PRTOLE = .TRUE.
              ENDIF
            END IF
C
   30     CONTINUE 
C
C ------- APPARIEMENT HORS TOLE_APPA ?
C
          IF (PRTOLE) THEN
            TYPAPP = 1
          ELSE
            TYPAPP = -2
          ENDIF
C   
C ------- NOEUD EXCLU
C        
          IF (LEXCL) THEN
            TYPAPP = -1
          ENDIF
C   
C ------- QUELLQUES VERIFS
C
          CALL ASSERT(TYPAPP.NE.0)
          CALL ASSERT(POSMIN.NE.0)             
C
C ------- PREPARATION STOCKAGE
C
          VALI(1) = TYPAPP
          VALI(2) = POSMIN
          VALR(1) = DISTM
          VALR(2) = VECPMM(1)
          VALR(3) = VECPMM(2)
          VALR(4) = VECPMM(3)
C
C ------- STOCKAGE DE L'INFORMATION DANS SDAPPA
C   
          CALL APSAUV('ND_PROCHE',SDAPPA,IZONE ,IP    ,VALI  ,
     &                 VALR  )
C
C ------- POINT SUIVANT
C
          IP     = IP + 1     
  20    CONTINUE
  10  CONTINUE 
C
      CALL ASSERT((IP-1).EQ.NTPT)    
C
      CALL JEDEMA()
C 
      END
