      SUBROUTINE APREMA(SDAPPA)
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
C RECHERCHE DE LA MAILLE MAITRE LA PLUS PROCHE DU POINT COURANT
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
      INTEGER      IZONE,IP,I,POSNOM
      INTEGER      NBZONE,NDIMG,NTPT
      INTEGER      NBPT,POSMAM,IPROJM
      REAL*8       COORPT(3),TAU1M(3),TAU2M(3),DISTM,KSI1M,KSI2M
      REAL*8       DIR(3),TOLEOU,EPSMAX,VECPMM(3)
      INTEGER      ITEMAX
      LOGICAL      DIRAPP,LMAESC,LSAUVE
      INTEGER      TYPAPP,ENTAPP
      INTEGER      VALI(2)
      REAL*8       VALR(12)
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFDBG('APPARIEMENT',IFM,NIV)   
C
C --- AFFICHAGE
C
      IF (NIV.GE.2) THEN
        WRITE (IFM,*) '<APPARIEMENT> RECH. MAILLE PLUS PROCHE' 
      ENDIF
C
C --- PARAMETRES
C
      CALL APPARI(SDAPPA,'APPARI_NBZONE' ,NBZONE)
      CALL APPARI(SDAPPA,'PROJ_NEWT_ITER',ITEMAX)
      CALL APPARR(SDAPPA,'PROJ_NEWT_RESI',EPSMAX)
      CALL APPARI(SDAPPA,'APPARI_NDIMG'  ,NDIMG )
      CALL APPARI(SDAPPA,'APPARI_NTPT'   ,NTPT  )       
C
C --- BOUCLE SUR LES ZONES
C
      IP     = 1
      DO 10 IZONE = 1,NBZONE 
C
C ----- INFORMATION SUR LA ZONE 
C   
        CALL APZONI(SDAPPA,IZONE ,'NBPT'          ,NBPT  )
        CALL APZONL(SDAPPA,IZONE ,'APPA_MAIT_ESCL',LMAESC)
        CALL APZONL(SDAPPA,IZONE ,'DIRE_APPA_FIXE',DIRAPP)
        IF (DIRAPP) THEN
          CALL APZONV(SDAPPA,IZONE ,'DIRE_APPA_VECT',DIR   )
        ENDIF
        CALL APZONR(SDAPPA,IZONE ,'TOLE_PROJ_EXT' ,TOLEOU)
C
C ----- BOUCLE SUR LES POINTS
C     
        DO 20 I = 1,NBPT       
C
C ------- LE POINT DOIT-IL ETRE APPARIE ?
C      
          CALL APINFI(SDAPPA,'APPARI_TYPE'  ,IP    ,TYPAPP)
          CALL ASSERT(TYPAPP.NE.0)
C
C ------- POINT A APPARIER !
C     
          IF (LMAESC) THEN
C
C --------- COORDONNEES DU POINT
C         
            CALL APCOPT(SDAPPA,IP   ,COORPT) 
C
C --------- NUMERO DU NOEUD MAITRE LE PLUS PROCHE
C           
            CALL APINFI(SDAPPA,'APPARI_ENTITE',IP    ,ENTAPP)
            POSNOM = ENTAPP
C
C --------- PROJECTION SUR LA MAILLE MAITRE
C            
            CALL APPROJ(SDAPPA,POSNOM,DIRAPP,DIR   ,ITEMAX,
     &                  EPSMAX,TOLEOU,COORPT,POSMAM,IPROJM,
     &                  KSI1M ,KSI2M ,TAU1M ,TAU2M ,DISTM ,
     &                  VECPMM)
C
C --------- ORTHOGONALISATION VECTEURS TANGENTS
C     
            CALL APORTH(SDAPPA,NDIMG ,POSMAM,COORPT,TAU1M ,
     &                  TAU2M )
C
            IF (TYPAPP.EQ.1) THEN
              IF (IPROJM.EQ.2) THEN
                TYPAPP = -3
              ELSE
                TYPAPP = 2
              ENDIF
            ENDIF
            LSAUVE = .TRUE. 
          ELSE
            LSAUVE = .FALSE.
          ENDIF        
C
C ------- STOCKAGE DE L'INFORMATION DANS SDAPPA
C   
          IF (LSAUVE) THEN 
            VALI(1)  = TYPAPP
            VALI(2)  = POSMAM
            VALR(1)  = DISTM 
            VALR(2)  = KSI1M
            VALR(3)  = KSI2M
            VALR(4)  = TAU1M(1)
            VALR(5)  = TAU1M(2)
            VALR(6)  = TAU1M(3)
            VALR(7)  = TAU2M(1)
            VALR(8)  = TAU2M(2)
            VALR(9)  = TAU2M(3)
            VALR(10) = VECPMM(1)
            VALR(11) = VECPMM(2)
            VALR(12) = VECPMM(3)
            CALL APSAUV('MA_PROCHE',SDAPPA,IZONE ,IP    ,VALI  ,
     &                  VALR       )           
          ENDIF   
C
C ------- POINT SUIVANT
C
          IP     = IP + 1   
  20    CONTINUE
  10  CONTINUE   
C
      CALL JEDEMA()
C 
      END
