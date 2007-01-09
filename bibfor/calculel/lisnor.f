      SUBROUTINE LISNOR(CARA,DIME,TYPMAI,NNORM ,NTANG  )  
C               
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 09/01/2007   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C                                                                       
C                                                                       
C ======================================================================
C RESPONSABLE ABBAS M.ABBAS
C     
      IMPLICIT NONE
      CHARACTER*8   CARA
      INTEGER       DIME
      CHARACTER*16  TYPMAI
      CHARACTER*10  NNORM,NTANG 
C      
C ----------------------------------------------------------------------
C
C ROUTINE ARLEQUIN
C
C LISSAGE DES NORMALES D'UN GROUPE D'ELEMENTS COQUES
C
C ----------------------------------------------------------------------
C     
C IN  CARA   : SD CARA_ELEM
C IN  DIME   : DIMENSION DE L'ESPACE
C IN  TYPMAI : SD CONTENANT NOM DES TYPES ELEMENTS (&&CATA.NOMTM)
C I/O NNORM  : NOM DE L'OBJET NORMALES LISSEES
C I/O NTANG  : NOM DE L'OBJET TANGENTES LISSEES
C
C SD DE SORTIE
C NNORM : NOEUD DU MAILLAGE -> NORMALE NORMEE * EPAISSEUR MOYENNE / 2
C          ( NX1, NY1, [NZ1], NX2, ...)
C NTANG : NOEUD DU MAILLAGE -> TANGENTE[S] NORMEE[S]
C          ( TX1.1, TY1.1, [TZ1.1, TX1.2, TY1.2, TZ1.2], TX2.1, ...)
C
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
C
      CHARACTER*32       JEXNUM , JEXATR
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
      REAL*8        DNRM2,R8PREM
      CHARACTER*8   MAIL,K8BID,NOMMAI,NOMNOE,VALK(2),TYPEMA
      REAL*8        EPAIS,CNOEUD(18),TANG(54),W(3),NORM
      INTEGER       NNO,NBMA,NZONE,NCMP,IZONE,NUNO,NCPMA,NBNO
      INTEGER       NBNORM,NBTANG
      INTEGER       ZONCOQ
      INTEGER       Q0,Q1
      INTEGER       JDESC,JCOMPT,JMAIL,JDIM,JVALE,JMA
      INTEGER       JCONX,JLONG,JCOORD
      INTEGER       JDECAL
      INTEGER       JNORM,JTANG
      INTEGER       ITANG
      INTEGER       I,J,NUMA,INO,JTYPMM
      LOGICAL       LTOUT,ISBORD
C      
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- INITIALISATIONS
C 
      CALL JEVEUO(TYPMAI,'L',JTYPMM)   
C
C --- DESACTIVATION FLOATING POINT EXCEPTION
C
      CALL MATFPE(-1)        
C      
C --- LECTURE INFOS CARA_ELEM DES COQUES
C --- NCMP: NOMBRE DE COMPOSANTES DANS SD CARA_COQ
C --- NZONE: NOMBRE DE ZONES DEFINIES DANS AFFE_CARA_ELEM
C
      CALL JEVEUO(CARA//'.CARCOQUE  .NOMA','L',JMAIL)
      CALL JEVEUO(CARA//'.CARCOQUE  .DESC','L',JDESC)
      CALL JEVEUO(CARA//'.CARCOQUE  .VALE','L',JVALE)
      CALL JELIRA(JEXNUM('&CATA.GD.NOMCMP',ZI(JDESC)),'LONMAX',
     &            NCMP,K8BID)
      NZONE = ZI(JDESC+2)      
C      
C --- LECTURE INFOS DU MAILLAGE
C --- MAIL: NOM DU MAILLAGE
C --- NNO : NOMBRE TOTAL DE NOEUDS DU MAILLAGE
C --- NBMA: NOMBRE TOTAL DE MAILLES DU MAILLAGES
C
      MAIL = ZK8(JMAIL)
      CALL JEVEUO(MAIL(1:8)//'.DIME','L',JDIM)
      NNO  = ZI(JDIM)
      NBMA = ZI(JDIM+2)
      CALL JEVEUO(MAIL(1:8)//'.CONNEX','L',JCONX)
      CALL JEVEUO(JEXATR(MAIL(1:8)//'.CONNEX','LONCUM'),'L',JLONG)
      CALL JEVEUO(MAIL(1:8)//'.COORDO    .VALE','L',JCOORD)
C
C --- CREATIONS SD POUR NORMALES ET TANGENTES
C --- NBNORM: NOMBRE DE COMPOSANTES POUR LA NORMALE EN UN NOEUD
C --- NBTANG: NOMBRE DE COMPOSANTES POUR LES TANGENTES EN UN NOEUD
C
      NBNORM = DIME
      NBTANG = (DIME-1)*DIME
C
      CALL WKVECT(NNORM,'V V R',NBNORM*NNO,JNORM)
      CALL WKVECT(NTANG,'V V R',NBTANG*NNO,JTANG)
      CALL WKVECT('&&LISNOR.COMPTEUR','V V I',NNO,JCOMPT)
C
C --- CALCUL DES NORMALES LISSEES
C
      DO 40 IZONE = 1, NZONE
C
C --- DEMI-EPAISSEUR DE LA COQUE
C
        EPAIS  = 0.5D0*ZR(JVALE+NCMP*(IZONE-1))    
        LTOUT  = .FALSE. 
C        
C --- CHOIX DU POINTEUR SUR SD CARA_COQ SELON SI MAILLE, GROUP_MA OU
C --- TOUT  
C     
        ZONCOQ = ZI(JDESC+1+2*IZONE)
C        
        IF (ZONCOQ.EQ.1) THEN
          LTOUT = .TRUE.
        ELSEIF (ZONCOQ.EQ.2) THEN
          J = ZI(JDESC+1+2*IZONE+1)
          CALL JEVEUO(JEXNUM(MAIL//'.GROUPEMA',J),'L',JMA)
          CALL JELIRA(JEXNUM(MAIL//'.GROUPEMA',J),'LONMAX',
     &                NBMA,K8BID)
        ELSEIF (ZONCOQ.EQ.3) THEN
          J = ZI(JDESC+1+2*IZONE+1)
          CALL JEVEUO(JEXNUM(CARA//'.CARCOQUE  .LIEL',J),'L',JMA)
          CALL JELIRA(JEXNUM(CARA//'.CARCOQUE  .LIEL',J),'LONMAX',
     &                NBMA,K8BID)
        ELSE    
          CALL ASSERT(.FALSE.)
        ENDIF              
C
C --- CALCUL EN CHAQUE NOEUD
C
        DO 60 I = 1, NBMA
          IF (LTOUT) THEN
            NUMA = I
          ELSE
            NUMA = ZI(JMA-1+I)
          ENDIF
C
C --- TEST DE LA MAILLE 
C      
          CALL JENUNO(JEXNUM(MAIL//'.NOMMAI',NUMA),NOMMAI)  
          TYPEMA = ZK8(JTYPMM+NUMA-1)    
          IF (ISBORD(NOMMAI,TYPEMA,DIME)) THEN
            GOTO 60
          ENDIF              
C
C --- COORDONNEES DES NOEUDS DE LA MAILLE NUMA
C
          CALL COSOLI(NUMA  ,ZI(JCONX),ZI(JLONG),ZR(JCOORD),DIME  ,
     &                CNOEUD)
          NBNO   = ZI(JLONG+NUMA) - ZI(JLONG+NUMA-1)
C    
C --- TANGENTES DE LA MAILLE NUMA EXPRIMEES AUX NOEUDS
C     
          CALL TANGNT(CNOEUD,NBNO,DIME,TANG)  
          JDECAL = JCONX-1+ZI(JLONG-1+NUMA)
          ITANG  = 1  
          DO 61 INO = 1, NBNO
            NUNO = ZI(JDECAL+INO-1)
            CALL JENUNO(JEXNUM(MAIL//'.NOMNOE',NUNO),NOMNOE)
            ZI(JCOMPT-1+NUNO) = ZI(JCOMPT-1+NUNO) + 1     
            IF (DIME.EQ.2) THEN
              NORM = DNRM2(2,TANG(ITANG),1)   
              IF (NORM.LE.R8PREM()) THEN
                VALK(1) = NOMNOE
                VALK(2) = NOMMAI              
                CALL U2MESK('F','ARLEQUIN_6',2,VALK)            
              ENDIF  
              ZR(JNORM + 2*(NUNO-1))   = ZR(JNORM + 2*(NUNO-1)  ) -
     &                   TANG(ITANG+1)*(EPAIS/NORM)
              ZR(JNORM + 2*(NUNO-1)+1) = ZR(JNORM + 2*(NUNO-1)+1) + 
     &                   TANG(ITANG  )*(EPAIS/NORM)
              ITANG = ITANG + 2
            ELSE
              W(1) = TANG(ITANG+1)*TANG(ITANG+5)-
     &               TANG(ITANG+2)*TANG(ITANG+4)
              W(2) = TANG(ITANG+2)*TANG(ITANG+3)-
     &               TANG(ITANG  )*TANG(ITANG+5)
              W(3) = TANG(ITANG  )*TANG(ITANG+4)-
     &               TANG(ITANG+1)*TANG(ITANG+3)
              NORM = DNRM2(3,W,1)
              IF (NORM.LE.R8PREM()) THEN
                VALK(1) = NOMNOE
                VALK(2) = NOMMAI              
                CALL U2MESK('F','ARLEQUIN_6',2,VALK)             
              ENDIF                         
              CALL DAXPY(3,EPAIS/NORM,W,1,ZR(JNORM + 3*(NUNO-1)),1)
              CALL DCOPY(3,TANG(ITANG),1,ZR(JTANG + 6*(NUNO-1)),1)
              ITANG = ITANG + 6
            ENDIF 
 61       CONTINUE
 60     CONTINUE                 
 40   CONTINUE
C
C --- CALCUL DE LA MOYENNE SUR LES NOEUDS
C

      Q0 = JNORM
      Q1 = JTANG
      DO 70 INO = 1, NNO
        NCPMA = ZI(JCOMPT)
        IF (NCPMA.GT.0) THEN     
          CALL JENUNO(JEXNUM(MAIL//'.NOMNOE',INO),NOMNOE)
          IF (DIME.EQ.2) THEN
            NORM = DNRM2(2,ZR(Q0),1)
            IF (NORM.LE.R8PREM()) THEN
              VALK(1) = NOMMAI              
              CALL U2MESK('F','ARLEQUIN_7',1,VALK) 
            ENDIF                  
            ZR(Q1  ) = ZR(Q0+1)/NORM
            ZR(Q1+1) =-ZR(Q0  )/NORM
          ELSE
            CALL PROVEC(ZR(Q0),ZR(Q1),ZR(Q1+3))
            NORM = DNRM2(3,ZR(Q1+3),1) 
            IF (NORM.LE.R8PREM()) THEN
              VALK(1) = NOMMAI              
              CALL U2MESK('F','ARLEQUIN_7',1,VALK) 
            ENDIF
            CALL DSCAL(3,1.D0/NORM,ZR(Q1+3),1)
            CALL PROVEC(ZR(Q1+3),ZR(Q0),ZR(Q1))
            CALL DSCAL(3,1.D0/NORM,ZR(Q1),1)
          ENDIF
          CALL DSCAL(DIME,1.D0/NCPMA,ZR(Q0),1)
        ENDIF
        Q0     = Q0 + NBNORM
        Q1     = Q1 + NBTANG
        JCOMPT = JCOMPT + 1
 70   CONTINUE
C
      CALL JEDETR('&&LISNOR.COMPTEUR')
C
C --- REACTIVATION FLOATING POINT EXCEPTION
C
      CALL MATFPE(1)      
C         
      CALL JEDEMA()
      END
