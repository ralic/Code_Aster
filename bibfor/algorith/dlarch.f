      SUBROUTINE DLARCH (IARCHI,TYPE,NOMRES,NOMCMD,MASSE,
     &                   NEQ,DEP0,VIT0,ACC0,TEMPS)
      IMPLICIT   NONE
      INTEGER       IARCHI, NEQ
      CHARACTER*16  TYPE(*)
      CHARACTER*8   NOMRES, MASSE
      CHARACTER*16  NOMCMD
      REAL*8        DEP0(*),VIT0(*),ACC0(*),TEMPS                 
C ---------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 08/06/99   AUTEUR ACBHHCD G.DEVESA 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ======================================================================
C
C  ARCHIVAGE DANS L'OBJET CHAMNO DU CHAMP DE DEPLACEMENT,DE VITESSE
C  ET/OU D'ACCELERATION ISSU D'UN CALCUL TRANSITOIRE DIRECT
C    
C ---------------------------------------------------------------------
C  INPUT:
C        IARCHI   : NUMERO D'ORDRE DU CHAMP A ARCHIVER
C        TYPE     : TABLEAU INDIQUANT SI ON ARCHIVE LES DIFFERENTS
C                   CHAMPS (DEPL, VIT ET ACC) (NBSORT)
C        NOMRES   : NOM UTILISATEUR DU RESULTAT DE L'OPERATEUR
C        NOMCMD   : NOM DE L'OPERATEUR
C        MASSE    : NOM DE LA MATRICE DE MASSE
C        NEQ      : NOMBRE D'EQUATIONS (D.D.L. ACTIFS)
C        DEP0     : TABLEAU DES DEPLACEMENTS A L'INSTANT N (NEQ)
C        VIT0     : TABLEAU DES VITESSES A L'INSTANT N (NEQ)
C        ACC0     : TABLEAU DES ACCELERATIONS A L'INSTANT N (NEQ)
C        TEMPS    : INSTANT DE CALCUL
C
C     ------------------------------------------------------------------
C      ----DEBUT DES COMMUNS JEVEUX--------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16              ZK16
      CHARACTER*24                        ZK24
      CHARACTER*32                                  ZK32
      CHARACTER*80                                            ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)

C      ----FIN DES COMMUNS JEVEUX----------
C
      INTEGER       IFM, NIV, NBSORT, IER, ITYPE, LVALE, IEQ 
      INTEGER       LINST, ITY
      CHARACTER*8   K8B
      CHARACTER*24  CHAMNO
C ----------------------------------------------------------------      
      CALL JEMARQ()
C
C-----RECUPERATION DU NIVEAU D'IMPRESSION
C
      CALL INFNIV(IFM,NIV)       
C       
      NBSORT = 3       
      DO 44 ITYPE = 1, NBSORT
             IF ( TYPE(ITYPE) .EQ. '    ' ) GOTO 44
             CALL RSEXCH(NOMRES,TYPE(ITYPE),IARCHI,CHAMNO,IER)
             IF ( IER .EQ. 0 ) THEN
                CALL UTMESS('A',NOMCMD,
     &                           CHAMNO//'CHAM_NO DEJA EXISTANT')
             ELSE IF ( IER .EQ. 100 ) THEN
                CALL VTCREM(CHAMNO,MASSE,'G','R')
             ELSE
                CALL UTMESS('F',NOMCMD,'APPEL ERRONE')
             ENDIF
             CHAMNO(20:24)  = '.VALE'
             CALL JEVEUO(CHAMNO,'E',LVALE)
             IF (ITYPE.EQ.1) THEN
                DO 46 IEQ = 1, NEQ
                     ZR(LVALE+IEQ-1) = DEP0(IEQ)
 46             CONTINUE
             ELSE IF (ITYPE.EQ.2) THEN
                DO 47 IEQ = 1, NEQ
                   ZR(LVALE+IEQ-1) = VIT0(IEQ)
 47             CONTINUE 
             ELSE 
                DO 48 IEQ = 1, NEQ
                   ZR(LVALE+IEQ-1) = ACC0(IEQ)
 48             CONTINUE
             ENDIF  
             CALL JELIBE(CHAMNO)
             CALL RSNOCH(NOMRES,TYPE(ITYPE),IARCHI,' ')
 44   CONTINUE
      CALL RSADPA(NOMRES,'E',1,'INST',IARCHI,0,LINST,K8B)
      ZR(LINST) = TEMPS
      IF (NIV.EQ.2) THEN
        WRITE(IFM,1000) (TYPE(ITY),ITY=1,3), IARCHI, TEMPS
      ENDIF
C 
 1000 FORMAT(1P,3X,'CHAMP(S) STOCKE(S):',3(1X,A4),
     &             ' NUME_ORDRE:',I8,' INSTANT:',D12.5)          
C
      CALL JEDEMA()
      END           
