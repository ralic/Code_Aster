      SUBROUTINE NMEVCO(SDDISC,RESOCO,IECHEC,IEVDAC)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 19/09/2011   AUTEUR ABBAS M.ABBAS 
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
      CHARACTER*24 RESOCO
      INTEGER      IECHEC,IEVDAC
      CHARACTER*19 SDDISC
C
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (ALGORITHME - EVENEMENTS)
C
C GESTION DE L'EVENEMENT COLLISION
C
C ----------------------------------------------------------------------
C
CE
C IN  DEFICO : SD POUR LA DEFINITION DE CONTACT
C IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
C IN  IECHEC : OCCURRENCE DE L'ECHEC
C OUT IEVDAC : VAUT IECHEC SI EVENEMENT DECLENCHE
C                   0 SINON
C
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
C
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      INTEGER      IFM,NIV
      INTEGER      CFDISD,NBLIAI
      INTEGER      ILIAI
      INTEGER      POSNOE
      CHARACTER*24 JEUX
      INTEGER      JJEUX
      CHARACTER*24 JEUEVD,NUMLIA
      INTEGER      JJEVD,JNUMLI
      REAL*8       ETATCO,JEUINI
      LOGICAL      LEVENT
      REAL*8       PRECCO
      INTEGER      IBID
      CHARACTER*8  K8BID
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFDBG('MECA_NON_LINE',IFM,NIV)
C
C --- AFFICHAGE
C
      IF (NIV.GE.2) THEN
        WRITE (IFM,*) '<MECANONLINE> ... COLLISION'
      ENDIF
C
C --- INITIALISATIONS
C
      CALL UTDIDT('L'   ,SDDISC,'ECHE',IECHEC,'PREC_COLLISION',
     &            PRECCO,IBID  ,K8BID )
      IEVDAC = 0
      LEVENT = .FALSE.
C
C --- PARAMETRES
C
      NBLIAI = CFDISD(RESOCO,'NBLIAI')     
C
C --- ACCES OBJETS DU CONTACT
C
      NUMLIA = RESOCO(1:14)//'.NUMLIA'
      JEUX   = RESOCO(1:14)//'.JEUX'
      JEUEVD = RESOCO(1:14)//'.JEVD'
      CALL JEVEUO(NUMLIA,'L',JNUMLI)
      CALL JEVEUO(JEUX  ,'L',JJEUX )
      CALL JEVEUO(JEUEVD,'E',JJEVD )
C
C --- DETECTION COLLUISION
C
      DO 10 ILIAI = 1,NBLIAI
        POSNOE = ZI(JNUMLI+4*(ILIAI-1)+2-1)
        ETATCO = ZR(JJEVD+3*(POSNOE-1)+3-1)
        JEUINI = ZR(JJEVD+3*(POSNOE-1)+2-1)
        IF (ETATCO.EQ.0.D0) THEN
          IF (JEUINI.LE.PRECCO) THEN
            ZR(JJEVD+3*(POSNOE-1)+3-1) = 1.D0
            LEVENT = .TRUE.     
          ENDIF
        ENDIF
  10  CONTINUE
C
C --- ACTIVATION EVENEMENT
C
      IF (LEVENT) THEN
        IEVDAC = IECHEC
      ENDIF
C
      CALL JEDEMA()
      END
