      SUBROUTINE NMLEEB(SDERRO,NOMBCL,ETABCL)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 02/04/2012   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
      CHARACTER*24 SDERRO
      CHARACTER*4  NOMBCL,ETABCL
C
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (ALGORITHME)
C
C ETAT DE LA CONVERGENCE
C
C ----------------------------------------------------------------------
C
C
C IN  SDERRO : SD GESTION DES ERREURS
C IN  NOMBCL : NOM DE LA BOUCLE
C               'RESI' - BOUCLE SUR LES RESIDUS D'EQUILIBRE
C               'NEWT' - BOUCLE DE NEWTON
C               'FIXE' - BOUCLE DE POINT FIXE
C               'INST' - BOUCLE SUR LES PAS DE TEMPS
C               'CALC' - CALCUL
C OUT ETABCL : ETAT DE LA BOUCLE
C               'CONT' - ON CONTINUE LA BOUCLE
C               'CTCD' - ON CONTINUE LA BOUCLE APRES LA PREDICTION
C               'CONV' - ON STOPPE LA BOUCLE : CONVERGEE
C               'EVEN' - EVENEMENT PENDANT LA BOUCLE
C               'ERRE' - ON STOPPE LA BOUCLE : ERREUR TRAITEE
C               'STOP' - ON STOPPE LA BOUCLE : ERREUR NON TRAITEE
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
      CHARACTER*24 ERRCVG
      INTEGER      JECONV
      INTEGER      ICONVE
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- INITIALISATIONS
C
      ICONVE = 0
      ETABCL = ' '
C
C --- ACCES SD
C
      ERRCVG = SDERRO(1:19)//'.CONV'
      CALL JEVEUO(ERRCVG,'L',JECONV)
C
C --- LECTURE DE LA CONVERGENCE
C
      IF (NOMBCL.EQ.'RESI') THEN
        ICONVE = ZI(JECONV-1+1)
      ELSEIF (NOMBCL.EQ.'NEWT') THEN
        ICONVE = ZI(JECONV-1+2)
      ELSEIF (NOMBCL.EQ.'FIXE') THEN
        ICONVE = ZI(JECONV-1+3)
      ELSEIF (NOMBCL.EQ.'INST') THEN
        ICONVE = ZI(JECONV-1+4)
      ELSEIF (NOMBCL.EQ.'CALC') THEN
        ICONVE = ZI(JECONV-1+5)
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF
C
C --- SELON ETAT
C
      IF (ICONVE.EQ.0) THEN
        ETABCL = 'CONT'
      ELSEIF (ICONVE.EQ.1) THEN
        ETABCL = 'CONV'
      ELSEIF (ICONVE.EQ.2) THEN
        ETABCL = 'EVEN'
      ELSEIF (ICONVE.EQ.3) THEN
        ETABCL = 'ERRE'
      ELSEIF (ICONVE.EQ.4) THEN
        ETABCL = 'STOP'
      ELSEIF (ICONVE.EQ.5) THEN
        ETABCL = 'CTCD'
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF
C
      CALL JEDEMA()
      END
