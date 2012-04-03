      SUBROUTINE NMECEB(SDERRO,NOMBCL,ETABCL)
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
C ECRITURE DE L'ETAT DE LA BOUCLE
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
C IN  ETABCL : ETAT DE LA BOUCLE
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
C
C --- ACCES SD
C
      ERRCVG = SDERRO(1:19)//'.CONV'
      CALL JEVEUO(ERRCVG,'E',JECONV)
C
C --- SELON ETAT
C
      IF (ETABCL.EQ.'CONT') THEN
        ICONVE = 0
      ELSEIF (ETABCL.EQ.'CONV') THEN
        ICONVE = 1
      ELSEIF (ETABCL.EQ.'EVEN') THEN
        ICONVE = 2
      ELSEIF (ETABCL.EQ.'ERRE') THEN
        ICONVE = 3
      ELSEIF (ETABCL.EQ.'STOP') THEN
        ICONVE = 4
      ELSEIF (ETABCL.EQ.'CTCD') THEN
        ICONVE = 5
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF
C
C --- ENREGISTREMENT DE LA CONVERGENCE
C
      IF (NOMBCL.EQ.'RESI') THEN
        ZI(JECONV-1+1) = ICONVE
      ELSEIF (NOMBCL.EQ.'NEWT') THEN
        ZI(JECONV-1+2) = ICONVE
      ELSEIF (NOMBCL.EQ.'FIXE') THEN
        ZI(JECONV-1+3) = ICONVE
      ELSEIF (NOMBCL.EQ.'INST') THEN
        ZI(JECONV-1+4) = ICONVE
      ELSEIF (NOMBCL.EQ.'CALC') THEN
        ZI(JECONV-1+5) = ICONVE
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF
C
      CALL JEDEMA()
      END
