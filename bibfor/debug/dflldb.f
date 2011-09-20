      SUBROUTINE DFLLDB(SDLIST,IFM  )
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF DEBUG  DATE 19/09/2011   AUTEUR ABBAS M.ABBAS 
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
C
      IMPLICIT NONE
      CHARACTER*8  SDLIST
      INTEGER      IFM
C
C ----------------------------------------------------------------------
C
C OPERATEUR DEFI_LIST_INST
C
C IMPRESSION DEBUG
C
C ----------------------------------------------------------------------
C
C IN  SDLIST : NOM DE LA SD RESULTAT
C IN  IFM    : UNITE LOGIQUE AFFICHAGE
C
C -------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------------
C
      INTEGER ZI
      COMMON /IVARJE/ ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
C ---------------- FIN DECLARATIONS NORMALISEES JEVEUX -----------------
C
      CHARACTER*24 LISIFR
      INTEGER      JLINR
      INTEGER      IECHEC,NECHEC,NBINST,NADAPT
      INTEGER      NBPAMX
      REAL*8       DTMIN,PASMIN,PASMAX
      INTEGER      DFLLVD,LEEVR,LEEVK,LESUR
      CHARACTER*24 LISEVR,LISEVK,LISESU
      INTEGER      JEEVR,JEEVK,JESUR
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- TAILLE DES VECTEURS
C
      LEEVR  = DFLLVD('LEEVR')
      LEEVK  = DFLLVD('LEEVK')
      LESUR  = DFLLVD('LESUR') 
C
C --- ACCES SDS
C
      LISIFR = SDLIST(1:8)//'.LIST.INFOR'
      CALL JEVEUO(LISIFR,'L',JLINR )
C
C --- LONGUEURS
C
      NECHEC = NINT(ZR(JLINR-1 + 9))
      NBINST = NINT(ZR(JLINR-1 + 8))
      NADAPT = NINT(ZR(JLINR-1 + 10))
C
C --- GESTION DE LA LISTE D'INSTANTS
C
      IF (ZR(JLINR-1+1).EQ.1.D0) THEN
        WRITE(IFM,*) '<DEFILISTINST> GESTION MANUELLE '//
     &               'DE LA LISTE D''INSTANTS' 
      ELSEIF (ZR(JLINR-1+1).EQ.2.D0) THEN
        WRITE(IFM,*) '<DEFILISTINST> GESTION AUTOMATIQUE '//
     &               'DE LA LISTE D''INSTANTS'       
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF
      DTMIN  = ZR(JLINR-1+5)
      WRITE(IFM,*) '<DEFILISTINST> ... LA LISTE CONTIENT ',NBINST,
     &             ' INSTANTS ET LE PAS MINIMUM VAUT ',DTMIN
C
C --- PARA. GESTION AUTO PAS DE TEMPS
C
      IF (ZR(JLINR-1+1).EQ.2.D0) THEN
        PASMIN = ZR(JLINR-1+2)
        PASMAX = ZR(JLINR-1+3)
        NBPAMX = NINT(ZR(JLINR-1+4))
        WRITE(IFM,*) '<DEFILISTINST> PARAMETRES DE LA GESTION  '//
     &               'AUTOMATIQUE DE LA LISTE D''INSTANTS'
        WRITE(IFM,*) '<DEFILISTINST> ... PAS MINI   : ',PASMIN
        WRITE(IFM,*) '<DEFILISTINST> ... PAS MAXI   : ',PASMAX        
        WRITE(IFM,*) '<DEFILISTINST> ... NB_PAS_MAXI: ',NBPAMX
      ENDIF
C
C --- ECHEC
C
      IF (NECHEC.GT.0) THEN
        LISEVR = SDLIST(1:8)//'.ECHE.EVENR'
        LISEVK = SDLIST(1:8)//'.ECHE.EVENK'
        LISESU = SDLIST(1:8)//'.ECHE.SUBDR'
        CALL JEVEUO(LISEVR,'L',JEEVR )
        CALL JEVEUO(LISEVK,'L',JEEVK )
        CALL JEVEUO(LISESU,'L',JESUR )
        WRITE(IFM,*) '<DEFILISTINST> GESTION DES EVENEMENTS (',
     &               NECHEC,' EVENEMENTS)'    
        DO 10 IECHEC = 1,NECHEC
          WRITE(IFM,*) '<DEFILISTINST> ... EVENEMENT : ',IECHEC
          IF (ZR(JEEVR-1+LEEVR*(IECHEC-1)+1).EQ.0.D0) THEN
            WRITE(IFM,*) '<DEFILISTINST> ...... DIVE_ERRE'
          ELSEIF (ZR(JEEVR-1+LEEVR*(IECHEC-1)+1).EQ.1.D0) THEN
            WRITE(IFM,*) '<DEFILISTINST> ...... DIVE_ITER'
          ELSEIF (ZR(JEEVR-1+LEEVR*(IECHEC-1)+1).EQ.2.D0) THEN
            WRITE(IFM,*) '<DEFILISTINST> ...... DELTA_GRANDEUR'
            WRITE(IFM,*) '<DEFILISTINST> ......... CHAMP      :',
     &         ZK16(JEEVK-1+LEEVK*(IECHEC-1)+1)
            WRITE(IFM,*) '<DEFILISTINST> ......... COMPOSANTE :',
     &         ZK16(JEEVK-1+LEEVK*(IECHEC-1)+2)
            WRITE(IFM,*) '<DEFILISTINST> ......... COMPARATEUR:',
     &         ZK16(JEEVK-1+LEEVK*(IECHEC-1)+3)           
          ELSEIF (ZR(JEEVR-1+LEEVR*(IECHEC-1)+1).EQ.3.D0) THEN
            WRITE(IFM,*) '<DEFILISTINST> ...... COMP_NCVG'
          ELSEIF (ZR(JEEVR-1+LEEVR*(IECHEC-1)+1).EQ.4.D0) THEN
            WRITE(IFM,*) '<DEFILISTINST> ...... COLLISION'
          ELSEIF (ZR(JEEVR-1+LEEVR*(IECHEC-1)+1).EQ.5.D0) THEN
            WRITE(IFM,*) '<DEFILISTINST> ...... INTERPENETRATION'
            WRITE(IFM,*) '<DEFILISTINST> ......... PENE_MAXI  :',
     &         ZR(JEEVR-1+LEEVR*(IECHEC-1)+6)          
          ELSE
            CALL ASSERT(.FALSE.)  
          ENDIF         
C
C ------- ACTION
C
          IF (ZR(JEEVR-1+LEEVR*(IECHEC-1)+2).EQ.0.D0) THEN
            WRITE(IFM,*) '<DEFILISTINST> ...... ARRET DU CALCUL'
            
          ELSEIF (ZR(JEEVR-1+LEEVR*(IECHEC-1)+2).EQ.1.D0) THEN
            WRITE(IFM,*) '<DEFILISTINST> ...... DECOUPE DU PAS'//
     &                   ' DE TEMPS'
            CALL DFLLD2(SDLIST,IFM  ,IECHEC)
     
          ELSEIF (ZR(JEEVR-1+LEEVR*(IECHEC-1)+2).EQ.2.D0) THEN
            WRITE(IFM,*) '<DEFILISTINST> ...... AUGMENTATION'//
     &                   ' DU NOMBRE D''ITERATIONS DE NEWTON'
            WRITE(IFM,*) '<DEFILISTINST> ......... EN'//
     &                   ' PERMETTANT',
     &                   NINT(ZR(JESUR-1+LESUR*(IECHEC-1)+7)),
     &                   ' % D''ITERATIONS EN PLUS'
     
            IF (ZR(JESUR-1+LESUR*(IECHEC-1)+1).EQ.0.D0) THEN
              WRITE(IFM,*) '<DEFILISTINST> ....... SANS '//
     &                     ' PERMETTRE UN DECOUPAGE EN CAS D''ECHEC'
            
            
            ELSEIF (ZR(JESUR-1+LESUR*(IECHEC-1)+1).EQ.1.D0) THEN
              WRITE(IFM,*) '<DEFILISTINST> ....... EN'//
     &                     ' PERMETTANT UN DECOUPAGE EN CAS D''ECHEC'
              CALL DFLLD2(SDLIST,IFM  ,IECHEC)
              
            ELSEIF (ZR(JESUR-1+LESUR*(IECHEC-1)+1).EQ.2.D0) THEN
              WRITE(IFM,*) '<DEFILISTINST> ....... EN'//
     &                     ' PERMETTANT UN DECOUPAGE EN CAS D''ECHEC'
              CALL DFLLD2(SDLIST,IFM  ,IECHEC)
            ELSE
              CALL ASSERT(.FALSE.)
            ENDIF     
     
              
          ELSEIF (ZR(JEEVR-1+LEEVR*(IECHEC-1)+2).EQ.3.D0) THEN
            WRITE(IFM,*) '<DEFILISTINST> ...... CHANGEMENT'//
     &                   ' DE LA SOLUTION DE PILOTAGE'
            IF (ZR(JESUR-1+LESUR*(IECHEC-1)+1).EQ.0.D0) THEN
              WRITE(IFM,*) '<DEFILISTINST> ....... SANS '//
     &                     ' PERMETTRE UN DECOUPAGE EN CAS D''ECHEC'
            
            
            ELSEIF (ZR(JESUR-1+LESUR*(IECHEC-1)+1).EQ.1.D0) THEN
              WRITE(IFM,*) '<DEFILISTINST> ....... EN'//
     &                     ' PERMETTANT UN DECOUPAGE EN CAS D''ECHEC'
              CALL DFLLD2(SDLIST,IFM  ,IECHEC)
            ENDIF     
          ELSEIF (ZR(JEEVR-1+LEEVR*(IECHEC-1)+2).EQ.4.D0) THEN
            WRITE(IFM,*) '<DEFILISTINST> ...... ADAPTATION'//
     &                   ' DU COEFFICIENT DE PENALISATION'
            WRITE(IFM,*) '<DEFILISTINST> ......... EN'//
     &                   ' PERMETTANT UN COEF. MAXI DE: ',
     &                   ZR(JESUR-1+LESUR*(IECHEC-1)+8)
          
          ELSE
            CALL ASSERT(.FALSE.)  
          ENDIF 
  10    CONTINUE
      
      ENDIF      
C
C --- ADAPTATION
C
      IF (NADAPT.GT.0) THEN
        WRITE(IFM,*) '<DEFILISTINST> SCHEMAS D''ADAPTATION DU'//
     &               ' PAS DE TEMPS  (',
     &               NADAPT,' ADAPTATIONS)'    
      ENDIF    
C
      CALL JEDEMA()
      END
